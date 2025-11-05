#include "libcdict.h"
#include "libcdict_format.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

cdict_t cdict_of_string(char const *data, int size)
{
  header_t const *h = (void const*)data;
  cdict_t dict = {
    .data = data,
    .size = size,
    .root_ptr = h->root_ptr,
    .freq = (uint8_t const*)(data + h->freq_off)
  };
  return dict;
}

/** ************************************************************************
    cdict_find
    ************************************************************************ */

static void cdict_find_node(void const *data, ptr_t ptr,
    char const *word, char const *end, int index, cdict_result_t *result);

static void cdict_find_branches(void const *data, int32_t off,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  branches_t const *b = data + off;
  char c = *word;
  while (c >= b->low + b->length)
  {
    if (b->has_next == 0)
      return;
    b = BRANCHES_NEXT(b);
  }
  if (c < b->low)
    return;
  ptr_or_null_t next = b->branches[c - b->low];
  if (next > 0)
    cdict_find_node(data, next, word + 1, end, index, result);
}

static void cdict_find_prefix(void const *data, int32_t off,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  prefix_t const *node = data + off;
  int i = 1;
  char p = node->prefix[0];
  while (true)
  {
    if (*word != p) return;
    word++;
    if (i == PREFIX_NODE_LENGTH) break;
    p = node->prefix[i];
    if (p == 0) break;
    if (word == end) return;
    i++;
  }
  cdict_find_node(data, node->next, word, end, index, result);
}

static void cdict_find_btree(void const *data, int32_t off,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  btree_t const *b = data + off;
  char k = *word;
  // This node cannot match a NUL byte because it is used in the encoding of
  // the tree.
  if (k == 0)
    return;
  int i = 0;
  while (i < BTREE_NODE_LENGTH)
  {
    // [l] is NUL when stepping outside of the tree but [k] cannot be NUL so we
    // don't have to check for this case.
    char l = b->labels[i];
    if (k == l)
    {
      cdict_find_node(data, b->next[i], word + 1, end, index, result);
      return;
    }
    else if (k < l)
      i = i * 2 + 1;
    else
      i = i * 2 + 2;
  }
}

static void cdict_find_number(void const *data, int32_t off,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  number_t const *n = data + off;
  cdict_find_node(data, n->next, word, end, index + n->number, result);
}

static void cdict_find_node(void const *data, ptr_t ptr,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  int32_t off = PTR_OFFSET(ptr);
  index += PTR_NUMBER(ptr);
  if (word == end)
  {
    result->found = (bool)PTR_IS_FINAL(ptr);
    result->index = index;
    // Remove the number field and final flag from prefix_ptr.
    result->prefix_ptr = ptr & ~(PTR_NUMBER_MASK | PTR_FLAG_FINAL);
    return;
  }
  if (PTR_IS_FINAL(ptr))
    index++;
  switch (PTR_KIND(ptr))
  {
    case BRANCHES:
      return cdict_find_branches(data, off, word, end, index, result);
    case PREFIX: return cdict_find_prefix(data, off, word, end, index, result);
    case BTREE: return cdict_find_btree(data, off, word, end, index, result);
    case NUMBER: return cdict_find_number(data, off, word, end, index, result);
    default: return;
  }
}

void cdict_find(cdict_t const *dict, char const *word, int word_size,
    cdict_result_t *result)
{
  *result = (cdict_result_t){
    .found = false,
    .index = -1,
  };
  cdict_find_node(dict->data, dict->root_ptr, word, word + word_size, 0,
      result);
}

/** ************************************************************************
    cdict_freq
    ************************************************************************ */

int cdict_freq(cdict_t const *dict, int index)
{
  uint8_t f = dict->freq[index / 2];
  if (index & 1) f = f >> 4;
  return f & 0xF;
}

/** ************************************************************************
    cdict_word
    ************************************************************************ */

static int cdict_word_node(void const *data, ptr_t ptr, int index, char *dst,
    int dsti, int max_len);

/** The number field of a pointer + the number field of the next node if it is
    a NUMBER node. */
static inline int ptr_number_next(void const *data, ptr_t ptr)
{
  int next_number = 0;
  if (PTR_KIND(ptr) == NUMBER)
    next_number = ((number_t const*)(data + PTR_OFFSET(ptr)))->number;
  return PTR_NUMBER(ptr) + next_number;
}

static int cdict_word_branches(void const *data, uint32_t off, int index,
    char *dst, int dsti, int max_len)
{
  branches_t const *b = data + off;
  if (b->length == 0)
    return dsti;
  while (b->has_next)
  {
    branches_t const *next_b = BRANCHES_NEXT(b);
    if (ptr_number_next(data, next_b->branches[0]) > index)
      break; // branches[0] is never equal to 0
    b = next_b;
  }
  int i = 0;
  while (true)
  {
    int j = i + 1;
    while (j < b->length && b->branches[j] == 0)
      j++;
    if (j >= b->length || ptr_number_next(data, b->branches[j]) > index)
      break;
    i = j;
  }
  dst[dsti] = b->low + i;
  return cdict_word_node(data, b->branches[i], index, dst, dsti + 1, max_len);
}

static int cdict_word_prefix(void const *data, uint32_t off, int index,
    char *dst, int dsti, int max_len)
{
  prefix_t const *b = data + off;
  dst[dsti++] = b->prefix[0];
  for (int i = 1; i < PREFIX_NODE_LENGTH && b->prefix[i] != 0
      && dsti < max_len; i++)
    dst[dsti++] = b->prefix[i];
  return cdict_word_node(data, b->next, index, dst, dsti, max_len);
}

static int cdict_word_number(void const *data, uint32_t off, int index,
    char *dst, int dsti, int max_len)
{
  number_t const *n = data + off;
  return cdict_word_node(data, n->next, index - n->number, dst, dsti, max_len);
}

static int cdict_word_btree(void const *data, uint32_t off, int index,
    char *dst, int dsti, int max_len)
{
  btree_t const *b = data + off;
  ptr_t next = 0;
  for (int i = 0; i < BTREE_NODE_LENGTH && b->labels[i] != 0; )
  {
    if (PTR_NUMBER(b->next[i]) > index)
      i = i * 2 + 1;
    else
    {
      next = b->next[i];
      dst[dsti] = b->labels[i];
      i = i * 2 + 2;
    }
  }
  return cdict_word_node(data, next, index, dst, dsti + 1, max_len);
}

static int cdict_word_node(void const *data, ptr_t ptr, int index, char *dst,
    int dsti, int max_len)
{
  if (dsti >= max_len)
    return dsti;
  uint32_t off = PTR_OFFSET(ptr);
  index -= PTR_NUMBER(ptr);
  if (PTR_IS_FINAL(ptr))
  {
    if (index == 0)
      return dsti;
    index--;
  }
  switch (PTR_KIND(ptr))
  {
    case BRANCHES: return cdict_word_branches(data, off, index, dst, dsti, max_len);
    case PREFIX: return cdict_word_prefix(data, off, index, dst, dsti, max_len);
    case BTREE: return cdict_word_btree(data, off, index, dst, dsti, max_len);
    case NUMBER: return cdict_word_number(data, off, index, dst, dsti, max_len);
  }
  return dsti;
}

int cdict_word(cdict_t const *dict, int index, char *dst, int max_length)
{
  return cdict_word_node(dict->data, dict->root_ptr, index, dst, 0,
      max_length);
}

/** ************************************************************************
    Priority queue storing the N most frequent words.
    ************************************************************************ */

typedef struct
{
  int freq:4;
  int index:28;
} word_freq_t;

typedef struct
{
  word_freq_t *q;
  int ends; // Cannot be bigger than [max_length].
  int max_length;
} priority_t;

static void priority_init(priority_t *p, word_freq_t *q, int q_len)
{
  *p = (priority_t){ .q = q, .ends = 0, .max_length = q_len };
}

/** Add a word to the priority queue, if possible. Do nothing if the queue is
    full and the new word ranks lower than any other word already stored.
    Stable: If two words with equal priority are added, the one added first
    ranks higher. */
static void priority_add(priority_t *p, int freq, int index)
{
  int i = p->ends;
  if (p->ends == p->max_length)
  {
    i--;
    if (p->q[i].freq >= freq)
      return; // New word ranks lower than the lowest already in the queue
  }
  else
    p->ends++;
  while (i > 0)
  {
    int parent = (i - 1) / 2;
    if (p->q[parent].freq >= freq)
      break;
    p->q[i] = p->q[parent];
    i = parent;
  }
  p->q[i] = (word_freq_t){ .freq = freq, .index = index };
}

/** ************************************************************************
    cdict_list_prefix
    ************************************************************************ */

static void prefix(cdict_t const *dict, ptr_t ptr, int index, priority_t *dst);

static void prefix_branches(cdict_t const *dict, uint32_t off, int index,
    priority_t *dst)
{
  branches_t const *b = dict->data + off;
  while (true)
  {
    for (int j = 0; j < b->length; j++)
    {
      if (b->branches[j] != 0)
        prefix(dict, b->branches[j], index, dst);
    }
    if (b->has_next == 0)
      break;
    b = BRANCHES_NEXT(b);
  }
}

// TODO: Iterate btree nodes in sorted order
static void prefix_btree(cdict_t const *dict, uint32_t off, int index,
    priority_t *dst)
{
  btree_t const *b = dict->data + off;
  for (int j = 0; j < BTREE_NODE_LENGTH && b->labels[j] != 0; j++)
    prefix(dict, b->next[j], index, dst);
}

static void prefix(cdict_t const *dict, ptr_t ptr, int index, priority_t *dst)
{
  int32_t off = PTR_OFFSET(ptr);
  index += PTR_NUMBER(ptr);
  if (PTR_IS_FINAL(ptr))
  {
    priority_add(dst, cdict_freq(dict, index), index);
    index++;
  }
  switch (PTR_KIND(ptr))
  {
    case BRANCHES:
      prefix_branches(dict, off, index, dst);
      break;
    case PREFIX:
      prefix_t const *p = dict->data + off;
      prefix(dict, p->next, index, dst);
      break;
    case BTREE:
      prefix_btree(dict, off, index, dst);
      break;
    case NUMBER:
      number_t const *n = dict->data + off;
      prefix(dict, n->next, index, dst);
      break;
  }
}

int cdict_list_prefix(cdict_t const *dict, cdict_result_t const *r, int *dst,
    int count)
{
  if (r->prefix_ptr == 0)
    return 0;
  word_freq_t words[count];
  priority_t queue;
  priority_init(&queue, words, count);
  prefix(dict, (ptr_t)r->prefix_ptr, r->index, &queue);
  for (int i = 0; i < queue.ends; i++)
    dst[i] = words[i].index;
  return queue.ends;
}
