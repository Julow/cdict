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

static inline int branches_number(branches_t const *b, int branch_i)
{
  switch (b->header & BRANCHES_NUMBERS_FORMAT_MASK)
  {
    case NUMBERS_8_BITS:
      return BRANCHES_NUMBERS(b, uint8_t)[branch_i] * MAX_PTR_NUMBER;
    case NUMBERS_16_BITS:
      return BRANCHES_NUMBERS(b, uint16_t)[branch_i] * MAX_PTR_NUMBER;
    case NUMBERS_NONE:
    default: return 0;
  }
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
  int len = b->length;
  for (int i = 0; i < len;)
  {
    // [l] is NUL when stepping outside of the tree but [k] cannot be NUL so we
    // don't have to check for this case.
    char l = b->labels[i];
    if (c == l)
    {
      index += branches_number(b, i);
      cdict_find_node(data, BRANCHES(b)[i], word + 1, end, index, result);
      return;
    }
    else if (c < l)
      i = i * 2 + 1;
    else
      i = i * 2 + 2;
  }
}

static void cdict_find_prefix(void const *data, int32_t off,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  prefix_t const *node = data + off;
  char const *prefix = node->prefix;
  char const *prefix_end = prefix + PREFIX_LENGTH(node);
  while (true)
  {
    if (*(word++) != *(prefix++)) return; // Prefix doesn't match
    if (prefix == prefix_end) break; // Prefix matches
    if (word == end) return; // Query ends
  }
  cdict_find_node(data, PREFIX_NEXT_PTR(node), word, end, index, result);
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
    default: return;
  }
}

#define RESULT_T_INIT ((cdict_result_t){ \
    .found = false, .index = 0, .prefix_ptr = 0, })

void cdict_find(cdict_t const *dict, char const *word, int word_size,
    cdict_result_t *result)
{
  *result = RESULT_T_INIT;
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

static int cdict_word_branches(void const *data, uint32_t off, int index,
    char *dst, int dsti, int max_len)
{
  branches_t const *b = data + off;
  // The 'number' field of each transition is in the same order as the labels.
  int len = b->length;
  ptr_t next = 0;
  int next_number = 0;
  for (int i = 0; i < len;)
  {
    int ni = branches_number(b, i);
    ptr_t bi = BRANCHES(b)[i];
    if (PTR_NUMBER(bi) + ni > index)
      i = i * 2 + 1;
    else
    {
      next = bi;
      next_number = ni;
      dst[dsti] = b->labels[i];
      i = i * 2 + 2;
    }
  }
  if (next == 0)
    return dsti;
  return cdict_word_node(data, next, index - next_number, dst, dsti + 1,
      max_len);
}

static int cdict_word_prefix(void const *data, uint32_t off, int index,
    char *dst, int dsti, int max_len)
{
  prefix_t const *b = data + off;
  int end = dsti + PREFIX_LENGTH(b);
  char const *prefix = b->prefix;
  if (end > max_len)
    end = max_len;
  while (dsti < end)
    dst[dsti++] = *(prefix++);
  return cdict_word_node(data, PREFIX_NEXT_PTR(b), index, dst, end, max_len);
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
    cdict_suffixes
    ************************************************************************ */

static void suffixes(cdict_t const *dict, ptr_t ptr, int index, priority_t *dst);

static void suffixes_branches(cdict_t const *dict, uint32_t off, int index,
    priority_t *dst)
{
  branches_t const *b = dict->data + off;
  int len = b->length;
  // TODO: Iterate branches in sorted order
  for (int i = 0; i < len; i++)
    suffixes(dict, BRANCHES(b)[i], index + branches_number(b, i), dst);
}

static void suffixes(cdict_t const *dict, ptr_t ptr, int index, priority_t *dst)
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
      suffixes_branches(dict, off, index, dst);
      break;
    case PREFIX:
      suffixes(dict, PREFIX_NEXT_PTR((prefix_t const*)(dict->data + off)),
          index, dst);
      break;
  }
}

int cdict_suffixes(cdict_t const *dict, cdict_result_t const *r, int *dst,
    int count)
{
  if (r->prefix_ptr == 0)
    return 0;
  word_freq_t words[count];
  priority_t queue;
  priority_init(&queue, words, count);
  suffixes(dict, r->prefix_ptr, r->index, &queue);
  for (int i = 0; i < queue.ends; i++)
    dst[i] = words[i].index;
  return queue.ends;
}

/** ************************************************************************
    cdict_distance
    ************************************************************************ */

static void distance(cdict_t const *dict, ptr_t ptr, char const *word,
    char const *end, int index, int dist, priority_t *q);

static void distance_branches(cdict_t const *dict, uint32_t off, char const *word,
    char const *end, int index, int dist, priority_t *q)
{
  branches_t const *b = dict->data + off;
  int len = b->length;
  char c = *word;
  for (int i = 0; i < len; i++)
  {
    int next_index = index + branches_number(b, i);
    // Change a letter
    distance(dict, BRANCHES(b)[i], word + 1, end, next_index,
        (c == b->labels[i]) ? dist : dist - 1, q);
    // Add a letter
    distance(dict, BRANCHES(b)[i], word, end, next_index, dist - 1, q);
  }
}

static void distance_prefix_(cdict_t const *dict, prefix_t const *p, int i,
    char const *word, char const *end, int index, int dist, priority_t *q)
{
  if (i == PREFIX_LENGTH(p))
    return distance(dict, PREFIX_NEXT_PTR(p), word, end, index, dist, q);
  if (word == end)
    return;
  if (dist > 0)
  {
    // Remove a letter
    distance_prefix_(dict, p, i, word + 1, end, index, dist - 1, q);
    // Add a letter
    distance_prefix_(dict, p, i + 1, word, end, index, dist - 1, q);
    // Change a letter
    if (*word != p->prefix[i])
      distance_prefix_(dict, p, i + 1, word + 1, end, index, dist - 1, q);
  }
  if (*word == p->prefix[i])
    return distance_prefix_(dict, p, i + 1, word + 1, end, index, dist, q);
}

static void distance_prefix(cdict_t const *dict, uint32_t off, char const *word,
    char const *end, int index, int dist, priority_t *q)
{
  distance_prefix_(dict, dict->data + off, 0, word, end, index, dist, q);
}

static void distance(cdict_t const *dict, ptr_t ptr, char const *word,
    char const *end, int index, int dist, priority_t *q)
{
  if (dist == 0)
  {
    cdict_result_t r = RESULT_T_INIT;
    cdict_find_node(dict->data, ptr, word, end, index, &r);
    if (r.found)
      priority_add(q, cdict_freq(dict, r.index), r.index);
    // Also add suffixes of any length.
    if (r.prefix_ptr != 0)
      suffixes(dict, r.prefix_ptr, r.index, q);
    return;
  }
  if (word == end)
    return;
  int32_t off = PTR_OFFSET(ptr);
  index += PTR_NUMBER(ptr);
  if (PTR_IS_FINAL(ptr))
    index++;
  // Remove a letter
  distance(dict, ptr, word + 1, end, index, dist - 1, q);
  switch (PTR_KIND(ptr))
  {
    case BRANCHES:
      return distance_branches(dict, off, word, end, index, dist, q);
    case PREFIX:
      return distance_prefix(dict, off, word, end, index, dist, q);
  }
}

int cdict_distance(cdict_t const *dict, char const *word, int wlen, int dist,
    int *dst, int count)
{
  word_freq_t words[count];
  priority_t queue;
  priority_init(&queue, words, count);
  distance(dict, dict->root_ptr, word, word + wlen, 0, dist, &queue);
  for (int i = 0; i < queue.ends; i++)
    dst[i] = words[i].index;
  return queue.ends;
}
