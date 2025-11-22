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

static inline int decode_int24(uint8_t const *ar)
{
  return (((int)(int8_t)ar[0]) << 16) | (ar[1] << 8) | ar[2];
}

/** ************************************************************************
    cdict_find
    ************************************************************************ */

static void cdict_find_node(void const *parent_node, int ptr,
    char const *word, char const *end, int index, cdict_result_t *result);

static void cdict_find_branches(branches_t const *b,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  char c = *word;
  int len = b->length;
  for (int i = 0; i < len;)
  {
    // [l] is NUL when stepping outside of the tree but [k] cannot be NUL so we
    // don't have to check for this case.
    char l = b->labels[i];
    if (c == l)
    {
      index += branch_number(b, i);
      cdict_find_node(b, branch(b, i), word + 1, end, index, result);
      return;
    }
    else if (c < l)
      i = i * 2 + 1;
    else
      i = i * 2 + 2;
  }
}

static void cdict_find_prefix(prefix_t const *node,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  char const *prefix = node->prefix;
  char const *prefix_end = prefix + node->length;
  while (true)
  {
    if (*(word++) != *(prefix++)) return; // Prefix doesn't match
    if (prefix == prefix_end) break; // Prefix matches
    if (word == end) return; // Query ends
  }
  cdict_find_node(node, decode_int24(node->next_ptr), word, end, index, result);
}

static void cdict_find_node(void const *parent_node, int ptr,
    char const *word, char const *end, int index, cdict_result_t *result)
{
  void const *node = PTR_NODE(ptr, parent_node);
  if (word == end)
  {
    result->found = (bool)PTR_IS_FINAL(ptr);
    result->index = index;
    result->prefix_ptr = PREFIX_PTR(node, ptr);
    return;
  }
  if (PTR_IS_FINAL(ptr))
    index++;
  switch (PTR_KIND(ptr))
  {
    case BRANCHES:
      return cdict_find_branches(node, word, end, index, result);
    case PREFIX: return cdict_find_prefix(node, word, end, index, result);
    default: return;
  }
}

#define RESULT_T_INIT ((cdict_result_t){ \
    .found = false, .index = 0, .prefix_ptr = 0 })

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

static int cdict_word_node(void const *parent_node, int ptr, int index,
    char *dst, int dsti, int max_len);

static int cdict_word_branches(branches_t const *b, int index,
    char *dst, int dsti, int max_len)
{
  // The 'number' field of each transition is in the same order as the labels.
  int len = b->length;
  int next = 0;
  int next_number = 0;
  for (int i = 0; i < len;)
  {
    int ni = branch_number(b, i);
    int bi = branch(b, i);
    if (ni > index)
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
  return cdict_word_node(b, next, index - next_number, dst, dsti + 1,
      max_len);
}

static int cdict_word_prefix(prefix_t const *p, int index,
    char *dst, int dsti, int max_len)
{
  int end = dsti + p->length;
  char const *prefix = p->prefix;
  if (end > max_len)
    end = max_len;
  while (dsti < end)
    dst[dsti++] = *(prefix++);
  return cdict_word_node(p, decode_int24(p->next_ptr), index, dst, end, max_len);
}

static int cdict_word_node(void const *parent_node, int ptr, int index,
    char *dst, int dsti, int max_len)
{
  if (dsti >= max_len)
    return dsti;
  void const *node = PTR_NODE(ptr, parent_node);
  if (PTR_IS_FINAL(ptr))
  {
    if (index == 0)
      return dsti;
    index--;
  }
  switch (PTR_KIND(ptr))
  {
    case BRANCHES: return cdict_word_branches(node, index, dst, dsti, max_len);
    case PREFIX: return cdict_word_prefix(node, index, dst, dsti, max_len);
  }
  return dsti;
}

int cdict_word(cdict_t const *dict, int index, char *dst, int max_length)
{
  return cdict_word_node(dict->data, dict->root_ptr, index, dst, 0, max_length);
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

static void suffixes(cdict_t const *dict, void const *parent_node, int ptr,
    int index, priority_t *dst);

static void suffixes_branches(cdict_t const *dict, branches_t const *b,
    int index, priority_t *dst)
{
  int len = b->length;
  // TODO: Iterate branches in sorted order
  for (int i = 0; i < len; i++)
    suffixes(dict, b, branch(b, i), index + branch_number(b, i), dst);
}

static void suffixes(cdict_t const *dict, void const *parent_node, int ptr,
    int index, priority_t *dst)
{
  void const *node = PTR_NODE(ptr, parent_node);
  if (PTR_IS_FINAL(ptr))
  {
    priority_add(dst, cdict_freq(dict, index), index);
    index++;
  }
  switch (PTR_KIND(ptr))
  {
    case BRANCHES:
      suffixes_branches(dict, node, index, dst);
      break;
    case PREFIX:
      suffixes(dict, node, decode_int24(((prefix_t const*)node)->next_ptr),
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
  suffixes(dict, PREFIX_PTR_NODE(r->prefix_ptr), PREFIX_PTR_PTR(r->prefix_ptr),
      r->index, &queue);
  for (int i = 0; i < queue.ends; i++)
    dst[i] = words[i].index;
  return queue.ends;
}

/** ************************************************************************
    cdict_distance
    ************************************************************************ */

static void distance(cdict_t const *dict, void const *parent_node, int ptr,
    char const *word, char const *end, int index, int dist, priority_t *q);

static void distance_branches(cdict_t const *dict, branches_t const *b,
    char const *word, char const *end, int index, int dist, priority_t *q)
{
  int len = b->length;
  char c = *word;
  for (int i = 0; i < len; i++)
  {
    int next_index = index + branch_number(b, i);
    int bi = branch(b, i);
    if (c == b->labels[i])
    {
      distance(dict, b, bi, word + 1, end, next_index, dist, q);
    }
    else
    {
      // Change a letter
      distance(dict, b, bi, word + 1, end, next_index, dist - 1, q);
      // Add a letter
      distance(dict, b, bi, word, end, next_index, dist - 1, q);
    }
  }
}

static inline int min(int a, int b) { return (a < b) ? a : b; }

/** Levenshtein distance between the strings 'a' and 'b'. */
static int levenshtein_distance(char const *a, int a_length,
    char const *b, int b_length)
{
  // From https://en.wikipedia.org/wiki/Levenshtein_distance
  int array[(b_length + 1) * 2];
  int *v0 = array;
  int *v1 = v0 + b_length + 1;
  for (int i = 0; i <= b_length; i++)
    v0[i] = i;
  for (int i = 0; i < a_length; i++)
  {
    v1[0] = i + 1;
    for (int j = 0; j < b_length; j++)
    {
      int del = v0[j + 1] + 1;
      int ins = v1[j] + 1;
      int rpl = v0[j] + ((a[i] == b[j]) ? 0 : 1);
      v1[j + 1] = min(del, min(ins, rpl));
    }
    int *swap = v0;
    v0 = v1; v1 = swap;
  }
  return v0[b_length];
}

static void distance_prefix(cdict_t const *dict, prefix_t const *p,
    char const *word, char const *end, int index, int dist, priority_t *q)
{
  int len = p->length;
  int word_len = ((intptr_t)end) - ((intptr_t)word);
  int common_len = min(word_len, len);
  int pdist = levenshtein_distance(word, common_len, p->prefix, common_len);
  if (word_len < len)
  {
    if (pdist + 1 == dist)
      suffixes(dict, p, decode_int24(p->next_ptr), index, q);
    return;
  }
  int next_dist = dist - pdist;
  for (int i = len; i >= 0 && next_dist >= 0; i--)
  {
    // Add suffixes of 'p', including the empty suffix.
    distance(dict, p, decode_int24(p->next_ptr), word + i, end, index, next_dist, q);
    next_dist--;
  }
}

static void distance(cdict_t const *dict, void const *parent_node, int ptr,
    char const *word, char const *end, int index, int dist, priority_t *q)
{
  if (dist == 0)
  {
    cdict_result_t r = RESULT_T_INIT;
    cdict_find_node(parent_node, ptr, word, end, index, &r);
    if (r.found)
      priority_add(q, cdict_freq(dict, r.index), r.index);
    return;
  }
  if (word == end)
  {
    if (dist == 1) // Adding a suffixe of any length is a single edit
      suffixes(dict, parent_node, ptr, index, q);
    return;
  }
  void const *node = PTR_NODE(ptr, parent_node);
  if (PTR_IS_FINAL(ptr))
    index++;
  switch (PTR_KIND(ptr))
  {
    case BRANCHES:
      // Remove a letter
      distance(dict, parent_node, ptr & ~PTR_FLAG_FINAL,
          word + 1, end, index, dist - 1, q);
      return distance_branches(dict, node, word, end, index, dist, q);
    case PREFIX:
      return distance_prefix(dict, node, word, end, index, dist, q);
  }
}

int cdict_distance(cdict_t const *dict, char const *word, int wlen, int dist,
    int *dst, int count)
{
  word_freq_t words[count];
  priority_t queue;
  priority_init(&queue, words, count);
  distance(dict, dict->data, dict->root_ptr, word, word + wlen, 0, dist, &queue);
  for (int i = 0; i < queue.ends; i++)
    dst[i] = words[i].index;
  return queue.ends;
}
