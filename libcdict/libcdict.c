#include "libcdict.h"
#include "libcdict_format.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define NOT_FOUND -1

cdict_t cdict_of_string(char const *data, int size)
{
  header_t const *h = (void const*)data;
  cdict_t dict = {
    .data = data,
    .size = size,
    .root_ptr = h->root_ptr,
    .leaves = (int32_t const*)(data + h->leaves_off)
  };
  return dict;
}

static int cdict_find_node(void const *data, ptr_t ptr,
    char const *word, char const *end, int index);

static int cdict_find_branches(void const *data, int32_t off,
    char const *word, char const *end, int index)
{
  branches_t const *b = data + off;
  char c = *word;
  while (c >= b->low + b->length)
  {
    if (b->has_next == 0)
      return NOT_FOUND;
    b = BRANCHES_NEXT(b);
  }
  if (c < b->low)
    return NOT_FOUND;
  ptr_or_null_t next = b->branches[c - b->low];
  if (next == 0)
    return NOT_FOUND;
  return cdict_find_node(data, next, word + 1, end, index);
}

static int cdict_find_prefix(void const *data, int32_t off,
    char const *word, char const *end, int index)
{
  prefix_t const *node = data + off;
  int i = 1;
  char p = node->prefix[0];
  while (true)
  {
    if (*word != p) return NOT_FOUND;
    word++;
    if (i == PREFIX_NODE_LENGTH) break;
    p = node->prefix[i];
    if (p == 0) break;
    if (word == end) return NOT_FOUND;
    i++;
  }
  return cdict_find_node(data, node->next, word, end, index);
}

static int cdict_find_btree(void const *data, int32_t off,
    char const *word, char const *end, int index)
{
  btree_t const *b = data + off;
  char k = *word;
  // This node cannot match a NUL byte because it is used in the encoding of
  // the tree.
  if (k == 0)
    return NOT_FOUND;
  int i = 0;
  while (i < BTREE_NODE_LENGTH)
  {
    // [l] is NUL when stepping outside of the tree but [k] cannot be NUL so we
    // don't have to check for this case.
    char l = b->labels[i];
    if (k == l)
      return cdict_find_node(data, b->next[i], word + 1, end, index);
    else if (k < l)
      i = i * 2 + 1;
    else
      i = i * 2 + 2;
  }
  return NOT_FOUND;
}

static int cdict_find_number(void const *data, int32_t off,
    char const *word, char const *end, int index)
{
  number_t const *n = data + off;
  return cdict_find_node(data, n->next, word, end, index + n->number);
}

static int cdict_find_node(void const *data, ptr_t ptr,
    char const *word, char const *end, int index)
{
  index += PTR_NUMBER(ptr);
  if (PTR_IS_FINAL(ptr))
  {
    if (word == end)
      return index;
    index++;
  }
  else if (word == end)
    return NOT_FOUND;
  int32_t off = PTR_OFFSET(ptr);
  switch (PTR_KIND(ptr))
  {
    case BRANCHES: return cdict_find_branches(data, off, word, end, index);
    case PREFIX: return cdict_find_prefix(data, off, word, end, index);
    case BTREE: return cdict_find_btree(data, off, word, end, index);
    case NUMBER: return cdict_find_number(data, off, word, end, index);
    default: return NOT_FOUND;
  }
}

bool cdict_find(cdict_t const *dict, char const *word, int word_size,
    int *result)
{
  int r = cdict_find_node(dict->data, dict->root_ptr, word, word + word_size, 0);
  if (r == NOT_FOUND)
    return false;
  if (result != NULL)
    *result = dict->leaves[r];
  return true;
}
