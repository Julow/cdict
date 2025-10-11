#include "libcdict.h"
#include "libcdict_format.h"
#include <assert.h>
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
    .root_ptr = h->root_ptr
  };
  return dict;
}

static ptr_or_null_t cdict_find_node(void const *data, ptr_t ptr,
    char const *word, char const *end);

static ptr_or_null_t cdict_find_branches(void const *data, int32_t off,
    char const *word, char const *end)
{
  branches_t const *b = data + off;
  if (word == end)
    return b->leaf;
  char c = *word;
  while (c >= b->low + b->length)
  {
    if (b->has_next == 0)
      return 0;
    b = BRANCHES_NEXT(b);
  }
  if (c < b->low)
    return 0;
  ptr_or_null_t next = b->branches[c - b->low];
  if (next == 0)
    return 0;
  return cdict_find_node(data, next, word + 1, end);
}

static ptr_or_null_t cdict_find_prefix(void const *data, int32_t off,
    char const *word, char const *end)
{
  prefix_t const *node = data + off;
  int i = 1;
  char p = node->prefix[0];
  while (true)
  {
    if (word == end) return 0;
    if (*word != p) return 0;
    word++;
    if (i == 4) break;
    p = node->prefix[i];
    if (p == 0) break;
    i++;
  }
  return cdict_find_node(data, node->next, word, end);
}

static ptr_or_null_t cdict_find_node(void const *data, ptr_t ptr,
    char const *word, char const *end)
{
  int32_t off = PTR_OFFSET(ptr);
  switch (PTR_KIND(ptr))
  {
    case LEAF: return (word == end) ? ptr : 0;
    case BRANCHES: return cdict_find_branches(data, off, word, end);
    case PREFIX: return cdict_find_prefix(data, off, word, end);
    default: return 0;
  }
}

bool cdict_find(cdict_t const *dict, char const *word, int word_size,
    int *leaf)
{
  assert(sizeof(branches_t) == 8);
  assert(sizeof(prefix_t) == 8);
  ptr_or_null_t r = cdict_find_node(dict->data, dict->root_ptr, word,
      word + word_size);
  if (r == 0 || PTR_KIND(r) != LEAF)
    return false;
  if (leaf != NULL)
    *leaf = LEAF_DATA(r);
  return true;
}
