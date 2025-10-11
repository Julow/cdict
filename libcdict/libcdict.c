#include "libcdict.h"
#include "libcdict_format.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

cdict_t cdict_of_string(char const *data, int size)
{
  assert(sizeof(branches_t) == 4);
  assert(sizeof(branches_with_leaf_t) == 4);
  assert(sizeof(prefix_t) == 4);
  assert(sizeof(btree_t) == 8);
  assert(sizeof(btree_with_leaf_t) == 4);
  header_t const *h = (void const*)data;
  cdict_t dict = {
    .data = data,
    .size = size,
    .root_ptr = h->root_ptr
  };
  return dict;
}

static ptr_or_null_t cdict_find_node_(void const *data, int32_t off, kind_t kind,
    char const *word, char const *end);

static ptr_or_null_t cdict_find_node(void const *data, ptr_t ptr,
    char const *word, char const *end)
{
  return cdict_find_node_(data, PTR_OFFSET(ptr), PTR_KIND(ptr), word, end);
}

static ptr_or_null_t cdict_find_branches(void const *data, int32_t off,
    char const *word, char const *end)
{
  branches_t const *b = data + off;
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

static ptr_or_null_t cdict_find_branches_with_leaf(void const *data, int32_t off,
    char const *word, char const *end)
{
  branches_with_leaf_t const *b = data + off;
  if (word == end)
    return b->leaf;
  return cdict_find_branches(data, off + 4, word, end);
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
    if (i == 3) break;
    p = node->prefix[i];
    if (p == 0) break;
    i++;
  }
  if (node->next_kind == LEAF)
    return (word == end) ? node->leaf[0] : 0;
  else
    return cdict_find_node_(data, off + sizeof(prefix_t), node->next_kind, word, end);
}

static ptr_or_null_t cdict_find_btree(void const *data, int32_t off,
    char const *word, char const *end)
{
  btree_t const *b = data + off;
  char k = *word;
  // This node cannot match a NUL byte because it is used in the encoding of
  // the tree.
  if (k == 0)
    return 0;
  int i = 0;
  while (i < BTREE_NODE_LENGTH)
  {
    // [l] is NUL when stepping outside of the tree but [k] cannot be NUL so we
    // don't have to check for this case.
    char l = b->labels[i];
    if (k == l)
      return cdict_find_node(data, b->next[i], word + 1, end);
    else if (k < l)
      i = i * 2 + 1;
    else
      i = i * 2 + 2;
  }
  return 0;
}

static ptr_or_null_t cdict_find_btree_with_leaf(void const *data, int32_t off,
    char const *word, char const *end)
{
  if (word == end)
  {
    btree_with_leaf_t const *b = data + off;
    return b->leaf;
  }
  return cdict_find_btree(data, off + sizeof(btree_with_leaf_t), word, end);
}

static ptr_or_null_t cdict_find_node_(void const *data, int32_t off, kind_t kind,
    char const *word, char const *end)
{
  switch (kind)
  {
    case LEAF: return (word == end) ? (off | (int32_t)kind) : 0;
    case BRANCHES: return cdict_find_branches(data, off, word, end);
    case BRANCHES_WITH_LEAF: return cdict_find_branches_with_leaf(data, off, word, end);
    case PREFIX: return cdict_find_prefix(data, off, word, end);
    case BTREE: return cdict_find_btree(data, off, word, end);
    case BTREE_WITH_LEAF: return cdict_find_btree_with_leaf(data, off, word, end);
    default: return 0;
  }
}

bool cdict_find(cdict_t const *dict, char const *word, int word_size,
    int *leaf)
{
  ptr_or_null_t r = cdict_find_node(dict->data, dict->root_ptr, word,
      word + word_size);
  if (r == 0 || PTR_KIND(r) != LEAF)
    return false;
  if (leaf != NULL)
    *leaf = LEAF_DATA(r);
  return true;
}
