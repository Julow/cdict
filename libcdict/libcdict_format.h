/** libdict

This file describes the encoding of the dictionary.

The main concepts are nodes and tagged pointers.

Nodes form the data structure and a tagged pointers reference a node.
There are several kinds of nodes.

Tagged pointers (ptr_t) are 32 bits integers with the following layout:
- The 29 highest bits represent the byte offset in the dictionary where the
  referenced node starts.
- The 3 lowest bits represent the kind of the referenced node.

The offset in the dictionary is obtained from a tagged pointers using
PTR_OFFSET(PTR).
NULL pointers (int32 equal to 0) are recognized as a pointer to a branches node
at offset 0, unless it is given an other meaning in specific contextes (field
of type ptr_or_null_t).
*/

#include <stdint.h>

typedef int32_t ptr_t;
typedef ptr_t ptr_or_null_t;

typedef enum
{
  LEAF = 0b001,
  BRANCHES = 0b000,
  PREFIX = 0b010,
  BTREE = 0b100,
  WITH_LEAF = 0b011,
} kind_t;

#define PTR_KIND_MASK 0b111
#define PTR_OFFSET_MASK ~0b111

/** Partial cast to [kind_t]. */
#define PTR_KIND(PTR) (kind_t)((PTR) & PTR_KIND_MASK)
#define PTR_OFFSET(PTR) ((PTR) & PTR_OFFSET_MASK)

/** LEAF nodes (size = 0)

Leaf nodes occupie no space in the dictionary, instead the 29 highest bits
represent arbitrary metadata.
The presence of a leaf node indicates that the word is present in the
dictionary.
*/

#define LEAF_DATA(PTR) ((PTR) >> 3)

/** BRANCHES nodes (size = 4 bytes + array)

A branching node that consumes 1 byte from the query.

The 'branches' fields is an array of node pointers indexed by the first byte of
the query, minus the 'low' value.
Node pointers are NULL when the queried word is not in the dictionary.
If the first byte of the query is greater than 'low + length - 1'
and 'has_next' is 1, the query should be tried again on the branches node at
the offset BRANCHES_NEXT(B). However, if 'has_next' is 0, no prefix higher than
'low + length - 1' are present in the dictionary.

The 'low', 'length' and 'has_next' can be used during construction to
remove regions or NULL pointers at the beginning and at the end of the
array, as well as inside the array if that reduces the total size.
*/

typedef struct
{
  char low; /** Lowest value represented by the node. */
  char length; /** Length of the 'branches' array in number of pointers. */
  char has_next; /** Whether a branches node follow after this one. */
  char padding; /** No meaning. */
  ptr_or_null_t branches[];
} branches_t;

/** Size in bytes of the branches node [B]. */
#define BRANCHES_SIZE(B) (sizeof(branches_t) + (B).length * sizeof(ptr_or_null_t))
/** Offset to the next [branches_t] node when [has_next]. Argument [B_PTR]
    appears twice. */
#define BRANCHES_NEXT(B_PTR) (((void*)(B_PTR)) + BRANCHES_SIZE(*(B_PTR)))

/** PREFIX nodes (size = 4 bytes)

A single branch, consuming up to 3 bytes from the query.
The 'prefix' field is interpreted as follow:
The first byte is always part of the prefix. The next bytes are only part of
the prefix if they are not NUL, (where XX can be anything, including 0):
XX 00 00  A 1 byte prefix
XX ++ 00  A 2 byte prefix
XX ++ ++  A 3 byte prefix
This means that a word containing a long string of NUL byte is very
inefficiently encoded.

The 'next_kind' field is interpreted as the kind of the node that follows, it's
a value of type 'kind_t'. The node that follows starts just after the prefix
node.

If 'next_kind' is LEAF, what follows is the pointer to the leaf node instead.
*/

#define PREFIX_NODE_LENGTH 3

typedef struct
{
  char prefix[PREFIX_NODE_LENGTH];
  char next_kind;
  ptr_t leaf[]; /** Present only if 'next_kind = LEAF'. */
} prefix_t;

/** BTREE nodes (size = 8 bytes to 40 bytes)

A branching node where the branch labels are stored in a binary search tree.
First search for the current byte prefix into 'labels' then lookup the
corresponding pointer in 'next'.

This node doesn't support NUL prefixes, finding a NUL in 'labels' means that
the end of tree was reached.
*/

#define BTREE_NODE_LENGTH 8

typedef struct
{
  char labels[BTREE_NODE_LENGTH];
  ptr_t next[];
} btree_t;

/** WITH_LEAF nodes (size = 4 bytes)

A node that doesn't consume any prefix and is treated as a leaf if it is
encountered at the end of the query.
The next node is at offset [+ sizeof(with_leaf_t)]. The 'leaf' field is not a
pointer to a leaf, instead it is composed of:
- The 29 highest bits are the leaf data, just like in a leaf pointer.
- The 3 lowest bits are the kind of the next node. It is never equal to 'LEAF'.
*/

typedef struct
{
  int32_t leaf; // Not a ptr_t
} with_leaf_t;

#define WITH_LEAF_LEAF(PTR) (PTR_OFFSET(((with_leaf_t const*)(PTR))->leaf) | LEAF)
#define WITH_LEAF_KIND(PTR) PTR_KIND(((with_leaf_t const*)(PTR))->leaf)

/** Dictionary header (size = 4 bytes + 4 bytes of padding)

Located at the beginning of the dictionary.
*/

typedef struct
{
  ptr_t root_ptr;
} header_t;
