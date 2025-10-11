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
} kind_t;

/** Partial cast to [kind_t]. */
#define PTR_KIND(PTR) (kind_t)((PTR) & 0b111)
#define PTR_OFFSET(PTR) ((PTR) & ~0b111)

/** LEAF nodes (size = 0)

Leaf nodes occupie no space in the dictionary, instead the 29 highest bits
represent arbitrary metadata.
The presence of a leaf node indicates that the word is present in the
dictionary.
*/

#define LEAF_DATA(PTR) ((PTR) >> 3)

/** BRANCHES nodes (size = 8 bytes + array)

A branching node that consumes 8 bits from the query.

The 'branches' fields is an array of node pointers indexed by the first byte of
the query, minus the 'low' value.
Node pointers are NULL when the queried word is not in the dictionary.
If the first byte of the query is greater than 'low + length - 1'
and 'has_next' is 1, the query should be tried again on the 8-bits node at
the offset BRANCHES_NEXT(B). However, if 'has_next' is 0, no prefix higher than
'low + length - 1' are present in the dictionary.

The 'low', 'length' and 'has_next' can be used during construction to
remove regions or NULL pointers at the beginning and at the end of the
array, as well as inside the array if that reduces the total size.
*/

typedef struct
{
  ptr_or_null_t leaf;
  /** A pointer to a leaf if a word is present in the dictionary at the current
      location, or 0. Pointers to other kind of nodes are not allowed. */
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

/** PREFIX nodes (size = 8 bytes)

A single branch, consuming up to a 4 bytes from the query.
The 'prefix' field is interpreted as follow:
The first byte is always part of the prefix. The next bytes are only part of
the prefix if they are not NUL, (where XX can be anything, including 0):
XX 00 00 00  A 1 byte prefix
XX ++ 00 00  A 2 byte prefix
XX ++ ++ 00  A 3 byte prefix
XX ++ ++ ++  A 4 byte prefix
This means that a word containing a long string of NUL byte is very
inefficiently encoded.
*/

typedef struct
{
  char prefix[4];
  ptr_t next;
} prefix_t;
