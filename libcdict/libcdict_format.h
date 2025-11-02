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

typedef uint32_t ptr_t;
typedef ptr_t ptr_or_null_t;

typedef enum
{
  BRANCHES = 0b000,
  PREFIX = 0b001,
  BTREE = 0b010,
  NUMBER = 0b011,
} kind_t;

#define PTR_NUMBER_MAX 0xFFu
#define PTR_NUMBER_OFFSET 24

#define PTR_KIND_MASK 0b011u
#define PTR_FLAGS_MASK 0b100u
#define PTR_NUMBER_MASK (PTR_NUMBER_MAX << PTR_NUMBER_OFFSET)
#define PTR_OFFSET_MASK (~(PTR_KIND_MASK | PTR_FLAGS_MASK | PTR_NUMBER_MASK))

/** Whether the pointer is a final transition. */
#define PTR_FLAG_FINAL 0b100u

#define PTR_KIND(PTR) (kind_t)((PTR) & PTR_KIND_MASK)
#define PTR_NUMBER(PTR) (((PTR) & PTR_NUMBER_MASK) >> PTR_NUMBER_OFFSET)
#define PTR_OFFSET(PTR) ((PTR) & PTR_OFFSET_MASK)
#define PTR_IS_FINAL(PTR) (bool)((PTR) & PTR_FLAG_FINAL)

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

/** PREFIX nodes (size = 8 bytes)

A single branch, consuming up to 3 bytes from the query.
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

#define PREFIX_NODE_LENGTH 4

typedef struct
{
  char prefix[PREFIX_NODE_LENGTH];
  ptr_t next;
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

/** NUMBER nodes (size = 8 bytes)

A non-branching node that stores a pointer to a next state. It is used to
workaround the limited storage for the 'number' field in pointers by adding an
indirection.
This node doesn't consume any prefix.

This node is never final.
*/

typedef struct
{
  int32_t number;
  ptr_t next;
} number_t;

/** Dictionary header (size = 8 bytes)

Located at the beginning of the dictionary.
*/

typedef struct
{
  ptr_t root_ptr;
  int32_t freq_off;
  /** Offset to the 4-bits integer array storing the frequency of each words. */
} header_t;
