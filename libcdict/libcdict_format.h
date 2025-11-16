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
  NUMBER = 0b011,
} kind_t;

#define MAX_PTR_NUMBER 0xFFu
#define PTR_NUMBER_OFFSET 24

#define PTR_KIND_MASK 0b011u
#define PTR_FLAGS_MASK 0b100u
#define PTR_NUMBER_MASK (MAX_PTR_NUMBER << PTR_NUMBER_OFFSET)
#define PTR_OFFSET_MASK (~(PTR_KIND_MASK | PTR_FLAGS_MASK | PTR_NUMBER_MASK))

/** Whether the pointer is a final transition. */
#define PTR_FLAG_FINAL 0b100u

#define PTR_KIND(PTR) (kind_t)((PTR) & PTR_KIND_MASK)
#define PTR_NUMBER(PTR) (int)(((PTR) & PTR_NUMBER_MASK) >> PTR_NUMBER_OFFSET)
#define PTR_OFFSET(PTR) ((PTR) & PTR_OFFSET_MASK)
#define PTR_IS_FINAL(PTR) (bool)((PTR) & PTR_FLAG_FINAL)

/** BRANCHES nodes (size = 4 bytes + array)

A branching node that consumes 1 byte from the query. The branch labels are
stored in a binary search tree.
First search for the current byte prefix into 'labels' then lookup the
corresponding pointer in 'branches'.
*/

typedef struct
{
  char length;
  /** Length of the 'labels' and 'branches' arrays. Not the offset to the
      'branches' array. */
  char labels[];
  // ptr_t branches[]; /** Use [BRANCHES(b)] to access. */
} branches_t;

/** Pointer to the 'branches' array. */
#define BRANCHES(B) ((ptr_t const*)(((void const*)(B)) + \
    ((sizeof(branches_t) + (B)->length + 3) & -4)))

/** PREFIX nodes (size = 8 bytes)

A single branch, consuming a prefix of the query.
The 'next' field contains the pointer to the next node and the length of the
'prefix' array. The number field is used to store the length. The pointer is
considered to have a number field equal to 0.

The length cannot be 0.
*/

typedef struct
{
  uint32_t next_ptr_and_len;
  char prefix[];
} prefix_t;

#define PREFIX_NEXT_PTR(P) ((P)->next_ptr_and_len & ~PTR_NUMBER_MASK)
#define PREFIX_LENGTH(P) PTR_NUMBER((P)->next_ptr_and_len)

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
