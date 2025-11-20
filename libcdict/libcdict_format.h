/** libdict

This file describes the encoding of the dictionary.

The main concepts are nodes and tagged pointers.

Nodes form the data structure and a tagged pointers reference a node.
There are several kinds of nodes.

Tagged pointers (ptr_t) are 32 bits integers with the following fields:
- offset: Signed integer representing the relative offset to the next node.
  The offset is relative to the start of the parent node.
- number: Used to increment the "index" value during traversal.
- kind: Kind of the next node.
- final: Flag final transition.

NULL pointers are not allowed unless specified.
*/

#include <stdint.h>

typedef int32_t ptr_t;

typedef enum
{
  BRANCHES = 0b0,
  PREFIX = 0b1,
} kind_t;

/** Whether the pointer is a final transition. */
#define PTR_FLAG_FINAL 0b10

// Number field (8 bits unsigned integer)
#define MAX_PTR_NUMBER 0xFFu
#define PTR_NUMBER_OFFSET 3
#define PTR_NUMBER_MASK (MAX_PTR_NUMBER << PTR_NUMBER_OFFSET)

#define PTR_NUMBER(PTR) ((int)(((PTR) & PTR_NUMBER_MASK) >> PTR_NUMBER_OFFSET))

// Kind and flags (2 bits)
#define PTR_KIND_MASK 0b1u
#define PTR_FLAGS_MASK PTR_FLAG_FINAL

#define PTR_KIND(PTR) (kind_t)((PTR) & PTR_KIND_MASK)
#define PTR_IS_FINAL(PTR) (bool)((PTR) & PTR_FLAG_FINAL)

// Offset field (21 bits signed integer)
#define PTR_OFFSET_OFFSET 11
#define PTR_OFFSET_MASK (~(PTR_KIND_MASK | PTR_FLAGS_MASK | PTR_NUMBER_MASK))

#define PTR_NODE(PTR, PARENT_NODE) (((void const*)(PARENT_NODE)) + ((PTR) >> PTR_OFFSET_OFFSET))

/** BRANCHES nodes (size = 4 bytes + array)

A branching node that consumes 1 byte from the query. The branch labels are
stored in a binary search tree.
First search for the current byte prefix into 'labels' then lookup the
corresponding pointer in 'branches'.
*/

typedef struct
{
  uint8_t header;
  /** Encode the format of the 'numbers' array. */
  uint8_t length;
  /** Length of the 'labels' and 'branches' arrays. Not the offset to the
      'branches' array. */
  char labels[];
  // ptr_t branches[]; /** Use [BRANCHES(b)] to access. */
  // char numbers_256[];
  // /** Number divided by MAX_PTR_NUMBER. Use [NUMBERS(b)] to access. */
} branches_t;

/** Pointer to the 'branches' array. */
#define BRANCHES(B) ((ptr_t const*)(((void const*)(B)) + \
    ((sizeof(branches_t) + (B)->length + 3) & -4)))

/** Pointer to the 'numbers' array. Format depends on the 'header'. */
#define BRANCHES_NUMBERS(B) ((uint8_t const *)(((void const*)(B)) + \
      ((sizeof(branches_t) + (B)->length * (1 + sizeof(ptr_t)) + 3) & -4)))

/** Format of the 'numbers' array.
    The numbers array is accessed using [BRANCHES_NUMBERS(B)] and the size of
    the elements depend on the format. */
typedef enum
{
  NUMBERS_NONE = 0,
  NUMBERS_8_BITS = 1,
  NUMBERS_16_BITS = 2,
  NUMBERS_24_BITS = 3,
} branches_numbers_format_t;

#define BRANCHES_NUMBERS_FORMAT_MASK 0b11
#define BRANCHES_NUMBERS_FORMAT_BYTE_LENGTH(F) ((int)(F))

/** PREFIX nodes (size = 8 bytes)

A single branch, consuming a prefix of the query.
The 'next' field contains the pointer to the next node and the length of the
'prefix' array. The number field is used to store the length. The pointer is
considered to have a number field equal to 0.

The length cannot be 0.
*/

typedef struct
{
  uint8_t next_ptr[3]; /** 24-bits big-endian integer. */
  uint8_t length;
  char prefix[];
} prefix_t;

#define PREFIX_MAX_LENGTH 0xFF

/** Prefix pointer

This is a pointer to a node, with the node kind embedded.
It is exposed in 'cdict_result_t' but not used outside of the library.

'prefix_ptr' can be NULL.
*/

#define PREFIX_PTR_NODE(P) ((void const*)((P) & ~(intptr_t)PTR_KIND_MASK))
#define PREFIX_PTR_PTR(P) ((kind_t)((P) & PTR_KIND_MASK))

#define PREFIX_PTR(NODE, PTR) (((intptr_t)(NODE)) | ((PTR) & PTR_KIND_MASK))

/** Dictionary header (size = 8 bytes)

Located at the beginning of the dictionary.
*/

typedef struct
{
  ptr_t root_ptr;
  int32_t freq_off;
  /** Offset to the 4-bits integer array storing the frequency of each words. */
} header_t;
