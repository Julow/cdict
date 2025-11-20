/** libdict

This file describes the encoding of the dictionary.
*/

#include <stdint.h>

typedef enum
{
  BRANCHES = 0b0,
  PREFIX = 0b1,
} kind_t;

/** Whether the pointer is a final transition. */
#define PTR_FLAG_FINAL 0b10

// Kind and flags (2 bits)
#define PTR_KIND_MASK 0b1u
#define PTR_FLAGS_MASK PTR_FLAG_FINAL

#define PTR_KIND(PTR) (kind_t)((PTR) & PTR_KIND_MASK)
#define PTR_IS_FINAL(PTR) (bool)((PTR) & PTR_FLAG_FINAL)

// Offset field
#define PTR_OFFSET_MASK (~(PTR_KIND_MASK | PTR_FLAGS_MASK))

#define PTR_NODE(PTR, PARENT_NODE) (((void const*)(PARENT_NODE)) + (int)((PTR) & PTR_OFFSET_MASK))

/** Sized integer arrays. */

/** Format of integer arrays. All formats are big-endians. This is also the
    size in bytes of each element. */
typedef enum
{
  FORMAT_8_BITS = 1,
  FORMAT_16_BITS = 2,
  FORMAT_24_BITS = 3,
} format_t;

/** Mask masking every values of [format_t]. */
#define MAX_FORMAT_T 0b11

/** Access the [i]th unsigned integer in array [ar] of format [fmt]. */
static inline int sized_int_array_unsigned(uint8_t const *ar, format_t fmt, int i)
{
  if (fmt == FORMAT_8_BITS)
    return ar[i];
  ar = ar + fmt * i;
  if (fmt == FORMAT_16_BITS)
    return ((ar[0] << 8) | ar[1]);
  return (int)(unsigned)((ar[0] << 16) | (ar[1] << 8) | ar[2]);
}

/** Access the [i]th unsigned integer in array [ar] of format [fmt]. */
static inline int sized_int_array_signed(uint8_t const *ar, format_t fmt, int i)
{
  if (fmt == FORMAT_8_BITS)
    return (int)(int8_t)ar[i];
  ar = ar + fmt * i;
  if (fmt == FORMAT_16_BITS)
    return (((int)((int8_t)ar[0]) << 8) | ar[1]);
  return (int)(((int8_t)ar[0] << 16) | (ar[1] << 8) | ar[2]);
}

/** BRANCHES nodes (size = 4 bytes + array)

A branching node that consumes 1 byte from the query. The branch labels are
stored in a binary search tree.
First search for the current byte prefix into 'labels' then lookup the
corresponding pointer in 'branches' and number in 'numbers'

Branches are not NULL and there is no padding within the node.
*/

typedef struct
{
  uint8_t header;
  /** Encode the format of the 'branches' and 'numbers' array. */
  uint8_t length;
  /** Length of the 'labels' and 'branches' arrays. Not the offset to the
      'branches' array. */
  char labels[];
  // uint8_t branches[]; /** Use [branch(b, i)] to access. */
  // uint8_t numbers[]; /** Use [branch_number(b, i)] to access. */
} branches_t;

#define BRANCHES_BRANCHES_FORMAT_OFFSET 2
#define BRANCHES_NUMBERS_FORMAT_OFFSET 0

#define BRANCHES_BRANCHES_FORMAT(B) \
  ((format_t)(((B)->header >> BRANCHES_BRANCHES_FORMAT_OFFSET) & MAX_FORMAT_T))

#define BRANCHES_NUMBERS_FORMAT(B) \
  ((format_t)(((B)->header >> BRANCHES_NUMBERS_FORMAT_OFFSET) & MAX_FORMAT_T))

/** Access a branch. */
static inline int branch(branches_t const *b, int i)
{
  uint8_t const *ar = ((void const*)b) + (sizeof(branches_t) + b->length);
  return sized_int_array_signed(ar, BRANCHES_BRANCHES_FORMAT(b), i);
}

/** Access a number. */
static inline unsigned int branch_number(branches_t const *b, int i)
{
  uint8_t const *ar = ((void const*)b) +
    (sizeof(branches_t) + b->length * (BRANCHES_BRANCHES_FORMAT(b) + 1));
  return sized_int_array_unsigned(ar, BRANCHES_NUMBERS_FORMAT(b), i);
}

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
