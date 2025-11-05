/** libdict

    This library implements a compact dictionary as a Radix Tree. Words are byte
    strings of arbitrary encoding, the alphabet size is 256.
    Several techniques are used to make the dictionary as small as possible.
*/

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct
{
  void const *data;
  size_t size;
  int32_t root_ptr;
  uint8_t const *freq;
} cdict_t;

/** Create a in-memory dictionary from a string of a given size. The string is
    not copied and must remain valid until the dictionary stops being used. */
cdict_t cdict_of_string(char const *data, int size);

/** Return value of [cdict_find]. */
typedef struct
{
  bool found; /** Whether the query is recognized. */
  int index;
  /** Unique index of the recognized word or [-1] if the query is not
      recognized. Find the corresponding freq at [dict->freq[index]]. */
  uint32_t prefix_ptr;
  /** Internal node where the search stopped. Use [cdict_list_prefix] to list
      the words starting with this prefix. Might be [0], in which case the
      queried is not the prefix of any word in the dictionary. */
} cdict_result_t;

/** Lookup the given word of the given size in the dictionary.
    Write its result to [result]. */
void cdict_find(cdict_t const *dict, char const *word, int word_size,
    cdict_result_t *result);

/** Frequency associated to a word. [index] is the corresponding field in
    [cdict_result_t]. */
int cdict_freq(cdict_t const *dict, int index);

/** Retrieve the word at the given index. Returns the number of chars written
    to [dst]. Do not write a NUL byte at the end of [dst]. */
int cdict_word(cdict_t const *dict, int index, char *dst, int max_length);

/** List the words starting with the prefix first queried with [cdict_find].
    This can be used even if [result->found] is false. Write up to [count] word
    indexes to [dst]. Return the number of word indexes written to [dst]. */
int cdict_list_prefix(cdict_t const *dict, cdict_result_t const *r, int *dst,
    int count);
