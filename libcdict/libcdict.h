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
  char const *name;
  void const *root_node;
  uint8_t const *freq;
} cdict_t;

/** Dictionary files can contain several dictionaries. */
typedef struct
{
  cdict_t const *dicts;
  /** The dictionary file can contain several dictionaries. The main dictionary
      is called "main". */
  int n_dicts; /** Length of the [names] and [dicts] array. */
  int total_size; /** Total size in bytes. */
} cdict_header_t;

typedef enum
{
  CDICT_OK, // The dictionary was succesfully loaded
  CDICT_NOT_A_DICTIONARY, // File is not a dictionary
  CDICT_UNSUPPORTED_FORMAT,
  // Dictionary was created for an incompatible version of the library
} cdict_cnstr_result_t;

/** Create a in-memory dictionary from a string of a given size. The string is
    copied. Allocate a single slot in memory and write a pointer to it into
    [dst]. Free using [free]. Returns [CDICT_OK] on success or an error code if
    the dictionary seems corrupted. */
cdict_cnstr_result_t cdict_of_string(char const *data, int size,
    cdict_header_t const **dst);

/** Text description of an error for use in exceptions and logs. */
char const* cdict_cnstr_result_to_string(cdict_cnstr_result_t r);

/** Return value of [cdict_find]. */
typedef struct
{
  bool found; /** Whether the query is recognized. */
  int index;
  /** Unique index of the recognized word or [-1] if the query is not
      recognized. Find the corresponding freq at [dict->freq[index]]. */
  intptr_t prefix_ptr;
  /** Internal node where the search stopped. Use [cdict_suffixes] to list
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

/** List the words starting with the word first queried with [cdict_find].
    This can be used even if [result->found] is false. Write up to [count] word
    indexes to [dst]. Return the number of word indexes written to [dst]. */
int cdict_suffixes(cdict_t const *dict, cdict_result_t const *r, int *dst,
    int count);

/** Lists words that are a [dist] editions away from the [word] according to
    Levenshtein distance. Do not return words that have a distance less than
    [dist]. The [count] most frequent words are returned. Adding suffixes of
    any length is considered to be a single edition. The returned array cannot
    contain more than [count] elements but might be smaller. */
int cdict_distance(cdict_t const *dict, char const *word, int wlen, int dist,
    int *dst, int count);

/** Version of the dictionary's format. Dictionaries built for a different
    version are not compatible. */
int cdict_format_version();
