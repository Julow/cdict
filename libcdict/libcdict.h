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
  char const *data;
  size_t size;
} cdict_t;

/** Create a in-memory dictionary from a string of a given size. The string is
    not copied and must remain valid until the dictionary stops being used. */
cdict_t cdict_of_string(char const *data, int size);

/** Lookup the given word of the given size in the dictionary. Returns [true]
    if the word is in the dictionary, [false] otherwise.
    If the function returned [true] and [leaf] is not NULL, the leaf data is
    written at [leaf]. */
bool cdict_find(cdict_t const *dict, char const *word, int word_size, int32_t *leaf);
