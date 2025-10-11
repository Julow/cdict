#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <string.h>
#include <libcdict.h>

#define CDICT_VAL(v) ((cdict_t*)Data_custom_val(v))

static struct custom_operations const cdict_t_ops = {
  "Cdict.t",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value cdict_of_string_ocaml(value str)
{
  CAMLparam1(str);
  CAMLlocal1(r);
  // We must copy the string to ensure that it doesn't move during GC.
  // The string is copied into the same custom block, after the cdict_t struct.
  int s_len = caml_string_length(str);
  r = caml_alloc_custom_mem(&cdict_t_ops, sizeof(cdict_t) + s_len, 0);
  void *data = ((void*)Data_custom_val(r)) + sizeof(cdict_t);
  memcpy(data, String_val(str), s_len);
  *CDICT_VAL(r) = cdict_of_string(data, s_len);
  CAMLreturn(r);
}

value cdict_find_ocaml(value dict, value str)
{
  CAMLparam2(dict, str);
  CAMLlocal1(r);
  r = Val_none;
  int leaf = 0;
  int s_len = caml_string_length(str);
  if (cdict_find(CDICT_VAL(dict), String_val(str), s_len, &leaf))
    r = caml_alloc_some(Val_int(leaf));
  CAMLreturn(r);
}
