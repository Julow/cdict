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

// C to OCaml
static value alloc_result(cdict_result_t const *r)
{
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_tuple(3);
  Store_field(v, 0, Val_bool(r->found));
  Store_field(v, 1, Val_int(r->index));
  Store_field(v, 2, Val_int(r->prefix_ptr));
  CAMLreturn(v);
}

// OCaml to C
static void result_of_value(value v, cdict_result_t *dst)
{
  dst->found = Bool_val(Field(v, 0));
  dst->index = Int_val(Field(v, 1));
  dst->prefix_ptr = Int_val(Field(v, 2));
}

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
  cdict_result_t result;
  int s_len = caml_string_length(str);
  cdict_find(CDICT_VAL(dict), String_val(str), s_len, &result);
  CAMLreturn(alloc_result(&result));
}

value cdict_freq_ocaml(value dict, value index)
{
  CAMLparam2(dict, index);
  CAMLreturn(Val_int(cdict_freq(CDICT_VAL(dict), Int_val(index))));
}

value cdict_word_ocaml(value dict, value index)
{
  CAMLparam2(dict, index);
  int const max_len = 15;
  char dst[max_len + 1];
  int len = cdict_word(CDICT_VAL(dict), Int_val(index), dst, max_len);
  dst[len] = '\0';
  CAMLreturn(caml_copy_string(dst));
}

value cdict_list_prefix_ocaml(value dict, value result, value length)
{
  CAMLparam3(dict, result, length);
  CAMLlocal1(array);
  int dst_len = Int_val(length);
  int dst[dst_len];
  cdict_result_t r;
  result_of_value(result, &r);
  int final_len = cdict_list_prefix(CDICT_VAL(dict), &r, dst, dst_len);
  array = caml_alloc_tuple(final_len);
  for (int i = 0; i < final_len; i++)
    Store_field(array, i, Val_int(dst[i]));
  CAMLreturn(array);
}

value cdict_distance_ocaml(value dict, value word, value dist, value count)
{
  CAMLparam4(dict, word, dist, count);
  CAMLlocal1(array);
  int dst_len = Int_val(count);
  int dst[dst_len];
  int final_len =
    cdict_distance(CDICT_VAL(dict), String_val(word), caml_string_length(word),
        Int_val(dist), dst, dst_len);
  array = caml_alloc_tuple(final_len);
  for (int i = 0; i < final_len; i++)
    Store_field(array, i, Val_int(dst[i]));
  CAMLreturn(array);
}
