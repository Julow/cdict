#include <libcdict_format.h>
#include <stddef.h>
#include <stdio.h>

static int indent = 0;
#define P_LINE(FMT, ...) printf("%*s" FMT "\n", indent * 2, "", ##__VA_ARGS__)

static void module(char const *name)
{
  P_LINE("module %s = struct", name);
  indent++;
}

static void end()
{
  indent--;
  P_LINE("end");
}

#define FIELD(TYPE, FIELD) P_LINE("let %s = %d", #TYPE "_" #FIELD, (int)offsetof(TYPE, FIELD))
#define SIZE(TYPE) P_LINE("let %s = %d", #TYPE, (int)sizeof(TYPE))
#define VAL(PREFIX, VAL) P_LINE("let %s = %d", #PREFIX #VAL, (int)VAL)
#define VAL_INT32(PREFIX, VAL) P_LINE("let %s = %dl", #PREFIX #VAL, (int32_t)VAL)

int main()
{
  module("C");

  VAL(c_, MAX_PTR_NUMBER);
  VAL(c_, PTR_NUMBER_OFFSET);
  VAL(c_, PTR_OFFSET_OFFSET);
  VAL(c_, NUMBERS_NONE);
  VAL(c_, NUMBERS_8_BITS);
  VAL(c_, NUMBERS_16_BITS);
  VAL_INT32(mask_, PTR_KIND_MASK);
  VAL_INT32(mask_, PTR_FLAGS_MASK);
  VAL_INT32(mask_, PTR_OFFSET_MASK);
  VAL_INT32(mask_, PTR_NUMBER_MASK);
  VAL_INT32(mask_, BRANCHES_NUMBERS_FORMAT_MASK);
  VAL_INT32(flag_, PTR_FLAG_FINAL);
  VAL_INT32(tag_, BRANCHES);
  VAL_INT32(tag_, PREFIX);

  end();
  module("O");

  FIELD(branches_t, header);
  FIELD(branches_t, length);
  FIELD(branches_t, labels);
  FIELD(prefix_t, next_ptr_and_len);
  FIELD(prefix_t, prefix);
  FIELD(header_t, root_ptr);
  FIELD(header_t, freq_off);

  end();
  module("S");

  SIZE(ptr_t);
  SIZE(branches_t);
  SIZE(prefix_t);
  SIZE(header_t);

  end();
  return 0;
}
