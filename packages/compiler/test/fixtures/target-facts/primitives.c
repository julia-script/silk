/* Header-free primitive machine-fact witness. No Silk-generated declarations or host headers. */
#define FACT(T) sizeof(T), _Alignof(T)
const unsigned int silk_primitive_facts[] = {
  FACT(unsigned int), FACT(_Bool), FACT(signed char), FACT(short), FACT(int), FACT(long long),
  FACT(float), FACT(double), FACT(void *), FACT(long)
};
/* Silk stores bool as an i32 lane; C _Bool has its own distinct storage fact. */
_Static_assert(sizeof(unsigned int) == 4 && _Alignof(unsigned int) == 4, "silk bool storage");
_Static_assert(sizeof(_Bool) == 1 && _Alignof(_Bool) == 1, "bool");
_Static_assert(sizeof(signed char) == 1 && _Alignof(signed char) == 1, "i8");
_Static_assert(sizeof(short) == 2 && _Alignof(short) == 2, "i16");
_Static_assert(sizeof(int) == 4 && _Alignof(int) == 4, "i32");
_Static_assert(sizeof(long long) == 8 && _Alignof(long long) == 8, "i64");
_Static_assert(sizeof(float) == 4 && _Alignof(float) == 4, "f32");
_Static_assert(sizeof(double) == 8 && _Alignof(double) == 8, "f64");
#if defined(__wasm32__)
_Static_assert(sizeof(void *) == 4 && _Alignof(void *) == 4 && sizeof(long) == 4, "ILP32");
#else
_Static_assert(sizeof(void *) == 8 && _Alignof(void *) == 8 && sizeof(long) == 8, "LP64");
#endif
#if __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#error Initial Silk targets are little endian
#endif
