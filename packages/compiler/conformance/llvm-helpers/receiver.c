#include <stddef.h>
#include <string.h>
#include <math.h>
#include <strings.h>
_Static_assert(sizeof(size_t) == 8 && sizeof(int) == 4, "helper scalar ABI");
_Static_assert(__builtin_types_compatible_p(__typeof__(&bcmp), int (*)(const void *, const void *, size_t)), "bcmp header signature");
_Static_assert(__builtin_types_compatible_p(__typeof__(&bzero), void (*)(void *, size_t)), "bzero header signature");
extern int equal_fixture(const void *, const void *, size_t);
extern void zero_fixture(void *, size_t);

/* Header-typed volatile function pointers prevent C builtin folding from replacing this ABI test. */
static void *(*volatile copy_bytes)(void *, const void *, size_t) = memcpy;
static void *(*volatile move_bytes)(void *, const void *, size_t) = memmove;
static void *(*volatile fill_bytes)(void *, int, size_t) = memset;
static int (*volatile compare_bytes)(const void *, const void *, size_t) = memcmp;
extern double remainder_fixture(double, double);
extern float remainder_float_fixture(float, float);
extern void copy_fixture(void *, const void *, size_t);
extern void move_fixture(void *, const void *, size_t);
extern void fill_fixture(void *, unsigned char, size_t);
extern int compare_fixture(const void *, const void *, size_t);

int main(void) {
  unsigned char source[65], destination[65];
  for (unsigned i = 0; i < 65; ++i) { source[i] = (unsigned char)(i * 17); destination[i] = 0; }
  if (copy_bytes(destination, source, 65) != destination) return 1;
  for (unsigned i = 0; i < 65; ++i) if (source[i] != destination[i]) return 2;
  if (fill_bytes(destination + 1, -1, 63) != destination + 1) return 3;
  if (destination[0] != source[0] || destination[64] != source[64]) return 4;
  for (unsigned i = 1; i < 64; ++i) if (destination[i] != 255) return 5;
  copy_bytes(destination, source, 65);
  if (move_bytes(destination + 1, destination, 64) != destination + 1) return 6;
  for (unsigned i = 1; i < 65; ++i) if (destination[i] != source[i - 1]) return 7;
  if (move_bytes(destination, destination + 1, 64) != destination) return 8;
  for (unsigned i = 0; i < 64; ++i) if (destination[i] != source[i]) return 9;
  if (move_bytes(destination, destination, 65) != destination) return 10;
  if (copy_bytes(destination, source, 0) != destination || fill_bytes(destination, 0, 0) != destination || compare_bytes(destination, source, 0) != 0) return 11;
  unsigned char low[] = {0, 127}, high[] = {0, 255};
  if (compare_bytes(low, high, 2) >= 0 || compare_bytes(high, low, 2) <= 0 || compare_bytes(low, low, 2) != 0) return 12;
  copy_fixture(destination, source, 65);
  if (compare_fixture(destination, source, 65) != 0) return 13;
  move_fixture(destination + 1, destination, 64);
  if (destination[64] != source[63]) return 14;
  fill_fixture(destination, 37, 65);
  for (unsigned i = 0; i < 65; ++i) if (destination[i] != 37) return 15;
  if (remainder_fixture(-5.5, 2.0) != -1.5 || remainder_float_fixture(5.5f, -2.0f) != 1.5f) return 16;
  if (!signbit(remainder_fixture(-4.0, 2.0)) || !signbit(remainder_float_fixture(-4.0f, 2.0f))) return 17;
  if (!isnan(remainder_fixture(1.0, 0.0)) || !isnan(remainder_float_fixture(__builtin_inff(), 2.0f))) return 18;
  if (equal_fixture(source, source, 65) != 1 || equal_fixture(low, high, 2) != 0) return 19;
  zero_fixture(destination, 65);
  for (unsigned i = 0; i < 65; ++i) if (destination[i] != 0) return 20;
  return 42;
}
