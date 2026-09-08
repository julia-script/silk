#include <stdio.h>
#include <string.h>
int main(void) {
  unsigned char *first = silk_execution_storage_create();
  unsigned char *second = silk_execution_storage_create();
  if (!first || !second || first == second) return 1;
  unsigned char *a = silk_execution_storage_acquire(first, 31, 256);
  unsigned char *b = silk_execution_storage_acquire(first, 127, 4096);
  unsigned char *c = silk_execution_storage_acquire(second, 63, 64);
  if (!a || !b || !c || (uintptr_t)a % 256 || (uintptr_t)b % 4096 || (uintptr_t)c % 64) return 2;
  memset(a, 0xab, 31); memset(b, 0xbc, 127); memset(c, 0xcd, 63);
  if (silk_execution_storage_acquire(first, SIZE_MAX, 4096) ||
      silk_execution_storage_acquire(first, 1, 3) ||
      silk_execution_storage_acquire(first, 0, 1)) return 3;
  silk_execution_storage_release(first, a); /* Non-LIFO release must unlink the correct reservation. */
  if (b[126] != 0xbc || c[62] != 0xcd) return 4;
  silk_execution_storage_release(second, c);
  silk_execution_storage_destroy(second);
  if (b[126] != 0xbc) return 5;
  silk_execution_storage_release(first, b);
  silk_execution_storage_destroy(first);
  puts("independent source storage states, aligned reservations, non-LIFO release and refusal passed");
  return 42;
}
