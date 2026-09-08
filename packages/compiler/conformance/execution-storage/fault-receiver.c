#include <stdio.h>
#include <string.h>
static _Alignas(4096) unsigned char arena[65536];
static void *addresses[32];
static unsigned char live[32];
static size_t cursor;
static int calls, selected, count, invalid;
void *malloc(size_t size) {
  if (++calls == selected) return NULL;
  size_t aligned = (cursor + 63) & ~(size_t)63;
  if (size > sizeof(arena) - aligned || count == 32) return NULL;
  void *address = arena + aligned;
  cursor = aligned + size;
  addresses[count] = address; live[count++] = 1;
  return address;
}
void free(void *pointer) {
  if (!pointer) return;
  for (int i = 0; i < count; ++i) {
    if (addresses[i] == pointer) {
      if (!live[i]) invalid = 1;
      live[i] = 0; return;
    }
  }
  invalid = 1;
}
static int exercise(int fail_at) {
  cursor = 0; calls = 0; count = 0; invalid = 0; selected = fail_at;
  memset(live, 0, sizeof(live));
  unsigned char *first = silk_execution_storage_create();
  unsigned char *second = first ? silk_execution_storage_create() : NULL;
  unsigned char *a = second ? silk_execution_storage_acquire(first, 64, 256) : NULL;
  unsigned char *b = a ? silk_execution_storage_acquire(first, 64, 4096) : NULL;
  unsigned char *c = b ? silk_execution_storage_acquire(second, 64, 64) : NULL;
  if (!fail_at && !c) return 1;
  if (a) {memset(a, 0xab, 64); if (b) memset(b, 0xbc, 64);}
  if (a) silk_execution_storage_release(first, a);
  if (b && b[63] != 0xbc) return 2;
  if (b) silk_execution_storage_release(first, b);
  if (c) silk_execution_storage_release(second, c);
  if (second) silk_execution_storage_destroy(second);
  if (first) silk_execution_storage_destroy(first);
  for (int i = 0; i < count; ++i) if (live[i]) invalid = 1;
  if (invalid || (fail_at && calls != fail_at)) return 3;
  return 0;
}
int main(void) {
  if (exercise(0)) return 1;
  for (int i = 1; i <= 5; ++i) if (exercise(i)) return 2;
  puts("every state/frame allocation refusal preserves prior reservations and releases each acquired allocation once");
  return 42;
}
