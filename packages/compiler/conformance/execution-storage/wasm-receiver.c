/* Freestanding, independently compiled wasm32 caller and allocator. */
typedef __SIZE_TYPE__ size_t;
typedef __UINTPTR_TYPE__ uintptr_t;
_Static_assert(sizeof(size_t) == 4 && sizeof(void *) == 4, "wasm32 C widths");
struct reservation { struct reservation *next; unsigned char *payload; size_t charged; };
struct accounting { struct reservation *head; size_t used; size_t limit; };
_Static_assert(sizeof(struct reservation) == 12, "reservation layout");
_Static_assert(sizeof(struct accounting) == 12, "accounting layout");
_Static_assert(__builtin_offsetof(struct reservation, payload) == 4, "payload offset");
_Static_assert(__builtin_offsetof(struct accounting, used) == 4, "accounting offset");
extern unsigned char *silk_execution_storage_create(void);
extern unsigned char *silk_execution_storage_acquire(unsigned char *, size_t, size_t);
extern void silk_execution_storage_release(unsigned char *, unsigned char *);
extern void silk_execution_storage_destroy(unsigned char *);

static _Alignas(4096) unsigned char arena[65536];
static void *addresses[32];
static unsigned char live[32];
static size_t cursor;
static int calls, selected, count, invalid;
void *malloc(size_t size) {
  if (++calls == selected) return 0;
  size_t aligned = (cursor + 63) & ~(size_t)63;
  if (size > sizeof(arena) - aligned || count == 32) return 0;
  void *address = arena + aligned;
  cursor = aligned + size;
  addresses[count] = address;
  live[count++] = 1;
  return address;
}
void free(void *pointer) {
  if (!pointer) return;
  for (int i = 0; i < count; ++i) {
    if (addresses[i] == pointer) {
      if (!live[i]) invalid = 1;
      live[i] = 0;
      return;
    }
  }
  invalid = 1;
}
void *memset(void *destination, int value, size_t length) {
  unsigned char *bytes = destination;
  for (size_t i = 0; i < length; ++i) bytes[i] = (unsigned char)value;
  return destination;
}
void *memcpy(void *destination, const void *source, size_t length) {
  unsigned char *out = destination;
  const unsigned char *in = source;
  for (size_t i = 0; i < length; ++i) out[i] = in[i];
  return destination;
}
static int exercise(int failure) {
  cursor = 0; calls = 0; count = 0; invalid = 0; selected = failure;
  memset(live, 0, sizeof(live));
  unsigned char *first = silk_execution_storage_create();
  unsigned char *second = first ? silk_execution_storage_create() : 0;
  unsigned char *a = second ? silk_execution_storage_acquire(first, 31, 256) : 0;
  unsigned char *b = a ? silk_execution_storage_acquire(first, 127, 4096) : 0;
  unsigned char *c = b ? silk_execution_storage_acquire(second, 63, 64) : 0;
  if (!failure && (!c || first == second)) return 1;
  if ((a && (uintptr_t)a % 256) || (b && (uintptr_t)b % 4096) ||
      (c && (uintptr_t)c % 64)) return 2;
  if (first && (silk_execution_storage_acquire(first, (size_t)-1, 4096) ||
                silk_execution_storage_acquire(first, 1, 3) ||
                silk_execution_storage_acquire(first, 0, 1))) return 3;
  if (a) memset(a, 0xab, 31);
  if (b) memset(b, 0xbc, 127);
  if (c) memset(c, 0xcd, 63);
  if (a) silk_execution_storage_release(first, a);
  if ((b && b[126] != 0xbc) || (c && c[62] != 0xcd)) return 4;
  if (c) silk_execution_storage_release(second, c);
  if (second) silk_execution_storage_destroy(second);
  if (b && b[126] != 0xbc) return 5;
  if (b) silk_execution_storage_release(first, b);
  if (first) silk_execution_storage_destroy(first);
  for (int i = 0; i < count; ++i) if (live[i]) invalid = 1;
  if (invalid || (failure && calls != failure)) return 6;
  return 0;
}
int storage_probe(void) {
  if (exercise(0)) return 1;
  for (int i = 1; i <= 5; ++i) if (exercise(i)) return 2;
  return 42;
}
