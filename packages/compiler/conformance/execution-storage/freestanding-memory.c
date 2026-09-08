/* Memory operations supplied by the independent freestanding receiver. */
typedef __SIZE_TYPE__ size_t;
#define NULL ((void *)0)
#define printf(...) ((void)0)
#define fprintf(...) ((void)0)
void *memcpy(void *destination, const void *source, size_t size) {
  unsigned char *out = destination;
  const unsigned char *in = source;
  for (size_t i = 0; i < size; ++i) out[i] = in[i];
  return destination;
}
void *memset(void *destination, int value, size_t size) {
  unsigned char *out = destination;
  for (size_t i = 0; i < size; ++i) out[i] = (unsigned char)value;
  return destination;
}
void *memmove(void *destination, const void *source, size_t size) {
  unsigned char *out = destination;
  const unsigned char *in = source;
  if ((__UINTPTR_TYPE__)out <= (__UINTPTR_TYPE__)in) return memcpy(destination, source, size);
  for (size_t i = size; i > 0; --i) out[i - 1] = in[i - 1];
  return destination;
}
int memcmp(const void *left, const void *right, size_t size) {
  const unsigned char *a = left, *b = right;
  for (size_t i = 0; i < size; ++i) {
    if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
  }
  return 0;
}
