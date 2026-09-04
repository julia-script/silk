/** Private freestanding runtime linked only into LLVM-generated WebAssembly modules. */

export const source = `typedef __SIZE_TYPE__ size_t;
typedef __UINTPTR_TYPE__ uintptr_t;

extern unsigned char __heap_base;

static uintptr_t silk_heap_cursor_v1 = 0;

static int silk_grow_memory_v1(uintptr_t required) {
  const size_t page_size = 65536;
  const size_t current_pages = __builtin_wasm_memory_size(0);
  if (current_pages > ((__SIZE_MAX__) / page_size)) return 0;
  const uintptr_t current_bytes = current_pages * page_size;
  if (required <= current_bytes) return 1;
  const uintptr_t missing = required - current_bytes;
  const size_t pages = (missing + page_size - 1) / page_size;
  return __builtin_wasm_memory_grow(0, pages) != (__SIZE_TYPE__)-1;
}

void *malloc(size_t size) {
  if (silk_heap_cursor_v1 == 0) silk_heap_cursor_v1 = (uintptr_t)&__heap_base;
  if (size == 0) size = 1;
  const uintptr_t aligned = (silk_heap_cursor_v1 + 15) & ~((uintptr_t)15);
  if (size > (__UINTPTR_MAX__) - aligned) return (void *)0;
  const uintptr_t next = aligned + size;
  if (!silk_grow_memory_v1(next)) return (void *)0;
  silk_heap_cursor_v1 = next;
  return (void *)aligned;
}

void free(void *allocation) { (void)allocation; }

__attribute__((optnone)) void *memcpy(void *destination, const void *source, size_t count) {
  unsigned char *destination_bytes = (unsigned char *)destination;
  const unsigned char *source_bytes = (const unsigned char *)source;
  for (size_t index = 0; index < count; index += 1) {
    destination_bytes[index] = source_bytes[index];
  }
  return destination;
}

__attribute__((optnone)) void *memmove(void *destination, const void *source, size_t count) {
  unsigned char *destination_bytes = (unsigned char *)destination;
  const unsigned char *source_bytes = (const unsigned char *)source;
  const uintptr_t destination_address = (uintptr_t)destination;
  const uintptr_t source_address = (uintptr_t)source;
  if (destination_address < source_address) {
    for (size_t index = 0; index < count; index += 1) {
      destination_bytes[index] = source_bytes[index];
    }
  } else if (destination_address > source_address) {
    for (size_t index = count; index > 0; index -= 1) {
      destination_bytes[index - 1] = source_bytes[index - 1];
    }
  }
  return destination;
}

__attribute__((optnone)) void *memset(void *destination, int value, size_t count) {
  unsigned char *destination_bytes = (unsigned char *)destination;
  for (size_t index = 0; index < count; index += 1) {
    destination_bytes[index] = (unsigned char)value;
  }
  return destination;
}

int memcmp(const void *left, const void *right, size_t count) {
  const unsigned char *left_bytes = (const unsigned char *)left;
  const unsigned char *right_bytes = (const unsigned char *)right;
  for (size_t index = 0; index < count; index += 1) {
    if (left_bytes[index] < right_bytes[index]) return -1;
    if (left_bytes[index] > right_bytes[index]) return 1;
  }
  return 0;
}

typedef struct silk_coroutine_frame_header_v1 {
  void *allocation;
} silk_coroutine_frame_header_v1;

void *silk_coroutine_frame_push_v1(size_t size, size_t alignment) {
  if (alignment < sizeof(void *)) alignment = sizeof(void *);
  if ((alignment & (alignment - 1)) != 0) return (void *)0;
  const size_t overhead = sizeof(silk_coroutine_frame_header_v1) + alignment - 1;
  if (size > (__SIZE_MAX__) - overhead) return (void *)0;
  void *allocation = malloc(size + overhead);
  if (allocation == (void *)0) return (void *)0;
  const uintptr_t start = (uintptr_t)allocation;
  const uintptr_t payload = (start + sizeof(silk_coroutine_frame_header_v1) + alignment - 1) &
    ~((uintptr_t)alignment - 1);
  silk_coroutine_frame_header_v1 *header =
    (silk_coroutine_frame_header_v1 *)(payload - sizeof(silk_coroutine_frame_header_v1));
  header->allocation = allocation;
  return (void *)payload;
}

void silk_coroutine_frame_pop_v1(void *frame) {
  if (frame == (void *)0) return;
  silk_coroutine_frame_header_v1 *header =
    (silk_coroutine_frame_header_v1 *)((unsigned char *)frame -
      sizeof(silk_coroutine_frame_header_v1));
  if (header->allocation == (void *)0) __builtin_trap();
  void *allocation = header->allocation;
  header->allocation = (void *)0;
  free(allocation);
}
`
