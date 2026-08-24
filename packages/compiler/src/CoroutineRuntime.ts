/** Private native runtime support for non-moving coroutine-frame segments. */

export const pushSymbol = 'silk_coroutine_frame_push_v1'
export const popSymbol = 'silk_coroutine_frame_pop_v1'

export const symbols: ReadonlyArray<string> = Object.freeze([pushSymbol, popSymbol])

/** Emits the runtime only for artifacts whose suspension lowering references it. */
export const source = (requested: ReadonlyArray<string>): string => {
  if (!symbols.some((symbol) => requested.includes(symbol))) return ''
  return `#include <stdint.h>
#include <stdlib.h>

typedef struct silk_coroutine_frame_header_v1 {
  void *allocation;
  size_t reserved;
} silk_coroutine_frame_header_v1;

static _Thread_local size_t silk_coroutine_segment_bytes_v1 = 0;

static size_t silk_coroutine_stack_limit_v1(void) {
  const char *text = getenv("SILK_PRIVATE_EXECUTION_STACK_LIMIT_BYTES");
  if (text == 0 || *text == 0) return SIZE_MAX;
  char *end = 0;
  const unsigned long long parsed = strtoull(text, &end, 10);
  if (end == text || *end != 0 || parsed > SIZE_MAX) return SIZE_MAX;
  return (size_t)parsed;
}

void *silk_coroutine_frame_push_v1(size_t size, size_t alignment) {
  if (alignment < sizeof(void *)) alignment = sizeof(void *);
  if ((alignment & (alignment - 1)) != 0) return 0;
  const size_t overhead = sizeof(silk_coroutine_frame_header_v1) + alignment - 1;
  if (size > SIZE_MAX - overhead) return 0;
  const size_t required = size + overhead;
  const size_t limit = silk_coroutine_stack_limit_v1();
  if (silk_coroutine_segment_bytes_v1 > limit ||
      required > limit - silk_coroutine_segment_bytes_v1) return 0;
  void *allocation = malloc(required);
  if (allocation == 0) return 0;
  const uintptr_t start = (uintptr_t)allocation;
  const uintptr_t payload = (start + sizeof(silk_coroutine_frame_header_v1) + alignment - 1) &
    ~((uintptr_t)alignment - 1);
  silk_coroutine_frame_header_v1 *header =
    (silk_coroutine_frame_header_v1 *)(payload - sizeof(silk_coroutine_frame_header_v1));
  header->allocation = allocation;
  header->reserved = required;
  silk_coroutine_segment_bytes_v1 += required;
  return (void *)payload;
}

void silk_coroutine_frame_pop_v1(void *frame) {
  if (frame == 0) return;
  silk_coroutine_frame_header_v1 *header =
    (silk_coroutine_frame_header_v1 *)((unsigned char *)frame -
      sizeof(silk_coroutine_frame_header_v1));
  if (header->allocation == 0 || header->reserved > silk_coroutine_segment_bytes_v1) abort();
  void *allocation = header->allocation;
  silk_coroutine_segment_bytes_v1 -= header->reserved;
  header->allocation = 0;
  free(allocation);
}
`
}
