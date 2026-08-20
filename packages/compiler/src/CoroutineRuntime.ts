/** Private native runtime support for non-moving coroutine-frame segments. */

export const pushSymbol = 'silk_coroutine_frame_push_v1'
export const popSymbol = 'silk_coroutine_frame_pop_v1'

export const symbols: ReadonlyArray<string> = Object.freeze([pushSymbol, popSymbol])

/** Emits the runtime only for artifacts whose suspension lowering references it. */
export const source = (requested: ReadonlyArray<string>): string => {
  if (!symbols.some((symbol) => requested.includes(symbol))) return ''
  return `#include <stdint.h>
#include <stdlib.h>

typedef struct silk_coroutine_segment_v1 {
  struct silk_coroutine_segment_v1 *previous;
  size_t used;
  size_t capacity;
  unsigned char bytes[];
} silk_coroutine_segment_v1;

typedef struct silk_coroutine_frame_header_v1 {
  silk_coroutine_segment_v1 *segment;
  size_t previous_used;
} silk_coroutine_frame_header_v1;

static _Thread_local silk_coroutine_segment_v1 *silk_coroutine_segment_top_v1 = 0;
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
  silk_coroutine_segment_v1 *segment = silk_coroutine_segment_top_v1;
  if (segment == 0 || required > segment->capacity - segment->used) {
    const size_t capacity = required > 65536 ? required : 65536;
    const size_t limit = silk_coroutine_stack_limit_v1();
    if (silk_coroutine_segment_bytes_v1 > limit ||
        capacity > limit - silk_coroutine_segment_bytes_v1) return 0;
    if (capacity > SIZE_MAX - sizeof(silk_coroutine_segment_v1)) return 0;
    segment = (silk_coroutine_segment_v1 *)malloc(
      sizeof(silk_coroutine_segment_v1) + capacity
    );
    if (segment == 0) return 0;
    segment->previous = silk_coroutine_segment_top_v1;
    segment->used = 0;
    segment->capacity = capacity;
    silk_coroutine_segment_top_v1 = segment;
    silk_coroutine_segment_bytes_v1 += capacity;
  }
  const uintptr_t start = (uintptr_t)(segment->bytes + segment->used);
  const uintptr_t payload = (start + sizeof(silk_coroutine_frame_header_v1) + alignment - 1) &
    ~((uintptr_t)alignment - 1);
  silk_coroutine_frame_header_v1 *header =
    (silk_coroutine_frame_header_v1 *)(payload - sizeof(silk_coroutine_frame_header_v1));
  header->segment = segment;
  header->previous_used = segment->used;
  segment->used = (size_t)(payload - (uintptr_t)segment->bytes) + size;
  return (void *)payload;
}

void silk_coroutine_frame_pop_v1(void *frame) {
  if (frame == 0) return;
  silk_coroutine_frame_header_v1 *header =
    (silk_coroutine_frame_header_v1 *)((unsigned char *)frame -
      sizeof(silk_coroutine_frame_header_v1));
  silk_coroutine_segment_v1 *segment = header->segment;
  if (segment == 0 || segment != silk_coroutine_segment_top_v1) abort();
  segment->used = header->previous_used;
  if (segment->used == 0 && segment->previous != 0) {
    silk_coroutine_segment_top_v1 = segment->previous;
    silk_coroutine_segment_bytes_v1 -= segment->capacity;
    free(segment);
  }
}
`
}
