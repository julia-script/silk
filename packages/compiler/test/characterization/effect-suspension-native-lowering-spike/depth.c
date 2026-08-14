#include "spike.h"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define SPIKE_MAX_LOGICAL_DEPTH 100000
#define SPIKE_BOUNDARIES_PER_DEPTH 2
#define SPIKE_MAX_CONTINUATIONS \
  (SPIKE_MAX_LOGICAL_DEPTH * SPIKE_BOUNDARIES_PER_DEPTH)

static void *continuations[SPIKE_MAX_CONTINUATIONS];
static uintptr_t watermark_minimum = UINTPTR_MAX;
static uintptr_t watermark_maximum = 0;
static size_t watermark_checkpoints = 0;

#if defined(__clang__) || defined(__GNUC__)
__attribute__((noinline))
#endif
static void driver_checkpoint(void) {
  volatile unsigned char marker = 0;
  uintptr_t address = (uintptr_t)&marker;
  if (address < watermark_minimum) {
    watermark_minimum = address;
  }
  if (address > watermark_maximum) {
    watermark_maximum = address;
  }
  watermark_checkpoints += 1;
  __asm__ volatile("" : : "r"(&marker) : "memory");
}

static void fail(const char *message) {
  fprintf(stderr, "depth spike failed: %s\n", message);
  exit(2);
}

static void *open_boundary(
    struct SpikeContext *context,
    int boundary,
    int owner,
    int32_t scalar) {
  if (boundary == 1) {
    silk_boundary_suspend_one();
  } else {
    silk_boundary_suspend_two();
  }
  void *continuation = spike_variant_open(context, boundary, owner, scalar);
  if (continuation == NULL) {
    fail("continuation allocation was refused");
  }
  spike_publish(context, boundary, owner);
  spike_child_reborrow(context, boundary);
  spike_child_start(context, boundary);
  return continuation;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fail("expected one logical depth argument");
  }
  char *end = NULL;
  unsigned long parsed = strtoul(argv[1], &end, 10);
  if (*argv[1] == '\0' || end == NULL || *end != '\0' || parsed == 0 ||
      parsed > SPIKE_MAX_LOGICAL_DEPTH) {
    fail("depth must be between 1 and 100000");
  }
  size_t depth = (size_t)parsed;
  size_t continuation_count = depth * SPIKE_BOUNDARIES_PER_DEPTH;
  struct SpikeContext context = {0};
  context.suppress_events = true;

  for (size_t index = 0; index < continuation_count; index += 1) {
    int boundary = (int)(index % SPIKE_BOUNDARIES_PER_DEPTH) + 1;
    continuations[index] = open_boundary(
        &context,
        boundary,
        (int)(index + 1),
        (int32_t)index);
    driver_checkpoint();
  }
  for (size_t index = continuation_count; index > 0; index -= 1) {
    driver_checkpoint();
    spike_variant_resume(continuations[index - 1]);
  }

  if (context.allocator.live_allocations != 0) {
    fail("continuation allocation leaked");
  }
  if (context.allocator.request_ordinal != (int)continuation_count) {
    fail("allocation request count differs from reached boundaries");
  }
  uintptr_t range = watermark_maximum - watermark_minimum;
  printf(
      "{\"variant\":\"%s\",\"depth\":%zu,\"continuations\":%zu,"
      "\"checkpoints\":%zu,\"minimum\":\"0x%" PRIxPTR
      "\",\"maximum\":\"0x%" PRIxPTR "\",\"range\":%" PRIuPTR
      ",\"liveAllocations\":%d}\n",
      spike_variant_name(),
      depth,
      continuation_count,
      watermark_checkpoints,
      watermark_minimum,
      watermark_maximum,
      range,
      context.allocator.live_allocations);
  return 0;
}
