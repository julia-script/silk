#ifndef SILK_EFFECT_SUSPENSION_NATIVE_SPIKE_H
#define SILK_EFFECT_SUSPENSION_NATIVE_SPIKE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

enum SpikeOutcomeTag {
  SPIKE_SUCCESS = 0,
  SPIKE_TYPED_FAILURE = 1,
  SPIKE_OUT_OF_MEMORY = 2,
};

struct SpikeEventLog {
  char bytes[32768];
  size_t length;
};

struct SpikeAllocator {
  int request_ordinal;
  int refusal_ordinal;
  int live_allocations;
  bool exclusively_borrowed;
};

struct SpikeContext {
  struct SpikeEventLog events;
  struct SpikeAllocator allocator;
  enum SpikeOutcomeTag outcome_tag;
  int32_t outcome_payload;
  bool suppress_events;
};

void spike_abort(const char *message);
void spike_event(struct SpikeContext *context, const char *format, ...);
void *spike_allocate(struct SpikeContext *context, size_t size, size_t alignment, int boundary);
void spike_reclaim(struct SpikeContext *context, void *allocation, int boundary);
void spike_publish(struct SpikeContext *context, int boundary, int owner);
void spike_child_reborrow(struct SpikeContext *context, int boundary);
void spike_child_start(struct SpikeContext *context, int boundary);
void spike_source_cleanup(struct SpikeContext *context, int owner);
void silk_boundary_suspend_one(void);
void silk_boundary_suspend_two(void);

void *spike_variant_open(
    struct SpikeContext *context,
    int boundary,
    int owner,
    int32_t scalar);
void spike_variant_resume(void *continuation);
void spike_variant_teardown(void *continuation);
const char *spike_variant_name(void);

#endif
