#include "spike.h"

#include <stdalign.h>
#include <stdint.h>

struct DirectContinuation {
  struct SpikeContext *context;
  int boundary;
  int owner;
  int32_t scalar;
};

void *spike_variant_open(
    struct SpikeContext *context,
    int boundary,
    int owner,
    int32_t scalar) {
  struct DirectContinuation *continuation = spike_allocate(
      context,
      sizeof(struct DirectContinuation),
      alignof(struct DirectContinuation),
      boundary);
  if (continuation == NULL) {
    return NULL;
  }

  continuation->context = context;
  continuation->boundary = boundary;
  continuation->owner = owner;
  continuation->scalar = scalar;
  return continuation;
}

void spike_variant_resume(void *opaque) {
  struct DirectContinuation *continuation = opaque;
  struct SpikeContext *context = continuation->context;
  int boundary = continuation->boundary;
  int owner = continuation->owner;
  volatile int32_t retained_scalar = continuation->scalar;
  (void)retained_scalar;
  spike_source_cleanup(context, owner);
  spike_reclaim(context, continuation, boundary);
}

void spike_variant_teardown(void *opaque) {
  struct DirectContinuation *continuation = opaque;
  spike_reclaim(continuation->context, continuation, continuation->boundary);
}

const char *spike_variant_name(void) { return "direct-iterative"; }
