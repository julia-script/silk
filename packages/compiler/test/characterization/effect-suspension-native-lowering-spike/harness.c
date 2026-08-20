#include "spike.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void require(bool condition, const char *message) {
  if (!condition) {
    spike_abort(message);
  }
}

void spike_abort(const char *message) {
  fprintf(stderr, "spike assertion failed: %s\n", message);
  abort();
}

void spike_event(struct SpikeContext *context, const char *format, ...) {
  if (context->suppress_events) {
    return;
  }
  va_list arguments;
  va_start(arguments, format);
  size_t remaining = sizeof(context->events.bytes) - context->events.length;
  int written = vsnprintf(
      context->events.bytes + context->events.length,
      remaining,
      format,
      arguments);
  va_end(arguments);
  require(written >= 0, "event formatting failed");
  require((size_t)written + 1 < remaining, "event log overflow");
  context->events.length += (size_t)written;
  context->events.bytes[context->events.length++] = '\n';
  context->events.bytes[context->events.length] = '\0';
}

void *spike_allocate(
    struct SpikeContext *context,
    size_t size,
    size_t alignment,
    int boundary) {
  require(size > 0, "allocator must observe the complete non-empty frame size");
  require(
      alignment >= sizeof(void *) && (alignment & (alignment - 1)) == 0,
      "allocator must observe a valid complete frame alignment");
  require(!context->allocator.exclusively_borrowed, "allocator loan overlapped");
  context->allocator.exclusively_borrowed = true;
  context->allocator.request_ordinal += 1;
  int ordinal = context->allocator.request_ordinal;
  spike_event(
      context,
      "allocate-begin:b%d:o%d:size=%zu:align=%zu",
      boundary,
      ordinal,
      size,
      alignment);

  if (ordinal == context->allocator.refusal_ordinal) {
    spike_event(context, "allocate-refused:b%d:o%d", boundary, ordinal);
    context->allocator.exclusively_borrowed = false;
    spike_event(context, "allocator-loan-close:b%d:o%d", boundary, ordinal);
    return NULL;
  }

  void *allocation = NULL;
  int allocation_result = posix_memalign(&allocation, alignment, size);
  require(allocation_result == 0 && allocation != NULL, "host allocation failed unexpectedly");
  context->allocator.live_allocations += 1;
  spike_event(context, "allocate-end:b%d:o%d", boundary, ordinal);
  context->allocator.exclusively_borrowed = false;
  spike_event(context, "allocator-loan-close:b%d:o%d", boundary, ordinal);
  return allocation;
}

void spike_reclaim(struct SpikeContext *context, void *allocation, int boundary) {
  require(allocation != NULL, "reclaim authority cannot be null");
  require(!context->allocator.exclusively_borrowed, "reclaim rediscovered an active provider loan");
  require(context->allocator.live_allocations > 0, "continuation reclaimed more than once");
  spike_event(context, "reclaim:b%d", boundary);
  context->allocator.live_allocations -= 1;
  free(allocation);
}

void spike_publish(struct SpikeContext *context, int boundary, int owner) {
  require(!context->allocator.exclusively_borrowed, "published before allocator loan closed");
  spike_event(context, "publish:b%d:owner=%d", boundary, owner);
}

void spike_child_reborrow(struct SpikeContext *context, int boundary) {
  require(!context->allocator.exclusively_borrowed, "child could not reborrow allocator");
  context->allocator.exclusively_borrowed = true;
  spike_event(context, "child-reborrow-begin:b%d", boundary);
  spike_event(context, "child-reborrow-end:b%d", boundary);
  context->allocator.exclusively_borrowed = false;
}

void spike_child_start(struct SpikeContext *context, int boundary) {
  require(!context->allocator.exclusively_borrowed, "child started under allocator loan");
  spike_event(context, "child-start:b%d", boundary);
}

void spike_source_cleanup(struct SpikeContext *context, int owner) {
  spike_event(context, "source-cleanup:owner=%d", owner);
}

#if defined(__clang__) || defined(__GNUC__)
#define SPIKE_NOINLINE __attribute__((noinline))
#else
#define SPIKE_NOINLINE
#endif

#line 1 "boundaries.silk"
SPIKE_NOINLINE void silk_boundary_suspend_one(void) {
  __asm__ volatile("" ::: "memory");
}
#line 2 "boundaries.silk"
SPIKE_NOINLINE void silk_boundary_suspend_two(void) {
  __asm__ volatile("" ::: "memory");
}
#line 120 "harness.c"

#ifndef SPIKE_NO_MAIN
static struct SpikeContext fresh_context(int refusal_ordinal) {
  struct SpikeContext context = {0};
  context.allocator.refusal_ordinal = refusal_ordinal;
  context.outcome_tag = SPIKE_SUCCESS;
  return context;
}

static void *reach_boundary(
    struct SpikeContext *context,
    int boundary,
    int owner,
    int32_t scalar) {
  if (boundary == 1) {
    silk_boundary_suspend_one();
  } else if (boundary == 2) {
    silk_boundary_suspend_two();
  }
  void *continuation = spike_variant_open(context, boundary, owner, scalar);
  if (continuation == NULL) {
    spike_source_cleanup(context, owner);
    context->outcome_tag = SPIKE_OUT_OF_MEMORY;
    context->outcome_payload = 0;
    return NULL;
  }
  spike_publish(context, boundary, owner);
  spike_child_reborrow(context, boundary);
  spike_child_start(context, boundary);
  return continuation;
}

static int occurrences(const char *text, const char *needle) {
  int count = 0;
  const char *cursor = text;
  size_t length = strlen(needle);
  while ((cursor = strstr(cursor, needle)) != NULL) {
    count += 1;
    cursor += length;
  }
  return count;
}

static void require_order(const char *events, const char *first, const char *second) {
  const char *first_at = strstr(events, first);
  const char *second_at = strstr(events, second);
  require(first_at != NULL, first);
  require(second_at != NULL, second);
  require(first_at < second_at, "event order differs from the frozen protocol");
}

static void verify_common(const struct SpikeContext *context) {
  require(!context->allocator.exclusively_borrowed, "allocator loan leaked");
  require(context->allocator.live_allocations == 0, "continuation allocation leaked");
}

static void print_scenario(const char *name, const struct SpikeContext *context) {
  printf("BEGIN %s\n", name);
  fputs(context->events.bytes, stdout);
  printf("OUTCOME %d %d\n", context->outcome_tag, context->outcome_payload);
  printf("END %s\n", name);
}

static void scenario_untaken(void) {
  struct SpikeContext context = fresh_context(0);
  context.outcome_payload = 17;
  verify_common(&context);
  require(context.allocator.request_ordinal == 0, "untaken suspension branch allocated");
  print_scenario("untaken", &context);
}

static void scenario_success(void) {
  struct SpikeContext context = fresh_context(0);
  void *one = reach_boundary(&context, 1, 101, 11);
  void *two = reach_boundary(&context, 2, 202, 22);
  require(one != NULL && two != NULL, "success setup refused allocation");
  context.outcome_tag = SPIKE_SUCCESS;
  context.outcome_payload = 33;
  spike_variant_resume(two);
  spike_variant_resume(one);
  verify_common(&context);
  require(context.allocator.request_ordinal == 2, "success did not allocate once per boundary");
  require(occurrences(context.events.bytes, "allocate-begin:") == 2, "hidden or missing request");
  require_order(context.events.bytes, "allocator-loan-close:b1", "publish:b1");
  require_order(context.events.bytes, "publish:b1", "child-start:b1");
  require_order(context.events.bytes, "source-cleanup:owner=202", "reclaim:b2");
  require_order(context.events.bytes, "reclaim:b2", "source-cleanup:owner=101");
  require_order(context.events.bytes, "source-cleanup:owner=101", "reclaim:b1");
  print_scenario("success", &context);
}

static void scenario_typed_failure(void) {
  struct SpikeContext context = fresh_context(0);
  void *one = reach_boundary(&context, 1, 101, 11);
  void *two = reach_boundary(&context, 2, 202, 22);
  require(one != NULL && two != NULL, "failure setup refused allocation");
  context.outcome_tag = SPIKE_TYPED_FAILURE;
  context.outcome_payload = 731;
  spike_variant_resume(two);
  spike_variant_resume(one);
  verify_common(&context);
  require(context.outcome_tag == SPIKE_TYPED_FAILURE, "typed failure tag changed");
  require(context.outcome_payload == 731, "typed failure payload changed");
  require_order(context.events.bytes, "source-cleanup:owner=202", "reclaim:b2");
  require_order(context.events.bytes, "reclaim:b2", "source-cleanup:owner=101");
  require_order(context.events.bytes, "source-cleanup:owner=101", "reclaim:b1");
  print_scenario("typed-failure", &context);
}

static void scenario_refusal(int refusal_ordinal) {
  struct SpikeContext context = fresh_context(refusal_ordinal);
  void *one = reach_boundary(&context, 1, 101, 11);
  if (refusal_ordinal == 1) {
    require(one == NULL, "first request was not refused");
  } else {
    require(one != NULL, "first request unexpectedly refused");
    void *two = reach_boundary(&context, 2, 202, 22);
    require(two == NULL, "second request was not refused");
    spike_variant_resume(one);
  }
  verify_common(&context);
  require(context.outcome_tag == SPIKE_OUT_OF_MEMORY, "refusal did not preserve OutOfMemoryError");
  require(context.outcome_payload == 0, "OutOfMemoryError unexpectedly gained a payload");
  char refused[32];
  char rejected_publish[32];
  char rejected_child[32];
  int rejected_boundary = refusal_ordinal;
  snprintf(refused, sizeof(refused), "allocate-refused:b%d", rejected_boundary);
  snprintf(rejected_publish, sizeof(rejected_publish), "publish:b%d", rejected_boundary);
  snprintf(rejected_child, sizeof(rejected_child), "child-start:b%d", rejected_boundary);
  require(strstr(context.events.bytes, refused) != NULL, "missing refusal event");
  require(strstr(context.events.bytes, rejected_publish) == NULL, "rejected owner was published");
  require(strstr(context.events.bytes, rejected_child) == NULL, "rejected child started");
  if (refusal_ordinal == 2) {
    require_order(context.events.bytes, "source-cleanup:owner=202", "source-cleanup:owner=101");
    require_order(context.events.bytes, "source-cleanup:owner=101", "reclaim:b1");
  }
  char name[32];
  snprintf(name, sizeof(name), "refuse-%d", refusal_ordinal);
  print_scenario(name, &context);
}

static void scenario_teardown(int checkpoint) {
  struct SpikeContext context = fresh_context(0);
  void *one = reach_boundary(&context, 1, 101, 11);
  require(one != NULL, "teardown first allocation refused");
  void *two = NULL;
  if (checkpoint == 2) {
    two = reach_boundary(&context, 2, 202, 22);
    require(two != NULL, "teardown second allocation refused");
  }
  spike_event(&context, "harness-defect:checkpoint=%d", checkpoint);
  if (two != NULL) {
    spike_variant_teardown(two);
  }
  spike_variant_teardown(one);
  verify_common(&context);
  require(
      occurrences(context.events.bytes, "source-cleanup:") == 0,
      "harness-only teardown reported source cleanup");
  require(
      occurrences(context.events.bytes, "reclaim:") == checkpoint,
      "harness-only teardown did not reclaim every raw frame exactly once");
  if (checkpoint == 2) {
    require_order(context.events.bytes, "reclaim:b2", "reclaim:b1");
  }
  char name[32];
  snprintf(name, sizeof(name), "teardown-%d", checkpoint);
  print_scenario(name, &context);
}

int main(void) {
  printf("VARIANT %s\n", spike_variant_name());
  scenario_untaken();
  scenario_success();
  scenario_typed_failure();
  scenario_refusal(1);
  scenario_refusal(2);
  scenario_teardown(1);
  scenario_teardown(2);
  return 0;
}
#endif
