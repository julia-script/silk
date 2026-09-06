#include <stdint.h>
#include <stdlib.h>
#include <pthread.h>

typedef int64_t (*native_operation)(int8_t, uint16_t, int64_t, float, double, int32_t *);
extern int64_t silk_indirect(native_operation, int32_t *);
extern int32_t silk_compare(const int32_t *, const int32_t *);

static pthread_t initiating_thread;
static int active;
static int depth;
static int comparisons;
static int invalid;

static void check_extent(void) {
  if (!active || !pthread_equal(initiating_thread, pthread_self())) invalid = 1;
}
void fixture_mark_comparison(void) {
  check_extent();
  ++comparisons;
}
static int compare(const void *left, const void *right) {
  return silk_compare(left, right);
}
static int64_t leaf(int8_t a, uint16_t b, int64_t c, float d, double e, int32_t *value) {
  check_extent();
  if (a != -7 || b != 65000 || c != INT64_C(1234567890123) || d != 1.25f || e != 2.5 || depth != 1)
    invalid = 1;
  *value += 3;
  return c;
}
static int64_t nested(int8_t a, uint16_t b, int64_t c, float d, double e, int32_t *outer) {
  check_extent();
  if (a != -7 || b != 65000 || c != INT64_C(1234567890123) || d != 1.25f || e != 2.5 || depth != 0)
    invalid = 1;
  int32_t inner = 11;
  ++depth;
  int64_t result = silk_indirect(leaf, &inner);
  --depth;
  if (inner != 14 || *outer != 7 || result != c) invalid = 1;
  *outer = 29;
  return result + 1;
}
int fixture_callbacks(void) {
  initiating_thread = pthread_self();
  active = 1;
  int32_t outer = 7;
  int64_t result = silk_indirect(nested, &outer);
  int32_t values[] = { 4, -2, 3, 0 };
  qsort(values, 4, sizeof(values[0]), compare);
  active = 0;
  if (invalid || depth != 0 || comparisons == 0 || outer != 29 || result != INT64_C(1234567890124)) return 11;
  if (values[0] != -2 || values[1] != 0 || values[2] != 3 || values[3] != 4) return 12;
  return 0;
}
