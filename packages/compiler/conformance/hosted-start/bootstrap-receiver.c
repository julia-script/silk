#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#ifdef __APPLE__
#include <crt_externs.h>
#else
extern char **environ;
#endif

_Static_assert(sizeof(int) == 4, "C entry int");
extern int main(int, char **);

static _Alignas(16) unsigned char arena[4 * 1024 * 1024];
static void *live[1024];
static size_t sizes[1024];
static size_t cursor;
static unsigned calls, refusal, invalid, bodies, drops, depth;
static int application_mode, fail_writes;
static char report[8192];
static size_t reported;
static char first[] = "entry";
static char second[] = "outer";
static char inner[] = "inner";
static char *arguments[] = { first, second, NULL };
static char *nested_arguments[] = { first, inner, NULL };
static char *missing_argument[] = { NULL };
static char environment_value[] = "SILK_BOOTSTRAP=owned";
static char *environment[] = { environment_value, NULL };

void *malloc(size_t bytes) {
  unsigned ordinal = ++calls;
  if (ordinal == refusal) return NULL;
  if (ordinal > 1024 || bytes > sizeof arena - cursor - 16) {
    invalid = 10;
    return NULL;
  }
  void *pointer = arena + cursor;
  cursor += ((bytes == 0 ? 1 : bytes) + 15) & ~(size_t)15;
  live[ordinal - 1] = pointer;
  sizes[ordinal - 1] = bytes;
  return pointer;
}

void free(void *pointer) {
  if (pointer == NULL) return;
  for (unsigned index = 0; index < 1024; ++index) {
    if (live[index] == pointer) {
      memset(pointer, 0xDD, sizes[index]);
      live[index] = NULL;
      return;
    }
  }
  invalid = 11;
}

static unsigned live_count(void) {
  unsigned count = 0;
  for (unsigned index = 0; index < 1024; ++index) count += live[index] != NULL;
  return count;
}

ssize_t write(int descriptor, const void *bytes, size_t length) {
  if (application_mode == 2 && bodies != 0 && drops != bodies) invalid = 12;
  if (descriptor != 2 || length > sizeof report - reported - 1) {
    invalid = 13;
    errno = EINVAL;
    return -1;
  }
  if (fail_writes) { errno = EIO; return -1; }
  size_t committed = length > 3 ? 3 : length;
  memcpy(report + reported, bytes, committed);
  reported += committed;
  report[reported] = 0;
  return (ssize_t)committed;
}

int bootstrap_body(void) {
  ++bodies;
  if (application_mode == 1 && depth == 0) {
    unsigned outer_live = live_count();
    depth = 1;
    int nested = main(2, nested_arguments);
    depth = 0;
    if (nested != 0 || live_count() != outer_live) invalid = 14;
  }
  return application_mode == 2 ? 7 : 42;
}

void bootstrap_dropped(void) { ++drops; }

static unsigned invoke(int argc, char **argv, unsigned fail, int output_failure, int mode) {
  if (live_count() != 0) _exit(15);
  cursor = 0; calls = 0; refusal = fail; invalid = 0;
  bodies = 0; drops = 0; depth = 0; reported = 0; report[0] = 0;
  application_mode = mode; fail_writes = output_failure;
  int status = main(argc, argv);
  if (invalid != 0) _exit((int)invalid);
  if (live_count() != 0) _exit(16);
  if (fail != 0 && calls < fail) _exit(17);
  if (argc < 0 || argv == missing_argument) {
    if (status != 1 || bodies != 0) _exit(18);
  } else if (bodies == 0) {
    if (status != 1 || fail == 0) _exit(19);
  } else if (mode == 2) {
    if (status != 1 || bodies != 1 || drops != 1) _exit(20);
  } else {
    if (status != 0 || bodies != (mode == 1 ? 2u : 1u) || drops != 0) _exit(21);
  }
  if (output_failure || status == 0) {
    if (reported != 0) _exit(22);
  } else {
    if (strncmp(report, "unhandled error: ", 17) != 0) _exit(23);
    if (mode == 2 && bodies != 0 && strstr(report, "entry-conformance/root.Problem") == NULL) _exit(24);
  }
  return calls;
}

/* Test harness only: invoke the actual source C entry before CRT invokes it, then exit. */
__attribute__((constructor)) static void bootstrap_cases(void) {
#ifdef __APPLE__
  *_NSGetEnviron() = environment;
#else
  environ = environment;
#endif
  unsigned baseline = invoke(2, arguments, 0, 0, 0);
  if (baseline == 0) _exit(25);
  for (unsigned fail = 1; fail <= baseline; ++fail) {
    (void)invoke(2, arguments, fail, 0, 0);
    (void)invoke(2, arguments, fail, 1, 0);
  }
  (void)invoke(-1, arguments, 0, 0, 0);
  (void)invoke(1, missing_argument, 0, 0, 0);
  (void)invoke(-1, arguments, 0, 1, 0);
  (void)invoke(2, arguments, 0, 0, 1);
  (void)invoke(2, arguments, 0, 0, 2);
  (void)invoke(2, arguments, 0, 1, 2);
  _exit(42);
}
