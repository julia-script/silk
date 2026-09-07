#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
_Static_assert(sizeof(int) == 4 && sizeof(size_t) == 8 && sizeof(ssize_t) == 8, "descriptor widths");
_Static_assert(_Alignof(size_t) == 8 && _Alignof(ssize_t) == 8 && (ssize_t)-1 < 0 && (size_t)-1 > 0, "descriptor count layout and signedness");
_Static_assert(__builtin_types_compatible_p(__typeof__(&read), ssize_t (*)(int, void *, size_t)), "read ABI");
_Static_assert(__builtin_types_compatible_p(__typeof__(&write), ssize_t (*)(int, const void *, size_t)), "write ABI");
_Static_assert(STDIN_FILENO == 0 && STDOUT_FILENO == 1 && STDERR_FILENO == 2 && EINTR == 4, "descriptor constants");
extern int stdout_fixture(int, int);
extern int stderr_fixture(void);
extern int read_fixture(int);
extern int empty_read_fixture(int);
static int scenario, writes, reads, error_reads, native_error, invalid;
static size_t committed;
#ifdef __APPLE__
_Static_assert(__builtin_types_compatible_p(__typeof__(&__error), int *(*)(void)), "Darwin errno ABI");
int *__error(void) { ++error_reads; return &native_error; }
#else
_Static_assert(__builtin_types_compatible_p(__typeof__(&__errno_location), int *(*)(void)), "GNU errno ABI");
int *__errno_location(void) { ++error_reads; return &native_error; }
#endif
ssize_t write(int fd, const void *buffer, size_t count) {
  ++writes;
  if (fd != (scenario == 6 ? 2 : 1) || count != 6 - committed || memcmp(buffer, &"abcdef"[committed], count)) invalid = 1;
  if (scenario == 1 && writes == 2) { native_error = EINTR; return -1; }
  if (scenario == 2) return 0;
  if (scenario == 3 && writes == 2) { native_error = EIO; return -1; }
  if (scenario == 4) return (ssize_t)count + 1;
  ssize_t result = (ssize_t)count;
  if ((scenario == 1 || scenario == 3) && writes == 1) result = 2;
  if (scenario == 1 && writes == 3) result = 1;
  committed += (size_t)result;
  return result;
}
ssize_t read(int fd, void *buffer, size_t count) {
  ++reads;
  if (fd != 0 || count == 0 || count > 4) invalid = 1;
  if (scenario == 11 && reads == 1) { native_error = EINTR; return -1; }
  if (scenario == 12 || scenario == 16) return 0;
  if (scenario == 13) { native_error = EIO; return -1; }
  if (scenario == 14) return (ssize_t)count + 1;
  ((unsigned char *)buffer)[0] = 1;
  ((unsigned char *)buffer)[1] = 2;
  return 2;
}
static void reset(int value) { scenario = value; writes = reads = error_reads = invalid = 0; native_error = EIO; committed = 0; }
#define CHECK(c) do { if (!(c)) return 10 + scenario; } while (0)
int main(void) {
  reset(0); CHECK(stdout_fixture(0, 0) == 42 && writes == 1 && error_reads == 0 && !invalid);
  reset(1); CHECK(stdout_fixture(0, 0) == 42 && writes == 4 && committed == 6 && error_reads == 1 && !invalid);
  reset(2); CHECK(stdout_fixture(0, 0) == 7 && writes == 1 && error_reads == 0 && !invalid);
  reset(3); CHECK(stdout_fixture(0, 0) == 7 && writes == 2 && committed == 2 && error_reads == 1 && !invalid);
  reset(4); CHECK(stdout_fixture(0, 0) == 7 && writes == 1 && error_reads == 0 && !invalid);
  reset(5); CHECK(stdout_fixture(1, 0) == 42 && stdout_fixture(0, 1) == 42 && writes == 0 && error_reads == 0);
  reset(6); CHECK(stderr_fixture() == 42 && writes == 1 && error_reads == 0 && !invalid);
  reset(10); CHECK(read_fixture(0) == 42 && reads == 1 && error_reads == 0 && !invalid);
  reset(11); CHECK(read_fixture(0) == 42 && reads == 2 && error_reads == 1 && !invalid);
  reset(12); CHECK(read_fixture(1) == 42 && reads == 1 && error_reads == 0 && !invalid);
  reset(13); CHECK(read_fixture(0) == 7 && reads == 1 && error_reads == 1 && !invalid);
  reset(14); CHECK(read_fixture(0) == 7 && reads == 1 && error_reads == 0 && !invalid);
  reset(15); CHECK(empty_read_fixture(0) == 42 && reads == 0 && error_reads == 0);
  reset(16); CHECK(empty_read_fixture(1) == 42 && reads == 1 && error_reads == 0 && !invalid);
  return 42;
}
