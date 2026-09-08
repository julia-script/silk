#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>

extern int32_t report_probe(int32_t mode);
static char captured[2048];
static size_t used;
static unsigned calls;
static int selected;

ssize_t write(int descriptor, const void *bytes, size_t length) {
  ++calls;
  if (descriptor != 2 || used + length > sizeof captured) { errno = EINVAL; return -1; }
  if (selected == 3 && calls > 1) { errno = EIO; return -1; }
  if (selected == 4 && calls == 1) { errno = EINTR; return -1; }
  if (selected == 5) return 0;
  size_t committed = length > 3 ? 3 : length;
  memcpy(captured + used, bytes, committed);
  used += committed;
  return (ssize_t)committed;
}

int main(void) {
  const char *normal = "unhandled error: app.Failure\n  at app.origin\n  at app.caller\nwhile handling: app.Cause\n  at app.cause\n";
  for (int mode = 0; mode <= 6; ++mode) {
    selected = mode; used = 0; calls = 0;
    memset(captured, 0, sizeof captured);
    int expected_status = mode == 0 || mode == 1 || mode == 4 ? 42 : 2;
    const char *expected = normal;
    if (mode == 1) expected = "fatal trap: division by zero\n  at app.origin\n  at app.caller\n";
    if (mode == 2 || mode == 5) expected = "";
    if (mode == 3) expected = "unh";
    if (mode == 6) expected = "unhandled error: app.Failure\n  at app.origin\n  [trace truncated]\n";
    if (report_probe(mode) != expected_status) return 10 + mode;
    if (used != strlen(expected) || memcmp(captured, expected, used) != 0) return 20 + mode;
  }
  return 42;
}
