#include <stddef.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <signal.h>

extern int context_render_probe(int mode);
extern int context_cancel_probe(int mode);
extern int context_fatal_probe(int mode);
static _Alignas(16) unsigned char arena[65536];
static void *live[32];
static size_t allocation_sizes[32];
static size_t cursor;
static unsigned calls;
static unsigned failure;
static unsigned invalid;
static char captured_storage[2048];
static char *captured = captured_storage;
static size_t used;
static unsigned writes;
static int mode;

ssize_t write(int descriptor, const void *bytes, size_t length) {
  ++writes;
  if (descriptor != 2 || used + length >= sizeof captured_storage) { errno = EINVAL; return -1; }
  if (mode == 6 && writes > 1) { errno = EIO; return -1; }
  size_t committed = length > 3 ? 3 : length;
  memcpy(captured + used, bytes, committed);
  used += committed;
  captured[used] = 0;
  return (ssize_t)committed;
}

void *malloc(size_t bytes) {
  unsigned ordinal = ++calls;
  if (ordinal == failure) return NULL;
  if (ordinal > 32 || bytes > sizeof(arena) - cursor) return NULL;
  void *result = arena + cursor;
  cursor += (bytes + 15) & ~(size_t)15;
  live[ordinal - 1] = result;
  allocation_sizes[ordinal - 1] = bytes;
  return result;
}

void free(void *pointer) {
  if (pointer == NULL) return;
  for (unsigned index = 0; index < 32; ++index) {
    if (live[index] == pointer) { memset(pointer, 0xDD, allocation_sizes[index]); live[index] = NULL; return; }
  }
  invalid = 1;
}

int main(void) {
  char normal[2048] = {0};
  for (mode = 0; mode <= 10; ++mode) {
    cursor = 0; calls = 0; used = 0; writes = 0; captured[0] = 0;
    failure = mode == 7 ? 1u : mode == 8 ? 2u : 0u;
    int outcome = context_render_probe(mode);
    if (outcome != (mode == 10 ? 42 : 1)) { fprintf(stderr, "context mode %d returned %d\n", mode, outcome); return 70 + mode; }
    unsigned expected_calls = mode == 1 ? 0u : mode == 7 ? 1u : 2u;
    if (calls != expected_calls || invalid) return 90;
    for (unsigned index = 0; index < 32; ++index)
      if (live[index] != NULL) return 91;
    if (mode == 10) { if (used != 0) return 100; continue; }
    if (mode == 4) { if (used != 0) return 84; continue; }
    if (mode == 6) { if (strcmp(captured, "unh") != 0) return 86; continue; }
    const char *prefix = "unhandled error: report-conformance/root.Secondary\n  at report-conformance/root.replace ";
    if (strncmp(captured, prefix, strlen(prefix)) != 0) { fprintf(stderr, "context mode %d output: %s\n", mode, captured); return 80 + mode; }
    if (mode == 0) {
      if (strstr(captured, "  at report-conformance/root.replacement ") == NULL ||
          strstr(captured, "while handling: report-conformance/root.Primary\n  at report-conformance/root.primary ") == NULL ||
          strstr(captured, "  at report-conformance/root.relay ") == NULL ||
          strstr(captured, "[trace truncated]") != NULL) { fprintf(stderr, "context output: %s\n", captured); return 92; }
      memcpy(normal, captured, used + 1);
    } else if (mode == 9) {
      if (strcmp(normal, captured) != 0) { fprintf(stderr, "repeated context output: %s\n", captured); return 99; }
    } else {
      const char *marker = strstr(captured, "  [trace truncated]\n");
      if (marker == NULL || marker[strlen("  [trace truncated]\n")] != 0) return 93;
    }
  }
  for (int notify = 0; notify < 2; ++notify) {
    cursor = 0; calls = 0; used = 0; writes = 0; captured[0] = 0; failure = 0; mode = 11;
    if (context_cancel_probe(notify) != 42 || used != 0 || invalid) return 101;
    for (unsigned index = 0; index < 32; ++index)
      if (live[index] != NULL) return 102;
  }
  captured = mmap(NULL, sizeof captured_storage, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  if (captured == MAP_FAILED) return 103;
  for (int fatal_mode = 0; fatal_mode < 2; ++fatal_mode) {
    cursor = 0; calls = 0; used = 0; writes = 0; captured[0] = 0; mode = 12;
    pid_t child = fork();
    if (child < 0) return 104;
    if (child == 0) { (void)context_fatal_probe(fatal_mode); _exit(105); }
    int status;
    while (waitpid(child, &status, 0) < 0) if (errno != EINTR) return 106;
    if (!WIFSIGNALED(status) || (WTERMSIG(status) != SIGILL && WTERMSIG(status) != SIGTRAP)) return 107;
    const char *prefix = "fatal trap: division by zero\n  at report-conformance/root.";
    if (strncmp(captured, prefix, strlen(prefix)) != 0) return 108;
    if (fatal_mode == 0) {
      if (strstr(captured, "while handling: report-conformance/root.Secondary\n") == NULL ||
          strstr(captured, "while handling: report-conformance/root.Primary\n") == NULL ||
          strstr(captured, "  at report-conformance/root.relay ") == NULL) return 109;
    } else if (strstr(captured, "while handling:") != NULL) return 110;
    if (strstr(captured, "[trace truncated]") != NULL || strstr(captured + 1, "fatal trap:") != NULL) return 111;
  }
  if (munmap(captured, sizeof captured_storage) != 0) return 112;
  return 42;
}
