#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
_Static_assert(sizeof(int) == 4, "C int width");
_Static_assert(sizeof(size_t) == sizeof(void *), "usize width");
_Static_assert(ERANGE == 34, "cwd growth errno");
extern int input_probe(int, const unsigned char *const *, const unsigned char *const *);
static _Alignas(4096) unsigned char arena[16777216];
static void *addresses[8192];
static unsigned char live[8192];
static size_t cursor;
static int calls, selected, count, invalid, cwd_mode, grew;
static unsigned char argument[] = {255, 32, 128, 61, 0};
static unsigned char variable[] = {'A', '=', 128, '=', 255, 0};
static char *snapshot_environment[] = {"SILK_INPUT_SNAPSHOT=ok", NULL};
#ifdef __APPLE__
static char **process_environment = snapshot_environment;
char ***_NSGetEnviron(void) { return &process_environment; }
#else
char **environ = snapshot_environment;
#endif
void mutate_inputs(void) { argument[0] = 1; variable[2] = 1; }
void *malloc(size_t size) {
  if (++calls == selected) return NULL;
  size_t aligned = (cursor + 63) & ~(size_t)63;
  if (aligned > sizeof(arena) || size > sizeof(arena) - aligned || count == 8192) {
    invalid = 1; return NULL;
  }
  void *address = arena + aligned;
  cursor = aligned + size;
  addresses[count] = address; live[count++] = 1;
  return address;
}
void free(void *pointer) {
  if (!pointer) return;
  for (int i = 0; i < count; ++i) {
    if (addresses[i] == pointer) {
      if (!live[i]) invalid = 1;
      live[i] = 0; return;
    }
  }
  invalid = 1;
}
char *getcwd(char *buffer, size_t size) {
  if (cwd_mode == 1) { errno = ENOENT; return NULL; }
  if (cwd_mode == 2 || size < 1024) { grew = 1; errno = ERANGE; return NULL; }
  if (cwd_mode == 3) { memset(buffer, 'x', size); return buffer; }
  memcpy(buffer, "/cwd", 5);
  return buffer;
}
static int exercise(int fail_at, int mode) {
  cursor = 0; calls = 0; count = 0; invalid = 0; selected = fail_at;
  cwd_mode = mode; grew = 0;
  memset(live, 0, sizeof(live));
  argument[0] = 255; variable[2] = 128;
  const unsigned char *args[] = {(const unsigned char *)"program", argument, (const unsigned char *)"", NULL};
  const unsigned char *env[] = {variable, (const unsigned char *)"A=second", (const unsigned char *)"EMPTY=", (const unsigned char *)"malformed", NULL};
  int result = input_probe(3, args, env);
  for (int i = 0; i < count; ++i) if (live[i]) invalid = 1;
  if (invalid) return 1;
  if (result != (fail_at || mode ? 14 : 42)) return 2;
  if (fail_at && calls != fail_at) return 3;
  if (!fail_at && mode != 1 && !grew) return 4;
  return 0;
}
int main(void) {
  if (exercise(0, 0)) return 1;
  int allocations = calls;
  for (int i = 1; i <= allocations; ++i) if (exercise(i, 0)) return 2;
  for (int mode = 1; mode <= 3; ++mode) if (exercise(0, mode)) return 3;
  return 42;
}
