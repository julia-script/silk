#include <stddef.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <crt_externs.h>
static char ***(*const checked_environment)(void) = &_NSGetEnviron;
#else
extern char **environ;
#endif
_Static_assert(sizeof(int) == 4, "C int width");
_Static_assert(sizeof(size_t) == sizeof(void *), "usize width");
_Static_assert(ERANGE == 34, "cwd growth errno");
static char *(*const checked_getcwd)(char *, size_t) = &getcwd;
extern int input_probe(int, const unsigned char *const *, const unsigned char *const *);
static unsigned char argument[] = {255, 32, 128, 61, 0};
static unsigned char variable[] = {'A', '=', 128, '=', 255, 0};
void mutate_inputs(void) { argument[0] = 1; variable[2] = 1; }
int main(void) {
  const unsigned char *args[] = {(const unsigned char *)"program", argument, (const unsigned char *)"", NULL};
  const unsigned char *env[] = {variable, (const unsigned char *)"A=second", (const unsigned char *)"EMPTY=", (const unsigned char *)"malformed", NULL};
  if (checked_getcwd == NULL) return 100;
  if (setenv("SILK_INPUT_SNAPSHOT", "ok", 1) != 0) return 101;
#ifdef __APPLE__
  if (checked_environment() == NULL) return 102;
#else
  if (environ == NULL) return 102;
#endif
  return input_probe(3, args, env);
}
