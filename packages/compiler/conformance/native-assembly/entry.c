#include <stdint.h>
#include <stdlib.h>
_Noreturn void native_entry_probe(const uintptr_t *stack) {
  const char *const *argv = (const char *const *)(stack + 1);
  _Exit(stack[0] == 1 && argv[0] != NULL && argv[0][0] != '\0' && argv[1] == NULL ? 23 : 91);
}
