#include <stdint.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <asm/unistd.h>
extern uint64_t sum(uint64_t, uint64_t);
extern void store(uint64_t *, uint64_t);
extern uint64_t load(const uint64_t *);
extern uint64_t silk_syscall(uint64_t) __asm__("syscall");
extern void terminal(void);
extern uint64_t sum7(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
extern uint64_t *alias(uint64_t *);
static void trapped(int signal_number) { _Exit(signal_number == SIGILL || signal_number == SIGTRAP ? 74 : 98); }
int main(int argc, char **argv) {
  (void)argv;
  if (argc > 1) { signal(SIGILL, trapped); signal(SIGTRAP, trapped); terminal(); return 99; }
  uint64_t value = 7;
  if (sum(17, 25) != 42) return 1;
  if (sum7(1, 2, 4, 8, 16, 32, 64) != 127 || alias(&value) != &value) return 4;
  store(&value, 73);
  if (value != 73 || load(&value) != 73) return 2;
  if (silk_syscall(__NR_getpid) != (uint64_t)getpid()) return 3;
  return 0;
}
