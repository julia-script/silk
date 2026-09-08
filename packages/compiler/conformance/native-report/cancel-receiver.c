#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <string.h>
extern int32_t cancel_probe(int32_t);
static unsigned guard_drops, owner_drops, events;
static int scenario;
void cancel_drop(int32_t which) {
  if (which == 1) {
    if (scenario == 5 && owner_drops != 1) _exit(74);
    ++guard_drops;
  }
  else if (which == 2) ++owner_drops;
  else _exit(70);
}
void cancel_event(int32_t mode) {
  if (scenario == 4 || scenario == 5) _exit(75);
  if (scenario == 6) {
    if (mode != 6 || guard_drops != 0 || owner_drops != 0 || ++events != 1) _exit(76);
    return;
  }
  if ((mode != 1 && mode != 3) || guard_drops != 1 || owner_drops != (mode == 3 ? 1u : 0u) || ++events != 1) _exit(71);
}
int main(void) {
  const int modes[] = {0, 1, 2, 3, 5, 4, 6};
  for (unsigned index = 0; index < sizeof modes / sizeof modes[0]; ++index) {
    int mode = modes[index];
    int fd[2];
    if (pipe(fd) != 0) return 10;
    pid_t child = fork();
    if (child < 0) return 11;
    if (child == 0) {
      scenario = mode;
      close(fd[0]);
      if (dup2(fd[1], 2) != 2) _exit(72);
      close(fd[1]);
      int code = cancel_probe(mode);
      _exit(code == 42 && guard_drops == 1 && owner_drops == 1 && events == 0 ? 42 : 73);
    }
    close(fd[1]);
    char output[2048] = {0};
    size_t used = 0;
    for (;;) {
      ssize_t count = read(fd[0], output + used, sizeof output - used - 1);
      if (count < 0 && errno == EINTR) continue;
      if (count < 0) return 12;
      if (count == 0) break;
      used += (size_t)count;
      if (used == sizeof output - 1) return 13;
    }
    close(fd[0]);
    int status;
    while (waitpid(child, &status, 0) < 0) if (errno != EINTR) return 14;
    if (mode == 0) {
      if (!WIFEXITED(status) || WEXITSTATUS(status) != 42 || used != 0) return 15;
    } else {
      if (!WIFSIGNALED(status) || (WTERMSIG(status) != SIGILL && WTERMSIG(status) != SIGTRAP)) return 20 + mode;
      if ((mode == 1 || mode == 3 || mode == 6) && strncmp(output, "fatal trap: division by zero\n  at ", strlen("fatal trap: division by zero\n  at ")) != 0) return 17;
      if ((mode == 2 || mode == 4 || mode == 5) && used != 0) return 18;
    }
  }
  return 42;
}
