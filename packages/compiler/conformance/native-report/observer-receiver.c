#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>

extern int32_t observer_probe(int32_t);
extern int32_t bare_probe(int32_t);
extern int32_t observer_has_context(void);
static unsigned drops;
static unsigned events;
static uintptr_t observed_live;
static uintptr_t observed_produced;
static unsigned checkpoints;
static unsigned payload_drops;
static unsigned unhandled_events;
static unsigned absent_checks;
static unsigned empty_drops;
void observer_absent(uintptr_t policy, int32_t mode) {
  if (policy != 0 || ++absent_checks != 1) _exit(137);
  if (mode == 19) {
    if (observed_live != 0 || unhandled_events != 0) _exit(138);
  } else {
    if (observed_live != (mode == 17 ? 0u : 1u) || observed_produced != 3 || payload_drops != 1) _exit(139);
    if (unhandled_events != (mode == 17 ? 1u : 0u)) _exit(140);
  }
}
void observer_empty_drop(void) {
  if (++empty_drops != 1 || observed_live != 1 || observed_produced != 3) _exit(141);
}
void observer_unhandled(uintptr_t handle, int32_t salt) {
  if (handle != 1 || observed_live != 1 || observed_produced != 3 || payload_drops != 1) _exit(135);
  if (++unhandled_events != 1 || (salt != 21 && salt != 23 && salt != 24)) _exit(136);
  if (salt == 23) close(2);
}
void observer_payload_drop(void) {
  if (observed_live != 1 || observed_produced != 3) _exit(133);
  ++payload_drops;
}
void observer_live(uintptr_t live, uintptr_t produced) { observed_live = live; observed_produced = produced; }
void observer_checkpoint(void) {
  if (observed_live != 0) _exit(132);
  ++checkpoints;
}

void observer_drop(void) { ++drops; }
void observer_context(uintptr_t live, uintptr_t produced) {
  if (live != 0) _exit(130);
  if (produced == 0) _exit(131);
}
void observer_event(uint8_t event, uintptr_t first, uintptr_t second, int32_t salt) {
  if (event != 6) {
    dprintf(2, "unexpected event %u, first %zu, second %zu, salt %d\n", event, (size_t)first, (size_t)second, salt);
    _exit(100 + event);
  }
  if (first != (salt == 19 ? 7u : (salt == 17 || salt == 18) ? 5u : (salt == 16 || salt == 20 || salt == 21) ? 1u : 0u)) _exit(110);
  if (salt == 21 && payload_drops != 1) _exit(134);
  if (second != 0) _exit(111);
  if (++events != 1) _exit(112);
  if (drops != (salt == 14 ? 2u : 1u)) _exit(120 + (int)drops);
  if (salt != 9 && salt != 11 && salt != 12 && salt != 14 && salt != 15 && salt != 16 && salt != 17 && salt != 18 && salt != 19 && salt != 20 && salt != 21) _exit(82);
  if (write(2, "observer\n", 9) != 9) _exit(83);
  if (salt == 12) (void)bare_probe(0);
  if (salt == 15) close(2);
}

int main(void) {
  int32_t initial = observer_probe(1);
  if (initial != 42) return initial;
  if (drops != 1) return 30 + (int)drops;
  if (events != 0) return 40 + (int)events;
  const int modes[] = {0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21};
  const unsigned mode_count = observer_has_context() ? 21u : 6u;
  for (unsigned index = 0; index < mode_count; ++index) {
    int descriptors[2];
    if (pipe(descriptors) != 0) return 11;
    pid_t child = fork();
    if (child < 0) return 12;
    if (child == 0) {
      close(descriptors[0]);
      if (dup2(descriptors[1], 2) != 2) _exit(84);
      close(descriptors[1]);
      int32_t result = modes[index] == 4 ? bare_probe(0) : observer_probe(modes[index]);
      if (modes[index] == 12 && result == 42 && drops == 2 && events == 0 && checkpoints == 1) _exit(42);
      if (modes[index] == 14 && result == 42 && drops == 2 && events == 0 && checkpoints == 1 && payload_drops == 1) _exit(42);
      if ((modes[index] == 15 || modes[index] == 16) && result == 42 && drops == 2 && events == 0 && checkpoints == 1 && payload_drops == 1 && unhandled_events == 1) _exit(42);
      if (modes[index] == 19 && result == 42 && drops == 1 && events == 0 && absent_checks == 1 && unhandled_events == 0) _exit(42);
      if ((modes[index] == 17 || modes[index] == 18 || modes[index] == 20 || modes[index] == 21) && result == 42 && drops == 2 && events == 0 && checkpoints == 1 && payload_drops == 1 && unhandled_events == 1 && absent_checks == 1 && empty_drops == (modes[index] == 18 ? 1u : 0u)) _exit(42);
      _exit(85);
    }
    close(descriptors[1]);
    char output[2048] = {0};
    size_t used = 0;
    for (;;) {
      ssize_t count = read(descriptors[0], output + used, sizeof output - used - 1);
      if (count < 0 && errno == EINTR) continue;
      if (count < 0) return 13;
      if (count == 0) break;
      used += (size_t)count;
      if (used == sizeof output - 1) return 14;
    }
    close(descriptors[0]);
    int status;
    while (waitpid(child, &status, 0) < 0) if (errno != EINTR) return 15;
    if (modes[index] == 19 && WIFEXITED(status) && WEXITSTATUS(status) == 42 && used == 0) continue;
    if ((modes[index] == 15 || modes[index] == 16 || modes[index] == 17 || modes[index] == 18 || modes[index] == 20 || modes[index] == 21) && WIFEXITED(status) && WEXITSTATUS(status) == 42) {
      if (modes[index] == 16 && used == 0) continue;
      const char *prefix = "unhandled error: report-conformance/root.CleanupPrimary\n  at report-conformance/root.cleanupPrimary ";
      if (modes[index] != 16 && strncmp(output, prefix, strlen(prefix)) == 0) continue;
      return 28;
    }
    if ((modes[index] == 12 || modes[index] == 14) && WIFEXITED(status) && WEXITSTATUS(status) == 42 && used == 0) continue;
    if (WIFEXITED(status)) {
      fprintf(stderr, "observer mode %d exited with %d: %s\n", modes[index], WEXITSTATUS(status), output);
      return WEXITSTATUS(status);
    }
    if (!WIFSIGNALED(status)) return 16;
    if (WTERMSIG(status) != SIGILL && WTERMSIG(status) != SIGTRAP) return 128 + WTERMSIG(status);
    if (modes[index] == 0 || modes[index] == 5 || modes[index] >= 7) {
      const char *report = output;
      if (modes[index] == 10) {
        const char *frames[] = {"  at silk/effect.Effect.catch ", "  at report-conformance/root.selectiveFailure "};
        for (unsigned frame = 0; frame < 2; ++frame) {
          if (strncmp(report, frames[frame], strlen(frames[frame])) != 0) return 24;
          const char *end = strchr(report, '\n');
          if (end == NULL) return 25;
          report = end + 1;
        }
      }
      if (modes[index] >= 8 && modes[index] <= 10) {
        const char *frames[] = {"  at report-conformance/root.recoverInput ", "  at silk/effect.Effect.catchAll "};
        for (unsigned frame = 0; frame < 2; ++frame) {
          if (strncmp(report, frames[frame], strlen(frames[frame])) != 0) return 22;
          const char *end = strchr(report, '\n');
          if (end == NULL) return 23;
          report = end + 1;
        }
      }
      const char *prefix = "observer\nfatal trap: division by zero\n  at ";
      if (strncmp(report, prefix, strlen(prefix)) != 0) return 17;
      if (strstr(output, modes[index] == 13 ? "cleanupRecovery" : modes[index] == 11 ? "allocationRecovery" : modes[index] >= 8 ? "terminalRecovery" : modes[index] == 7 ? "recoverInput" : "application") == NULL) return 18;
    } else if (modes[index] == 4) {
      if (used != 0) return 19;
    } else if (strcmp(output, "observer\n") != 0) return 20;
  }
  if (observer_probe(1) != 42 || drops != 2 || events != 0) return 21;
  return 42;
}
