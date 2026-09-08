#define _GNU_SOURCE 1
#include <errno.h>
#include <stdarg.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

extern int process_fixture(void);
static int allocation_ordinal, allocation_selected;
static int ordinal, selected, failed, next_fd, live[256], role[256], reads[256];
static int pipes, acquired, reaped, killed, actions, attributes, violated;
static int interrupted_poll, interrupted_read, interrupted_wait, wait_stage, notice_kind;
static int step(void) {
  if (++ordinal == selected) {failed = 1; errno = EIO; return 1;}
  return 0;
}
int fixture_allocation(void) {
  if (++allocation_ordinal == allocation_selected) {failed = 2; return 1;}
  return 0;
}
static int descriptor(int kind) {
  int fd = next_fd++;
  if (fd >= 256) abort();
  live[fd] = 1; role[fd] = kind; return fd;
}
int open(const char *path, int flags, ...) {
  if (strcmp(path, "/dev/null") || !(flags & O_CLOEXEC)) violated = 1;
  if (step()) return -1;
  return descriptor(0);
}
int close(int fd) {
  if (fd < 0 || fd >= 256 || !live[fd]) {violated = 1; return -1;}
  live[fd] = 0;
  if (step()) return -1;
  if (failed) {errno = EACCES; return -1;}
  return 0;
}
int fcntl(int fd, int command, ...) {
  if (fd < 0 || fd >= 256 || !live[fd] || command != F_DUPFD_CLOEXEC) violated = 1;
  va_list values; va_start(values, command); int minimum = va_arg(values, int); va_end(values);
  if (minimum != 4) violated = 1;
  if (step()) return -1;
  if (next_fd < minimum) next_fd = minimum;
  return descriptor(role[fd]);
}
static int pair(int fds[2]) {
  if (step()) return -1;
  ++pipes;
  fds[0] = descriptor(pipes); fds[1] = descriptor(0); return 0;
}
#if defined(__APPLE__)
int pipe(int fds[2]) {return pair(fds);}
int posix_spawn_file_actions_init(posix_spawn_file_actions_t *p) {
  if (step()) return EIO;
  memset(p, 0, sizeof(*p)); ++actions; return 0;
}
int posix_spawn_file_actions_destroy(posix_spawn_file_actions_t *p) {
  (void)p; if (--actions != 0) violated = 1; return failed ? EACCES : 0;
}
int posix_spawnattr_init(posix_spawnattr_t *p) {
  if (step()) return EIO;
  memset(p, 0, sizeof(*p)); ++attributes; return 0;
}
int posix_spawnattr_destroy(posix_spawnattr_t *p) {
  (void)p; if (--attributes != 0) violated = 1; return failed ? EACCES : 0;
}
int posix_spawnattr_setflags(posix_spawnattr_t *p, short flags) {
  (void)p; if (flags != POSIX_SPAWN_CLOEXEC_DEFAULT) violated = 1;
  return step() ? EIO : 0;
}
int posix_spawn_file_actions_adddup2(posix_spawn_file_actions_t *p, int from, int to) {
  (void)p; if (from < 4 || !live[from] || to < 0 || to > 2) violated = 1;
  return step() ? EIO : 0;
}
int posix_spawn_file_actions_addclose(posix_spawn_file_actions_t *p, int fd) {
  (void)p; if (fd < 4 || !live[fd]) violated = 1; return step() ? EIO : 0;
}
int posix_spawn_file_actions_addchdir_np(posix_spawn_file_actions_t *p, const char *path) {
  (void)p; if (strcmp(path, "/")) violated = 1; return step() ? EIO : 0;
}
int posix_spawn(pid_t *pid, const char *path, const posix_spawn_file_actions_t *a,
                const posix_spawnattr_t *b, char *const argv[], char *const env[]) {
  (void)a; (void)b;
  if (strcmp(path,"/fixture") || strcmp(argv[0],"/fixture") || strcmp(argv[1],"argument") || argv[2] || strcmp(env[0],"name=value") || env[1]) violated = 1;
  if (step()) return EIO;
  *pid = 4242; ++acquired; return 0;
}
#else
int pipe2(int fds[2], int flags) {
  if (flags != O_CLOEXEC) violated = 1; return pair(fds);
}
pid_t fork(void) {if (step()) return -1; ++acquired; return 4242;}
#endif
int poll(struct pollfd *fds, nfds_t count, int timeout) {
  if (count != 2 || timeout != -1) violated = 1;
  if (step()) return -1;
  if (!interrupted_poll++) {errno = EINTR; return -1;}
  int ready = 0;
  for (nfds_t i = 0; i < count; ++i) {
    fds[i].revents = fds[i].fd < 0 ? 0 : POLLIN;
    if (fds[i].fd >= 0) {if (!live[fds[i].fd]) violated = 1; ++ready;}
  }
  return ready;
}
ssize_t read(int fd, void *buffer, size_t count) {
  if (fd < 0 || fd >= 256 || !live[fd] || count == 0) violated = 1;
  if (step()) return -1;
  if (!interrupted_read++) {errno = EINTR; return -1;}
  if (role[fd] == 3) {
    int offset = reads[fd]++;
    int length = notice_kind == 2 ? 1 : notice_kind ? 4 : 0;
    if (offset >= length) return 0;
    ((unsigned char *)buffer)[0] = notice_kind == 3 ? (offset == 3 ? 128 : 0) : (offset == 0 ? ENOENT : 0);
    return 1;
  }
  if (reads[fd]++) return 0;
  ((unsigned char *)buffer)[0] = 'x'; return 1;
}
int kill(pid_t pid, int signal) {
  if (pid != 4242 || signal != SIGKILL || acquired != 1 || reaped || killed++) violated = 1;
  errno = EACCES; return -1; /* Cleanup must still reap, preserving the primary failure. */
}
pid_t waitpid(pid_t pid, int *status, int options) {
  if (pid != 4242 || options || acquired != 1 || reaped) violated = 1;
  if (step()) return -1;
  if (!interrupted_wait++) {errno = EINTR; return -1;}
  if (wait_stage++ == 0) {*status = (SIGSTOP << 8) | 127; return pid;}
  if (wait_stage == 2) {*status = 65535; return pid;}
  *status = 7 << 8; ++reaped; return pid;
}
static int one(int fail_at, int allocation_at) {
  allocation_ordinal = 0; allocation_selected = allocation_at;
  ordinal = failed = pipes = acquired = reaped = killed = actions = attributes = violated = 0;
  interrupted_poll = interrupted_read = interrupted_wait = wait_stage = 0;
  selected = fail_at; next_fd = 0;
  memset(live,0,sizeof(live)); memset(role,0,sizeof(role)); memset(reads,0,sizeof(reads));
  int result = process_fixture();
  for (int fd = 0; fd < 256; ++fd) if (live[fd]) violated = 1;
  if (actions || attributes || acquired != reaped || violated || result != (failed == 2 ? 93 : failed ? EIO : notice_kind == 1 ? ENOENT : notice_kind ? 0 : 42)) {
    fprintf(stderr,"ordinal=%d calls=%d result=%d failed=%d acquired=%d reaped=%d killed=%d handles=%d/%d violation=%d\n", fail_at,ordinal,result,failed,acquired,reaped,killed,actions,attributes,violated);
    return 1;
  }
  return 0;
}
int main(void) {
  if (one(0, 0)) return 1;
  int calls = ordinal;
  int allocations = allocation_ordinal;
  for (int i = 1; i <= calls; ++i) if (one(i, 0)) return 2;
  for (int i = 1; i <= allocations; ++i) if (one(0, i)) return 3;
  printf("%d foreign-call failure ordinals preserve resources and primary error\n", calls);
  printf("%d allocation failures preserve descriptors and child ownership\n", allocations);
#if !defined(__APPLE__)
  for (notice_kind = 1; notice_kind <= 3; ++notice_kind) if (one(0, 0)) return 4;
  puts("fragmented, truncated and malformed startup notices preserve child ownership");
#endif
  return 42;
}
