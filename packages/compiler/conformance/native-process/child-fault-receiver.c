#define _GNU_SOURCE 1
#include <errno.h>
#include <stdarg.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <unistd.h>

extern int process_fixture(void);
static pid_t parent;
static int mode;
static volatile struct { unsigned injected, writes, exited; int status; } *observed;
static int child(void) { return getpid() != parent; }
static int refuse(int at) {
  if (child() && mode == at) { ++observed->injected; errno = EIO; return 1; }
  return 0;
}
int fixture_allocation(void) { return 0; }
int dup2(int from, int to) {
  if (refuse(to == 3 ? 1 : to + 3)) return -1;
  return (int)syscall(SYS_dup3, from, to, 0);
}
int fcntl(int fd, int command, ...) {
  va_list arguments; va_start(arguments, command);
  int argument = va_arg(arguments, int); va_end(arguments);
  if (command == F_SETFD && refuse(2)) return -1;
  return (int)syscall(SYS_fcntl, fd, command, argument);
}
int chdir(const char *path) {
  if (refuse(6)) return -1;
  return (int)syscall(SYS_chdir, path);
}
int execve(const char *path, char *const arguments[], char *const environment[]) {
  if (strcmp(path, "/fixture") || strcmp(arguments[0], "/fixture") ||
      strcmp(arguments[1], "argument") || arguments[2] ||
      strcmp(environment[0], "name=value") || environment[1]) {
    errno = EINVAL; return -1;
  }
  char directory[8];
  if (syscall(SYS_getcwd, directory, sizeof directory) < 0 || strcmp(directory, "/") ||
      syscall(SYS_fcntl, 3, F_GETFD, 0) != FD_CLOEXEC ||
      (syscall(SYS_fcntl, 0, F_GETFL, 0) & O_ACCMODE) != O_RDONLY ||
      (syscall(SYS_fcntl, 1, F_GETFL, 0) & O_ACCMODE) != O_WRONLY ||
      (syscall(SYS_fcntl, 2, F_GETFL, 0) & O_ACCMODE) != O_WRONLY) {
    errno = EINVAL; return -1;
  }
  for (int fd = 4; fd < 256; ++fd)
    if (syscall(SYS_fcntl, fd, F_GETFD, 0) != -1 || errno != EBADF) {
      errno = EINVAL; return -1;
    }
  ++observed->injected; errno = EACCES; return -1;
}
ssize_t write(int fd, const void *bytes, size_t length) {
  if (!child()) return (ssize_t)syscall(SYS_write, fd, bytes, length);
  unsigned call = ++observed->writes;
  if (mode == 8 && call == 1) { errno = EINTR; return -1; }
  if (mode == 9) return 0;
  if (mode == 10 && call > 1) { errno = EIO; return -1; }
  return (ssize_t)syscall(SYS_write, fd, bytes, length > 1 ? 1 : length);
}
_Noreturn void _exit(int status) {
  observed->status = status; ++observed->exited;
  syscall(SYS_exit_group, status);
  __builtin_unreachable();
}
int main(void) {
  parent = getpid();
  observed = mmap(NULL, sizeof *observed, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  if ((const void *)observed == MAP_FAILED) return 1;
  int baseline[256];
  for (int fd = 0; fd < 256; ++fd) baseline[fd] = (int)syscall(SYS_fcntl, fd, F_GETFD, 0);
  for (mode = 1; mode <= 10; ++mode) {
    observed->injected = observed->writes = observed->exited = 0; observed->status = 0;
    int result = process_fixture();
    int expected = mode <= 6 ? EIO : mode == 9 ? 127 : mode == 10 ? 0 : EACCES;
    int status;
    if (waitpid(-1, &status, WNOHANG) != -1 || errno != ECHILD) return 2;
    for (int fd = 0; fd < 256; ++fd)
      if ((int)syscall(SYS_fcntl, fd, F_GETFD, 0) != baseline[fd]) return 3;
    /* The parent may kill a child after receiving its complete error packet. */
    unsigned writes = mode == 8 ? 5 : mode == 9 ? 1 : mode == 10 ? 2 : 4;
    if (result != expected || observed->injected != 1 || observed->writes != writes ||
        (observed->exited && observed->status != 127)) {
      fprintf(stderr, "child mode=%d result=%d expected=%d injected=%u writes=%u exited=%u status=%d\n",
        mode, result, expected, observed->injected, observed->writes, observed->exited, observed->status);
      return 4;
    }
  }
  if (munmap((void *)observed, sizeof *observed)) return 5;
  puts("10 real child startup faults preserve descriptors and reap ownership");
  return 42;
}
