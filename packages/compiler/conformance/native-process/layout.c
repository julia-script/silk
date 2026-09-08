#define _GNU_SOURCE 1
#include <spawn.h>
#include <poll.h>
#include <stdio.h>
#include <stddef.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <signal.h>
_Static_assert(sizeof(pid_t) == 4, "pid_t");
_Static_assert(sizeof(struct pollfd) == 8, "pollfd size");
_Static_assert(offsetof(struct pollfd, fd) == 0 && offsetof(struct pollfd, events) == 4 && offsetof(struct pollfd, revents) == 6, "pollfd fields");
_Static_assert(POLLIN == 1 && POLLHUP == 16 && POLLERR == 8 && POLLNVAL == 32, "poll flags");
_Static_assert(SIGKILL == 9, "kill signal");
#if defined(__APPLE__)
_Static_assert(sizeof(posix_spawn_file_actions_t) == 8 && _Alignof(posix_spawn_file_actions_t) == 8, "actions handle");
_Static_assert(sizeof(posix_spawnattr_t) == 8 && _Alignof(posix_spawnattr_t) == 8, "attribute handle");
_Static_assert(sizeof(nfds_t) == 4 && POSIX_SPAWN_CLOEXEC_DEFAULT == 0x4000, "Darwin constants");
#else
_Static_assert(sizeof(posix_spawn_file_actions_t) == 80 && _Alignof(posix_spawn_file_actions_t) == 8, "actions object");
_Static_assert(offsetof(posix_spawn_file_actions_t, __allocated) == 0 && offsetof(posix_spawn_file_actions_t, __used) == 4 && offsetof(posix_spawn_file_actions_t, __actions) == 8 && offsetof(posix_spawn_file_actions_t, __pad) == 16, "actions fields");
_Static_assert(sizeof(nfds_t) == 8, "GNU nfds_t");
#endif

#include <errno.h>
_Static_assert(F_SETFD == 2 && FD_CLOEXEC == 1, "descriptor flags");
_Static_assert(EINTR == 4 && ECHILD == 10 && ENOENT == 2, "process errors");
#if defined(__APPLE__)
_Static_assert(O_CLOEXEC == 16777216 && F_DUPFD_CLOEXEC == 67, "Darwin descriptor constants");
_Static_assert(ENOSYS == 78 && ENOTSUP == 45 && ENAMETOOLONG == 63 && ELOOP == 62 && EAGAIN == 35, "Darwin errors");
#else
_Static_assert(O_CLOEXEC == 524288 && F_DUPFD_CLOEXEC == 1030, "GNU descriptor constants");
_Static_assert(ENOSYS == 38 && ENOTSUP == 95 && ENAMETOOLONG == 36 && ELOOP == 40 && EAGAIN == 11, "GNU errors");
#endif

#define SIGNATURE(name, type) _Static_assert(__builtin_types_compatible_p(__typeof__(&(name)), type), #name " signature")
SIGNATURE(close, int (*)(int));
SIGNATURE(read, ssize_t (*)(int, void *, size_t));
SIGNATURE(write, ssize_t (*)(int, const void *, size_t));
SIGNATURE(waitpid, pid_t (*)(pid_t, int *, int));
SIGNATURE(kill, int (*)(pid_t, int));
SIGNATURE(open, int (*)(const char *, int, ...));
SIGNATURE(fcntl, int (*)(int, int, ...));
SIGNATURE(poll, int (*)(struct pollfd *, nfds_t, int));
#if defined(__APPLE__)
SIGNATURE(__error, int *(*)(void));
SIGNATURE(pipe, int (*)(int *));
SIGNATURE(posix_spawn_file_actions_init, int (*)(posix_spawn_file_actions_t *));
SIGNATURE(posix_spawn_file_actions_destroy, int (*)(posix_spawn_file_actions_t *));
SIGNATURE(posix_spawn_file_actions_adddup2, int (*)(posix_spawn_file_actions_t *, int, int));
SIGNATURE(posix_spawn_file_actions_addclose, int (*)(posix_spawn_file_actions_t *, int));
SIGNATURE(posix_spawn_file_actions_addchdir_np, int (*)(posix_spawn_file_actions_t *, const char *));
SIGNATURE(posix_spawnattr_init, int (*)(posix_spawnattr_t *));
SIGNATURE(posix_spawnattr_destroy, int (*)(posix_spawnattr_t *));
SIGNATURE(posix_spawnattr_setflags, int (*)(posix_spawnattr_t *, short));
SIGNATURE(posix_spawn, int (*)(pid_t *, const char *, const posix_spawn_file_actions_t *, const posix_spawnattr_t *, char *const *, char *const *));
#else
SIGNATURE(__errno_location, int *(*)(void));
SIGNATURE(pipe2, int (*)(int *, int));
SIGNATURE(fork, pid_t (*)(void));
SIGNATURE(dup2, int (*)(int, int));
SIGNATURE(closefrom, void (*)(int));
SIGNATURE(chdir, int (*)(const char *));
SIGNATURE(execve, int (*)(const char *, char *const *, char *const *));
SIGNATURE(_exit, void (*)(int));
#endif
#undef SIGNATURE
