/**
 * Compiler-owned native runtime symbols for the remaining child-process and host-input protocols.
 */
export const symbols = Object.freeze([
  'silk_os_process_execute_v1',
  'silk_os_process_capture_v1',
  'silk_os_host_argument_count_v1',
  'silk_os_host_argument_v1',
  'silk_os_host_variable_v1',
  'silk_os_host_working_directory_v1',
] as const)

export type Symbol = (typeof symbols)[number]

const statusPrelude = `
enum {
  SILK_NOT_FOUND = 0,
  SILK_ALREADY_EXISTS = 1,
  SILK_PERMISSION_DENIED = 2,
  SILK_INVALID_PATH = 3,
  SILK_WRONG_TYPE = 4,
  SILK_NOT_EMPTY = 5,
  SILK_NO_SPACE = 6,
  SILK_TOO_LARGE = 7,
  SILK_BUFFER_TOO_SMALL = 8,
  SILK_UNSUPPORTED = 9,
  SILK_OTHER = 10
};

static int silk_reason_from_errno(int value) {
  switch (value) {
    case ENOENT: return SILK_NOT_FOUND;
    case EEXIST: return SILK_ALREADY_EXISTS;
    case EACCES:
    case EPERM: return SILK_PERMISSION_DENIED;
    case ENOTDIR:
    case EISDIR: return SILK_WRONG_TYPE;
    case ENOTEMPTY: return SILK_NOT_EMPTY;
    case ENOSPC: return SILK_NO_SPACE;
    case EFBIG:
    case ENAMETOOLONG:
    case EOVERFLOW: return SILK_TOO_LARGE;
    case ELOOP:
    case EINVAL: return SILK_INVALID_PATH;
#ifdef ENOTSUP
    case ENOTSUP: return SILK_UNSUPPORTED;
#endif
    default: return SILK_OTHER;
  }
}

static void silk_failure(int *reason, uint32_t *native_code, int native) {
  *reason = silk_reason_from_errno(native);
  *native_code = (uint32_t)native;
}

static void silk_protocol_failure(int *reason, uint32_t *native_code, int selected) {
  *reason = selected;
  *native_code = 0;
}

static void silk_success(int *reason, uint32_t *native_code) {
  *reason = 0;
  *native_code = 0;
}

static int silk_transfer(size_t count, size_t *output) {
  *output = count;
  return 1;
}
`

const stringPrelude = `
static char *silk_string(const unsigned char *bytes, size_t length) {
  if (length == SIZE_MAX) return NULL;
  char *value = (char *)malloc(length + 1);
  if (value == NULL) return NULL;
  memcpy(value, bytes, length);
  value[length] = 0;
  return value;
}
`

const childProcessPrelude = `
/*
 * One completed child execution retains its captured streams here until the next execute replaces
 * them. The service is blocking and single-child by contract, so exactly one capture is live at a
 * time and no ownership passes to the caller.
 */
typedef struct { unsigned char *bytes; size_t length; } silk_capture;
static silk_capture silk_captures[2];

static void silk_capture_release(void) {
  for (int index = 0; index < 2; index += 1) {
    free(silk_captures[index].bytes);
    silk_captures[index].bytes = NULL;
    silk_captures[index].length = 0;
  }
}

static int silk_capture_append(silk_capture *capture, const unsigned char *bytes, size_t count) {
  unsigned char *grown = (unsigned char *)realloc(capture->bytes, capture->length + count + 1);
  if (grown == NULL) return 0;
  memcpy(grown + capture->length, bytes, count);
  capture->bytes = grown;
  capture->length += count;
  return 1;
}

/* Entries are NUL-terminated in place, so the terminator count is the entry count. */
static size_t silk_entry_count(const unsigned char *values, size_t length) {
  size_t count = 0;
  for (size_t index = 0; index < length; index += 1) {
    if (values[index] == 0) count += 1;
  }
  return count;
}

/* Borrows one NULL-terminated pointer vector over an already NUL-terminated entry block. */
static char **silk_entry_vector(const unsigned char *values, size_t length, char *leading) {
  size_t count = silk_entry_count(values, length) + (leading == NULL ? 0 : 1);
  char **vector = (char **)calloc(count + 1, sizeof(char *));
  if (vector == NULL) return NULL;
  size_t written = 0;
  if (leading != NULL) vector[written++] = leading;
  size_t start = 0;
  for (size_t index = 0; index < length; index += 1) {
    if (values[index] != 0) continue;
    vector[written++] = (char *)(uintptr_t)(const void *)(values + start);
    start = index + 1;
  }
  return vector;
}

static int silk_terminated(const unsigned char *values, size_t length) {
  return length == 0 || values[length - 1] == 0;
}

static void silk_child_failed(int channel, int value) {
  ssize_t ignored;
  do { ignored = write(channel, &value, sizeof(int)); } while (ignored < 0 && errno == EINTR);
  (void)ignored;
  _exit(127);
}
`

const hostInputPrelude = `
/* The command line the entry point captured. The shim defines the storage and fills it before it
   calls silk_main, so a host-input read never consults an ambient global of its own. */
extern int silk_host_argc_v1;
extern char **silk_host_argv_v1;

#if defined(__APPLE__)
#include <crt_externs.h>
#define silk_host_environ (*_NSGetEnviron())
#else
extern char **environ;
#define silk_host_environ environ
#endif

/* Copies the prefix of one host value that fits and reports the value's complete byte length, so a
   caller that received a short buffer can size an exact one and ask again. */
static int silk_host_copy(const unsigned char *value, size_t length,
                          unsigned char *output, size_t capacity, size_t *count,
                          int *reason, uint32_t *native_code) {
  size_t committed = length < capacity ? length : capacity;
  if (committed > 0) memcpy(output, value, committed);
  silk_success(reason, native_code);
  return silk_transfer(length, count);
}

/* An absent value: an index past the last argument, or an unset variable name. */
static int silk_host_absent(int *reason, uint32_t *native_code) {
  silk_protocol_failure(reason, native_code, SILK_NOT_FOUND);
  return 0;
}
`

const implementations: Readonly<Record<Symbol, string>> = Object.freeze({
  silk_os_process_execute_v1: `
/*
 * Runs one child to completion. The pre-exec channel is close-on-exec, so a failure to start is
 * reported as that errno rather than as an exit code, and both captures drain concurrently so a
 * child that fills one pipe while the parent waits on the other cannot deadlock.
 */
static int silk_spawn(char *program_string, char **argv, char **envp, char *directory_string,
                      int *status, int *code, int *reason, uint32_t *native_code) {
  int output[2];
  int errors[2];
  int notice[2];
  if (pipe(output) != 0) { silk_failure(reason, native_code, errno); return 0; }
  if (pipe(errors) != 0) {
    int selected = errno;
    close(output[0]); close(output[1]);
    silk_failure(reason, native_code, selected);
    return 0;
  }
  if (pipe(notice) != 0) {
    int selected = errno;
    close(output[0]); close(output[1]); close(errors[0]); close(errors[1]);
    silk_failure(reason, native_code, selected);
    return 0;
  }
  if (fcntl(notice[1], F_SETFD, FD_CLOEXEC) != 0) {
    int selected = errno;
    close(output[0]); close(output[1]); close(errors[0]); close(errors[1]);
    close(notice[0]); close(notice[1]);
    silk_failure(reason, native_code, selected);
    return 0;
  }
  pid_t child = fork();
  if (child < 0) {
    int selected = errno;
    close(output[0]); close(output[1]); close(errors[0]); close(errors[1]);
    close(notice[0]); close(notice[1]);
    silk_failure(reason, native_code, selected);
    return 0;
  }
  if (child == 0) {
    /* Standard input is closed by contract: the child reads an immediate end of input. */
    int null_fd = open("/dev/null", O_RDONLY);
    if (null_fd < 0) silk_child_failed(notice[1], errno);
    if (dup2(null_fd, 0) < 0 || dup2(output[1], 1) < 0 || dup2(errors[1], 2) < 0)
      silk_child_failed(notice[1], errno);
    if (null_fd > 2) close(null_fd);
    if (output[0] > 2) close(output[0]);
    if (output[1] > 2) close(output[1]);
    if (errors[0] > 2) close(errors[0]);
    if (errors[1] > 2) close(errors[1]);
    if (notice[0] > 2) close(notice[0]);
    if (directory_string != NULL && chdir(directory_string) != 0)
      silk_child_failed(notice[1], errno);
    execve(program_string, argv, envp);
    silk_child_failed(notice[1], errno);
  }
  close(output[1]); close(errors[1]); close(notice[1]);
  int reported = 0;
  ssize_t noticed;
  do { noticed = read(notice[0], &reported, sizeof(int)); } while (noticed < 0 && errno == EINTR);
  int notice_errno = errno;
  close(notice[0]);
  if (noticed != 0) {
    int waited_status = 0;
    pid_t waited;
    do { waited = waitpid(child, &waited_status, 0); } while (waited < 0 && errno == EINTR);
    close(output[0]); close(errors[0]);
    silk_failure(reason, native_code, noticed < 0 ? notice_errno : reported);
    return 0;
  }
  struct pollfd watched[2];
  watched[0].fd = output[0]; watched[0].events = POLLIN; watched[0].revents = 0;
  watched[1].fd = errors[0]; watched[1].events = POLLIN; watched[1].revents = 0;
  int live = 2;
  int failed = 0;
  int failure_errno = 0;
  int protocol = -1;
  while (live > 0 && failed == 0) {
    int ready = poll(watched, 2, -1);
    if (ready < 0) {
      if (errno == EINTR) continue;
      failed = 1; failure_errno = errno;
      break;
    }
    for (int index = 0; index < 2; index += 1) {
      if (watched[index].fd < 0 || watched[index].revents == 0) continue;
      unsigned char chunk[4096];
      ssize_t transferred = read(watched[index].fd, chunk, sizeof(chunk));
      if (transferred < 0) {
        if (errno == EINTR || errno == EAGAIN) continue;
        failed = 1; failure_errno = errno;
        break;
      }
      if (transferred == 0) {
        close(watched[index].fd); watched[index].fd = -1; live -= 1;
        continue;
      }
      if (!silk_capture_append(&silk_captures[index], chunk, (size_t)transferred)) {
        failed = 1; protocol = SILK_NO_SPACE;
        break;
      }
    }
  }
  for (int index = 0; index < 2; index += 1) {
    if (watched[index].fd >= 0) close(watched[index].fd);
  }
  int waited_status = 0;
  pid_t waited;
  do { waited = waitpid(child, &waited_status, 0); } while (waited < 0 && errno == EINTR);
  int waited_errno = errno;
  if (failed != 0) {
    silk_capture_release();
    if (protocol >= 0) silk_protocol_failure(reason, native_code, protocol);
    else silk_failure(reason, native_code, failure_errno);
    return 0;
  }
  if (waited < 0) {
    silk_capture_release();
    silk_failure(reason, native_code, waited_errno);
    return 0;
  }
  if (WIFEXITED(waited_status)) { *status = 0; *code = WEXITSTATUS(waited_status); }
  else if (WIFSIGNALED(waited_status)) { *status = 1; *code = WTERMSIG(waited_status); }
  else {
    silk_capture_release();
    silk_protocol_failure(reason, native_code, SILK_OTHER);
    return 0;
  }
  silk_success(reason, native_code);
  return 1;
}

int silk_os_process_execute_v1(const unsigned char *program, size_t program_length,
                               const unsigned char *arguments, size_t arguments_length,
                               const unsigned char *environment, size_t environment_length,
                               const unsigned char *working_directory,
                               size_t working_directory_length,
                               int *status, int *code,
                               size_t *output_length, size_t *error_length,
                               int *reason, uint32_t *native_code) {
  silk_capture_release();
  *status = 0; *code = 0; *output_length = 0; *error_length = 0;
  if (program_length == 0 || memchr(program, 0, program_length) != NULL ||
      !silk_terminated(arguments, arguments_length) ||
      !silk_terminated(environment, environment_length) ||
      (working_directory_length != 0 &&
       memchr(working_directory, 0, working_directory_length) != NULL)) {
    silk_protocol_failure(reason, native_code, SILK_INVALID_PATH);
    return 0;
  }
  char *program_string = silk_string(program, program_length);
  char *directory_string = working_directory_length == 0
    ? NULL
    : silk_string(working_directory, working_directory_length);
  /* The child's argv[0] is the program itself; the request's arguments follow it in order. */
  char **argv = program_string == NULL
    ? NULL
    : silk_entry_vector(arguments, arguments_length, program_string);
  char **envp = silk_entry_vector(environment, environment_length, NULL);
  int prepared = program_string != NULL && argv != NULL && envp != NULL &&
                 (working_directory_length == 0 || directory_string != NULL);
  int completed = prepared == 0
    ? 0
    : silk_spawn(program_string, argv, envp, directory_string, status, code, reason, native_code);
  if (prepared == 0) silk_protocol_failure(reason, native_code, SILK_NO_SPACE);
  free(program_string); free(directory_string); free(argv); free(envp);
  if (completed == 0) return 0;
  *output_length = silk_captures[0].length;
  *error_length = silk_captures[1].length;
  return 1;
}
`,
  silk_os_process_capture_v1: `
int silk_os_process_capture_v1(int stream, size_t offset, unsigned char *output,
                               size_t capacity, size_t *count,
                               int *reason, uint32_t *native_code) {
  if (stream != 0 && stream != 1) {
    silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE);
    return 0;
  }
  silk_capture *capture = &silk_captures[stream];
  if (offset > capture->length) {
    silk_protocol_failure(reason, native_code, SILK_INVALID_PATH);
    return 0;
  }
  size_t remaining = capture->length - offset;
  size_t transferred = remaining < capacity ? remaining : capacity;
  if (transferred != 0) memcpy(output, capture->bytes + offset, transferred);
  silk_success(reason, native_code);
  return silk_transfer(transferred, count);
}
`,
  silk_os_host_argument_count_v1: `
int silk_os_host_argument_count_v1(size_t *count, int *reason, uint32_t *native_code) {
  if (silk_host_argc_v1 < 0 || silk_host_argv_v1 == NULL) {
    silk_protocol_failure(reason, native_code, SILK_UNSUPPORTED);
    return 0;
  }
  *count = (size_t)silk_host_argc_v1;
  silk_success(reason, native_code);
  return 1;
}
`,
  silk_os_host_argument_v1: `
int silk_os_host_argument_v1(size_t index, unsigned char *output, size_t capacity, size_t *count,
                             int *reason, uint32_t *native_code) {
  if (silk_host_argv_v1 == NULL || silk_host_argc_v1 < 0 ||
      index >= (size_t)silk_host_argc_v1) {
    return silk_host_absent(reason, native_code);
  }
  const char *selected = silk_host_argv_v1[index];
  if (selected == NULL) return silk_host_absent(reason, native_code);
  return silk_host_copy((const unsigned char *)selected, strlen(selected), output, capacity, count,
                        reason, native_code);
}
`,
  silk_os_host_variable_v1: `
int silk_os_host_variable_v1(const unsigned char *name, size_t name_length,
                             unsigned char *output, size_t capacity, size_t *count,
                             int *reason, uint32_t *native_code) {
  char **entries = silk_host_environ;
  if (entries == NULL) return silk_host_absent(reason, native_code);
  /* The environment block is scanned by raw bytes rather than through getenv, so a name or value
     that is not valid UTF-8 is matched and returned exactly as the process received it. */
  for (size_t entry = 0; entries[entry] != NULL; entry += 1) {
    const unsigned char *text = (const unsigned char *)entries[entry];
    const unsigned char *separator = (const unsigned char *)strchr(entries[entry], '=');
    if (separator == NULL) continue;
    if ((size_t)(separator - text) != name_length) continue;
    if (name_length > 0 && memcmp(text, name, name_length) != 0) continue;
    const unsigned char *value = separator + 1;
    return silk_host_copy(value, strlen((const char *)value), output, capacity, count, reason,
                          native_code);
  }
  return silk_host_absent(reason, native_code);
}
`,
  silk_os_host_working_directory_v1: `
int silk_os_host_working_directory_v1(unsigned char *output, size_t capacity, size_t *count,
                                      int *reason, uint32_t *native_code) {
  size_t room = 256;
  while (room <= ((size_t)1 << 20)) {
    char *buffer = (char *)malloc(room);
    if (buffer == NULL) {
      silk_protocol_failure(reason, native_code, SILK_NO_SPACE);
      return 0;
    }
    if (getcwd(buffer, room) != NULL) {
      int result = silk_host_copy((const unsigned char *)buffer, strlen(buffer), output, capacity,
                                  count, reason, native_code);
      free(buffer);
      return result;
    }
    int selected = errno;
    free(buffer);
    if (selected != ERANGE) {
      silk_failure(reason, native_code, selected);
      return 0;
    }
    room *= 2;
  }
  silk_protocol_failure(reason, native_code, SILK_TOO_LARGE);
  return 0;
}
`,
})

const childProcessSymbols: ReadonlySet<Symbol> = new Set([
  'silk_os_process_execute_v1',
  'silk_os_process_capture_v1',
])
const hostInputSymbols: ReadonlySet<Symbol> = new Set([
  'silk_os_host_argument_count_v1',
  'silk_os_host_argument_v1',
  'silk_os_host_variable_v1',
  'silk_os_host_working_directory_v1',
])

const includes = (groups: {
  readonly childProcess: boolean
  readonly hostInput: boolean
}): string => {
  const legacy = groups.childProcess || groups.hostInput
  const selected: ReadonlyArray<readonly [boolean, string]> = [
    [legacy, '<errno.h>'],
    [groups.childProcess, '<fcntl.h>'],
    [groups.childProcess, '<poll.h>'],
    [legacy, '<stddef.h>'],
    [legacy, '<stdint.h>'],
    [groups.childProcess || groups.hostInput, '<stdlib.h>'],
    [groups.childProcess || groups.hostInput, '<string.h>'],
    [groups.childProcess, '<sys/types.h>'],
    [groups.childProcess, '<sys/wait.h>'],
    [groups.childProcess || groups.hostInput, '<unistd.h>'],
  ]
  return selected
    .filter(([needed]) => needed)
    .map(([, header]) => `#include ${header}`)
    .join('\n')
}

/** Generates only the selected native OS entry points and capability-scoped private helpers. */
export const source = (selected: ReadonlyArray<string>): string => {
  const retained = symbols.filter((symbol) => selected.includes(symbol))
  if (retained.length === 0) return ''
  const has = (group: ReadonlySet<Symbol>): boolean => retained.some((symbol) => group.has(symbol))
  const groups = Object.freeze({
    childProcess: has(childProcessSymbols),
    hostInput: has(hostInputSymbols),
  })
  const legacy = groups.childProcess || groups.hostInput
  return [
    includes(groups),
    legacy ? statusPrelude : '',
    groups.childProcess ? stringPrelude : '',
    groups.childProcess ? childProcessPrelude : '',
    groups.hostInput ? hostInputPrelude : '',
    ...retained.map((symbol) => implementations[symbol]),
  ]
    .filter((fragment) => fragment.length > 0)
    .join('\n')
}
