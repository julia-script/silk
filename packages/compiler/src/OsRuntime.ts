/**
 * Compiler-owned native runtime symbols for the sealed OS filesystem, byte-input, and
 * child-process protocols.
 */
export const symbols = Object.freeze([
  'silk_os_file_open_v1',
  'silk_os_file_read_v1',
  'silk_os_file_write_v1',
  'silk_os_directory_open_v1',
  'silk_os_directory_next_v1',
  'silk_os_path_inspect_v1',
  'silk_os_directory_create_v1',
  'silk_os_file_remove_v1',
  'silk_os_directory_remove_v1',
  'silk_os_handle_close_v1',
  'silk_os_standard_input_read_v1',
  'silk_os_process_execute_v1',
  'silk_os_process_capture_v1',
  'silk_os_host_argument_count_v1',
  'silk_os_host_argument_v1',
  'silk_os_host_variable_v1',
  'silk_os_host_working_directory_v1',
] as const)

export type Symbol = (typeof symbols)[number]

export const isSymbol = (value: string): value is Symbol =>
  symbols.some((candidate) => candidate === value)

const common = `#if defined(__APPLE__)
#define _DARWIN_C_SOURCE
#elif defined(__linux__)
#define _GNU_SOURCE
#endif
#define _POSIX_C_SOURCE 200809L
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef O_NOFOLLOW
#error "Silk OS filesystem requires O_NOFOLLOW"
#endif

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

typedef struct { size_t identity; int kind; int active; } silk_os_handle;
typedef struct { int tag; size_t value; } silk_option_usize;

typedef struct {
  int kind;
  int fd;
  DIR *directory;
  unsigned char *pending_name;
  size_t pending_length;
  int pending_kind;
} silk_native_handle;

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

static int silk_utf8(const unsigned char *bytes, size_t length) {
  size_t index = 0;
  while (index < length) {
    unsigned char first = bytes[index++];
    if (first == 0) return 0;
    if (first < 0x80) continue;
    size_t remaining = 0;
    uint32_t code = 0;
    if (first >= 0xc2 && first <= 0xdf) { remaining = 1; code = first & 0x1f; }
    else if (first >= 0xe0 && first <= 0xef) { remaining = 2; code = first & 0x0f; }
    else if (first >= 0xf0 && first <= 0xf4) { remaining = 3; code = first & 0x07; }
    else return 0;
    if (length - index < remaining) return 0;
    for (size_t offset = 0; offset < remaining; offset += 1) {
      unsigned char next = bytes[index++];
      if ((next & 0xc0) != 0x80) return 0;
      code = (code << 6) | (uint32_t)(next & 0x3f);
    }
    if ((remaining == 1 && code < 0x80) ||
        (remaining == 2 && code < 0x800) ||
        (remaining == 3 && code < 0x10000) ||
        (code >= 0xd800 && code <= 0xdfff) || code > 0x10ffff) return 0;
  }
  return 1;
}

static int silk_component_valid(const unsigned char *bytes, size_t length) {
  if (length == 0) return 0;
  if (length == 1 && bytes[0] == '.') return 0;
  if (length == 2 && bytes[0] == '.' && bytes[1] == '.') return 0;
  return 1;
}

static char *silk_string(const unsigned char *bytes, size_t length) {
  if (length == SIZE_MAX) return NULL;
  char *value = (char *)malloc(length + 1);
  if (value == NULL) return NULL;
  memcpy(value, bytes, length);
  value[length] = 0;
  return value;
}

/* Resolve a normalized provider-absolute path to an opened parent plus an owned final name. */
static int silk_parent(const unsigned char *root, size_t root_length,
                       const unsigned char *path, size_t path_length,
                       int *parent, char **leaf, int *reason, uint32_t *native_code) {
  *parent = -1;
  *leaf = NULL;
  if (root_length == 0 || root[0] != '/' || path_length == 0 || path[0] != '/' ||
      !silk_utf8(root, root_length) || !silk_utf8(path, path_length)) {
    silk_protocol_failure(reason, native_code, SILK_INVALID_PATH);
    return 0;
  }
  char *root_string = silk_string(root, root_length);
  if (root_string == NULL) {
    silk_protocol_failure(reason, native_code, SILK_NO_SPACE);
    return 0;
  }
  int current = open(root_string, O_RDONLY | O_DIRECTORY | O_NOFOLLOW);
  int opened_errno = errno;
  free(root_string);
  if (current < 0) {
    silk_failure(reason, native_code, opened_errno);
    return 0;
  }
  if (path_length == 1) {
    *parent = current;
    return 1;
  }
  size_t start = 1;
  while (start < path_length) {
    size_t end = start;
    while (end < path_length && path[end] != '/') end += 1;
    if (!silk_component_valid(path + start, end - start) || end == start ||
        (end < path_length && end + 1 == path_length)) {
      close(current);
      silk_protocol_failure(reason, native_code, SILK_INVALID_PATH);
      return 0;
    }
    char *component = silk_string(path + start, end - start);
    if (component == NULL) {
      close(current);
      silk_protocol_failure(reason, native_code, SILK_NO_SPACE);
      return 0;
    }
    if (end == path_length) {
      *parent = current;
      *leaf = component;
      return 1;
    }
    int next = openat(current, component, O_RDONLY | O_DIRECTORY | O_NOFOLLOW);
    int next_errno = errno;
    free(component);
    close(current);
    if (next < 0) {
      silk_failure(reason, native_code, next_errno);
      return 0;
    }
    current = next;
    start = end + 1;
  }
  close(current);
  silk_protocol_failure(reason, native_code, SILK_INVALID_PATH);
  return 0;
}

static silk_native_handle *silk_live(silk_os_handle *handle, int kind,
                                     int *reason, uint32_t *native_code) {
  if (handle == NULL || handle->active != 1 || handle->kind != kind || handle->identity == 0) {
    silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE);
    return NULL;
  }
  silk_native_handle *native = (silk_native_handle *)(uintptr_t)handle->identity;
  if (native->kind != kind) {
    silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE);
    return NULL;
  }
  return native;
}

static silk_option_usize silk_transfer(size_t count) {
  silk_option_usize result = { 1, count };
  return result;
}

static silk_option_usize silk_transfer_failure(void) {
  silk_option_usize result = { 0, 0 };
  return result;
}

static silk_native_handle *silk_allocate_handle(int kind, int fd, DIR *directory) {
  silk_native_handle *native = (silk_native_handle *)calloc(1, sizeof(silk_native_handle));
  if (native == NULL) return NULL;
  native->kind = kind;
  native->fd = fd;
  native->directory = directory;
  return native;
}

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
static silk_option_usize silk_host_copy(const unsigned char *value, size_t length,
                                        unsigned char *output, size_t capacity,
                                        int *reason, uint32_t *native_code) {
  size_t committed = length < capacity ? length : capacity;
  if (committed > 0) memcpy(output, value, committed);
  silk_success(reason, native_code);
  return silk_transfer(length);
}

/* An absent value: an index past the last argument, or an unset variable name. */
static silk_option_usize silk_host_absent(int *reason, uint32_t *native_code) {
  silk_protocol_failure(reason, native_code, SILK_NOT_FOUND);
  return silk_transfer_failure();
}
`

const implementations: Readonly<Record<Symbol, string>> = Object.freeze({
  silk_os_file_open_v1: `
int silk_os_file_open_v1(const unsigned char *root, size_t root_length,
                         const unsigned char *path, size_t path_length, int mode,
                         int *reason, uint32_t *native_code,
                         size_t *identity, int *kind, int *active) {
  int parent;
  char *leaf;
  if (!silk_parent(root, root_length, path, path_length, &parent, &leaf, reason, native_code))
    return 0;
  if (leaf == NULL) {
    close(parent);
    silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE);
    return 0;
  }
  int flags = mode == 0 ? O_RDONLY : mode == 1 ? O_WRONLY | O_CREAT | O_TRUNC : -1;
  if (flags < 0) {
    free(leaf); close(parent);
    silk_protocol_failure(reason, native_code, SILK_INVALID_PATH);
    return 0;
  }
  int fd = openat(parent, leaf, flags | O_NOFOLLOW, 0666);
  int opened_errno = errno;
  free(leaf); close(parent);
  if (fd < 0) { silk_failure(reason, native_code, opened_errno); return 0; }
  struct stat info;
  if (fstat(fd, &info) != 0 || !S_ISREG(info.st_mode)) {
    int selected = errno;
    close(fd);
    if (selected != 0) silk_failure(reason, native_code, selected);
    else silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE);
    return 0;
  }
  silk_native_handle *native = silk_allocate_handle(0, fd, NULL);
  if (native == NULL) { close(fd); silk_protocol_failure(reason, native_code, SILK_NO_SPACE); return 0; }
  silk_success(reason, native_code);
  *identity = (size_t)(uintptr_t)native; *kind = 0; *active = 1;
  return 1;
}
`,
  silk_os_file_read_v1: `
silk_option_usize silk_os_file_read_v1(silk_os_handle *handle, unsigned char *output,
                                       size_t capacity, int *reason, uint32_t *native_code) {
  silk_native_handle *native = silk_live(handle, 0, reason, native_code);
  if (native == NULL) return silk_transfer_failure();
  ssize_t received;
  do { received = read(native->fd, output, capacity); } while (received < 0 && errno == EINTR);
  if (received < 0) { silk_failure(reason, native_code, errno); return silk_transfer_failure(); }
  silk_success(reason, native_code);
  return silk_transfer((size_t)received);
}
`,
  silk_os_file_write_v1: `
silk_option_usize silk_os_file_write_v1(silk_os_handle *handle, const unsigned char *input,
                                        size_t length, size_t offset,
                                        int *reason, uint32_t *native_code) {
  silk_native_handle *native = silk_live(handle, 0, reason, native_code);
  if (native == NULL) return silk_transfer_failure();
  if (offset > length) { silk_protocol_failure(reason, native_code, SILK_INVALID_PATH); return silk_transfer_failure(); }
  ssize_t written;
  do { written = write(native->fd, input + offset, length - offset); } while (written < 0 && errno == EINTR);
  if (written < 0) { silk_failure(reason, native_code, errno); return silk_transfer_failure(); }
  silk_success(reason, native_code);
  return silk_transfer((size_t)written);
}
`,
  silk_os_directory_open_v1: `
int silk_os_directory_open_v1(const unsigned char *root, size_t root_length,
                              const unsigned char *path, size_t path_length,
                              int *reason, uint32_t *native_code,
                              size_t *identity, int *kind, int *active) {
  int parent;
  char *leaf;
  if (!silk_parent(root, root_length, path, path_length, &parent, &leaf, reason, native_code))
    return 0;
  int fd = leaf == NULL ? parent : openat(parent, leaf, O_RDONLY | O_DIRECTORY | O_NOFOLLOW);
  int opened_errno = errno;
  if (leaf != NULL) { free(leaf); close(parent); }
  if (fd < 0) { silk_failure(reason, native_code, opened_errno); return 0; }
  DIR *directory = fdopendir(fd);
  if (directory == NULL) { int selected = errno; close(fd); silk_failure(reason, native_code, selected); return 0; }
  silk_native_handle *native = silk_allocate_handle(1, fd, directory);
  if (native == NULL) { closedir(directory); silk_protocol_failure(reason, native_code, SILK_NO_SPACE); return 0; }
  silk_success(reason, native_code);
  *identity = (size_t)(uintptr_t)native; *kind = 1; *active = 1;
  return 1;
}
`,
  silk_os_directory_next_v1: `
silk_option_usize silk_os_directory_next_v1(silk_os_handle *handle, unsigned char *output,
                                            size_t capacity, int *kind, size_t *required,
                                            int *reason, uint32_t *native_code) {
  silk_native_handle *native = silk_live(handle, 1, reason, native_code);
  if (native == NULL) return silk_transfer_failure();
  while (native->pending_name == NULL) {
    errno = 0;
    struct dirent *entry = readdir(native->directory);
    if (entry == NULL) {
      if (errno != 0) { silk_failure(reason, native_code, errno); return silk_transfer_failure(); }
      silk_success(reason, native_code);
      return silk_transfer(0);
    }
    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) continue;
    struct stat info;
    if (fstatat(native->fd, entry->d_name, &info, AT_SYMLINK_NOFOLLOW) != 0) {
      silk_failure(reason, native_code, errno);
      return silk_transfer_failure();
    }
    if (S_ISLNK(info.st_mode) || (!S_ISREG(info.st_mode) && !S_ISDIR(info.st_mode))) {
      silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE);
      return silk_transfer_failure();
    }
    native->pending_length = strlen(entry->d_name);
    native->pending_name = (unsigned char *)malloc(native->pending_length);
    if (native->pending_name == NULL) {
      silk_protocol_failure(reason, native_code, SILK_NO_SPACE);
      return silk_transfer_failure();
    }
    memcpy(native->pending_name, entry->d_name, native->pending_length);
    native->pending_kind = S_ISREG(info.st_mode) ? 0 : 1;
  }
  if (capacity < native->pending_length) {
    *required = native->pending_length;
    silk_protocol_failure(reason, native_code, SILK_BUFFER_TOO_SMALL);
    return silk_transfer_failure();
  }
  memcpy(output, native->pending_name, native->pending_length);
  *kind = native->pending_kind;
  size_t length = native->pending_length;
  free(native->pending_name);
  native->pending_name = NULL;
  native->pending_length = 0;
  silk_success(reason, native_code);
  return silk_transfer(length);
}
`,
  silk_os_path_inspect_v1: `
int silk_os_path_inspect_v1(const unsigned char *root, size_t root_length,
                            const unsigned char *path, size_t path_length,
                            int *kind, size_t *byte_length,
                            int *reason, uint32_t *native_code) {
  int parent;
  char *leaf;
  if (!silk_parent(root, root_length, path, path_length, &parent, &leaf, reason, native_code)) return 0;
  struct stat info;
  int status = leaf == NULL ? fstat(parent, &info) : fstatat(parent, leaf, &info, AT_SYMLINK_NOFOLLOW);
  int selected = errno;
  free(leaf); close(parent);
  if (status != 0) { silk_failure(reason, native_code, selected); return 0; }
  if (S_ISLNK(info.st_mode) || (!S_ISREG(info.st_mode) && !S_ISDIR(info.st_mode))) {
    silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE); return 0;
  }
  *kind = S_ISREG(info.st_mode) ? 0 : 1;
  *byte_length = S_ISREG(info.st_mode) ? (size_t)info.st_size : 0;
  silk_success(reason, native_code);
  return 1;
}
`,
  silk_os_directory_create_v1: `
int silk_os_directory_create_v1(const unsigned char *root, size_t root_length,
                                const unsigned char *path, size_t path_length,
                                int *reason, uint32_t *native_code) {
  int parent; char *leaf;
  if (!silk_parent(root, root_length, path, path_length, &parent, &leaf, reason, native_code)) return 0;
  if (leaf == NULL) { close(parent); silk_protocol_failure(reason, native_code, SILK_ALREADY_EXISTS); return 0; }
  int status = mkdirat(parent, leaf, 0777); int selected = errno;
  free(leaf); close(parent);
  if (status != 0) { silk_failure(reason, native_code, selected); return 0; }
  silk_success(reason, native_code); return 1;
}
`,
  silk_os_file_remove_v1: `
int silk_os_file_remove_v1(const unsigned char *root, size_t root_length,
                           const unsigned char *path, size_t path_length,
                           int *reason, uint32_t *native_code) {
  int parent; char *leaf;
  if (!silk_parent(root, root_length, path, path_length, &parent, &leaf, reason, native_code)) return 0;
  if (leaf == NULL) { close(parent); silk_protocol_failure(reason, native_code, SILK_INVALID_PATH); return 0; }
  struct stat info;
  if (fstatat(parent, leaf, &info, AT_SYMLINK_NOFOLLOW) != 0) { int selected = errno; free(leaf); close(parent); silk_failure(reason, native_code, selected); return 0; }
  if (!S_ISREG(info.st_mode)) { free(leaf); close(parent); silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE); return 0; }
  int status = unlinkat(parent, leaf, 0); int selected = errno;
  free(leaf); close(parent);
  if (status != 0) { silk_failure(reason, native_code, selected); return 0; }
  silk_success(reason, native_code); return 1;
}
`,
  silk_os_directory_remove_v1: `
int silk_os_directory_remove_v1(const unsigned char *root, size_t root_length,
                                const unsigned char *path, size_t path_length,
                                int *reason, uint32_t *native_code) {
  int parent; char *leaf;
  if (!silk_parent(root, root_length, path, path_length, &parent, &leaf, reason, native_code)) return 0;
  if (leaf == NULL) { close(parent); silk_protocol_failure(reason, native_code, SILK_INVALID_PATH); return 0; }
  struct stat info;
  if (fstatat(parent, leaf, &info, AT_SYMLINK_NOFOLLOW) != 0) { int selected = errno; free(leaf); close(parent); silk_failure(reason, native_code, selected); return 0; }
  if (!S_ISDIR(info.st_mode)) { free(leaf); close(parent); silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE); return 0; }
  int status = unlinkat(parent, leaf, AT_REMOVEDIR); int selected = errno;
  free(leaf); close(parent);
  if (status != 0) { silk_failure(reason, native_code, selected); return 0; }
  silk_success(reason, native_code); return 1;
}
`,
  silk_os_handle_close_v1: `
int silk_os_handle_close_v1(size_t identity, int kind, int active,
                            int *reason, uint32_t *native_code) {
  if (identity == 0 || active != 1 || (kind != 0 && kind != 1)) {
    silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE); return 0;
  }
  silk_native_handle *native = (silk_native_handle *)(uintptr_t)identity;
  int status;
  if (native->kind != kind) {
    free(native->pending_name); free(native);
    silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE); return 0;
  }
  if (kind == 1) status = closedir(native->directory);
  else status = close(native->fd);
  int selected = errno;
  free(native->pending_name);
  free(native);
  if (status != 0) { silk_failure(reason, native_code, selected); return 0; }
  silk_success(reason, native_code); return 1;
}
`,
  silk_os_standard_input_read_v1: `
silk_option_usize silk_os_standard_input_read_v1(unsigned char *output, size_t capacity,
                                                 int *reason, uint32_t *native_code) {
  ssize_t received;
  do { received = read(0, output, capacity); } while (received < 0 && errno == EINTR);
  if (received < 0) { silk_failure(reason, native_code, errno); return silk_transfer_failure(); }
  silk_success(reason, native_code);
  return silk_transfer((size_t)received);
}
`,
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
  if (pipe(notice) != 0 || fcntl(notice[1], F_SETFD, FD_CLOEXEC) != 0) {
    int selected = errno;
    close(output[0]); close(output[1]); close(errors[0]); close(errors[1]);
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
silk_option_usize silk_os_process_capture_v1(int stream, size_t offset, unsigned char *output,
                                             size_t capacity, int *reason, uint32_t *native_code) {
  if (stream != 0 && stream != 1) {
    silk_protocol_failure(reason, native_code, SILK_WRONG_TYPE);
    return silk_transfer_failure();
  }
  silk_capture *capture = &silk_captures[stream];
  if (offset > capture->length) {
    silk_protocol_failure(reason, native_code, SILK_INVALID_PATH);
    return silk_transfer_failure();
  }
  size_t remaining = capture->length - offset;
  size_t transferred = remaining < capacity ? remaining : capacity;
  if (transferred != 0) memcpy(output, capture->bytes + offset, transferred);
  silk_success(reason, native_code);
  return silk_transfer(transferred);
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
silk_option_usize silk_os_host_argument_v1(size_t index, unsigned char *output, size_t capacity,
                                           int *reason, uint32_t *native_code) {
  if (silk_host_argv_v1 == NULL || silk_host_argc_v1 < 0 ||
      index >= (size_t)silk_host_argc_v1) {
    return silk_host_absent(reason, native_code);
  }
  const char *selected = silk_host_argv_v1[index];
  if (selected == NULL) return silk_host_absent(reason, native_code);
  return silk_host_copy((const unsigned char *)selected, strlen(selected), output, capacity,
                        reason, native_code);
}
`,
  silk_os_host_variable_v1: `
silk_option_usize silk_os_host_variable_v1(const unsigned char *name, size_t name_length,
                                           unsigned char *output, size_t capacity,
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
    return silk_host_copy(value, strlen((const char *)value), output, capacity, reason,
                          native_code);
  }
  return silk_host_absent(reason, native_code);
}
`,
  silk_os_host_working_directory_v1: `
silk_option_usize silk_os_host_working_directory_v1(unsigned char *output, size_t capacity,
                                                    int *reason, uint32_t *native_code) {
  size_t room = 256;
  while (room <= ((size_t)1 << 20)) {
    char *buffer = (char *)malloc(room);
    if (buffer == NULL) {
      silk_protocol_failure(reason, native_code, SILK_NO_SPACE);
      return silk_transfer_failure();
    }
    if (getcwd(buffer, room) != NULL) {
      silk_option_usize result = silk_host_copy((const unsigned char *)buffer, strlen(buffer),
                                                output, capacity, reason, native_code);
      free(buffer);
      return result;
    }
    int selected = errno;
    free(buffer);
    if (selected != ERANGE) {
      silk_failure(reason, native_code, selected);
      return silk_transfer_failure();
    }
    room *= 2;
  }
  silk_protocol_failure(reason, native_code, SILK_TOO_LARGE);
  return silk_transfer_failure();
}
`,
})

/** Generates only the selected native filesystem entry points plus their private shared helpers. */
export const source = (selected: ReadonlyArray<string>): string => {
  const retained = symbols.filter((symbol) => selected.includes(symbol))
  if (retained.length === 0) return ''
  return `${common}\n${retained.map((symbol) => implementations[symbol]).join('\n')}`
}
