/** Compiler-owned native runtime symbols for the sealed OS filesystem and byte-input protocols. */
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
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
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
