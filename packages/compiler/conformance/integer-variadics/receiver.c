#include <stdarg.h>
#include <stdint.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#if defined(__APPLE__)
_Static_assert(sizeof(mode_t) == 2, "Darwin mode_t must promote to int");
#else
_Static_assert(sizeof(mode_t) == 4, "GNU mode_t retains unsigned int");
#endif

int silk_test_varargs(int shape, ...) {
    va_list args;
    va_start(args, shape);
    int result = 0;
    if (shape == 1) {
        if (va_arg(args, int) != -7) result = 1;
        if (va_arg(args, int) != 255) result = 2;
        if (va_arg(args, int) != -12345) result = 3;
        if (va_arg(args, int) != 65535) result = 4;
        if (va_arg(args, unsigned int) != 4000000000U) result = 5;
        if (va_arg(args, int64_t) != INT64_C(-4294967297)) result = 6;
        if (va_arg(args, uint64_t) != UINT64_C(4294967298)) result = 7;
        if (va_arg(args, intptr_t) != INTPTR_MIN + 1) result = 8;
        if (va_arg(args, uintptr_t) != UINTPTR_MAX) result = 9;
        for (int i = 10; i <= 13; ++i)
            if (va_arg(args, int) != i) result = i;
    } else if (shape == 2) {
        if (va_arg(args, int) != 42) result = 14;
    } else if (shape != 0) result = 15;
    va_end(args);
    return result;
}

/* Helpers provide test data and observe results; Silk calls open/openat directly. */
const unsigned char *silk_test_path(int which) {
    return (const unsigned char *)(which == 0 ? "silk-open.fixture" : "silk-openat.fixture");
}
void silk_test_init(void) { umask(0); }
int silk_test_finish(void) {
    struct stat a, b;
    int result = stat("silk-open.fixture", &a) || stat("silk-openat.fixture", &b);
    if (!result && ((a.st_mode & 0777) != 0600 || (b.st_mode & 0777) != 0640)) result = 1;
    if (unlink("silk-open.fixture")) result = 1;
    if (unlink("silk-openat.fixture")) result = 1;
    return result;
}

int silk_test_varargs_callback(int (*callback)(int), int tag, ...) {
    va_list args;
    va_start(args, tag);
    int value = va_arg(args, int);
    va_end(args);
    return tag == 0 ? callback(value) : -1;
}
