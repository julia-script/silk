#include <asm/unistd.h>
#include <linux/auxvec.h>
#include <linux/mman.h>
#include <asm/mman.h>
_Static_assert(AT_NULL == 0 && AT_PAGESZ == 6, "auxiliary vector constants");
_Static_assert(PROT_READ == 1 && PROT_WRITE == 2, "mapping protection");
_Static_assert(MAP_PRIVATE == 2 && MAP_ANONYMOUS == 32, "mapping flags");
_Static_assert(sizeof(void *) == 8 && sizeof(unsigned long) == 8, "kernel word");
#if defined(__x86_64__)
_Static_assert(__NR_read == 0 && __NR_write == 1 && __NR_mmap == 9 &&
               __NR_munmap == 11 && __NR_exit_group == 231 && __NR_getpid == 39, "x86-64 syscalls");
#elif defined(__aarch64__)
_Static_assert(__NR_read == 63 && __NR_write == 64 && __NR_mmap == 222 &&
               __NR_munmap == 215 && __NR_exit_group == 94 && __NR_getpid == 172, "ARM64 syscalls");
#else
#error unsupported architecture
#endif
