#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
struct reservation {struct reservation *next; unsigned char *payload; size_t charged;};
struct accounting {struct reservation *head; size_t used; size_t limit;};
_Static_assert(sizeof(size_t) == sizeof(void *), "target-width size");
_Static_assert(sizeof(struct reservation) == 3 * sizeof(void *), "reservation layout");
_Static_assert(sizeof(struct accounting) == 3 * sizeof(void *), "accounting layout");
_Static_assert(_Alignof(struct reservation) == _Alignof(void *), "reservation alignment");
_Static_assert(_Alignof(struct accounting) == _Alignof(void *), "accounting alignment");
_Static_assert(offsetof(struct reservation, payload) == sizeof(void *), "payload offset");
_Static_assert(offsetof(struct reservation, charged) == 2 * sizeof(void *), "charge offset");
_Static_assert(offsetof(struct accounting, used) == sizeof(void *), "used offset");
_Static_assert(offsetof(struct accounting, limit) == 2 * sizeof(void *), "limit offset");
extern unsigned char *silk_execution_storage_create(void);
extern unsigned char *silk_execution_storage_acquire(unsigned char *, size_t, size_t);
extern void silk_execution_storage_release(unsigned char *, unsigned char *);
extern void silk_execution_storage_destroy(unsigned char *);
