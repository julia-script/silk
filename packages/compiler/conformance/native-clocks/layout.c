#define _POSIX_C_SOURCE 200809L
#include <time.h>
#include <stdint.h>
#include <stddef.h>
_Static_assert(sizeof(struct timespec)==16, "timespec size");
_Static_assert(_Alignof(struct timespec)==8, "timespec alignment");
_Static_assert(offsetof(struct timespec,tv_sec)==0, "seconds offset");
_Static_assert(offsetof(struct timespec,tv_nsec)==8, "nanoseconds offset");
_Static_assert(sizeof(time_t)==8 && (time_t)-1<0, "signed64 time_t");
_Static_assert(sizeof(long)==8, "long64");
_Static_assert(sizeof(clockid_t)==4, "clockid32");
#ifdef __APPLE__
_Static_assert((clockid_t)-1>0, "unsigned Darwin clockid");
_Static_assert(CLOCK_MONOTONIC==6, "Darwin monotonic");
static int (*const sleep_signature)(const struct timespec *,struct timespec *)=nanosleep;
#else
_Static_assert((clockid_t)-1<0, "signed GNU clockid");
_Static_assert(CLOCK_MONOTONIC==1 && TIMER_ABSTIME==1, "GNU constants");
static int (*const sleep_signature)(clockid_t,int,const struct timespec *,struct timespec *)=clock_nanosleep;
#endif
_Static_assert(CLOCK_REALTIME==0, "realtime");
static int (*const read_signature)(clockid_t,struct timespec *)=clock_gettime;
static int (*const resolution_signature)(clockid_t,struct timespec *)=clock_getres;
