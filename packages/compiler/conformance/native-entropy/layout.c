#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/types.h>
_Static_assert(sizeof(size_t)==8 && _Alignof(size_t)==8 && (size_t)-1>0, "size_t unsigned64");
#ifdef __APPLE__
static void (*const entropy_signature)(void *,size_t)=arc4random_buf;
#else
#include <sys/random.h>
#include <errno.h>
_Static_assert(sizeof(ssize_t)==8 && _Alignof(ssize_t)==8 && (ssize_t)-1<0, "ssize_t signed64");
_Static_assert(sizeof(unsigned int)==4 && sizeof(int)==4, "C integer lanes");
_Static_assert(GRND_NONBLOCK==1 && EINTR==4 && EAGAIN==11, "GNU constants");
static ssize_t (*const entropy_signature)(void *,size_t,unsigned int)=getrandom;
static int *(*const errno_signature)(void)=__errno_location;
#endif
