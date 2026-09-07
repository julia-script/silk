#define _GNU_SOURCE 1
#define _DARWIN_C_SOURCE 1
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#define ABI(name, type) _Static_assert(__builtin_types_compatible_p(__typeof__(&name), type), #name " signature")
ABI(open,int (*)(const char *,int,...));
ABI(openat,int (*)(int,const char *,int,...));
ABI(close,int (*)(int));
ABI(read,ssize_t (*)(int,void *,size_t));
ABI(write,ssize_t (*)(int,const void *,size_t));
ABI(fstat,int (*)(int,struct stat *));
ABI(fstatat,int (*)(int,const char *,struct stat *,int));
ABI(ftruncate,int (*)(int,off_t));
ABI(mkdirat,int (*)(int,const char *,mode_t));
ABI(unlinkat,int (*)(int,const char *,int));
ABI(fdopendir,DIR *(*)(int));
ABI(dirfd,int (*)(DIR *));
ABI(readdir,struct dirent *(*)(DIR *));
ABI(closedir,int (*)(DIR *));
_Static_assert(sizeof(off_t)==8 && (off_t)-1<0 && sizeof(size_t)==8 && sizeof(ssize_t)==8, "count and offset widths");
_Static_assert(_Alignof(struct stat)==8 && _Alignof(struct dirent)==8, "record alignment");
_Static_assert(offsetof(struct dirent,d_reclen)==16, "record length offset");
_Static_assert(S_IFMT==61440 && S_IFREG==32768 && S_IFDIR==16384 && S_IFLNK==40960, "kinds");
_Static_assert(ENOENT==2 && EEXIST==17 && EACCES==13 && EPERM==1 && EINVAL==22 && ENOTDIR==20 && EISDIR==21 && ENOSPC==28 && ENOMEM==12 && EFBIG==27 && EINTR==4 && EBADF==9, "shared errors");
#ifdef __APPLE__
ABI(__error,int *(*)(void));
_Static_assert(sizeof(mode_t)==2 && (mode_t)-1>0, "Darwin mode");
_Static_assert(sizeof(struct stat)==144 && offsetof(struct stat,st_mode)==4 && offsetof(struct stat,st_size)==96, "Darwin stat");
_Static_assert(sizeof(struct dirent)==1048 && offsetof(struct dirent,d_name)==21 && offsetof(struct dirent,d_namlen)==18, "Darwin dirent");
_Static_assert(O_CREAT==512 && O_EXCL==2048 && O_NONBLOCK==4 && O_DIRECTORY==1048576 && O_NOFOLLOW==256 && O_CLOEXEC==16777216, "Darwin open flags");
_Static_assert(AT_SYMLINK_NOFOLLOW==32 && AT_REMOVEDIR==128 && ENOTEMPTY==66 && EOVERFLOW==84 && ENAMETOOLONG==63 && ELOOP==62, "Darwin metadata flags/errors");
#else
ABI(__errno_location,int *(*)(void));
_Static_assert(sizeof(mode_t)==4 && (mode_t)-1>0, "GNU mode");
_Static_assert(sizeof(struct dirent)==280 && offsetof(struct dirent,d_name)==19, "GNU dirent");
_Static_assert(O_CREAT==64 && O_EXCL==128 && O_NONBLOCK==2048 && O_CLOEXEC==524288, "GNU open flags");
_Static_assert(AT_SYMLINK_NOFOLLOW==256 && AT_REMOVEDIR==512 && ENOTEMPTY==39 && EOVERFLOW==75 && ENAMETOOLONG==36 && ELOOP==40, "GNU metadata flags/errors");
#ifdef __x86_64__
_Static_assert(sizeof(struct stat)==144 && offsetof(struct stat,st_mode)==24 && offsetof(struct stat,st_size)==48, "GNU x86 stat");
_Static_assert(O_DIRECTORY==65536 && O_NOFOLLOW==131072, "GNU x86 flags");
#else
_Static_assert(sizeof(struct stat)==128 && offsetof(struct stat,st_mode)==16 && offsetof(struct stat,st_size)==48, "GNU ARM stat");
_Static_assert(O_DIRECTORY==16384 && O_NOFOLLOW==32768, "GNU ARM flags");
#endif
#endif

#define OFFSET(record, field, value) _Static_assert(offsetof(struct record,field)==value, #record "." #field)
#ifdef __APPLE__
OFFSET(stat, st_dev, 0);
OFFSET(stat, st_mode, 4);
OFFSET(stat, st_nlink, 6);
OFFSET(stat, st_ino, 8);
OFFSET(stat, st_uid, 16);
OFFSET(stat, st_gid, 20);
OFFSET(stat, st_rdev, 24);
OFFSET(stat, st_atimespec, 32);
OFFSET(stat, st_mtimespec, 48);
OFFSET(stat, st_ctimespec, 64);
OFFSET(stat, st_birthtimespec, 80);
OFFSET(stat, st_size, 96);
OFFSET(stat, st_blocks, 104);
OFFSET(stat, st_blksize, 112);
OFFSET(stat, st_flags, 116);
OFFSET(stat, st_gen, 120);
OFFSET(stat, st_lspare, 124);
OFFSET(stat, st_qspare, 128);
OFFSET(dirent, d_ino, 0);
OFFSET(dirent, d_seekoff, 8);
OFFSET(dirent, d_reclen, 16);
OFFSET(dirent, d_namlen, 18);
OFFSET(dirent, d_type, 20);
OFFSET(dirent, d_name, 21);
#elif defined(__x86_64__)
OFFSET(stat, st_dev, 0);
OFFSET(stat, st_ino, 8);
OFFSET(stat, st_nlink, 16);
OFFSET(stat, st_mode, 24);
OFFSET(stat, st_uid, 28);
OFFSET(stat, st_gid, 32);
OFFSET(stat, __pad0, 36);
OFFSET(stat, st_rdev, 40);
OFFSET(stat, st_size, 48);
OFFSET(stat, st_blksize, 56);
OFFSET(stat, st_blocks, 64);
OFFSET(stat, st_atim, 72);
OFFSET(stat, st_mtim, 88);
OFFSET(stat, st_ctim, 104);
OFFSET(stat, __glibc_reserved, 120);
OFFSET(dirent, d_ino, 0);
OFFSET(dirent, d_off, 8);
OFFSET(dirent, d_reclen, 16);
OFFSET(dirent, d_type, 18);
OFFSET(dirent, d_name, 19);
#else
OFFSET(stat, st_dev, 0);
OFFSET(stat, st_ino, 8);
OFFSET(stat, st_mode, 16);
OFFSET(stat, st_nlink, 20);
OFFSET(stat, st_uid, 24);
OFFSET(stat, st_gid, 28);
OFFSET(stat, st_rdev, 32);
OFFSET(stat, __pad1, 40);
OFFSET(stat, st_size, 48);
OFFSET(stat, st_blksize, 56);
OFFSET(stat, __pad2, 60);
OFFSET(stat, st_blocks, 64);
OFFSET(stat, st_atim, 72);
OFFSET(stat, st_mtim, 88);
OFFSET(stat, st_ctim, 104);
OFFSET(stat, __glibc_reserved, 120);
OFFSET(dirent, d_ino, 0);
OFFSET(dirent, d_off, 8);
OFFSET(dirent, d_reclen, 16);
OFFSET(dirent, d_type, 18);
OFFSET(dirent, d_name, 19);
#endif
