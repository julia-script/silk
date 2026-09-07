#include <stdio.h>
extern int transfer_fixture(int,int), list_fixture(void), inspect_fixture(int), command_fixture(int), unique_fixture(void), cancellation_fixture(void);
static int last_result;
static int scenario, invalid, native_error, need_capture, error_reads;
static int opens, children, stats, transfers, truncates, closes, stream_closes, iterations, creations, removals, next_fd;
static int active[512], directory_fd[512], stream_fd;
static size_t committed;
static unsigned char stream_storage[8];
static struct dirent *entry;
static void before(void) { if (need_capture) invalid=1; }
static int failure(int error) { native_error=error; need_capture=1; return -1; }
#ifdef __APPLE__
int *__error(void) { ++error_reads; need_capture=0; return &native_error; }
#else
int *__errno_location(void) { ++error_reads; need_capture=0; return &native_error; }
#endif
static int acquire(int directory) { int fd=++next_fd; if(fd>=512) abort(); active[fd]=1; directory_fd[fd]=directory; return fd; }
int open(const char *path,int flags,...) {
  before(); ++opens;
  if(strcmp(path,"/sandbox") || flags!=(O_DIRECTORY|O_NOFOLLOW|O_CLOEXEC)) invalid=1;
  if(scenario==19) return failure(EACCES);
  return acquire(1);
}
int openat(int parent,const char *path,int flags,...) {
  before(); ++children;
  if(parent<1 || !active[parent] || !directory_fd[parent] || !(flags&O_NOFOLLOW) || !(flags&O_CLOEXEC) || (flags&O_TRUNC)) invalid=1;
  if(flags&O_CREAT) {
    va_list ap; va_start(ap,flags);
#ifdef __APPLE__
    int mode=va_arg(ap,int);
#else
    unsigned int mode=va_arg(ap,unsigned int);
#endif
    va_end(ap); if(mode!=0666) invalid=1;
  }
  if(!(flags&O_DIRECTORY) && !(flags&O_NONBLOCK)) invalid=1;
  if(scenario==14) return failure(EACCES);
  if(scenario==33) return failure(ELOOP);
  if(strcmp(path,"file") && strcmp(path,"dir") && strcmp(path,"nested")) invalid=1;
  return acquire(!!(flags&O_DIRECTORY));
}
int close(int fd) {
  before(); ++closes;
  if(fd<1 || fd>=512 || !active[fd]) { invalid=1; return -1; }
  active[fd]=0;
  if((scenario==15 && directory_fd[fd]) || ((scenario==3 || scenario==16) && !directory_fd[fd])) { native_error=EBADF; return -1; }
  if(scenario==17 && !directory_fd[fd]) { native_error=EINTR; return -1; }
  native_error=EBADF;
  return 0;
}
int fstat(int fd,struct stat *out) {
  before(); ++stats;
  if(!active[fd]) invalid=1;
  if(scenario==13) return failure(EACCES);
  memset(out,0,sizeof(*out));
  out->st_mode=(scenario==12 ? S_IFDIR : S_IFREG)|0644;
  out->st_size=123; native_error=EIO;
  return 0;
}
int fstatat(int fd,const char *path,struct stat *out,int flags) {
  before(); ++stats;
  if(!active[fd] || flags!=AT_SYMLINK_NOFOLLOW) invalid=1;
  if(scenario==35) return failure(EACCES);
  if(scenario==20 && memcmp(path,"\xff" "abc",5)) invalid=1;
  if(scenario==32 && ((unsigned char)path[0]!=255 || path[1])) invalid=1;
  memset(out,0,sizeof(*out));
  out->st_mode=(scenario==26 || scenario==34 || scenario==43 ? S_IFLNK : scenario==42 ? S_IFDIR : S_IFREG)|0644;
  out->st_size=123; native_error=EIO;
  return 0;
}
int ftruncate(int fd,off_t length) { before(); ++truncates; if(!active[fd] || length) invalid=1; if(scenario==18) return failure(EIO); return 0; }
ssize_t write(int fd,const void *data,size_t count) {
  before(); ++transfers;
  if(!active[fd] || count!=6-committed || memcmp(data,"abcdef"+committed,count)) invalid=1;
  if(scenario==1 && transfers==2) return failure(EINTR);
  if(scenario==2) return 0;
  if(scenario==3 && transfers==2) return failure(EIO);
  if(scenario==4) return (ssize_t)count+1;
  size_t accepted=((scenario==1 || scenario==3) && transfers==1) ? 2 : count;
  committed+=accepted; return (ssize_t)accepted;
}
ssize_t read(int fd,void *data,size_t count) {
  before(); ++transfers;
  if(!active[fd] || count!=4) invalid=1;
  if(scenario==7 && transfers==1) return failure(EINTR);
  if(scenario==8) return 0;
  if(scenario==9) return failure(EIO);
  if(scenario==10) return (ssize_t)count+1;
  ((unsigned char *)data)[0]=1; ((unsigned char *)data)[1]=2; return 2;
}
DIR *fdopendir(int fd) { before(); if(!active[fd] || !directory_fd[fd]) invalid=1; if(scenario==21) { failure(EACCES); return NULL; } stream_fd=fd; return (DIR *)stream_storage; }
int dirfd(DIR *stream) { before(); if(stream!=(DIR *)stream_storage) invalid=1; if(scenario==25) return failure(EBADF); return stream_fd; }
struct dirent *readdir(DIR *stream) {
  before(); ++iterations;
  if(stream!=(DIR *)stream_storage || native_error!=0) invalid=1;
  if(entry) { memset(entry,0x55,offsetof(struct dirent,d_name)+5); free(entry); entry=NULL; }
  if(scenario==23) { failure(EIO); return NULL; }
  if(iterations==3) return NULL;
  size_t size=offsetof(struct dirent,d_name)+5;
  entry=malloc(size); if(!entry) abort(); memset(entry,0,size);
  entry->d_reclen=(unsigned short)size;
  const char *name=iterations==1 ? "." : "\xff" "abc";
  size_t length=strlen(name);
#ifdef __APPLE__
  entry->d_namlen=(unsigned short)length;
#endif
  memcpy((unsigned char *)entry+offsetof(struct dirent,d_name),name,length+1);
  if(scenario==24) memset((unsigned char *)entry+offsetof(struct dirent,d_name),'x',5);
  return entry;
}
int closedir(DIR *stream) {
  before(); ++stream_closes;
  if(stream!=(DIR *)stream_storage || !active[stream_fd]) invalid=1;
  active[stream_fd]=0;
  if(entry) { free(entry); entry=NULL; }
  if(scenario==22) { native_error=EBADF; return -1; }
  return 0;
}
int mkdirat(int fd,const char *path,mode_t mode) {
  before(); ++creations;
  if(!active[fd] || mode!=0700) invalid=1;
  if(scenario==44) return failure(ENOSPC);
  if(scenario>=50) {
    if(strlen(path)!=20 || strncmp(path,"tmp-",4)) invalid=1;
    unsigned long long suffix=strtoull(path+4,NULL,16);
    if(suffix!=(unsigned long long)(creations-1)) invalid=1;
    if(scenario==52) return failure(EACCES);
    if(scenario==51 || creations<3) return failure(EEXIST);
  } else if(strcmp(path,"file")) invalid=1;
  return 0;
}
int unlinkat(int fd,const char *path,int flags) { before(); ++removals; if(!active[fd] || strcmp(path,"file") || flags!=(scenario==42 ? AT_REMOVEDIR : 0)) invalid=1; return 0; }
static void reset(int value) {
  scenario=value; invalid=need_capture=error_reads=0; native_error=EIO;
  opens=children=stats=transfers=truncates=closes=stream_closes=iterations=creations=removals=0;
  next_fd=0; committed=0; stream_fd=0; memset(active,0,sizeof(active));
}
static int clean(void) { for(int i=1;i<=next_fd;i++) if(active[i]) return 0; return !invalid && !need_capture; }
#define CHECK(c) do { if(!(c)) { fprintf(stderr,"scenario=%d result=%d invalid=%d capture=%d errors=%d closes=%d stream_closes=%d transfers=%d committed=%zu\n",scenario,last_result,invalid,need_capture,error_reads,closes,stream_closes,transfers,committed); return 10+scenario; } } while(0)
int main(void) {
  reset(0); CHECK((last_result=transfer_fixture(1,0))==42 && transfers==1 && truncates==1 && closes==2 && clean());
  reset(1); CHECK((last_result=transfer_fixture(1,0))==42 && transfers==3 && committed==6 && error_reads==1 && clean());
  reset(2); CHECK((last_result=transfer_fixture(1,0))==100 && transfers==1 && error_reads==0 && clean());
  reset(3); CHECK((last_result=transfer_fixture(1,0))==100+EIO && committed==2 && closes==2 && clean());
  reset(4); CHECK((last_result=transfer_fixture(1,0))==100 && error_reads==0 && clean());
  reset(5); CHECK((last_result=transfer_fixture(1,1))==42 && transfers==0 && clean());
  reset(6); CHECK((last_result=transfer_fixture(0,0))==42 && transfers==1 && clean());
  reset(7); CHECK((last_result=transfer_fixture(0,0))==42 && transfers==2 && error_reads==1 && clean());
  reset(8); CHECK((last_result=transfer_fixture(0,0))==42 && transfers==1 && clean());
  reset(9); CHECK((last_result=transfer_fixture(0,0))==100+EIO && closes==2 && clean());
  reset(10); CHECK((last_result=transfer_fixture(0,0))==100 && error_reads==0 && clean());
  reset(11); CHECK((last_result=transfer_fixture(0,1))==42 && transfers==0 && clean());
  reset(12); CHECK((last_result=transfer_fixture(1,0))==100 && truncates==0 && error_reads==0 && closes==2 && clean());
  reset(13); CHECK((last_result=transfer_fixture(1,0))==100+EACCES && truncates==0 && closes==2 && clean());
  reset(14); CHECK((last_result=transfer_fixture(1,0))==100+EACCES && closes==1 && clean());
  reset(15); CHECK((last_result=transfer_fixture(1,0))==100+EBADF && closes==2 && clean());
  reset(16); CHECK((last_result=transfer_fixture(1,0))==100+EBADF && closes==2 && clean());
  reset(17); CHECK((last_result=transfer_fixture(1,0))==100+EINTR && closes==2 && clean());
  reset(18); CHECK((last_result=transfer_fixture(1,0))==100+EIO && closes==2 && clean());
  reset(19); CHECK((last_result=transfer_fixture(1,0))==100+EACCES && closes==0 && clean());
  reset(20); CHECK((last_result=list_fixture())==42 && iterations==3 && stream_closes==1 && closes==1 && clean());
  reset(21); CHECK((last_result=list_fixture())==100+EACCES && stream_closes==0 && closes==2 && clean());
  reset(22); CHECK((last_result=list_fixture())==100+EBADF && stream_closes==1 && clean());
  reset(23); CHECK((last_result=list_fixture())==100+EIO && stream_closes==1 && clean());
  reset(24); CHECK((last_result=list_fixture())==100 && stream_closes==1 && clean());
  reset(25); CHECK((last_result=list_fixture())==100+EBADF && stream_closes==1 && clean());
  reset(26); CHECK((last_result=list_fixture())==100 && stream_closes==1 && clean());
  reset(30); CHECK((last_result=inspect_fixture(0))==42 && opens==1 && children==1 && closes==2 && clean());
  reset(31); for(int i=1;i<=4;i++) CHECK((last_result=inspect_fixture(i))==100 && opens==0 && clean());
  reset(32); CHECK((last_result=inspect_fixture(5))==42 && closes==1 && clean());
  reset(33); CHECK((last_result=inspect_fixture(0))==100+ELOOP && closes==1 && clean());
  reset(34); CHECK((last_result=inspect_fixture(0))==100 && error_reads==0 && clean());
  reset(35); CHECK((last_result=inspect_fixture(0))==100+EACCES && clean());
  reset(40); CHECK((last_result=command_fixture(0))==42 && creations==1 && clean());
  reset(41); CHECK((last_result=command_fixture(1))==42 && removals==1 && clean());
  reset(42); CHECK((last_result=command_fixture(2))==42 && removals==1 && clean());
  reset(43); CHECK((last_result=command_fixture(1))==100 && removals==0 && error_reads==0 && clean());
  reset(44); CHECK((last_result=command_fixture(0))==100+ENOSPC && clean());
  reset(50); CHECK((last_result=unique_fixture())==42 && creations==3 && closes==2 && clean());
  reset(51); CHECK((last_result=unique_fixture())==100 && creations==128 && clean());
  reset(52); CHECK((last_result=unique_fixture())==100+EACCES && creations==1 && clean());
  reset(60); CHECK((last_result=cancellation_fixture())==42 && closes==2 && clean());
  return 42;
}
