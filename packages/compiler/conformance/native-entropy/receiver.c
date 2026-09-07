#include <signal.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/resource.h>
extern int32_t construct_fixture(void);
extern int32_t fill_fixture(int32_t);
static int calls, errors, mode, slot;
static unsigned char *base;
static size_t committed;
static void require(int condition) { if (!condition) _exit(71); }
static void reset(int scenario) { calls=errors=0; mode=scenario; base=NULL; committed=0; }
static void check_state(void) {
  for(size_t i=0;i<257;++i) require(base[i]==(i<committed ? 42 : 9));
}
#ifdef __APPLE__
void arc4random_buf(void *buffer,size_t count) {
  ++calls; require(calls==1 && count==257);
  base=buffer; check_state();
  for(size_t i=0;i<count;++i) base[i]=42;
  committed=count;
}
#else
int *__errno_location(void) { require(mode!=2 && mode!=6); ++errors; check_state(); return &slot; }
ssize_t getrandom(void *buffer,size_t count,unsigned int flags) {
  require(flags==GRND_NONBLOCK);
  if(calls==0) base=buffer;
  ++calls; require(calls<8);
  require((unsigned char *)buffer==base+committed);
  size_t remaining=257-committed;
  require(count==(remaining<256?remaining:256));
  check_state();
  // After partial progress, fail with distinct native errors or invalid counts.
  if(mode>=2 && (mode!=7 || calls>1)) {
    if(mode==2) { slot=EINTR; return 0; }
    if(mode==3) { slot=EAGAIN; return -1; }
    if(mode==4) { slot=ENOSYS; return -1; }
    if(mode==5) { slot=EPERM; return -1; }
    if(mode==6) { slot=EINTR; return (ssize_t)count+1; }
    slot=EIO; return -1;
  }
  if(mode==1 && calls==2) { slot=EINTR; return -1; }
  size_t transferred=(mode==1 || mode==7) && calls==1 ? 3 : count;
  for(size_t i=0;i<transferred;++i) ((unsigned char *)buffer)[i]=42;
  committed+=transferred;
  slot=EIO; // A successful transfer must never consult stale errno.
  return (ssize_t)transferred;
}
#endif
int main(void) {
  struct rlimit limit={0,0}; require(setrlimit(RLIMIT_CORE,&limit)==0);
  reset(0); require(construct_fixture()==42 && calls==0 && errors==0);
  require(fill_fixture(1)==42 && calls==0 && errors==0);
  require(fill_fixture(0)==42 && committed==257 && errors==0);
#ifdef __APPLE__
  require(calls==1);
#else
  require(calls==2);
  reset(1); require(fill_fixture(0)==42 && committed==257 && calls==3 && errors==1);
  for(int scenario=2;scenario<=7;++scenario) {
    pid_t child=fork(); require(child>=0);
    if(child==0) { reset(scenario); fill_fixture(0); _exit(73); }
    int status=0; require(waitpid(child,&status,0)==child);
    require(WIFSIGNALED(status));
    require(WTERMSIG(status)==SIGILL || WTERMSIG(status)==SIGTRAP || WTERMSIG(status)==SIGABRT);
  }
#endif
  return 42;
}
