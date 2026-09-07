#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/resource.h>

_Static_assert(EINTR == 4, "EINTR");
extern int32_t construct_fixture(void);
extern int32_t read_fixture(int32_t, int64_t, int64_t);
extern uint64_t resolution_fixture(int32_t);
extern int32_t wait_fixture(int64_t, int64_t);
extern int32_t relative_fixture(uint64_t);

static struct timespec readings[8];
static int read_count, sleep_count, error_count, read_status, resolution_status, sleep_status;
static int expected_monotonic, direct_mode, error_slot;
static struct timespec resolution;
static void require(int condition) { if (!condition) _exit(71); }
static void reset(void) {
  read_count = sleep_count = error_count = read_status = resolution_status = sleep_status = 0;
  expected_monotonic = 1; direct_mode = 0; error_slot = EIO;
  readings[0] = (struct timespec){5, 900000000};
  readings[1] = (struct timespec){6, 100000000};
  readings[2] = (struct timespec){6, 700000000};
  readings[3] = (struct timespec){7, 100000000};
  resolution = (struct timespec){0, 1};
}
int clock_gettime(clockid_t clock, struct timespec *value) {
  require(clock == (expected_monotonic ? CLOCK_MONOTONIC : CLOCK_REALTIME));
  require(read_count < 8);
  *value = readings[read_count++];
  return read_status;
}
int clock_getres(clockid_t clock, struct timespec *value) {
  require(clock == (expected_monotonic ? CLOCK_MONOTONIC : CLOCK_REALTIME));
  *value = resolution;
  return resolution_status;
}
#ifdef __APPLE__
int *__error(void) { ++error_count; return &error_slot; }
int nanosleep(const struct timespec *request, struct timespec *remaining) {
  require(remaining == NULL);
  ++sleep_count;
  if (sleep_status != 0) { error_slot = EIO; return sleep_status; }
  // 7.1 - 5.9 = 1.2; after EINTR 7.1 - 6.1 = 1.0; after early success = .4.
  require(sleep_count <= 3);
  const struct timespec expected[3] = {{1,200000000},{1,0},{0,400000000}};
  require(request->tv_sec == expected[sleep_count-1].tv_sec);
  require(request->tv_nsec == expected[sleep_count-1].tv_nsec);
  if (sleep_count == 1) { error_slot = EINTR; return -1; }
  error_slot = EIO; // Stale errno on successful sleep must not be consulted.
  return 0;
}
#else
int clock_nanosleep(clockid_t clock, int flags, const struct timespec *request, struct timespec *remaining) {
  require(clock == CLOCK_MONOTONIC && flags == TIMER_ABSTIME && remaining == NULL);
  ++sleep_count;
  if (sleep_status != 0) { errno = EINTR; return sleep_status; }
  if (direct_mode) { require(request->tv_sec == 0 && request->tv_nsec == 0); return 0; }
  require(request->tv_sec == 7 && request->tv_nsec == 100000000);
  require(sleep_count <= 3);
  errno = EIO;
  return sleep_count < 3 ? EINTR : 0;
}
#endif

static void invalid_case(int which) {
  reset();
  switch(which) {
    case 0: read_status=-1; read_fixture(1,0,0); break;
    case 1: readings[0].tv_nsec=-1; read_fixture(1,0,0); break;
    case 2: readings[0].tv_nsec=1000000000; read_fixture(1,0,0); break;
    case 3: readings[0].tv_sec=-1; read_fixture(1,0,0); break;
    case 4: resolution_status=-1; resolution_fixture(1); break;
    case 5: resolution=(struct timespec){0,0}; resolution_fixture(1); break;
    case 6: resolution=(struct timespec){-1,1}; resolution_fixture(1); break;
    case 7: resolution=(struct timespec){1,-1}; resolution_fixture(1); break;
    case 8: resolution=(struct timespec){1,1000000000}; resolution_fixture(1); break;
    case 9: resolution=(struct timespec){18446744074LL,0}; resolution_fixture(1); break;
    case 10: resolution=(struct timespec){18446744073LL,709551616}; resolution_fixture(1); break;
    case 11: wait_fixture(-1,0); break;
    case 12: wait_fixture(1,-1); break;
    case 13: wait_fixture(1,1000000000); break;
    case 14:
#ifdef __APPLE__
      sleep_status=-1;
#else
      sleep_status=EIO;
#endif
      wait_fixture(7,100000000); break;
    case 15: readings[0]=(struct timespec){INT64_MAX,999999999}; relative_fixture(1); break;
    case 16: sleep_status=2; wait_fixture(7,100000000); break;
    default: _exit(72);
  }
  _exit(73); // Returning normally is never an acceptable fatal-policy outcome.
}
int main(void) {
  struct rlimit limit = {0,0}; require(setrlimit(RLIMIT_CORE,&limit)==0);
  reset(); require(construct_fixture()==42); require(read_count==0 && sleep_count==0 && error_count==0);
  require(read_fixture(1,5,900000000)==42);
  reset(); expected_monotonic=0; readings[0]=(struct timespec){-1,999999999};
  require(read_fixture(0,-1,999999999)==42); require(error_count==0);
  reset(); require(resolution_fixture(1)==1);
  expected_monotonic=0; resolution=(struct timespec){18446744073LL,709551615};
  require(resolution_fixture(0)==UINT64_MAX);
  reset(); require(wait_fixture(7,100000000)==42); require(sleep_count==3);
#ifdef __APPLE__
  require(read_count==4 && error_count==1);
#else
  require(read_count==0 && error_count==0);
#endif
  reset();
  // The relative operation derives 7.1 from one starting read of 5.9 + 1.2s.
#ifdef __APPLE__
  readings[1]=readings[0]; readings[2]=(struct timespec){6,100000000};
  readings[3]=(struct timespec){6,700000000}; readings[4]=(struct timespec){7,100000000};
#endif
  require(relative_fixture(1200000000)==42); require(sleep_count==3);
#ifdef __APPLE__
  require(read_count==5);
#else
  require(read_count==1);
#endif
  reset(); direct_mode=1; require(wait_fixture(0,0)==42);
#ifdef __APPLE__
  require(sleep_count==0 && read_count==1);
#endif
  reset(); readings[0]=(struct timespec){0,0}; readings[1]=(struct timespec){0,0}; direct_mode=1;
  require(relative_fixture(0)==42);
#ifdef __APPLE__
  require(sleep_count==0 && read_count==2);
#else
  require(read_count==1);
#endif
  for (int which=0; which<17; ++which) {
    pid_t child=fork(); require(child>=0);
    if (child==0) invalid_case(which);
    int status=0; require(waitpid(child,&status,0)==child);
    require(WIFSIGNALED(status));
    require(WTERMSIG(status)==SIGILL || WTERMSIG(status)==SIGTRAP || WTERMSIG(status)==SIGABRT);
  }
  return 42;
}
