#include <pthread.h>
#include <stdio.h>
extern int process_real_fixture(void);
static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t condition = PTHREAD_COND_INITIALIZER;
static int ready;
static void *exercise(void *result) {
  pthread_mutex_lock(&lock);
  if (++ready == 2) pthread_cond_broadcast(&condition);
  while (ready != 2) pthread_cond_wait(&condition, &lock);
  pthread_mutex_unlock(&lock);
  *(int *)result = process_real_fixture();
  return NULL;
}
int main(void) {
  pthread_t first, second;
  int a = 0, b = 0;
  if (pthread_create(&first, NULL, exercise, &a)) return 1;
  if (pthread_create(&second, NULL, exercise, &b)) return 2;
  if (pthread_join(first, NULL) || pthread_join(second, NULL)) return 3;
  if (a != 42 || b != 42) {fprintf(stderr, "process invocations: %d %d\n", a, b); return 4;}
  puts("overlapping process invocations preserve byte arguments and separate captures");
  return 42;
}
