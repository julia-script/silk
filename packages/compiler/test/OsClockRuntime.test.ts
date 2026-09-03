import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as OsRuntime from '../src/OsRuntime.js'
import * as Termination from '../src/Termination.js'
import * as ToolchainPlan from '../src/ToolchainPlan.js'

const testRoot = mkdtempSync(join(tmpdir(), 'silk-os-clock-runtime-'))
afterAll(() => {
  rmSync(testRoot, { recursive: true, force: true })
})

const clang = Effect.runSync(
  Config.string('SILK_TEST_CLANG').pipe(Config.withDefault('/usr/bin/clang')),
)
const termination = (...identities: ReadonlyArray<string>): Termination.Contract =>
  Object.freeze({
    _tag: 'EntryTermination',
    success: identities.length === 0 ? 'ReturnedStatus' : 'Zero',
    failures: Object.freeze(
      identities.map((identity, ordinal) => Object.freeze({ tag: ordinal + 1, identity })),
    ),
    logicalFrames: Object.freeze([]),
    report: Termination.emptyReport,
  })

const clockSymbols = Object.freeze([
  'silk_os_monotonic_clock_now_v1',
  'silk_os_monotonic_clock_resolution_v1',
  'silk_os_monotonic_clock_wait_until_v1',
])

const strictSyntaxCheck = (source: string): void => {
  const checked = spawnSync(
    clang,
    [
      '-x',
      'c',
      '-std=c11',
      '-Wall',
      '-Wextra',
      '-Werror',
      '-Wno-unused-function',
      '-fsyntax-only',
      '-',
    ],
    { input: source, encoding: 'utf8' },
  )
  assert.strictEqual(checked.status, 0, checked.stderr)
}

it('keeps clock-only runtime source minimal and owns feature macros in the complete unit', () => {
  const nowOnly = OsRuntime.source(['silk_os_monotonic_clock_now_v1'])
  assert.include(nowOnly, 'silk_os_monotonic_clock_now_v1')
  assert.include(nowOnly, 'silk_clock_read')
  assert.notInclude(nowOnly, 'silk_clock_resolution')
  assert.notInclude(nowOnly, 'silk_clock_deadline')
  assert.notInclude(nowOnly, 'silk_os_system_clock_now_v1')
  assert.notInclude(nowOnly, '<errno.h>')

  const clockOnly = OsRuntime.source(clockSymbols)
  assert.include(clockOnly, '#include <time.h>')
  assert.include(clockOnly, 'silk_os_monotonic_clock_wait_until_v1')
  assert.notInclude(clockOnly, '_POSIX_C_SOURCE')
  assert.notInclude(clockOnly, '<dirent.h>')
  assert.notInclude(clockOnly, '<poll.h>')
  assert.notInclude(clockOnly, 'O_NOFOLLOW')
  assert.notInclude(clockOnly, 'silk_reason_from_errno')
  assert.notInclude(clockOnly, 'silk_os_path_inspect_v1')

  const combined = ToolchainPlan.executableSource(termination(), [
    ...clockSymbols,
    'silk_standard_stream_write_v1',
  ])
  const featureMacro = process.platform === 'darwin' ? '_DARWIN_C_SOURCE' : '_GNU_SOURCE'
  assert.isBelow(combined.indexOf(featureMacro), combined.indexOf('#include'))
  assert.isBelow(combined.indexOf('_POSIX_C_SOURCE'), combined.indexOf('#include'))
  strictSyntaxCheck(combined)

  const typedFailure = ToolchainPlan.executableSource(termination('clock.Failure'), clockSymbols)
  assert.isBelow(typedFailure.indexOf(featureMacro), typedFailure.indexOf('#include'))
  strictSyntaxCheck(typedFailure)
})

const fakeTimeHeader = `#ifndef SILK_TEST_TIME_H
#define SILK_TEST_TIME_H
#include <stddef.h>
typedef int clockid_t;
typedef __int128 time_t;
struct timespec { time_t tv_sec; long tv_nsec; };
#define CLOCK_REALTIME 1
#define CLOCK_MONOTONIC 2
#define TIMER_ABSTIME 1
int clock_gettime(clockid_t clock, struct timespec *value);
int clock_getres(clockid_t clock, struct timespec *value);
int clock_nanosleep(clockid_t clock, int flags, const struct timespec *request,
                    struct timespec *remaining);
int nanosleep(const struct timespec *request, struct timespec *remaining);
#endif
`

const harnessBody = `
#define SILK_TEST_LIMIT 16

static struct timespec gettime_values[SILK_TEST_LIMIT];
static int gettime_statuses[SILK_TEST_LIMIT];
static int gettime_count;
static int gettime_index;
static clockid_t gettime_clock;

static struct timespec getres_values[SILK_TEST_LIMIT];
static int getres_statuses[SILK_TEST_LIMIT];
static int getres_count;
static int getres_index;
static clockid_t getres_clock;

static int absolute_statuses[SILK_TEST_LIMIT];
static int absolute_errnos[SILK_TEST_LIMIT];
static struct timespec absolute_requests[SILK_TEST_LIMIT];
static int absolute_count;
static int absolute_index;
static clockid_t absolute_clock;
static int absolute_flags;

static int relative_statuses[SILK_TEST_LIMIT];
static int relative_errnos[SILK_TEST_LIMIT];
static struct timespec relative_requests[SILK_TEST_LIMIT];
static int relative_count;
static int relative_index;

int silk_test_clock_gettime(clockid_t clock, struct timespec *value) {
  gettime_clock = clock;
  if (gettime_index >= gettime_count) return -1;
  *value = gettime_values[gettime_index];
  return gettime_statuses[gettime_index++];
}

int silk_test_clock_getres(clockid_t clock, struct timespec *value) {
  getres_clock = clock;
  if (getres_index >= getres_count) return -1;
  *value = getres_values[getres_index];
  return getres_statuses[getres_index++];
}

int silk_test_clock_nanosleep(clockid_t clock, int flags, const struct timespec *request,
                              struct timespec *remaining) {
  (void)remaining;
  absolute_clock = clock;
  absolute_flags = flags;
  if (absolute_index >= absolute_count) return 99;
  absolute_requests[absolute_index] = *request;
  errno = absolute_errnos[absolute_index];
  return absolute_statuses[absolute_index++];
}

int silk_test_nanosleep(const struct timespec *request, struct timespec *remaining) {
  (void)remaining;
  if (relative_index >= relative_count) return -1;
  relative_requests[relative_index] = *request;
  errno = relative_errnos[relative_index];
  return relative_statuses[relative_index++];
}

static void reset_calls(void) {
  for (int index = 0; index < SILK_TEST_LIMIT; index += 1) {
    gettime_statuses[index] = 0;
    getres_statuses[index] = 0;
    absolute_statuses[index] = 0;
    absolute_errnos[index] = 0;
    relative_statuses[index] = 0;
    relative_errnos[index] = 0;
  }
  gettime_count = 0;
  gettime_index = 0;
  getres_count = 0;
  getres_index = 0;
  absolute_count = 0;
  absolute_index = 0;
  relative_count = 0;
  relative_index = 0;
}

#define CHECK(condition, code) do { if (!(condition)) return (code); } while (0)

static int check_reads_and_resolutions(void) {
  int64_t seconds = 41;
  int64_t nanoseconds = 42;
  uint64_t resolution = UINT64_C(43);

  reset_calls();
  gettime_count = 3;
  gettime_values[0] = (struct timespec){ (time_t)7, 8L };
  gettime_values[1] = (struct timespec){ (time_t)7, 8L };
  gettime_values[2] = (struct timespec){ (time_t)7, 9L };
  CHECK(silk_os_monotonic_clock_now_v1(&seconds, &nanoseconds) == 1, 19);
  CHECK(seconds == 7 && nanoseconds == 8, 20);
  CHECK(silk_os_monotonic_clock_now_v1(&seconds, &nanoseconds) == 1, 21);
  CHECK(seconds == 7 && nanoseconds == 8, 22);
  CHECK(silk_os_monotonic_clock_now_v1(&seconds, &nanoseconds) == 1, 23);
  CHECK(seconds == 7 && nanoseconds == 9 && gettime_clock == CLOCK_MONOTONIC, 24);

  seconds = 41; nanoseconds = 42;
  reset_calls();
  gettime_count = 1;
  gettime_values[0] = (struct timespec){ (time_t)7, -1L };
  CHECK(silk_os_monotonic_clock_now_v1(&seconds, &nanoseconds) == 0, 25);
  CHECK(seconds == 41 && nanoseconds == 42, 26);

  reset_calls();
  gettime_count = 1;
  gettime_statuses[0] = -1;
  CHECK(silk_os_monotonic_clock_now_v1(&seconds, &nanoseconds) == 0, 27);
  CHECK(seconds == 41 && nanoseconds == 42, 28);

  resolution = UINT64_C(43);
  reset_calls();
  getres_count = 1;
  getres_values[0] = (struct timespec){ (time_t)0, 25L };
  CHECK(silk_os_monotonic_clock_resolution_v1(&resolution) == 1, 29);
  CHECK(resolution == UINT64_C(25) && getres_clock == CLOCK_MONOTONIC, 30);

  resolution = UINT64_C(43);
  reset_calls();
  getres_count = 1;
  getres_values[0] = (struct timespec){ (time_t)0, 1000000000L };
  CHECK(silk_os_monotonic_clock_resolution_v1(&resolution) == 0, 31);
  CHECK(resolution == UINT64_C(43), 32);

  reset_calls();
  getres_count = 1;
  getres_statuses[0] = -1;
  CHECK(silk_os_monotonic_clock_resolution_v1(&resolution) == 0, 33);
  CHECK(resolution == UINT64_C(43), 34);
  return 0;
}

static int check_linux_wait(void) {
  reset_calls();
  CHECK(silk_os_monotonic_clock_wait_until_v1(-1, 0) == 0, 40);
  CHECK(silk_os_monotonic_clock_wait_until_v1(1, -1) == 0, 41);
  CHECK(silk_os_monotonic_clock_wait_until_v1(1, 1000000000LL) == 0, 42);
  CHECK(absolute_index == 0 && gettime_index == 0, 43);

  reset_calls();
  absolute_count = 1;
  CHECK(silk_os_monotonic_clock_wait_until_v1(1, 2) == 1, 44);
  CHECK(absolute_index == 1 && absolute_clock == CLOCK_MONOTONIC, 45);
  CHECK(absolute_flags == TIMER_ABSTIME, 46);
  CHECK(absolute_requests[0].tv_sec == 1 && absolute_requests[0].tv_nsec == 2, 47);

  reset_calls();
  absolute_count = 3;
  absolute_statuses[0] = EINTR;
  absolute_statuses[1] = EINTR;
  absolute_statuses[2] = 0;
  absolute_errnos[0] = EINVAL;
  absolute_errnos[1] = EINVAL;
  absolute_errnos[2] = EINVAL;
  CHECK(silk_os_monotonic_clock_wait_until_v1(9, 10) == 1, 48);
  CHECK(absolute_index == 3, 49);
  for (int index = 0; index < 3; index += 1) {
    CHECK(absolute_requests[index].tv_sec == 9 && absolute_requests[index].tv_nsec == 10, 50);
  }

  reset_calls();
  absolute_count = 1;
  absolute_statuses[0] = EINVAL;
  absolute_errnos[0] = EINTR;
  CHECK(silk_os_monotonic_clock_wait_until_v1(9, 10) == 0, 51);
  CHECK(absolute_index == 1, 52);
  return 0;
}

static int check_fallback_wait(void) {
  reset_calls();
  CHECK(silk_os_monotonic_clock_wait_until_v1(-1, 0) == 0, 60);
  CHECK(silk_os_monotonic_clock_wait_until_v1(1, -1) == 0, 61);
  CHECK(silk_os_monotonic_clock_wait_until_v1(1, 1000000000LL) == 0, 62);
  CHECK(relative_index == 0 && gettime_index == 0, 63);

  reset_calls();
  gettime_count = 1;
  gettime_values[0] = (struct timespec){ (time_t)10, 0L };
  CHECK(silk_os_monotonic_clock_wait_until_v1(9, 999999999) == 1, 64);
  CHECK(gettime_index == 1 && relative_index == 0, 65);

  reset_calls();
  gettime_count = 2;
  gettime_values[0] = (struct timespec){ (time_t)5, 900000000L };
  gettime_values[1] = (struct timespec){ (time_t)6, 100000000L };
  relative_count = 1;
  relative_statuses[0] = 0;
  CHECK(silk_os_monotonic_clock_wait_until_v1(6, 100000000) == 1, 66);
  CHECK(relative_index == 1 && gettime_index == 2, 67);
  CHECK(relative_requests[0].tv_sec == 0 && relative_requests[0].tv_nsec == 200000000L, 68);

  reset_calls();
  gettime_count = 4;
  gettime_values[0] = (struct timespec){ (time_t)8, 0L };
  gettime_values[1] = (struct timespec){ (time_t)9, 500000000L };
  gettime_values[2] = (struct timespec){ (time_t)9, 750000000L };
  gettime_values[3] = (struct timespec){ (time_t)10, 0L };
  relative_count = 3;
  relative_statuses[0] = -1;
  relative_errnos[0] = EINTR;
  relative_statuses[1] = 0;
  relative_statuses[2] = -1;
  relative_errnos[2] = EINTR;
  CHECK(silk_os_monotonic_clock_wait_until_v1(10, 0) == 1, 69);
  CHECK(relative_index == 3 && gettime_index == 4, 70);
  CHECK(relative_requests[0].tv_sec == 2 && relative_requests[0].tv_nsec == 0, 71);
  CHECK(relative_requests[1].tv_sec == 0 && relative_requests[1].tv_nsec == 500000000L, 72);
  CHECK(relative_requests[2].tv_sec == 0 && relative_requests[2].tv_nsec == 250000000L, 73);

  reset_calls();
  gettime_count = 1;
  gettime_statuses[0] = -1;
  CHECK(silk_os_monotonic_clock_wait_until_v1(10, 0) == 0, 74);
  CHECK(relative_index == 0, 75);

  reset_calls();
  gettime_count = 1;
  gettime_values[0] = (struct timespec){ (time_t)9, 0L };
  relative_count = 1;
  relative_statuses[0] = -1;
  relative_errnos[0] = EINVAL;
  CHECK(silk_os_monotonic_clock_wait_until_v1(10, 0) == 0, 76);
  CHECK(relative_index == 1, 77);
  return 0;
}

int main(void) {
  int read_status = check_reads_and_resolutions();
  if (read_status != 0) return read_status;
#if SILK_TEST_FALLBACK
  int wait_status = check_fallback_wait();
#else
  int wait_status = check_linux_wait();
#endif
  return wait_status == 0 ? 42 : wait_status;
}
`

const runHarness = (name: string, forceFallback: boolean): void => {
  const runtime = OsRuntime.source(clockSymbols)
  const selectedRuntime = forceFallback
    ? runtime.replace('#if defined(__linux__)', '#if 0 /* forced fallback harness */')
    : runtime.replace('#if defined(__linux__)', '#if 1 /* forced Linux harness */')
  assert.notStrictEqual(selectedRuntime, runtime)
  const source = `#define clock_gettime silk_test_clock_gettime
#define clock_getres silk_test_clock_getres
#define clock_nanosleep silk_test_clock_nanosleep
#define nanosleep silk_test_nanosleep
#define SILK_TEST_FALLBACK ${forceFallback ? '1' : '0'}
${selectedRuntime}
${harnessBody}`
  const sourcePath = join(testRoot, `${name}.c`)
  const executablePath = join(testRoot, name)
  writeFileSync(sourcePath, source)
  const built = spawnSync(
    clang,
    [
      '-std=c11',
      '-Wall',
      '-Wextra',
      '-Werror',
      '-Wno-unused-function',
      '-I',
      testRoot,
      sourcePath,
      '-o',
      executablePath,
    ],
    { encoding: 'utf8' },
  )
  assert.strictEqual(built.status, 0, built.stderr)
  const executed = spawnSync(executablePath, [], { encoding: 'utf8' })
  assert.strictEqual(executed.status, 42, executed.stderr)
}

writeFileSync(join(testRoot, 'time.h'), fakeTimeHeader)

it('runs the substituted POSIX clock conversion and Linux absolute-wait harness', () => {
  runHarness('clock-linux-harness', false)
})

it('runs the forced non-Linux absolute-deadline fallback harness', () => {
  runHarness('clock-fallback-harness', true)
})
