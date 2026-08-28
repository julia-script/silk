import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as OsRuntime from '../src/OsRuntime.js'

const testRoot = mkdtempSync(join(tmpdir(), 'silk-os-random-runtime-'))
afterAll(() => {
  rmSync(testRoot, { recursive: true, force: true })
})

const clang = process.env.SILK_TEST_CLANG ?? '/usr/bin/clang'
const symbol = 'silk_os_random_fill_v1'

const harness = `
#define SILK_TEST_LIMIT 16
static long results[SILK_TEST_LIMIT];
static int errors[SILK_TEST_LIMIT];
static unsigned char *pointers[SILK_TEST_LIMIT];
static size_t lengths[SILK_TEST_LIMIT];
static unsigned int flags_seen[SILK_TEST_LIMIT];
static int calls;

static void reset_calls(void) {
  calls = 0;
  for (int index = 0; index < SILK_TEST_LIMIT; index += 1) {
    results[index] = 0;
    errors[index] = 0;
    pointers[index] = NULL;
    lengths[index] = 0;
    flags_seen[index] = 0;
  }
}

ssize_t silk_test_getrandom(void *output, size_t length, unsigned int flags) {
  if (calls >= SILK_TEST_LIMIT) return -1;
  pointers[calls] = (unsigned char *)output;
  lengths[calls] = length;
  flags_seen[calls] = flags;
  ssize_t result = (ssize_t)results[calls];
  errno = errors[calls];
  if (result > 0) {
    for (long index = 0; index < result; index += 1) {
      ((unsigned char *)output)[index] = (unsigned char)(calls + 1);
    }
  }
  calls += 1;
  return result;
}

void silk_test_arc4random_buf(void *output, size_t length) {
  pointers[calls] = (unsigned char *)output;
  lengths[calls] = length;
  for (size_t index = 0; index < length; index += 1) {
    ((unsigned char *)output)[index] = (unsigned char)(40 + index);
  }
  calls += 1;
}

#define CHECK(condition, code) do { if (!(condition)) return (code); } while (0)

static int check_linux(void) {
  unsigned char output[12] = { 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90 };
  reset_calls();
  CHECK(silk_os_random_fill_v1(output, 0) == 1 && calls == 0, 1);

  reset_calls();
  results[0] = 2; results[1] = 4; results[2] = 4;
  CHECK(silk_os_random_fill_v1(output, 10) == 1, 2);
  CHECK(calls == 3, 3);
  CHECK(lengths[0] == 4 && lengths[1] == 4 && lengths[2] == 4, 4);
  CHECK(pointers[0] == output && pointers[1] == output + 2 && pointers[2] == output + 6, 5);
  CHECK(output[0] == 1 && output[2] == 2 && output[6] == 3, 6);
  CHECK(flags_seen[0] == GRND_NONBLOCK && flags_seen[1] == GRND_NONBLOCK &&
        flags_seen[2] == GRND_NONBLOCK, 12);

  reset_calls();
  results[0] = -1; errors[0] = EINTR; results[1] = 3;
  CHECK(silk_os_random_fill_v1(output, 3) == 1 && calls == 2, 7);
  CHECK(pointers[0] == output && pointers[1] == output, 8);

  reset_calls();
  results[0] = -1; errors[0] = EAGAIN;
  CHECK(silk_os_random_fill_v1(output, 3) == 0 && calls == 1, 9);

  reset_calls();
  results[0] = -1; errors[0] = EINVAL;
  CHECK(silk_os_random_fill_v1(output, 3) == 0 && calls == 1, 10);

  reset_calls();
  results[0] = 2; results[1] = 0;
  CHECK(silk_os_random_fill_v1(output, 3) == 0 && calls == 2, 11);
  return 0;
}

static int check_macos(void) {
  unsigned char output[5] = { 90, 90, 90, 90, 90 };
  reset_calls();
  CHECK(silk_os_random_fill_v1(output, 0) == 1 && calls == 0, 20);
  CHECK(silk_os_random_fill_v1(output, 5) == 1, 21);
  CHECK(calls == 1 && pointers[0] == output && lengths[0] == 5, 22);
  CHECK(output[0] == 40 && output[4] == 44, 23);
  return 0;
}

int main(void) {
#if SILK_TEST_MACOS
  int status = check_macos();
#else
  int status = check_linux();
#endif
  return status == 0 ? 42 : status;
}
`

const compileAndRun = (name: string, macos: boolean): void => {
  const runtime = OsRuntime.source([symbol])
  const withSmallCap = runtime.replace('((size_t)33554431)', '((size_t)4)')
  const selected = macos
    ? withSmallCap
        .replace(
          '#elif defined(__APPLE__)\n#include <stdlib.h>\n#else\n#error "Silk OS random supports only GNU/Linux and macOS"',
          '#else\n#include <stdlib.h>',
        )
        .replaceAll('#if defined(__linux__)', '#if 0 /* forced macOS harness */')
    : withSmallCap
        .replace(
          '#elif defined(__APPLE__)\n#include <stdlib.h>\n#else\n#error "Silk OS random supports only GNU/Linux and macOS"',
          '#elif 0\n#include <stdlib.h>\n#else\n#error "forced Linux harness selected no platform"',
        )
        .replaceAll('#if defined(__linux__)', '#if 1 /* forced Linux harness */')
  const source = `#include <errno.h>
#include <sys/types.h>
ssize_t silk_test_getrandom(void *output, size_t length, unsigned int flags);
void silk_test_arc4random_buf(void *output, size_t length);
#ifndef GRND_NONBLOCK
#define GRND_NONBLOCK 1
#endif
#define getrandom silk_test_getrandom
#define arc4random_buf silk_test_arc4random_buf
#define SILK_TEST_MACOS ${macos ? '1' : '0'}
${selected}
${harness}`
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

it('keeps secure random support reachability-selected and independent', () => {
  const source = OsRuntime.source([symbol])
  assert.include(source, '#define SILK_GETRANDOM_MAX ((size_t)33554431)')
  assert.include(source, 'GRND_NONBLOCK')
  assert.include(source, 'arc4random_buf(output, length)')
  assert.notInclude(source, 'silk_entropy')
  assert.notInclude(source, 'silk_reason_from_errno')
  assert.notInclude(source, 'silk_os_system_clock')
  assert.notInclude(OsRuntime.source([]), symbol)
})

it('runs the forced GNU/Linux nonblocking exact-fill harness', () => {
  compileAndRun('random-linux-harness', false)
})

it('runs the forced macOS one-call exact-fill harness', () => {
  compileAndRun('random-macos-harness', true)
})
