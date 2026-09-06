import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Stdlib from '../src/Stdlib.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('clock/main', encoder.encode(source), target)

it('registers the clock modules, namespaces, and shared Instant alias', () => {
  assert.strictEqual(Stdlib.findNamespace('SystemClock')?.module, 'silk/system_clock')
  assert.strictEqual(Stdlib.findNamespace('MonotonicClock')?.module, 'silk/monotonic_clock')
  assert.strictEqual(Stdlib.findNamespace('Instant')?.module, 'silk/system_clock')
  assert.strictEqual(Stdlib.findNamespace('OsSystemClock')?.module, 'silk/os_system_clock')
  assert.strictEqual(Stdlib.findNamespace('OsMonotonicClock')?.module, 'silk/os_monotonic_clock')
  assert.deepEqual(Stdlib.find('silk/system_clock')?.aliases, ['Instant'])
})

it.effect('lowers the system clock to libc externs on Darwin and Linux', () =>
  Effect.gen(function* () {
    const source = `import silk.effect { Effect }
import silk.os_system_clock { OsSystemClock }
import silk.system_clock { SystemClock }
import silk.i64 as i64
pub fn main() -> i32 {
  let mut provider = OsSystemClock.make()
  let instant = run Effect.provideMut(SystemClock.now(), &mut provider)
  let resolution = run Effect.provideMut(SystemClock.getResolution(), &mut provider)
  if resolution > 0 { return i64.toI32(SystemClock.seconds(&instant)) }
  return 0
}`
    for (const target of ['aarch64-apple-darwin', 'x86_64-unknown-linux-gnu'] as const) {
      const system = yield* snapshot(source, target)
      assert.deepEqual(Analysis.diagnostics(system), [])
      const systemCalls = Analysis.loweredMir(system)
        .functions.flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'OsCall')
      assert.deepEqual(systemCalls, [])
      assert.deepEqual(
        system.instances.foreignCalls.map((call) => call.symbol),
        ['clock_getres', 'clock_gettime'],
      )
      const systemArtifact = yield* Analysis.codegen(system, { mode: 'release' })
      assert.deepEqual(systemArtifact.nativeRuntimeSymbols, ['silk_trap_report_v1'])
      assert.deepEqual(
        systemArtifact.foreignImports.map((entry) => entry.symbol),
        ['clock_getres', 'clock_gettime'],
      )
    }
  }),
)

it.effect('keeps monotonic clock intrinsics isolated from the system-clock migration', () =>
  Effect.gen(function* () {
    const monotonic = yield* snapshot(`import silk.effect { Effect }
import silk.monotonic_clock { MonotonicClock }
import silk.os_monotonic_clock { OsMonotonicClock }
pub fn main() -> i32 {
  let mut provider = OsMonotonicClock.make()
  run Effect.provideMut(MonotonicClock.waitFor(18446744073709551615), &mut provider)
  let resolution = run Effect.provideMut(MonotonicClock.getResolution(), &mut provider)
  if resolution > 0 { return 42 }
  return 0
}`)
    assert.deepEqual(Analysis.diagnostics(monotonic), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(monotonic)), [])
    const monotonicCalls = Analysis.loweredMir(monotonic)
      .functions.flatMap(MirVerification.operations)
      .filter((operation) => operation._tag === 'OsCall')
      .map((operation) => operation.operation.name)
    assert.deepEqual([...new Set(monotonicCalls)].sort(), [
      'osMonotonicClockNow',
      'osMonotonicClockResolution',
      'osMonotonicClockWaitUntil',
    ])
    const monotonicArtifact = yield* Analysis.codegen(monotonic, { mode: 'release' })
    assert.deepEqual([...monotonicArtifact.nativeRuntimeSymbols].sort(), [
      'silk_os_monotonic_clock_now_v1',
      'silk_os_monotonic_clock_resolution_v1',
      'silk_os_monotonic_clock_wait_until_v1',
      'silk_trap_report_v1',
    ])
  }),
)
