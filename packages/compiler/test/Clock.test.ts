import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Stdlib from '../src/Stdlib.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('clock/main', encoder.encode(source), target)

const fixedSystemClock = `import silk.system_clock as SystemClock
import silk.u64 as u64

struct FixedSystemClock {
  seconds: i64
  nanoseconds: i64
  resolution: u64
}

effect fn fixedSystemNow(self: &mut FixedSystemClock) -> SystemClock.Instant {
  return SystemClock.make(self.seconds, self.nanoseconds)
}

effect fn fixedSystemResolution(self: &mut FixedSystemClock) -> u64 {
  return self.resolution
}

impl SystemClock.SystemClock for FixedSystemClock {
  now: FixedSystemClock.fixedSystemNow
  getResolution: FixedSystemClock.fixedSystemResolution
}
`

const scriptedMonotonicClock = `import silk.monotonic_clock as MonotonicClock
import silk.system_clock as SystemClock
import silk.u64 as u64

struct ScriptedMonotonicClock {
  seconds: i64
  nanoseconds: i64
  resolution: u64
  waits: u64
}

effect fn scriptedMonotonicNow(
  self: &mut ScriptedMonotonicClock
) -> SystemClock.Instant {
  return SystemClock.make(self.seconds, self.nanoseconds)
}

effect fn scriptedMonotonicResolution(self: &mut ScriptedMonotonicClock) -> u64 {
  return self.resolution
}

effect fn scriptedWaitUntil(
  self: &mut ScriptedMonotonicClock,
  when: SystemClock.Instant
) -> () {
  let mut deadline = move when
  let targetSeconds = SystemClock.seconds(&deadline)
  let targetNanoseconds = SystemClock.nanoseconds(&deadline)
  let mut future = targetSeconds > self.seconds
  if targetSeconds == self.seconds {
    future = targetNanoseconds > self.nanoseconds
  }
  if future {
    self.seconds = targetSeconds
    self.nanoseconds = targetNanoseconds
  }
  self.waits = self.waits + u64.toU64(1)
  return ()
}

effect fn scriptedWaitFor(self: &mut ScriptedMonotonicClock, howLong: u64) -> () {
  let billion = u64.toU64(1000000000)
  let wholeSeconds = u64.toI64(howLong / billion)
  let fraction = u64.toI64(howLong % billion)
  let mut nextNanoseconds = self.nanoseconds + fraction
  let mut carry = wholeSeconds - wholeSeconds
  if nextNanoseconds >= 1000000000 {
    nextNanoseconds = nextNanoseconds - 1000000000
    carry = 1
  }
  self.seconds = self.seconds + wholeSeconds + carry
  self.nanoseconds = nextNanoseconds
  self.waits = self.waits + u64.toU64(1)
  return ()
}

impl MonotonicClock.MonotonicClock for ScriptedMonotonicClock {
  now: ScriptedMonotonicClock.scriptedMonotonicNow
  getResolution: ScriptedMonotonicClock.scriptedMonotonicResolution
  waitUntil: ScriptedMonotonicClock.scriptedWaitUntil
  waitFor: ScriptedMonotonicClock.scriptedWaitFor
}
`

it('registers the clock modules, namespaces, and shared Instant alias', () => {
  assert.strictEqual(Stdlib.findNamespace('SystemClock')?.module, 'silk/system_clock')
  assert.strictEqual(Stdlib.findNamespace('MonotonicClock')?.module, 'silk/monotonic_clock')
  assert.strictEqual(Stdlib.findNamespace('Instant')?.module, 'silk/system_clock')
  assert.deepEqual(Stdlib.find('silk/system_clock')?.aliases, ['Instant'])
})

it.effect('reads canonical pre-epoch system time through an ordinary provider', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`${fixedSystemClock}
import silk.effect as Effect

pub fn main() -> i32 {
  let mut epoch = SystemClock.make(0, 0)
  if SystemClock.seconds(&epoch) != 0 { return 1 }
  if SystemClock.nanoseconds(&epoch) != 0 { return 2 }
  let mut provider = FixedSystemClock {
    seconds: -1,
    nanoseconds: 999999999,
    resolution: u64.toU64(1)
  }
  let instant = run Effect.provideMut(SystemClock.now(), &mut provider)
  if SystemClock.seconds(&instant) != -1 { return 3 }
  if SystemClock.nanoseconds(&instant) != 999999999 { return 4 }
  let resolution = run Effect.provideMut(SystemClock.getResolution(), &mut provider)
  if resolution != u64.toU64(1) { return 5 }
  return 42
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('traps on noncanonical Instant fractions and keeps its fields private', () =>
  Effect.gen(function* () {
    for (const fraction of ['-1', '1000000000']) {
      const self = yield* snapshot(`import silk.system_clock as SystemClock
pub fn main() -> i32 {
  let instant = SystemClock.make(0, ${fraction})
  drop instant
  return 42
}`)
      assert.deepEqual(Analysis.diagnostics(self), [])
      assert.strictEqual(Analysis.evaluate(self)._tag, 'Trap')
    }

    const privateField = yield* snapshot(`import silk.system_clock as SystemClock
pub fn main() -> i32 {
  let instant = SystemClock.make(0, 1)
  return instant.seconds
}`)
    assert.deepEqual(
      Analysis.diagnostics(privateField).map((diagnostic) => diagnostic.code),
      ['SEM0028'],
    )
  }),
)

it.effect('advances a scripted monotonic timeline without moving backwards', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`${scriptedMonotonicClock}
import silk.effect as Effect

pub fn main() -> i32 {
  let mut provider = ScriptedMonotonicClock {
    seconds: 10,
    nanoseconds: 500,
    resolution: u64.toU64(5),
    waits: u64.MIN
  }
  let initial = run Effect.provideMut(MonotonicClock.now(), &mut provider)
  if SystemClock.seconds(&initial) != 10 { return 1 }
  if SystemClock.nanoseconds(&initial) != 500 { return 2 }
  let equal = run Effect.provideMut(MonotonicClock.now(), &mut provider)
  if SystemClock.seconds(&equal) != 10 { return 3 }
  if SystemClock.nanoseconds(&equal) != 500 { return 4 }

  let past = SystemClock.make(10, 400)
  run Effect.provideMut(MonotonicClock.waitUntil(move past), &mut provider)
  if provider.nanoseconds != 500 { return 5 }

  let future = SystemClock.make(11, 25)
  run Effect.provideMut(MonotonicClock.waitUntil(move future), &mut provider)
  run Effect.provideMut(MonotonicClock.waitFor(u64.toU64(0)), &mut provider)
  if provider.seconds != 11 { return 6 }
  if provider.nanoseconds != 25 { return 7 }

  run Effect.provideMut(MonotonicClock.waitFor(u64.toU64(999999975)), &mut provider)
  if provider.seconds != 12 { return 8 }
  if provider.nanoseconds != 0 { return 9 }
  if provider.waits == u64.toU64(4) {} else { return 10 }
  let resolution = run Effect.provideMut(MonotonicClock.getResolution(), &mut provider)
  if resolution != u64.toU64(5) { return 11 }
  return 42
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('keeps the two portable clock requirements independent on direct Wasm', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      `${fixedSystemClock}
${scriptedMonotonicClock}
import silk.effect as Effect

effect fn useBoth() -> i32
? &mut SystemClock.SystemClock | &mut MonotonicClock.MonotonicClock {
  let wall = run SystemClock.now()
  let mark = run MonotonicClock.now()
  if SystemClock.seconds(&wall) != 20 { return 1 }
  if SystemClock.seconds(&mark) != 22 { return 2 }
  return 42
}

pub fn main() -> i32 {
  let mut system = FixedSystemClock {
    seconds: 20,
    nanoseconds: 0,
    resolution: u64.toU64(1)
  }
  let mut monotonic = ScriptedMonotonicClock {
    seconds: 22,
    nanoseconds: 0,
    resolution: u64.toU64(1),
    waits: u64.toU64(0)
  }
  let withSystem = Effect.provideMut<SystemClock.SystemClock>(useBoth(), &mut system)
  return run Effect.provideMut<MonotonicClock.MonotonicClock>(move withSystem, &mut monotonic)
}`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)
