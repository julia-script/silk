import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as InspectorFlowModel from '../src/InspectorFlowModel.js'
import * as InspectorProjectBackend from '../src/InspectorProjectBackend.js'
import * as MirVerification from '../src/MirVerification.js'
import * as MonotonicClockHost from '../src/MonotonicClock.js'
import * as Stdlib from '../src/Stdlib.js'
import * as SystemClockHost from '../src/SystemClock.js'

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

const nativeSystemProgram = `import silk.effect as Effect
import silk.os_system_clock as OsSystemClock
import silk.system_clock as SystemClock
pub fn main() -> i32 {
  let mut provider = OsSystemClock.make()
  let instant = run Effect.provideMut(SystemClock.now(), &mut provider)
  let resolution = run Effect.provideMut(SystemClock.getResolution(), &mut provider)
  if SystemClock.seconds(&instant) != 9007199254740993 { return 1 }
  if SystemClock.nanoseconds(&instant) != 999999999 { return 2 }
  if resolution != 18446744073709551615 { return 3 }
  return 42
}`

const nativeMonotonicWaitProgram = `import silk.effect as Effect
import silk.monotonic_clock as MonotonicClock
import silk.os_monotonic_clock as OsMonotonicClock
pub fn main() -> i32 {
  let mut provider = OsMonotonicClock.make()
  run Effect.provideMut(MonotonicClock.waitFor(0), &mut provider)
  run Effect.provideMut(MonotonicClock.waitFor(200000000), &mut provider)
  run Effect.provideMut(MonotonicClock.waitFor(18446744073709551615), &mut provider)
  return 42
}`

const nativeSystemReadProgram = `import silk.effect as Effect
import silk.os_system_clock as OsSystemClock
import silk.system_clock as SystemClock
pub fn main() -> i32 {
  let mut provider = OsSystemClock.make()
  let instant = run Effect.provideMut(SystemClock.now(), &mut provider)
  return 42
}`

it('keeps fixed system-clock host values exact beyond JavaScript number precision', () => {
  const built = SystemClockHost.fixed(
    { seconds: 9_007_199_254_740_993n, nanoseconds: 999_999_999n },
    18_446_744_073_709_551_615n,
  )
  assert.strictEqual(built._tag, 'Constructed')
  if (built._tag !== 'Constructed') return
  assert.deepEqual(built.value.provider.now(), {
    _tag: 'Read',
    instant: { seconds: 9_007_199_254_740_993n, nanoseconds: 999_999_999n },
  })
  assert.deepEqual(built.value.provider.resolution(), {
    _tag: 'Resolution',
    nanoseconds: 18_446_744_073_709_551_615n,
  })
})

it('returns explicit construction failures outside exact clock scalar ranges', () => {
  const minimum = -(1n << 63n)
  const maximum = (1n << 63n) - 1n
  for (const [seconds, nanoseconds, resolution, reason] of [
    [minimum - 1n, 0n, 1n, 'SecondsOutOfRange'],
    [maximum + 1n, 0n, 1n, 'SecondsOutOfRange'],
    [0n, -1n, 1n, 'NanosecondsOutOfRange'],
    [0n, 1_000_000_000n, 1n, 'NanosecondsOutOfRange'],
    [0n, 0n, 0n, 'ResolutionOutOfRange'],
    [0n, 0n, 1n << 64n, 'ResolutionOutOfRange'],
  ] as const) {
    const built = SystemClockHost.fixed({ seconds, nanoseconds }, resolution)
    assert.strictEqual(built._tag, 'ConstructionFailure')
    if (built._tag === 'ConstructionFailure') assert.strictEqual(built.reason._tag, reason)
  }
  assert.strictEqual(
    SystemClockHost.fixed({ seconds: minimum, nanoseconds: 0n }, 1n)._tag,
    'Constructed',
  )
  assert.strictEqual(
    SystemClockHost.fixed({ seconds: maximum, nanoseconds: 999_999_999n }, (1n << 64n) - 1n)._tag,
    'Constructed',
  )
})

it('validates and advances only a scripted monotonic timeline', () => {
  const built = MonotonicClockHost.scripted(
    [
      { seconds: 10n, nanoseconds: 5n },
      { seconds: 10n, nanoseconds: 5n },
      { seconds: 11n, nanoseconds: 0n },
    ],
    7n,
  )
  assert.strictEqual(built._tag, 'Constructed')
  if (built._tag !== 'Constructed') return
  assert.deepEqual(built.value.provider.now(), {
    _tag: 'Read',
    instant: { seconds: 10n, nanoseconds: 5n },
  })
  assert.deepEqual(built.value.provider.waitUntil({ seconds: 9n, nanoseconds: 0n }), {
    _tag: 'Waited',
  })
  assert.deepEqual(built.value.current(), { seconds: 10n, nanoseconds: 5n })
  assert.deepEqual(built.value.provider.waitUntil({ seconds: 12n, nanoseconds: 25n }), {
    _tag: 'Waited',
  })
  assert.deepEqual(built.value.current(), { seconds: 12n, nanoseconds: 25n })
  assert.deepEqual(built.value.provider.now(), {
    _tag: 'Read',
    instant: { seconds: 12n, nanoseconds: 25n },
  })
  const waits = built.value.waits()
  assert.isTrue(Object.isFrozen(waits))
  assert.deepEqual(waits, [
    {
      _tag: 'WaitUntil',
      deadline: { seconds: 9n, nanoseconds: 0n },
      before: { seconds: 10n, nanoseconds: 5n },
      after: { seconds: 10n, nanoseconds: 5n },
    },
    {
      _tag: 'WaitUntil',
      deadline: { seconds: 12n, nanoseconds: 25n },
      before: { seconds: 10n, nanoseconds: 5n },
      after: { seconds: 12n, nanoseconds: 25n },
    },
  ])
})

it('rejects malformed and decreasing monotonic scripts without throwing', () => {
  assert.strictEqual(MonotonicClockHost.scripted([], 1n)._tag, 'ConstructionFailure')
  assert.strictEqual(
    MonotonicClockHost.scripted([{ seconds: 0n, nanoseconds: -1n }], 1n)._tag,
    'ConstructionFailure',
  )
  assert.strictEqual(
    MonotonicClockHost.scripted([{ seconds: 0n, nanoseconds: 0n }], 0n)._tag,
    'ConstructionFailure',
  )
  const decreasing = MonotonicClockHost.scripted(
    [
      { seconds: 1n, nanoseconds: 0n },
      { seconds: 0n, nanoseconds: 999_999_999n },
    ],
    1n,
  )
  assert.strictEqual(decreasing._tag, 'ConstructionFailure')
  if (decreasing._tag === 'ConstructionFailure') {
    assert.strictEqual(decreasing.reason._tag, 'DecreasingScript')
  }
  assert.deepEqual(SystemClockHost.failing('no wall clock').now(), {
    _tag: 'BoundaryFailure',
    message: 'no wall clock',
  })
  assert.deepEqual(
    MonotonicClockHost.failing('no monotonic clock').waitUntil({ seconds: 0n, nanoseconds: 0n }),
    {
      _tag: 'BoundaryFailure',
      message: 'no monotonic clock',
    },
  )
})

it('registers the clock modules, namespaces, and shared Instant alias', () => {
  assert.strictEqual(Stdlib.findNamespace('SystemClock')?.module, 'silk/system_clock')
  assert.strictEqual(Stdlib.findNamespace('MonotonicClock')?.module, 'silk/monotonic_clock')
  assert.strictEqual(Stdlib.findNamespace('Instant')?.module, 'silk/system_clock')
  assert.strictEqual(Stdlib.findNamespace('OsSystemClock')?.module, 'silk/os_system_clock')
  assert.strictEqual(Stdlib.findNamespace('OsMonotonicClock')?.module, 'silk/os_monotonic_clock')
  assert.deepEqual(Stdlib.find('silk/system_clock')?.aliases, ['Instant'])
})

it.effect('constructs unused native providers on direct Wasm without retaining host calls', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      `import silk.os_monotonic_clock as OsMonotonicClock
import silk.os_system_clock as OsSystemClock
pub fn main() -> i32 {
  let system = OsSystemClock.make()
  let monotonic = OsMonotonicClock.make()
  drop system
  drop monotonic
  return 42
}`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      Analysis.loweredMir(self)
        .functions.flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'OsCall'),
      [],
    )
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('lowers each native clock provider only to its own intrinsic operations', () =>
  Effect.gen(function* () {
    const system = yield* snapshot(`import silk.effect as Effect
import silk.os_system_clock as OsSystemClock
import silk.system_clock as SystemClock
import silk.i64 as i64
pub fn main() -> i32 {
  let mut provider = OsSystemClock.make()
  let instant = run Effect.provideMut(SystemClock.now(), &mut provider)
  let resolution = run Effect.provideMut(SystemClock.getResolution(), &mut provider)
  if resolution > 0 { return i64.toI32(SystemClock.seconds(&instant)) }
  return 0
}`)
    assert.deepEqual(Analysis.diagnostics(system), [])
    const systemCalls = Analysis.loweredMir(system)
      .functions.flatMap(MirVerification.operations)
      .filter((operation) => operation._tag === 'OsCall')
      .map((operation) => operation.operation.name)
    assert.deepEqual([...new Set(systemCalls)].sort(), [
      'osSystemClockNow',
      'osSystemClockResolution',
    ])

    const monotonic = yield* snapshot(`import silk.effect as Effect
import silk.monotonic_clock as MonotonicClock
import silk.os_monotonic_clock as OsMonotonicClock
pub fn main() -> i32 {
  let mut provider = OsMonotonicClock.make()
  run Effect.provideMut(MonotonicClock.waitFor(18446744073709551615), &mut provider)
  let resolution = run Effect.provideMut(MonotonicClock.getResolution(), &mut provider)
  if resolution > 0 { return 42 }
  return 0
}`)
    assert.deepEqual(Analysis.diagnostics(monotonic), [])
    const monotonicCalls = Analysis.loweredMir(monotonic)
      .functions.flatMap(MirVerification.operations)
      .filter((operation) => operation._tag === 'OsCall')
      .map((operation) => operation.operation.name)
    assert.deepEqual([...new Set(monotonicCalls)].sort(), [
      'osMonotonicClockNow',
      'osMonotonicClockResolution',
      'osMonotonicClockWaitUntil',
    ])
  }),
)

it.effect('executes exact native system-clock values through an injected evaluator host', () =>
  Effect.gen(function* () {
    const built = SystemClockHost.fixed(
      { seconds: 9_007_199_254_740_993n, nanoseconds: 999_999_999n },
      18_446_744_073_709_551_615n,
    )
    assert.strictEqual(built._tag, 'Constructed')
    if (built._tag !== 'Constructed') return
    const self = yield* snapshot(nativeSystemProgram)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self, { systemClock: built.value.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.deepEqual(
      outcome.trace
        .filter((event) => event._tag === 'OsCall')
        .map((event) => [event.operation.name, event.outcome]),
      [
        ['osSystemClockNow', 'Completed'],
        ['osSystemClockResolution', 'Completed'],
      ],
    )
  }),
)

it.effect('derives zero, carry, and maximum monotonic waits from one read each', () =>
  Effect.gen(function* () {
    const built = MonotonicClockHost.scripted(
      [
        { seconds: 1n, nanoseconds: 900_000_000n },
        { seconds: 1n, nanoseconds: 900_000_000n },
        { seconds: 2n, nanoseconds: 100_000_000n },
      ],
      1n,
    )
    assert.strictEqual(built._tag, 'Constructed')
    if (built._tag !== 'Constructed') return
    const self = yield* snapshot(nativeMonotonicWaitProgram)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self, { monotonicClock: built.value.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.deepEqual(
      built.value.waits().map((event) => event.deadline),
      [
        { seconds: 1n, nanoseconds: 900_000_000n },
        { seconds: 2n, nanoseconds: 100_000_000n },
        { seconds: 18_446_744_075n, nanoseconds: 809_551_615n },
      ],
    )
    assert.deepEqual(
      outcome.trace.filter((event) => event._tag === 'OsCall').map((event) => event.operation.name),
      [
        'osMonotonicClockNow',
        'osMonotonicClockWaitUntil',
        'osMonotonicClockNow',
        'osMonotonicClockWaitUntil',
        'osMonotonicClockNow',
        'osMonotonicClockWaitUntil',
      ],
    )
  }),
)

it.effect('traps on a source-derived monotonic deadline overflow before waiting', () =>
  Effect.gen(function* () {
    const built = MonotonicClockHost.scripted(
      [{ seconds: (1n << 63n) - 1n, nanoseconds: 999_999_999n }],
      1n,
    )
    assert.strictEqual(built._tag, 'Constructed')
    if (built._tag !== 'Constructed') return
    const self = yield* snapshot(`import silk.effect as Effect
import silk.monotonic_clock as MonotonicClock
import silk.os_monotonic_clock as OsMonotonicClock
pub fn main() -> i32 {
  let mut provider = OsMonotonicClock.make()
  run Effect.provideMut(MonotonicClock.waitFor(1), &mut provider)
  return 42
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self, { monotonicClock: built.value.provider })
    assert.strictEqual(outcome._tag, 'Trap')
    assert.deepEqual(built.value.waits(), [])
    assert.deepEqual(
      outcome.trace.filter((event) => event._tag === 'OsCall').map((event) => event.operation.name),
      ['osMonotonicClockNow'],
    )
  }),
)

it.effect(
  'maps explicit, malformed, and thrown clock host results to false without partial output',
  () =>
    Effect.gen(function* () {
      const self = yield* snapshot(nativeSystemReadProgram)
      assert.deepEqual(Analysis.diagnostics(self), [])

      const explicit = Analysis.evaluate(self, { systemClock: SystemClockHost.failing('offline') })
      assert.strictEqual(explicit._tag, 'Trap')
      assert.deepEqual(
        explicit.trace
          .filter((event) => event._tag === 'OsCall')
          .map((event) => [event.outcome, event.cause]),
        [['Failure', undefined]],
      )

      const malformed: SystemClockHost.Provider = Object.freeze({
        now: () =>
          Object.freeze({
            _tag: 'Read',
            instant: Object.freeze({ seconds: 0n, nanoseconds: 1_000_000_000n }),
          }),
        resolution: () => Object.freeze({ _tag: 'Resolution', nanoseconds: 1n }),
      })
      const rejected = Analysis.evaluate(self, { systemClock: malformed })
      assert.strictEqual(rejected._tag, 'Trap')
      assert.deepEqual(
        rejected.trace.filter((event) => event._tag === 'OsCall').map((event) => event.outcome),
        ['Failure'],
      )

      const cause = Object.freeze({ reason: 'clock exploded' })
      const throwing: SystemClockHost.Provider = Object.freeze({
        now: () => {
          throw cause
        },
        resolution: () => Object.freeze({ _tag: 'Resolution', nanoseconds: 1n }),
      })
      const thrown = Analysis.evaluate(self, { systemClock: throwing })
      assert.strictEqual(thrown._tag, 'Trap')
      assert.deepEqual(
        thrown.trace
          .filter((event) => event._tag === 'OsCall')
          .map((event) => [event.outcome, event.cause]),
        [['Failure', cause]],
      )
    }),
)

it.effect('blocks only the absent matching clock host after retaining preceding trace', () =>
  Effect.gen(function* () {
    const system = SystemClockHost.fixed({ seconds: 20n, nanoseconds: 0n }, 1n)
    assert.strictEqual(system._tag, 'Constructed')
    if (system._tag !== 'Constructed') return
    const self = yield* snapshot(`import silk.effect as Effect
import silk.monotonic_clock as MonotonicClock
import silk.os_monotonic_clock as OsMonotonicClock
import silk.os_system_clock as OsSystemClock
import silk.system_clock as SystemClock
fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let mut system = OsSystemClock.make()
  let wall = run Effect.provideMut(SystemClock.now(), &mut system)
  let mut monotonic = OsMonotonicClock.make()
  let mark = run Effect.provideMut(MonotonicClock.now(), &mut monotonic)
  return identity(42)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self, { systemClock: system.value.provider })
    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'MissingMonotonicClock')
    assert.deepEqual(
      outcome.trace
        .filter((event) => event._tag === 'OsCall')
        .map((event) => [event.operation.name, event.outcome]),
      [['osSystemClockNow', 'Completed']],
    )
    const flow = InspectorFlowModel.projectDataFlow(Analysis.rootAnalysis(self), outcome)
    assert.isTrue(
      flow.nodes.some(
        (node) => node.kind === 'Terminal' && node.detail.includes('MissingMonotonicClock'),
      ),
    )
    assert.isTrue(
      InspectorProjectBackend.evaluationRows(outcome).some(
        (row) => row.detail === 'missing MonotonicClock host provider',
      ),
    )

    const missingSystem = Analysis.evaluate(yield* snapshot(nativeSystemReadProgram))
    assert.strictEqual(missingSystem._tag, 'Blocked')
    if (missingSystem._tag === 'Blocked') {
      assert.strictEqual(missingSystem.reason._tag, 'MissingSystemClock')
    }
    assert.isTrue(
      InspectorProjectBackend.evaluationRows(missingSystem).some(
        (row) => row.detail === 'missing SystemClock host provider',
      ),
    )
  }),
)

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
