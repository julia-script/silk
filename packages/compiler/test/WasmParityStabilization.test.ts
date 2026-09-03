import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

/**
 * Engine-parity regressions for the direct WebAssembly backend (REQ-23): the evaluator is the
 * oracle, and every program here once diverged on Wasm (and, where noted, on native).
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-wasm-parity-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const evaluated = (snapshot: Analysis.Snapshot): bigint => {
  const outcome = Analysis.evaluate(snapshot)
  assert.strictEqual(outcome._tag, 'Completed')
  return outcome._tag === 'Completed' ? outcome.result.value : -1n
}

const runWasm = Effect.fnUntraced(function* (snapshot: Analysis.Snapshot) {
  const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
  return (instance.exports.silk_main as () => number)()
})

const runNative = Effect.fnUntraced(function* (name: string, source: string) {
  const outcome = yield* Driver.compile({
    compilation: { root: SourceFile.make('memory/driver', ascii(source)) },
    toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
    profile: 'release',
    destination: join(destinationRoot, name.replace(/[^a-z0-9]/gi, '-')),
    cache: false,
  }).pipe(Effect.provide(SourceResolver.empty))
  assert.strictEqual(outcome._tag, 'Compiled')
  if (outcome._tag !== 'Compiled') return -1
  return spawnSync(outcome.path, [], { encoding: 'utf8' }).status
})

const agree = (
  name: string,
  source: string,
  expected: number,
  engines: 'wasm' | 'wasm+native' = 'wasm',
) =>
  it.effect(
    name,
    () =>
      Effect.gen(function* () {
        const snapshot = yield* Analysis.ofSourceRealized(
          'memory/driver',
          ascii(source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [])
        assert.strictEqual(evaluated(snapshot), BigInt(expected))
        assert.strictEqual(yield* runWasm(snapshot), expected)
        if (engines === 'wasm+native')
          assert.strictEqual(yield* runNative(name, source), expected & 0xff)
      }),
    120_000,
  )

// ISSUE-48: a write through an exclusive view must reach the owner it aliases, whether the view
// is returned from a call or bound directly.
agree(
  'applies a write through a returned exclusive slice view to its array owner',
  `fn identityMut(values: &mut [i32]) -> &mut [i32] { return values }
pub fn main() -> i32 {
  let mut v = [1, 2]
  let m = identityMut(&mut v)
  m[1] = 40
  return v[1] + 2
}`,
  42,
  'wasm+native',
)
agree(
  'applies a write through a returned exclusive field view to its struct owner',
  `struct Inner { v: i32 }
struct Outer { inner: Inner }
fn innerMut(o: &mut Outer) -> &mut Inner { return &mut o.inner }
pub fn main() -> i32 {
  let mut o = Outer { inner: Inner { v: 1 } }
  let im = innerMut(&mut o)
  im.v = 42
  return o.inner.v
}`,
  42,
  'wasm+native',
)
agree(
  'applies a write through a directly bound exclusive view to its owner',
  `pub fn main() -> i32 {
  let mut v = [1, 2]
  let m = &mut v
  m[1] = 40
  return v[1] + 2
}`,
  42,
  'wasm+native',
)

// ISSUE-98: passing the array by value hands out lanes, not an address, so the call must not
// reload the never-materialized frame root.
agree(
  'reads real elements through a slice borrowed after the array was passed by value',
  `fn read(values: [i32; 3]) -> i32 { return values[2] }
fn readSlice(values: &[i32]) -> i32 { return values[0] }
pub fn main() -> i32 {
  let v = [10, 20, 30]
  let a = read(v)
  let b = readSlice(&v)
  return b * 1000 + a
}`,
  10030,
)

// ISSUE-99: an unsigned sub-word subtraction that borrows clamps to zero, not to the maximum.
agree(
  'clamps u8.saturatingSubtract at zero',
  `import silk.u8 as u8
fn f(a: u8, b: u8) -> u8 { return u8.saturatingSubtract(a, b) }
pub fn main() -> i32 { return u8.toI32(f(0, 1)) + u8.toI32(u8.saturatingSubtract(0, 1)) }`,
  0,
)
agree(
  'clamps u16.saturatingSubtract at zero',
  `import silk.u16 as u16
pub fn main() -> i32 { return u16.toI32(u16.saturatingSubtract(0, 1)) }`,
  0,
)

// ISSUE-79: the pending child's transfer payload carries the borrow into the root a relay moves
// into its coroutine frame, so the child must write where the resumed parent reads.
agree(
  'keeps a mutation made through an exclusive loan inside a suspended child',
  `import silk.effect { Effect }
struct Record { count: i32 }
effect fn bumpNamed(value: &mut Record) -> () {
  value.count = value.count + 1
  return ()
}
effect fn bump(value: &mut Record) -> () {
  run Effect.suspend(bumpNamed(move value))
  return ()
}
pub fn main() -> i32 {
  let mut record = Record { count: 10 }
  run bump(&mut record)
  return record.count
}`,
  11,
)
agree(
  'keeps mutations made through an exclusive loan across a chain of suspended children',
  `import silk.effect { Effect }
struct Record { count: i32 }
effect fn bump(value: &mut Record) -> () {
  run Effect.suspend(effect { value.count = value.count + 1 return () })
  return ()
}
effect fn deep(value: &mut Record, n: i32) -> () {
  if n == 0 { return () }
  value.count = value.count + 1
  run Effect.suspend(deep(move value, n - 1))
  return ()
}
pub fn main() -> i32 {
  let mut record = Record { count: 10 }
  run bump(&mut record)
  let afterBump = record.count
  run deep(&mut record, 100)
  if afterBump != 11 { return 1 }
  return record.count
}`,
  111,
)

// ISSUE-85: the registration guard returned by the park callback drops when the parked body
// resumes, before it continues.
agree(
  'drops the Execution.park registration guard before the resumed body continues',
  `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
import silk.shared { Shared }
struct Log { entries: i32 }
fn record(self: &mut Log, id: i32) -> () { self.entries = self.entries * 10 + id return () }
fn readLog(self: &mut Log) -> i32 { return self.entries }
struct Guard { log: Shared<Log> }
impl Drop for Guard { fn drop(self: &mut Guard) -> () { Shared.withMut(&self.log, record(7)) return () } }
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 log: Shared<Log> }
fn register(wake: Intrinsic.Wake, log: Shared<Log>) -> Guard {
  Intrinsic.wake(move wake)
  Shared.withMut(&log, record(2))
  return Guard { log: move log }
}
effect fn body(log: Shared<Log>) -> i32 {
  run Execution.park(register(Shared.clone(&log)))
  Shared.withMut(&log, record(3))
  run Execution.park(register(Shared.clone(&log)))
  Shared.withMut(&log, record(4))
  drop log
  return 42
}
fn complete(owner: &mut Owner, result: i32) -> () {
  owner.result = result
  Shared.withMut(&owner.log, record(6))
  return ()
}
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  Shared.withMut(&owner.log, record(5))
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
fn ready(state: &Shared<Log>) -> () { return () }
effect fn driveOnce(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn driveStored(selected: Empty | Stored, owner: &mut Owner) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution: next } => run driveOnce(move next, move owner)
  }
}
effect fn program() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let log = run Shared.make<Log>(Log { entries: 0 }) |> Effect.provideMut<Allocator>(&mut allocator)
  let mut owner = Owner { slot: Empty {}, result: 0, log: Shared.clone(&log) }
  let execution = run Execution.make(body(Shared.clone(&log)), Shared.clone(&log), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run driveOnce(move execution, &mut owner)
  let first = Intrinsic.replace(owner.slot, Empty {})
  run driveStored(move first, &mut owner)
  let second = Intrinsic.replace(owner.slot, Empty {})
  run driveStored(move second, &mut owner)
  let Owner { slot, result, log: ownerLog } = move owner
  drop slot
  drop ownerLog
  let entries = Shared.withMut(&log, readLog)
  drop log
  if result != 42 { return -1 }
  return entries
}
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`,
  257325746,
)

// ISSUE-44: a shared handle that only ever appears as a union alternative still lays out the
// allocation its control block embeds.
agree(
  'emits a Shared handle stored in a union alternative',
  `import silk.shared { Shared }
struct Empty {}
fn make() -> Empty | Shared<i32> { return Empty {} }
pub fn main() -> i32 {
  let o = make()
  return 42
}`,
  42,
  'wasm+native',
)
agree(
  'emits an absent Option of a Shared handle',
  `import silk.option { Option }
import silk.shared { Shared }
pub fn main() -> i32 {
  let o = Option.none<Shared<i32>>()
  return 42
}`,
  42,
  'wasm+native',
)

// ISSUE-90: a raw pointer access is reason enough for private memory, and a null dereference
// traps on Wasm exactly as the evaluator does.
it.effect('emits a program whose only raw access is a null pointer read, and traps on it', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'memory/driver',
      ascii(`import silk.pointer { Pointer }
pub fn main() -> i32 {
  let null = Pointer.null<i32>()
  let value = unsafe Pointer.read(null)
  return value
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(Analysis.evaluate(snapshot)._tag, 'Trap')
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.throws(() => (instance.exports.silk_main as () => number)(), WebAssembly.RuntimeError)
  }),
)

// ISSUE-86: a resumed invocation puts every address-taken root it restores by value back into
// its private frame slot; the parent's Shared handle must survive two child fibers touching the
// same log, with or without timer waits.
const sharedLogScheduler = (children: string) =>
  `${children}
effect fn program(log: Shared<Log>) -> i32
! OutOfMemoryError | TaskIdExhaustedError | Cancelled
? &mut Scheduler | &mut MonotonicClock {
  let a = run Fiber.forkChild<i32, never>(child(Shared.clone(&log), 1, 20))
  let b = run Fiber.forkChild<i32, never>(child(Shared.clone(&log), 2, 10))
  let ra = run Fiber.join<i32, never>(move a)
  let rb = run Fiber.join<i32, never>(move b)
  let entries = Shared.withMut(&log, readLog)
  drop log
  return entries
}
` +
  `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.fiber { Fiber, Cancelled }
import silk.local_scheduler { LocalScheduler, StalledError }
import silk.monotonic_clock { MonotonicClock }
import silk.scheduler { Scheduler, TaskIdExhaustedError }
import silk.shared { Shared }
import silk.system_clock { SystemClock }
import silk.system_clock { Instant }

struct ParentClock { mark: Instant }
effect fn parentNow(self: &mut ParentClock) -> Instant {
  return SystemClock.make(SystemClock.seconds(&self.mark), SystemClock.nanoseconds(&self.mark))
}
effect fn parentResolution(self: &mut ParentClock) -> u64 { return 1 }
effect fn parentWaitUntil(self: &mut ParentClock, deadline: Instant) -> () {
  self.mark = move deadline
  return ()
}
effect fn parentWaitFor(self: &mut ParentClock, duration: u64) -> () {
  let deadline = MonotonicClock.deadlineAfter(&self.mark, duration)
  return run parentWaitUntil(move self, move deadline)
}
impl MonotonicClock for ParentClock {
  now: ParentClock.parentNow
  getResolution: ParentClock.parentResolution
  waitUntil: ParentClock.parentWaitUntil
  waitFor: ParentClock.parentWaitFor
}
struct Log { entries: i32 }
fn record(self: &mut Log, id: i32) -> () { self.entries = self.entries * 10 + id return () }
fn readLog(self: &mut Log) -> i32 { return self.entries }
effect fn recover(error: OutOfMemoryError | TaskIdExhaustedError | Cancelled | StalledError) -> i32 {
  return match move error {
    StalledError {} => 94
    OutOfMemoryError {} => 91
    TaskIdExhaustedError {} => 92
    Cancelled {} => 93
  }
}
effect fn run2() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let log = run Shared.make<Log>(Log { entries: 0 }) |> Effect.provideMut<Allocator>(&mut allocator)
  let mut scheduler = LocalScheduler.make()
  let mut clock = ParentClock { mark: SystemClock.make(0, 0) }
  let scheduled = Effect.catchAll(LocalScheduler.execute(&mut scheduler, program(move log)), recover)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
  return run move scheduled
}
effect fn recoverOom(error: OutOfMemoryError) -> i32 { return 99 }
pub fn main() -> i32 { return run Effect.catchAll(run2(), recoverOom) }
`
agree(
  'keeps the parent Shared handle across two forked children mutating a shared log',
  sharedLogScheduler(`effect fn child(log: Shared<Log>, id: i32, duration: u64) -> i32 {
  Shared.withMut(&log, record(id))
  drop log
  return id
}`),
  12,
  'wasm+native',
)
agree(
  'keeps the parent Shared handle across two timer-waiting children mutating a shared log',
  sharedLogScheduler(`effect fn child(log: Shared<Log>, id: i32, duration: u64) -> i32
? &mut MonotonicClock {
  run MonotonicClock.waitFor(duration)
  Shared.withMut(&log, record(id))
  drop log
  return id
}`),
  21,
  'wasm+native',
)
