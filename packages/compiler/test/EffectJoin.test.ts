import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as WasmBackend from '../src/WasmBackend.js'
import * as Projections from './support/projections.js'
import * as WasmMain from './support/WasmMain.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct First {}
struct Second {}
fn choose(input: First | Second) -> Effect<i32> {
  return match move input {
    First {} => effect { return 41 }
    Second {} => effect { return 42 }
  }
}
pub fn main() -> i32 {
  return run choose(First {})
}`

const branchSource = `fn choose(flag: bool) -> Effect<i32> {
  if flag { return effect { return 41 } }
  return effect { return 42 }
}
pub fn main() -> i32 { return run choose(false) }`

const captureSource = `struct First {}
struct Second {}
fn choose(input: First | Second, integer: i32, decimal: f64) -> Effect<i32> {
  return match move input {
    First {} => effect { return integer }
    Second {} => effect {
      drop decimal
      return 42
    }
  }
}
pub fn main() -> i32 {
  let first = run choose(First {}, 41, 0.0)
  let second = run choose(Second {}, 0, 42.0)
  return first + second
}`

const failureSource = `struct First {}
struct Second {}
struct FirstError { code: i32 }
struct SecondError { code: i32 }
fn choose(input: First | Second) -> Effect<i32 ! FirstError | SecondError> {
  return match move input {
    First {} => effect {
      if 1 == 1 { fail FirstError { code: 41 } }
      return 0
    }
    Second {} => effect {
      if 1 == 1 { fail SecondError { code: 42 } }
      return 0
    }
  }
}
pub fn main() -> i32 {
  let selected = choose(Second {})
  drop selected
  return 42
}`

const selectedFailureSource = `struct First {}
struct Second {}
pub struct FirstError { code: i32 }
pub struct SecondError { code: i32 }
fn choose(input: First | Second) -> Effect<i32 ! FirstError | SecondError> {
  return match move input {
    First {} => effect {
      if 1 == 1 { fail FirstError { code: 41 } }
      return 0
    }
    Second {} => effect {
      if 1 == 1 { fail SecondError { code: 42 } }
      return 0
    }
  }
}
pub effect fn main() -> () ! FirstError | SecondError {
  let value = run choose(Second {})
  drop value
  return ()
}`

const requirementSource = `import silk.effect as Effect
service LeftClock {
  effect fn read() -> i32 ? &LeftClock
}
service RightClock {
  effect fn read() -> i32 ? &RightClock
}
struct Left { value: i32 }
struct Right { value: i32 }
effect fn readLeft(self: &Left) -> i32 { return self.value }
effect fn readRight(self: &Right) -> i32 { return self.value }
impl LeftClock for Left { read: Left.readLeft }
impl RightClock for Right { read: Right.readRight }
effect fn useLeft() -> i32 ? &LeftClock { return run LeftClock.read() }
effect fn useRight() -> i32 ? &RightClock { return run RightClock.read() }
struct First {}
struct Second {}
fn choose(input: First | Second) -> Effect<i32 ? &LeftClock | &RightClock> {
  return match move input {
    First {} => useLeft()
    Second {} => useRight()
  }
}
pub fn main() -> i32 {
  let left = Left { value: 41 }
  let right = Right { value: 42 }
  let selected = choose(Second {})
    |> Effect.provide<LeftClock>(&left)
    |> Effect.provide<RightClock>(&right)
  return run selected
}`

const affineCaptureSource = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct First {}
struct Second {}
fn choose(input: First | Second, storage: Allocation) -> once Effect<i32> {
  return match move input {
    First {} => effect {
      drop move storage
      return 41
    }
    Second {} => effect {
      drop move storage
      return 42
    }
  }
}
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let layout = Layout.of<i32>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let storage = run recipe
  let selected = choose(First {}, move storage)
  drop selected
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

const accessSource = `struct First {}
struct Second {}
struct Counter { value: i32 }
fn choose(input: First | Second, counter: &mut Counter) -> mut Effect<i32> {
  return match move input {
    First {} => effect { return 41 }
    Second {} => effect {
      counter.value = counter.value + 1
      return counter.value
    }
  }
}
pub fn main() -> i32 {
  let mut counter = Counter { value: 41 }
  let pending = choose(Second {}, &mut counter)
  return run pending
}`

const hookCaptureSource = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct First {}
struct Second {}
struct Guard { storage: Allocation }
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () { return () }
}
fn choose(input: First | Second, guard: Guard) -> once Effect<i32> {
  return match move input {
    First {} => effect {
      drop move guard
      return 41
    }
    Second {} => effect {
      drop move guard
      return 42
    }
  }
}
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let layout = Layout.of<i32>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let storage = run recipe
  let selected = choose(Second {}, Guard { storage: move storage })
  drop selected
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

const nonFiniteSource = `struct First {}
struct Second {}
fn choose<F: Effect<i32>>(input: First | Second, operation: F) -> Effect<i32> {
  return match move input {
    First {} => operation
    Second {} => effect { return 42 }
  }
}`

const snapshotOf = (name: string, text: string) =>
  Analysis.ofSourceRealized(name, ascii(text), 'wasm32-unknown-unknown')

const runWasm = Effect.fnUntraced(function* (snapshot: Analysis.Snapshot, operation: string) {
  const program = Analysis.loweredMir(snapshot)
  assert.deepEqual(MirVerification.verify(program), [])
  const wasm = yield* Backend.emit(WasmBackend.WasmBackend, program, { mode: 'release' })
  return yield* WasmMain.invoke(wasm.bytes, operation)
})

it.effect('runs the selected member of a finite Effect join', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/basic', source)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [], MirEncoding.encode(program))
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Trap' ? outcome.reason : undefined,
    )
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 41n)
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.run'), 41)
  }),
)

it.effect('emits the same allocation-free composite artifact repeatedly', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/determinism', source)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    const first = yield* Backend.emit(WasmBackend.WasmBackend, program, { mode: 'release' })
    const second = yield* Backend.emit(WasmBackend.WasmBackend, program, { mode: 'release' })
    assert.deepEqual(first.bytes, second.bytes)
  }),
)

it.effect('joins Effects returned from distinct finite control-flow branches', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/branches', branchSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [], MirEncoding.encode(program))
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Trap' ? outcome.reason : undefined,
    )
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.branches'), 42)
  }),
)

it.effect('preserves the selected alternative capture shape', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/captures', captureSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [], MirEncoding.encode(program))
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Trap' ? outcome.reason : undefined,
    )
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 83n)
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.captures'), 83)
  }),
)

it.effect('normalizes distinct failure channels on a value that can be dropped', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/failures', failureSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Trap' ? outcome.reason : undefined,
    )
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.failures'), 42)
  }),
)

it.effect('propagates the selected alternative failure', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/selected-failure', selectedFailureSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [], MirEncoding.encode(program))
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'UnhandledFailure')
    if (outcome._tag === 'UnhandledFailure') {
      assert.strictEqual(
        outcome.identity,
        'effect-join/selected-failure.SecondError',
        MirEncoding.encode(program),
      )
    }
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.selectedFailure'), 1)
  }),
)

it.effect('cleans up only the selected alternative capture', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/affine-capture', affineCaptureSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Trap' ? outcome.reason : undefined,
    )
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 42n)
      assert.deepEqual(
        Projections.allocationTraceEventsOf(outcome).map((event) => event._tag),
        ['AllocationAcquire', 'AllocationRelease'],
      )
    }
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.affineCapture'), 42)
  }),
)

it.effect('joins shared and exclusive capture access at the stronger access', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/access', accessSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [], MirEncoding.encode(program))
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Trap' ? outcome.reason : undefined,
    )
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.access'), 42)
  }),
)

it.effect('runs the selected capture Drop hook and reclaim exactly once', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/hook-capture', hookCaptureSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Trap' ? outcome.reason : undefined,
    )
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 42n)
      assert.strictEqual(
        outcome.trace.filter(
          (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl#'),
        ).length,
        1,
      )
      assert.deepEqual(
        Projections.allocationTraceEventsOf(outcome).map((event) => event._tag),
        ['AllocationAcquire', 'AllocationRelease'],
      )
    }
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.hookCapture'), 42)
  }),
)

it.effect('diagnoses a join whose representation is not a closed finite set', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/non-finite', nonFiniteSource)
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0132'],
    )
  }),
)

it.effect('normalizes requirements while passing only the selected alternative providers', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/requirements', requirementSource)
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(yield* runWasm(snapshot, 'EffectJoin.requirements'), 42)
  }),
)
