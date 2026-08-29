import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ExecutionPackage from '../src/ExecutionPackage.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SuspensionMode from '../src/SuspensionMode.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'
import { ordinaryStorageSource } from './support/ordinaryStorageSource.js'

const specialization = (suspension: SuspensionMode.Summary): ExecutionPackage.Specialization =>
  Object.freeze({
    result: 'i32',
    body: Type.effect('i32', Object.freeze([]), 'Take'),
    endpoint: Type.unit,
    callback: Type.callable(Object.freeze([Type.reference('Shared', Type.unit)]), Type.unit),
    suspension,
  })

const exactCleanupProgram = (
  owner: 'Body' | 'Endpoint',
): string => `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.layout { Layout }
struct Guard { storage: Allocation }
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () { let observable = 1 / 0 return () }
}
struct Ready { guard: Guard }
fn ready(state: &Ready) -> () { return () }
fn readyUnit(state: &()) -> () { return () }
fn complete(state: (), value: i32) -> () { return () }
fn suspend(state: (), execution: Intrinsic.Execution<i32>) -> () {
  drop execution
  return ()
}
effect fn body(guard: Guard) -> i32 { return 42 }
effect fn packaged() -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let cleanupLayout = Layout.of<i32>()
  let cleanupStorage = run Allocator.allocate(move cleanupLayout)
  ${
    owner === 'Body'
      ? `let execution = run Execution.make(
    body(Guard { storage: move cleanupStorage }),
    (),
    readyUnit
  )
  drop execution
  return ()`
      : `let execution = run Execution.make(
    effect { return 42 },
    Ready { guard: Guard { storage: move cleanupStorage } },
    ready
  )
  return run Execution.drive(move execution, (), complete, suspend)`
  }
}
effect fn program() -> () ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run packaged() |> Effect.provideMut<Allocator>(&mut allocator)
}
effect fn recover(error: Allocator.OutOfMemoryError) -> () { return () }
pub fn main() -> () { return run Effect.catchAll(program(), recover) }`

const nominalUnionExecutionCleanup = `import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.i8 as i8
import silk.layout { Layout }

struct Guard {
  left: i8
  right: i8
  storage: Allocation
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    let observed = i8.toI32(self.left) + i8.toI32(self.right)
    if observed != 42 {
      let boom = 1 / 0
    }
    return ()
  }
}

union Ready {
  Small { marker: i8, guard: Guard },
  Wide { value: i64 }
}

fn ready(state: &Ready) -> () { return () }

effect fn packaged() -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let cleanupLayout = Layout.of<i32>()
  let cleanupStorage = run Allocator.allocate(move cleanupLayout)
  let state = Ready.Small {
    marker: i8.toI8(7),
    guard: Guard {
      left: i8.toI8(19),
      right: i8.toI8(23),
      storage: move cleanupStorage
    }
  }
  let execution = run Execution.make(effect { return 42 }, move state, ready)
  drop execution
  return ()
}

effect fn program() -> () ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run packaged() |> Effect.provideMut<Allocator>(&mut allocator)
}

effect fn recover(error: Allocator.OutOfMemoryError) -> () { return () }

pub fn main() -> () { return run Effect.catchAll(program(), recover) }`

const nominalUnionDriveCallbackCleanup = `import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.i8 as i8
import silk.layout { Layout }

struct Guard {
  left: i8
  right: i8
  storage: Allocation
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    let observed = i8.toI32(self.left) + i8.toI32(self.right)
    if observed != 42 {
      let boom = 1 / 0
    }
    return ()
  }
}

union Choice {
  Small { marker: i8, guard: Guard },
  Wide { value: i64 }
}

fn ready(state: &()) -> () { return () }
fn complete(state: (), value: i32) -> () { return () }

fn suspend(state: (), execution: Intrinsic.Execution<i32>, choice: Choice) -> () {
  drop execution
  drop choice
  return ()
}

fn suspendWith(choice: Choice) -> some<F: once fn((), Intrinsic.Execution<i32>) -> ()> F {
  return suspend(move choice)
}

effect fn packaged() -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout)
  let choice = Choice.Small {
    marker: i8.toI8(7),
    guard: Guard {
      left: i8.toI8(19),
      right: i8.toI8(23),
      storage: move storage
    }
  }
  let execution = run Execution.make(effect { return 42 }, (), ready)
  return run Execution.drive(move execution, (), complete, suspendWith(move choice))
}

effect fn program() -> () ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run packaged() |> Effect.provideMut<Allocator>(&mut allocator)
}

effect fn recover(error: Allocator.OutOfMemoryError) -> () { return () }

pub fn main() -> () { return run Effect.catchAll(program(), recover) }`

const nominalUnionSeparatedDriveCleanup = `import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.i8 as i8
import silk.layout { Layout }

struct Guard {
  left: i8
  right: i8
  storage: Allocation
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    let observed = i8.toI32(self.left) + i8.toI32(self.right)
    if observed != 42 {
      let boom = 1 / 0
    }
    return ()
  }
}

union Ready {
  Small { marker: i8, guard: Guard },
  Wide { value: i64 }
}

fn ready(state: &Ready) -> () { return () }
fn complete(state: (), value: i32) -> () { return () }

fn suspend(state: (), execution: Intrinsic.Execution<i32>) -> () {
  drop execution
  return ()
}

effect fn makeOne() -> Intrinsic.Execution<i32> ! Allocator.OutOfMemoryError ? &mut Allocator {
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout)
  let state = Ready.Small {
    marker: i8.toI8(7),
    guard: Guard {
      left: i8.toI8(19),
      right: i8.toI8(23),
      storage: move storage
    }
  }
  return run Execution.make(effect { return 42 }, move state, ready)
}

effect fn driveOne(execution: Intrinsic.Execution<i32>) -> () {
  return run Execution.drive(move execution, (), complete, suspend)
}

effect fn packaged() -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let execution = run makeOne()
  return run driveOne(move execution)
}

effect fn program() -> () ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run packaged() |> Effect.provideMut<Allocator>(&mut allocator)
}

effect fn recover(error: Allocator.OutOfMemoryError) -> () { return () }

pub fn main() -> () { return run Effect.catchAll(program(), recover) }`

it('plans exact direct, nested, and externally parkable combined packages', () => {
  const target = Target.wasm32UnknownUnknown
  const layouts = Object.freeze({
    body: Object.freeze({ size: 8, alignment: 4 }),
    endpoint: Object.freeze({ size: 0, alignment: 1 }),
    callback: Object.freeze({ size: 0, alignment: 1 }),
  })
  const direct = ExecutionPackage.plan(target, specialization(SuspensionMode.direct), layouts)
  const nested = ExecutionPackage.plan(
    target,
    specialization(SuspensionMode.openExecutable(Object.freeze(['Intrinsic.NonParking']))),
    layouts,
  )
  const external = ExecutionPackage.plan(
    target,
    specialization(SuspensionMode.openExecutable(Object.freeze([]))),
    layouts,
  )
  assert.strictEqual(direct._tag, 'ExecutionPackagePlan')
  assert.strictEqual(nested._tag, 'ExecutionPackagePlan')
  assert.strictEqual(external._tag, 'ExecutionPackagePlan')
  if (
    direct._tag !== 'ExecutionPackagePlan' ||
    nested._tag !== 'ExecutionPackagePlan' ||
    external._tag !== 'ExecutionPackagePlan'
  )
    return
  assert.isFalse(direct.readinessStorage)
  assert.isFalse(direct.initialContinuationSegment)
  assert.isFalse(nested.readinessStorage)
  assert.isTrue(nested.initialContinuationSegment)
  assert.isTrue(external.readinessStorage)
  assert.isTrue(external.initialContinuationSegment)
  assert.notStrictEqual(direct.provenance, nested.provenance)
  assert.notStrictEqual(nested.provenance, external.provenance)
  assert.isFalse(direct.components.some((component) => component.role === 'EndpointState'))
})

it('rejects overflow and every mismatched initializer provenance dimension', () => {
  const target = Target.wasm32UnknownUnknown
  const selected = ExecutionPackage.planWithin(
    target,
    specialization(SuspensionMode.direct),
    {
      body: { size: 8, alignment: 8 },
      endpoint: { size: 0, alignment: 1 },
      callback: { size: 0, alignment: 1 },
    },
    4096,
  )
  assert.strictEqual(selected._tag, 'ExecutionPackagePlan')
  if (selected._tag !== 'ExecutionPackagePlan') return
  const accepted = Object.freeze({
    target: selected.target,
    size: selected.size,
    alignment: selected.alignment,
    package: selected.provenance,
  })
  assert.deepEqual(ExecutionPackage.validateInitialization(selected, accepted), {
    _tag: 'Accepted',
    state: 'Initial',
  })
  assert.strictEqual(
    ExecutionPackage.validateInitialization(selected, {
      ...accepted,
      target: 'x86_64-unknown-linux-gnu',
    })._tag,
    'Rejected',
  )
  assert.strictEqual(
    ExecutionPackage.validateInitialization(selected, { ...accepted, size: selected.size + 1 })
      ._tag,
    'Rejected',
  )
  assert.strictEqual(
    ExecutionPackage.validateInitialization(selected, {
      ...accepted,
      alignment: selected.alignment * 2,
    })._tag,
    'Rejected',
  )
  assert.strictEqual(
    ExecutionPackage.validateInitialization(selected, { ...accepted, package: 'wrong' })._tag,
    'Rejected',
  )
  assert.strictEqual(
    ExecutionPackage.planWithin(
      target,
      specialization(SuspensionMode.direct),
      {
        body: { size: 4096, alignment: 8 },
        endpoint: { size: 0, alignment: 1 },
        callback: { size: 0, alignment: 1 },
      },
      64,
    )._tag,
    'ExecutionPackageUnavailable',
  )
})

it.effect('constructs without running source and never-driven drop releases one package', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/never-driven',
      new TextEncoder().encode(
        ordinaryStorageSource(`import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.layout { Layout }
struct Ready { storage: Allocation }
fn ready(state: &Ready) -> () { return () }
effect fn capturedBody(storage: Allocation) -> i32 {
  drop storage
  return 99
}
fn absurd<T>() -> T { let boom = 1 / 0 return absurd<T>() }
effect fn create<
  F: once Effect<i32> + Intrinsic.Detached,
  R: fn(&Ready) -> () + Intrinsic.Detached + Intrinsic.NonParking
>(body: F, readyState: Ready, onReady: R) -> Intrinsic.Execution<i32>
! Allocator.OutOfMemoryError
? &mut Allocator {
  let allocation = run Allocator.allocate(Intrinsic.executionLayout<i32, F, Ready, R>())
  unsafe {
    return Intrinsic.executionFromAllocation<i32, F, Ready, R>(
      move allocation,
      move body,
      move readyState,
      move onReady
    )
  }
  return absurd<Intrinsic.Execution<i32>>()
}
effect fn package() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let bodyLayout = Layout.of<i32>()
  let bodyStorage = run Intrinsic.systemAllocationAcquire(move bodyLayout)
  let readyLayout = Layout.of<i32>()
  let readyStorage = run Intrinsic.systemAllocationAcquire(move readyLayout)
  let body = capturedBody(move bodyStorage)
  let creating = create(move body, Ready { storage: move readyStorage }, ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let execution = run creating
  drop execution
  return 42
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(package(), recover) }`),
      ),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.layout._tag, 'Available')
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.layout._tag !== 'Available' || snapshot.mir._tag !== 'Available') return
    assert.lengthOf(snapshot.layout.value.executionPackages.plans, 1)
    assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
    assert.lengthOf(
      snapshot.mir.value.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'ExecutionFromAllocation'),
      1,
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result._tag, 'IntegerValue')
    if (evaluated.result._tag === 'IntegerValue') assert.strictEqual(evaluated.result.value, 42n)
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire'),
      3,
    )
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease'),
      3,
    )
    assert.isFalse(
      evaluated.trace.some(
        (event) => event._tag === 'Call' && event.target.name === 'capturedBody$effect$-1',
      ),
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('executes exact stored body and endpoint cleanup on WebAssembly package exits', () =>
  Effect.gen(function* () {
    for (const owner of ['Body', 'Endpoint'] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `execution-package/exact-${owner.toLowerCase()}-cleanup`,
        new TextEncoder().encode(exactCleanupProgram(owner)),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], owner)
      assert.strictEqual(Analysis.evaluate(snapshot)._tag, 'Trap', `${owner} evaluator cleanup`)
      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      assert.throws(() => (instance.exports.silk_main as () => void)(), WebAssembly.RuntimeError)
    }
  }),
)

it.effect('reserves nominal-union scratch for synthetic execution cleanup helpers', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/nominal-union-cleanup-frame',
      new TextEncoder().encode(nominalUnionExecutionCleanup),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')

    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    ;(instance.exports.silk_main as () => void)()
  }),
)

it.effect('roots unused nominal-union callbacks released by execution drive', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/nominal-union-drive-callback-cleanup',
      new TextEncoder().encode(nominalUnionDriveCallbackCleanup),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(Analysis.evaluate(snapshot)._tag, 'Completed')

    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    ;(instance.exports.silk_main as () => void)()
  }),
)

it.effect('reserves package cleanup scratch when construction and drive are separated', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/nominal-union-separated-drive-cleanup',
      new TextEncoder().encode(nominalUnionSeparatedDriveCleanup),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(Analysis.evaluate(snapshot)._tag, 'Completed')

    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    ;(instance.exports.silk_main as () => void)()
  }),
)

it.effect('drives one direct package to completion on an independent logical root', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/direct-completion',
      new TextEncoder().encode(`import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.layout { Layout }
struct State { value: i32 }
fn ready(state: &()) -> () { return () }
fn complete(state: &mut State, value: i32, storage: Allocation) -> () {
  drop storage
  state.value = value
  return ()
}
fn suspend(state: &mut State, execution: Intrinsic.Execution<i32>, storage: Allocation) -> () {
  drop storage
  drop execution
  return ()
}
effect fn packaged(state: &mut State) -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let completeLayout = Layout.of<i32>()
  let completeStorage = run Allocator.allocate(move completeLayout)
  let suspendLayout = Layout.of<i32>()
  let suspendStorage = run Allocator.allocate(move suspendLayout)
  let onComplete = complete(move completeStorage)
  let onSuspend = suspend(move suspendStorage)
  let mut execution = run Execution.make(effect { return 42 }, (), ready)
  Execution.notifyInitial(&mut execution)
  return run Execution.drive(move execution, move state, move onComplete, move onSuspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut state = State { value: 7 }
  run packaged(&mut state) |> Effect.provideMut<Allocator>(&mut allocator)
  return state.value
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
    assert.lengthOf(
      snapshot.mir.value.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'ExecutionDrive'),
      1,
    )
    assert.lengthOf(
      snapshot.mir.value.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'ExecutionNotifyInitial'),
      1,
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result._tag, 'IntegerValue')
    if (evaluated.result._tag === 'IntegerValue') assert.strictEqual(evaluated.result.value, 42n)
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire'),
      3,
    )
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease'),
      3,
    )
    const notified = evaluated.trace.findIndex(
      (event) => event._tag === 'ExecutionTransition' && event.event === 'NotifyInitial',
    )
    const bodyStarted = evaluated.trace.findIndex(
      (event) =>
        event._tag === 'Call' && event.depth === 0 && event.caller.module === 'silk/execution',
    )
    assert.isAtLeast(notified, 0)
    assert.isAbove(bodyStarted, notified)
    assert.isTrue(
      evaluated.trace.some(
        (event) =>
          event._tag === 'Call' && event.depth === 0 && event.caller.module === 'silk/execution',
      ),
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('publishes initial readiness exactly once before the body is driven', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/initial-readiness',
      new TextEncoder().encode(`import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.shared as Shared
struct Counter { value: i32 }
fn increment(counter: &mut Counter) -> () {
  counter.value = counter.value + 1
  return ()
}
fn read(counter: &mut Counter) -> i32 { return counter.value }
fn ready(counter: &Shared.Shared<Counter>) -> () {
  return Shared.withMut(counter, increment)
}
effect fn packaged() -> i32 ! Allocator.OutOfMemoryError ? &mut Allocator {
  let counter = run Shared.make<Counter>(Counter { value: 0 })
  let observed = Shared.clone<Counter>(&counter)
  let mut execution = run Execution.make(effect { return 42 }, move counter, ready)
  Execution.notifyInitial(&mut execution)
  let result = Shared.withMut(&observed, read)
  drop execution
  drop observed
  return result
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run packaged() |> Effect.provideMut<Allocator>(&mut allocator)
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
    assert.lengthOf(
      snapshot.mir.value.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'ExecutionNotifyInitial'),
      1,
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result._tag, 'IntegerValue')
    if (evaluated.result._tag === 'IntegerValue') assert.strictEqual(evaluated.result.value, 1n)
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease'),
      2,
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 1)
  }),
)

it.effect('traps duplicate initial readiness before invoking the endpoint twice', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/duplicate-initial-readiness',
      new TextEncoder().encode(`import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
fn ready(state: &()) -> () { return () }
effect fn packaged() -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let mut execution = run Execution.make(effect { return () }, (), ready)
  Execution.notifyInitial(&mut execution)
  Execution.notifyInitial(&mut execution)
  drop execution
  return ()
}
effect fn program() -> () ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run packaged() |> Effect.provideMut<Allocator>(&mut allocator)
}
effect fn recover(error: Allocator.OutOfMemoryError) -> () { return () }
pub fn main() -> () { return run Effect.catchAll(program(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(Analysis.evaluate(snapshot)._tag, 'Trap')
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.throws(() => (instance.exports.silk_main as () => void)(), WebAssembly.RuntimeError)
  }),
)

it.effect('keeps wrapper failure and cleanup before a refused package is initialized', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/refused',
      new TextEncoder().encode(
        ordinaryStorageSource(`import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect as Effect
import silk.execution as Execution
import silk.layout { Layout }
struct Exhausted {}
struct Ready { storage: Allocation }
fn ready(state: &Ready) -> () { return () }
effect fn allocate(self: &mut Exhausted, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}
impl Allocator for Exhausted { allocate: Exhausted.allocate }
effect fn body(storage: Allocation) -> i32 {
  drop storage
  return 99
}
effect fn package() -> i32 ! OutOfMemoryError {
  let bodyLayout = Layout.of<i32>()
  let bodyStorage = run Intrinsic.systemAllocationAcquire(move bodyLayout)
  let readyLayout = Layout.of<i32>()
  let readyStorage = run Intrinsic.systemAllocationAcquire(move readyLayout)
  let captured = body(move bodyStorage)
  let mut allocator = Exhausted {}
  let creating = Execution.make(move captured, Ready { storage: move readyStorage }, ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let execution = run creating
  drop execution
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(package(), recover) }`),
      ),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result._tag, 'IntegerValue')
    if (evaluated.result._tag === 'IntegerValue') assert.strictEqual(evaluated.result.value, 42n)
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire'),
      2,
    )
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease'),
      2,
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('rejects an initializer whose allocation has different layout provenance', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/mismatched-provenance',
      new TextEncoder().encode(`import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.layout { Layout }
struct Ready {}
fn ready(state: &Ready) -> () { return () }
fn absurd<T>() -> T { let boom = 1 / 0 return absurd<T>() }
effect fn create<
  F: once Effect<i32> + Intrinsic.Detached,
  R: fn(&Ready) -> () + Intrinsic.Detached + Intrinsic.NonParking
>(body: F, onReady: R) -> Intrinsic.Execution<i32>
! Allocator.OutOfMemoryError
? &mut Allocator {
  let allocation = run Allocator.allocate(Layout.of<i32>())
  unsafe {
    return Intrinsic.executionFromAllocation<i32, F, Ready, R>(
      move allocation,
      move body,
      Ready {},
      move onReady
    )
  }
  return absurd<Intrinsic.Execution<i32>>()
}
effect fn package() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let creating = create(effect { return 42 }, ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let execution = run creating
  drop execution
  return 42
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(package(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0142'],
    )
  }),
)

it.effect('keeps nested suspension inside the drive activation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/nested-completion',
      new TextEncoder().encode(`import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct State { value: i32 }
fn ready(state: &()) -> () { return () }
fn complete(state: &mut State, value: i32) -> () { state.value = value return () }
fn suspend(state: &mut State, execution: Intrinsic.Execution<i32>) -> () {
  state.value = 1
  drop execution
  return ()
}
effect fn body() -> i32 {
  let left = run Effect.suspend(effect { return 40 })
  return left + 2
}
effect fn packaged(state: &mut State) -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let execution = run Execution.make(body(), (), ready)
  return run Execution.drive(move execution, move state, complete, suspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut state = State { value: 7 }
  run packaged(&mut state) |> Effect.provideMut<Allocator>(&mut allocator)
  return state.value
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result._tag, 'IntegerValue')
    if (evaluated.result._tag === 'IntegerValue') assert.strictEqual(evaluated.result.value, 42n)
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'SuspensionChildComplete'),
      1,
    )
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease'),
      1,
    )
    const exhausted = Analysis.evaluate(snapshot, { maxExecutionStackBytes: 1 })
    assert.strictEqual(exhausted._tag, 'Trap')
    if (exhausted._tag === 'Trap') {
      assert.strictEqual(exhausted.reason, 'private execution stack exhausted')
      assert.lengthOf(
        exhausted.trace.filter((event) => event._tag === 'EffectFailure'),
        0,
      )
      assert.lengthOf(
        exhausted.trace.filter((event) => event._tag === 'AllocationAcquire'),
        1,
      )
    }
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('roots two executions independently when their first drives are non-LIFO', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/two-roots',
      new TextEncoder().encode(`import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct Branch { tag: i32 }
fn ready(state: &()) -> () { return () }
fn complete(branch: Branch, value: i32) -> () { return () }
fn suspend(branch: Branch, execution: Intrinsic.Execution<i32>) -> () {
  drop execution
  return ()
}
effect fn packaged() -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let first = run Execution.make(effect { return 20 }, (), ready)
  let second = run Execution.make(effect { return 22 }, (), ready)
  run Execution.drive(move second, Branch { tag: 2 }, complete, suspend)
  return run Execution.drive(move first, Branch { tag: 1 }, complete, suspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  run packaged() |> Effect.provideMut<Allocator>(&mut allocator)
  return 42
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    const roots = evaluated.trace.filter(
      (event) =>
        event._tag === 'Call' && event.depth === 0 && event.caller.module === 'silk/execution',
    )
    assert.lengthOf(roots, 2)
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease'),
      2,
    )
  }),
)

it.effect('completes a reified typed failure as data and releases its package once', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/reified-failure',
      new TextEncoder().encode(`import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.result { Result }
struct State { value: i32 }
struct Failed { code: i32 }
struct Ready {}
fn ready(state: &Ready) -> () { return () }
fn observe(result: Result<i32, Failed>) -> i32 {
  return match move result {
      Result<i32, Failed>.Success { value } => value
      Result<i32, Failed>.Failure { error } => match move error { Failed { code } => code }
  }
}
fn complete(state: &mut State, value: Result<i32, Failed>) -> () {
  state.value = observe(move value)
  return ()
}
fn suspend(state: &mut State, execution: Intrinsic.Execution<Result<i32, Failed>>) -> () {
  state.value = 1
  drop execution
  return ()
}
effect fn failed() -> i32 ! Failed { fail Failed { code: 42 } }
effect fn reified() -> Result<i32, Failed> { return run Effect.result(failed()) }
effect fn packaged(state: &mut State) -> () ! Allocator.OutOfMemoryError ? &mut Allocator {
  let body = reified()
  let execution = run Execution.make(move body, Ready {}, ready)
  return run Execution.drive(move execution, move state, complete, suspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut state = State { value: 7 }
  run packaged(&mut state) |> Effect.provideMut<Allocator>(&mut allocator)
  return state.value
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const reified = Analysis.evaluate(snapshot)
    assert.strictEqual(reified._tag, 'Completed')
    if (reified._tag !== 'Completed') return
    assert.strictEqual(reified.result._tag, 'IntegerValue')
    if (reified.result._tag === 'IntegerValue') assert.strictEqual(reified.result.value, 42n)
    assert.lengthOf(
      reified.trace.filter((event) => event._tag === 'AllocationRelease'),
      1,
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)
