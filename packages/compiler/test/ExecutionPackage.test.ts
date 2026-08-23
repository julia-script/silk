import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ExecutionPackage from '../src/ExecutionPackage.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SuspensionMode from '../src/SuspensionMode.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'

const specialization = (suspension: SuspensionMode.Summary): ExecutionPackage.Specialization =>
  Object.freeze({
    result: 'i32',
    body: Type.effect('i32', Object.freeze([]), 'Take'),
    endpoint: Type.unit,
    callback: Type.callable(Object.freeze([Type.reference('Shared', Type.unit)]), Type.unit),
    suspension,
  })

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

it('enforces legal drive entry and exactly-once ownership branches', () => {
  const initial = ExecutionPackage.initializedOwnership()
  const entered = ExecutionPackage.enterDrive(initial, 'owner-a')
  assert.strictEqual(entered._tag, 'Entered')
  if (entered._tag !== 'Entered') return
  const completed = ExecutionPackage.completeDrive(entered.ownership)
  assert.strictEqual(completed.state, 'Completed')
  assert.strictEqual(completed.branch, 'Transferred')
  assert.strictEqual(completed.completionCallback, 'Invoked')
  assert.strictEqual(completed.suspensionCallback, 'Cleaned')
  assert.strictEqual(completed.allocation, 'Released')
  const second = ExecutionPackage.completeDrive(completed)
  assert.strictEqual(second, completed)

  const suspended = ExecutionPackage.suspendDrive(entered.ownership)
  assert.strictEqual(suspended.state, 'Dormant')
  assert.strictEqual(suspended.suspensionCallback, 'Invoked')
  assert.strictEqual(suspended.completionCallback, 'Cleaned')
  assert.strictEqual(
    ExecutionPackage.enterDrive(suspended, 'owner-b')._tag,
    'FatalIntrinsicStateTrap',
  )
  const notifying = Object.freeze({ ...suspended, state: 'Notifying' as const })
  assert.strictEqual(
    ExecutionPackage.enterDrive(notifying, 'owner-b')._tag,
    'FatalIntrinsicStateTrap',
  )
  const eligible = Object.freeze({ ...suspended, state: 'Eligible' as const })
  const resumed = ExecutionPackage.enterDrive(eligible, 'execution-a')
  assert.strictEqual(resumed._tag, 'Entered')
  if (resumed._tag !== 'Entered') return
  assert.strictEqual(resumed.root, 'execution-a')

  const other = ExecutionPackage.enterDrive(ExecutionPackage.initializedOwnership(), 'execution-b')
  assert.strictEqual(other._tag, 'Entered')
  if (other._tag !== 'Entered') return
  assert.strictEqual(other.root, 'execution-b')
  assert.notStrictEqual(resumed.root, other.root)
})

it.effect('constructs without running source and never-driven drop releases one package', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/never-driven',
      new TextEncoder().encode(`import silk.core as Core
import silk.core { Allocator }
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
! Core.OutOfMemoryError
? &mut Core.Allocator {
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
effect fn package() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let bodyLayout = Layout.of<i32>()
  let bodyStorage = run Intrinsic.systemAllocationAcquire(move bodyLayout)
  let readyLayout = Layout.of<i32>()
  let readyStorage = run Intrinsic.systemAllocationAcquire(move readyLayout)
  let body = capturedBody(move bodyStorage)
  let creating = create(move body, Ready { storage: move readyStorage }, ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  let execution = run creating
  drop execution
  return 42
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(package(), recover) }`),
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

it.effect('drives one direct package to completion on an independent logical root', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/direct-completion',
      new TextEncoder().encode(`import silk.core as Core
import silk.core { Allocator }
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
effect fn packaged(state: &mut State) -> () ! Core.OutOfMemoryError ? &mut Allocator {
  let completeLayout = Layout.of<i32>()
  let completeStorage = run Allocator.allocate(move completeLayout)
  let suspendLayout = Layout.of<i32>()
  let suspendStorage = run Allocator.allocate(move suspendLayout)
  let onComplete = complete(move completeStorage)
  let onSuspend = suspend(move suspendStorage)
  let execution = run Execution.make(effect { return 42 }, (), ready)
  return run Execution.drive(move execution, move state, move onComplete, move onSuspend)
}
effect fn program() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let mut state = State { value: 7 }
  run packaged(&mut state) |> Effect.provideMut<Core.Allocator>(&mut allocator)
  return state.value
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return 0 }
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

it.effect('keeps wrapper failure and cleanup before a refused package is initialized', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'execution-package/refused',
      new TextEncoder().encode(`import silk.core { Allocator, OutOfMemoryError }
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
      new TextEncoder().encode(`import silk.core as Core
import silk.core { Allocator }
import silk.effect as Effect
import silk.layout { Layout }
struct Ready {}
fn ready(state: &Ready) -> () { return () }
fn absurd<T>() -> T { let boom = 1 / 0 return absurd<T>() }
effect fn create<
  F: once Effect<i32> + Intrinsic.Detached,
  R: fn(&Ready) -> () + Intrinsic.Detached + Intrinsic.NonParking
>(body: F, onReady: R) -> Intrinsic.Execution<i32>
! Core.OutOfMemoryError
? &mut Core.Allocator {
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
effect fn package() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let creating = create(effect { return 42 }, ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  let execution = run creating
  drop execution
  return 42
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return 0 }
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
      new TextEncoder().encode(`import silk.core as Core
import silk.core { Allocator }
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
effect fn packaged(state: &mut State) -> () ! Core.OutOfMemoryError ? &mut Allocator {
  let execution = run Execution.make(body(), (), ready)
  return run Execution.drive(move execution, move state, complete, suspend)
}
effect fn program() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let mut state = State { value: 7 }
  run packaged(&mut state) |> Effect.provideMut<Core.Allocator>(&mut allocator)
  return state.value
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return 0 }
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
      new TextEncoder().encode(`import silk.core as Core
import silk.core { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct Branch { tag: i32 }
fn ready(state: &()) -> () { return () }
fn complete(branch: Branch, value: i32) -> () { return () }
fn suspend(branch: Branch, execution: Intrinsic.Execution<i32>) -> () {
  drop execution
  return ()
}
effect fn packaged() -> () ! Core.OutOfMemoryError ? &mut Allocator {
  let first = run Execution.make(effect { return 20 }, (), ready)
  let second = run Execution.make(effect { return 22 }, (), ready)
  run Execution.drive(move second, Branch { tag: 2 }, complete, suspend)
  return run Execution.drive(move first, Branch { tag: 1 }, complete, suspend)
}
effect fn program() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  run packaged() |> Effect.provideMut<Allocator>(&mut allocator)
  return 42
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return 0 }
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
      new TextEncoder().encode(`import silk.core as Core
import silk.core { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.result { Result, Success, Failure }
struct State { value: i32 }
struct Failed { code: i32 }
struct Ready {}
fn ready(state: &Ready) -> () { return () }
fn observe(result: Result<i32, Failed>) -> i32 {
  return match move result {
    Result<i32, Failed> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<Failed> { error } => match move error { Failed { code } => code }
    }
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
effect fn packaged(state: &mut State) -> () ! Core.OutOfMemoryError ? &mut Allocator {
  let body = reified()
  let execution = run Execution.make(move body, Ready {}, ready)
  return run Execution.drive(move execution, move state, complete, suspend)
}
effect fn program() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let mut state = State { value: 7 }
  run packaged(&mut state) |> Effect.provideMut<Allocator>(&mut allocator)
  return state.value
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return 0 }
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
