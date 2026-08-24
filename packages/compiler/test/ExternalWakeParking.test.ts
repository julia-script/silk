import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CoroutineFrame from '../src/CoroutineFrame.js'
import * as ExecutionAffinity from '../src/ExecutionAffinity.js'
import type * as ExecutionTransition from '../src/ExecutionTransition.js'
import * as Layout from '../src/Layout.js'
import type * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as SuspensionOwnership from '../src/SuspensionOwnership.js'
import * as Type from '../src/Type.js'
import {
  independentExecutionEligibleDrop,
  independentExecutionIllegalDormantDrive,
  independentExecutionIllegalDormantDriveObservable,
  independentExecutionIllegalNotifyingDrive,
  independentExecutionIllegalNotifyingDriveObservable,
  independentExecutionLateCancelledWake,
  independentExecutionLocalReactor,
  independentExecutionMultiplePackages,
  independentExecutionNonLifo,
  independentExecutionParkedTypedFailure,
  independentExecutionReentrantDestroy,
  independentExecutionRepeatedGenerations,
  independentExecutionStackExhaustion,
  independentExecutionStackExhaustionObservable,
} from './support/corpus.js'

const replaceMirOperation = (
  module: Mir.Module,
  target: Mir.Operation,
  replacement: object,
): Mir.Module => {
  const rewrite = (operation: Mir.Operation): Mir.Operation => {
    // This boundary deliberately admits malformed operation shapes so verifier-negative tests can
    // prove rejection of values TypeScript correctly excludes from the valid MIR union.
    if (operation === target) return replacement as Mir.Operation
    if (operation._tag === 'ShortCircuit')
      return Object.freeze({
        ...operation,
        right: Object.freeze({
          ...operation.right,
          operations: Object.freeze(operation.right.operations.map(rewrite)),
        }),
      })
    if (operation._tag !== 'Match') return operation
    return Object.freeze({
      ...operation,
      arms: Object.freeze(
        operation.arms.map((arm) =>
          Object.freeze({
            ...arm,
            ...(arm.guard === undefined
              ? {}
              : {
                  guard: Object.freeze({
                    ...arm.guard,
                    operations: Object.freeze(arm.guard.operations.map(rewrite)),
                  }),
                }),
            selected: Object.freeze({
              ...arm.selected,
              operations: Object.freeze(arm.selected.operations.map(rewrite)),
            }),
          }),
        ),
      ),
    })
  }
  return Object.freeze({
    ...module,
    functions: Object.freeze(
      module.functions.map((fn) =>
        Object.freeze({
          ...fn,
          regions: Object.freeze(
            fn.regions.map(
              (region): Mir.Region =>
                region._tag === 'OperationRegion'
                  ? Object.freeze({
                      ...region,
                      operations: Object.freeze(region.operations.map(rewrite)),
                    })
                  : region,
            ),
          ),
        }),
      ),
    ),
  })
}

const llvmIndentedBlock = (ir: string, label: RegExp): string => {
  const match = ir.match(new RegExp(String.raw`${label.source}:\n((?:  .*\n)+)`))
  assert.isDefined(match, `missing LLVM block ${label.source}`)
  return match?.at(1) ?? ''
}

const assertTrapOnlyBlock = (block: string): void => {
  assert.match(block, /call void @llvm\.trap\(\)/)
  assert.match(block, /unreachable/)
  assert.notMatch(block, /call (?!void @llvm\.trap\(\))/)
  assert.notMatch(block, /on_(?:complete|suspend)/)
}

const assertNativeDriveAdmissionBeforeCallbacks = (ir: string): void => {
  const admission = ir.search(
    /br i1 %drive\d+_valid, label %drive\d+_accepted, label %drive\d+_rejected/,
  )
  assert.isAtLeast(admission, 0)
  const callback = ir.search(/drive\d+_on_(?:complete|suspend)/)
  assert.strictEqual(callback < 0 || admission < callback, true)
  assertTrapOnlyBlock(llvmIndentedBlock(ir, /drive\d+_rejected/))
}

const source = `import silk.core as Core
import silk.core { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.layout { Layout }
import silk.shared as Shared
struct Empty {}
struct Waiting { wake: Intrinsic.Wake }
struct WaiterState { slot: Empty | Waiting }
struct Guard { storage: Allocation state: Shared.Shared<WaiterState> }
fn install(state: &mut WaiterState, wake: Intrinsic.Wake) -> () {
  let previous = Intrinsic.replace(state.slot, Waiting { wake: move wake })
  drop previous
  return ()
}
fn extract(state: &mut WaiterState) -> Empty | Waiting {
  return Intrinsic.replace(state.slot, Empty {})
}
fn signal(state: &Shared.Shared<WaiterState>) -> () {
  let selected = Shared.withMut(state, extract)
  return match move selected {
    Empty {} => ()
    Waiting { wake } => Intrinsic.wake(move wake)
  }
}
fn register(
  wake: Intrinsic.Wake,
  storage: Allocation,
  state: Shared.Shared<WaiterState>
) -> Guard {
  let installing = install(move wake)
  let installed = Shared.withMut(&state, move installing)
  signal(&state)
  return Guard { storage: move storage, state: move state }
}
effect fn parked(storage: Allocation, state: Shared.Shared<WaiterState>) -> () {
  let retained = Shared.clone(&state)
  let registration = register(move storage, move state)
  let resumed = run Execution.park(move registration)
  drop retained
  return ()
}
fn ready(state: &()) -> () { return () }
effect fn program() -> () ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let guardStorage = run Allocator.allocate(Layout.of<i32>())
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  let state = run Shared.make<WaiterState>(WaiterState { slot: Empty {} })
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  let execution = run Execution.make(parked(move guardStorage, move state), (), ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  drop execution
  return ()
}
effect fn recover(error: Core.OutOfMemoryError) -> () { return () }
pub fn main() -> () { return run Effect.catchAll(program(), recover) }`

it.effect('seals Wake and lowers ordinary-source park and wake through verified MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/lowering',
      new TextEncoder().encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    assert.strictEqual(snapshot.layout._tag, 'Available')
    if (snapshot.mir._tag !== 'Available' || snapshot.layout._tag !== 'Available') return
    assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
    const wakeLayout = Layout.entry(snapshot.layout.value, Type.wake)
    assert.isDefined(wakeLayout)
    assert.isFalse(wakeLayout?.copy ?? true)
    assert.strictEqual(wakeLayout?.size, snapshot.layout.value.target.pointerSize)
    assert.strictEqual(wakeLayout?.representation._tag, 'Reference')
    const operations = snapshot.mir.value.functions.flatMap(MirVerification.operations)
    assert.lengthOf(
      operations.filter((operation) => operation._tag === 'ExecutionWake'),
      1,
    )
    const parkOperations = operations.filter((operation) => operation._tag === 'ExecutionPark')
    assert.lengthOf(parkOperations, 1)
    assert.strictEqual(parkOperations.at(0)?.registerCleanup._tag, 'CallableCleanup')
    assert.strictEqual(parkOperations.at(0)?.guardCleanup._tag, 'StructCleanup')
    const provisional = ProvisionalMir.build(
      snapshot.instances,
      snapshot.layout.value,
      snapshot.index,
    )
    const ownership = SuspensionOwnership.plan(snapshot.mir.value, provisional, snapshot.index)
    assert.deepEqual(ownership.violations, [])
    const package_ = ownership.executionPackages.find((plan) => plan.package.readinessStorage)
    assert.strictEqual(package_?.wakeControl, 'StableGenerationCell')
    assert.strictEqual(package_?.wakeAllocation, 'IndivisibleUntilFinalAuthority')
    const parkOperation = parkOperations.at(0)
    const parked = ownership.plans.find(
      (plan) =>
        parkOperation !== undefined && plan.span.start === parkOperation.provenance.span.start,
    )
    assert.isDefined(parked, SuspensionOwnership.encode(ownership))
    assert.lengthOf(parked?.success.releases ?? [], 1)
    assert.strictEqual(parked?.success.releases.at(0)?.cleanup._tag, 'StructCleanup')
    assert.isTrue(
      parked?.slots.some(
        (slot) =>
          slot.executionAffinity._tag === 'LocalExecution' &&
          slot.localSharedObligations._tag !== 'NoLocalSharedObligation',
      ) ?? false,
      SuspensionOwnership.encode(ownership),
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length,
      3,
    )
    assert.strictEqual(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease').length,
      3,
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const repeatedArtifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.deepEqual(repeatedArtifact.bytes, artifact.bytes)
    assert.strictEqual(repeatedArtifact.wat, artifact.wat)
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 0)
  }),
)

it.effect('emits deterministic native never-driven package cleanup', () =>
  Effect.gen(function* () {
    const first = yield* Analysis.ofSourceRealized(
      'external-wake-parking/native-cleanup',
      new TextEncoder().encode(source),
      'aarch64-apple-darwin',
    )
    const second = yield* Analysis.ofSourceRealized(
      'external-wake-parking/native-cleanup',
      new TextEncoder().encode(source),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(first), [])
    assert.deepEqual(Analysis.diagnostics(second), [])
    const firstArtifact = yield* Analysis.codegen(first, { mode: 'release' })
    const secondArtifact = yield* Analysis.codegen(second, { mode: 'release' })
    assert.strictEqual(firstArtifact.ir, secondArtifact.ir)
    assert.deepEqual(firstArtifact.bitcode, secondArtifact.bitcode)
  }),
)

it.effect('evaluates a latched park through an execution-owned root and later owner drive', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/evaluator-resume',
      new TextEncoder().encode(`import silk.core as Core
import silk.core { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard {
  Intrinsic.wake(move wake)
  return Guard {}
}
effect fn body() -> i32 {
  run Execution.park(register)
  return 42
}
fn complete(owner: &mut Owner, result: i32) -> () {
  owner.result = result
  return ()
}
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
fn ready(state: &()) -> () { return () }
effect fn driveOnce(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn finish(selected: Empty | Stored, owner: &mut Owner) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution: next } => run finishStored(move next, move owner)
  }
}
effect fn finishStored(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn program() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let mut owner = Owner { slot: Empty {}, result: 0 }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  run driveOnce(move execution, &mut owner)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  run finish(move selected, &mut owner)
  return owner.result
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const transitions = evaluated.trace.filter((event) => event._tag === 'ExecutionTransition')
    assert.deepEqual(
      transitions.map((event) => event.event),
      [
        'Initialize',
        'Drive',
        'Register',
        'Latch',
        'RetainGuard',
        'Notify',
        'Eligible',
        'Resume',
        'Drive',
        'Complete',
      ],
    )
    assert.lengthOf(new Set(transitions.map((event) => event.root)), 1)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('resumes two independent execution roots in non-LIFO owner-selected order', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/non-lifo',
      new TextEncoder().encode(independentExecutionNonLifo),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 240n)
    const transitions = evaluated.trace.filter((event) => event._tag === 'ExecutionTransition')
    const roots = [...new Set(transitions.map((event) => event.root))]
    assert.lengthOf(roots, 2)
    assert.deepEqual(
      transitions.filter((event) => event.event === 'Complete').map((event) => event.root),
      [...roots].reverse(),
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 240)
  }),
)

it.effect('traps a Dormant owner drive before invoking either outcome callback', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/illegal-dormant-drive',
      new TextEncoder().encode(independentExecutionIllegalDormantDrive),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(Analysis.evaluate(snapshot)._tag, 'Trap')
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.throws(() => (instance.exports.silk_main as () => number)(), WebAssembly.RuntimeError)
    const observable = yield* Analysis.ofSourceRealized(
      'independent-execution/illegal-dormant-drive-observable',
      new TextEncoder().encode(independentExecutionIllegalDormantDriveObservable),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(observable), [])
    const observableArtifact = yield* Analysis.codegenWasm(observable, { mode: 'release' })
    const observableInstance = new WebAssembly.Instance(
      new WebAssembly.Module(observableArtifact.bytes.slice()),
      {},
    )
    assert.throws(() => (observableInstance.exports.silk_main as () => number)(), /unreachable/)
    const native = yield* Analysis.ofSourceRealized(
      'independent-execution/illegal-dormant-drive-native',
      new TextEncoder().encode(independentExecutionIllegalDormantDrive),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
    assertNativeDriveAdmissionBeforeCallbacks(nativeArtifact.ir)
  }),
)

it.effect('traps a Notifying reentrant drive before invoking either outcome callback', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/illegal-notifying-drive',
      new TextEncoder().encode(independentExecutionIllegalNotifyingDrive),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Trap')
    if (evaluated._tag !== 'Trap') return
    assert.strictEqual(evaluated.reason, 'Execution drive entered an illegal lifecycle state')
    assert.isFalse(
      evaluated.trace.some(
        (event) =>
          event._tag === 'Call' &&
          (event.target.name === 'reentrantComplete' || event.target.name === 'reentrantSuspend'),
      ),
    )
    const observable = yield* Analysis.ofSourceRealized(
      'independent-execution/illegal-notifying-drive-observable',
      new TextEncoder().encode(independentExecutionIllegalNotifyingDriveObservable),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(observable), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(observable)), [])
    const artifact = yield* Analysis.codegenWasm(observable, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.throws(() => (instance.exports.silk_main as () => number)(), /unreachable/)
    const native = yield* Analysis.ofSourceRealized(
      'independent-execution/illegal-notifying-drive-native',
      new TextEncoder().encode(independentExecutionIllegalNotifyingDrive),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
    assertNativeDriveAdmissionBeforeCallbacks(nativeArtifact.ir)
  }),
)

it.effect('traps independent-root stack exhaustion before the completion callback', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/stack-exhaustion',
      new TextEncoder().encode(independentExecutionStackExhaustion),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot, { maxExecutionStackBytes: 1 })
    assert.strictEqual(evaluated._tag, 'Trap')
    if (evaluated._tag === 'Trap') {
      assert.strictEqual(evaluated.reason, 'private execution stack exhausted')
      assert.isFalse(
        evaluated.trace.some((event) => event._tag === 'Call' && event.target.name === 'complete'),
      )
    }
    const observable = yield* Analysis.ofSourceRealized(
      'independent-execution/stack-exhaustion-observable',
      new TextEncoder().encode(independentExecutionStackExhaustionObservable),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(observable), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(observable)), [])
    const artifact = yield* Analysis.codegenWasm(observable, {
      mode: 'release',
      privateExecutionStackPages: 1,
    })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.throws(() => (instance.exports.silk_main as () => number)(), /unreachable/)
    const native = yield* Analysis.ofSourceRealized(
      'independent-execution/stack-exhaustion-native',
      new TextEncoder().encode(independentExecutionStackExhaustion),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
    assert.match(
      nativeArtifact.ir,
      /br i1 %suspend_invocation_frame_exhausted, label %suspend_invocation_frame_trap, label %suspend_invocation_frame_pushed/,
    )
    assertTrapOnlyBlock(llvmIndentedBlock(nativeArtifact.ir, /suspend_invocation_frame_trap/))
  }),
)

it.effect('selects exact package provenance when one result type has multiple body packages', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/multiple-packages',
      new TextEncoder().encode(independentExecutionMultiplePackages),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.layout._tag, 'Available')
    if (snapshot.layout._tag !== 'Available') return
    assert.isAtLeast(snapshot.layout.value.executionPackages.plans.length, 2)
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'rejects forged execution package and take-once callback authorities before lowering',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'independent-execution/forged-mir-authority',
        new TextEncoder().encode(independentExecutionMultiplePackages),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.strictEqual(snapshot.mir._tag, 'Available')
      assert.strictEqual(snapshot.layout._tag, 'Available')
      if (snapshot.mir._tag !== 'Available' || snapshot.layout._tag !== 'Available') return
      const module = snapshot.mir.value
      const operations = module.functions.flatMap(MirVerification.operations)
      const initialize = operations.find(
        (
          operation,
        ): operation is Extract<Mir.Operation, { readonly _tag: 'ExecutionFromAllocation' }> =>
          operation._tag === 'ExecutionFromAllocation',
      )
      const drive = operations.find(
        (operation): operation is Extract<Mir.Operation, { readonly _tag: 'ExecutionDrive' }> =>
          operation._tag === 'ExecutionDrive',
      )
      const wake = operations.find(
        (operation): operation is Extract<Mir.Operation, { readonly _tag: 'ExecutionWake' }> =>
          operation._tag === 'ExecutionWake',
      )
      const otherPlan = snapshot.layout.value.executionPackages.plans.find(
        (plan) => plan.provenance !== initialize?.plan.provenance,
      )
      assert.isDefined(initialize)
      assert.isDefined(drive)
      assert.isDefined(wake)
      assert.isDefined(otherPlan)
      if (
        initialize === undefined ||
        drive === undefined ||
        wake === undefined ||
        otherPlan === undefined
      )
        return
      const authority = module.executionTransitions.at(0)
      const registerEdge = authority?.edges.find((edge) => edge.event === 'Register')
      const pendingEdge = authority?.edges.find((edge) => edge.after.execution === 'DestroyPending')
      assert.isDefined(authority)
      assert.isDefined(registerEdge)
      assert.isDefined(pendingEdge)
      if (authority === undefined || registerEdge === undefined || pendingEdge === undefined) return
      const authorityForgeries: ReadonlyArray<ExecutionTransition.Authority> = [
        Object.freeze({ ...authority, edges: Object.freeze(authority.edges.slice(1)) }),
        Object.freeze({
          ...authority,
          edges: Object.freeze(
            authority.edges.map((edge) =>
              edge === registerEdge
                ? edge.after.wake === undefined
                  ? edge
                  : Object.freeze({
                      ...edge,
                      after: Object.freeze({
                        ...edge.after,
                        wake: Object.freeze({
                          ...edge.after.wake,
                          generation: edge.after.wake.generation + 1,
                        }),
                      }),
                    })
                : edge,
            ),
          ),
        }),
        Object.freeze({
          ...authority,
          edges: Object.freeze(
            authority.edges.map((edge) =>
              edge === pendingEdge
                ? Object.freeze({ ...edge, cleanup: Object.freeze(['Endpoint' as const]) })
                : edge,
            ),
          ),
        }),
      ]
      const forgeries: ReadonlyArray<Mir.Module> = [
        replaceMirOperation(module, initialize, Object.freeze({ ...initialize, plan: otherPlan })),
        replaceMirOperation(module, drive, Object.freeze({ ...drive, completionAccess: 'Shared' })),
        replaceMirOperation(module, wake, Object.freeze({ ...wake, wakeAccess: 'Shared' })),
        ...authorityForgeries.map((forgedAuthority) =>
          Object.freeze({
            ...module,
            executionTransitions: Object.freeze([
              forgedAuthority,
              ...module.executionTransitions.slice(1),
            ]),
          }),
        ),
      ]
      for (const forged of forgeries)
        assert.include(
          MirVerification.verify(forged).map((violation) => violation.rule),
          'InvalidExecutionOperation',
        )
    }),
)

it.effect('releases a destroyed dormant execution only when its late cancelled Wake arrives', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/late-cancelled-wake',
      new TextEncoder().encode(independentExecutionLateCancelledWake),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const transitions = evaluated.trace.filter((event) => event._tag === 'ExecutionTransition')
    assert.deepEqual(
      transitions.map((event) => event.event),
      ['Initialize', 'Drive', 'Register', 'RetainGuard', 'Cancel', 'Cleanup', 'Release'],
    )
    const acquired = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length
    const released = evaluated.trace.filter((event) => event._tag === 'AllocationRelease').length
    assert.strictEqual(released, acquired)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('cancels a latched Wake when onSuspend destroys the execution', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/latched-on-suspend-destroy',
      new TextEncoder().encode(`import silk.core as Core
import silk.core { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.shared as Shared
struct Guard {}
struct ReadyState { called: i32 }
fn register(wake: Intrinsic.Wake) -> Guard {
  Intrinsic.wake(move wake)
  return Guard {}
}
effect fn body() -> i32 { run Execution.park(register) return 1 }
fn markReady(state: &mut ReadyState) -> () { state.called = 1 return () }
fn ready(state: &Shared.Shared<ReadyState>) -> () {
  Shared.withMut(state, markReady)
  return ()
}
fn readReady(state: &mut ReadyState) -> i32 { return state.called }
fn complete(state: &mut (), value: i32) -> () { return () }
fn suspend(state: &mut (), execution: Intrinsic.Execution<i32>) -> () {
  drop execution
  return ()
}
effect fn driveOnce(execution: Intrinsic.Execution<i32>, state: &mut ()) -> () {
  return run Execution.drive(move execution, move state, complete, suspend)
}
effect fn program() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let readyState = run Shared.make<ReadyState>(ReadyState { called: 0 })
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  let endpoint = Shared.clone(&readyState)
  let execution = run Execution.make(body(), move endpoint, ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  let mut state = ()
  run driveOnce(move execution, &mut state)
  let called = Shared.withMut(&readyState, readReady)
  drop readyState
  return 42 + called * 1000
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('defers reentrant notification destruction and never publishes Eligible', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/reentrant-destroy',
      new TextEncoder().encode(independentExecutionReentrantDestroy),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const transitions = evaluated.trace.filter((event) => event._tag === 'ExecutionTransition')
    assert.deepEqual(
      transitions.map((event) => event.event),
      [
        'Initialize',
        'Drive',
        'Register',
        'Latch',
        'RetainGuard',
        'Notify',
        'Cancel',
        'Cleanup',
        'Release',
      ],
    )
    assert.isFalse(transitions.some((event) => event.event === 'Eligible'))
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('reuses one execution for repeated generations and releases an eligible drop', () =>
  Effect.gen(function* () {
    const repeated = yield* Analysis.ofSourceRealized(
      'independent-execution/repeated-generations',
      new TextEncoder().encode(independentExecutionRepeatedGenerations),
      'wasm32-unknown-unknown',
    )
    const eligibleDrop = yield* Analysis.ofSourceRealized(
      'independent-execution/eligible-drop',
      new TextEncoder().encode(independentExecutionEligibleDrop),
      'wasm32-unknown-unknown',
    )
    const typedFailure = yield* Analysis.ofSourceRealized(
      'independent-execution/parked-typed-failure',
      new TextEncoder().encode(independentExecutionParkedTypedFailure),
      'wasm32-unknown-unknown',
    )
    for (const snapshot of [repeated, eligibleDrop, typedFailure]) {
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, 42n)
      const acquired = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length
      const released = evaluated.trace.filter((event) => event._tag === 'AllocationRelease').length
      assert.strictEqual(released, acquired)
      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }
    const transitions = Analysis.evaluate(repeated)
    assert.strictEqual(transitions._tag, 'Completed')
    if (transitions._tag !== 'Completed') return
    assert.deepEqual(
      transitions.trace
        .filter(
          (
            event,
          ): event is Extract<
            (typeof transitions.trace)[number],
            { readonly _tag: 'ExecutionTransition' }
          > => event._tag === 'ExecutionTransition' && event.event === 'Register',
        )
        .map((event) => event.generation),
      [1, 2],
    )
    const dropped = Analysis.evaluate(eligibleDrop)
    assert.strictEqual(dropped._tag, 'Completed')
    if (dropped._tag !== 'Completed') return
    assert.deepEqual(
      dropped.trace
        .filter((event) => event._tag === 'ExecutionTransition')
        .map((event) => event.event),
      [
        'Initialize',
        'Drive',
        'Register',
        'Latch',
        'RetainGuard',
        'Notify',
        'Eligible',
        'Cancel',
        'Cleanup',
        'Release',
      ],
    )
  }),
)

it.effect('delivers a retained Wake through an ordinary same-thread reactor poll', () =>
  Effect.gen(function* () {
    const renamed = independentExecutionLocalReactor
      .replaceAll('Reactor', 'PulseSource')
      .replaceAll('reactor', 'pulseSource')
      .replaceAll('poll', 'advance')
    const snapshots = yield* Effect.all(
      [independentExecutionLocalReactor, renamed].map((text, ordinal) =>
        Analysis.ofSourceRealized(
          `independent-execution/local-reactor-${ordinal}`,
          new TextEncoder().encode(text),
          'wasm32-unknown-unknown',
        ),
      ),
    )
    for (const snapshot of snapshots) {
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, 42n)
    }
    const snapshot = snapshots.at(0)
    if (snapshot === undefined) return
    const transitions = Analysis.evaluate(snapshot)
    assert.strictEqual(transitions._tag, 'Completed')
    if (transitions._tag !== 'Completed') return
    assert.deepEqual(
      transitions.trace
        .filter((event) => event._tag === 'ExecutionTransition')
        .map((event) => event.event),
      [
        'Initialize',
        'Drive',
        'Register',
        'RetainGuard',
        'Relinquish',
        'Notify',
        'Eligible',
        'Resume',
        'Drive',
        'Complete',
      ],
    )
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const wasmInstance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((wasmInstance.exports.silk_main as () => number)(), 42)
    assert.notMatch(wasm.wat, /atomic\.|worker|work.steal|scheduler/i)
    const nativeSnapshot = yield* Analysis.ofSourceRealized(
      'independent-execution/local-reactor-native',
      new TextEncoder().encode(independentExecutionLocalReactor),
      'aarch64-apple-darwin',
    )
    const native = yield* Analysis.codegen(nativeSnapshot, { mode: 'release' })
    assert.strictEqual(native._tag, 'LlvmBitcodeArtifact')
    if (native._tag !== 'LlvmBitcodeArtifact') return
    assert.notMatch(native.ir, /atomicrmw|cmpxchg|worker|work.steal|scheduler/i)
    const unavailable = yield* Analysis.ofSourceRealized(
      'independent-execution/local-reactor-unavailable',
      new TextEncoder().encode(independentExecutionLocalReactor),
      'mips-unknown-none',
    )
    assert.strictEqual(Analysis.targetOf(unavailable)._tag, 'Unavailable')
    assert.strictEqual(Analysis.layoutOf(unavailable)._tag, 'Unavailable')
    assert.strictEqual(Analysis.mirOf(unavailable)._tag, 'Unavailable')
  }),
)

it.effect('assigns only the sealed nominal Wake the local-execution affinity seed', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/affinity',
      new TextEncoder().encode(`import silk.shared as Shared
struct Wake {}
struct Empty {}
struct Holder { wake: Intrinsic.Wake }
fn intrinsic(value: Intrinsic.Wake) -> () { drop value return () }
fn ordinary(value: Wake) -> () { drop value return () }
fn aggregate(value: Holder) -> () { drop value return () }
fn union(value: Intrinsic.Wake | Empty) -> () { drop value return () }
fn array(value: [Intrinsic.Wake; 1]) -> () { drop value return () }
fn shared(value: Shared.Shared<Holder>) -> () { drop value return () }
pub fn main() -> i32 { return 42 }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const declarations = snapshot.index.modules.at(0)?.declarations ?? []
    const parameter = (name: string): Type.Type | undefined => {
      const declared = declarations
        .find(
          (declaration) =>
            declaration.name._tag === 'Present' && declaration.name.spelling === name,
        )
        ?.parameters.at(0)?.declaredType
      return declared?._tag === 'Resolved' ? declared.type : undefined
    }
    const intrinsic = parameter('intrinsic')
    const ordinary = parameter('ordinary')
    assert.isTrue(intrinsic !== undefined && Type.isWake(intrinsic))
    assert.strictEqual(Type.wake.sealed, 'Intrinsic.Wake')
    assert.strictEqual(
      intrinsic === undefined
        ? 'Unrestricted'
        : ExecutionAffinity.ofType(snapshot.index, intrinsic)._tag,
      'LocalExecution',
    )
    assert.strictEqual(
      ordinary === undefined ? 'Missing' : ExecutionAffinity.ofType(snapshot.index, ordinary)._tag,
      'Unrestricted',
    )
    for (const name of ['aggregate', 'union', 'array', 'shared']) {
      const type = parameter(name)
      assert.strictEqual(
        type === undefined ? 'Missing' : ExecutionAffinity.ofType(snapshot.index, type)._tag,
        'LocalExecution',
      )
    }
  }),
)

it.effect('rejects external parking at a complete entry without an explicit Execution owner', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/unowned-entry',
      new TextEncoder().encode(`import silk.execution as Execution
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { drop wake return Guard {} }
pub fn main() -> () { return run Execution.park(register) }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0140'],
    )
  }),
)

it.effect('rejects a second signal as an ordinary use after move', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/double-signal',
      new TextEncoder().encode(`fn duplicate(wake: Intrinsic.Wake) -> () {
  Intrinsic.wake(move wake)
  Intrinsic.wake(move wake)
  return ()
}
pub fn main() -> i32 { return 42 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['OWN0001'],
    )
  }),
)

it.effect('rejects a registration callback that transitively parks', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/non-parking-registration',
      new TextEncoder().encode(`import silk.execution as Execution
struct Guard {}
fn harmless(wake: Intrinsic.Wake) -> Guard { drop wake return Guard {} }
effect fn nested() -> () { return run Execution.park(harmless) }
fn invalid(wake: Intrinsic.Wake) -> Guard {
  drop wake
  let parked = run nested()
  return Guard {}
}
pub fn main() -> () { return run Execution.park(invalid) }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0140', 'SEM0139'],
    )
  }),
)

it.effect('rejects wake and direct or transitive park while Shared source access is active', () =>
  Effect.gen(function* () {
    const variants = [
      {
        callback: `fn selected(value: &Pair) -> i32 {
  let parked = run Execution.park(register)
  return value.value
}`,
        access: 'with',
      },
      {
        callback: `fn inner(value: &mut Pair) -> i32 {
  let parked = run Execution.park(register)
  return value.value
}
fn selected(value: &mut Pair) -> i32 { return inner(move value) }`,
        access: 'withMut',
      },
      {
        callback: `fn parks() -> i32 {
  let parked = run Execution.park(register)
  return 1
}
fn selected(value: &mut Pair) -> i32 { return parks() }`,
        access: 'withMut',
      },
    ]
    for (const [ordinal, variant] of variants.entries()) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `external-wake-parking/shared-access-${ordinal}`,
        new TextEncoder().encode(`import silk.execution as Execution
import silk.shared as Shared
struct Guard {}
struct Pair { value: i32 }
fn register(wake: Intrinsic.Wake) -> Guard { drop wake return Guard {} }
${variant.callback}
fn access(self: &Shared.Shared<Pair>) -> i32 { return Shared.${variant.access}(self, selected) }
pub fn main() -> i32 { return 42 }`),
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot)
          .filter((diagnostic) => diagnostic.code === 'OWN0016')
          .map((diagnostic) => diagnostic.code),
        ['OWN0016'],
        `variant ${ordinal}`,
      )
    }

    const wakeSnapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/shared-access-wake',
      new TextEncoder().encode(`import silk.shared as Shared
struct Empty {}
struct Armed { wake: Intrinsic.Wake }
struct State { slot: Empty | Armed }
fn signalInside(state: &mut State) -> () {
  let selected = Intrinsic.replace(state.slot, Empty {})
  return match move selected {
    Empty {} => ()
    Armed { wake } => Intrinsic.wake(move wake)
  }
}
fn access(self: &Shared.Shared<State>) -> () {
  return Shared.withMut(self, signalInside)
}
pub fn main() -> i32 { return 42 }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(wakeSnapshot).map((diagnostic) => diagnostic.code),
      ['OWN0016'],
    )
  }),
)

it.effect('retains represented callable registration guards in ExecutionPark cleanup facts', () =>
  Effect.gen(function* () {
    const module = 'external-wake-parking/callable-guard'
    const source = `import silk.core as Core
import silk.core { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.layout { Layout }
fn use(value: (), holder: &Allocation) -> () { return () }
fn register(wake: Intrinsic.Wake, holder: &Allocation) -> once fn(()) -> () {
  drop wake
  return use(holder)
}
effect fn parked(holder: Allocation) -> () {
  let registration = register(&holder)
  let resumed = run Execution.park(move registration)
  drop holder
  return ()
}
fn ready(state: &()) -> () { return () }
effect fn program() -> () ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let holder = run Allocator.allocate(Layout.of<i32>())
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  let execution = run Execution.make(parked(move holder), (), ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  drop execution
  return ()
}
effect fn recover(error: Core.OutOfMemoryError) -> () { return () }
pub fn main() -> () { return run Effect.catchAll(program(), recover) }`
    const snapshot = yield* Analysis.ofSourceRealized(module, new TextEncoder().encode(source))
    const capture = snapshot.ownership
      .get(module)
      ?.functions.flatMap((fn) => [...fn.loans])
      .find((loan) => loan.origin === 'CallableCapture')
    assert.strictEqual(
      capture === undefined
        ? undefined
        : source.slice(capture.endSpan.start, capture.endSpan.end).trim(),
      'run Execution.park(move registration)',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const parks = snapshot.mir.value.functions
      .flatMap(MirVerification.operations)
      .filter((operation) => operation._tag === 'ExecutionPark')
    assert.lengthOf(parks, 1)
    assert.strictEqual(parks.at(0)?.guardCleanup._tag, 'CallableCleanup')
    const parked = snapshot.mir.value.functions.find((fn) =>
      MirVerification.operations(fn).some((operation) => operation === parks.at(0)),
    )
    const parkRegion = parked?.suspension?.regions
      .flatMap((region) =>
        region._tag === 'RunSuspendableEffectRegion' && region.operation === parks.at(0)
          ? [region]
          : [],
      )
      .at(0)
    const state = parkRegion?.relay.state
    assert.deepEqual(
      state?.failure.releases.map((release) => release.cleanup._tag),
      ['CallableCleanup'],
    )
    const layout =
      state === undefined ? undefined : CoroutineFrame.stateLayout(snapshot.mir.value, state.point)
    assert.isDefined(layout)
    if (parked !== undefined && layout !== undefined)
      assert.deepEqual(
        CoroutineFrame.cleanupReleases(parked, layout).map((field) => field.access.cleanup._tag),
        ['CallableCleanup'],
      )
    const caller = snapshot.mir.value.functions.find((fn) =>
      fn.id.name.startsWith('parked$effect$'),
    )
    const callerState = caller?.suspension?.regions
      .flatMap((region) =>
        region._tag === 'RunSuspendableEffectRegion' ? [region.relay.state] : [],
      )
      .find((candidate) => candidate !== undefined)
    assert.deepEqual(
      callerState?.failure.releases.map((release) => release.cleanup._tag),
      ['AllocationCleanup'],
    )
    const callerLayout =
      callerState === undefined
        ? undefined
        : CoroutineFrame.stateLayout(snapshot.mir.value, callerState.point)
    assert.isDefined(callerLayout)
    if (caller !== undefined && callerLayout !== undefined)
      assert.deepEqual(
        CoroutineFrame.cleanupReleases(caller, callerLayout).map(
          (field) => field.access.cleanup._tag,
        ),
        ['AllocationCleanup'],
      )
    assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
  }),
)

it.effect(
  'accepts a timer-shaped extract-then-signal boundary without privileged actor policy',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'external-wake-parking/timer-shaped',
        new TextEncoder().encode(`import silk.execution as Execution
import silk.shared as Shared
struct Empty {}
struct Armed { wake: Intrinsic.Wake }
struct TimerState { registration: Empty | Armed }
struct Unlink { state: Shared.Shared<TimerState> }
fn install(state: &mut TimerState, wake: Intrinsic.Wake) -> () {
  let previous = Intrinsic.replace(state.registration, Armed { wake: move wake })
  drop previous
  return ()
}
fn take(state: &mut TimerState) -> Empty | Armed {
  return Intrinsic.replace(state.registration, Empty {})
}
fn fire(state: &Shared.Shared<TimerState>) -> () {
  let selected = Shared.withMut(state, take)
  return match move selected {
    Empty {} => ()
    Armed { wake } => Intrinsic.wake(move wake)
  }
}
fn register(wake: Intrinsic.Wake, state: Shared.Shared<TimerState>) -> Unlink {
  let installing = install(move wake)
  let installed = Shared.withMut(&state, move installing)
  return Unlink { state: move state }
}
effect fn sleep(state: Shared.Shared<TimerState>) -> () {
  let retained = Shared.clone(&state)
  let registration = register(move state)
  let resumed = run Execution.park(move registration)
  drop retained
  return ()
}
pub fn main() -> i32 { return 42 }`),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
    }),
)
