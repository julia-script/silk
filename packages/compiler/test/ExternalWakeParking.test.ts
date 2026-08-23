import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ExecutionAffinity from '../src/ExecutionAffinity.js'
import * as Layout from '../src/Layout.js'
import * as MirVerification from '../src/MirVerification.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as SuspensionOwnership from '../src/SuspensionOwnership.js'
import * as Type from '../src/Type.js'

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
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 0)
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
    const snapshot = yield* Analysis.ofSourceRealized(
      'external-wake-parking/callable-guard',
      new TextEncoder().encode(`import silk.core as Core
import silk.effect as Effect
import silk.execution as Execution
fn use(value: (), holder: &i32) -> () { return () }
fn register(wake: Intrinsic.Wake, holder: &i32) -> once fn(()) -> () {
  drop wake
  return use(holder)
}
effect fn parked(holder: i32) -> () {
  let registration = register(&holder)
  let resumed = run Execution.park(move registration)
  return ()
}
fn ready(state: &()) -> () { return () }
effect fn program() -> () ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let execution = run Execution.make(parked(1), (), ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  drop execution
  return ()
}
effect fn recover(error: Core.OutOfMemoryError) -> () { return () }
pub fn main() -> () { return run Effect.catchAll(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const parks = snapshot.mir.value.functions
      .flatMap(MirVerification.operations)
      .filter((operation) => operation._tag === 'ExecutionPark')
    assert.lengthOf(parks, 1)
    assert.strictEqual(parks.at(0)?.guardCleanup._tag, 'CallableCleanup')
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
