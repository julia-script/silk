import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Backend from '../src/Backend.js'
import * as ExecutionAffinity from '../src/ExecutionAffinity.js'
import * as Instances from '../src/Instances.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as SuspensionOwnership from '../src/SuspensionOwnership.js'
import * as Type from '../src/Type.js'
import { renameIndependentPolicy } from './support/independentPolicyRename.js'
import * as Projections from './support/projections.js'
import { unreachable } from './support/raise.js'

const encoder = new TextEncoder()
const canonical = readFileSync(
  new URL('../../../examples/language-pressure/local-shared-slp1/main.silk', import.meta.url),
  'utf8',
)
const renamed = readFileSync(
  new URL(
    '../../../examples/language-pressure/local-shared-slp1/renamed-main.silk',
    import.meta.url,
  ),
  'utf8',
)
const independentExecution = readFileSync(
  new URL(
    '../../../examples/language-pressure/independent-execution-separation/main.silk',
    import.meta.url,
  ),
  'utf8',
)
const deferredFirstActivation = readFileSync(
  new URL(
    '../../../examples/language-pressure/independent-execution-separation/first-activation.silk',
    import.meta.url,
  ),
  'utf8',
)
const alternateOwner = readFileSync(
  new URL(
    '../../../examples/language-pressure/independent-execution-separation/coroutine.silk',
    import.meta.url,
  ),
  'utf8',
)
const dormantCancellation = readFileSync(
  new URL(
    '../../../examples/language-pressure/independent-execution-separation/dormant-cancel.silk',
    import.meta.url,
  ),
  'utf8',
)
const timerOwner = readFileSync(
  new URL(
    '../../../examples/language-pressure/independent-execution-separation/timer.silk',
    import.meta.url,
  ),
  'utf8',
)
const selectiveReady = readFileSync(
  new URL(
    '../../../examples/language-pressure/independent-execution-separation/selective-ready.silk',
    import.meta.url,
  ),
  'utf8',
)
const postPublicationFailure = readFileSync(
  new URL(
    '../../../examples/language-pressure/independent-execution-separation/post-publication-failure.silk',
    import.meta.url,
  ),
  'utf8',
)

const explicitPayUseSource = (body: string): string => `import silk.core as Core
import silk.effect as Effect
import silk.execution as Execution
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { drop wake return Guard {} }
effect fn body(flag: bool) -> i32 { ${body} }
fn ready(state: &()) -> () { return () }
fn complete(owner: &mut Owner, result: i32) -> () { owner.result = result return () }
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
effect fn drive(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn program() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let execution = run Execution.make(body(true), (), ready)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  let mut owner = Owner { slot: Empty {}, result: 0 }
  run drive(move execution, &mut owner)
  return owner.result
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return -1 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

const sharedPayUseSource = (nested: boolean): string => `import silk.core as Core
import silk.effect as Effect
import silk.shared as Shared
struct State { value: i32 }
fn read(self: &State) -> i32 { return self.value }
effect fn body(state: Shared.Shared<State>) -> i32 {
  ${
    nested
      ? 'let value = run Effect.suspend(effect { return Shared.with<State, i32>(&state, read) })'
      : 'let value = Shared.with<State, i32>(&state, read)'
  }
  drop state
  return value
}
effect fn program() -> i32 ! Core.OutOfMemoryError {
  let mut allocator = Core.make()
  let state = run Shared.make<State>(State { value: 42 })
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
  return run body(move state)
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return -1 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

const payUseSources = {
  direct: 'pub fn main() -> i32 { return 42 }',
  nested: `import silk.effect as Effect
effect fn body() -> i32 { return run Effect.suspend(effect { return 42 }) }
pub fn main() -> i32 { return run body() }`,
  explicitDirect: explicitPayUseSource('return 42'),
  explicitNested: explicitPayUseSource(
    'let value = run Effect.suspend(effect { return 42 }) return value',
  ),
  explicitExternal: explicitPayUseSource(
    'if flag { return 42 } run Execution.park(register) return 0',
  ),
  sharedDirect: sharedPayUseSource(false),
  sharedNested: sharedPayUseSource(true),
} as const

const runtimeInventory = (snapshot: Analysis.Snapshot) => {
  if (snapshot.layout._tag !== 'Available') {
    throw new RangeError('runtime inventory requires an available target layout')
  }
  const mir = Analysis.loweredMir(snapshot)
  const operations = mir.functions.flatMap(MirVerification.operations)
  const provisional = ProvisionalMir.build(
    snapshot.instances,
    snapshot.layout.value,
    snapshot.index,
  )
  const ownership = SuspensionOwnership.plan(mir, provisional, snapshot.index)
  return {
    nestedRegions: mir.functions.reduce(
      (total, fn) => total + (fn.suspension?.regions.length ?? 0),
      0,
    ),
    packages: ownership.executionPackages.length,
    wakePackages: ownership.executionPackages.filter(
      (package_) => package_.wakeControl === 'StableGenerationCell',
    ).length,
    constructs: operations.filter((operation) => operation._tag === 'ExecutionFromAllocation')
      .length,
    drives: operations.filter((operation) => operation._tag === 'ExecutionDrive').length,
    parks: operations.filter((operation) => operation._tag === 'ExecutionPark').length,
    wakes: operations.filter((operation) => operation._tag === 'ExecutionWake').length,
  }
}

const emittedRuntimeInventory = (artifact: Backend.Artifact) => {
  const text = artifact._tag === 'WebAssemblyModuleArtifact' ? artifact.wat : artifact.ir
  const declarations = artifact.symbols.map(
    (entry) => `${entry.declaration.module}.${entry.declaration.name}`,
  )
  const hasDeclaration = (module: string, name: string): boolean =>
    declarations.includes(`${module}.${name}`)
  const externalParking = hasDeclaration('silk/execution', 'park')
  const nestedRunner = /suspend_step|silk_coroutine_frame_push_v1/.test(text)
  return Object.freeze({
    nestedRunner,
    executionPackage: hasDeclaration('silk/execution', 'make'),
    drive: hasDeclaration('silk/execution', 'drive'),
    dormantContinuation: externalParking && nestedRunner,
    wake: externalParking,
    notification: externalParking,
    atomicThread: [
      ...artifact.nativeRuntimeSymbols,
      ...(artifact._tag === 'WebAssemblyModuleArtifact'
        ? artifact.hostImports.map((entry) => `${entry.module}.${entry.name}`)
        : []),
    ].some((entry) => /atomic|thread|worker/i.test(entry)),
  })
}

const realized = Effect.fnUntraced(function* (name: string, source: string) {
  return yield* Analysis.ofSourceRealized(name, encoder.encode(source), 'wasm32-unknown-unknown')
})

const completed = (snapshot: Analysis.Snapshot) => {
  const outcome = Analysis.evaluate(snapshot)
  assert.strictEqual(
    outcome._tag,
    'Completed',
    JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? `${value}n` : value)),
  )
  return outcome._tag === 'Completed' ? outcome : unreachable('expected completed evaluation')
}

const runWasm = Effect.fnUntraced(function* (snapshot: Analysis.Snapshot) {
  const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
  const main = instance.exports.silk_main
  assert.strictEqual(typeof main, 'function')
  return {
    bytes: artifact.bytes,
    artifact,
    result: typeof main === 'function' ? main() : unreachable('expected Wasm entry'),
  }
})

const rejectingSource = (source: string, ordinal: 0 | 1): string => {
  const declaration =
    ordinal === 0
      ? 'let mut firstAllocator = SystemAllocator.make()'
      : 'let mut secondAllocator = SystemAllocator.make()'
  const rejected =
    ordinal === 0
      ? 'let mut firstAllocator = ExhaustedAllocator {}'
      : 'let mut secondAllocator = ExhaustedAllocator {}'
  assert.strictEqual(source.split(declaration).length, 2)
  return source.replace(declaration, rejected)
}

const independentExecutionFailureSource = (ordinal: number): string => {
  const names = ['inbox', 'tasks', 'result', 'waiter', 'producer'] as const
  const name = names.at(ordinal) ?? unreachable('expected construction allocation')
  return independentExecution
    .replace(
      'import silk.shared as Shared',
      `import silk.shared as Shared
import silk.core { Allocator, OutOfMemoryError }
import silk.layout { Layout }

struct ExhaustedAllocator {}
effect fn refuse(
  self: &mut ExhaustedAllocator,
  layout: Layout,
) -> Allocation ! OutOfMemoryError {
  drop layout
  fail OutOfMemoryError {}
}
impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.refuse }`,
    )
    .replace(
      `let mut ${name}Allocator = Core.make()`,
      `let mut ${name}Allocator = ExhaustedAllocator {}`,
    )
}

const renamePairs = [
  ['pressure/local-shared-slp1-renamed', 'pressure/local-shared-slp1'],
  ['SignalBox', 'ReadyInbox'],
  ['FutureCell', 'DeferredState'],
  ['Parcel', 'Payload'],
  ['Sender', 'Producer'],
  ['Reader', 'Waiter'],
  ['SignalCapture', 'CallbackCapture'],
  ['SignalCallback', 'ReadyCallback'],
  ['Delivered', 'Published'],
  ['Removed', 'Extracted'],
  ['Retained', 'Stored'],
  ['markReady', 'notifyReady'],
  ['appendFixed', 'pushNoGrow'],
  ['recordSignalCleanup', 'recordCallbackCleanup'],
  ['recordParcelCleanup', 'recordPayloadCleanup'],
  ['signalCaptured', 'enqueueCaptured'],
  ['signalFingerprint', 'inboxFingerprint'],
  ['lifecycleSignalFingerprint', 'lifecycleFingerprint'],
  ['subscribe', 'register'],
  ['deliverState', 'publishState'],
  ['notify', 'invoke'],
  ['readParcel', 'readPayload'],
  ['observeParcel', 'observePayload'],
  ['deliver', 'publish'],
  ['observeOwnedSignals', 'observeOwnedInbox'],
  ['signal', 'enqueue'],
  ['linked', 'connected'],
  ['runPressure', 'runWitness'],
  ['RecoveryValue', 'LaterValue'],
  ['readRecovery', 'readLater'],
  ['recoverLater', 'laterSuccess'],
  ['lastRecovery', 'finalRecovery'],
  ['EmptyAllocator', 'ExhaustedAllocator'],
  ['refuse', 'reject'],
  ['WorkRegistry', 'TaskStore'],
  ['SignalQueue', 'ReadyInbox'],
  ['PromiseCell', 'ResultState'],
  ['JobEndpoint', 'ReadyEndpoint'],
  ['WorkResult', 'TaskOutput'],
  ['Controller', 'Owner'],
  ['EventLoop', 'TimerReactor'],
  ['DelayToken', 'TimerGuard'],
  ['DeniedAllocator', 'ExhaustedAllocator'],
  ['ChannelState', 'PortState'],
  ['ChannelToken', 'PortGuard'],
  ['PendingCondition', 'Condition'],
  ['DispatchQueue', 'Inbox'],
  ['WorkSet', 'Tasks'],
  ['activateIdentity', 'driveIdentity'],
  ['awaitValue', 'awaitResult'],
  ['emitValue', 'produceResult'],
  ['pauseLocal', 'sleep'],
  ['tickLocal', 'poll'],
  ['advancePort', 'resume'],
  ['ExecutionFacade', 'Execution'],
  ['Platform', 'Core'],
  ['StoragePolicy', 'Allocator'],
  ['HeapProvider', 'SystemAllocator'],
] as const

const normalizeSpelling = (value: string): string => {
  const moduleNormalized = value.replaceAll(
    'pressure/local-shared-slp1-renamed',
    'pressure/local-shared-slp1',
  )
  return renamePairs
    .slice(1)
    .reduce(
      (normalized, [renamed, canonical]) =>
        normalized.replace(new RegExp(`\\b${renamed}\\b`, 'g'), canonical),
      moduleNormalized,
    )
}

const semanticFingerprint = (snapshot: Analysis.Snapshot) =>
  snapshot.instances.instances.map((instance) => ({
    target: normalizeSpelling(
      `${instance.key.declaration.module}.${instance.key.declaration.name}`,
    ),
    arguments: instance.key.typeArguments.map((argument) =>
      normalizeSpelling(Type.encodeGenericArgument(argument)),
    ),
    parameters: instance.function.declaration.parameters.flatMap((parameter) =>
      parameter.declaredType._tag === 'Resolved'
        ? [
            normalizeSpelling(
              Type.encode(Type.substitute(parameter.declaredType.type, instance.substitution)),
            ),
          ]
        : [],
    ),
    result:
      instance.function.declaration.returnType._tag === 'Resolved'
        ? normalizeSpelling(
            Type.encode(
              Type.substitute(instance.function.declaration.returnType.type, instance.substitution),
            ),
          )
        : 'unavailable',
  }))

const mirStructureFingerprint = (mir: Mir.Module) =>
  mir.functions
    .map((fn) => ({
      target: normalizeSpelling(`${fn.id.module}.${fn.id.name}`),
      parameters: fn.parameterCount,
      locals: fn.localTypes.map((type) => normalizeSpelling(Type.encode(Mir.semanticType(type)))),
      result: normalizeSpelling(Type.encode(Mir.semanticType(fn.result))),
      operations: MirVerification.operations(fn).map((operation) => operation._tag),
    }))
    .sort((left, right) => JSON.stringify(left).localeCompare(JSON.stringify(right)))

it.effect('proves the ordinary and renamed local-shared pressure witnesses', () =>
  Effect.gen(function* () {
    const ordinary = yield* realized('pressure/local-shared-slp1', canonical)
    const spellingIndependent = yield* realized('pressure/local-shared-slp1-renamed', renamed)
    const ordinaryMir = Analysis.loweredMir(ordinary)
    const spellingIndependentMir = Analysis.loweredMir(spellingIndependent)

    for (const [snapshot, mir] of [
      [ordinary, ordinaryMir],
      [spellingIndependent, spellingIndependentMir],
    ] as const) {
      const diagnostics = Analysis.diagnostics(snapshot)
      assert.deepEqual(
        diagnostics.map((diagnostic) => diagnostic.code),
        [],
        JSON.stringify(diagnostics),
      )
      assert.deepEqual(MirVerification.verify(mir), [])
      assert.strictEqual(completed(snapshot).result.value, 42n)
    }

    assert.deepEqual(semanticFingerprint(spellingIndependent), semanticFingerprint(ordinary))
    assert.deepEqual(
      mirStructureFingerprint(spellingIndependentMir),
      mirStructureFingerprint(ordinaryMir),
    )
    assert.deepEqual(
      Intrinsic.inventory()
        .filter((entry) => entry.operation.startsWith('Intrinsic.shared'))
        .map((entry) => entry.consumer),
      ['silk/shared.make', 'silk/shared.make', 'silk/shared.clone', 'silk/shared.withMut'],
    )
    assert.isFalse(
      Intrinsic.inventory().some((entry) =>
        /ReadyInbox|DeferredState|Producer|Waiter|SignalBox|FutureCell|Sender|Reader/.test(
          `${entry.operation}.${entry.consumer}`,
        ),
      ),
    )

    const evaluated = completed(ordinary)
    const events = Projections.allocationTraceEventsOf(evaluated)
    const lifecycleCounts = Object.fromEntries(
      [
        'AllocationAcquire',
        'AllocationRelease',
        'SharedInitialize',
        'SharedClone',
        'SharedAccessBegin',
        'SharedAccessEnd',
        'SharedDecrement',
        'SharedLastCleanup',
      ].map((tag) => [tag, events.filter((event) => event._tag === tag).length]),
    )
    assert.deepEqual(lifecycleCounts, {
      AllocationAcquire: 2,
      AllocationRelease: 2,
      SharedInitialize: 2,
      SharedClone: 24,
      SharedAccessBegin: 26,
      SharedAccessEnd: 26,
      SharedDecrement: 24,
      SharedLastCleanup: 2,
    })
    assert.strictEqual(
      evaluated.trace.filter(
        (event) =>
          event._tag === 'Call' &&
          event.target.module === 'pressure/local-shared-slp1' &&
          event.target.name === 'publishState',
      ).length,
      1,
    )
    assert.strictEqual(
      evaluated.trace.filter(
        (event) =>
          event._tag === 'Call' &&
          event.target.module === 'pressure/local-shared-slp1' &&
          event.target.name === 'enqueue',
      ).length,
      3,
    )
    assert.strictEqual(
      evaluated.trace.filter(
        (event) =>
          event._tag === 'Call' &&
          event.target.module === 'pressure/local-shared-slp1' &&
          event.target.name === 'drop@impl#0',
      ).length,
      3,
    )
    const firstWasm = yield* runWasm(ordinary)
    const renamedWasm = yield* runWasm(spellingIndependent)
    assert.strictEqual(firstWasm.result, 42)
    assert.strictEqual(renamedWasm.result, 42)
  }),
)

it.effect('recovers deterministically at every exercised construction quota', () =>
  Effect.gen(function* () {
    const programs = [rejectingSource(canonical, 0), rejectingSource(canonical, 1), canonical]
    const expectedResults = [142n, 142n, 42n]
    const acquireCounts = [2, 3, 2]
    const initializedActors = [
      ['LaterValue', 'LaterValue'],
      ['ReadyInbox', 'LaterValue', 'LaterValue'],
      ['ReadyInbox', 'DeferredState'],
    ]
    for (const [ordinal, source] of programs.entries()) {
      const snapshot = yield* realized(`pressure/local-shared-slp1-quota-${ordinal}`, source)
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        JSON.stringify(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      const first = completed(snapshot)
      const second = completed(snapshot)
      const expected = expectedResults.at(ordinal) ?? unreachable('expected quota result')
      assert.strictEqual(first.result.value, expected)
      assert.strictEqual(second.result.value, expected)
      const events = Projections.allocationTraceEventsOf(first)
      assert.deepEqual(
        events.flatMap((event) =>
          event._tag === 'SharedInitialize' &&
          event.element !== undefined &&
          Type.isNominal(event.element)
            ? [event.element.name]
            : [],
        ),
        initializedActors.at(ordinal) ?? unreachable('expected initialized actors'),
      )
      assert.strictEqual(
        events.filter((event) => event._tag === 'AllocationAcquire').length,
        acquireCounts.at(ordinal),
      )
      assert.strictEqual(
        events.filter((event) => event._tag === 'AllocationRelease').length,
        acquireCounts.at(ordinal),
      )
      assert.strictEqual(
        yield* runWasm(snapshot).pipe(Effect.map((result) => result.result)),
        Number(expected),
      )
    }
  }),
)

it.effect('runs the connected ordinary-source Execution and Wake companion', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'pressure/independent-execution-separation',
      independentExecution,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      JSON.stringify(Analysis.diagnostics(snapshot)),
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = completed(snapshot)
    assert.strictEqual(evaluated.result.value, 42n)
    const transitions = evaluated.trace.filter((event) => event._tag === 'ExecutionTransition')
    const transitionCount = (event: (typeof transitions)[number]['event']): number =>
      transitions.filter((transition) => transition.event === event).length
    assert.strictEqual(transitionCount('Initialize'), 2)
    assert.strictEqual(transitionCount('Register'), 1)
    assert.strictEqual(transitionCount('Relinquish'), 1)
    assert.strictEqual(transitionCount('Notify'), 1)
    assert.strictEqual(transitionCount('Eligible'), 1)
    assert.strictEqual(transitionCount('Resume'), 1)
    assert.strictEqual(transitionCount('Complete'), 2)
    const firstDrive = evaluated.trace.findIndex(
      (event) => event._tag === 'ExecutionTransition' && event.event === 'Drive',
    )
    assert.isAtLeast(firstDrive, 0)
    assert.isFalse(
      evaluated.trace.slice(firstDrive).some((event) => event._tag === 'AllocationAcquire'),
    )
    let sharedAccessDepth = 0
    for (const event of evaluated.trace) {
      if (event._tag === 'SharedAccessBegin') sharedAccessDepth += 1
      if (event._tag === 'SharedAccessEnd') sharedAccessDepth -= 1
      if (event._tag === 'Call' && event.target.name === 'publishEndpoint') {
        assert.strictEqual(sharedAccessDepth, 0)
      }
    }
    assert.strictEqual(sharedAccessDepth, 0)
    const allocationEvents = evaluated.trace.filter(
      (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
    )
    assert.strictEqual(
      allocationEvents.filter((event) => event._tag === 'AllocationAcquire').length,
      allocationEvents.filter((event) => event._tag === 'AllocationRelease').length,
      JSON.stringify(allocationEvents),
    )
    assert.strictEqual((yield* runWasm(snapshot)).result, 42)
  }),
)

it.effect('lets one ordinary owner choose first activation across exact body representations', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'pressure/independent-execution-first-activation',
      deferredFirstActivation,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = completed(snapshot)
    assert.strictEqual(evaluated.result.value, 20n)
    assert.isFalse(
      evaluated.trace.some(
        (event) => event._tag === 'Call' && event.target.name === 'firstBody$effect$-1',
      ),
    )
    const allocationEvents = evaluated.trace.filter(
      (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
    )
    assert.strictEqual(
      allocationEvents.filter((event) => event._tag === 'AllocationAcquire').length,
      allocationEvents.filter((event) => event._tag === 'AllocationRelease').length,
      JSON.stringify(allocationEvents),
    )
    assert.strictEqual((yield* runWasm(snapshot)).result, 20)
  }),
)

it.effect('reuses the Execution and Wake lifecycle from a bounded alternate owner', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'pressure/independent-execution-alternate-owner',
      alternateOwner,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      JSON.stringify(Analysis.diagnostics(snapshot)),
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = completed(snapshot)
    assert.strictEqual(evaluated.result.value, 123n)
    const transitions = evaluated.trace.filter((event) => event._tag === 'ExecutionTransition')
    assert.deepEqual(
      transitions.map((event) => event.event),
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
        'Register',
        'RetainGuard',
        'Relinquish',
        'Notify',
        'Eligible',
        'Resume',
        'Drive',
        'Complete',
        'Initialize',
        'Drive',
        'Register',
        'RetainGuard',
        'Relinquish',
        'Cancel',
        'Cleanup',
        'Release',
      ],
    )
    const allocationEvents = evaluated.trace.filter(
      (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
    )
    assert.strictEqual(
      allocationEvents.filter((event) => event._tag === 'AllocationAcquire').length,
      allocationEvents.filter((event) => event._tag === 'AllocationRelease').length,
      JSON.stringify(allocationEvents),
    )
    assert.strictEqual((yield* runWasm(snapshot)).result, 123)
  }),
)

it.effect('suppresses a retained Wake after post-suspension Dormant destruction', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'pressure/independent-execution-dormant-cancel',
      dormantCancellation,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = completed(snapshot)
    assert.deepEqual(
      evaluated.trace.flatMap((event) =>
        event._tag === 'Call' &&
        ['markGuard', 'markFrame', 'markBody', 'markEndpoint'].includes(event.target.name)
          ? [event.target.name]
          : [],
      ),
      ['markBody', 'markFrame', 'markGuard', 'markEndpoint'],
    )
    assert.strictEqual(evaluated.result.value, 1111n)
    assert.deepEqual(
      evaluated.trace
        .filter((event) => event._tag === 'ExecutionTransition')
        .map((event) => event.event),
      [
        'Initialize',
        'Drive',
        'Register',
        'RetainGuard',
        'Relinquish',
        'Cancel',
        'Cleanup',
        'Release',
      ],
    )
    const allocationEvents = evaluated.trace.filter(
      (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
    )
    assert.strictEqual(
      allocationEvents.filter((event) => event._tag === 'AllocationAcquire').length,
      allocationEvents.filter((event) => event._tag === 'AllocationRelease').length,
      JSON.stringify(allocationEvents),
    )
    assert.strictEqual((yield* runWasm(snapshot)).result, 1111)
  }),
)

it.effect('drives a fallibly prepared same-thread timer and cancels before readiness', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized('pressure/independent-execution-timer-owner', timerOwner)
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      JSON.stringify(Analysis.diagnostics(snapshot)),
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = completed(snapshot)
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      evaluated.trace
        .filter((event) => event._tag === 'ExecutionTransition')
        .map((event) => event.event),
      [
        'Initialize',
        'Drive',
        'Register',
        'RetainGuard',
        'Relinquish',
        'Initialize',
        'Drive',
        'Register',
        'RetainGuard',
        'Relinquish',
        'Notify',
        'Notify',
        'Eligible',
        'Eligible',
        'Resume',
        'Drive',
        'Complete',
        'Resume',
        'Drive',
        'Complete',
        'Initialize',
        'Drive',
        'Register',
        'RetainGuard',
        'Relinquish',
        'Initialize',
        'Drive',
        'Register',
        'RetainGuard',
        'Relinquish',
        'Cancel',
        'Cancel',
        'Cleanup',
        'Cleanup',
        'Release',
        'Release',
      ],
    )
    const callIndex = (name: string): number =>
      evaluated.trace.findIndex((event) => event._tag === 'Call' && event.target.name === name)
    assert.isBelow(callIndex('progressSibling'), callIndex('poll'))
    assert.isBelow(callIndex('poll'), callIndex('childReady'))
    assert.isBelow(callIndex('childReady'), callIndex('outerReady'))
    const allocationEvents = evaluated.trace.filter(
      (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
    )
    assert.strictEqual(
      allocationEvents.filter((event) => event._tag === 'AllocationAcquire').length,
      allocationEvents.filter((event) => event._tag === 'AllocationRelease').length,
      JSON.stringify(allocationEvents),
    )
    assert.strictEqual((yield* runWasm(snapshot)).result, 42)
  }),
)

it.effect('publishes one task identity without scanning and consumes a stale ready identity', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'pressure/independent-execution-selective-ready',
      selectiveReady,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      JSON.stringify(Analysis.diagnostics(snapshot)),
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = completed(snapshot)
    assert.strictEqual(evaluated.result.value, 22n)
    const notifications = evaluated.trace
      .filter((event) => event._tag === 'ExecutionTransition')
      .filter((event) => event.event === 'Notify' || event.event === 'Eligible')
    assert.deepEqual(
      notifications.map((event) => event.event),
      ['Notify', 'Eligible', 'Notify', 'Eligible'],
    )
    assert.strictEqual((yield* runWasm(snapshot)).result, 22)
  }),
)

it.effect('rolls back every connected-owner construction failure before publication', () =>
  Effect.gen(function* () {
    for (let quota = 0; quota < 5; quota += 1) {
      const snapshot = yield* realized(
        `pressure/independent-execution-separation-quota-${quota}`,
        independentExecutionFailureSource(quota),
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        JSON.stringify(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      const first = completed(snapshot)
      const second = completed(snapshot)
      assert.strictEqual(first.result.value, -100n)
      assert.strictEqual(second.result.value, -100n)
      assert.isFalse(
        first.trace.some(
          (event) =>
            event._tag === 'Call' &&
            (event.target.name === 'storeFirst' || event.target.name === 'storeSecond'),
        ),
      )
      const allocations = first.trace.filter(
        (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
      )
      assert.strictEqual(
        allocations.filter((event) => event._tag === 'AllocationAcquire').length,
        allocations.filter((event) => event._tag === 'AllocationRelease').length,
        JSON.stringify(allocations),
      )
      assert.strictEqual((yield* runWasm(snapshot)).result, -100)
    }
  }),
)

it.effect('preserves a published Initial task when later waiter allocation fails', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'pressure/independent-execution-post-publication-failure',
      postPublicationFailure,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      JSON.stringify(Analysis.diagnostics(snapshot)),
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = completed(snapshot)
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      evaluated.trace
        .filter((event) => event._tag === 'ExecutionTransition')
        .map((event) => event.event),
      ['Initialize', 'Drive', 'Complete'],
    )
    assert.isFalse(
      evaluated.trace.some(
        (event) => event._tag === 'Call' && event.target.name === 'insertWaiter',
      ),
    )
    const allocationEvents = evaluated.trace.filter(
      (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
    )
    assert.strictEqual(
      allocationEvents.filter((event) => event._tag === 'AllocationAcquire').length,
      allocationEvents.filter((event) => event._tag === 'AllocationRelease').length,
    )
    assert.strictEqual((yield* runWasm(snapshot)).result, 42)
  }),
)

it.effect('rejects a nested owner child that retains Scheduler and Allocator requirements', () =>
  Effect.gen(function* () {
    const source = `import silk.core as Core
import silk.core { Allocator }
import silk.effect as Effect
import silk.layout { Layout }
service Scheduler { effect fn join() -> i32 ? &Scheduler }
struct LocalScheduler {}
effect fn join(self: &LocalScheduler) -> i32 { return 42 }
impl Scheduler for LocalScheduler { join: LocalScheduler.join }
effect fn nested() -> i32 ! Core.OutOfMemoryError ? &Scheduler | &mut Core.Allocator {
  let value = run Scheduler.join()
  let storage = run Allocator.allocate(Layout.of<i32>())
  drop storage
  return value
}
effect fn recover(error: Core.OutOfMemoryError) -> i32 { return 0 }
fn schedule<F: once Effect<i32> + Intrinsic.Detached>(child: F) -> () {
  drop child
  return ()
}
pub fn main() -> () {
  let scheduler = LocalScheduler {}
  let mut allocator = Core.make()
  let child = nested()
    |> Effect.provide<Scheduler>(&scheduler)
    |> Effect.provideMut<Core.Allocator>(&mut allocator)
    |> Effect.catchAll(recover)
  return schedule(move child)
}`
    const snapshot = yield* realized('pressure/independent-execution-nested-owner-rejected', source)
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0139'],
      JSON.stringify(diagnostics),
    )
    const diagnostic = diagnostics.at(0)
    assert.strictEqual(diagnostic?.reason._tag, 'UnsatisfiedExecutableProperty')
    assert.strictEqual(
      diagnostic?.reason._tag === 'UnsatisfiedExecutableProperty'
        ? diagnostic.reason.property
        : undefined,
      'Intrinsic.Detached',
    )
    assert.include(
      diagnostic?.reason._tag === 'UnsatisfiedExecutableProperty'
        ? diagnostic.reason.causes.join(';')
        : '',
      'Provider',
    )
    assert.strictEqual(
      diagnostic === undefined
        ? undefined
        : source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      'schedule(move child)',
    )
  }),
)

it.effect('diagnoses an unowned park-capable complete entry at the explicit boundary', () =>
  Effect.gen(function* () {
    const source = `import silk.execution as Execution
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { drop wake return Guard {} }
pub fn main() -> () { return run Execution.park(register) }`
    const snapshot = yield* realized('pressure/independent-execution-unowned-entry', source)
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0140'],
    )
    const diagnostic = diagnostics.at(0)
    assert.strictEqual(diagnostic?.reason._tag, 'MissingExplicitExecutionOwner')
    assert.strictEqual(
      diagnostic === undefined
        ? undefined
        : source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      'pub fn main() -> () { return run Execution.park(register) }',
    )
    assert.notInclude(
      diagnostics.map((candidate) => candidate.code),
      'SEM0123',
    )
    assert.isFalse(
      Analysis.loweredMir(snapshot)
        .functions.flatMap(MirVerification.operations)
        .some((operation) => operation._tag === 'ExecutionFromAllocation'),
    )
  }),
)

it.effect(
  'reports static pay-for-use tiers without selecting atomic or policy machinery',
  () =>
    Effect.gen(function* () {
      const snapshots = new Map<string, Analysis.Snapshot>()
      const wasmArtifacts = new Map<string, Backend.WebAssemblyModuleArtifact>()
      for (const [name, source] of Object.entries(payUseSources)) {
        const snapshot = yield* realized(`pressure/pay-use-${name}`, source)
        snapshots.set(name, snapshot)
        assert.deepEqual(
          Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
          [],
          `${name}: ${JSON.stringify(Analysis.diagnostics(snapshot))}`,
        )
        assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [], name)
        assert.strictEqual(completed(snapshot).result.value, 42n, name)
        const wasm = yield* runWasm(snapshot)
        assert.strictEqual(wasm.result, 42, name)
        wasmArtifacts.set(name, wasm.artifact)
      }

      const inventory = (name: keyof typeof payUseSources) =>
        runtimeInventory(snapshots.get(name) ?? unreachable(`expected ${name} snapshot`))
      assert.deepEqual(inventory('direct'), {
        nestedRegions: 0,
        packages: 0,
        wakePackages: 0,
        constructs: 0,
        drives: 0,
        parks: 0,
        wakes: 0,
      })
      assert.deepInclude(inventory('nested'), {
        packages: 0,
        wakePackages: 0,
        constructs: 0,
        drives: 0,
        parks: 0,
        wakes: 0,
      })
      assert.isAbove(inventory('nested').nestedRegions, 0)
      assert.deepEqual(inventory('explicitDirect'), {
        nestedRegions: 0,
        packages: 1,
        wakePackages: 0,
        constructs: 1,
        drives: 1,
        parks: 0,
        wakes: 0,
      })
      assert.deepInclude(inventory('explicitNested'), {
        packages: 1,
        wakePackages: 0,
        constructs: 1,
        drives: 1,
        parks: 0,
        wakes: 0,
      })
      assert.isAbove(inventory('explicitNested').nestedRegions, 0)
      assert.deepInclude(inventory('explicitExternal'), {
        packages: 1,
        wakePackages: 1,
        constructs: 1,
        drives: 1,
        parks: 1,
        wakes: 0,
      })

      for (const name of ['sharedDirect', 'sharedNested'] as const) {
        const snapshot = snapshots.get(name) ?? unreachable(`expected ${name} snapshot`)
        assert.strictEqual(runtimeInventory(snapshot).packages, 0)
        assert.isTrue(
          Analysis.loweredMir(snapshot).functions.some((fn) =>
            fn.localTypes.some(
              (type) =>
                ExecutionAffinity.ofType(snapshot.index, Mir.semanticType(type))._tag ===
                'LocalExecution',
            ),
          ),
        )
      }
      assert.strictEqual(inventory('sharedDirect').nestedRegions, 0)
      assert.isAbove(inventory('sharedNested').nestedRegions, 0)

      const external =
        snapshots.get('explicitExternal') ?? unreachable('expected external snapshot')
      const externalArtifact = yield* Analysis.codegenWasm(external, { mode: 'release' })
      const repeatedArtifact = yield* Analysis.codegenWasm(external, { mode: 'release' })
      assert.deepEqual(repeatedArtifact.bytes, externalArtifact.bytes)
      assert.strictEqual(repeatedArtifact.wat, externalArtifact.wat)
      assert.notMatch(externalArtifact.wat, /atomic|worker|scheduler/i)

      const expectedArtifacts = {
        direct: {
          nestedRunner: false,
          executionPackage: false,
          drive: false,
          dormantContinuation: false,
          wake: false,
          notification: false,
          atomicThread: false,
        },
        nested: {
          nestedRunner: true,
          executionPackage: false,
          drive: false,
          dormantContinuation: false,
          wake: false,
          notification: false,
          atomicThread: false,
        },
        explicitDirect: {
          nestedRunner: false,
          executionPackage: true,
          drive: true,
          dormantContinuation: false,
          wake: false,
          notification: false,
          atomicThread: false,
        },
        explicitNested: {
          nestedRunner: true,
          executionPackage: true,
          drive: true,
          dormantContinuation: false,
          wake: false,
          notification: false,
          atomicThread: false,
        },
        explicitExternal: {
          nestedRunner: true,
          executionPackage: true,
          drive: true,
          dormantContinuation: true,
          wake: true,
          notification: true,
          atomicThread: false,
        },
        sharedDirect: {
          nestedRunner: false,
          executionPackage: false,
          drive: false,
          dormantContinuation: false,
          wake: false,
          notification: false,
          atomicThread: false,
        },
        sharedNested: {
          nestedRunner: true,
          executionPackage: false,
          drive: false,
          dormantContinuation: false,
          wake: false,
          notification: false,
          atomicThread: false,
        },
      } as const

      for (const name of Object.keys(payUseSources) as ReadonlyArray<keyof typeof payUseSources>) {
        const wasm = wasmArtifacts.get(name) ?? unreachable(`expected ${name} Wasm artifact`)
        assert.deepEqual(emittedRuntimeInventory(wasm), expectedArtifacts[name], `${name}: wasm`)
        const native = yield* Analysis.ofSourceRealized(
          `pressure/pay-use-${name}-native`,
          encoder.encode(payUseSources[name]),
          'aarch64-apple-darwin',
        )
        assert.deepEqual(Analysis.diagnostics(native), [], name)
        assert.deepEqual(runtimeInventory(native), inventory(name), name)
        const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
        assert.deepEqual(
          emittedRuntimeInventory(nativeArtifact),
          expectedArtifacts[name],
          `${name}: native`,
        )
        for (const fn of Analysis.loweredMir(native).functions) {
          if (
            MirVerification.operations(fn).some((operation) => operation._tag === 'ExecutionDrive')
          ) {
            assert.notInclude(
              Instances.suspensionOf(native.instances, fn.instance).modes,
              'ExternalPark',
              `${name}: owner-side drive must remain NonParking`,
            )
          }
        }
      }
    }),
  60_000,
)

const actorNeutralFixtures = [
  { name: 'scheduler-deferred', source: independentExecution, result: 42n },
  { name: 'timer', source: timerOwner, result: 42n },
  { name: 'coroutine', source: alternateOwner, result: 123n },
  { name: 'ready-owner', source: selectiveReady, result: 22n },
] as const

const verifyActorNeutralFixture = Effect.fnUntraced(function* (
  fixture: (typeof actorNeutralFixtures)[number],
) {
  const sourceId = `pressure/actor-neutral-${fixture.name}`
  const renamedSource = renameIndependentPolicy(fixture.source)
  const ordinary = yield* realized(sourceId, fixture.source)
  const renamedPolicy = yield* realized(sourceId, renamedSource)
  const ordinaryMir = Analysis.loweredMir(ordinary)
  const renamedMir = Analysis.loweredMir(renamedPolicy)
  for (const [snapshot, mir] of [
    [ordinary, ordinaryMir],
    [renamedPolicy, renamedMir],
  ] as const) {
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      [],
      `${fixture.name}: ${JSON.stringify(diagnostics)}`,
    )
    assert.deepEqual(MirVerification.verify(mir), [])
    assert.strictEqual(completed(snapshot).result.value, fixture.result)
  }
  assert.deepEqual(semanticFingerprint(renamedPolicy), semanticFingerprint(ordinary))
  assert.deepEqual(mirStructureFingerprint(renamedMir), mirStructureFingerprint(ordinaryMir))

  const ordinaryWasm = yield* runWasm(ordinary)
  const renamedWasm = yield* runWasm(renamedPolicy)
  assert.strictEqual(renamedWasm.result, ordinaryWasm.result)
  assert.deepEqual(
    emittedRuntimeInventory(renamedWasm.artifact),
    emittedRuntimeInventory(ordinaryWasm.artifact),
  )

  const ordinaryNative = yield* Analysis.ofSourceRealized(
    `${sourceId}-native`,
    encoder.encode(fixture.source),
    'aarch64-apple-darwin',
  )
  const renamedNative = yield* Analysis.ofSourceRealized(
    `${sourceId}-native`,
    encoder.encode(renamedSource),
    'aarch64-apple-darwin',
  )
  const ordinaryNativeMir = Analysis.loweredMir(ordinaryNative)
  const renamedNativeMir = Analysis.loweredMir(renamedNative)
  assert.deepEqual(MirVerification.verify(ordinaryNativeMir), [])
  assert.deepEqual(MirVerification.verify(renamedNativeMir), [])
  const ordinaryNativeArtifact = yield* Analysis.codegen(ordinaryNative, { mode: 'release' })
  const renamedNativeArtifact = yield* Analysis.codegen(renamedNative, { mode: 'release' })
  assert.deepEqual(
    emittedRuntimeInventory(renamedNativeArtifact),
    emittedRuntimeInventory(ordinaryNativeArtifact),
  )
})

for (const fixture of actorNeutralFixtures) {
  it.effect(
    `keeps the ${fixture.name} pressure-policy actor spelling neutral`,
    () => verifyActorNeutralFixture(fixture),
    60_000,
  )
}

it('keeps pressure-policy spellings out of the compiler privilege inventory', () => {
  assert.isFalse(
    Intrinsic.inventory().some((entry) =>
      /Scheduler|Deferred|Timer|Coroutine|TaskStore|ReadyInbox|Reactor|Allocator|WorkRegistry|SignalQueue|EventLoop|ChannelState/.test(
        `${entry.operation}.${entry.consumer}`,
      ),
    ),
  )
  const privilegedPhases = [
    '../src/NameResolution.ts',
    '../src/Type.ts',
    '../src/ExecutableOrigin.ts',
    '../src/LowerExpression.ts',
    '../src/LowerBuiltin.ts',
    '../src/MirVerification.ts',
    '../src/Intrinsic.ts',
  ] as const
  for (const phase of privilegedPhases) {
    const source = readFileSync(new URL(phase, import.meta.url), 'utf8')
    assert.notMatch(
      source,
      /silk\/core\.(?:OutOfMemoryError|Allocator|SystemAllocator)|\b(?:outOfMemoryError|systemAllocator)\b/,
      phase,
    )
  }
})
