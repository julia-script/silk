import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Type from '../src/Type.js'
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

const mirStructureFingerprint = (snapshot: Analysis.Snapshot) =>
  Analysis.loweredMir(snapshot).functions.map((fn) => ({
    target: normalizeSpelling(`${fn.id.module}.${fn.id.name}`),
    parameters: fn.parameterCount,
    locals: fn.localTypes.map((type) => normalizeSpelling(Type.encode(Mir.semanticType(type)))),
    result: normalizeSpelling(Type.encode(Mir.semanticType(fn.result))),
    operations: MirVerification.operations(fn).map((operation) => operation._tag),
  }))

it.effect('proves the ordinary and renamed local-shared pressure witnesses', () =>
  Effect.gen(function* () {
    const ordinary = yield* realized('pressure/local-shared-slp1', canonical)
    const spellingIndependent = yield* realized('pressure/local-shared-slp1-renamed', renamed)

    for (const snapshot of [ordinary, spellingIndependent]) {
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        JSON.stringify(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      assert.strictEqual(completed(snapshot).result.value, 42n)
    }

    assert.deepEqual(semanticFingerprint(spellingIndependent), semanticFingerprint(ordinary))
    assert.deepEqual(
      mirStructureFingerprint(spellingIndependent),
      mirStructureFingerprint(ordinary),
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
    assert.strictEqual(completed(snapshot).result.value, 42n)
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
    assert.strictEqual(completed(snapshot).result.value, 0n)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 0)
  }),
)

it.effect('drives a fallibly prepared same-thread timer and cancels before readiness', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'pressure/independent-execution-timer-owner',
      timerOwner,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      JSON.stringify(Analysis.diagnostics(snapshot)),
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    const evaluated = completed(snapshot)
    assert.strictEqual(evaluated.result.value, 42n)
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
    const notifications = evaluated.trace.filter(
      (event) =>
        event._tag === 'ExecutionTransition' &&
        (event.event === 'Notify' || event.event === 'Eligible'),
    )
    assert.deepEqual(
      notifications.map((event) => event.event),
      ['Notify', 'Eligible', 'Notify', 'Eligible'],
    )
    assert.strictEqual((yield* runWasm(snapshot)).result, 22)
  }),
)
