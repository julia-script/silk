import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as Instances from '../src/Instances.js'
import * as TypeInference from '../src/internal/TypeInference.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as Type from '../src/Type.js'
import * as Projections from './support/projections.js'
import { unreachable } from './support/raise.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('provisional-mir/main', encoder.encode(source))

const available = (self: Analysis.Snapshot): ProvisionalMir.Module => {
  const provisional = Projections.provisionalMirOf(self)
  assert.strictEqual(provisional._tag, 'Available')
  if (provisional._tag === 'Available') return provisional.value
  return unreachable('expected provisional MIR')
}

const outcomes = (self: ProvisionalMir.Module): ReadonlyArray<ProvisionalMir.Outcome> =>
  self.executions.flatMap((execution) => execution.regions.map((region) => region.outcome))

it.effect('separates the canonical suspension origin from complete-or-relay callers', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
effect fn delayed(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
pub fn main() -> i32 {
  return run delayed(42)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const provisional = available(self)
    assert.deepEqual(ProvisionalMir.verify(provisional), [])
    const controls = outcomes(provisional)
    const origins = controls.filter((control) => control._tag === 'SuspendEffect')
    const relays = controls.filter((control) => control._tag === 'RunSuspendableEffect')
    assert.lengthOf(origins, 1)
    assert.isAtLeast(relays.length, 1)
    const origin = origins.at(0)
    assert.strictEqual(origin?._tag, 'SuspendEffect')
    if (origin?._tag === 'SuspendEffect') {
      assert.strictEqual(origin.transfer._tag, 'OriginateTransfer')
      assert.strictEqual(origin.span.sourceId, 'silk/effect')
    }
    for (const relay of relays) {
      if (relay._tag !== 'RunSuspendableEffect') continue
      assert.deepEqual(relay.relay, {
        _tag: 'RelayExistingTransfer',
        preserves: ['Child', 'Origin', 'TypedOutcome'],
      })
    }
  }),
)

it.effect('uses ordinary propagation for source-defined combinators', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
effect fn seed(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
fn increment(value: i32) -> i32 { return value + 1 }
effect fn program() -> i32 {
  return run seed(41) |> Effect.map(increment)
}
pub fn main() -> i32 {
  return run program()
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const provisional = available(self)
    assert.deepEqual(ProvisionalMir.verify(provisional), [])
    const relays = outcomes(provisional).filter(
      (control) => control._tag === 'RunSuspendableEffect',
    )
    assert.isAtLeast(relays.length, 1)
    assert.isTrue(relays.every((control) => control.completion._tag === 'Propagate'))
  }),
)

it.effect('classifies a selected suspending service implementation on the provided runner', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
service Value {
  effect fn get() -> i32 ? &Value
}
struct SuspendedValue { value: i32 }
effect fn get(self: &SuspendedValue) -> i32 {
  return run Effect.suspend(effect { return self.value })
}
impl Value for SuspendedValue { get: SuspendedValue.get }
effect fn read() -> i32 ? &Value {
  return run Value.get()
}
pub fn main() -> i32 {
  let provider = SuspendedValue { value: 42 }
  return run Effect.provide(read(), &provider)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const provisional = available(self)
    assert.deepEqual(ProvisionalMir.verify(provisional), [])
    const selected = outcomes(provisional).find(
      (control) =>
        control._tag === 'RunSuspendableEffect' &&
        control.runner.execution._tag === 'ProvidedEffectRunnerExecution' &&
        control.runner.providers.some((provider) => provider.capability.name === 'Value'),
    )
    assert.strictEqual(selected?._tag, 'RunSuspendableEffect', ProvisionalMir.encode(provisional))
    if (selected?._tag === 'RunSuspendableEffect') {
      assert.strictEqual(selected.runner.classification, 'Suspendable')
      assert.strictEqual(selected.runner.execution._tag, 'ProvidedEffectRunnerExecution')
    }
  }),
)

it.effect('does not contaminate a generic service provider with another specialization', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
service Value<S> {
  effect fn get(value: &S) -> i32 ? &Value<S>
}

struct Token {}
struct Other {}
struct Fixed<S> {}
effect fn get<S>(self: &Fixed<S>, value: &S) -> i32 { return 42 }
impl<S> Value<S> for Fixed<S> { get: Fixed.get }
effect fn read(value: &Token) -> i32 ? &Value<Token> { return run Value.get<Token>(value) }
pub fn main() -> i32 {
  let provider = Fixed<Token> {}
  let token = Token {}
  return run Effect.provide(read(&token), &provider)
}`)
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => `${diagnostic.code}: ${diagnostic.message}`),
      [],
    )
    const token = Type.nominal('provisional-mir/main', 'Token')
    const get = Instances.matchingSpecialization(self.instances, {
      declaration: { module: 'provisional-mir/main', name: 'get' },
      typeArguments: [token],
    }).at(0)
    assert.isDefined(get)
    if (get === undefined) return
    const other = Type.nominal('provisional-mir/main', 'Other')
    const substitution = TypeInference.substitution(
      get.function.declaration.typeParameters.map((parameter) => parameter.type),
      [other],
    )
    assert.isDefined(substitution)
    if (substitution === undefined) return
    const otherKey: Instances.InstanceKey = Object.freeze({
      ...get.key,
      typeArguments: Object.freeze([other]),
    })
    const otherInstance: Instances.Instance = Object.freeze({
      ...get,
      key: otherKey,
      substitution,
    })
    const discovery: Instances.Discovery = Object.freeze({
      ...self.instances,
      instances: Object.freeze([...self.instances.instances, otherInstance]),
      suspension: Object.freeze([
        ...self.instances.suspension,
        Object.freeze({
          _tag: 'SuspensionFact' as const,
          subject: Object.freeze({ _tag: 'Instance' as const, key: otherKey }),
          summary: Object.freeze({
            _tag: 'SuspensionModeSummary' as const,
            availability: 'Complete' as const,
            modes: Object.freeze(['NestedTransfer' as const]),
            causes: Object.freeze([]),
          }),
        }),
        Object.freeze({
          _tag: 'SuspensionFact' as const,
          subject: Object.freeze({ _tag: 'Execution' as const, key: otherKey }),
          summary: Object.freeze({
            _tag: 'SuspensionModeSummary' as const,
            availability: 'Complete' as const,
            modes: Object.freeze(['NestedTransfer' as const]),
            causes: Object.freeze([]),
          }),
        }),
      ]),
    })
    assert.strictEqual(self.layout._tag, 'Available')
    if (self.layout._tag !== 'Available') return
    const provisional = ProvisionalMir.build(
      discovery,
      self.layout.value,
      Analysis.declarationIndex(self),
    )
    const provided = outcomes(provisional).filter(
      (
        outcome,
      ): outcome is Extract<ProvisionalMir.Outcome, { readonly _tag: 'RunSuspendableEffect' }> =>
        outcome._tag === 'RunSuspendableEffect' &&
        outcome.runner.execution._tag === 'ProvidedEffectRunnerExecution' &&
        outcome.runner.providers.some((provider) => provider.capability.name === 'Value'),
    )
    assert.lengthOf(provided, 0, ProvisionalMir.encode(provisional))
  }),
)

it.effect('specializes generic suspension captures without retaining type parameters', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
struct Owner { value: i32 }
effect fn delayed<T>(value: T) -> T {
  return run Effect.suspend(effect { return move value })
}
pub fn main() -> i32 {
  let scalarValue = run delayed<i32>(1)
  let ownerValue = run delayed<Owner>(Owner { value: 2 })
  return scalarValue + ownerValue.value
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const provisional = available(self)
    const origins = outcomes(provisional).filter(
      (control): control is Extract<ProvisionalMir.Outcome, { readonly _tag: 'SuspendEffect' }> =>
        control._tag === 'SuspendEffect',
    )
    assert.lengthOf(origins, 2, ProvisionalMir.encode(provisional))
    const captures = origins.flatMap((origin) => origin.deferred.captures)
    assert.isTrue(
      captures.some((capture) => capture.type === 'i32' && capture.access === 'Take'),
      Json.stringify(captures),
    )
    assert.isTrue(
      captures.some(
        (capture) =>
          typeof capture.type !== 'string' &&
          capture.type._tag === 'NominalType' &&
          capture.type.name === 'Owner' &&
          capture.access === 'Take',
      ),
    )
    assert.notInclude(ProvisionalMir.encode(provisional), 'TypeParameter')
  }),
)

it.effect('emits no suspension control for a closed synchronous corpus', () =>
  Effect.gen(function* () {
    const source = `import silk.effect { Effect }
effect fn seed(value: i32) -> i32 { return value }
fn increment(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 { return run seed(41) |> Effect.map(increment) }`
    const first = yield* snapshot(source)
    const second = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(first), [])
    const firstMir = available(first)
    const secondMir = available(second)
    assert.deepEqual(ProvisionalMir.verify(firstMir), [])
    assert.deepEqual(outcomes(firstMir), [])
    assert.strictEqual(ProvisionalMir.encode(firstMir), ProvisionalMir.encode(secondMir))
  }),
)

it.effect('converges through a deep suspension chain independently of discovery order', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
effect fn level0(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
effect fn level1(value: i32) -> i32 { return run level0(value) }
effect fn level2(value: i32) -> i32 { return run level1(value) }
effect fn level3(value: i32) -> i32 { return run level2(value) }
effect fn level4(value: i32) -> i32 { return run level3(value) }
effect fn level5(value: i32) -> i32 { return run level4(value) }
effect fn level6(value: i32) -> i32 { return run level5(value) }
effect fn level7(value: i32) -> i32 { return run level6(value) }
pub fn main() -> i32 {
  return run level7(42)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(self.layout._tag, 'Available')
    if (self.layout._tag !== 'Available') return

    const forward = ProvisionalMir.build(
      self.instances,
      self.layout.value,
      Analysis.declarationIndex(self),
    )
    const reversed = ProvisionalMir.build(
      Object.freeze({
        ...self.instances,
        instances: Object.freeze([...self.instances.instances].reverse()),
      }),
      self.layout.value,
      Analysis.declarationIndex(self),
    )
    assert.deepEqual(ProvisionalMir.verify(forward), [])
    assert.deepEqual(ProvisionalMir.verify(reversed), [])
    const compareIdentities = (left: string, right: string): number => {
      if (left < right) return -1
      if (left > right) return 1
      return 0
    }
    const classifications = (module_: ProvisionalMir.Module) =>
      module_.executions
        .map((execution) => [execution.key.identity, execution.classification] as const)
        .sort(([left], [right]) => compareIdentities(left, right))
    assert.deepEqual(classifications(forward), classifications(reversed))
    assert.isAtLeast(
      outcomes(forward).filter((outcome) => outcome._tag === 'RunSuspendableEffect').length,
      8,
      ProvisionalMir.encode(forward),
    )
  }),
)
