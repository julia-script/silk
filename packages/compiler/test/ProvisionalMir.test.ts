import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('provisional-mir/main', encoder.encode(source))

const available = (self: Analysis.Snapshot): ProvisionalMir.Module => {
  const provisional = Analysis.provisionalMirOf(self)
  assert.strictEqual(provisional._tag, 'Available')
  if (provisional._tag === 'Available') return provisional.value
  throw new RangeError('expected provisional MIR')
}

const outcomes = (self: ProvisionalMir.Module): ReadonlyArray<ProvisionalMir.Outcome> =>
  self.executions.flatMap((execution) => execution.regions.map((region) => region.outcome))

const recover = `effect fn recover(error: OutOfMemory) -> i32 { return 0 }`

it.effect('separates the canonical suspension origin from complete-or-relay callers', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`${recover}
effect fn delayed(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return value })
}
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let pending = delayed(42) |> Effect.provideMut(&mut allocator)
  return run Effect.catch(move pending, recover)
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
      assert.strictEqual(origin.transfer._tag, 'OriginateUnpublishedTransfer')
      assert.strictEqual(origin.span.sourceId, 'silk/effects')
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

it.effect('retains Reify completion for ordinary source-defined combinators', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`${recover}
effect fn seed(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return value })
}
fn increment(value: i32) -> i32 { return value + 1 }
effect fn program() -> i32 ! OutOfMemory ? &mut Allocator {
  return run seed(41) |> Effect.map(increment)
}
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let pending = program() |> Effect.provideMut(&mut allocator)
  return run Effect.catch(move pending, recover)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const provisional = available(self)
    assert.deepEqual(ProvisionalMir.verify(provisional), [])
    assert.isTrue(
      outcomes(provisional).some(
        (control) => control._tag === 'RunSuspendableEffect' && control.completion._tag === 'Reify',
      ),
      ProvisionalMir.encode(provisional),
    )
  }),
)

it.effect('classifies a selected suspending service implementation on the provided runner', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`service Value {
  effect fn get() -> i32 ! OutOfMemory ? &Value | &mut Allocator
}
struct SuspendedValue { value: i32 }
effect fn get(self: &SuspendedValue) -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return self.value })
}
impl Value for SuspendedValue { get: SuspendedValue.get }
effect fn read() -> i32 ! OutOfMemory ? &Value | &mut Allocator {
  return run Value.get()
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let provider = SuspendedValue { value: 42 }
  let mut allocator = SystemAllocator.make()
  let selected = read() |> Effect.provide(&provider)
  let complete = move selected |> Effect.provideMut(&mut allocator)
  return run Effect.catch(move complete, recover)
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

it.effect('specializes generic suspension captures without retaining type parameters', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Owner { value: i32 }
effect fn delayed<T>(value: T) -> T ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return move value })
}
effect fn recoverScalar(error: OutOfMemory) -> i32 { return 0 }
effect fn recoverOwner(error: OutOfMemory) -> Owner { return Owner { value: 0 } }
pub fn main() -> i32 {
  let mut scalarAllocator = SystemAllocator.make()
  let scalar = delayed<i32>(1) |> Effect.provideMut(&mut scalarAllocator)
  let scalarValue = run Effect.catch(move scalar, recoverScalar)
  let mut ownerAllocator = SystemAllocator.make()
  let owner = delayed<Owner>(Owner { value: 2 }) |> Effect.provideMut(&mut ownerAllocator)
  let ownerValue = run Effect.catch(move owner, recoverOwner)
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
      JSON.stringify(captures),
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
    const source = `effect fn seed(value: i32) -> i32 { return value }
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
