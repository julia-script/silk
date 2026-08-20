import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized(
    'effect-suspension-composition/main',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )

const inspect = (value: unknown): string =>
  JSON.stringify(value, (_key, member) => (typeof member === 'bigint' ? `${member}n` : member), 2)

const assertCompleted = (self: Analysis.Snapshot, expected: number): void => {
  assert.deepEqual(Analysis.diagnostics(self), [])
  const outcome = Analysis.evaluate(self)
  assert.strictEqual(outcome._tag, 'Completed', inspect(outcome))
  if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, BigInt(expected))
}

it.effect('composes through map and flatMap without suspension-specific channels', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effects as Effect
effect fn delayed(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
fn increment(value: i32) -> i32 { return value + 1 }
fn next(value: i32) -> Effect<i32> { return delayed(value + 1) }
pub fn main() -> i32 {
  let mapped = delayed(40) |> Effect.map(increment)
  return run mapped |> Effect.flatMap(next)
}`)
    assertCompleted(self, 42)
    const program = Analysis.loweredMir(self)
    assert.deepEqual(Mir.verify(program), [])
    const suspensionOutcomes = program.functions.flatMap((fn) =>
      (fn.suspension?.regions ?? []).map((region) =>
        region._tag === 'SuspendEffectRegion' ? region.deferred.outcome : region.runner.outcome,
      ),
    )
    assert.isAbove(suspensionOutcomes.length, 0)
    for (const outcome of suspensionOutcomes) {
      assert.deepEqual(Type.failureMembers(outcome), [])
      assert.deepEqual(Type.requirementMembers(outcome), [])
    }
  }),
)

it.effect('composes through recovery and retry using only declared failures', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effects as Effect
struct BoomError {}
effect fn delayed(value: i32) -> i32 ! BoomError {
  return run Effect.suspend(effect {
    if value < 0 { fail BoomError {} }
    return value
  })
}
effect fn recover(error: BoomError) -> i32 { return 41 }
pub fn main() -> i32 {
  let retried = delayed(42) |> Effect.retry(2) |> Effect.catchAll(recover)
  let recovered = delayed(-1) |> Effect.catchAll(recover)
  return (run retried) + (run recovered) - 41
}`)
    assertCompleted(self, 42)
    const encoded = Mir.encode(Analysis.loweredMir(self))
    assert.notInclude(encoded, 'OutOfMemoryError')
    assert.notInclude(encoded, 'ContinuationRequest')
  }),
)

it.effect('preserves provisioned requirements without adding an allocator requirement', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effects as Effect
service Counter {
  effect fn get() -> i32 ? &Counter
}
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
impl Counter for Fixed { get: Fixed.get }
effect fn read() -> i32 ? &Counter {
  return run Effect.suspend(effect { return run Counter.get() })
}
pub fn main() -> i32 {
  let provider = Fixed { value: 42 }
  return run Effect.provide(read(), &provider)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const program = Analysis.loweredMir(self)
    for (const fn of program.functions)
      for (const region of fn.suspension?.regions ?? []) {
        const runner = region._tag === 'SuspendEffectRegion' ? region.deferred : region.runner
        assert.isFalse(
          runner.providers.some(
            (provider) =>
              provider.capability.module === 'silk/core' &&
              provider.capability.name === 'Allocator',
          ),
        )
        assert.isTrue(
          runner.providers.every((provider) => provider.purposes.join(',') === 'ChildRequirement'),
        )
      }
  }),
)

it.effect('allows an equivalent user combinator to preserve exact channels', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effects as Effect
effect fn preserve<A, E, ?R>(
  self: once Effect<A ! E ? R>
) -> A ! E ? R {
  return run self
}
effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 42 })
}
pub fn main() -> i32 { return run preserve(delayed()) }`)
    assertCompleted(self, 42)
  }),
)

it.effect('keeps nested Effect success nested until explicitly run again', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effects as Effect
effect fn nested() -> Effect<i32> {
  return run Effect.suspend(effect {
    return effect { return 42 }
  })
}
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const expressionTypes = Analysis.expressionsOf(
      self,
      'effect-suspension-composition/main',
    ).flatMap((expression) =>
      expression.type._tag === 'Available' ? [Type.encode(expression.type.type)] : [],
    )
    assert.isTrue(expressionTypes.some((type) => type.startsWith('Effect<Effect<i32>')))
  }),
)

it.effect('emits frame lifecycle traces without allocation transactions', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effects as Effect
effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 42 })
}
effect fn program() -> i32 {
  let value = run delayed()
  return value
}
pub fn main() -> i32 { return run program() }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    const tags = outcome.trace.map((event) => event._tag)
    assert.include(tags, 'CoroutineFramePush')
    assert.include(tags, 'CoroutineFrameStateTransition')
    assert.include(tags, 'CoroutineFrameResume')
    assert.include(tags, 'CoroutineFrameComplete')
    assert.notInclude(tags, 'AllocationAcquire')
    assert.notInclude(tags, 'AllocationRelease')
  }),
)
