import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized(
    'effect-suspension-composition/main',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )

it.effect('preserves provisioned requirements without adding an allocator requirement', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
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
              provider.capability.module === 'silk/allocator' &&
              provider.capability.name === 'Allocator',
          ),
        )
        assert.isTrue(
          runner.providers.every((provider) => provider.purposes.join(',') === 'ChildRequirement'),
        )
      }
  }),
)

it.effect('keeps nested Effect success nested until explicitly run again', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
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
