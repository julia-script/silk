import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('numeric-interface/main', encoder.encode(source))

it.effect('specializes generic integer addition to one concrete primitive', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.numeric { add }
pub fn main() -> i32 {
  return add(20, 22)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result._tag, 'I32Value')
      if (outcome.result._tag === 'I32Value') assert.strictEqual(outcome.result.value, 42)
    }

    const operations = Analysis.loweredMir(self).functions.flatMap(Mir.operations)
    const binary = operations.filter(
      (operation) => operation._tag === 'Binary' && operation.operator === 'Add',
    )
    assert.strictEqual(binary.length, 1)
    if (binary[0]?._tag === 'Binary') assert.deepEqual(binary[0].type, { _tag: 'i32' })
    assert.isFalse(
      operations.some((operation) =>
        ['Switch', 'Provide', 'ServiceCall', 'ServiceEffectConstruct'].includes(operation._tag),
      ),
    )
  }),
)

it.effect('rejects a type without the static Integer conformance', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.numeric { add }
pub fn main() -> bool {
  return add(true, false)
}`)
    assert.isTrue(
      Analysis.diagnostics(self).some((diagnostic) =>
        diagnostic.message.includes('bool does not implement Integer'),
      ),
    )
  }),
)
