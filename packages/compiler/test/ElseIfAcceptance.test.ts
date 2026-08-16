import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const chained = `fn classify(value: i32) -> i32 {
  if value < 0 {
    return 0
  } else if value < 10 {
    return 1
  } else if value < 100 {
    return 2
  } else {
    return 3
  }
  return 4
}

pub fn main() -> i32 {
  if classify(0 - 5) == 0 {} else { return 1 }
  if classify(5) == 1 {} else { return 2 }
  if classify(50) == 2 {} else { return 3 }
  if classify(500) == 3 {} else { return 4 }
  return 42
}`

it.effect(
  'takes the first matching arm of a chain on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'else-if/chain',
        ascii(chained),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, (_, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  60_000,
)

it.effect('keeps a chained arm equal to the same chain written as nested blocks', () =>
  Effect.gen(function* () {
    const nested = `fn classify(value: i32) -> i32 {
  if value < 0 {
    return 0
  } else {
    if value < 10 {
      return 1
    } else {
      if value < 100 {
        return 2
      } else {
        return 3
      }
    }
  }
  return 4
}

pub fn main() -> i32 {
  if classify(0 - 5) == 0 {} else { return 1 }
  if classify(5) == 1 {} else { return 2 }
  if classify(50) == 2 {} else { return 3 }
  if classify(500) == 3 {} else { return 4 }
  return 42
}`

    for (const [name, source] of [
      ['else-if/chained', chained],
      ['else-if/nested', nested],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source))
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)
    }
  }),
)

it.effect('reports a chained arm whose condition is not a boolean', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'else-if/condition-type',
      ascii(`pub fn main() -> i32 {
  if false {
    return 1
  } else if 2 {
    return 2
  }
  return 0
}`),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0011',
    )
  }),
)
