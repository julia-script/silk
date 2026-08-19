import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** Swap a scalar field through an exclusive reference and observe both halves. */
const scalarSwap = `struct Counter {
  value: i32
}
fn bump(self: &mut Counter) -> i32 {
  let old = Intrinsic.replace(self.value, 42)
  return old
}
pub fn main() -> i32 {
  let mut counter = Counter { value: 41 }
  let old = bump(&mut counter)
  return old + counter.value - 41
}`

/**
 * Swap a union field through an exclusive reference: the affine payload moves out through the
 * swap while the place stays initialized, so no partial move exists at any point.
 */
const unionSwap = `struct Empty {}
struct Full { value: i32 }
struct Cell {
  state: Empty | Full
}
fn take(self: &mut Cell) -> i32 {
  let old = Intrinsic.replace(self.state, Empty {})
  return match move old {
    Empty {} => 0
    Full { value } => value
  }
}
pub fn main() -> i32 {
  let mut cell = Cell { state: Full { value: 42 } }
  let first = take(&mut cell)
  let second = take(&mut cell)
  return first + second
}`

it.effect('swaps places atomically on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    for (const [name, source, expected] of [
      ['scalar', scalarSwap, 42],
      ['union', unionSwap, 42],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `place-replace/${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', name)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, BigInt(expected), name)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)
    }
  }),
)

it.effect('rejects invalid replace places with assignment diagnostics', () =>
  Effect.gen(function* () {
    const immutable = yield* Analysis.ofSourceRealized(
      'place-replace/immutable',
      ascii(`pub fn main() -> i32 {
  let value = 1
  let old = Intrinsic.replace(value, 2)
  return old
}`),
    )
    assert.include(
      Analysis.diagnostics(immutable).map((diagnostic) => diagnostic.code),
      'SEM0035',
    )

    const sharedRoot = yield* Analysis.ofSourceRealized(
      'place-replace/shared',
      ascii(`struct Counter {
  value: i32
}
fn peek(self: &Counter) -> i32 {
  let old = Intrinsic.replace(self.value, 2)
  return old
}
pub fn main() -> i32 { return 0 }`),
    )
    assert.include(
      Analysis.diagnostics(sharedRoot).map((diagnostic) => diagnostic.code),
      'SEM0036',
    )

    const missingRoot = yield* Analysis.ofSourceRealized(
      'place-replace/missing',
      ascii(`pub fn main() -> i32 {
  let old = Intrinsic.replace(missing, 2)
  return old
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(missingRoot).map((diagnostic) => diagnostic.code),
      ['SEM0006'],
    )

    const arity = yield* Analysis.ofSourceRealized(
      'place-replace/arity',
      ascii(`pub fn main() -> i32 {
  let mut value = 1
  let old = Intrinsic.replace(value)
  return old
}`),
    )
    assert.include(
      Analysis.diagnostics(arity).map((diagnostic) => diagnostic.code),
      'SEM0007',
    )
  }),
)
