import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('reads and writes fields through nominal references on both targets', () =>
  Effect.gen(function* () {
    const source = `struct Counter { value: I32 }

fn bump(self: &mut Counter) -> I32 {
  self.value = self.value + 1
  return self.value
}

fn peek(self: &Counter) -> I32 {
  return self.value
}

pub fn main() -> I32 {
  let mut counter = Counter { value: 40 }
  let bumped = bump(&mut counter)
  let again = bump(&mut counter)
  return again + peek(&counter) - again
}`
    for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown']) {
      const snapshot = yield* Analysis.ofSource(
        'reference-projection/counter',
        ascii(source),
        target,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], target)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', target)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, 42, target)
    }
    const wasm = yield* Analysis.codegenWasm(
      yield* Analysis.ofSource(
        'reference-projection/counter',
        ascii(source),
        'wasm32-unknown-unknown',
      ),
      { mode: 'release' },
    )
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('keeps reference projection inside the borrow contract', () =>
  Effect.gen(function* () {
    // Writing through a shared reference is not a writable place.
    const shared = yield* Analysis.ofSource(
      'reference-projection/shared-write',
      ascii(`struct Counter { value: I32 }
fn bump(self: &Counter) -> I32 {
  self.value = 1
  return self.value
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(shared).map((diagnostic) => diagnostic.code),
      ['SEM0036'],
    )

    // Consuming a field through a reference stays a partial move.
    const stolen = yield* Analysis.ofSource(
      'reference-projection/steal',
      ascii(`struct Token { value: I32 }
struct Holder { token: Token }
fn steal(self: &mut Holder) -> Token {
  return move self.token
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(stolen).map((diagnostic) => diagnostic.code),
      ['OWN0002'],
    )
  }),
)
