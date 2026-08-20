import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as Mir from '../src/Mir.js'
import * as WasmBackend from '../src/WasmBackend.js'
import * as WasmMain from './support/WasmMain.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct First {}
struct Second {}
fn choose(input: First | Second) -> Effect<i32> {
  return match move input {
    First {} => effect { return 41 }
    Second {} => effect { return 42 }
  }
}
pub fn main() -> i32 {
  return run choose(First {})
}`

it.effect('runs the selected member of a finite Effect join', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect-join/basic',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(Mir.verify(program), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 41n)
    const wasm = yield* Backend.emit(WasmBackend.WasmBackend, program, { mode: 'release' })
    assert.strictEqual(yield* WasmMain.invoke(wasm.bytes, 'EffectJoin.run'), 41)
  }),
)
