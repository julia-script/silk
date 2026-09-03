import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** The CALLABLE-002 Confirmed Example: every stage passes through a binding. */
const bindings = `fn combine(a: i32, b: i32, c: i32) -> i32 {
  return a + b + c
}

fn staged() -> i32 {
  let withThree = combine(3)
  let withTwoAndThree = withThree(2)
  return withTwoAndThree(1)
}
pub fn main() -> i32 { return staged() + 36 }`

/** The captured suffix must keep its position: 1 + 10 * 2 + 100 * 3. */
const ordered = `fn combine(a: i32, b: i32, c: i32) -> i32 { return a + 10 * b + 100 * c }
pub fn main() -> i32 {
  let withThree = combine(3)
  let withTwoAndThree = withThree(2)
  return withTwoAndThree(1) - 321 + 42
}`

/** A stage over an erased callable parameter whose environment is only known per instance. */
const parameter = `fn combine(a: i32, b: i32, c: i32) -> i32 { return a * 100 + b * 10 + c }
fn stage(f: fn(i32, i32) -> i32) -> i32 {
  let g = f(2)
  return g(1)
}
pub fn main() -> i32 { return stage(combine(3)) - 123 + 42 }`

/** A stage over a bare function item passed as a parameter, so the base has no environment. */
const item = `fn combine(a: i32, b: i32, c: i32) -> i32 { return a * 100 + b * 10 + c }
fn stage(f: fn(i32, i32, i32) -> i32) -> i32 {
  let g = f(2, 3)
  return g(1)
}
pub fn main() -> i32 { return stage(combine) - 123 + 42 }`

/** A staged callable handed on to a further function, so its identity must survive as a value. */
const forwarded = `fn combine(a: i32, b: i32, c: i32) -> i32 { return a * 100 + b * 10 + c }
fn apply(f: fn(i32) -> i32, v: i32) -> i32 { return f(v) }
fn stage(f: fn(i32, i32) -> i32) -> i32 {
  let g = f(2)
  return apply(g, 1)
}
pub fn main() -> i32 { return stage(combine(3)) - 123 + 42 }`

/** A shared stage stays reusable: the base environment is copied, not consumed. */
const reused = `fn combine(a: i32, b: i32, c: i32) -> i32 { return a * 100 + b * 10 + c }
pub fn main() -> i32 {
  let withThree = combine(3)
  let one = withThree(2)
  let two = withThree(5)
  return one(1) + two(4) - 123 - 453 + 42
}`

const programs = [
  ['bindings', bindings],
  ['ordered', ordered],
  ['parameter', parameter],
  ['item', item],
  ['forwarded', forwarded],
  ['reused', reused],
] as const

it.effect(
  'stages a section through a binding or parameter and agrees on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      for (const [name, source] of programs) {
        const snapshot = yield* Analysis.ofSourceRealized(
          `staged-section/${name}`,
          ascii(source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [], name)

        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(
          evaluated._tag,
          'Completed',
          `${name}: ${Json.stringify(evaluated, (_, value) =>
            typeof value === 'bigint' ? value.toString() : value,
          )}`,
        )
        if (evaluated._tag !== 'Completed') return
        assert.strictEqual(evaluated.result.value, 42n, `${name} evaluator`)

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual((instance.exports.silk_main as () => number)(), 42, `${name} wasm`)
      }
    }),
  120_000,
)

it.effect('still rejects supplying more arguments than a section awaits', () =>
  Effect.gen(function* () {
    const source = `fn combine(a: i32, b: i32, c: i32) -> i32 { return a + b + c }
pub fn main() -> i32 {
  let withThree = combine(3)
  return withThree(1, 2, 4)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'staged-section/too-many',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0007'],
    )
  }),
)
