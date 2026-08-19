import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * Every manifest namespace is auto-injected into user scope, so a program names Option, Result,
 * and Vector as qualified actors without writing a single import statement.
 */
const qualified = `fn present(value: Option<i32>) -> i32 {
  return match move value {
    None {} => 0
    Some<i32> { value: carried } => carried
  }
}

fn settled(value: Result<i32, i32>) -> i32 {
  drop value
  return 2
}

pub fn main() -> i32 {
  let values = Vector.make<i32>()
  drop values
  return present(Option.some<i32>(40)) + settled(Result.succeed<i32, i32>(1))
}`

/** The selective import form keeps resolving the same members alongside the injected namespaces. */
const selective = `import silk.vector { Vector, make }
import silk.option { Option, Some, None, some }
import silk.result { Result, Success, Failure, succeed }

fn settled(value: Result<i32, i32>) -> i32 {
  return match move value {
    Result<i32, i32> { value: outcome } => match move outcome {
      Success<i32> { value: carried } => carried
      Failure<i32> { error: failure } => failure
    }
  }
}

fn present(value: Option<i32>) -> i32 {
  return match move value {
    None {} => 0
    Some<i32> { value: carried } => carried
  }
}

pub fn main() -> i32 {
  let values = make<i32>()
  drop values
  return present(some<i32>(40)) + settled(succeed<i32, i32>(2))
}`

const agrees = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')
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
    assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  })

it.effect(
  'resolves Option, Result, and Vector namespaces with no import on the evaluator and Wasm',
  () => agrees('stdlib-namespace/qualified', qualified),
  60_000,
)

it.effect(
  'keeps the selective import form compiling alongside the injected namespaces',
  () => agrees('stdlib-namespace/selective', selective),
  60_000,
)
