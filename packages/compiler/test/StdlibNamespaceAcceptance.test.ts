import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import { portableRandomCapabilities, seededRandomFingerprint } from './support/corpus.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * Every manifest namespace is auto-injected into user scope, so a program names Option, Result,
 * and Vector as qualified actors without writing a single import statement.
 */
const qualified = `import silk.option { None }
import silk.option { Option }
import silk.option { Some }
import silk.result { Result }
import silk.vector { Vector }
fn present(value: Option<i32>) -> i32 {
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

it.effect('resolves selected scope actors for nonprimitive operation modules', () =>
  Effect.gen(function* () {
    const source = `import silk.execution { Execution }
import silk.format { Format }
import silk.hash { Hash }
import silk.metrics { Metrics }
import silk.numeric { Numeric }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
import silk.unicode { Unicode }
import silk.unicode_tables { UnicodeTables }

fn rawCount(buffer: &RawBuffer<i32>) -> usize {
  unsafe { return RawBuffer.count<i32>(buffer) }
  return 0
}

fn take(slot: Slot<i32>) -> i32 {
  unsafe { return Slot.take<i32>(move slot) }
  return 0
}

fn notify(execution: &mut Intrinsic.Execution<i32>) -> () {
  return Execution.notifyInitial<i32>(move execution)
}

pub fn main() -> i32 {
  let parsed = Format.signedValue("42")
  let seed = Hash.seed(17)
  let metrics = Metrics.make()
  let answer = Numeric.add<i32>(40, 2)
  let unicodeVersion = Unicode.dataVersion()
  let tableVersion = UnicodeTables.dataVersion()
  drop parsed
  drop seed
  drop metrics
  drop unicodeVersion
  drop tableVersion
  return answer
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'stdlib-namespace/scope-actors',
      ascii(source),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

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

it.effect(
  'runs the seeded InsecureRandom fingerprint on the evaluator and direct WebAssembly',
  () => agrees('stdlib-namespace/random', seededRandomFingerprint),
  60_000,
)

it.effect(
  'runs portable secure Random and InsecureSeed providers on the evaluator and direct WebAssembly',
  () => agrees('stdlib-namespace/random-capabilities', portableRandomCapabilities),
  60_000,
)
