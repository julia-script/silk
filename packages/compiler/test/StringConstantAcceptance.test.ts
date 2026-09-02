import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

const assertRunsEverywhere = Effect.fnUntraced(function* (
  sourceId: string,
  source: string,
  expected: number,
) {
  const bytes = encoder.encode(source)
  const snapshot = yield* Analysis.ofSourceRealized(sourceId, bytes, 'wasm32-unknown-unknown')
  assert.deepEqual(Analysis.diagnostics(snapshot), [])

  const evaluated = Analysis.evaluate(snapshot)
  assert.strictEqual(
    evaluated._tag,
    'Completed',
    Json.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
  )
  if (evaluated._tag === 'Completed') {
    assert.strictEqual(evaluated.result.value, BigInt(expected))
  }

  const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
  assert.strictEqual((instance.exports.silk_main as () => number)(), expected)
})

/**
 * A module-level string constant reads as the literal it names. The escaped and raw constants
 * below decode to identical bytes, so each must equal the inline literal it stands for.
 */
const readsConstants = `import silk.usize as usize
import silk.string { String }

const escapedPattern: string = "\\\\d+\\\\.\\\\d+"
const rawPattern: string = r"\\d+\\.\\d+"
const windowsPath: string = r"C:\\Users\\build"

fn sameText(left: string, right: string) -> bool {
  if String.byteLength(left) == String.byteLength(right) {} else { return false }
  let leftBytes = String.utf8Bytes(left)
  let rightBytes = String.utf8Bytes(right)
  let mut index = usize.ZERO
  while index < leftBytes.length {
    if leftBytes[index] == rightBytes[index] {} else { return false }
    index = index + usize.ONE
  }
  return true
}

pub fn main() -> i32 {
  if sameText(escapedPattern, r"\\d+\\.\\d+") {} else { return 1 }
  if sameText(rawPattern, "\\\\d+\\\\.\\\\d+") {} else { return 2 }
  if sameText(rawPattern, escapedPattern) {} else { return 3 }
  if usize.toI32(String.byteLength(rawPattern)) == 8 {} else { return 4 }
  if usize.toI32(String.byteLength(windowsPath)) == 14 {} else { return 5 }
  // A named constant passes wherever a string value is accepted.
  if sameText(windowsPath, r"C:\\Users\\build") {} else { return 6 }
  return 42
}`

it.effect(
  'reads module-level string constants on the evaluator and Wasm',
  () => assertRunsEverywhere('string-constant/reads', readsConstants, 42),
  60_000,
)
