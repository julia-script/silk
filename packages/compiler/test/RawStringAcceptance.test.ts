import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
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
    JSON.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
  )
  if (evaluated._tag === 'Completed') {
    assert.strictEqual(evaluated.result.value, BigInt(expected))
  }

  const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
  assert.strictEqual((instance.exports.silk_main as () => number)(), expected)
})

/**
 * `r"\n"` is a backslash and an `n`; `"\n"` is one LF. The raw form keeps the escape vocabulary
 * out of its body entirely, so a regular expression or a Windows path needs no doubled backslash.
 */
const escapePolicy = `import silk.string { byteLength, utf8Bytes }

pub fn main() -> i32 {
  let raw = r"\\n"
  let escaped = "\\n"
  if usize.toI32(byteLength(raw)) == 2 {} else { return 1 }
  if usize.toI32(byteLength(escaped)) == 1 {} else { return 2 }

  let rawBytes = utf8Bytes(raw)
  if rawBytes[usize.ZERO] == 92 {} else { return 3 }
  if rawBytes[usize.add(1, 0)] == 110 {} else { return 4 }
  if utf8Bytes(escaped)[usize.ZERO] == 10 {} else { return 5 }

  let decimalPattern = r"\\d+\\.\\d+"
  let windowsPath = r"C:\\Users\\build"
  if usize.toI32(byteLength(decimalPattern)) == 8 {} else { return 6 }
  if usize.toI32(byteLength(windowsPath)) == 14 {} else { return 7 }
  return 42
}`

it.effect(
  'decodes a raw literal body without escapes on the evaluator and Wasm',
  () => assertRunsEverywhere('raw-string/escape-policy', escapePolicy, 42),
  60_000,
)

/** The triple-delimited raw form takes the same body policy as its single-delimited form. */
const multiline = `import silk.string { byteLength, utf8Bytes }

pub fn main() -> i32 {
  let helpText = r"""
Usage: silk build
  --target \\path\\to\\dir
"""
  let bytes = utf8Bytes(helpText)
  if bytes[usize.ZERO] == 10 {} else { return 1 }
  // A backslash inside the body is content, exactly as in the single-delimited form.
  if bytes[usize.add(30, 0)] == 92 {} else { return 2 }
  if usize.toI32(byteLength(helpText)) == 43 {} else { return 3 }
  return 42
}`

it.effect(
  'keeps the raw body policy across both delimiter widths on the evaluator and Wasm',
  () => assertRunsEverywhere('raw-string/multiline', multiline, 42),
  60_000,
)

it.effect('gives a raw literal the string type, so it passes where a text literal does', () =>
  Effect.gen(function* () {
    // `byteLength` accepts `string` only, so accepting this call is the type evidence.
    const source = `import silk.string { byteLength }

fn measure(value: string) -> usize { return byteLength(value) }

pub fn main() -> i32 {
  return usize.toI32(measure(r"\\d")) + usize.toI32(measure("d"))
}`
    const snapshot = yield* Analysis.ofSourceRealized('raw-string/type', encoder.encode(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 3n)
  }),
)

it.effect('rejects a raw literal whose body is not valid UTF-8', () =>
  Effect.gen(function* () {
    const bytes = Uint8Array.from([
      ...encoder.encode('pub fn main() -> i32 { let value = r"'),
      0xff,
      ...encoder.encode('" return 0 }'),
    ])
    const snapshot = yield* Analysis.ofSourceRealized('raw-string/invalid-utf8', bytes)
    assert.isAbove(Analysis.diagnostics(snapshot).length, 0)
  }),
)
