import { readFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

/**
 * The pinned Unicode conformance data, run through the standard library's own normalizer.
 *
 * Hand-picked examples pass while an implementation is subtly wrong — a composition exclusion left
 * in, a combining class off by one, a blocked mark composed anyway. `NormalizationTest.txt` is the
 * file that catches those, so the whole of it runs here rather than a selection of it.
 *
 * Each case asserts the UAX #15 invariants for the two canonical forms:
 *
 *   NFC(c1) == NFC(c2) == NFC(c3) == c2
 *   NFD(c1) == NFD(c2) == NFD(c3) == c3
 *
 * The compatibility columns are not checked; NFKC and NFKD are out of scope for #42.
 *
 * The corpus travels into the program as one packed byte string the Silk harness walks, rather than
 * as one call statement per case. That is a speed decision with a measured basis: as call
 * statements the corpus costs about 14 ms of analysis per case, which is over four minutes for the
 * whole file, while a packed corpus makes analysis a constant and hands the actual work to compiled
 * WebAssembly, where it is nearly free. The chunking exists because a program's static data has to
 * fit below the wasm heap table at 64 KiB, which the Unicode tables already occupy 19 KiB of.
 */

const dataRoot = join(dirname(fileURLToPath(import.meta.url)), '..', 'unicode-data')

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

interface Case {
  readonly line: number
  readonly source: ReadonlyArray<number>
  readonly nfc: ReadonlyArray<number>
  readonly nfd: ReadonlyArray<number>
}

const parse = (): ReadonlyArray<Case> => {
  const text = readFileSync(join(dataRoot, 'NormalizationTest.txt'), 'utf8')
  const points = (field: string) =>
    field
      .split(' ')
      .filter((value) => value !== '')
      .map((value) => Number.parseInt(value, 16))
  const cases: Array<Case> = []
  text.split('\n').forEach((raw, index) => {
    const line = raw.trim()
    if (line === '' || line.startsWith('#') || line.startsWith('@')) return
    const columns = line.split(';')
    cases.push({
      line: index + 1,
      source: points(columns[0]),
      nfc: points(columns[1]),
      nfd: points(columns[2]),
    })
  })
  return cases
}

const utf8 = (points: ReadonlyArray<number>): Uint8Array =>
  new TextEncoder().encode(String.fromCodePoint(...points))

/** One case as `[length][bytes]` for each of the source, NFC, and NFD columns. */
const encode = (entry: Case): ReadonlyArray<number> =>
  [entry.source, entry.nfc, entry.nfd].flatMap((points) => {
    const bytes = utf8(points)
    if (bytes.length > 255)
      throw new Error(`conformance field at line ${entry.line} exceeds a byte`)
    return [bytes.length, ...bytes]
  })

const literal = (bytes: ReadonlyArray<number>): string =>
  `b"${bytes.map((byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`

/**
 * A program returning the number of failing assertions across its chunk.
 *
 * The harness is ordinary Silk calling the same public API a user program would, comparing with
 * ordinary exact `string` equality — the equality this issue must leave alone.
 */
const program = (
  cases: ReadonlyArray<Case>,
): string => `import silk.result { Result, Success, Failure }
import silk.string { String, InvalidUtf8, fromUtf8, ownedUtf8Bytes }
import silk.unicode { normalizeNfc, normalizeNfd }
import silk.vector { Vector, make as vectorMake, append as vectorAppend, asSlice as vectorAsSlice }

/// Copies one length-prefixed field out of the corpus so it can be validated into text.
effect fn field(
  data: &[u8],
  start: usize,
  count: usize,
) -> Vector<u8> ! OutOfMemory ? &mut Allocator {
  let mut buffer = vectorMake<u8>()
  let mut index = usize.ZERO
  while index < count {
    let appended = run vectorAppend<u8>(&mut buffer, data[start + index])
    index = index + usize.ONE
  }
  return move buffer
}

fn same(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index = usize.ZERO
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

/// Normalizes one input both ways and checks it against the expected forms.
///
/// Comparison is on bytes rather than on a second borrowed \`string\` because the wasm backend
/// invalidates a heap-backed \`string\` once another one is derived before the first is read; see
/// the note on this test in the pull request. Exact \`string\` equality itself is checked in
/// \`UnicodeNormalization.test.ts\`, on values that do not go through this corpus reader.
effect fn checkFrom(
  text: string,
  nfc: &[u8],
  nfd: &[u8],
) -> i32 ! OutOfMemory ? &mut Allocator {
  let mut failures = 0
  let composed = run normalizeNfc(text)
  if same(ownedUtf8Bytes(&composed), nfc) {} else { failures = failures + 1 }
  let decomposed = run normalizeNfd(text)
  if same(ownedUtf8Bytes(&decomposed), nfd) {} else { failures = failures + 1 }
  return failures
}

effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let data = ${literal(cases.flatMap(encode))}
  let mut offset = usize.ZERO
  let mut remaining = usize.add(0, ${cases.length})
  let mut failures = 0
  while usize.ZERO < remaining {
    let sourceLength = u8.toUsize(data[offset])
    let sourceBytes = run field(data, offset + usize.ONE, sourceLength)
      |> Effect.provideMut(&mut allocator)
    offset = offset + usize.ONE + sourceLength
    let nfcLength = u8.toUsize(data[offset])
    let nfcBytes = run field(data, offset + usize.ONE, nfcLength)
      |> Effect.provideMut(&mut allocator)
    offset = offset + usize.ONE + nfcLength
    let nfdLength = u8.toUsize(data[offset])
    let nfdBytes = run field(data, offset + usize.ONE, nfdLength)
      |> Effect.provideMut(&mut allocator)
    offset = offset + usize.ONE + nfdLength
    let sourceText = match move fromUtf8(vectorAsSlice<u8>(&sourceBytes)) {
      Result<string, InvalidUtf8> { value: outcome } => match move outcome {
        Success<string> { value } => value
        Failure<InvalidUtf8> { error } => ""
      }
    }
    let fromSource = run checkFrom(
      sourceText,
      vectorAsSlice<u8>(&nfcBytes),
      vectorAsSlice<u8>(&nfdBytes),
    ) |> Effect.provideMut(&mut allocator)
    let nfcText = match move fromUtf8(vectorAsSlice<u8>(&nfcBytes)) {
      Result<string, InvalidUtf8> { value: outcome } => match move outcome {
        Success<string> { value } => value
        Failure<InvalidUtf8> { error } => ""
      }
    }
    let fromNfc = run checkFrom(
      nfcText,
      vectorAsSlice<u8>(&nfcBytes),
      vectorAsSlice<u8>(&nfdBytes),
    ) |> Effect.provideMut(&mut allocator)
    let nfdText = match move fromUtf8(vectorAsSlice<u8>(&nfdBytes)) {
      Result<string, InvalidUtf8> { value: outcome } => match move outcome {
        Success<string> { value } => value
        Failure<InvalidUtf8> { error } => ""
      }
    }
    let fromNfd = run checkFrom(
      nfdText,
      vectorAsSlice<u8>(&nfcBytes),
      vectorAsSlice<u8>(&nfdBytes),
    ) |> Effect.provideMut(&mut allocator)
    failures = failures + fromSource + fromNfc + fromNfd
    remaining = remaining - usize.ONE
  }
  return failures
}

effect fn recover(error: OutOfMemory) -> i32 { return -1 }

pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

/** Static data has to clear the wasm heap table at 64 KiB, and the Unicode tables take 19 KiB. */
const chunkBudget = 32 * 1024

const chunks = (cases: ReadonlyArray<Case>): ReadonlyArray<ReadonlyArray<Case>> => {
  const grouped: Array<Array<Case>> = []
  let current: Array<Case> = []
  let size = 0
  for (const entry of cases) {
    const width = encode(entry).length
    if (size + width > chunkBudget && current.length > 0) {
      grouped.push(current)
      current = []
      size = 0
    }
    current.push(entry)
    size += width
  }
  if (current.length > 0) grouped.push(current)
  return grouped
}

const failuresIn = (name: string, cases: ReadonlyArray<Case>) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      name,
      ascii(program(cases)),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code} ${diagnostic.message}`,
      ),
      [],
    )
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    return (instance.exports.silk_main as () => number)()
  })

const spell = (points: ReadonlyArray<number>) =>
  points.map((point) => point.toString(16).toUpperCase().padStart(4, '0')).join(' ')

it.effect(
  'normalizes every case in the pinned Unicode conformance data',
  () =>
    Effect.gen(function* () {
      const cases = parse()
      assert.isAbove(cases.length, 19_000, 'the conformance corpus was read')

      const failing: Array<string> = []
      let checked = 0
      let chunkIndex = 0
      for (const chunk of chunks(cases)) {
        const failures = yield* failuresIn(`unicode-conformance/chunk-${chunkIndex}`, chunk)
        chunkIndex += 1
        checked += chunk.length
        if (failures === 0) continue
        // A chunk only fails while something is wrong, so paying for a per-case re-run to name the
        // offenders costs nothing on a healthy suite and is the whole value on a sick one.
        for (const entry of chunk) {
          const single = yield* failuresIn(`unicode-conformance/line-${entry.line}`, [entry])
          if (single === 0) continue
          failing.push(
            `line ${entry.line}: ${spell(entry.source)} ; NFC ${spell(entry.nfc)} ; NFD ${spell(entry.nfd)} (${single}/6 assertions failed)`,
          )
        }
      }

      assert.strictEqual(checked, cases.length)
      assert.deepEqual(
        failing,
        [],
        `${failing.length} of ${cases.length} conformance cases failed:\n${failing.slice(0, 40).join('\n')}`,
      )
    }),
  1_800_000,
)
