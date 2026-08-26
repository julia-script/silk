import { readdirSync, readFileSync } from 'node:fs'
import { dirname, join, relative } from 'node:path'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import type * as Mir from '../src/Mir.js'

/**
 * No engine contains Unicode.
 *
 * `silk/unicode` is ordinary library source and `silk/unicode_tables` is ordinary library data.
 * Normalization is a call to a Silk function, the tables are byte-string literals, and a later
 * Unicode version changes those two files and nothing else. That is a constraint on the compiler
 * rather than on the module, so it is checked where a violation would appear: in the compiler's own
 * sources, in the catalogue of operations it provides, and in the MIR of a program that really
 * normalizes text.
 *
 * The subtle way this constraint gets violated is not an intrinsic — that would be obvious — but a
 * phase that recognizes a declaration by its spelling. The last test here is the one that catches
 * that: the same algorithm over the same table encoding, written by a program under names nothing
 * could recognize, has to produce the same answers as the standard library's.
 */

const packageRoot = join(dirname(fileURLToPath(import.meta.url)), '..')
const sourceRoot = join(packageRoot, 'src')

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => `${diagnostic.code} ${diagnostic.message}`)

/**
 * A name that would betray Unicode *policy*, as opposed to the scalar decoding the language has
 * always had.
 *
 * The terms are deliberately specific. `Unicode scalar` appears throughout the compiler and always
 * has — a character literal holds one, and the lexer counts them — and matching that would say
 * nothing. What may never appear is a normalization form, a combining class, a canonical
 * decomposition, a grapheme cluster, Hangul arithmetic, or either of the two module names.
 */
const unicodeShaped =
  /silk\/unicode|unicode_tables|normalizeNf|canonicalDecomposition|canonicalComposition|canonicalCombiningClass|combiningClass|\bNF(C|D|KC|KD)\b|grapheme|hangul|\bjamo\b|composition[_ ]?exclusion|uax\s*#?\s*(15|29)/i

const walk = (directory: string): ReadonlyArray<string> =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name)
    return entry.isDirectory() ? walk(path) : [path]
  })

it('carries no Unicode policy anywhere in the compiler’s own sources', () => {
  // `Stdlib.generated.ts` is the one exception, and it is exempt for the reason that makes it safe:
  // it is generated, it embeds every standard-library module as opaque source text, and it does not
  // read any of them. Nothing else under src may mention Unicode at all.
  const files = walk(sourceRoot).filter((path) => path.endsWith('.ts'))
  assert.isAbove(files.length, 20, 'the compiler sources were read')

  const offenders = files.flatMap((path) => {
    const name = relative(packageRoot, path)
    if (name === join('src', 'Stdlib.generated.ts')) return []
    const lines = readFileSync(path, 'utf8').split('\n')
    return lines.flatMap((line, index) =>
      unicodeShaped.test(line) ? [`${name}:${index + 1}: ${line.trim()}`] : [],
    )
  })
  assert.deepEqual(
    offenders,
    [],
    `compiler sources naming Unicode policy:\n${offenders.join('\n')}`,
  )
})

it('provides no Unicode operation to any engine', () => {
  // Every intrinsic the compiler declares, by the spelling a program would write. An engine cannot
  // implement an operation the catalogue does not name, so this one assertion covers semantic
  // analysis, the HIR, the MIR, the evaluator, and both backends at once.
  const spellings = Intrinsic.all().flatMap((actor) =>
    actor.operations.map((operation) => `${actor.spelling}.${operation.spelling}`),
  )
  assert.isAbove(spellings.length, 0, 'the catalogue was read')
  assert.deepEqual(
    spellings.filter((spelling) => unicodeShaped.test(spelling)),
    [],
  )
})

/** Every operation of every region of every lowered function, in one flat sequence. */
const operationsOf = (module: Mir.Module): ReadonlyArray<Mir.Operation> =>
  module.functions.flatMap((lowered) =>
    lowered.regions.flatMap((region) => {
      if (region._tag === 'OperationRegion') return region.operations
      if (region._tag === 'CleanupRegion') return region.releases
      return []
    }),
  )

const normalizing = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.string { String, view }
import silk.unicode { normalizeNfc, normalizeNfd }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let composed = run normalizeNfc("E\\u{304}\\u{300}") |> Effect.provideMut(&mut allocator)
  if view(&composed) == "\\u{1e14}" {} else { return 1 }
  let decomposed = run normalizeNfd("\\u{ac01}") |> Effect.provideMut(&mut allocator)
  if view(&decomposed) == "\\u{1100}\\u{1161}\\u{11a8}" {} else { return 2 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('lowers a normalizing program to a MIR that names no Unicode operation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'unicode-privilege/normalize',
      ascii(normalizing),
    )
    assert.deepEqual(messages(snapshot), [])

    // The program really runs, so the MIR examined below is the MIR of working normalization rather
    // than of something that happened to lower.
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)

    const mir = Analysis.mirOf(snapshot)
    assert.strictEqual(mir._tag, 'Available')
    if (mir._tag !== 'Available') return

    const operations = operationsOf(mir.value)
    assert.isAbove(operations.length, 0, 'the program lowered to something')
    const kinds = [...new Set(operations.map((operation) => operation._tag))].sort()
    assert.deepEqual(
      kinds.filter((kind) => unicodeShaped.test(kind)),
      [],
      `MIR operations used: ${kinds.join(', ')}`,
    )
  }),
)

it.effect('normalizes through ordinary Silk functions reached by ordinary calls', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'unicode-privilege/normalize',
      ascii(normalizing),
    )
    assert.deepEqual(messages(snapshot), [])
    const mir = Analysis.mirOf(snapshot)
    assert.strictEqual(mir._tag, 'Available')
    if (mir._tag !== 'Available') return

    // The normalizer and the table lookups are functions of the standard library's own modules,
    // lowered like any other Silk function — so they have bodies in the MIR rather than being names
    // a backend recognizes.
    const lowered = mir.value.functions.map((fn) => `${fn.id.module}.${fn.id.name}`)
    assert.include(lowered, 'silk/unicode.normalizeNfc', `lowered: ${lowered.join(', ')}`)
    assert.include(lowered, 'silk/unicode.normalizeNfd')
    assert.include(lowered, 'silk/unicode_tables.canonicalDecomposition')
    assert.include(lowered, 'silk/unicode_tables.canonicalComposition')
    assert.include(lowered, 'silk/unicode_tables.combiningClass')

    // And they are reached the ordinary way: a `Call` naming the declaration, not an operation of
    // its own. This is the positive half of the constraint — the policy exists, and it is Silk.
    // The effectful entry points lower through the effect machinery rather than as a direct call,
    // which is why the table lookups and the pure composition step are what is asserted here; they
    // are the places a privileged implementation would have had to appear.
    const calls = operationsOf(mir.value).flatMap((operation) =>
      operation._tag === 'Call' ? [`${operation.target.module}.${operation.target.name}`] : [],
    )
    assert.include(calls, 'silk/unicode_tables.combiningClass', `calls: ${calls.join(', ')}`)
    assert.include(calls, 'silk/unicode_tables.canonicalDecomposition')
    assert.include(calls, 'silk/unicode_tables.canonicalComposition')
    assert.include(calls, 'silk/unicode.composePair')
  }),
)

/**
 * The tables are treated as ordinary data rather than by their spelling.
 *
 * This program encodes a slice of the same combining-class table in the same packed format, under a
 * module-less name nothing in the compiler could recognize, and searches it with the same binary
 * search. If any phase special-cased `silk/unicode_tables` or the name `combiningClass`, the two
 * answers would not agree.
 */
const ownTable = `import silk.u32 as u32
import silk.u8 as u8
import silk.usize as usize
import silk.unicode { canonicalCombiningClass }

fn read3(data: &[u8], at: usize) -> u32 {
  return u8.toU32(data[at]) * 65536
    + u8.toU32(data[at + usize.ONE]) * 256
    + u8.toU32(data[at + 2])
}

/// The runs covering U+0300..U+0304, U+0323 and U+05B0, packed exactly as the generator packs them.
fn mine(scalar: u32) -> u32 {
  let data = b"\\x00\\x03\\x00\\x04\\xe6\\x00\\x03\\x23\\x00\\xdc\\x00\\x05\\xb0\\x00\\x0a"
  let mut low = usize.ZERO
  let mut high = usize.add(0, 3)
  while low < high {
    let middle = low + (high - low) / 2
    let at = middle * 5
    let start = read3(data, at)
    if scalar < start {
      high = middle
    } else {
      let count = u8.toU32(data[at + 3]) + 1
      if scalar < start + count { return u8.toU32(data[at + 4]) }
      low = middle + usize.ONE
    }
  }
  return 0
}

fn agrees(scalar: u32) -> bool {
  return mine(scalar) == canonicalCombiningClass(scalar)
}

pub fn main() -> i32 {
  if agrees(u32.toU32(768)) {} else { return 1 }
  if agrees(u32.toU32(772)) {} else { return 2 }
  if agrees(u32.toU32(803)) {} else { return 3 }
  if agrees(u32.toU32(1456)) {} else { return 4 }
  // A starter, which neither table lists.
  if agrees(u32.toU32(101)) {} else { return 5 }
  // And the values are the real ones, not zero on both sides.
  if canonicalCombiningClass(u32.toU32(768)) == 230 {} else { return 6 }
  if canonicalCombiningClass(u32.toU32(803)) == 220 {} else { return 7 }
  if canonicalCombiningClass(u32.toU32(101)) == 0 {} else { return 8 }
  return 42
}`

it.effect('treats a Unicode table as ordinary data rather than by its spelling', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'unicode-privilege/own-table',
      ascii(ownTable),
    )
    assert.deepEqual(messages(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)
  }),
)

it('keeps the Unicode tables out of the compiler and inside the standard library', () => {
  // The tables live in exactly one place. `Stdlib.generated.ts` carries them the way it carries
  // every other module: as source text it never reads.
  const tables = readFileSync(join(packageRoot, 'stdlib', 'silk', 'unicode_tables.silk'), 'utf8')
  assert.include(tables, 'Generated by scripts/generate-unicode-tables.mjs')
  assert.isAbove(tables.length, 40_000, 'the generated tables were read')

  const generated = readFileSync(join(sourceRoot, 'Stdlib.generated.ts'), 'utf8')
  assert.include(generated, "module: 'silk/unicode_tables'")
  assert.include(generated, 'Generated by scripts/generate-stdlib.mjs. Do not edit.')
})
