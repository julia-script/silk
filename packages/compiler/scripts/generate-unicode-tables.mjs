// Generates the Unicode tables the standard library's `silk/unicode` module reads.
//
// The output is ordinary Silk source with no compiler involvement whatsoever: the tables are
// byte-string literals, and the accessors around them are plain functions doing plain binary
// searches. Nothing in the compiler knows this module exists, and a Unicode version bump is a
// re-run of `unicode:vendor` followed by a re-run of this script.
//
// Byte-string literals become wasm data segments, and the wasm backend lays static data below the
// heap table at 64 KiB, so every byte spent here is a byte a user program cannot spend on its own
// literals. That is why the encodings below are packed rather than convenient.
//
// Usage:
//   node scripts/generate-unicode-tables.mjs            # write the module
//   node scripts/generate-unicode-tables.mjs --check    # fail if the module would change

import { readFile, writeFile } from 'node:fs/promises'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const packageRoot = join(dirname(fileURLToPath(import.meta.url)), '..')
const dataRoot = join(packageRoot, 'unicode-data')
const outputPath = join(packageRoot, 'stdlib', 'silk', 'unicode_tables.silk')

const check = process.argv.includes('--check')

const read = async (name) => readFile(join(dataRoot, name), 'utf8')
const lines = (text) =>
  text
    .split('\n')
    .map((line) => line.trim())
    .filter((line) => line !== '' && !line.startsWith('#'))

const version = (await read('VERSION')).trim()

// ---------------------------------------------------------------------------------------------
// Source data
// ---------------------------------------------------------------------------------------------

const combining = new Map()
const singletons = []
const pairs = []

for (const line of lines(await read('UnicodeData.subset.txt'))) {
  const [codePoint, combiningClass, decomposition] = line.split(';')
  const scalar = Number.parseInt(codePoint, 16)
  const klass = Number.parseInt(combiningClass, 10)
  if (klass !== 0) combining.set(scalar, klass)
  if (decomposition === '') continue
  const components = decomposition.split(' ').map((value) => Number.parseInt(value, 16))
  if (components.length === 1) singletons.push({ scalar, target: components[0] })
  else if (components.length === 2)
    pairs.push({ scalar, first: components[0], second: components[1] })
  else throw new Error(`Canonical decomposition of U+${codePoint} is not one or two scalars`)
}

const scriptExclusions = new Set(
  lines(await read('CompositionExclusions.txt')).map((line) => Number.parseInt(line, 16)),
)

// ---------------------------------------------------------------------------------------------
// Invariants the emitted Silk relies on
// ---------------------------------------------------------------------------------------------

const decomposable = new Set([...singletons, ...pairs].map((entry) => entry.scalar))

// The runtime decomposer walks the first component in a loop and treats the second as final. That
// is only sound because no canonical decomposition's second component decomposes further.
for (const pair of pairs) {
  if (decomposable.has(pair.second)) {
    throw new Error(
      `U+${pair.scalar.toString(16)} has a decomposable second component; the runtime decomposer assumes otherwise`,
    )
  }
}

// The runtime decomposer collects trailing components in a fixed-size buffer, so the deepest
// decomposition has to be known here rather than discovered at run time.
const byScalar = new Map()
for (const entry of singletons) byScalar.set(entry.scalar, [entry.target])
for (const entry of pairs) byScalar.set(entry.scalar, [entry.first, entry.second])

const fullLength = (scalar, seen = new Set()) => {
  const mapping = byScalar.get(scalar)
  if (mapping === undefined) return 1
  if (seen.has(scalar))
    throw new Error(`Canonical decomposition of U+${scalar.toString(16)} cycles`)
  const next = new Set(seen).add(scalar)
  return mapping.reduce((total, component) => total + fullLength(component, next), 0)
}

const maximumDecomposition = [...byScalar.keys()].reduce(
  (longest, scalar) => Math.max(longest, fullLength(scalar)),
  1,
)

// ---------------------------------------------------------------------------------------------
// Composition
// ---------------------------------------------------------------------------------------------

// Full_Composition_Exclusion is the script-listed exclusions plus the two derived families: every
// singleton decomposition, and every decomposition whose first component is not a starter.
const composable = pairs
  .map((pair, index) => ({ ...pair, index }))
  .filter(
    (pair) =>
      !scriptExclusions.has(pair.scalar) &&
      (combining.get(pair.first) ?? 0) === 0 &&
      (combining.get(pair.scalar) ?? 0) === 0,
  )
  .sort((left, right) => left.first - right.first || left.second - right.second)

if (composable.length > 0xffff) {
  throw new Error('The composition index no longer fits a 16-bit pair index')
}

// ---------------------------------------------------------------------------------------------
// Encoding
// ---------------------------------------------------------------------------------------------

const wide = (value, width) => {
  if (value < 0 || value >= 256 ** width) {
    throw new Error(`${value} does not fit ${width} bytes`)
  }
  const bytes = []
  for (let shift = width - 1; shift >= 0; shift -= 1) bytes.push((value >>> (shift * 8)) & 0xff)
  return bytes
}

/** Contiguous runs of one combining class, split so a run length always fits a single byte. */
const combiningRuns = () => {
  const ordered = [...combining.entries()].sort((left, right) => left[0] - right[0])
  const runs = []
  for (const [scalar, klass] of ordered) {
    const last = runs.at(-1)
    if (
      last !== undefined &&
      last.klass === klass &&
      last.start + last.count === scalar &&
      last.count < 256
    ) {
      last.count += 1
      continue
    }
    runs.push({ start: scalar, count: 1, klass })
  }
  return runs
}

const runs = combiningRuns()
const sortedSingletons = [...singletons].sort((left, right) => left.scalar - right.scalar)
const sortedPairs = [...pairs].sort((left, right) => left.scalar - right.scalar)
const pairPosition = new Map(sortedPairs.map((pair, index) => [pair.scalar, index]))

const combiningBytes = runs.flatMap((run) => [...wide(run.start, 3), run.count - 1, run.klass])
const singletonBytes = sortedSingletons.flatMap((entry) => [
  ...wide(entry.scalar, 3),
  ...wide(entry.target, 3),
])
const pairBytes = sortedPairs.flatMap((entry) => [
  ...wide(entry.scalar, 3),
  ...wide(entry.first, 3),
  ...wide(entry.second, 3),
])
const compositionBytes = composable.flatMap((entry) => wide(pairPosition.get(entry.scalar), 2))

const literal = (bytes) =>
  `b"${bytes.map((byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`

// ---------------------------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------------------------

const emit =
  () => `//! Generated canonical-normalization data for the \`silk.unicode\` public façade.
//!
//! # When to use
//! Application code should normally call \`silk.unicode\` rather than this backing module. These
//! declarations are public so the ordinary Silk normalization implementation can read the pinned
//! data without compiler privilege.
//!
//! # Details
//! The tables come from Unicode ${version}, are packed as big-endian byte strings, and are searched
//! with ordinary Silk functions. Regeneration may change their representation and record layout;
//! the compiler gives neither this module nor its declaration names special treatment.
//!
//! # Gotchas
//! A zero lookup result is a sentinel, not Unicode scalar U+0000. Hangul decomposition and
//! composition are algorithmic and are not present in these tables.
//!
//! # Examples
//! ## Read the canonical decomposition of é
//! \`\`\`silk
//! import silk.unicode_tables { UnicodeTables }
//!
//! pub fn main() -> i32 {
//!   let decomposition = UnicodeTables.canonicalDecomposition(233)
//!   if decomposition.first != 101 || decomposition.second != 769 {
//!     return 0
//!   }
//!   return 42
//! }
//! \`\`\`
//
// Generated by scripts/generate-unicode-tables.mjs from the vendored Unicode ${version} database.
// Do not edit. Run: pnpm --filter @silklang/compiler unicode:generate

import silk.u32 as u32
import silk.u8 as u8
import silk.usize as usize

/// The importable name of the \`silk.unicode_tables\` module scope.
///
/// # Details
///
/// This struct carries no data and is never constructed by the library. Importing it as
/// \`import silk.unicode_tables { UnicodeTables }\` names the module scope, so generated lookup
/// operations resolve through \`UnicodeTables\`.
pub struct UnicodeTables {}

/// One canonical decomposition with zero sentinels for an absent mapping or second scalar.
pub struct Decomposition {
  /// The first mapped scalar, or zero when the input has no table mapping.
  pub first: u32
  /// The second mapped scalar, or zero when the mapping contains one scalar.
  pub second: u32
}

/// One two-scalar canonical decomposition and its composed scalar.
pub struct PairRecord {
  /// The scalar whose canonical mapping is this pair.
  pub composed: u32
  /// First scalar of the canonical mapping.
  pub first: u32
  /// Second scalar of the canonical mapping.
  pub second: u32
}

/// Returns the Unicode data version that defines all lookup results in this module.
pub fn dataVersion() -> string {
  return "${version}"
}

/// Returns the longest full canonical decomposition of one scalar in these tables.
pub fn maximumDecompositionLength() -> usize {
  return ${maximumDecomposition}
}

fn read2(data: &[u8], at: usize) -> u32 {
  return u8.toU32(data[at]) * 256 + u8.toU32(data[at + usize.ONE])
}

fn read3(data: &[u8], at: usize) -> u32 {
  return u8.toU32(data[at]) * 65536
    + u8.toU32(data[at + usize.ONE]) * 256
    + u8.toU32(data[at + 2])
}

/// Returns a scalar's canonical combining class, or zero for a starter or unknown value.
pub fn combiningClass(scalar: u32) -> u32 {
  let data = ${literal(combiningBytes)}
  let mut low = usize.ZERO
  let mut high = usize.add(0, ${runs.length})
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

/// Returns the canonical decomposition of a scalar, or both components zero when it has none.
/// Hangul syllables decompose algorithmically and are absent from this table by design.
pub fn canonicalDecomposition(scalar: u32) -> Decomposition {
  let singletons = ${literal(singletonBytes)}
  let mut low = usize.ZERO
  let mut high = usize.add(0, ${sortedSingletons.length})
  while low < high {
    let middle = low + (high - low) / 2
    let at = middle * 6
    let key = read3(singletons, at)
    if scalar < key {
      high = middle
    } else {
      if key < scalar {
        low = middle + usize.ONE
      } else {
        return Decomposition { first: read3(singletons, at + 3), second: 0 }
      }
    }
  }
  let found = pairIndexOf(scalar)
  if found == ${sortedPairs.length} { return Decomposition { first: 0, second: 0 } }
  let record = pairRecord(found)
  return Decomposition { first: record.first, second: record.second }
}

/// Returns the position of a scalar in the canonical pair table, or the table length when absent.
fn pairIndexOf(scalar: u32) -> usize {
  let mut low = usize.ZERO
  let mut high = usize.add(0, ${sortedPairs.length})
  while low < high {
    let middle = low + (high - low) / 2
    let composed = pairRecord(middle).composed
    if scalar < composed {
      high = middle
    } else {
      if composed < scalar { low = middle + usize.ONE } else { return middle }
    }
  }
  return ${sortedPairs.length}
}

/// Returns one record of the canonical pair table. The table is held here alone so both the
/// decomposition search and the composition search read the same bytes.
fn pairRecord(index: usize) -> PairRecord {
  let data = ${literal(pairBytes)}
  let at = index * 9
  return PairRecord {
    composed: read3(data, at),
    first: read3(data, at + 3),
    second: read3(data, at + 6),
  }
}

/// Composes a starter and a following scalar, or returns zero when the pair does not compose.
///
/// # Details
///
/// Excluded composition pairs are absent. A nonzero result is always a primary composite.
///
/// # Gotchas
///
/// Hangul composition is algorithmic and is not included in this lookup.
pub fn canonicalComposition(first: u32, second: u32) -> u32 {
  let index = ${literal(compositionBytes)}
  let mut low = usize.ZERO
  let mut high = usize.add(0, ${composable.length})
  while low < high {
    let middle = low + (high - low) / 2
    let record = pairRecord(read2(index, middle * 2) |> u32.toUsize)
    if first < record.first {
      high = middle
    } else {
      if record.first < first {
        low = middle + usize.ONE
      } else {
        if second < record.second {
          high = middle
        } else {
          if record.second < second { low = middle + usize.ONE } else { return record.composed }
        }
      }
    }
  }
  return 0
}
`

const generated = emit()

if (check) {
  const current = await readFile(outputPath, 'utf8').catch(() => undefined)
  if (current !== generated) {
    throw new Error('Generated Unicode tables are stale; run pnpm unicode:generate')
  }
} else {
  await writeFile(outputPath, generated)
  const report = [
    ['combining class runs', runs.length, combiningBytes.length],
    ['singleton decompositions', sortedSingletons.length, singletonBytes.length],
    ['canonical pairs', sortedPairs.length, pairBytes.length],
    ['composition index', composable.length, compositionBytes.length],
  ]
  const total = report.reduce((sum, [, , bytes]) => sum + bytes, 0)
  for (const [name, count, bytes] of report) {
    process.stdout.write(
      `${name.padEnd(26)} ${String(count).padStart(5)} entries ${String(bytes).padStart(6)} bytes\n`,
    )
  }
  process.stdout.write(
    `${'total table bytes'.padEnd(26)} ${' '.repeat(13)}${String(total).padStart(6)} bytes\n` +
      `${'longest decomposition'.padEnd(26)} ${String(maximumDecomposition).padStart(5)} scalars\n` +
      `${'emitted source'.padEnd(26)} ${String(generated.length).padStart(5)} bytes\n`,
  )
}
