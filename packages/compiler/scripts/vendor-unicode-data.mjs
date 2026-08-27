// Vendors the pinned Unicode Character Database subset the standard-library table generator reads.
//
// The Unicode Consortium's own host is not reachable from every environment this repository builds
// in, so the vendored data comes from the `ucd-full` npm package, which republishes the official
// UCD files verbatim as JSON. The package version *is* the Unicode version: `ucd-full@17.0.0` is
// the 17.0.0 database. This script downloads that exact version, checks the tarball against the
// integrity hash the registry publishes for it, and rewrites the fields the generator needs into
// small line-oriented files under `unicode-data/`.
//
// The vendored files are checked in so `unicode:check` can verify the generated tables offline, on
// a machine with no network at all. Re-running this script is the first half of a Unicode version
// bump; `unicode:generate` is the second.
//
// Usage:
//   node scripts/vendor-unicode-data.mjs            # download and rewrite
//   node scripts/vendor-unicode-data.mjs --check    # fail if the vendored files would change

import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { mkdtempSync, rmSync } from 'node:fs'
import { mkdir, readdir, readFile, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const packageRoot = join(dirname(fileURLToPath(import.meta.url)), '..')
const dataRoot = join(packageRoot, 'unicode-data')

/** The pinned Unicode version. Bumping this is the whole of a data-version upgrade's input. */
const unicodeVersion = '17.0.0'
const registry = 'https://registry.npmjs.org'

const check = process.argv.includes('--check')

const fetchJson = async (url) => {
  const response = await fetch(url)
  if (!response.ok) throw new Error(`${url} responded ${response.status}`)
  return response.json()
}

const download = async () => {
  const metadata = await fetchJson(`${registry}/ucd-full`)
  const release = metadata.versions?.[unicodeVersion]
  if (release === undefined) {
    throw new Error(
      `ucd-full has no ${unicodeVersion} release; the Unicode version is not published`,
    )
  }
  const response = await fetch(release.dist.tarball)
  if (!response.ok) throw new Error(`tarball responded ${response.status}`)
  const tarball = Buffer.from(await response.arrayBuffer())
  const digest = createHash('sha512').update(tarball).digest('base64')
  const expected = release.dist.integrity
  if (expected !== `sha512-${digest}`) {
    throw new Error(
      `ucd-full@${unicodeVersion} tarball does not match the published integrity hash`,
    )
  }
  return tarball
}

const extract = async (tarball) => {
  const scratch = mkdtempSync(join(tmpdir(), 'silk-ucd-'))
  try {
    const archive = join(scratch, 'ucd.tgz')
    await writeFile(archive, tarball)
    const extracted = spawnSync('tar', ['xzf', archive, '-C', scratch], { encoding: 'utf8' })
    if (extracted.status !== 0) throw new Error(`tar failed: ${extracted.stderr}`)
    const read = async (relative) =>
      JSON.parse(await readFile(join(scratch, 'package', relative), 'utf8'))
    return {
      unicodeData: (await read('UnicodeData.json')).UnicodeData,
      compositionExclusions: (await read('CompositionExclusions.json')).CompositionExclusions,
      graphemeBreakProperty: (await read('auxiliary/GraphemeBreakProperty.json'))
        .GraphemeBreakProperty,
      emojiData: (await read('emoji/emoji-data.json'))['emoji-data'],
      derivedCoreProperties: (await read('DerivedCoreProperties.json')).DerivedCoreProperties,
      specialCasing: (await read('SpecialCasing.json')).SpecialCasing,
      normalizationTest: (await read('NormalizationTest.json')).NormalizationTest,
      graphemeBreakTest: (await read('auxiliary/GraphemeBreakTest.json')).GraphemeBreakTest,
    }
  } finally {
    rmSync(scratch, { recursive: true, force: true })
  }
}

const point = (value) => Number.parseInt(value, 16)
const spell = (value) => value.toString(16).toUpperCase().padStart(4, '0')
const spellAll = (values) => values.map(spell).join(' ')

const header = (name, description) =>
  `# ${name}\n# Unicode ${unicodeVersion}, vendored from ucd-full@${unicodeVersion}.\n# ${description}\n# Regenerate with: pnpm --filter @silklang/compiler unicode:vendor\n`

/**
 * Canonical combining class, canonical decomposition, and simple case mappings, one line per code
 * point that carries at least one of them. Compatibility decompositions (the `<tag>` forms) are
 * dropped: NFKC and NFKD are out of scope, and keeping them would triple the file for no reader.
 */
const unicodeDataSubset = (entries) => {
  const lines = []
  for (const entry of entries) {
    const mapping = entry.characterDecompositionMapping
    const canonical = mapping !== undefined && !mapping.startsWith('<') ? mapping.trim() : ''
    const combining = Number.parseInt(entry.canonicalCombiningClass, 10)
    const upper = entry.upper ?? ''
    const lower = entry.lower ?? ''
    if (canonical === '' && combining === 0 && upper === '' && lower === '') continue
    lines.push(
      `${entry.codepoint};${combining};${canonical};${upper};${lower}`.replace(/;+$/, (run) =>
        ';'.repeat(run.length),
      ),
    )
  }
  return `${header(
    'UnicodeData.subset.txt',
    'Fields: code point; canonical combining class; canonical decomposition; simple uppercase; simple lowercase.',
  )}${lines.join('\n')}\n`
}

const rangeLines = (entries, predicate, label) => {
  const lines = []
  for (const entry of entries) {
    if (!predicate(entry)) continue
    const [first, last] = entry.range
    const span =
      last === undefined ? spell(point(first)) : `${spell(point(first))}..${spell(point(last))}`
    lines.push(label === undefined ? span : `${span}; ${label(entry)}`)
  }
  return lines
}

const specialCasing = (entries) => {
  const lines = []
  for (const entry of entries) {
    // Conditional mappings carry a language tag or a context condition. Language-tailored rows are
    // out of scope; context-conditioned rows are not, so the condition travels with the row and the
    // generator decides.
    const condition = entry.conditions ?? ''
    lines.push(
      [
        entry.codepoint,
        spellAll((entry.lowerSequence ?? []).map(point)),
        spellAll((entry.titleSequence ?? []).map(point)),
        spellAll((entry.upperSequence ?? []).map(point)),
        Array.isArray(condition) ? condition.join(' ') : condition,
      ].join('; '),
    )
  }
  return `${header(
    'SpecialCasing.txt',
    'Fields: code point; full lowercase; full titlecase; full uppercase; condition.',
  )}${lines.join('\n')}\n`
}

const normalizationTest = (entries) => {
  const lines = []
  for (const entry of entries) {
    const source = entry.sourceSequence ?? []
    if (source.length === 1 && source[0].startsWith('@')) {
      lines.push(source[0])
      continue
    }
    lines.push(
      [
        spellAll(source.map(point)),
        spellAll((entry.NFCSequence ?? []).map(point)),
        spellAll((entry.NFDSequence ?? []).map(point)),
        spellAll((entry.NFKCSequence ?? []).map(point)),
        spellAll((entry.NFKDSequence ?? []).map(point)),
      ].join(';'),
    )
  }
  return `${header(
    'NormalizationTest.txt',
    'Conformance data. Fields: source; NFC; NFD; NFKC; NFKD. @PartN lines mark the official sections.',
  )}${lines.join('\n')}\n`
}

const files = (ucd) => ({
  VERSION: `${unicodeVersion}\n`,
  'UnicodeData.subset.txt': unicodeDataSubset(ucd.unicodeData),
  'CompositionExclusions.txt': `${header(
    'CompositionExclusions.txt',
    'Script-listed composition exclusions. The derived exclusions are computed by the generator.',
  )}${ucd.compositionExclusions.map((value) => spell(point(value))).join('\n')}\n`,
  'GraphemeBreakProperty.txt': `${header(
    'GraphemeBreakProperty.txt',
    'Fields: code point range; Grapheme_Cluster_Break value.',
  )}${rangeLines(
    ucd.graphemeBreakProperty,
    () => true,
    (entry) => entry.property,
  ).join('\n')}\n`,
  'ExtendedPictographic.txt': `${header(
    'ExtendedPictographic.txt',
    'Code point ranges with Extended_Pictographic=Yes, from emoji-data.',
  )}${rangeLines(ucd.emojiData, (entry) => entry.property === 'Extended_Pictographic').join('\n')}\n`,
  'IndicConjunctBreak.txt': `${header(
    'IndicConjunctBreak.txt',
    'Fields: code point range; Indic_Conjunct_Break value. Drives rule GB9c.',
  )}${rangeLines(
    ucd.derivedCoreProperties,
    (entry) => entry.property === 'InCB',
    (entry) => entry.syllabicCategory,
  ).join('\n')}\n`,
  'SpecialCasing.txt': specialCasing(ucd.specialCasing),
  'NormalizationTest.txt': normalizationTest(ucd.normalizationTest),
  'GraphemeBreakTest.txt': `${header(
    'GraphemeBreakTest.txt',
    'Conformance data. ÷ marks a cluster boundary and × marks a prohibited break.',
  )}${ucd.graphemeBreakTest.join('\n')}\n`,
})

const written = files(await download().then(extract))
const digests = Object.entries(written)
  .map(([name, content]) => `${createHash('sha256').update(content).digest('hex')}  ${name}`)
  .join('\n')
written.SHA256SUMS = `${digests}\n`

await mkdir(dataRoot, { recursive: true })

if (check) {
  const existing = await readdir(dataRoot).catch(() => [])
  const unexpected = existing.filter((name) => written[name] === undefined)
  if (unexpected.length > 0) {
    throw new Error(`Vendored Unicode data has unexpected files: ${unexpected.join(', ')}`)
  }
  for (const [name, content] of Object.entries(written)) {
    const current = await readFile(join(dataRoot, name), 'utf8').catch(() => undefined)
    if (current !== content) {
      throw new Error(`Vendored ${name} is stale; run pnpm unicode:vendor`)
    }
  }
} else {
  for (const [name, content] of Object.entries(written)) {
    await writeFile(join(dataRoot, name), content)
  }
}
