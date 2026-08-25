import { existsSync, readdirSync, readFileSync, writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { inventoryKinds, sha256 } from './parity-lib.mjs'

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const parityRoot = resolve(packageRoot, 'parity')
const manifest = JSON.parse(readFileSync(resolve(parityRoot, 'manifest.json'), 'utf8'))
const inventoryPath = resolve(parityRoot, manifest.inventory.path)
const inventoryBytes = readFileSync(inventoryPath)
const inventory = JSON.parse(inventoryBytes.toString('utf8'))
const benchmarks = JSON.parse(readFileSync(resolve(parityRoot, 'BENCHMARKS.json'), 'utf8'))
const failures = []
const check = (condition, message) => {
  if (!condition) failures.push(message)
}
const duplicates = (values) => {
  const seen = new Set()
  return values.filter((value) => {
    if (seen.has(value)) return true
    seen.add(value)
    return false
  })
}
const sourceFiles = (directory) =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = resolve(directory, entry.name)
    if (entry.isDirectory()) return sourceFiles(path)
    if (path.endsWith('.ts')) return [path]
    return []
  })

check(manifest.schemaVersion === 1, 'unsupported manifest schemaVersion')
check(inventory.schemaVersion === 1, 'unsupported inventory schemaVersion')
check(manifest.baseline.commit === inventory.commit, 'inventory commit differs from manifest')
check(sha256(inventoryBytes) === manifest.inventory.sha256, 'inventory hash is stale')
for (const [name, hash] of Object.entries(manifest.baseline.sources)) {
  check(inventory.sources[name]?.sha256 === hash, `${name} hash differs from pinned baseline`)
}
check(
  /^\d+\.\d+\.\d+$/.test(manifest.baseline.llvm.version),
  'LLVM version must pin major.minor.patch',
)
check(manifest.baseline.llvm.tools.length === 4, 'authoritative LLVM tool list is incomplete')
check(benchmarks.schemaVersion === 1, 'unsupported benchmark schemaVersion')
check(benchmarks.sampleCount >= 5, 'benchmarks require at least five repeated samples')
const expectedWorkloads = [
  'control-flow-heavy',
  'interning-heavy',
  'large-blobs',
  'many-small-functions',
  'metadata-heavy',
  'one-large-function',
]
check(
  benchmarks.workloads
    .map((entry) => entry.name)
    .sort()
    .join('\n') === expectedWorkloads.join('\n'),
  'benchmark workload inventory is incomplete',
)
for (const benchmark of [...benchmarks.workloads, ...benchmarks.candidates]) {
  check(
    benchmark.samples.length === benchmarks.sampleCount,
    `${benchmark.name} benchmark sample count is stale`,
  )
  check(
    Number.isFinite(benchmark.medianMs) && benchmark.medianMs >= 0,
    `${benchmark.name} lacks a median`,
  )
}
check(benchmarks.candidates.length === 2, 'traced/untraced candidate comparison is incomplete')
check(
  typeof benchmarks.decision.retainEffectFnUntraced === 'boolean',
  'benchmark decision is missing',
)
for (const source of sourceFiles(resolve(packageRoot, 'src'))) {
  if (source.includes(`${resolve(packageRoot, 'src')}/internal/`)) continue
  const contents = readFileSync(source, 'utf8')
  check(
    !/export const \w+ = Effect\.fnUntraced/.test(contents),
    `public actor operation must use named Effect.fn tracing: ${source}`,
  )
}

const allEntries = inventoryKinds.flatMap((kind) => inventory[kind] ?? [])
const entryDuplicates = duplicates(allEntries.map((entry) => entry.id))
check(entryDuplicates.length === 0, `duplicate inventory IDs: ${entryDuplicates.join(', ')}`)
const ruleIds = manifest.rules.map((rule) => rule.id)
check(duplicates(ruleIds).length === 0, 'duplicate manifest rule IDs')
const allowedDispositions = new Set([
  'implemented',
  'intentional-deviation',
  'upstream-unsupported',
])
const fixtureClasses = new Set(manifest.fixtureClasses)
for (const kind of inventoryKinds) {
  const matching = manifest.rules.filter((rule) => rule.inventoryKind === kind)
  check(matching.length === 1, `${kind} requires exactly one disposition rule`)
  check((inventory[kind] ?? []).length > 0, `${kind} inventory is empty`)
}
for (const rule of manifest.rules) {
  check(inventoryKinds.includes(rule.inventoryKind), `${rule.id} uses unknown inventory kind`)
  check(allowedDispositions.has(rule.disposition), `${rule.id} uses unknown disposition`)
  const entries = inventory[rule.inventoryKind] ?? []
  check(entries.length > 0, `${rule.id} has no source entries`)
  if (rule.disposition === 'implemented' || rule.disposition === 'intentional-deviation') {
    check(rule.actors?.length > 0, `${rule.id} lacks actor evidence`)
    check(rule.tests?.length > 0, `${rule.id} lacks test evidence`)
    check(rule.fixtures?.length > 0, `${rule.id} lacks fixture evidence`)
    for (const test of rule.tests ?? []) {
      check(existsSync(resolve(packageRoot, test)), `${rule.id} test does not exist: ${test}`)
    }
    for (const actor of rule.actors ?? []) {
      const path = actor === 'Bitstream' ? 'src/internal/Bitstream.ts' : `src/${actor}.ts`
      check(existsSync(resolve(packageRoot, path)), `${rule.id} actor does not exist: ${actor}`)
    }
    for (const fixture of rule.fixtures ?? []) {
      check(fixtureClasses.has(fixture), `${rule.id} uses unknown fixture class: ${fixture}`)
    }
  }
  if (rule.disposition !== 'implemented') {
    check(typeof rule.documentation === 'string', `${rule.id} lacks deviation documentation`)
  }
}

if (failures.length > 0)
  throw new Error(`Parity manifest validation failed:\n- ${failures.join('\n- ')}`)

const counts = Object.fromEntries(inventoryKinds.map((kind) => [kind, inventory[kind].length]))
const dispositionCounts = Object.fromEntries(
  [...allowedDispositions].map((disposition) => [
    disposition,
    manifest.rules
      .filter((rule) => rule.disposition === disposition)
      .reduce((total, rule) => total + inventory[rule.inventoryKind].length, 0),
  ]),
)
const report = `# LLVM builder parity report

Generated from the validated parity manifest. Do not edit this file directly.

## Compatibility baseline

- Zig commit: \`${manifest.baseline.commit}\`
- LLVM: \`${manifest.baseline.llvm.version}\`
- Sources: ${Object.entries(manifest.baseline.sources)
  .map(([name, hash]) => `\`${name}\` \`${hash}\``)
  .join('; ')}

## Source inventory

${Object.entries(counts)
  .map(([kind, count]) => `- ${kind}: ${count}`)
  .join('\n')}

Total ledger entries: ${Object.values(counts).reduce((left, right) => left + right, 0)}.

## Dispositions

${Object.entries(dispositionCounts)
  .map(([disposition, count]) => `- ${disposition}: ${count}`)
  .join('\n')}

The public Zig operation and raw enum inventories are intentional representation deviations: the
Effect port groups behavior by actor and exposes typed settings rather than mirroring Zig pointer,
allocator, and assume-capacity APIs. The complete 158-entry intrinsic inventory is implemented.
Raw block and abbreviation vocabularies are reconciled through direct bitcode emission and LLVM
semantic fixtures; unused legacy schema vocabulary is not exposed as a public API.

## Performance evidence

Recorded on \`${benchmarks.runtime}\`/\`${benchmarks.architecture}\` with ${benchmarks.sampleCount}
samples per workload:

${benchmarks.workloads.map((entry) => `- ${entry.name}: median ${entry.medianMs} ms`).join('\n')}

The candidate-untraced comparison improved the median by ${benchmarks.decision.improvementPercent}%
against a ${benchmarks.decision.materialThresholdPercent}% materiality threshold. Public actor
operations therefore remain traced; reusable internal Effect functions use \`Effect.fnUntraced\`,
while measured bit/byte packing loops remain imperative behind the typed \`Bitcode.encode\`
boundary.

## Verification evidence

- Exact-byte bitstream and pinned foundation fixtures.
- Canonical semantic declaration, core, advanced, and metadata text/bitcode fixtures.
- LLVM ${manifest.baseline.llvm.version} assembly, disassembly, verification, and bitcode analysis.
- Fresh-process determinism checks for the runtime and architecture recorded by CI.
- Boundary, malformed-input, ownership, transactional rollback, recursive-graph, and benchmark
  coverage maintained by the package tests and parity scripts.

See [UPSTREAM.md](../UPSTREAM.md) for deviations, unsupported upstream paths, provenance, and the
candidate-update workflow.
`

if (process.argv.includes('--write-report')) {
  writeFileSync(resolve(parityRoot, 'REPORT.md'), report)
}
process.stdout.write(`${JSON.stringify({ counts, dispositions: dispositionCounts })}\n`)
