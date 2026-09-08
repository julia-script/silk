import assert from 'node:assert/strict'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import { createHash } from 'node:crypto'
import { existsSync, readFileSync, readdirSync } from 'node:fs'
import { basename, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

// Run from any directory. --artifacts also requires the preserved local conformance reports.
const repository = fileURLToPath(new URL('../../../', import.meta.url))
const directory = resolve(repository, 'openspec/changes/complete-native-runtime-migration')
const log = (...values) => Effect.runSync(Console.log(...values))
const read = (path) => readFileSync(resolve(repository, path))
const json = (path) => JSON.parse(read(path).toString('utf8'))
const digest = (value) => createHash('sha256').update(value).digest('hex')
const inventory = JSON.parse(readFileSync(resolve(directory, 'source-absence-audit.json'), 'utf8'))
const sourceFiles = (directory) =>
  readdirSync(resolve(repository, directory), { withFileTypes: true }).flatMap((entry) => {
    const path = `${directory}/${entry.name}`
    if (entry.isDirectory()) return sourceFiles(path)
    return entry.name.endsWith('.ts') && !entry.name.endsWith('.generated.ts') ? [path] : []
  })
assert.deepEqual(
  [...sourceFiles('packages/compiler/src'), ...sourceFiles('packages/cli/src')].sort(
    (left, right) => left.localeCompare(right),
  ),
  inventory.files.map(({ path }) => path).sort((left, right) => left.localeCompare(right)),
  'The source audit must account for every current non-generated compiler and CLI module.',
)
for (const file of inventory.files) {
  const bytes = read(file.path)
  assert.equal(digest(bytes), file.sha256, `Source changed since review: ${file.path}`)
  const source = bytes.toString('utf8')
  for (const pattern of [...inventory.retiredPatterns, ...inventory.sourceModuleSpellings])
    assert.equal(source.includes(pattern), false, `${file.path} contains ${pattern}`)
}
for (const file of inventory.removedFiles)
  assert.equal(existsSync(resolve(repository, file.path)), false, file.path)
assert.deepEqual(inventory.findings, [])
log(`Source audit: ${inventory.files.length} modules; no retired paths or provider spellings.`)

const arguments_ = process.argv.slice(2)
assert.ok(
  arguments_.every((argument) => argument === '--artifacts'),
  'Expected only --artifacts.',
)
if (arguments_.includes('--artifacts')) {
  const audit = JSON.parse(readFileSync(resolve(directory, 'artifact-absence-audit.json'), 'utf8'))
  const retired = audit.patterns.map(
    (pattern) => new RegExp(`(?<![A-Za-z0-9_])_?${pattern}(?![A-Za-z0-9_])`),
  )
  const reports = new Map()
  for (const entry of audit.native) {
    let preserved = reports.get(entry.report)
    if (preserved === undefined) {
      const bytes = read(entry.report)
      preserved = { sha256: digest(bytes), report: JSON.parse(bytes.toString('utf8')) }
      reports.set(entry.report, preserved)
    }
    assert.equal(preserved.sha256, entry.reportSha256, `Report changed: ${entry.report}`)
    const report = preserved.report
    const lane = report.lanes.find(
      (lane) =>
        (lane.fixture ?? lane.name ?? basename(entry.report, '.json')) === entry.fixture &&
        lane.optimization === entry.optimization,
    )
    assert.ok(lane, `Missing ${entry.fixture}/${entry.optimization} in ${entry.report}`)
    assert.equal(report.target, entry.target)
    assert.equal(lane.inspection.status, 0)
    assert.equal(digest(lane.inspection.stdout), entry.inspectionSha256)
    assert.equal(lane.execution.status, entry.executionStatus)
    for (const pattern of retired) assert.equal(pattern.test(lane.inspection.stdout), false)
  }
  const wasm = json('.scratch/execution-storage/wasm32/report.json')
  const lanes = [...wasm.lanes, ...wasm.lifecycle, ...wasm.entry]
  assert.equal(lanes.length, audit.wasm.length)
  for (const entry of audit.wasm) {
    const lane = lanes.find(
      (lane) =>
        lane.name === entry.fixture &&
        lane.optimization === entry.optimization &&
        lane.artifact === entry.artifact,
    )
    assert.ok(lane, `Missing Wasm ${entry.fixture}/${entry.optimization}/${entry.artifact}`)
    const exports = lane.execution.exports.map(({ name }) => name)
    assert.deepEqual(exports, entry.exports)
    for (const symbol of exports)
      for (const pattern of retired) assert.equal(pattern.test(symbol), false)
  }
  assert.deepEqual(audit.findings, [])
  log(
    `Artifact audit: ${audit.native.length} native inspections and ${audit.wasm.length} Wasm export sets; no retired symbols.`,
  )
}
