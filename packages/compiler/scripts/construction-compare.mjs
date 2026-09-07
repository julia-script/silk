import { readFileSync, writeFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { isDeepStrictEqual } from 'node:util'

const [baselinePath, candidatePath] = process.argv.slice(2)
if (!baselinePath || !candidatePath)
  throw new Error('Usage: node construction-compare.mjs BASELINE_DIRECTORY CANDIDATE_DIRECTORY')
const baseline = JSON.parse(readFileSync(resolve(baselinePath, 'samples.json'), 'utf8'))
const candidate = JSON.parse(readFileSync(resolve(candidatePath, 'samples.json'), 'utf8'))
for (const report of [baseline, candidate]) {
  if (
    report.schemaVersion !== 1 ||
    report.warmups !== 2 ||
    report.measuredSamples !== 5 ||
    !isDeepStrictEqual(
      report.results
        .map((r) => r.name)
        .sort((left, right) => {
          if (left === right) return 0
          return left < right ? -1 : 1
        }),
      ['arithmetic', 'lexer', 'minimal'],
    )
  )
    throw new Error('Incomplete benchmark run')
  for (const result of report.results) {
    if (
      result.timing.timings.length !== 7 ||
      result.memory.length !== 5 ||
      result.timing.timings.some((duration) => !Number.isFinite(duration) || duration < 0)
    )
      throw new Error('Invalid sample inventory')
  }
}
const results = baseline.results.map((base) => {
  const next = candidate.results.find((entry) => entry.name === base.name)
  if (next === undefined) throw new Error(`Missing ${base.name}`)
  for (const key of [
    'node',
    'nodeFlags',
    'nodeOptions',
    'host',
    'target',
    'profile',
    'request',
    'cache',
    'sources',
    'diagnostics',
    'mirViolations',
    'inventorySha256',
    'harnessSha256',
  ]) {
    if (!isDeepStrictEqual(base.timing[key], next.timing[key]))
      throw new Error(`${base.name}: incomparable ${key}`)
  }
  const compiler = (entry) =>
    entry.timing.builtIdentity.find((artifact) => artifact.name === 'compiler')?.sha256
  if (compiler(base) === undefined || compiler(base) !== compiler(next))
    throw new Error(`${base.name}: compiler build changed`)
  const inventoryEqual = readFileSync(resolve(baselinePath, `${base.name}-inventory.json`)).equals(
    readFileSync(resolve(candidatePath, `${base.name}-inventory.json`)),
  )
  const bitcodeEqual = readFileSync(resolve(baselinePath, `${base.name}.bc`)).equals(
    readFileSync(resolve(candidatePath, `${base.name}.bc`)),
  )
  const limit =
    base.name === 'minimal' ? base.medianMs + Math.max(base.medianMs * 0.1, 5) : base.medianMs * 0.6
  return {
    workload: base.name,
    baselineMs: base.medianMs,
    candidateMs: next.medianMs,
    ratio: next.medianMs / base.medianMs,
    timePass: next.medianMs <= limit,
    baselinePeakRssKiB: base.medianPeakRssKiB,
    candidatePeakRssKiB: next.medianPeakRssKiB,
    rssPass: next.medianPeakRssKiB <= base.medianPeakRssKiB * 1.1,
    inventoryEqual,
    bitcodeEqual,
  }
})
const report = {
  baseline: baseline.results[0].timing.llvmRevision,
  candidate: candidate.results[0].timing.llvmRevision,
  results,
  passed: results.every((r) => r.timePass && r.rssPass && r.inventoryEqual && r.bitcodeEqual),
}
writeFileSync(resolve(candidatePath, 'comparison.json'), `${JSON.stringify(report, null, 2)}\n`)
process.stdout.write(`${JSON.stringify(report, null, 2)}\n`)
if (!report.passed) process.exitCode = 1
