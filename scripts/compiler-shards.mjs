import { readdirSync, readFileSync, writeFileSync } from 'node:fs'
import { dirname, join, relative, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const compilerDirectory = join(root, 'packages/compiler')
const baselinePath = join(root, 'scripts/compiler-shard-timings.json')
const excludedTests = new Set([
  'test/DriverNativeAcceptance.test.ts',
  'test/DriverTlsClientWasmAcceptance.test.ts',
  'test/DriverTlsConnectionWasmAcceptance.test.ts',
])
const excludedDirectories = new Set(['conformance', 'dist', 'node_modules', '.git'])
const testFilePattern = /\.(?:test|spec)\.(?:[cm]?[jt]sx?)$/
const comparePaths = (left, right) => {
  if (left < right) return -1
  if (left > right) return 1
  return 0
}

// Refresh from a completed four-shard CI run at the matching checkout:
//   gh run download RUN_ID --pattern 'compiler-shard-*-timings-a1' --dir /tmp/compiler-timings
//   node scripts/compiler-shards.mjs --refresh /tmp/compiler-timings RUN_ID HEAD_SHA
// Review the resulting baseline diff and `--audit` totals before committing it.
export const discoverTests = (directory = compilerDirectory) => {
  const files = []
  const visit = (path) => {
    for (const entry of readdirSync(path, { withFileTypes: true })) {
      if (entry.isDirectory()) {
        if (!excludedDirectories.has(entry.name)) visit(join(path, entry.name))
      } else if (entry.isFile() && testFilePattern.test(entry.name)) {
        const name = relative(directory, join(path, entry.name)).replaceAll('\\', '/')
        if (!excludedTests.has(name)) files.push(name)
      }
    }
  }
  visit(directory)
  return files.sort(comparePaths)
}

export const assignTests = (files, timings, shardCount = 4) => {
  if (!Number.isSafeInteger(shardCount) || shardCount < 1) throw new Error('Invalid shard count')
  const knownCosts = Object.values(timings).sort((a, b) => a - b)
  if (knownCosts.length === 0 || knownCosts.some((cost) => !Number.isFinite(cost) || cost <= 0)) {
    throw new Error('Timing baseline must contain positive finite durations')
  }
  // A new file gets the median measured cost until the baseline is refreshed.
  const fallbackCost = knownCosts[Math.floor(knownCosts.length / 2)]
  const shards = Array.from({ length: shardCount }, () => ({ files: [], totalMs: 0 }))
  const ordered = [...files]
    .map((file) => ({ file, cost: timings[file] ?? fallbackCost }))
    .sort((a, b) => b.cost - a.cost || comparePaths(a.file, b.file))
  for (const { file, cost } of ordered) {
    let target = shards[0]
    for (const shard of shards) if (shard.totalMs < target.totalMs) target = shard
    target.files.push(file)
    target.totalMs += cost
  }
  for (const shard of shards) shard.files.sort(comparePaths)
  return { shards, fallbackCost, unmeasured: files.filter((file) => !(file in timings)) }
}

export const exclusionFlags = (files, shards, shardIndex) => {
  const selected = new Set(shards[shardIndex].files)
  return files.filter((file) => !selected.has(file)).map((file) => `--exclude=${file}`)
}

export const readArtifactTimings = (directory) => {
  const artifacts = []
  const visit = (path) => {
    for (const entry of readdirSync(path, { withFileTypes: true })) {
      if (entry.isDirectory()) visit(join(path, entry.name))
      else if (/^compiler-shard-[1-4]\.json$/.test(entry.name))
        artifacts.push(join(path, entry.name))
    }
  }
  visit(directory)
  if (artifacts.length !== 4)
    throw new Error(`Expected four compiler shard JSON artifacts, found ${artifacts.length}`)
  const timings = {}
  for (const artifact of artifacts.sort(comparePaths)) {
    const report = JSON.parse(readFileSync(artifact, 'utf8'))
    if (!report.success) throw new Error(`Compiler shard did not pass: ${artifact}`)
    for (const result of report.testResults) {
      const marker = '/packages/compiler/'
      const offset = result.name.replaceAll('\\', '/').lastIndexOf(marker)
      if (offset < 0) throw new Error(`Unexpected compiler test path: ${result.name}`)
      const file = result.name.slice(offset + marker.length).replaceAll('\\', '/')
      const duration = Math.round(result.endTime - result.startTime)
      if (!Number.isFinite(duration) || duration <= 0)
        throw new Error(`Invalid duration for ${file}`)
      if (file in timings) throw new Error(`Duplicate test across shards: ${file}`)
      timings[file] = duration
    }
  }
  return Object.fromEntries(Object.entries(timings).sort(([a], [b]) => comparePaths(a, b)))
}

const main = (args) => {
  if (args[0] === '--refresh') {
    const [, artifactDirectory, runId, headSha] = args
    if (!artifactDirectory || !/^\d+$/.test(runId ?? '') || !/^[0-9a-f]{40}$/.test(headSha ?? '')) {
      throw new Error('Usage: --refresh ARTIFACT_DIRECTORY RUN_ID HEAD_SHA')
    }
    const timings = readArtifactTimings(artifactDirectory)
    const files = discoverTests()
    const missing = files.filter((file) => !(file in timings))
    const extra = Object.keys(timings).filter((file) => !files.includes(file))
    if (missing.length || extra.length) {
      throw new Error(
        `Artifact coverage mismatch: missing ${missing.join(', ') || 'none'}; extra ${extra.join(', ') || 'none'}`,
      )
    }
    writeFileSync(
      baselinePath,
      `${JSON.stringify({ source: { runId: Number(runId), headSha }, timings }, null, 2)}\n`,
    )
    return
  }

  const { source, timings } = JSON.parse(readFileSync(baselinePath, 'utf8'))
  const files = discoverTests()
  const { shards, fallbackCost, unmeasured } = assignTests(files, timings)
  if (args[0] === '--audit') {
    process.stdout.write(`Baseline: GitHub Actions run ${source.runId}, head ${source.headSha}\n`)
    process.stdout.write(
      `Eligible: ${files.length}; measured: ${files.length - unmeasured.length}; new: ${unmeasured.length}; new-file cost: ${fallbackCost} ms\n`,
    )
    for (const [index, shard] of shards.entries()) {
      process.stdout.write(
        `Shard ${index + 1}: ${shard.files.length} files, ${(shard.totalMs / 1000).toFixed(1)} predicted file-seconds\n`,
      )
    }
    if (unmeasured.length) process.stdout.write(`New files: ${unmeasured.join(', ')}\n`)
    return
  }
  const match = /^--exclude-for-shard=([1-4])\/4$/.exec(args[0] ?? '')
  if (!match || args.length !== 1)
    throw new Error('Usage: --exclude-for-shard=N/4 | --audit | --refresh DIR RUN_ID HEAD_SHA')
  for (const flag of exclusionFlags(files, shards, Number(match[1]) - 1)) {
    process.stdout.write(flag + '\n')
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  try {
    main(process.argv.slice(2))
  } catch (error) {
    process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n`)
    process.exitCode = 1
  }
}
