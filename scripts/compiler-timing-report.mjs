import { appendFileSync, existsSync, readFileSync } from 'node:fs'
import { dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const compilerDirectory = join(root, 'packages/compiler')
const nearBudgetFraction = 0.75
const maxRows = 10

const formatSeconds = (milliseconds) =>
  `${(milliseconds / 1000).toFixed(Math.abs(milliseconds) < 1000 ? 2 : 1)}s`
const fileName = (name) => {
  const normalized = name.replaceAll('\\', '/')
  const marker = '/packages/compiler/'
  const offset = normalized.lastIndexOf(marker)
  return offset < 0 ? null : normalized.slice(offset + marker.length)
}

export const testBudgets = (source, defaultTimeoutMs) => {
  const budgets = new Map()
  const starts = [...source.matchAll(/\b(?:it|test)(?:\.effect)?\s*\(\s*(['"`])([^'"`\n]+)\1\s*,/g)]
  for (const [index, match] of starts.entries()) {
    const title = match[2]
    const end = starts[index + 1]?.index ?? source.length
    const body = source.slice(match.index, end)
    const timeout = [
      ...body.matchAll(/,\s*(?:(?:\/\/[^\n]*\n)\s*)*\{\s*timeout:\s*([\d_]+)\s*\}\s*,?\s*\)/g),
    ].at(-1)
    const budget = timeout ? Number(timeout[1].replaceAll('_', '')) : defaultTimeoutMs
    const previous = budgets.get(title)
    budgets.set(title, previous === undefined || previous === budget ? budget : null)
  }
  return budgets
}

const defaultTimeout = () => {
  const source = readFileSync(join(root, 'vitest.shared.ts'), 'utf8')
  const match = /^export const testTimeout = ([\d_]+)$/m.exec(source)
  if (!match) throw new Error('Could not read the shared Vitest test timeout')
  return Number(match[1].replaceAll('_', ''))
}

export const renderReport = (
  report,
  {
    shard,
    readSource = (file) => readFileSync(join(compilerDirectory, file), 'utf8'),
    defaultTimeoutMs = defaultTimeout(),
  } = {},
) => {
  const files = Array.isArray(report?.testResults) ? report.testResults : []
  const lines = [`### Compiler shard ${shard} timing report`]
  if (!Array.isArray(report?.testResults)) {
    lines.push(
      'Timing JSON has no test results; the Vitest output above contains the failure details.',
    )
    return lines.join('\n') + '\n'
  }

  const tests = []
  const fileCosts = []
  let incomplete = 0
  for (const result of files) {
    const file = typeof result?.name === 'string' ? fileName(result.name) : null
    if (!file) {
      incomplete++
      continue
    }
    const duration = result.endTime - result.startTime
    if (Number.isFinite(duration) && duration >= 0) fileCosts.push({ file, duration })
    else incomplete++
    let budgets
    try {
      budgets = testBudgets(readSource(file), defaultTimeoutMs)
    } catch {
      budgets = null
      incomplete++
    }
    for (const assertion of result.assertionResults ?? []) {
      const duration = assertion.duration
      if (!Number.isFinite(duration) || duration < 0) {
        if (assertion.status === 'failed')
          tests.push({
            file,
            name: assertion.fullName ?? assertion.title ?? 'unknown test',
            status: 'failed',
            duration: null,
            budget: null,
          })
        continue
      }
      const title = assertion.title ?? assertion.fullName
      const budget = budgets?.get(title) ?? null
      tests.push({
        file,
        name: assertion.fullName ?? title,
        status: assertion.status,
        duration,
        budget,
      })
    }
  }

  const total = fileCosts.reduce((sum, file) => sum + file.duration, 0)
  const starts = files.map((file) => file.startTime).filter(Number.isFinite)
  const ends = files.map((file) => file.endTime).filter(Number.isFinite)
  const elapsed = starts.length && ends.length ? Math.max(...ends) - Math.min(...starts) : null
  lines.push(
    `Result: ${report.success ? 'passed' : 'failed or incomplete'}; ${files.length} files, ${tests.length} timed tests.`,
  )
  lines.push(
    `Actual shard total: ${formatSeconds(total)} summed file time${elapsed === null ? '' : `; ${formatSeconds(elapsed)} test-run span`}.`,
  )
  if (incomplete)
    lines.push(
      `${incomplete} file result(s) lacked a readable path, duration, or source; totals may be partial.`,
    )

  const nearBudget = tests.filter(
    (test) =>
      test.status === 'failed' ||
      (test.budget !== null && test.duration >= test.budget * nearBudgetFraction),
  )
  const slowest = [...tests]
    .filter((test) => test.duration !== null)
    .sort((a, b) => b.duration - a.duration)
    .slice(0, 5)
  const noteworthy = [...new Set([...nearBudget, ...slowest])].sort(
    (a, b) =>
      (a.status === 'failed' ? -1 : 0) - (b.status === 'failed' ? -1 : 0) ||
      (b.duration ?? 0) - (a.duration ?? 0),
  )
  lines.push(
    `Slowest tests plus those at or above ${Math.round(nearBudgetFraction * 100)}% of their budget or failed (${noteworthy.length}):`,
  )
  lines.push('| Test | Time | Budget | Headroom | Status |')
  lines.push('| --- | ---: | ---: | ---: | --- |')
  for (const test of noteworthy.slice(0, maxRows)) {
    const headroom =
      test.budget === null || test.duration === null
        ? 'unknown'
        : formatSeconds(test.budget - test.duration)
    lines.push(
      `| ${test.file} › ${test.name.replaceAll('|', '\\|')} | ${test.duration === null ? 'unknown' : formatSeconds(test.duration)} | ${test.budget === null ? 'unknown' : formatSeconds(test.budget)} | ${headroom} | ${test.status} |`,
    )
  }
  if (!noteworthy.length) lines.push('| None | — | — | — | — |')
  if (noteworthy.length > maxRows)
    lines.push(`${noteworthy.length - maxRows} more notable tests omitted; see the JSON artifact.`)

  lines.push(`Longest files (top ${Math.min(maxRows, fileCosts.length)}):`)
  for (const { file, duration } of fileCosts
    .sort((a, b) => b.duration - a.duration)
    .slice(0, maxRows)) {
    lines.push(`- ${file}: ${formatSeconds(duration)}`)
  }
  return lines.join('\n') + '\n'
}

export const reportArtifact = (path, shard) => {
  let output
  if (!existsSync(path)) {
    output = `### Compiler shard ${shard} timing report\nTiming JSON is missing; inspect the Vitest output above for the original failure.\n`
  } else {
    try {
      output = renderReport(JSON.parse(readFileSync(path, 'utf8')), { shard })
    } catch (error) {
      output = `### Compiler shard ${shard} timing report\nTiming JSON could not be read (${error instanceof Error ? error.message : String(error)}); inspect the Vitest output above.\n`
    }
  }
  process.stdout.write(output)
  if (process.env.GITHUB_STEP_SUMMARY) {
    try {
      appendFileSync(process.env.GITHUB_STEP_SUMMARY, output + '\n')
    } catch (error) {
      process.stderr.write(
        `Could not write timing summary: ${error instanceof Error ? error.message : String(error)}\n`,
      )
    }
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  reportArtifact(process.argv[2], process.argv[3] ?? '?')
}
