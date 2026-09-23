import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { test } from 'node:test'
import { fileURLToPath } from 'node:url'
import { renderReport, testBudgets } from './compiler-timing-report.mjs'

const source = `
it.effect('ordinary', () => work())
it.effect('extended', () => work(), { timeout: 120_000 })
`
const file = '/home/runner/work/silk/silk/packages/compiler/test/Sample.test.ts'
const assertion = (title, duration, status = 'passed') => ({
  title,
  fullName: title,
  duration,
  status,
})
const result = (assertions, startTime = 1000, endTime = 126000) => ({
  name: file,
  startTime,
  endTime,
  assertionResults: assertions,
})
const options = { shard: 2, defaultTimeoutMs: 60_000, readSource: () => source }

test('reads explicit test budgets and reports real headroom and shard totals', () => {
  assert.deepEqual(
    [...testBudgets(source, 60_000)],
    [
      ['ordinary', 60_000],
      ['extended', 120_000],
    ],
  )
  const report = renderReport(
    {
      success: true,
      testResults: [result([assertion('ordinary', 48_000), assertion('extended', 100_000)])],
    },
    options,
  )
  assert.match(report, /Actual shard total: 125\.0s summed file time; 125\.0s test-run span/)
  assert.match(report, /ordinary \| 48\.0s \| 60\.0s \| 12\.0s/)
  assert.match(report, /extended \| 100\.0s \| 120\.0s \| 20\.0s/)
})

test('reports timed-out tests and tolerates partial valid JSON', () => {
  const report = renderReport(
    {
      success: false,
      testResults: [
        result([assertion('ordinary', 60_012, 'failed')], 1000, 62012),
        { name: '/unexpected/path', assertionResults: [] },
      ],
    },
    options,
  )
  assert.match(report, /Result: failed or incomplete/)
  assert.match(report, /Actual shard total: 61\.0s summed file time/)
  assert.match(report, /ordinary \| 60\.0s \| 60\.0s \| -0\.01s \| failed/)
  assert.match(report, /totals may be partial/)
  assert.match(renderReport({ success: false }, options), /Timing JSON has no test results/)
})

test('missing and malformed artifacts keep the reporting step successful', (context) => {
  const directory = mkdtempSync(join(tmpdir(), 'silk-compiler-report-'))
  context.after(() => rmSync(directory, { recursive: true, force: true }))
  const script = fileURLToPath(new URL('./compiler-timing-report.mjs', import.meta.url))
  const missing = spawnSync(process.execPath, [script, join(directory, 'missing.json'), '4'], {
    encoding: 'utf8',
  })
  assert.equal(missing.status, 0, missing.stderr)
  assert.match(missing.stdout, /Timing JSON is missing/)

  const malformedPath = join(directory, 'partial.json')
  writeFileSync(malformedPath, '{"testResults": [')
  const malformed = spawnSync(process.execPath, [script, malformedPath, '4'], {
    encoding: 'utf8',
  })
  assert.equal(malformed.status, 0, malformed.stderr)
  assert.match(malformed.stdout, /Timing JSON could not be read/)
})
