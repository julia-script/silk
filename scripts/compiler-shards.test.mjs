import assert from 'node:assert/strict'
import { spawnSync } from 'node:child_process'
import { mkdirSync, mkdtempSync, readFileSync, realpathSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join, relative } from 'node:path'
import { test } from 'node:test'
import { fileURLToPath } from 'node:url'
import { assignTests, discoverTests, exclusionFlags } from './compiler-shards.mjs'

const baseline = JSON.parse(
  readFileSync(new URL('./compiler-shard-timings.json', import.meta.url), 'utf8'),
)

await test('the CI assignment covers every eligible compiler file once, including a new file', () => {
  const files = discoverTests()
  const added = 'test/NewCompilerFeature.test.ts'
  const { shards, unmeasured } = assignTests([...files, added], baseline.timings)
  const assigned = shards.flatMap((shard) => shard.files)
  assert.deepEqual(
    [...assigned].sort((a, b) => a.localeCompare(b)),
    [...files, added].sort((a, b) => a.localeCompare(b)),
  )
  assert.equal(new Set(assigned).size, files.length + 1)
  assert.deepEqual(unmeasured, [added])
  assert.ok(shards.every((shard) => shard.files.length > 0))
})

await test('measured-cost assignment is stable and balances the reviewed baseline', () => {
  const files = discoverTests()
  const first = assignTests(files, baseline.timings)
  const reversed = assignTests([...files].reverse(), baseline.timings)
  assert.deepEqual(first, reversed)
  assert.deepEqual(first.unmeasured, [])
  const totals = first.shards.map((shard) => shard.totalMs)
  assert.ok(Math.max(...totals) - Math.min(...totals) < 1_000)
})

await test('Vitest selects same-stem test files on only their assigned shards', (context) => {
  const directory = realpathSync(mkdtempSync(join(tmpdir(), 'silk-compiler-shards-')))
  context.after(() => rmSync(directory, { recursive: true, force: true }))
  mkdirSync(join(directory, 'test'))
  const files = [
    'test/Analysis.test.ts',
    'test/Analysis.test.tsx',
    'test/Other.test.ts',
    'test/Fourth.test.ts',
  ]
  for (const file of files) writeFileSync(join(directory, file), '')
  const config = join(directory, 'vitest.config.mjs')
  writeFileSync(config, 'export default {}\n')

  const { shards } = assignTests(files, {
    'test/Analysis.test.ts': 100,
    'test/Analysis.test.tsx': 90,
    'test/Other.test.ts': 80,
    'test/Fourth.test.ts': 70,
  })
  const vitest = fileURLToPath(new URL('../node_modules/.bin/vitest', import.meta.url))
  const selected = []
  for (const [index, shard] of shards.entries()) {
    const result = spawnSync(
      vitest,
      [
        'list',
        '--root',
        directory,
        '--config',
        config,
        '--filesOnly',
        '--json',
        ...exclusionFlags(files, shards, index),
      ],
      { encoding: 'utf8', env: { ...process.env, CI: 'true' } },
    )
    assert.equal(result.status, 0, result.stderr)
    const actual = JSON.parse(result.stdout).map(({ file }) => relative(directory, file))
    assert.deepEqual(
      actual.sort((a, b) => a.localeCompare(b)),
      shard.files,
    )
    selected.push(...actual)
  }
  assert.equal(new Set(selected).size, files.length)
  assert.deepEqual(
    selected.sort((a, b) => a.localeCompare(b)),
    files.sort((a, b) => a.localeCompare(b)),
  )
})
