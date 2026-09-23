import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { test } from 'node:test'
import { assignTests, discoverTests } from './compiler-shards.mjs'

const baseline = JSON.parse(
  readFileSync(new URL('./compiler-shard-timings.json', import.meta.url), 'utf8'),
)

test('the CI assignment covers every eligible compiler file once, including a new file', () => {
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

test('measured-cost assignment is stable and balances the reviewed baseline', () => {
  const files = discoverTests()
  const first = assignTests(files, baseline.timings)
  const reversed = assignTests([...files].reverse(), baseline.timings)
  assert.deepEqual(first, reversed)
  assert.deepEqual(first.unmeasured, [])
  const totals = first.shards.map((shard) => shard.totalMs)
  assert.ok(Math.max(...totals) - Math.min(...totals) < 1_000)
})
