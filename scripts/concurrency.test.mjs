import assert from 'node:assert/strict'
import { test } from 'node:test'
import { deriveConcurrency, hostParallelism, tasksOf, workersPerTask } from './concurrency.mjs'

const cpuCounts = [1, 2, 4, 8, 16, 64]

test('a run containing tests keeps the peak within the worker budget', () => {
  for (const cpus of cpuCounts) {
    const workers = workersPerTask(['typecheck', 'test'], cpus)
    const peak = deriveConcurrency(cpus, workers, true) * workers
    assert.ok(peak <= cpus, `peak ${peak} workers exceeds the budget for ${cpus} cores`)
  }
})

test("the per-suite worker count is vitest's host-derived default", () => {
  // Kept in step with vitest.shared.ts, which deliberately does not override it: the spare core
  // is what a syscall-heavy test needs to stay schedulable (#177).
  assert.equal(workersPerTask(['test'], 4), 3)
  assert.equal(workersPerTask(['test'], 2), 1)
  assert.equal(workersPerTask(['test'], 1), 1)
})

test('a run of single-process tasks is not held to the test bound', () => {
  // `tsc` takes one core, so bounding builds and typechecks to the test concurrency would cost
  // wall-clock without preventing any oversubscription.
  assert.equal(deriveConcurrency(4, workersPerTask(['build'], 4)), 8)
  assert.equal(deriveConcurrency(4, workersPerTask(['typecheck'], 4)), 8)
})

test('a full-machine test suite is serialized while one-worker suites may share the host', () => {
  assert.equal(deriveConcurrency(10, workersPerTask(['test'], 10), true), 1)
  assert.equal(deriveConcurrency(4, workersPerTask(['test'], 4), true), 1)
  assert.equal(deriveConcurrency(2, workersPerTask(['test'], 2), true), 2)
  assert.equal(deriveConcurrency(1, workersPerTask(['test'], 1), true), 1)
})

test('tasks are read from the turbo invocation, not from its flags', () => {
  assert.deepEqual(tasksOf(['run', 'typecheck', 'test', '--force']), ['typecheck', 'test'])
  assert.deepEqual(tasksOf(['run', 'build']), ['build'])
  assert.deepEqual(tasksOf(['run', 'dev', '--concurrency=15']), ['dev'])
  assert.deepEqual(tasksOf(['--version']), [])
})

test('the host reports a whole number of at least one core', () => {
  assert.ok(hostParallelism() >= 1)
  assert.equal(Number.isInteger(hostParallelism()), true)
})
