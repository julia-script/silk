import assert from 'node:assert/strict'
import { test } from 'node:test'
import { deriveConcurrency, hostParallelism, tasksOf, workersPerTask } from './concurrency.mjs'

const cpuCounts = [1, 2, 4, 8, 16, 64]

test('a run containing tests overlaps a fixed number of suites at any machine size', () => {
  // A suite may use every core, so the number of suites allowed to overlap is the oversubscription
  // factor itself and holds at any host size — what scales with the host is the worker budget
  // underneath it.
  for (const cpus of cpuCounts) {
    const concurrency = deriveConcurrency(cpus, workersPerTask(['typecheck', 'test'], cpus))
    assert.equal(concurrency, 2, `unexpected concurrency for ${cpus} cores`)
  }
})

test('the total worker bound tracks the host rather than a hardcoded runner', () => {
  for (const cpus of cpuCounts) {
    const concurrency = deriveConcurrency(cpus, workersPerTask(['test'], cpus))
    assert.equal(concurrency * cpus, cpus * 2, `unexpected worker budget for ${cpus} cores`)
  }
})

test('a run of single-process tasks is not held to the test bound', () => {
  // `tsc` takes one core, so bounding builds and typechecks to the test concurrency would cost
  // wall-clock without preventing any oversubscription.
  assert.equal(deriveConcurrency(4, workersPerTask(['build'], 4)), 8)
  assert.equal(deriveConcurrency(4, workersPerTask(['typecheck'], 4)), 8)
})

test('the run is never serialized down to one task', () => {
  for (const cpus of cpuCounts) {
    assert.ok(deriveConcurrency(cpus, workersPerTask(['test'], cpus)) >= 2, `${cpus} cores`)
  }
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
