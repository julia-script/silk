import { availableParallelism } from 'node:os'

/**
 * How many test workers the workspace may have runnable at once, as a multiple of the host's
 * cores.
 *
 * Not 1x. A Vitest worker spends a large part of its wall-clock transforming and importing rather
 * than on the CPU — a 490 s compiler run measured 110 s of import alone — so one extra runnable
 * worker per core is absorbed by that idle time instead of queueing behind it. Beyond 2x it stops
 * being absorbed: at the ~12x this workspace reached before this bound existed, `packages/lsp`'s
 * suite ran 3.4x its idle cost and its slowest test crossed Vitest's 5 s default, which is how a
 * green branch turned red in a package it had not touched.
 */
const OVERSUBSCRIPTION = 2

/** Never serialize the run down to a single task — that trades a flaky gate for a slow one. */
const MINIMUM_CONCURRENCY = 2

export const hostParallelism = () => Math.max(1, availableParallelism())

/**
 * How many Turbo tasks may run at once, derived from the host rather than from today's runner.
 *
 * The unit being bounded is the test *worker*, not the task: `turbo run test` starts one process
 * per package and each of those starts a worker per core, so the total is the product of the two
 * and neither factor alone bounds it. `vitest.shared.ts` deliberately lets one suite use every
 * core, so the budget divided by that is how many suites may overlap.
 */
export const deriveConcurrency = (cpus = hostParallelism(), workersPerTask = cpus) =>
  Math.max(MINIMUM_CONCURRENCY, Math.floor((cpus * OVERSUBSCRIPTION) / Math.max(1, workersPerTask)))

/**
 * How much of the machine one task of this run takes.
 *
 * A `test` task starts a Vitest process that spreads over vitest's own host-derived default, one
 * worker short of the core count — keep this in step with `vitest.shared.ts`, since a smaller
 * per-suite worker count would let proportionally more suites overlap rather than fewer.
 * Everything else in this workspace is a single `tsc` or `next build` process, so bounding those
 * to the same count would cost wall-clock and prevent nothing.
 */
export const workersPerTask = (tasks, cpus = hostParallelism()) =>
  tasks.includes('test') ? Math.max(1, cpus - 1) : 1

/** The tasks of a `turbo run <task...> [flags]` invocation. */
export const tasksOf = (argv) => {
  const run = argv.indexOf('run')
  if (run === -1) return []
  const tasks = []
  for (const arg of argv.slice(run + 1)) {
    if (arg.startsWith('-')) break
    tasks.push(arg)
  }
  return tasks
}
