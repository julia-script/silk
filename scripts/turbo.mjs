#!/usr/bin/env node
import { spawn } from 'node:child_process'
import { createRequire } from 'node:module'
import { deriveConcurrency, hostParallelism, tasksOf, workersPerTask } from './concurrency.mjs'

/**
 * Runs Turbo with a concurrency bound derived from the host.
 *
 * Turbo's own default — and the literal `"concurrency": "15"` this replaces — is a count of tasks,
 * which says nothing about how much of the machine those tasks take. `turbo run typecheck test`
 * schedules every package at once and each test task starts a worker per core, so on a 2-core
 * runner the workspace asked for an order of magnitude more runnable workers than it had cores.
 * Nothing failed outright; whichever suite had the least timeout headroom at that moment was
 * reported as a timeout, which is why the failing test kept moving between packages.
 *
 * Every workspace entry point goes through here so there is one place that decides this. A caller
 * that passes its own `--concurrency` keeps it: `pnpm dev` starts a persistent watch task per
 * package and Turbo refuses to run when the bound is below that count, and a watch session is not
 * the thing being bounded here anyway.
 */

const args = process.argv.slice(2)
const hasExplicitConcurrency = args.some(
  (arg) => arg === '--concurrency' || arg.startsWith('--concurrency='),
)

const cpus = hostParallelism()
const tasks = tasksOf(args)
const concurrency = deriveConcurrency(cpus, workersPerTask(tasks, cpus), tasks.includes('test'))
const turboArgs = hasExplicitConcurrency ? args : [...args, `--concurrency=${concurrency}`]

if (!hasExplicitConcurrency) {
  console.log(`turbo: --concurrency=${concurrency} (${cpus} available cores)`)
}

// Resolve Turbo's own entry point rather than trusting PATH, so this behaves the same whether it
// is reached through a package script or run directly.
const require = createRequire(import.meta.url)
const turboBin = require.resolve('turbo/bin/turbo')

/**
 * Worktrees under `.claude/worktrees/` share the main checkout's Turbo cache. Turbo's hashes are
 * repo-relative, so a task cached in one worktree is a valid hit in another; without this every
 * fresh worktree re-runs the whole gate cold. Cache writes are content-addressed and atomic, so
 * concurrent runs at worst duplicate a write. An explicit TURBO_CACHE_DIR still wins.
 */
const worktreeRoot = process.cwd().match(/^(.*?)[\\/]\.claude[\\/]worktrees[\\/]/)
const env =
  worktreeRoot && !process.env.TURBO_CACHE_DIR
    ? { ...process.env, TURBO_CACHE_DIR: `${worktreeRoot[1]}/.turbo/cache` }
    : process.env

const child = spawn(process.execPath, [turboBin, ...turboArgs], { stdio: 'inherit', env })

child.on('error', (error) => {
  console.error(`turbo: failed to start (${error.message})`)
  process.exit(1)
})

child.on('exit', (code, signal) => {
  if (signal !== null) {
    process.kill(process.pid, signal)
    return
  }
  process.exit(code ?? 1)
})
