import { defineConfig } from 'vitest/config'

/**
 * Building the standard library's documentation analyzes every module of the shipped manifest and
 * its import closure, which is minutes of CPU. Turbo runs many package test tasks at once, so a
 * package that spawns a worker per file turns that into contention that tips unrelated tests over
 * their own timeouts. Cap the workers, as the compiler package does for the same reason.
 */
export default defineConfig({
  test: {
    testTimeout: 30_000,
    hookTimeout: 30_000,
    maxWorkers: 2,
  },
})
