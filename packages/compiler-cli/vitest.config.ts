import { defineConfig } from 'vitest/config'
import * as Timeouts from './test/timeouts.js'

export default defineConfig({
  test: {
    // Tests in this package shell out to Clang to compile and link real executables, which costs
    // far more than Vitest's 5 s default on a loaded shared runner. Raising the package default
    // keeps a newly added build test from silently inheriting a budget below the cost of the
    // operation it exercises, which is how #147 reached `main`.
    testTimeout: Timeouts.nativeBuild,
    hookTimeout: Timeouts.nativeBuild,
  },
})
