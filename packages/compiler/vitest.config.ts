import { homedir } from 'node:os'
import { join } from 'node:path'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    testTimeout: 30_000,
    hookTimeout: 30_000,
    maxWorkers: 4,
    env: {
      // Shared across worktrees: identical bitcode + Clang version → cached executable.
      SILK_NATIVE_CACHE_DIR:
        process.env.SILK_NATIVE_CACHE_DIR ?? join(homedir(), '.cache', 'silk-effect', 'native'),
    },
  },
})
