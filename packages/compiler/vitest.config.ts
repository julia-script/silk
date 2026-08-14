import { homedir } from 'node:os'
import { join } from 'node:path'
import { defineSilkConfig } from '../../vitest.shared.js'

export default defineSilkConfig({
  test: {
    env: {
      // Shared across worktrees: identical bitcode + Clang version → cached executable.
      SILK_NATIVE_CACHE_DIR:
        process.env.SILK_NATIVE_CACHE_DIR ?? join(homedir(), '.cache', 'silk-effect', 'native'),
    },
  },
})
