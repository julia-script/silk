import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    // The lab presets drive the real compiler pipeline, so a case can take tens of seconds on a
    // cold CI runner. Matches packages/compiler.
    testTimeout: 30_000,
    hookTimeout: 30_000,
  },
})
