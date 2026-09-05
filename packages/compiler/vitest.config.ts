import { homedir } from 'node:os'
import { join } from 'node:path'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import { defineSilkConfig, wholeMachineWorkers } from '../../vitest.shared.js'

const nativeCacheDirectory = Effect.runSync(
  Config.string('SILK_NATIVE_CACHE_DIR').pipe(
    Config.withDefault(join(homedir(), '.cache', 'silk-effect', 'native')),
  ),
)

export default defineSilkConfig({
  test: {
    /**
     * The one package that raises this, and the acceptance criterion of #173 asks for the reason
     * rather than for the raise to be removed.
     *
     * This suite is ~2,580 s of the workspace's ~3,100 s of test CPU and it is the critical path of
     * `pnpm check`: everything else finishes while it is still running, so for most of the gate it
     * is the only thing on the machine. On vitest's default of one worker short of the core count,
     * that leaves a core idle for roughly ten minutes and lengthens the whole gate. The rest of the
     * workspace stays on the default, which is where the spare core earns its keep — see the note
     * in `vitest.shared.ts`.
     *
     * Derived from the host, not from today's runner.
     */
    maxWorkers: wholeMachineWorkers,
    env: {
      // Shared across worktrees for backend emission and independently eligible cache entries.
      SILK_NATIVE_CACHE_DIR: nativeCacheDirectory,
    },
  },
})
