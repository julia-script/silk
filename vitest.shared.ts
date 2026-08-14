import { availableParallelism } from 'node:os'
import { defineConfig, mergeConfig, type ViteUserConfig } from 'vitest/config'

/**
 * Workspace-wide test defaults.
 *
 * Every package that runs Vitest extends this file, so the numbers below are decided once for the
 * workspace rather than rediscovered by each package after it has turned someone else's pull
 * request red. `scripts/test-config-coverage.test.mjs` fails the gate if a package with tests
 * stops extending it.
 */

/**
 * A test suite that is running on its own may use the whole machine — one worker per core is not
 * oversubscription, it is the machine being used. What has to be bounded is how many suites run
 * at once, and that is `scripts/concurrency.mjs`, not this file.
 *
 * This is deliberately not a cap. The compiler suite is ~80% of the workspace's test CPU and sits
 * on the critical path of `pnpm check`; taking a worker away from it lengthens the whole gate.
 */
export const testWorkers = Math.max(1, availableParallelism())

/**
 * Vitest's default is 5 s, which is a budget for a test that does arithmetic, not for one that
 * drives a compiler. Two of the flaky-CI issues on this repo (#147, #173) were a test whose real
 * cost was well inside 5 s locally and outside it on a loaded shared runner: `packages/compiler-cli`
 * measured an amplification of ~57x between an idle machine and a contended one, and
 * `packages/lsp`'s slowest test runs 2.3 s locally, which needs only a 2.2x amplification to be
 * reported as a timeout rather than as a result.
 *
 * 30 s is not a performance gate and nothing here asserts how fast anything is. It exists so that
 * a correctness assertion is never reported as a timeout. A package whose work genuinely costs
 * more than this says so in its own config, with the measurement that justifies it — see
 * `packages/compiler-cli/test/timeouts.ts`.
 */
export const testTimeout = 30_000

export const silkTestDefaults = {
  testTimeout,
  hookTimeout: testTimeout,
  maxWorkers: testWorkers,
} as const

/** Extend the workspace defaults with whatever a package genuinely needs on top of them. */
export const defineSilkConfig = (overrides: ViteUserConfig = {}): ViteUserConfig =>
  mergeConfig(defineConfig({ test: { ...silkTestDefaults } }), defineConfig(overrides))

export default defineSilkConfig()
