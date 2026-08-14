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
 * The floor is 60 s because 30 s was measured to be too low, not because 60 is a round number.
 * `pnpm check` saturates the runner end to end — the compiler suite alone is ~2,580 s of test CPU
 * on a 4-core host — and at that saturation the slowest tests in `packages/compiler` legitimately
 * run 30–38 s: `StackVmPressureDeterminism` 37.6 s, `ModuleVerification` 31.4 s,
 * `LexerPressureDeterminism` 30.3 s. Those pass only because each carries an explicit timeout;
 * `ChildProcess.test.ts > lowers the native execution to reachable native-only runtime symbols`
 * carries none, inherited a 30 s floor, and was reported as a timeout on CI. A floor a test in the
 * workspace already exceeds is not a floor. 60 s also matches the value `packages/compiler-cli`
 * arrived at independently from its own measurement.
 *
 * This is not a performance gate and nothing here asserts how fast anything is. It exists so that
 * a correctness assertion is never reported as a timeout. The cost of raising it is that a test
 * that genuinely hangs takes a minute to say so, which is the trade `packages/compiler-cli` had
 * already accepted for the same reason.
 */
export const testTimeout = 60_000

export const silkTestDefaults = {
  testTimeout,
  hookTimeout: testTimeout,
  maxWorkers: testWorkers,
} as const

/** Extend the workspace defaults with whatever a package genuinely needs on top of them. */
export const defineSilkConfig = (overrides: ViteUserConfig = {}): ViteUserConfig =>
  mergeConfig(defineConfig({ test: { ...silkTestDefaults } }), defineConfig(overrides))

export default defineSilkConfig()
