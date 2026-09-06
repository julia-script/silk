/**
 * Wall-clock budget for a test that drives the native toolchain: Clang emits objects for the
 * program and every runtime object, links them, and the test then executes the result.
 *
 * The value is sized from measurement, not from feel. On an idle 4-core machine the slowest
 * such test in this package (`Cli.test.ts > initializes a project that loads, checks, builds,
 * and runs with defaults`) takes ~474 ms. Issue #147 recorded `Workflow.test.ts > builds to the
 * deterministic target and optimization path` taking 13,572 ms on a shared CI runner, against ~238 ms
 * for the same test locally — an amplification of roughly 57x. Applying that amplification to
 * the slowest test puts the worst case near 27 s, so 60 s leaves a little over 2x headroom.
 *
 * This is not a performance gate. Nothing here asserts how fast a build is; the budget exists
 * only so a correctness assertion is never reported as a timeout. If build speed is worth
 * guarding, that belongs in a separate test that says so.
 */
export const nativeBuild = 60_000
