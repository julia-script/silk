/**
 * Wall-clock budget for a test that drives the native toolchain: Clang emits objects for the
 * program and every runtime object, links them, and the test then executes the result.
 *
 * A single build gets 60 seconds on shared CI runners. Tests that deliberately drive several
 * compiler pipelines use a corresponding multiple at the call site. Source startup is now part
 * of compilation: the init/check/build/run integration takes about 20 seconds locally and has
 * exceeded one 60-second build budget on Linux CI.
 *
 * This is not a performance gate. Nothing here asserts how fast a build is; the budget exists
 * only so a correctness assertion is never reported as a timeout. If build speed is worth
 * guarding, that belongs in a separate test that says so.
 */
export const nativeBuild = 60_000
