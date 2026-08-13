import type * as Project from '@silk-effect/documentation/Project'
import * as Effect from 'effect/Effect'
import * as Stdlib from '../../src/Stdlib.js'

/**
 * The standard library's documentation, built once for the whole test file.
 *
 * Building it analyzes every module of the shipped manifest and its import closure, which costs
 * around a minute. Three tests need the same value, and rebuilding it per test spent that minute
 * three times over — enough to blow a test timeout on a loaded runner while proving nothing.
 *
 * The memo lives here rather than in `Stdlib.documentation` itself: a module-level cache in the
 * shipped workflow would hand a long-running process a stale answer after the sources changed
 * under it, which is a real behavior change and not one this needs.
 */
let pending: Promise<Project.Project> | undefined

export const documentation: Effect.Effect<Project.Project> = Effect.promise(
  () => (pending ??= Effect.runPromise(Stdlib.documentation())),
)
