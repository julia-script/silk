import * as Effect from 'effect/Effect'
import type * as Project from '../../src/Project.js'
import * as Stdlib from '../../src/Stdlib.js'

/**
 * The standard library's documentation, built once for the whole test file.
 *
 * Three tests need the same immutable value, so they share one project analysis rather than proving
 * the same input-to-documentation relationship repeatedly.
 *
 * The memo lives here rather than in `Stdlib.documentation` itself: a module-level cache in the
 * shipped workflow would hand a long-running process a stale answer after the sources changed
 * under it, which is a real behavior change and not one this needs.
 */
let pending: Promise<Project.Project> | undefined

export const documentation: Effect.Effect<Project.Project> = Effect.promise(
  () => (pending ??= Effect.runPromise(Stdlib.documentation('aarch64-apple-darwin'))),
)
