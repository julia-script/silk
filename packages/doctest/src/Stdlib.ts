import * as Analysis from '@silk-effect/compiler/Analysis'
import * as CompilerStdlib from '@silk-effect/compiler/Stdlib'
import * as DocumentationProject from '@silk-effect/documentation/Project'
import * as Effect from 'effect/Effect'
import type * as Sources from './Sources.js'

/**
 * Documentation for the compiler-shipped standard library, and the source lookup that goes with it.
 *
 * `silk doc` documents a project rooted at one module, and the standard library is not a project —
 * it is 38 modules the compiler carries, with no root that reaches all of them. So the value is
 * built the same way the generated standard-library page is built: one analysis per manifest entry,
 * each contributing its own documented module. Coverage follows the manifest, so a module added to
 * the library is doctested without anything here being edited.
 */

/** Answers with the compiler-shipped bytes of a standard-library module. */
export const sources: Sources.Lookup = (sourceId) => CompilerStdlib.sources.get(sourceId)

/**
 * Builds documentation for every standard-library module, private declarations included.
 *
 * Private declarations are documented here because a doctest cares about whether an example
 * compiles, not about whether its declaration is part of the published surface.
 */
export const documentation = Effect.fn('Stdlib.documentation')(function* (): Effect.fn.Return<
  DocumentationProject.Project,
  never,
  never
> {
  const modules: Array<DocumentationProject.Module> = []
  for (const entry of CompilerStdlib.manifest) {
    const bytes = CompilerStdlib.sources.get(entry.module)
    if (bytes === undefined) continue
    const snapshot = yield* Analysis.ofSource(entry.module, bytes)
    const project = DocumentationProject.make(snapshot, { includePrivate: true })
    const documented = project.modules.find((module) => module.name === entry.module)
    if (documented !== undefined) modules.push(documented)
  }
  return Object.freeze({
    schema: 'silk-documentation',
    experimental: true,
    modules: Object.freeze(modules),
  })
})
