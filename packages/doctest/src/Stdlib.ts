import * as ProjectAnalysis from '@silk-lang/compiler/ProjectAnalysis'
import * as SourceFile from '@silk-lang/compiler/SourceFile'
import * as SourceResolver from '@silk-lang/compiler/SourceResolver'
import * as CompilerStdlib from '@silk-lang/compiler/Stdlib'
import * as DocumentationProject from '@silk-lang/documentation/Project'
import * as Effect from 'effect/Effect'
import type * as Sources from './Sources.js'

/**
 * Documentation for the compiler-shipped standard library, and the source lookup that goes with it.
 *
 * Every manifest entry is a project root, so the compiler can analyze their union once while still
 * covering modules that are not reachable from one distinguished root. Coverage follows the
 * manifest, so a module added to the library is doctested without anything here being edited.
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
  const roots: Array<SourceFile.SourceFile> = []
  for (const entry of CompilerStdlib.manifest) {
    const bytes = CompilerStdlib.sources.get(entry.module)
    if (bytes === undefined)
      return yield* Effect.die(`Missing compiler-shipped stdlib source: ${entry.module}`)
    roots.push(SourceFile.make(entry.module, bytes))
  }
  if (roots.length === 0) return yield* Effect.die('The stdlib manifest is empty')
  const analysis = yield* ProjectAnalysis.make(roots).pipe(Effect.provide(SourceResolver.empty))
  return DocumentationProject.fromProjectAnalysis(analysis, { includePrivate: true })
})
