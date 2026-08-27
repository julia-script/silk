import * as ProjectAnalysis from '@silk-lang/compiler/ProjectAnalysis'
import * as SourceFile from '@silk-lang/compiler/SourceFile'
import * as SourceResolver from '@silk-lang/compiler/SourceResolver'
import * as CompilerStdlib from '@silk-lang/compiler/Stdlib'
import * as Json from '@silk-lang/documentation/Json'
import * as DocumentationProject from '@silk-lang/documentation/Project'
import * as Effect from 'effect/Effect'

/**
 * Real documentation JSON for the compiler-shipped standard library, as text.
 *
 * The compiler and the documentation model appear here and only here — as development dependencies
 * of a test, never as dependencies of the renderer. That is deliberate: the renderer's boundary is
 * that it declares no workspace dependency at all, and the way to keep its local view of the JSON
 * from drifting is to feed it what the emitter really writes rather than a fixture that ages.
 */
const build = Effect.fn('stdlibDocumentation.build')(function* (): Effect.fn.Return<
  string,
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
  return Json.encode(DocumentationProject.fromProjectAnalysis(analysis, { includePrivate: false }))
})

/**
 * The same text for every test in the file, built once.
 *
 * Three tests need the same immutable bytes, so they share one project analysis rather than proving
 * the same input-to-documentation relationship repeatedly.
 */
let pending: Promise<string> | undefined

export const encoded: Effect.Effect<string> = Effect.promise(
  () => (pending ??= Effect.runPromise(build())),
)

/** The module identities the shipped manifest promises, in its own order. */
export const manifestModules: ReadonlyArray<string> = CompilerStdlib.manifest.map(
  (entry) => entry.module,
)
