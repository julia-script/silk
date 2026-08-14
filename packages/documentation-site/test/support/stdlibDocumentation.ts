import * as Analysis from '@silk-effect/compiler/Analysis'
import * as CompilerStdlib from '@silk-effect/compiler/Stdlib'
import * as Json from '@silk-effect/documentation/Json'
import * as DocumentationProject from '@silk-effect/documentation/Project'
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
  const modules: Array<DocumentationProject.Module> = []
  for (const entry of CompilerStdlib.manifest) {
    const bytes = CompilerStdlib.sources.get(entry.module)
    if (bytes === undefined) continue
    const snapshot = yield* Analysis.ofSource(entry.module, bytes)
    const documented = DocumentationProject.make(snapshot, { includePrivate: false }).modules.find(
      (module) => module.name === entry.module,
    )
    if (documented !== undefined) modules.push(documented)
  }
  return Json.encode(
    Object.freeze({
      schema: 'silk-documentation',
      experimental: true,
      modules: Object.freeze(modules),
    }),
  )
})

/**
 * The same text for every test in the file, built once.
 *
 * Building it analyzes every module of the shipped manifest and its import closure, which costs
 * around a minute; three tests need the same bytes, and rebuilding per test spent that minute three
 * times over — enough to blow a test timeout on a loaded runner while proving nothing new.
 */
let pending: Promise<string> | undefined

export const encoded: Effect.Effect<string> = Effect.promise(
  () => (pending ??= Effect.runPromise(build())),
)

/** The module identities the shipped manifest promises, in its own order. */
export const manifestModules: ReadonlyArray<string> = CompilerStdlib.manifest.map(
  (entry) => entry.module,
)
