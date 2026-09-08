import * as Effect from 'effect/Effect'
import * as Analysis from '../../src/Analysis.js'
import type * as Frontend from '../../src/Frontend.js'
import type * as ModuleClosure from '../../src/ModuleClosure.js'
import * as SourceFile from '../../src/SourceFile.js'
import * as SourceResolver from '../../src/SourceResolver.js'

/** Explicit test composition for language claims that retain main without executing startup. */
export const configuration = (
  module: string,
  target = 'x86_64-unknown-linux-gnu',
  declarations: ReadonlyArray<string> = ['main'],
): NonNullable<ModuleClosure.CompilationRequest['configuration']> => ({
  profile: { target, artifact: 'object', runtime: { kind: 'none' } },
  composition: {
    runtimes: [],
    defaults: [],
    retention: declarations.map((declaration) => ({ module, declaration })),
    requirements: [],
    components: [
      {
        capability: 'execution-storage',
        bindings: ['create', 'acquire', 'release', 'destroy'].map((operation) => ({
          operation,
          module: 'silk/execution_storage',
          declaration: `silk_execution_storage_${operation}`,
        })),
      },
    ],
  },
})

/** Builds the frontend once with explicit retained roots for a later realization assertion. */
export const frontend = Effect.fnUntraced(function* (
  sourceId: string,
  bytes: Uint8Array,
  target = 'x86_64-unknown-linux-gnu',
): Effect.fn.Return<Analysis.SingleRootFrontendSnapshot> {
  return yield* Analysis.make({
    root: SourceFile.make(sourceId, bytes),
    configuration: configuration(sourceId, target),
  }).pipe(Effect.provide(SourceResolver.empty))
})

/** Realizes a retained function graph; default executable composition has separate conformance. */
export const retainingMain = Effect.fnUntraced(function* (
  sourceId: string,
  bytes: Uint8Array,
  target = 'x86_64-unknown-linux-gnu',
  options: Frontend.Options = {},
): Effect.fn.Return<Analysis.Snapshot> {
  const selected = configuration(sourceId, target)
  return yield* Analysis.make({
    root: SourceFile.make(sourceId, bytes),
    configuration: selected,
  }).pipe(
    Effect.flatMap((frontend) => Analysis.realize(frontend, selected, options)),
    Effect.provide(SourceResolver.empty),
  )
})

/** Checks declarations with no retained application function or executable startup. */
export const declarations = Effect.fnUntraced(function* (
  sourceId: string,
  bytes: Uint8Array,
  target = 'x86_64-unknown-linux-gnu',
): Effect.fn.Return<Analysis.Snapshot> {
  const selected = configuration(sourceId, target, [])
  return yield* Analysis.makeRealized({
    root: SourceFile.make(sourceId, bytes),
    configuration: selected,
  }).pipe(Effect.provide(SourceResolver.empty))
})
