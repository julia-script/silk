import * as Analysis from '@silk-lang/compiler/Analysis'
import * as SourceFile from '@silk-lang/compiler/SourceFile'
import * as SourceResolver from '@silk-lang/compiler/SourceResolver'
import * as Effect from 'effect/Effect'

/** Builds one browser snapshot from immutable virtual sources at the application boundary. */
export const make = (request: {
  readonly rootModule: string
  readonly sources: ReadonlyMap<string, Uint8Array>
  readonly target?: string
}): Analysis.Snapshot => {
  const rootBytes = request.sources.get(request.rootModule)
  if (rootBytes === undefined) {
    throw new RangeError(`Browser snapshot has no root source ${request.rootModule}`)
  }
  const compilation =
    request.target === undefined
      ? { root: SourceFile.make(request.rootModule, rootBytes) }
      : { root: SourceFile.make(request.rootModule, rootBytes), target: request.target }
  return Effect.runSync(
    Analysis.makeRealized(compilation).pipe(Effect.provide(SourceResolver.memory(request.sources))),
  )
}

/** Builds one single-source browser snapshot with the compiler's empty resolver. */
export const ofSource = (
  sourceId: string,
  bytes: Uint8Array,
  target?: string,
): Analysis.Snapshot => Effect.runSync(Analysis.ofSourceRealized(sourceId, bytes, target))
