import * as Analysis from '@silk-effect/compiler/Analysis'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as FileSourceResolver from '@silk-effect/compiler-cli/FileSourceResolver'
import * as Project from '@silk-effect/compiler-cli/Project'
import * as Effect from 'effect/Effect'
import type * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import * as Result from 'effect/Result'
import * as Document from './Document.js'

/** Locates one document's project source root, falling back to its own directory. */
export const sourceRootOf = Effect.fn('Workspace.sourceRootOf')(function* (
  documentPath: string,
): Effect.fn.Return<string, never, FileSystem.FileSystem | Path.Path> {
  const path = yield* Path.Path
  const directory = path.dirname(documentPath)
  const project = yield* Effect.result(Project.load({ workingDirectory: directory }))
  return Result.isSuccess(project) ? project.success.entry.sourceRoot : directory
})

/** Derives the canonical module identity of one document path under a source root. */
export const moduleOf = Effect.fn('Workspace.moduleOf')(function* (
  sourceRoot: string,
  documentPath: string,
): Effect.fn.Return<string, never, Path.Path> {
  const path = yield* Path.Path
  const withoutExtension = (value: string): string =>
    value.endsWith('.silk') ? value.slice(0, -'.silk'.length) : value
  const relative = withoutExtension(path.relative(sourceRoot, documentPath))
  const candidate = relative.split(path.sep).join('/')
  if (SourceResolver.isCanonicalModule(candidate)) return candidate
  const basename = withoutExtension(path.basename(documentPath))
  return SourceResolver.isCanonicalModule(basename) ? basename : 'untitled'
})

/** Opens one synchronized document with its discovered project identity. */
export const open = Effect.fn('Workspace.open')(function* (options: {
  readonly uri: string
  readonly bytes: Uint8Array
}): Effect.fn.Return<Document.Document, never, FileSystem.FileSystem | Path.Path> {
  const path = yield* Path.Path
  const documentPath = yield* path
    .fromFileUrl(new URL(options.uri))
    .pipe(Effect.orElseSucceed(() => options.uri))
  const sourceRoot = yield* sourceRootOf(documentPath)
  const module = yield* moduleOf(sourceRoot, documentPath)
  return Document.make({ uri: options.uri, module, sourceRoot, bytes: options.bytes })
})

/** A resolver serving open-document overlays first, then rooted `.silk` files. */
export const resolver = (
  sourceRoot: string,
  overlays: ReadonlyMap<string, Uint8Array>,
): Layer.Layer<SourceResolver.SourceResolver, never, FileSystem.FileSystem | Path.Path> =>
  Layer.effect(
    SourceResolver.SourceResolver,
    Effect.gen(function* () {
      const files = yield* SourceResolver.SourceResolver
      return {
        resolve: Effect.fnUntraced(function* (module: string) {
          const bytes = overlays.get(module)
          if (bytes !== undefined) return Option.some(Uint8Array.from(bytes))
          return yield* files.resolve(module)
        }),
      }
    }),
  ).pipe(Layer.provide(FileSourceResolver.layer(FileSourceResolver.make(sourceRoot))))

/** Analyzes one document as its compilation root, seeing sibling open documents. */
export const analyze = Effect.fn('Workspace.analyze')(function* (
  document: Document.Document,
  openDocuments: Iterable<Document.Document>,
): Effect.fn.Return<Analysis.Snapshot, never, FileSystem.FileSystem | Path.Path> {
  const overlays = new Map<string, Uint8Array>()
  for (const open of openDocuments) {
    if (open.sourceRoot === document.sourceRoot && open.uri !== document.uri) {
      overlays.set(open.module, open.bytes)
    }
  }
  return yield* Analysis.make({
    root: SourceFile.make(document.module, document.bytes),
  }).pipe(Effect.provide(resolver(document.sourceRoot, overlays)))
})

/** Maps one module identity back to a document URI for cross-file references. */
export const uriOf = Effect.fn('Workspace.uriOf')(function* (
  sourceRoot: string,
  module: string,
  openDocuments: Iterable<Document.Document>,
): Effect.fn.Return<string | undefined, never, Path.Path> {
  for (const open of openDocuments) {
    if (open.sourceRoot === sourceRoot && open.module === module) return open.uri
  }
  const path = yield* Path.Path
  const url = yield* path.toFileUrl(path.join(sourceRoot, `${module}.silk`)).pipe(Effect.option)
  return Option.isSome(url) ? url.value.href : undefined
})
