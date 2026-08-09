import * as Analysis from '@silk-effect/compiler/Analysis'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SourceOrigin from '@silk-effect/compiler/SourceOrigin'
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

export interface Identity {
  readonly workspace: string
  readonly sourceRoot: string
}

const virtualModule = (uri: string): string => {
  let hash = 2166136261
  for (const character of uri) {
    hash ^= character.codePointAt(0) ?? 0
    hash = Math.imul(hash, 16777619)
  }
  return `virtual/${(hash >>> 0).toString(16)}`
}

/** Locates one document's project source root, falling back to its own directory. */
export const sourceRootOf = Effect.fn('Workspace.sourceRootOf')(function* (
  documentPath: string,
): Effect.fn.Return<string, never, FileSystem.FileSystem | Path.Path> {
  const path = yield* Path.Path
  const directory = path.dirname(documentPath)
  const project = yield* Effect.result(Project.load({ workingDirectory: directory }))
  return Result.isSuccess(project) ? project.success.entry.sourceRoot : directory
})

/** Discovers the stable project or standalone workspace identity for one filesystem path. */
export const identityOf = Effect.fn('Workspace.identityOf')(function* (
  documentPath: string,
): Effect.fn.Return<Identity, never, FileSystem.FileSystem | Path.Path> {
  const path = yield* Path.Path
  const directory = path.dirname(documentPath)
  const project = yield* Effect.result(Project.load({ workingDirectory: directory }))
  return Result.isSuccess(project)
    ? Object.freeze({
        workspace: `project:${project.success.manifestPath}`,
        sourceRoot: project.success.entry.sourceRoot,
      })
    : Object.freeze({ workspace: `standalone:${directory}`, sourceRoot: directory })
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
  readonly version: number
  readonly bytes: Uint8Array
}): Effect.fn.Return<Document.Document, never, FileSystem.FileSystem | Path.Path> {
  const path = yield* Path.Path
  const parsedUrl = yield* Effect.try({
    try: () => new URL(options.uri),
    catch: (cause) => cause,
  }).pipe(Effect.option)
  const documentPath =
    Option.isNone(parsedUrl) || parsedUrl.value.protocol !== 'file:'
      ? Option.none<string>()
      : yield* path.fromFileUrl(parsedUrl.value).pipe(Effect.option)
  if (Option.isNone(documentPath)) {
    return Document.make({
      uri: options.uri,
      version: options.version,
      workspace: `virtual:${options.uri}`,
      module: virtualModule(options.uri),
      sourceRoot: options.uri,
      bytes: options.bytes,
    })
  }
  const identity = yield* identityOf(documentPath.value)
  const module = yield* moduleOf(identity.sourceRoot, documentPath.value)
  return Document.make({
    uri: options.uri,
    version: options.version,
    workspace: identity.workspace,
    module,
    sourceRoot: identity.sourceRoot,
    bytes: options.bytes,
  })
})

/** A resolver serving open-document overlays first, then rooted `.silk` files. */
export const resolver = (
  sourceRoot: string,
  overlays: ReadonlyMap<string, SourceResolver.ResolvedSource>,
): Layer.Layer<SourceResolver.SourceResolver, never, FileSystem.FileSystem | Path.Path> =>
  Layer.effect(
    SourceResolver.SourceResolver,
    Effect.gen(function* () {
      const files = yield* SourceResolver.SourceResolver
      return {
        resolve: Effect.fnUntraced(function* (module: string) {
          const source = overlays.get(module)
          if (source !== undefined) return Option.some(source)
          return yield* files.resolve(module)
        }),
        resolveStandardLibrary: files.resolveStandardLibrary,
      }
    }),
  ).pipe(Layer.provide(FileSourceResolver.layer(FileSourceResolver.make(sourceRoot))))

/** Analyzes one document as its compilation root, seeing sibling open documents. */
export const analyze = Effect.fn('Workspace.analyze')(function* (
  document: Document.Document,
  openDocuments: Iterable<Document.Document>,
): Effect.fn.Return<Analysis.FrontendSnapshot, never, FileSystem.FileSystem | Path.Path> {
  const overlays = new Map<string, SourceResolver.ResolvedSource>()
  for (const open of openDocuments) {
    if (open.workspace === document.workspace && open.uri !== document.uri) {
      overlays.set(open.module, SourceResolver.resolved(open.bytes, SourceOrigin.memory(open.uri)))
    }
  }
  return yield* Analysis.make({
    root: SourceFile.make(document.module, document.bytes, SourceOrigin.memory(document.uri)),
  }).pipe(Effect.provide(resolver(document.sourceRoot, overlays)))
})

/** Maps one module identity back to a document URI for cross-file references. */
export const uriOf = Effect.fn('Workspace.uriOf')(function* (
  source: SourceFile.SourceFile,
  openDocuments: Iterable<Document.Document>,
): Effect.fn.Return<string | undefined, never, Path.Path> {
  for (const open of openDocuments) {
    if (open.module === source.id) return open.uri
  }
  switch (source.origin._tag) {
    case 'Memory':
    case 'ToolchainFile':
      return source.origin.uri
    case 'ProjectFile': {
      const path = yield* Path.Path
      const url = yield* path.toFileUrl(source.origin.path).pipe(Effect.option)
      return Option.isSome(url) ? url.value.href : undefined
    }
  }
})
