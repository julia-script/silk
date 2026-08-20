import * as SourceOrigin from '@silk-effect/compiler/SourceOrigin'
import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as Stdlib from '@silk-effect/compiler/Stdlib'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'

/** Rooted physical storage policy for canonical Silk modules. */
export interface FileSourceResolver {
  readonly _tag: 'FileSourceResolver'
  readonly root: string
  readonly toolchainRoot?: string
}

/** Creates a resolver configuration from an already normalized absolute source root. */
export const make = (root: string, toolchainRoot?: string): FileSourceResolver =>
  Object.freeze(
    toolchainRoot === undefined
      ? { _tag: 'FileSourceResolver', root }
      : { _tag: 'FileSourceResolver', root, toolchainRoot },
  )

/** Maps one canonical module exactly to `<source-root>/<module>.silk`. */
export const sourcePath = (self: FileSourceResolver, module: string, path: Path.Path): string =>
  path.join(self.root, `${module}.silk`)

/** Provides the compiler resolver capability over Effect filesystem and path services. */
export const layer = (
  self: FileSourceResolver,
): Layer.Layer<SourceResolver.SourceResolver, never, FileSystem.FileSystem | Path.Path> =>
  Layer.effect(
    SourceResolver.SourceResolver,
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const path = yield* Path.Path
      const readToolchainSource = Effect.fnUntraced(function* (module: string) {
        const entry = Stdlib.find(module)
        if (entry === undefined) return Option.none<SourceResolver.ResolvedSource>()
        const sourceUrl =
          self.toolchainRoot === undefined
            ? entry.sourceUrl
            : new URL(entry.path, self.toolchainRoot)
        const file = yield* path.fromFileUrl(sourceUrl).pipe(
          Effect.mapError(
            (cause) =>
              new SourceResolver.SourceResolverError({
                operation: 'FileSourceResolver.resolveStandardLibrary',
                module,
                message: `Invalid toolchain source URL for ${module}: ${sourceUrl.href}`,
                reason: { _tag: 'WrappedFailure', cause },
              }),
          ),
        )
        return yield* Effect.matchEffect(fileSystem.readFile(file), {
          onFailure: (cause) =>
            cause.reason._tag === 'NotFound'
              ? Effect.fail(
                  new SourceResolver.SourceResolverError({
                    operation: 'FileSourceResolver.resolveStandardLibrary',
                    module,
                    message: `Missing toolchain source module ${module} at ${file}`,
                    reason: { _tag: 'MissingToolchainSource', path: file },
                  }),
                )
              : Effect.fail(
                  new SourceResolver.SourceResolverError({
                    operation: 'FileSourceResolver.resolveStandardLibrary',
                    module,
                    message: `Cannot read toolchain source module ${module} at ${file}`,
                    reason: { _tag: 'WrappedFailure', cause },
                  }),
                ),
          onSuccess: (bytes) =>
            Effect.succeed(
              Option.some(
                SourceResolver.resolved(
                  Uint8Array.from(bytes),
                  SourceOrigin.toolchainFile(sourceUrl.href),
                ),
              ),
            ),
        })
      })
      return {
        resolve: Effect.fn('FileSourceResolver.resolve')((module: string) => {
          if (!SourceResolver.isCanonicalModule(module)) {
            return Effect.fail(
              new SourceResolver.SourceResolverError({
                operation: 'FileSourceResolver.resolve',
                module,
                message: `Source module identity ${module} is not canonical`,
                reason: { _tag: 'InvalidModuleIdentity' },
              }),
            )
          }
          const file = sourcePath(self, module, path)
          return Effect.matchEffect(fileSystem.readFile(file), {
            onFailure: (cause) =>
              cause.reason._tag === 'NotFound'
                ? Effect.succeed(Option.none())
                : Effect.fail(
                    new SourceResolver.SourceResolverError({
                      operation: 'FileSourceResolver.resolve',
                      module,
                      message: `Cannot read source module ${module} at ${file}`,
                      reason: { _tag: 'WrappedFailure', cause },
                    }),
                  ),
            onSuccess: (bytes) =>
              Effect.succeed(
                Option.some(
                  SourceResolver.resolved(Uint8Array.from(bytes), SourceOrigin.projectFile(file)),
                ),
              ),
          })
        }),
        resolveStandardLibrary: Effect.fn('FileSourceResolver.resolveStandardLibrary')(
          readToolchainSource,
        ),
        toolchainSources: Effect.fn('FileSourceResolver.toolchainSources')(function* () {
          const loaded = new Map<string, SourceResolver.ResolvedSource>()
          for (const entry of Stdlib.manifest) {
            const source = yield* readToolchainSource(entry.module)
            if (Option.isSome(source)) loaded.set(entry.module, source.value)
          }
          return loaded
        }),
      }
    }),
  )
