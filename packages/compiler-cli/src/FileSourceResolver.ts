import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'

/** Rooted physical storage policy for canonical Silk modules. */
export interface FileSourceResolver {
  readonly _tag: 'FileSourceResolver'
  readonly root: string
}

/** Creates a resolver configuration from an already normalized absolute source root. */
export const make = (root: string): FileSourceResolver =>
  Object.freeze({ _tag: 'FileSourceResolver', root })

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
            onSuccess: (bytes) => Effect.succeed(Option.some(Uint8Array.from(bytes))),
          })
        }),
      }
    }),
  )
