import * as Context from 'effect/Context'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'

/** Why a source resolver could not determine the bytes for one canonical module identity. */
export type SourceResolverErrorReason =
  | { readonly _tag: 'InvalidModuleIdentity' }
  | { readonly _tag: 'WrappedFailure'; readonly cause: unknown }

/** A typed operational failure at the source-storage boundary. */
export class SourceResolverError extends Data.TaggedError('SourceResolverError')<{
  readonly operation: string
  readonly module: string
  readonly message: string
  readonly reason: SourceResolverErrorReason
  readonly cause?: unknown
}> {
  constructor(options: {
    readonly operation: string
    readonly module: string
    readonly message: string
    readonly reason: SourceResolverErrorReason
  }) {
    super(
      options.reason._tag === 'WrappedFailure'
        ? { ...options, cause: options.reason.cause }
        : options,
    )
  }
}

/** Tests whether a string is a canonical, extensionless Silk module identity. */
export const isCanonicalModule = (module: string): boolean =>
  module.length > 0 &&
  !module.startsWith('/') &&
  !module.endsWith('/') &&
  !module.includes('\\') &&
  module
    .split('/')
    .every(
      (segment) =>
        segment.length > 0 &&
        segment !== '.' &&
        segment !== '..' &&
        /^[A-Za-z0-9_-]+$/.test(segment),
    )

/** The replaceable capability used to resolve imported source bytes. */
export class SourceResolver extends Context.Service<
  SourceResolver,
  {
    readonly resolve: (
      module: string,
    ) => Effect.Effect<Option.Option<Uint8Array>, SourceResolverError>
  }
>()('@silk-effect/compiler/SourceResolver') {}

/** Resolves one validated canonical module through the active source resolver. */
export const resolve = Effect.fn('SourceResolver.resolve')(function* (
  module: string,
): Effect.fn.Return<Option.Option<Uint8Array>, SourceResolverError, SourceResolver> {
  if (!isCanonicalModule(module)) {
    return yield* new SourceResolverError({
      operation: 'SourceResolver.resolve',
      module,
      message: `Source module identity ${module} is not canonical`,
      reason: { _tag: 'InvalidModuleIdentity' },
    })
  }
  const resolver = yield* SourceResolver
  return yield* resolver.resolve(module)
})

/** An immutable in-memory resolver layer suitable for browsers, tooling, and tests. */
export const memory = (sources: ReadonlyMap<string, Uint8Array>): Layer.Layer<SourceResolver> => {
  const snapshot = new Map(
    [...sources].map(([module, bytes]) => [module, Uint8Array.from(bytes)] as const),
  )
  return Layer.succeed(SourceResolver, {
    resolve: Effect.fn('SourceResolver.memory.resolve')((module: string) => {
      const bytes = snapshot.get(module)
      return Effect.succeed(
        bytes === undefined ? Option.none() : Option.some(Uint8Array.from(bytes)),
      )
    }),
  })
}

/** A resolver layer that authoritatively reports every imported module as absent. */
export const empty: Layer.Layer<SourceResolver> = memory(new Map())
