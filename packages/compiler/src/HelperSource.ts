import * as Effect from 'effect/Effect'
import * as Analysis from './Analysis.js'
import type * as Backend from './Backend.js'
import * as CompilationProfile from './CompilationProfile.js'
import * as HelperCapability from './HelperCapability.js'
import * as SourceFile from './SourceFile.js'
import * as SourceResolver from './SourceResolver.js'

/** Compiles one explicitly rooted source provider without an application or runtime root. */
export const compile = Effect.fn('HelperSource.compile')(function* (
  provider: HelperCapability.Provider,
  profile: CompilationProfile.Facts,
): Effect.fn.Return<
  {
    readonly artifact: Backend.LlvmBitcodeArtifact
    readonly profile: CompilationProfile.CompilationProfile
  },
  HelperCapability.HelperError
> {
  const invalid = (subject: string) =>
    new HelperCapability.HelperError({
      operation: 'HelperSource.compile',
      code: 'InvalidSupportProfile',
      subject,
      origins: [provider.id],
    })
  if (provider.kind !== 'source' || !provider.targets.includes(profile.target.id))
    return yield* invalid('Provider is not a compatible source provider')
  const input: CompilationProfile.Input = {
    target: profile.target.id,
    cpu: profile.cpu,
    ...(profile.deployment === undefined ? {} : { deployment: profile.deployment }),
    artifact: 'object',
    entry: { kind: 'none' },
    runtime: { kind: 'none' },
    libc: 'none',
    relocation: profile.relocation,
    codeModel: profile.codeModel,
    optimization: profile.optimization,
    debug: profile.debug,
    unwind: 'none',
    sanitizers: [],
  }
  const snapshot = yield* Analysis.makeRealized({
    root: SourceFile.make(
      'compiler-support/root',
      new TextEncoder().encode(`import ${provider.root}\n`),
    ),
    configuration: { profile: input },
  }).pipe(Effect.provide(SourceResolver.empty))
  const diagnostics = Analysis.diagnostics(snapshot)
  if (diagnostics.length !== 0 || snapshot.profile === undefined)
    return yield* invalid(diagnostics.map((entry) => `${entry.code}: ${entry.message}`).join('\n'))
  const artifact = yield* Analysis.codegen(snapshot, {
    mode: profile.optimization === 'none' ? 'debug' : 'release',
    support: true,
  }).pipe(Effect.mapError((failure) => invalid(failure.message)))
  yield* HelperCapability.verifyExports(provider, artifact.foreignExports, profile.target)
  return Object.freeze({ artifact, profile: snapshot.profile })
})
