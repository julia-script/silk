import * as Effect from 'effect/Effect'
import * as CompilationProfile from './CompilationProfile.js'
import * as ConfigurationError from './ConfigurationError.js'
import * as ConfigurationOrigin from './ConfigurationOrigin.js'
import * as NativeRequirement from './NativeRequirement.js'
import * as SourceResolver from './SourceResolver.js'
import * as Canonical from './internal/Canonical.js'

/** One explicit, unambiguous runtime declaration retained without changing source visibility. */
export interface RootSelector {
  readonly module: string
  readonly declaration: string
}

/** A named source composition; invocation is independent of any loader entry symbol. */
export interface Runtime {
  readonly name: string
  readonly module: string
  readonly invoke?: string
}

/** Build/package inputs from which one profile selects its source composition. */
export interface Input {
  readonly runtimes: ReadonlyArray<Runtime>
  readonly defaults: ReadonlyArray<string>
  readonly retention: ReadonlyArray<RootSelector>
  readonly requirements: ReadonlyArray<NativeRequirement.Input>
  readonly entry?: CompilationProfile.Selection
}

export interface Root extends RootSelector {
  readonly origin: ConfigurationOrigin.ConfigurationOrigin
}

/** Validated build catalog with current diagnostic origins and normalized native constraints. */
export interface ArtifactComposition {
  readonly runtimes: ReadonlyArray<
    Runtime & { readonly origin: ConfigurationOrigin.ConfigurationOrigin }
  >
  readonly defaults: ReadonlyArray<string>
  readonly retention: ReadonlyArray<Root>
  readonly requirements: ReadonlyArray<NativeRequirement.NativeRequirement>
  readonly entry: CompilationProfile.Selection
  readonly origin: ConfigurationOrigin.ConfigurationOrigin
}

/** Exactly one selected source runtime or none, plus independent invocation/retention roots. */
export interface Resolved {
  readonly application: string
  readonly request: CompilationProfile.Selection
  readonly runtime:
    | (Runtime & { readonly origin: ConfigurationOrigin.ConfigurationOrigin })
    | undefined
  readonly invocation: Root | undefined
  readonly retention: ReadonlyArray<Root>
  readonly requirements: ReadonlyArray<NativeRequirement.NativeRequirement>
  readonly loader: {
    readonly request: CompilationProfile.Selection
    readonly composition: CompilationProfile.Selection
    readonly resolved: CompilationProfile.Selection
  }
  readonly modules: ReadonlyArray<string>
  readonly identity: string
}

const record = (input: unknown): input is Record<string, unknown> =>
  typeof input === 'object' && input !== null && !Array.isArray(input)
const name = (input: unknown): input is string =>
  typeof input === 'string' && /^[A-Za-z_][A-Za-z0-9_]*$/.test(input)
const exact = (input: Record<string, unknown>, fields: ReadonlyArray<string>): boolean =>
  Object.keys(input).every((key) => fields.includes(key))

/** The existing hosted application policy, explicitly supplied by build/analysis entry points. */
export const defaults = (
  application: string,
  profile: Pick<CompilationProfile.Facts, 'artifact' | 'target'>,
): Input =>
  Object.freeze({
    runtimes:
      profile.artifact === 'executable' || profile.target.kind === 'WebAssembly'
        ? Object.freeze([{ name: 'application', module: application, invoke: 'main' }])
        : Object.freeze([]),
    defaults:
      profile.artifact === 'executable' || profile.target.kind === 'WebAssembly'
        ? Object.freeze(['application'])
        : Object.freeze([]),
    retention: Object.freeze([]),
    requirements: Object.freeze([]),
  })

/** Strictly decodes the build catalog; absent fields are empty sets, not hidden runtime defaults. */
export const decode = Effect.fn('ArtifactComposition.decode')(function* (
  input: unknown,
  origin: ConfigurationOrigin.ConfigurationOrigin = ConfigurationOrigin.literal(
    'artifact composition',
  ),
): Effect.fn.Return<ArtifactComposition, ConfigurationError.ConfigurationError> {
  const invalid = (subject: string, origins = [origin]) =>
    ConfigurationError.make('ArtifactComposition.decode', 'InvalidInput', subject, origins)
  if (!ConfigurationOrigin.isPublic(origin))
    return yield* ConfigurationError.make(
      'ArtifactComposition.decode',
      'ForbiddenProvenance',
      'artifact composition',
      [origin],
    )
  if (
    !record(input) ||
    !exact(input, ['runtimes', 'defaults', 'retention', 'requirements', 'entry'])
  )
    return yield* invalid('artifact composition fields')
  const runtimeInputs = input.runtimes ?? []
  const defaultInputs = input.defaults ?? []
  const retentionInputs = input.retention ?? []
  const requirementInputs = input.requirements ?? []
  if (
    !Array.isArray(runtimeInputs) ||
    !Array.isArray(defaultInputs) ||
    !Array.isArray(retentionInputs) ||
    !Array.isArray(requirementInputs)
  )
    return yield* invalid('artifact composition lists')
  const runtimes: Array<Runtime & { readonly origin: ConfigurationOrigin.ConfigurationOrigin }> = []
  for (const [ordinal, candidate] of runtimeInputs.entries()) {
    const at = ConfigurationOrigin.snapshot({
      ...origin,
      source: `${origin.source}.runtimes[${ordinal}]`,
    })
    if (
      !record(candidate) ||
      !exact(candidate, ['name', 'module', 'invoke']) ||
      typeof candidate.name !== 'string' ||
      !NativeRequirement.isIdentity(candidate.name) ||
      typeof candidate.module !== 'string' ||
      !SourceResolver.isCanonicalModule(candidate.module) ||
      (candidate.invoke !== undefined && !name(candidate.invoke))
    )
      return yield* invalid('runtime descriptor', [at])
    runtimes.push(
      Object.freeze({
        name: candidate.name,
        module: candidate.module,
        ...(candidate.invoke === undefined ? {} : { invoke: candidate.invoke }),
        origin: at,
      }),
    )
  }
  const duplicateNames = new Set(
    runtimes
      .filter(
        (runtime, ordinal) =>
          runtimes.findIndex((candidate) => candidate.name === runtime.name) !== ordinal,
      )
      .map((runtime) => runtime.name),
  )
  if (duplicateNames.size > 0)
    return yield* ConfigurationError.make(
      'ArtifactComposition.decode',
      'ConflictingBindings',
      'runtime descriptors',
      runtimes
        .filter((runtime) => duplicateNames.has(runtime.name))
        .map((runtime) => runtime.origin),
    )
  const defaults: Array<string> = []
  for (const value of defaultInputs) {
    if (typeof value !== 'string' || !NativeRequirement.isIdentity(value))
      return yield* invalid('default runtime identity')
    defaults.push(value)
  }
  const retention: Array<Root> = []
  for (const [ordinal, candidate] of retentionInputs.entries()) {
    const at = ConfigurationOrigin.snapshot({
      ...origin,
      source: `${origin.source}.retention[${ordinal}]`,
    })
    if (
      !record(candidate) ||
      !exact(candidate, ['module', 'declaration']) ||
      typeof candidate.module !== 'string' ||
      !SourceResolver.isCanonicalModule(candidate.module) ||
      !name(candidate.declaration)
    )
      return yield* invalid('retention selector', [at])
    retention.push(
      Object.freeze({ module: candidate.module, declaration: candidate.declaration, origin: at }),
    )
  }
  const requirements: Array<NativeRequirement.NativeRequirement> = []
  for (const [ordinal, candidate] of requirementInputs.entries())
    requirements.push(
      yield* NativeRequirement.decode(
        candidate,
        { kind: 'artifact' },
        {
          ...origin,
          source: `${origin.source}.requirements[${ordinal}]`,
        },
      ),
    )
  let entry: CompilationProfile.Selection = Object.freeze({ kind: 'default' })
  if (input.entry !== undefined) {
    if (!record(input.entry)) return yield* invalid('composition loader entry')
    if (
      (input.entry.kind === 'default' || input.entry.kind === 'none') &&
      exact(input.entry, ['kind'])
    )
      entry = Object.freeze({ kind: input.entry.kind })
    else if (
      input.entry.kind === 'named' &&
      exact(input.entry, ['kind', 'name']) &&
      typeof input.entry.name === 'string' &&
      NativeRequirement.isIdentity(input.entry.name)
    )
      entry = Object.freeze({ kind: 'named', name: input.entry.name })
    else return yield* invalid('composition loader entry')
  }
  return Object.freeze({
    runtimes: Object.freeze(runtimes.sort((a, b) => Canonical.compare(a.name, b.name))),
    defaults: Object.freeze([...new Set(defaults)].sort(Canonical.compare)),
    retention: Object.freeze(retention.sort((a, b) => Canonical.compare(rootKey(a), rootKey(b)))),
    requirements: Object.freeze(requirements),
    entry,
    origin: ConfigurationOrigin.snapshot(origin),
  })
})

/** Canonical declaration selector encoding, excluding current configuration location. */
export const rootKey = (self: RootSelector): string =>
  Canonical.record('root', [self.module, self.declaration])

/** Selects one profile's roots from a validated catalog without resolving source or physical files. */
export const resolve = Effect.fn('ArtifactComposition.resolve')(function* (
  self: ArtifactComposition,
  application: string,
  profile: CompilationProfile.Facts,
): Effect.fn.Return<Resolved, ConfigurationError.ConfigurationError> {
  if (!SourceResolver.isCanonicalModule(application))
    return yield* ConfigurationError.make(
      'ArtifactComposition.resolve',
      'InvalidInput',
      'application root',
      [self.origin],
    )
  const request = profile.runtime
  let candidates: ReadonlyArray<string> = []
  if (request.kind === 'default') candidates = self.defaults
  else if (request.kind === 'named') candidates = [request.name]
  const selected = self.runtimes.filter((runtime) => candidates.includes(runtime.name))
  if (selected.length !== candidates.length)
    return yield* ConfigurationError.make(
      'ArtifactComposition.resolve',
      'MissingParameter',
      'runtime composition',
      [self.origin],
      candidates.filter((candidate) => !selected.some((runtime) => runtime.name === candidate)),
    )
  if (selected.length > 1)
    return yield* ConfigurationError.make(
      'ArtifactComposition.resolve',
      'ConflictingBindings',
      'runtime composition',
      selected.map((runtime) => runtime.origin),
    )
  const runtime = selected[0]
  const invocation =
    runtime?.invoke === undefined
      ? undefined
      : Object.freeze({
          module: runtime.module,
          declaration: runtime.invoke,
          origin: runtime.origin,
        })
  const retained = new Map(self.retention.map((root) => [rootKey(root), root]))
  const retention = Object.freeze([...retained.values()])
  if (
    profile.entry.kind !== 'default' &&
    self.entry.kind !== 'default' &&
    CompilationProfile.encodeSelection(profile.entry) !==
      CompilationProfile.encodeSelection(self.entry)
  )
    return yield* ConfigurationError.make(
      'ArtifactComposition.resolve',
      'ConflictingBindings',
      'loader entry',
      [ConfigurationOrigin.literal('profile.entry'), self.origin],
    )
  const loader = Object.freeze({
    request: profile.entry,
    composition: self.entry,
    resolved: profile.entry.kind === 'default' ? self.entry : profile.entry,
  })
  const modules = Object.freeze(
    [
      ...new Set([
        application,
        ...(runtime === undefined ? [] : [runtime.module]),
        ...retention.map((root) => root.module),
      ]),
    ].sort(Canonical.compare),
  )
  const identity = Canonical.record('ArtifactComposition.v1', [
    application,
    CompilationProfile.encodeSelection(request),
    Canonical.array(request.kind === 'default' ? self.defaults : []),
    Canonical.record(
      'runtime',
      runtime === undefined ? [] : [runtime.name, runtime.module, runtime.invoke ?? ''],
    ),
    Canonical.array(retention.map(rootKey)),
    Canonical.array(self.requirements.map(NativeRequirement.encode).sort(Canonical.compare)),
    CompilationProfile.encodeSelection(loader.request),
    CompilationProfile.encodeSelection(loader.composition),
    CompilationProfile.encodeSelection(loader.resolved),
  ])
  return Object.freeze({
    application,
    request,
    runtime,
    invocation,
    retention,
    requirements: self.requirements,
    loader,
    modules,
    identity,
  })
})

/** Projects a validated catalog into portable build fields without diagnostic metadata. */
export const input = (self: ArtifactComposition): Input =>
  Object.freeze({
    runtimes: Object.freeze(
      self.runtimes.map(({ origin: _origin, ...runtime }) => Object.freeze(runtime)),
    ),
    defaults: self.defaults,
    retention: Object.freeze(
      self.retention.map(({ origin: _origin, ...root }) => Object.freeze(root)),
    ),
    requirements: Object.freeze(
      self.requirements.map(({ origin: _origin, scope: _scope, ...requirement }) =>
        Object.freeze(requirement),
      ),
    ),
    entry: self.entry,
  })
