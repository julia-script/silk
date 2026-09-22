import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as AuthoredEncoding from './AuthoredEncoding.js'
import type * as AuthoredIdentity from './AuthoredIdentity.js'
import * as AuthoredLowering from './AuthoredLowering.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Elaboration from './Elaboration.js'
import * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'
import type * as NativeLinkPlan from './NativeLinkPlan.js'
import * as SuspensionMode from './SuspensionMode.js'
import type * as TestDiscovery from './TestDiscovery.js'
import * as ToolchainIntegrity from './ToolchainIntegrity.js'
import * as Type from './Type.js'
import * as Canonical from './internal/Canonical.js'

/** The explicit reason one discovered test cannot safely reuse a prior result. */
export type IneligibilityReason =
  | { readonly _tag: 'MissingTestRoot' }
  | { readonly _tag: 'AmbiguousTestRoot' }
  | {
      readonly _tag: 'IncompleteExecutionClosure'
      readonly gaps: ReadonlyArray<Instances.ExecutionGap>
    }
  | {
      readonly _tag: 'MissingAuthoredDependency'
      readonly declaration: DeclarationFacts.CanonicalId
    }
  | { readonly _tag: 'IncompleteEnvironment'; readonly component: 'Runner' }

export type Eligibility =
  | { readonly _tag: 'Eligible'; readonly identity: string }
  | { readonly _tag: 'Ineligible'; readonly reason: IneligibilityReason }

/** One discovered test paired with its complete execution-reuse decision. */
export interface Entry {
  readonly test: TestDiscovery.Info
  readonly eligibility: Eligibility
}

/** Canonical-order execution identities for one completed test-purpose compilation. */
export interface Manifest {
  readonly _tag: 'TestExecutionManifest'
  readonly catalogIdentity: string
  readonly environmentIdentity: string
  readonly entries: ReadonlyArray<Entry>
}

/** Execution-wide inputs shared by every test in one native test artifact. */
export interface Environment {
  readonly profileIdentity: string
  readonly bootstrapIdentity: string
  readonly runnerIdentity: string
  readonly compilerIdentity: string
  readonly runtimeIdentity: string
  readonly nativeIdentity: string
  readonly complete: boolean
}

export interface Input {
  readonly catalog: TestDiscovery.Catalog
  readonly discovery: Instances.Discovery
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly environment: Environment
}

const digest = (value: string): string => ToolchainIntegrity.contentDigest(value)

const declarationIdentity = (declaration: DeclarationFacts.CanonicalId): string =>
  Canonical.record('Declaration', [declaration.module, declaration.name])

const authoredDigest = Effect.fnUntraced(function* (
  results: ReadonlyMap<string, Elaboration.Result>,
  module: string,
  owner: AuthoredIdentity.Identity,
): Effect.fn.Return<string | undefined> {
  const result = results.get(module)
  if (result === undefined) return undefined
  const authored = AuthoredLowering.declarationOf(result.authored, owner)
  if (authored === undefined) return undefined
  const encoded = yield* Effect.result(
    Effect.gen(function* () {
      const header = yield* AuthoredEncoding.header(result.authored.module.pool, authored)
      const body = yield* AuthoredEncoding.body(result.authored.module.pool, authored)
      return Canonical.record('AuthoredDeclaration.v1', [
        ToolchainIntegrity.contentDigest(Uint8Array.from(header)),
        ToolchainIntegrity.contentDigest(Uint8Array.from(body)),
      ])
    }),
  )
  return Result.isSuccess(encoded) ? encoded.success : undefined
})

const rootOf = (
  discovery: Instances.Discovery,
  declaration: DeclarationFacts.CanonicalId,
): ReadonlyArray<Instances.Instance> =>
  discovery.instances.filter(
    (instance) =>
      instance.key.declaration.module === declaration.module &&
      instance.key.declaration.name === declaration.name &&
      instance.key.typeArguments.length === 0 &&
      instance.key.evidence.length === 0 &&
      instance.key.staticArguments.length === 0,
  )

const providerEncoding = (provider: Instances.CallProvider): string =>
  Canonical.record('Provider', [
    provider.role,
    Type.key(provider.capability),
    Type.key(provider.providerType),
  ])

const instanceEncoding = (instance: Instances.Instance, authored: string): string =>
  Canonical.record('Instance', [
    Instances.keyText(instance.key),
    authored,
    instance.resultCallable === undefined
      ? ''
      : Type.runtimeGenericArgumentKey(instance.resultCallable),
    instance.resultEffect ?? '',
  ])

const edgeEncoding = (edge: Instances.ExecutionEdge): string =>
  Canonical.record('Edge', [
    edge.kind,
    Instances.keyText(edge.owner),
    Instances.keyText(edge.target),
    Canonical.array((edge.providers ?? []).map(providerEncoding)),
  ])

const callableEncoding = (callable: Instances.CallableInstance): string =>
  Canonical.record('Callable', [
    Instances.callableIdentity(callable),
    callable.target._tag,
    Canonical.array(callable.typeArguments.map(Type.runtimeGenericArgumentKey)),
    Canonical.array(callable.captureTypes.map(Type.key)),
    Canonical.array(
      callable.captures.map((capture) =>
        Canonical.record('Capture', [
          String(capture.ordinal),
          String(capture.parameterOrdinal),
          capture.access,
          Type.key(capture.type),
          capture.callableIdentity === undefined
            ? ''
            : Type.runtimeGenericArgumentKey(capture.callableIdentity),
        ]),
      ),
    ),
    Type.key(callable.type),
    callable.mode,
  ])

const effectEncoding = (effect: Instances.EffectInstance): string =>
  Canonical.record('Effect', [
    effect.identity,
    effect.representationIdentity,
    Instances.keyText(effect.owner),
    declarationIdentity(effect.runner),
    Canonical.array(effect.typeArguments.map(Type.runtimeGenericArgumentKey)),
    Canonical.array(
      effect.captures.map((capture) =>
        Canonical.record('Capture', [
          String(capture.ordinal),
          capture.source,
          String(capture.sourceOrdinal),
          capture.access,
          Type.key(capture.type),
          capture.effectIdentity ?? '',
          capture.callableIdentity === undefined
            ? ''
            : Type.runtimeGenericArgumentKey(capture.callableIdentity),
          capture.providedRequirement === undefined
            ? ''
            : Canonical.record('Requirement', [
                Type.key(capture.providedRequirement.capability),
                capture.providedRequirement.role,
                capture.providedRequirement.requirementAccess,
                capture.providedRequirement.providerAccess,
              ]),
        ]),
      ),
    ),
    Type.key(effect.type),
    SuspensionMode.encode(effect.suspension),
  ])

const residualEncoding = (body: Instances.ExecutionClosure['residualBodies'][number]): string =>
  Canonical.record('Residual', [
    declarationIdentity(body.declaration),
    body.reason,
    Canonical.array(
      body.dependencies.map((dependency) =>
        Canonical.record(dependency.kind, [
          declarationIdentity(dependency.declaration),
          dependency.canonical,
        ]),
      ),
    ),
  ])

/** Stable identity of the compiler-owned runner policy used by synthetic or isolated consumers. */
export const runnerPolicyIdentity = digest('silk-test-runner-policy-v1')

/** Authored identity of the executable roots that invoke the discovered test catalog. */
export const runnerIdentity = (
  discovery: Instances.Discovery,
): { readonly identity: string; readonly complete: boolean } => {
  const instances = new Map(
    discovery.instances.map((instance) => [Instances.keyText(instance.key), instance]),
  )
  const retainedRoots =
    discovery.retention.length > 0
      ? discovery.retention
      : discovery.instances
          .filter(
            (instance) =>
              instance.key.declaration.module === discovery.rootModule &&
              instance.key.declaration.name === 'main',
          )
          .map((instance) => instance.key)
  const roots: string[] = []
  let complete = retainedRoots.length > 0
  for (const key of retainedRoots) {
    const instance = instances.get(Instances.keyText(key))
    if (instance === undefined) {
      complete = false
      roots.push(Canonical.record('MissingRunnerRoot', [Instances.keyText(key)]))
      continue
    }
    roots.push(Canonical.record('RunnerRoot', [Instances.keyText(key)]))
  }
  return {
    identity: digest(
      Canonical.record('TestRunner.v1', [runnerPolicyIdentity, Canonical.array(roots)]),
    ),
    complete,
  }
}

/** Stable execution-wide identity included in every eligible test key. */
export const environmentIdentity = (self: Environment): string =>
  digest(
    Canonical.record('TestExecutionEnvironment.v1', [
      self.profileIdentity,
      self.bootstrapIdentity,
      self.runnerIdentity,
      self.compilerIdentity,
      self.runtimeIdentity,
      self.nativeIdentity,
      self.complete ? 'Complete' : 'Incomplete',
    ]),
  )

/**
 * Physical native inputs that can change execution without folding in generated test objects or
 * the final linked artifact identity.
 */
export const nativeIdentity = (
  plan: NativeLinkPlan.NativeLinkPlan,
  generatedObjects: ReadonlyArray<string>,
  bindingsIdentity: string,
  helperIdentities: ReadonlyArray<string>,
): string => {
  const generated = new Set(generatedObjects)
  return digest(
    Canonical.record('TestNativeEnvironment.v1', [
      plan.supply.target.id,
      plan.supply.libc,
      plan.supply.deployment ?? '',
      plan.supply.version ?? '',
      plan.supply.compiler.digest,
      plan.supply.compiler.version,
      plan.supply.linker.digest,
      plan.supply.linker.version,
      Canonical.array(
        Object.entries(plan.supply.consultedEnvironment)
          .toSorted(([left], [right]) => Canonical.compare(left, right))
          .map(([name, value]) => Canonical.record('Environment', [name, value])),
      ),
      Canonical.array(
        plan.inputs
          .filter((input) => !generated.has(input.path) && !generated.has(input.selectedPath))
          .map((input) => Canonical.record('Input', [input.role, input.digest])),
      ),
      Canonical.array(plan.scripts.map((script) => digest(script.source))),
      Canonical.array(plan.translations.map((translation) => translation.identity)),
      bindingsIdentity,
      Canonical.array(helperIdentities),
    ]),
  )
}

/** Selected runtime-support distribution identity, narrowed by the referenced runtime symbols. */
export const runtimeIdentity = (
  distribution: ToolchainIntegrity.Graph,
  symbols: ReadonlyArray<string>,
): string =>
  digest(
    Canonical.record('TestRuntimeEnvironment.v1', [
      Canonical.array(
        distribution.components
          .filter((component) => component.kind === 'RuntimeSupport')
          .map((component) => Canonical.record(component.id, [component.digest])),
      ),
      Canonical.array(symbols.toSorted(Canonical.compare)),
    ]),
  )

const eligible = Effect.fnUntraced(function* (
  input: Input,
  entry: TestDiscovery.Entry,
  environment: string,
): Effect.fn.Return<Eligibility> {
  if (!input.environment.complete)
    return {
      _tag: 'Ineligible',
      reason: { _tag: 'IncompleteEnvironment', component: 'Runner' },
    }
  const declaration = entry.declaration.canonical
  if (declaration._tag !== 'Canonical')
    return { _tag: 'Ineligible', reason: { _tag: 'MissingTestRoot' } }
  const roots = rootOf(input.discovery, declaration.id)
  if (roots.length === 0) return { _tag: 'Ineligible', reason: { _tag: 'MissingTestRoot' } }
  if (roots.length !== 1) return { _tag: 'Ineligible', reason: { _tag: 'AmbiguousTestRoot' } }
  const root = roots.at(0)
  if (root === undefined) return { _tag: 'Ineligible', reason: { _tag: 'MissingTestRoot' } }
  const closure = Instances.executionClosure(input.discovery, root.key)
  if (closure.gaps.length > 0)
    return {
      _tag: 'Ineligible',
      reason: { _tag: 'IncompleteExecutionClosure', gaps: closure.gaps },
    }
  const authored = new Map<string, string>()
  for (const instance of closure.instances) {
    const value = yield* authoredDigest(
      input.results,
      instance.key.declaration.module,
      instance.function.declaration.owner,
    )
    if (value === undefined)
      return {
        _tag: 'Ineligible',
        reason: {
          _tag: 'MissingAuthoredDependency',
          declaration: instance.key.declaration,
        },
      }
    authored.set(Instances.keyText(instance.key), value)
  }
  const encoded = Canonical.record('TestExecution.v1', [
    entry.info.identity,
    environment,
    Canonical.array(
      closure.instances.map((instance) =>
        instanceEncoding(instance, authored.get(Instances.keyText(instance.key)) ?? ''),
      ),
    ),
    Canonical.array(closure.edges.map(edgeEncoding)),
    Canonical.array(closure.callables.map(callableEncoding)),
    Canonical.array(closure.effects.map(effectEncoding)),
    Canonical.array(
      closure.intrinsics.map((call) =>
        Canonical.record('Intrinsic', [Intrinsic.operationText(call.operation)]),
      ),
    ),
    Canonical.array(
      closure.foreignCalls.map((call) =>
        Canonical.record('Foreign', [
          call.symbol,
          declarationIdentity(call.declaration),
          JSON.stringify(call.signature),
        ]),
      ),
    ),
    Canonical.array(closure.residualBodies.map(residualEncoding)),
  ])
  return { _tag: 'Eligible', identity: digest(encoded) }
})

/** Derives canonical-order per-test execution identities, failing closed per entry. */
export const make = Effect.fn('TestExecution.make')(function* (
  input: Input,
): Effect.fn.Return<Manifest> {
  const environment = environmentIdentity(input.environment)
  const entries: Array<Entry> = []
  for (const entry of input.catalog.entries) {
    entries.push({ test: entry.info, eligibility: yield* eligible(input, entry, environment) })
  }
  return {
    _tag: 'TestExecutionManifest',
    catalogIdentity: digest(
      Canonical.record('TestExecutionCatalog.v1', [
        Canonical.array(
          input.catalog.entries.map((entry, ordinal) =>
            Canonical.record('Test', [String(ordinal), entry.info.identity]),
          ),
        ),
      ]),
    ),
    environmentIdentity: environment,
    entries,
  }
})
