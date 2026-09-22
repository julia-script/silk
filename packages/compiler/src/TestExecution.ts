import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as AuthoredEncoding from './AuthoredEncoding.js'
import type * as AuthoredIdentity from './AuthoredIdentity.js'
import * as AuthoredLowering from './AuthoredLowering.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as Elaboration from './Elaboration.js'
import * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'
import type * as NativeLinkPlan from './NativeLinkPlan.js'
import * as SuspensionMode from './SuspensionMode.js'
import * as StaticValue from './StaticValue.js'
import type * as TestDiscovery from './TestDiscovery.js'
import * as Tir from './Tir.js'
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
  | { readonly _tag: 'IncompleteDependencyAttribution'; readonly detail: string }
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
          dependency.specialization,
        ]),
      ),
    ),
  ])

type ExecutionEncoding =
  | { readonly _tag: 'Complete'; readonly encoding: string }
  | {
      readonly _tag: 'Incomplete'
      readonly reason: string
      readonly declaration?: DeclarationFacts.CanonicalId
    }

const declarationKey = (declaration: DeclarationFacts.CanonicalId): string =>
  `${declaration.module}\u0000${declaration.name}`

const gapEncoding = (gap: Instances.ExecutionGap): string => {
  switch (gap._tag) {
    case 'MissingRoot':
      return Canonical.record(gap._tag, [Instances.keyText(gap.root)])
    case 'MissingTarget':
      return Canonical.record(gap._tag, [edgeEncoding(gap.edge)])
    case 'MissingResidualAttribution':
    case 'IncompleteResidualAttribution':
      return Canonical.record(gap._tag, [Instances.keyText(gap.instance)])
  }
}

const factTypes = (fact: DeclarationFacts.MemberFact): ReadonlyArray<Type.Type> => {
  if (fact._tag === 'StructDeclaration')
    return fact.fields.flatMap((field) =>
      field.declaredType._tag === 'Resolved' ? [field.declaredType.type] : [],
    )
  if (fact._tag === 'UnionDeclaration')
    return fact.variants.flatMap((variant) =>
      variant.fields.flatMap((field) =>
        field.declaredType._tag === 'Resolved' ? [field.declaredType.type] : [],
      ),
    )
  return []
}

const typeFactEncoding = (fact: DeclarationFacts.MemberFact): string => {
  if (fact._tag === 'StructDeclaration')
    return Canonical.record('Struct', [
      fact.layout._tag,
      Canonical.array(
        fact.fields.map((field) =>
          Canonical.record('Field', [
            field.member._tag,
            field.name._tag === 'Present' ? field.name.spelling : '',
            field.declaredType._tag === 'Resolved'
              ? Type.key(field.declaredType.type)
              : 'Unavailable',
          ]),
        ),
      ),
    ])
  if (fact._tag === 'UnionDeclaration')
    return Canonical.record('Union', [
      Canonical.array(
        fact.variants.map((variant) =>
          Canonical.record('Variant', [
            variant.name._tag === 'Present' ? variant.name.spelling : '',
            Canonical.array(
              variant.fields.map((field) =>
                field.declaredType._tag === 'Resolved'
                  ? Type.key(field.declaredType.type)
                  : 'Unavailable',
              ),
            ),
          ]),
        ),
      ),
    ])
  return Canonical.record('NominalDeclaration', [fact._tag])
}

const executionEncoding = Effect.fnUntraced(function* (
  discovery: Instances.Discovery,
  results: ReadonlyMap<string, Elaboration.Result>,
  closure: Instances.ExecutionClosure,
): Effect.fn.Return<ExecutionEncoding> {
  if (closure.gaps.length > 0)
    return {
      _tag: 'Incomplete',
      reason: Canonical.record('ExecutionGaps', [Canonical.array(closure.gaps.map(gapEncoding))]),
    }
  const declarationIndex = discovery.declarationIndex
  if (declarationIndex === undefined)
    return { _tag: 'Incomplete', reason: 'MissingDeclarationIndex' }
  const authored = new Map<string, string>()
  for (const instance of closure.instances) {
    const value = yield* authoredDigest(
      results,
      instance.key.declaration.module,
      instance.function.declaration.owner,
    )
    if (value === undefined)
      return {
        _tag: 'Incomplete',
        reason: 'MissingAuthoredDependency',
        declaration: instance.key.declaration,
      }
    authored.set(Instances.keyText(instance.key), value)
  }

  const types = new Map<string, Type.Type>()
  const constants = new Map<string, DeclarationFacts.CanonicalId>()
  const addType = (type: Type.Type): void => {
    types.set(Type.key(type), type)
  }
  const addArgument = (argument: Type.GenericArgument): void => {
    if (Type.isTypeArgument(argument)) addType(argument)
  }
  for (const instance of closure.instances) {
    for (const argument of instance.key.typeArguments) addArgument(argument)
    if (instance.function.contract._tag !== 'Unavailable') {
      for (const parameter of instance.function.contract.parameters) addType(parameter)
      addType(instance.function.contract.result)
    }
    for (const local of instance.function.locals ?? []) addType(local.type)
    for (const statement of instance.function.statements)
      for (const expression of Tir.statementExpressions(statement).flatMap(Tir.expressionTree)) {
        if ('type' in expression) addType(expression.type)
        if (expression.constant !== undefined)
          constants.set(declarationKey(expression.constant), expression.constant)
        if (expression._tag === 'ConstantReference')
          constants.set(declarationKey(expression.declaration), expression.declaration)
      }
  }
  for (const callable of closure.callables) {
    addType(callable.type)
    for (const argument of callable.typeArguments) addArgument(argument)
    for (const type of callable.captureTypes) addType(type)
  }
  for (const effect of closure.effects) {
    addType(effect.type)
    for (const argument of effect.typeArguments) addArgument(argument)
    for (const capture of effect.captures) addType(capture.type)
  }
  for (const body of closure.residualBodies)
    for (const dependency of body.dependencies)
      for (const argument of dependency.typeArguments) addArgument(argument)

  const selectedConstants = new Map(
    discovery.constants.map((constant) => [declarationKey(constant.declaration), constant]),
  )
  const constantEncodings: Array<string> = []
  for (const declaration of [...constants.values()].sort((left, right) =>
    Canonical.compare(declarationKey(left), declarationKey(right)),
  )) {
    const selected = selectedConstants.get(declarationKey(declaration))
    const fact = DeclarationFacts.byCanonical(declarationIndex, declaration)
    const source =
      fact === undefined
        ? undefined
        : yield* authoredDigest(results, declaration.module, fact.anchor.owner)
    if (selected === undefined || fact === undefined || source === undefined)
      return {
        _tag: 'Incomplete',
        reason: 'MissingConstantDependency',
        declaration,
      }
    constantEncodings.push(
      Canonical.record('Constant', [
        declarationIdentity(declaration),
        StaticValue.key(selected.value),
        source,
      ]),
    )
  }

  const typeEncodings = new Map<string, string>()
  const pending = [...types.values()]
  while (pending.length > 0) {
    const type = pending.pop()
    if (type === undefined) continue
    for (const nominal of Type.nominalTypes(type)) {
      const key = Type.key(nominal)
      if (typeEncodings.has(key)) continue
      if (Type.isIntrinsicNominal(nominal) || Type.equals(nominal, Type.unit)) {
        typeEncodings.set(key, Canonical.record('SealedType', [key]))
        continue
      }
      const declaration: DeclarationFacts.CanonicalId = {
        _tag: 'CanonicalDeclarationId',
        module: nominal.module,
        name: nominal.name,
      }
      const fact = DeclarationFacts.byCanonical(declarationIndex, declaration)
      const source =
        fact === undefined
          ? undefined
          : yield* authoredDigest(results, declaration.module, fact.anchor.owner)
      if (fact === undefined)
        return {
          _tag: 'Incomplete',
          reason: 'MissingTypeDependency',
          declaration,
        }
      typeEncodings.set(
        key,
        Canonical.record('ResolvedType', [
          declarationIdentity(declaration),
          key,
          typeFactEncoding(fact),
          source ?? '',
        ]),
      )
      pending.push(...factTypes(fact))
    }
  }

  return {
    _tag: 'Complete',
    encoding: Canonical.record('ExecutionClosure.v2', [
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
      Canonical.array(constantEncodings),
      Canonical.array([...typeEncodings.values()].sort(Canonical.compare)),
    ]),
  }
})

/** Stable identity of the compiler-owned runner policy used by synthetic or isolated consumers. */
export const runnerPolicyIdentity = digest('silk-test-runner-policy-v1')

/** Authored identity of the executable roots that invoke the discovered test catalog. */
export const runnerIdentity = Effect.fn('TestExecution.runnerIdentity')(function* (
  discovery: Instances.Discovery,
  results: ReadonlyMap<string, Elaboration.Result>,
  catalog: TestDiscovery.Catalog,
): Effect.fn.Return<{ readonly identity: string; readonly complete: boolean }> {
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
  const excluded = new Set(
    catalog.entries.flatMap((entry) =>
      entry.declaration.canonical._tag === 'Canonical'
        ? [`${entry.declaration.canonical.id.module}\u0000${entry.declaration.canonical.id.name}`]
        : [],
    ),
  )
  let complete = retainedRoots.length > 0
  for (const key of retainedRoots) {
    const instance = instances.get(Instances.keyText(key))
    if (instance === undefined) {
      complete = false
      roots.push(Canonical.record('MissingRunnerRoot', [Instances.keyText(key)]))
      continue
    }
    const closure = Instances.executionClosure(discovery, key, excluded)
    const encoded = yield* executionEncoding(discovery, results, closure)
    if (encoded._tag === 'Incomplete') complete = false
    roots.push(
      Canonical.record('RunnerRoot', [
        Instances.keyText(key),
        encoded._tag === 'Complete' ? encoded.encoding : encoded.reason,
      ]),
    )
  }
  return {
    identity: digest(
      Canonical.record('TestRunner.v1', [runnerPolicyIdentity, Canonical.array(roots)]),
    ),
    complete,
  }
})

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
  helperPolicyIdentity: string,
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
      helperPolicyIdentity,
    ]),
  )
}

/** Selected runtime-support distribution identity, narrowed by the referenced runtime symbols. */
export const runtimeIdentity = (distribution: ToolchainIntegrity.Graph): string =>
  digest(
    Canonical.record('TestRuntimeEnvironment.v1', [
      Canonical.array(
        distribution.components
          .filter((component) => component.kind === 'RuntimeSupport')
          .map((component) => Canonical.record(component.id, [component.digest])),
      ),
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
  const closureEncoding = yield* executionEncoding(input.discovery, input.results, closure)
  if (closureEncoding._tag === 'Incomplete')
    return closureEncoding.declaration === undefined
      ? {
          _tag: 'Ineligible',
          reason: {
            _tag: 'IncompleteDependencyAttribution',
            detail: closureEncoding.reason,
          },
        }
      : {
          _tag: 'Ineligible',
          reason: {
            _tag: 'MissingAuthoredDependency',
            declaration: closureEncoding.declaration,
          },
        }
  const encoded = Canonical.record('TestExecution.v1', [
    entry.info.identity,
    environment,
    closureEncoding.encoding,
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
