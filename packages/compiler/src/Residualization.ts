import * as Location from './Location.js'
import * as Provenance from './Provenance.js'
import * as Effect from 'effect/Effect'
import * as CompilerTrace from './CompilerTrace.js'
import * as ToolchainIntegrity from './ToolchainIntegrity.js'
import type * as CompilationProfile from './CompilationProfile.js'
import * as Lifetime from './Lifetime.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as BodyBuilder from './BodyBuilder.js'
import { analyzeExpression } from './ExpressionAnalysis.js'
import type * as Tir from './Tir.js'
import * as FunctionIndex from './internal/FunctionIndex.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Canonical from './internal/Canonical.js'
import * as NameResolution from './NameResolution.js'
import * as RowAlgebra from './RowAlgebra.js'
import { analyzeFunctionBody } from './StatementAnalysis.js'
import * as Evaluation from './Evaluation.js'
import * as StaticValue from './StaticValue.js'
import type * as AuthoredHir from './AuthoredHir.js'
import * as AuthoredIdentity from './AuthoredIdentity.js'
import * as AuthoredWalk from './AuthoredWalk.js'
import * as SemanticContext from './SemanticContext.js'
import * as Semantic from './Semantic.js'
import type * as Target from './Target.js'
import * as Type from './Type.js'
import type * as TestDiscovery from './TestDiscovery.js'

/** The specialization fields needed before an executable instance is admitted. */
export interface ApplicationKey {
  readonly declaration: DeclarationFacts.CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly evidence: ReadonlyArray<string>
  readonly contractRow: ReadonlyArray<string>
  readonly staticArguments: ReadonlyArray<StaticValue.Value>
}

export interface ResidualBody {
  readonly _tag: 'ResidualBody'
  readonly artifact: Tir.ArtifactId
  readonly function: Tir.TirFunction
  /** The tables the body published, which later stages read beside its nodes. */
  readonly results: Elaboration.BodyResults
  readonly diagnostics: ReadonlyArray<Diagnostic.Located>
}

export type Result =
  | ResidualBody
  | {
      readonly _tag: 'StaticFailure'
      readonly failure: Evaluation.StaticFailure
      readonly diagnostics: ReadonlyArray<Diagnostic.Located>
    }

/** The first declaration-local reason a runtime body needs static selection. */
export type SelectionReason =
  | 'StaticArguments'
  | 'StaticParameter'
  | 'StaticControlFlow'
  | 'StaticBinding'
  | 'UnresolvedConstant'
  | 'CompileError'
  | 'StaticCall'
  | 'UnavailableDeclaration'
  | 'UnavailableBody'

/** Actual residual body work, distinct from runtime instance count and ownership verification. */
export interface Counters {
  readonly _tag: 'ResidualizationCounters'
  readonly requests: number
  readonly sourceReused: number
  readonly checked: number
  readonly cacheReused: number
  readonly rejected: number
  readonly failures: number
}

export interface Observation {
  readonly declaration: DeclarationFacts.CanonicalId
  readonly reason: SelectionReason | 'UnchangedBody'
  readonly counters: Counters
}

type MutableCounters = { -readonly [Key in keyof Counters]: Counters[Key] }

const emptyCounters = (): MutableCounters => ({
  _tag: 'ResidualizationCounters',
  requests: 0,
  sourceReused: 0,
  checked: 0,
  cacheReused: 0,
  rejected: 0,
  failures: 0,
})

/** Work reported when target selection prevented residualization from starting. */
export const noWork: Counters = Object.freeze(emptyCounters())

interface State {
  conditionDiagnostics?: Array<Diagnostic.Located>
  conditionExpression?: Elaboration.ExpressionDecision
  readonly target: Target.Target
  readonly dependencies: Map<string, string>
  readonly parameters: ReadonlyMap<string, StaticValue.Value>
  readonly environment: Evaluation.TargetEnvironment
  readonly results: ReadonlyMap<string, Elaboration.Result>
  /** Anchor-to-span resolution for every elaborated module of this closure. */
  readonly spans: SemanticContext.Registry
  readonly resolution: NameResolution.Resolution
  readonly index: DeclarationIndex.Index
  readonly semantic: Semantic.Session
  /** Generated declarations published by source and residual bodies in this session. */
  readonly generatedAggregates: Map<string, DeclarationFacts.StructFact>
  readonly evaluation: Evaluation.Evaluation<StaticValue.Value>
  readonly residuals: Evaluation.Evaluation<ResidualBody>
  readonly staticResultOrigins: Map<string, Evaluation.TextOrigin>
  readonly counters: MutableCounters
  readonly observations: Map<string, Observation & { readonly counters: MutableCounters }>
  readonly selectionReasons: Map<DeclarationFacts.DeclarationFact, SelectionReason | undefined>
  readonly testCatalog?: TestDiscovery.Catalog
}

const stateSymbol: unique symbol = Symbol('Residualization.state')

interface EvaluationCoordinator {
  readonly _tag: 'ResidualizationCoordinator' | 'ProfileBootstrapCoordinator'
  readonly [stateSymbol]: State
}

/** An ordinary specialization coordinator, constructible only from a completed profile. */
export interface Coordinator extends EvaluationCoordinator {
  readonly _tag: 'ResidualizationCoordinator'
}

/** A private-bootstrap evaluator that cannot enter runtime residualization. */
export interface BootstrapCoordinator extends EvaluationCoordinator {
  readonly _tag: 'ProfileBootstrapCoordinator'
}

const generatedAggregateKey = (id: DeclarationFacts.CanonicalId): string =>
  `${id.module}:${id.name}`

const lookupDeclaration = (
  self: EvaluationCoordinator,
  id: DeclarationFacts.CanonicalId,
): DeclarationFacts.MemberFact | undefined =>
  self[stateSymbol].generatedAggregates.get(generatedAggregateKey(id)) ??
  DeclarationFacts.byCanonical(self[stateSymbol].index, id)

const publishGeneratedAggregates = (
  self: EvaluationCoordinator,
  aggregates: Iterable<DeclarationFacts.StructFact>,
): void => {
  for (const aggregate of aggregates) {
    if (aggregate.canonical._tag !== 'Canonical') continue
    self[stateSymbol].generatedAggregates.set(
      generatedAggregateKey(aggregate.canonical.id),
      aggregate,
    )
  }
}

const makeState = (
  compilation: CompilationProfile.Initial | CompilationProfile.CompilationProfile,
  results: ReadonlyMap<string, Elaboration.Result>,
  resolution: NameResolution.Resolution,
  index: DeclarationIndex.Index,
  limits: Evaluation.Limits = Evaluation.defaultLimits,
  parameters: ReadonlyMap<string, StaticValue.Value> = new Map(),
  trace: CompilerTrace.CompilerTrace = CompilerTrace.none,
  testCatalog?: TestDiscovery.Catalog,
): State => {
  const sourceIdentity = ToolchainIntegrity.contentDigest(
    Canonical.array([
      ...[...results]
        .toSorted(([a], [b]) => Canonical.compare(a, b))
        .map(([module, result]) =>
          Canonical.record(module, [
            ToolchainIntegrity.contentDigest(
              new TextEncoder().encode(result.authored.presentation.revision),
            ),
          ]),
        ),
      Canonical.record('TestCatalog', [testCatalog?.identity ?? '']),
    ]),
  )
  const generatedAggregates = new Map(index.generatedAggregates)
  const semantic = Semantic.makeSession(
    `residualization:${sourceIdentity}:${compilation.target.id}`,
    index,
    resolution,
    compilation.target.id,
  )
  return {
    target: compilation.target,
    parameters: new Map(parameters),
    dependencies: new Map(),
    environment: Evaluation.targetEnvironment(compilation, sourceIdentity),
    results,
    spans: SemanticContext.fromModules([...results.values()]),
    resolution,
    index: Object.freeze({ ...index, generatedAggregates }),
    semantic,
    generatedAggregates,
    evaluation: Evaluation.make<StaticValue.Value>(compilation, limits, sourceIdentity, trace),
    residuals: Evaluation.make<ResidualBody>(compilation, limits, sourceIdentity, trace),
    staticResultOrigins: new Map<string, Evaluation.TextOrigin>(),
    counters: emptyCounters(),
    observations: new Map(),
    selectionReasons: new Map(),
    ...(testCatalog === undefined ? {} : { testCatalog }),
  }
}

/** Starts ordinary specialization for one complete logical configuration and source graph. */
export const make = (
  compilation: CompilationProfile.CompilationProfile,
  results: ReadonlyMap<string, Elaboration.Result>,
  resolution: NameResolution.Resolution,
  index: DeclarationIndex.Index,
  limits: Evaluation.Limits = Evaluation.defaultLimits,
  parameters: ReadonlyMap<string, StaticValue.Value> = new Map(),
  trace: CompilerTrace.CompilerTrace = CompilerTrace.none,
  testCatalog?: TestDiscovery.Catalog,
): Coordinator =>
  Object.freeze({
    _tag: 'ResidualizationCoordinator',
    [stateSymbol]: makeState(
      compilation,
      results,
      resolution,
      index,
      limits,
      parameters,
      trace,
      testCatalog,
    ),
  })

/** Starts default/predicate evaluation without granting runtime-specialization admission. */
export const makeBootstrap = (
  compilation: CompilationProfile.Initial,
  results: ReadonlyMap<string, Elaboration.Result>,
  resolution: NameResolution.Resolution,
  index: DeclarationIndex.Index,
  parameters: ReadonlyMap<string, StaticValue.Value>,
  trace: CompilerTrace.CompilerTrace = CompilerTrace.none,
): BootstrapCoordinator =>
  Object.freeze({
    _tag: 'ProfileBootstrapCoordinator',
    [stateSymbol]: makeState(
      compilation,
      results,
      resolution,
      index,
      Evaluation.defaultLimits,
      parameters,
      trace,
    ),
  })

/** Returns canonical source bodies actually demanded during this evaluation session. */
export const dependencies = (self: EvaluationCoordinator): string =>
  Canonical.array(
    [...self[stateSymbol].dependencies]
      .toSorted(([a], [b]) => Canonical.compare(a, b))
      .map(([key, value]) => Canonical.record(key, [value])),
  )

/** Snapshots work performed by this target-scoped coordinator. */
export const counters = (self: EvaluationCoordinator): Counters =>
  Object.freeze({ ...self[stateSymbol].counters })

/** Snapshots source and target-specialized aggregates created during this evaluation session. */
export const generatedAggregates = (
  self: EvaluationCoordinator,
): ReadonlyMap<string, DeclarationFacts.StructFact> =>
  new Map(self[stateSymbol].generatedAggregates)

/** Snapshots declaration/reason attribution without counting retained proof work as execution. */
export const observations = (self: EvaluationCoordinator): ReadonlyArray<Observation> =>
  Object.freeze(
    [...self[stateSymbol].observations.entries()]
      .toSorted(([left], [right]) => {
        if (left < right) return -1
        return left > right ? 1 : 0
      })
      .map(([, observation]) =>
        Object.freeze({ ...observation, counters: Object.freeze({ ...observation.counters }) }),
      ),
  )

const record = (
  self: EvaluationCoordinator,
  declaration: DeclarationFacts.CanonicalId,
  reason: Observation['reason'],
  outcome: 'sourceReused' | 'checked' | 'cacheReused' | 'rejected',
  failed: boolean,
): void => {
  const state = self[stateSymbol]
  const key = Canonical.record('ResidualBodyWork', [declaration.module, declaration.name, reason])
  let observation = state.observations.get(key)
  if (observation === undefined) {
    observation = { declaration, reason, counters: emptyCounters() }
    state.observations.set(key, observation)
  }
  for (const work of [state.counters, observation.counters]) {
    work.requests += 1
    work[outcome] += 1
    if (failed) work.failures += 1
  }
}

const reflectAggregate = (
  self: EvaluationCoordinator,
  authorization: DeclarationFacts.DeclarationFact,
  owner: Type.Type,
  kind: 'Type' | 'Fields',
  span: Location.Location,
  trace: Evaluation.Trace,
  lookup: Evaluation.NodeContext['lookup'],
): Evaluation.Outcome<StaticValue.Value> => {
  if (!Type.isNominal(owner) || authorization.canonical._tag !== 'Canonical')
    return Evaluation.failed(
      Evaluation.phaseViolation(
        'Evaluation.reflect',
        'reflection requires one concrete nominal owner and canonical authorization',
        span,
        trace,
      ),
    )
  const declaration = lookup({
    _tag: 'CanonicalDeclarationId',
    module: owner.module,
    name: owner.name,
  })
  if (declaration?._tag !== 'StructDeclaration' || declaration.canonical._tag !== 'Canonical')
    return Evaluation.failed(
      Evaluation.phaseViolation(
        'Evaluation.reflect',
        `${Type.encode(owner)} is not a concrete aggregate`,
        span,
        trace,
      ),
    )
  const authorizationId = authorization.canonical.id
  const declarationId = declaration.canonical.id
  const descriptor: StaticValue.TypeDescriptorValue = Object.freeze({
    _tag: 'TypeDescriptorValue',
    owner,
    kind: declaration.aggregateKind,
  })
  const substitution = TypeInference.substitution(
    declaration.typeParameters.map((parameter) => parameter.type),
    owner.arguments,
  )
  if (substitution === undefined)
    return Evaluation.failed(
      Evaluation.phaseViolation(
        'Evaluation.reflect',
        `${Type.encode(owner)} does not completely specialize its aggregate declaration`,
        span,
        trace,
      ),
    )
  const candidate: StaticValue.Value =
    kind === 'Type'
      ? descriptor
      : Object.freeze({
          _tag: 'FieldCollectionValue',
          owner: descriptor,
          fields: Object.freeze(
            declaration.fields.flatMap((field): ReadonlyArray<StaticValue.FieldDescriptorValue> => {
              if (
                (field.visibility === 'Private' &&
                  authorizationId.module !== declarationId.module) ||
                field.declaredType._tag !== 'Resolved'
              )
                return []
              const member: StaticValue.ReflectedMember =
                field.member._tag === 'LabeledAggregateMember'
                  ? Object.freeze({ _tag: 'LabeledField', label: field.member.label })
                  : Object.freeze({ _tag: 'PositionalField', ordinal: field.member.ordinal })
              return [
                Object.freeze({
                  _tag: 'FieldDescriptorValue',
                  owner: descriptor,
                  declarationOrdinal: field.id.ordinal,
                  member,
                  valueType: Type.substitute(field.declaredType.type, substitution),
                  authorization: authorizationId,
                  provenance: Object.freeze({ anchor: field.anchor }),
                }),
              ]
            }),
          ),
        })
  const admission = StaticValue.admit(candidate, {
    pointerBits: self[stateSymbol].environment.pointerBits,
  })
  return admission._tag === 'Admitted'
    ? Evaluation.complete(admission.value)
    : Evaluation.failed(
        Evaluation.phaseViolation('Evaluation.reflect', admission.detail, span, trace),
      )
}

const declarationOf = (
  self: EvaluationCoordinator,
  identity: DeclarationFacts.CanonicalId,
): DeclarationFacts.DeclarationFact | undefined => {
  const declaration = lookupDeclaration(self, identity)
  if (declaration?._tag === 'FunctionDeclaration') return declaration
  return FunctionIndex.tirByCanonical(self[stateSymbol].results.get(identity.module)?.tir, identity)
    ?.declaration
}

const moduleInput = (
  self: EvaluationCoordinator,
  declaration: { readonly id: DeclarationFacts.DeclarationId },
):
  | {
      readonly result: Elaboration.Result
      readonly scope: NameResolution.ModuleScope
      readonly declarations: ReadonlyArray<DeclarationFacts.DeclarationFact>
    }
  | undefined => {
  const result = self[stateSymbol].results.get(declaration.id.sourceId)
  const scope = NameResolution.scopeOf(self[stateSymbol].resolution, declaration.id.sourceId)
  const headers = self[stateSymbol].index.modules.find(
    (module) => module.module === declaration.id.sourceId,
  )
  return result === undefined || scope === undefined || headers === undefined
    ? undefined
    : Object.freeze({ result, scope, declarations: headers.declarations })
}

/**
 * Gives a synthetic module-condition declaration a deterministic non-source-position slot.
 * The full authored key remains its canonical name; this compact ordinal only satisfies the
 * declaration table's numeric carrier and occupies a range ordinary collected declarations never use.
 */
const moduleConditionOrdinal = (owner: AuthoredIdentity.Identity): number => {
  let hash = 0x811c9dc5
  for (const character of AuthoredIdentity.key(owner)) {
    hash ^= character.codePointAt(0) ?? 0
    hash = Math.imul(hash, 0x01000193) >>> 0
  }
  return 0x40000000 + (hash & 0x3fffffff)
}

const emptyFailureRow = (): DeclarationFacts.FailureRowFact =>
  Object.freeze({
    _tag: 'FailureRow',
    members: Object.freeze([]),
    parameters: Object.freeze([]),
    failures: Object.freeze([]),
    available: true,
    expression: Object.freeze({ _tag: 'EmptyRowExpression' }),
    row: RowAlgebra.concrete(Type.failureRowPolicy(), []),
  })

const emptyRequirementRow = (): DeclarationFacts.RequirementRowFact =>
  Object.freeze({
    _tag: 'RequirementRow',
    entries: Object.freeze([]),
    parameters: Object.freeze([]),
    requirements: Object.freeze([]),
    available: true,
    expression: Object.freeze({ _tag: 'EmptyRowExpression' }),
    row: RowAlgebra.concrete(Type.requirementRowPolicy(), []),
  })

const constantHost = (
  declaration: DeclarationFacts.ConstantFact,
): DeclarationFacts.DeclarationFact =>
  Object.freeze({
    _tag: 'FunctionDeclaration',
    id: declaration.id,
    canonical: declaration.canonical,
    visibility: 'Private',
    phase: 'Static',
    functionKind: 'Ordinary',
    test: false,
    unsafe: false,
    typeParameters: Object.freeze([]),
    parameterCount: 0,
    parameters: Object.freeze([]),
    name: declaration.name,
    returnType: declaration.declaredType,
    failureRow: emptyFailureRow(),
    requirementRow: emptyRequirementRow(),
    constraints: Object.freeze([]),
    constraintContracts: Object.freeze([]),
    anchor: declaration.anchor,
    owner: declaration.anchor.owner,
  })

const bindStaticParameters = (
  declaration: DeclarationFacts.DeclarationFact,
  arguments_: ReadonlyArray<StaticValue.Value>,
  argumentSpans: ReadonlyArray<Location.Location | undefined> = Object.freeze([]),
  argumentOrigins: ReadonlyArray<Evaluation.TextOrigin | undefined> = Object.freeze([]),
  originScope?: string,
):
  | {
      readonly values: Map<string, StaticValue.Value>
      readonly valueSpans: Map<string, Location.Location>
      readonly valueOrigins: Map<string, Evaluation.TextOrigin>
    }
  | undefined => {
  const parameters = declaration.parameters.filter((parameter) => parameter.phase === 'Static')
  if (parameters.length !== arguments_.length) return undefined
  const values = new Map(
    parameters.flatMap((parameter, ordinal) => {
      const value = arguments_.at(ordinal)
      return value === undefined
        ? []
        : [
            [Evaluation.localValueKey(parameter), value] as const,
            [
              Evaluation.tirLocalKey({ _tag: 'TirLocal', ordinal: parameter.id.ordinal }),
              value,
            ] as const,
          ]
    }),
  )
  const valueSpans = new Map(
    parameters.flatMap((parameter, ordinal) => {
      const span = argumentSpans.at(ordinal)
      return span === undefined
        ? []
        : [
            [Evaluation.localValueKey(parameter), span] as const,
            [
              Evaluation.tirLocalKey({ _tag: 'TirLocal', ordinal: parameter.id.ordinal }),
              span,
            ] as const,
          ]
    }),
  )
  const valueOrigins = new Map(
    parameters.flatMap((parameter, ordinal) => {
      const value = arguments_.at(ordinal)
      const origin = argumentOrigins.at(ordinal)
      const selected =
        origin ??
        Evaluation.parameterTextOrigin(
          ordinal,
          value?._tag === 'TextValue' ? value.bytes.length : 0,
          originScope,
        )
      return [
        [Evaluation.localValueKey(parameter), selected] as const,
        [
          Evaluation.tirLocalKey({ _tag: 'TirLocal', ordinal: parameter.id.ordinal }),
          selected,
        ] as const,
      ]
    }),
  )
  return Object.freeze({ values, valueSpans, valueOrigins })
}

const resolveTextOrigin = (
  origin: Evaluation.TextOrigin | undefined,
  arguments_: ReadonlyArray<Evaluation.TextOrigin | undefined>,
  scope: string,
): Evaluation.TextOrigin | undefined => {
  if (origin === undefined) return undefined
  // Each parameter part becomes what this caller wrote for it; a computed argument leaves none.
  const resolved = Provenance.substitute(origin, arguments_, scope)
  return resolved.length === 0 ? undefined : resolved
}

const resolveValueOrigins = (
  value: StaticValue.Value,
  arguments_: ReadonlyArray<Evaluation.TextOrigin | undefined>,
  scope: string,
): StaticValue.Value => {
  if (value._tag === 'TextValue') {
    const origin = resolveTextOrigin(value.origin, arguments_, scope)
    return origin === value.origin
      ? value
      : Object.freeze({ ...value, ...(origin === undefined ? {} : { origin }) })
  }
  if (value._tag === 'AggregateValue')
    return Object.freeze({
      ...value,
      fields: Object.freeze(
        value.fields.map((field) =>
          Object.freeze({
            ...field,
            value: resolveValueOrigins(field.value, arguments_, scope),
          }),
        ),
      ),
    })
  if (value._tag === 'StaticSequenceValue')
    return Object.freeze({
      ...value,
      elements: Object.freeze(
        value.elements.map((element) => resolveValueOrigins(element, arguments_, scope)),
      ),
    })
  return value
}

/** The headers a body can name: the index, and before it the aggregates the body generated. */
const bodyLookup =
  (
    lookup: Evaluation.NodeContext['lookup'],
    generated: ReadonlyArray<DeclarationFacts.StructFact>,
  ): Evaluation.NodeContext['lookup'] =>
  (id) =>
    generated.find(
      (aggregate) =>
        aggregate.canonical._tag === 'Canonical' &&
        aggregate.canonical.id.module === id.module &&
        aggregate.canonical.id.name === id.name,
    ) ?? lookup(id)

const resolveTextSpan = (
  origin: Evaluation.TextOrigin | undefined,
  arguments_: ReadonlyArray<Location.Location | undefined>,
): Location.Location | undefined => {
  // The node a result is reported at is where its text begins: a literal, or this call's argument.
  const first = origin?.at(0)?.from
  if (first === undefined) return undefined
  return first._tag === 'Literal' ? Location.at(first.at) : arguments_.at(first.ordinal)
}

const evaluateStaticFunction = (
  self: EvaluationCoordinator,
  declaration: DeclarationFacts.DeclarationFact,
  arguments_: ReadonlyArray<StaticValue.Value>,
  argumentSpans: ReadonlyArray<Location.Location | undefined>,
  argumentOrigins: ReadonlyArray<Evaluation.TextOrigin | undefined>,
  span: Parameters<Evaluation.NodeContext['call']>[4],
  parentTrace: Evaluation.Trace,
  identity: Parameters<Evaluation.NodeContext['call']>[6],
  lookup: Parameters<Evaluation.NodeContext['call']>[7],
): Evaluation.CallResult => {
  if (declaration.canonical._tag !== 'Canonical')
    return Object.freeze({
      outcome: Evaluation.failed(
        Evaluation.phaseViolation(
          'Evaluation.call',
          'static callee has no canonical identity',
          span,
          parentTrace,
        ),
      ),
    })
  self[stateSymbol].dependencies.set(
    Canonical.record('helper', [declaration.canonical.id.module, declaration.canonical.id.name]),
    declaration.bodyTemplate?.canonical ?? '',
  )
  const application: Evaluation.Application = Object.freeze({
    declaration: declaration.canonical.id,
    typeArguments: Object.freeze(identity.typeArguments.map(Type.genericArgumentKey)),
    evidence: identity.evidence,
    contractRow: identity.contractRow,
    staticArguments: arguments_,
    span,
  })
  const originScope = Evaluation.applicationKey(self[stateSymbol].environment, application)
  const result = Semantic.evaluateFrom(
    self[stateSymbol].semantic,
    self[stateSymbol].evaluation,
    application,
    parentTrace,
    (evaluation) => {
      const input = moduleInput(self, declaration)
      const bindings = bindStaticParameters(
        declaration,
        arguments_,
        argumentSpans,
        Object.freeze([]),
        originScope,
      )
      const typeSubstitution = TypeInference.substitution(
        declaration.typeParameters.map((parameter) => parameter.type),
        identity.typeArguments,
      )
      if (input === undefined || bindings === undefined || typeSubstitution === undefined)
        return Evaluation.failed(
          Evaluation.phaseViolation(
            'Evaluation.call',
            'static application does not match its declaration',
            span,
            evaluation.trace,
          ),
        )
      const call: Evaluation.NodeContext['call'] = (
        callee,
        nestedArguments,
        nestedArgumentSpans,
        nestedArgumentOrigins,
        callSpan,
        trace,
        nestedIdentity,
        nestedLookup,
      ) =>
        evaluateStaticFunction(
          self,
          callee,
          nestedArguments,
          nestedArgumentSpans,
          nestedArgumentOrigins,
          callSpan,
          trace,
          nestedIdentity,
          nestedLookup,
        )
      const semantic = SemanticContext.make(input.result.authored)
      const builder = BodyBuilder.make(
        Object.freeze({ owner: declaration.owner, request: Object.freeze({ _tag: 'Check' }) }),
      )
      const staticContext = {
        environment: self[stateSymbol].environment,
        ...(self[stateSymbol].testCatalog === undefined
          ? {}
          : { testCatalog: self[stateSymbol].testCatalog }),
        typeSubstitution,
        values: bindings.values,
        valueSpans: bindings.valueSpans,
        valueOrigins: bindings.valueOrigins,
        expressionSpans: new Map<Tir.Expression, Location.Location>(),
        expressionOrigins: new Map<Tir.Expression, Evaluation.TextOrigin>(),
        nodes: BodyBuilder.staticLowering(semantic, builder),
        lookup,
        returnedTextSpan: { value: undefined },
        returnedTextOrigin: { value: undefined },
        trace: evaluation.trace,
        call,
        chargeStaticIteration: (trace: Evaluation.Trace) => evaluation.stepAt(trace),
        reflect: (
          owner: Type.Type,
          kind: 'Type' | 'Fields',
          reflectSpan: Location.Location,
          trace: Evaluation.Trace,
          lookup: Evaluation.NodeContext['lookup'],
        ) => reflectAggregate(self, declaration, owner, kind, reflectSpan, trace, lookup),
        constant: (
          constant: DeclarationFacts.ConstantFact,
          constantSpan: Location.Location,
          trace: Evaluation.Trace,
        ) => evaluateConstantValue(self, constant, constantSpan, trace),
      }
      const analyzed = analyzeFunctionBody(
        semantic,
        declaration,
        input.declarations,
        Object.freeze({
          scope: input.scope,
          index: self[stateSymbol].index,
          semantic: self[stateSymbol].semantic,
          builder,
        }),
        staticContext,
      )
      publishGeneratedAggregates(self, analyzed.fact.generatedAggregates)
      self[stateSymbol].conditionDiagnostics?.push(...analyzed.diagnostics)
      let nestedStaticFailure: Evaluation.StaticFailure | undefined
      Elaboration.visitStatements(analyzed.fact.statements, {
        expression: (expression) => {
          if (
            nestedStaticFailure === undefined &&
            expression._tag === 'Call' &&
            expression.staticFailure !== undefined
          )
            nestedStaticFailure = expression.staticFailure
        },
        node: (expression) => {
          if (nestedStaticFailure !== undefined) return
          if (expression._tag === 'StaticCall' && expression.failure !== undefined) {
            nestedStaticFailure = expression.failure
            return
          }
        },
      })
      if (nestedStaticFailure !== undefined) {
        return Evaluation.failed(nestedStaticFailure)
      }
      const firstError = analyzed.diagnostics.find((diagnostic) => diagnostic.severity === 'error')
      if (firstError !== undefined) {
        return Evaluation.failed(
          Evaluation.phaseViolation(
            'Evaluation.call',
            firstError.message,
            firstError.span,
            evaluation.trace,
          ),
        )
      }
      const value = Evaluation.evaluateStatements(
        staticContext.nodes.statements(analyzed.fact.statements),
        {
          ...staticContext,
          lookup: bodyLookup(staticContext.lookup, analyzed.fact.generatedAggregates),
          step: () => evaluation.step(),
        },
      )
      if (value._tag === 'Complete') {
        if (staticContext.returnedTextOrigin.value !== undefined)
          self[stateSymbol].staticResultOrigins.set(
            Evaluation.applicationKey(self[stateSymbol].environment, evaluation.application),
            staticContext.returnedTextOrigin.value,
          )
        const retained = evaluation.retain(value.value)
        if (retained !== undefined) return Evaluation.failed(retained)
      }
      return value
    },
  )
  if (result._tag === 'Failed') {
    if (result.failure._tag !== 'CompileError')
      return Object.freeze({ outcome: Evaluation.failed(result.failure) })
    const origin = resolveTextOrigin(result.failure.origin, argumentOrigins, originScope)
    const failure = Object.freeze({
      ...result.failure,
      span:
        (origin === undefined
          ? undefined
          : Evaluation.textOriginLocation(origin, Location.anchorOf(result.failure.span))) ??
        result.failure.span,
      ...(origin === undefined ? {} : { origin }),
    })
    return Object.freeze({ outcome: Evaluation.failed(failure) })
  }
  const cachedOrigin = self[stateSymbol].staticResultOrigins.get(result.key)
  const textOrigin = resolveTextOrigin(cachedOrigin, argumentOrigins, originScope)
  const textSpan = resolveTextSpan(cachedOrigin, argumentSpans)
  return Object.freeze({
    outcome: Evaluation.complete(resolveValueOrigins(result.value, argumentOrigins, originScope)),
    ...(textSpan === undefined ? {} : { textSpan }),
    ...(textOrigin === undefined ? {} : { textOrigin }),
  })
}

const staticValueType = (value: StaticValue.Value): Type.Type | undefined => {
  if (value._tag === 'BooleanValue') return 'bool'
  if (value._tag === 'CharacterValue') return 'char'
  if (value._tag === 'IntegerValue' || value._tag === 'FloatValue') return value.type
  if (value._tag === 'TextValue') return Type.string(Lifetime.staticLifetime)
  return undefined
}

function evaluateConstantValue(
  self: EvaluationCoordinator,
  declaration: DeclarationFacts.ConstantFact,
  span: Location.Location,
  parentTrace: Evaluation.Trace,
  predicate?: AuthoredHir.Expression,
): Evaluation.Outcome<StaticValue.Value> {
  if (declaration.canonical._tag !== 'Canonical')
    return Evaluation.failed(
      Evaluation.phaseViolation(
        'Evaluation.constant',
        'constant has no canonical identity',
        span,
        parentTrace,
      ),
    )
  const initializer = predicate ?? declaration.initializer
  const expected =
    predicate === undefined && declaration.declaredType._tag === 'Resolved'
      ? declaration.declaredType.type
      : 'bool'
  if (predicate === undefined && declaration._tag === 'PackageParameterDeclaration') {
    const bound = self[stateSymbol].parameters.get(
      Canonical.record('PackageParameter', [
        declaration.canonical.id.module,
        declaration.canonical.id.name,
      ]),
    )
    if (bound !== undefined) return Evaluation.complete(bound)
  }
  let dependencyTemplate = declaration.initializerTemplate?.canonical ?? ''
  if (predicate !== undefined)
    dependencyTemplate =
      declaration._tag === 'PackageParameterDeclaration'
        ? (declaration.predicateTemplate?.canonical ?? '')
        : ''
  self[stateSymbol].dependencies.set(
    Canonical.record(predicate === undefined ? 'default' : 'predicate', [
      declaration.canonical.id.module,
      declaration.canonical.id.name,
    ]),
    dependencyTemplate,
  )
  const application: Evaluation.Application = Object.freeze({
    declaration:
      predicate === undefined
        ? declaration.canonical.id
        : Object.freeze({
            ...declaration.canonical.id,
            name: `${declaration.canonical.id.name}#validation`,
          }),
    typeArguments: Object.freeze([]),
    evidence: Object.freeze([]),
    contractRow: Object.freeze([]),
    staticArguments: Object.freeze([]),
    span,
  })
  const result = Semantic.evaluateFrom(
    self[stateSymbol].semantic,
    self[stateSymbol].evaluation,
    application,
    parentTrace,
    (evaluation) => {
      const input = moduleInput(self, declaration)
      if (input === undefined || declaration.declaredType._tag !== 'Resolved')
        return Evaluation.failed(
          Evaluation.phaseViolation(
            'Evaluation.constant',
            'constant declaration is unavailable',
            Location.at(initializer.anchor),
            evaluation.trace,
          ),
        )
      const call: Evaluation.NodeContext['call'] = (
        callee,
        arguments_,
        argumentSpans,
        argumentOrigins,
        callSpan,
        trace,
        identity,
        lookup,
      ) =>
        evaluateStaticFunction(
          self,
          callee,
          arguments_,
          argumentSpans,
          argumentOrigins,
          callSpan,
          trace,
          identity,
          lookup,
        )
      const constant: NonNullable<Evaluation.NodeContext['constant']> = (
        nested,
        nestedSpan,
        trace,
      ) => evaluateConstantValue(self, nested, nestedSpan, trace)
      const host = constantHost(declaration)
      const semantic = SemanticContext.make(input.result.authored)
      const builder = BodyBuilder.make(
        Object.freeze({ owner: host.owner, request: Object.freeze({ _tag: 'Check' }) }),
      )
      const generatedAggregates = new Map<string, DeclarationFacts.StructFact>()
      const staticContext = Object.freeze({
        environment: self[stateSymbol].environment,
        ...(self[stateSymbol].testCatalog === undefined
          ? {}
          : { testCatalog: self[stateSymbol].testCatalog }),
        values: new Map<string, StaticValue.Value>(),
        valueSpans: new Map<string, Location.Location>(),
        valueOrigins: new Map<string, Evaluation.TextOrigin>(),
        expressionSpans: new Map<Tir.Expression, Location.Location>(),
        expressionOrigins: new Map<Tir.Expression, Evaluation.TextOrigin>(),
        nodes: BodyBuilder.staticLowering(semantic, builder),
        lookup: (id: DeclarationFacts.CanonicalId) => lookupDeclaration(self, id),
        trace: evaluation.trace,
        call,
        reflect: (
          owner: Type.Type,
          kind: 'Type' | 'Fields',
          reflectSpan: Location.Location,
          trace: Evaluation.Trace,
          lookup: Evaluation.NodeContext['lookup'],
        ) => reflectAggregate(self, host, owner, kind, reflectSpan, trace, lookup),
        constant,
      })
      const analyzed = analyzeExpression(
        semantic,
        initializer,
        input.declarations,
        host,
        Object.freeze({
          parameters: Object.freeze([]),
          bindings: Object.freeze([]),
          patternBindings: Object.freeze([]),
        }),
        Object.freeze({
          scope: input.scope,
          index: self[stateSymbol].index,
          semantic: self[stateSymbol].semantic,
          staticContext,
          builder,
          generatedAggregates,
        }),
        expected,
      )
      publishGeneratedAggregates(self, generatedAggregates.values())
      if (analyzed !== undefined)
        self[stateSymbol].conditionDiagnostics?.push(...analyzed.diagnostics)
      if (analyzed !== undefined && predicate === undefined)
        self[stateSymbol].conditionExpression = analyzed.fact
      let nestedFailure: Evaluation.StaticFailure | undefined
      if (analyzed !== undefined)
        Elaboration.visitExpressionDecisions(analyzed.fact, {
          expression: (expression) => {
            if (
              nestedFailure === undefined &&
              expression._tag === 'Call' &&
              expression.staticFailure !== undefined
            )
              nestedFailure = expression.staticFailure
          },
        })
      if (nestedFailure !== undefined) return Evaluation.failed(nestedFailure)
      const firstError = analyzed?.diagnostics.find((diagnostic) => diagnostic.severity === 'error')
      if (analyzed === undefined || firstError !== undefined)
        return Evaluation.failed(
          Evaluation.phaseViolation(
            'Evaluation.constant',
            firstError?.message ?? 'constant initializer cannot be analyzed',
            firstError?.span ?? Location.at(initializer.anchor),
            evaluation.trace,
          ),
        )
      const value = Evaluation.evaluateNode(staticContext.nodes.expression(analyzed.fact), {
        ...staticContext,
        step: () => evaluation.step(),
      })
      if (value._tag === 'Failed') return value
      const actual = staticValueType(value.value)
      if (
        (predicate !== undefined || declaration._tag !== 'PackageParameterDeclaration') &&
        (actual === undefined || !Type.equals(actual, expected))
      )
        return Evaluation.failed(
          Evaluation.phaseViolation(
            'Evaluation.constant',
            `initializer produced ${actual === undefined ? 'an unsupported aggregate' : Type.display(actual)} instead of ${Type.display(declaration.declaredType.type)}`,
            Location.at(initializer.anchor),
            evaluation.trace,
          ),
        )
      const retained = evaluation.retain(value.value)
      return retained === undefined ? value : Evaluation.failed(retained)
    },
  )
  return result._tag === 'Complete'
    ? Evaluation.complete(result.value)
    : Evaluation.failed(result.failure)
}

/** Evaluates one explicitly typed primitive constant for this coordinator's selected target. */
export const evaluateConstant = (
  self: EvaluationCoordinator,
  declaration: DeclarationFacts.ConstantFact,
): Evaluation.Outcome<StaticValue.Value> =>
  evaluateConstantValue(
    self,
    declaration,
    Location.at(declaration.initializer.anchor),
    Object.freeze([]),
  )

/** Checks and evaluates a module condition through ordinary static expression and helper semantics. */
export const evaluateModuleCondition = Effect.fn('Residualization.evaluateModuleCondition')(
  (
    self: Coordinator,
    declaration: AuthoredHir.Declaration,
  ): Effect.Effect<{
    readonly outcome: Evaluation.Outcome<StaticValue.Value>
    readonly diagnostics: ReadonlyArray<Diagnostic.Located>
    readonly expression?: Elaboration.ExpressionDecision
  }> =>
    Effect.sync(() => {
      const anchor: AuthoredHir.Anchor = {
        _tag: 'AuthoredAnchor',
        owner: declaration.owner,
        path: [],
      }
      const span = Location.at(anchor)
      if (declaration.header._tag !== 'ConditionalHeader')
        return {
          outcome: Evaluation.failed(
            Evaluation.phaseViolation(
              'ModuleSelection.condition',
              'declaration does not own a module condition',
              span,
              [],
            ),
          ),
          diagnostics: Object.freeze([]),
        }
      const expression = declaration.header.condition
      const name = `#module-condition:${AuthoredIdentity.key(declaration.owner)}`
      const canonical: DeclarationFacts.CanonicalId = {
        _tag: 'CanonicalDeclarationId',
        module: declaration.owner.module,
        name,
      }
      const constant: DeclarationFacts.ConstantDeclaration = {
        _tag: 'ConstantDeclaration',
        id: {
          _tag: 'DeclarationId',
          sourceId: declaration.owner.module,
          ordinal: moduleConditionOrdinal(declaration.owner),
        },
        canonical: { _tag: 'Canonical', id: canonical },
        visibility: 'Private',
        typeParameters: [],
        name: { _tag: 'Present', spelling: name, anchor },
        declaredType: { _tag: 'Resolved', type: 'bool', spelling: 'bool', anchor },
        initializerTemplate: {
          _tag: 'StaticExpressionTemplate',
          anchor: expression.anchor,
          canonical: name,
        },
        literal: { _tag: 'Unavailable', anchor: expression.anchor },
        initializer: expression,
        anchor,
      }
      const diagnostics: Array<Diagnostic.Located> = []
      self[stateSymbol].conditionDiagnostics = diagnostics
      const outcome = evaluateConstant(self, constant)
      const expressionFact = self[stateSymbol].conditionExpression
      delete self[stateSymbol].conditionDiagnostics
      delete self[stateSymbol].conditionExpression
      return Object.freeze({
        outcome,
        diagnostics: Object.freeze(diagnostics),
        ...(expressionFact === undefined ? {} : { expression: expressionFact }),
      })
    }),
)

/** Evaluates a package predicate through the same calls and final-value environment as defaults. */
export const evaluateParameterPredicate = (
  self: EvaluationCoordinator,
  declaration: DeclarationFacts.PackageParameterFact,
): Evaluation.Outcome<StaticValue.Value> =>
  declaration.predicate === undefined
    ? Evaluation.complete(StaticValue.boolean(true))
    : evaluateConstantValue(
        self,
        declaration,
        Location.at(declaration.predicate.anchor),
        Object.freeze([]),
        declaration.predicate,
      )

/**
 * Whether one authored block nests a static conditional or static iteration at any depth,
 * including inside match arms and effect blocks, which hold statements without being statements.
 */
const blockHasStaticControlFlow = (block: AuthoredHir.Block): boolean =>
  AuthoredWalk.statements(block).some(
    (statement) =>
      statement._tag === 'StaticConditionalStatement' || statement._tag === 'StaticForStatement',
  )

/** Whether the authored body behind one declaration fact selects on static control flow. */
const hasStaticControlFlow = (
  module: AuthoredHir.Module,
  declaration: DeclarationFacts.DeclarationFact,
): boolean => {
  const authored = AuthoredWalk.declarationOf(module, declaration.owner)
  return authored?.body._tag === 'CallableBody' && authored.body.block !== undefined
    ? blockHasStaticControlFlow(authored.body.block)
    : false
}

/** Explains static selection without re-walking one declaration for each ordinary specialization. */
export const selectionReason = (
  self: EvaluationCoordinator,
  key: ApplicationKey,
): SelectionReason | undefined => {
  if (key.staticArguments.length > 0) return 'StaticArguments'
  const declaration = declarationOf(self, key.declaration)
  if (declaration === undefined) return 'UnavailableDeclaration'
  const cache = self[stateSymbol].selectionReasons
  if (cache.has(declaration)) return cache.get(declaration)
  const reason = (): SelectionReason | undefined => {
    if (declaration.parameters.some((parameter) => parameter.phase === 'Static'))
      return 'StaticParameter'
    // The authored body decides static control flow before any elaboration result exists.
    const scope = NameResolution.scopeOf(self[stateSymbol].resolution, declaration.id.sourceId)
    if (scope !== undefined && hasStaticControlFlow(scope.context.module, declaration))
      return 'StaticControlFlow'
    const input = moduleInput(self, declaration)
    const body = input?.result.bodies.find(
      (candidate) => candidate.declaration.id.ordinal === declaration.id.ordinal,
    )
    return body === undefined ? 'UnavailableBody' : body.results.staticStructure
  }
  const selected = reason()
  cache.set(declaration, selected)
  return selected
}

/** Produces one concrete residual TIR body for a demanded runtime application. */
export const residualize = (self: Coordinator, key: ApplicationKey): Result => {
  const declaration = declarationOf(self, key.declaration)
  const input = declaration === undefined ? undefined : moduleInput(self, declaration)
  const bindings =
    declaration === undefined
      ? undefined
      : // A residual body is shared by every call that selects it, so its static text names its own
        // parameters; each selecting call site substitutes what it passed when it reports.
        bindStaticParameters(declaration, key.staticArguments)
  if (declaration === undefined || input === undefined || bindings === undefined) {
    // A declaration that is gone has no node; its module's root resolves to that module's start.
    const span = Location.at(
      declaration?.anchor ?? {
        _tag: 'AuthoredAnchor',
        owner: AuthoredIdentity.module('', key.declaration.module),
        path: [],
      },
    )
    const failure = Evaluation.phaseViolation(
      'Residualization.residualize',
      'application does not match one runtime declaration',
      span,
      Object.freeze([]),
    )
    record(
      self,
      key.declaration,
      declaration === undefined ? 'UnavailableDeclaration' : 'UnavailableBody',
      'rejected',
      true,
    )
    return Object.freeze({ _tag: 'StaticFailure', failure, diagnostics: Object.freeze([]) })
  }
  const reason = selectionReason(self, key)
  if (reason === undefined) {
    const body = input.result.bodies.find(
      (candidate) => candidate.declaration.id.ordinal === declaration.id.ordinal,
    )
    if (body !== undefined) {
      record(self, key.declaration, 'UnchangedBody', 'sourceReused', false)
      return Object.freeze({
        _tag: 'ResidualBody',
        artifact: body.artifact,
        function: body.function,
        results: body.results,
        diagnostics: Object.freeze([]),
      })
    }
  }
  const application: Evaluation.Application = Object.freeze({
    declaration: key.declaration,
    typeArguments: Object.freeze(key.typeArguments.map(Type.genericArgumentKey)),
    evidence: key.evidence,
    contractRow: key.contractRow,
    staticArguments: key.staticArguments,
    span: Location.at(declaration.anchor),
  })
  let executed = false
  const evaluated = Semantic.evaluate(
    self[stateSymbol].semantic,
    self[stateSymbol].residuals,
    application,
    (evaluation) => {
      executed = true
      const typeSubstitution = TypeInference.substitution(
        declaration.typeParameters.map((parameter) => parameter.type),
        key.typeArguments,
      )
      if (typeSubstitution === undefined)
        return Evaluation.failed(
          Evaluation.phaseViolation(
            'Residualization.residualize',
            'runtime application does not completely specialize its declaration',
            Location.at(declaration.anchor),
            evaluation.trace,
          ),
        )
      const call: Evaluation.NodeContext['call'] = (
        callee,
        arguments_,
        argumentSpans,
        argumentOrigins,
        span,
        trace,
        identity,
        lookup,
      ) =>
        evaluateStaticFunction(
          self,
          callee,
          arguments_,
          argumentSpans,
          argumentOrigins,
          span,
          trace,
          identity,
          lookup,
        )
      const constant: NonNullable<Evaluation.NodeContext['constant']> = (
        declaration,
        span,
        trace,
      ) => evaluateConstantValue(self, declaration, span, trace)
      const chargedStaticIterationNodes = { value: 0 }
      const semantic = SemanticContext.make(input.result.authored)
      const request: Tir.ArtifactId['request'] = Object.freeze({
        _tag: 'Specialize',
        application: Evaluation.applicationKey(
          self[stateSymbol].environment,
          evaluation.application,
        ),
      })
      const builder = BodyBuilder.make(Object.freeze({ owner: declaration.owner, request }))
      const analyzed = analyzeFunctionBody(
        semantic,
        declaration,
        input.declarations,
        Object.freeze({
          scope: input.scope,
          index: self[stateSymbol].index,
          semantic: self[stateSymbol].semantic,
          builder,
        }),
        Object.freeze({
          environment: self[stateSymbol].environment,
          ...(self[stateSymbol].testCatalog === undefined
            ? {}
            : { testCatalog: self[stateSymbol].testCatalog }),
          typeSubstitution,
          values: bindings.values,
          valueSpans: bindings.valueSpans,
          valueOrigins: bindings.valueOrigins,
          expressionSpans: new Map<Tir.Expression, Location.Location>(),
          expressionOrigins: new Map<Tir.Expression, Evaluation.TextOrigin>(),
          nodes: BodyBuilder.staticLowering(semantic, builder),
          lookup: (id: DeclarationFacts.CanonicalId) => lookupDeclaration(self, id),
          trace: evaluation.trace,
          call,
          chargeStaticIteration: (trace: Evaluation.Trace, residualNodes: number) => {
            const stepFailure = evaluation.stepAt(trace)
            return stepFailure ?? evaluation.growResidualAt(trace, residualNodes)
          },
          chargedStaticIterationNodes,
          reflect: (
            owner: Type.Type,
            kind: 'Type' | 'Fields',
            reflectSpan: Location.Location,
            trace: Evaluation.Trace,
            lookup: Evaluation.NodeContext['lookup'],
          ) => reflectAggregate(self, declaration, owner, kind, reflectSpan, trace, lookup),
          constant,
        }),
      )
      publishGeneratedAggregates(self, analyzed.fact.generatedAggregates)
      let nodes = 0
      Elaboration.visitStatements(analyzed.fact.statements, {
        statement: () => {
          nodes += 1
        },
        expression: () => {
          nodes += 1
        },
      })
      const remainingNodes = Math.max(0, nodes - chargedStaticIterationNodes.value)
      const growthFailure = evaluation.growResidual(
        chargedStaticIterationNodes.value === 0 ? Math.max(1, remainingNodes) : remainingNodes,
      )
      if (growthFailure !== undefined) return Evaluation.failed(growthFailure)
      const body = Elaboration.checkedBody(
        SemanticContext.make(input.result.authored),
        self[stateSymbol].index,
        analyzed.fact,
        undefined,
        request,
        builder,
      )
      if (declaration.phase === 'Static')
        throw new RangeError('Static functions have no runtime TIR body')
      return Evaluation.complete(
        Object.freeze({
          _tag: 'ResidualBody' as const,
          artifact: body.artifact,
          function: body.function,
          results: body.results,
          diagnostics: analyzed.diagnostics,
        }),
      )
    },
  )
  let branch: 'cacheReused' | 'checked' | 'rejected' = 'rejected'
  if (evaluated.cached) branch = 'cacheReused'
  else if (executed) branch = 'checked'
  record(
    self,
    key.declaration,
    reason ?? 'UnavailableBody',
    branch,
    evaluated._tag !== 'Complete' ||
      evaluated.value.diagnostics.some((diagnostic) => diagnostic.severity === 'error'),
  )
  return evaluated._tag === 'Complete'
    ? evaluated.value
    : Object.freeze({
        _tag: 'StaticFailure',
        failure: evaluated.failure,
        diagnostics: Object.freeze([]),
      })
}
