import type * as AuthoredLowering from './AuthoredLowering.js'
import * as BodyQuery from './BodyQuery.js'
import * as CompilerTrace from './CompilerTrace.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Elaboration from './Elaboration.js'
import * as Evaluation from './Evaluation.js'
import type * as ExpressionAnalysis from './ExpressionAnalysis.js'
import * as NameResolution from './NameResolution.js'
import * as SemanticQuery from './SemanticQuery.js'
import * as SemanticContext from './SemanticContext.js'
import * as StatementAnalysis from './StatementAnalysis.js'

/** The sealed semantic reader for one immutable closure/selection epoch. */
export interface Session {
  readonly _tag: 'SemanticSession'
  readonly epoch: string
  readonly configuration: string
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly queries: SemanticQuery.Session
}

/** Builds one fresh query session; answer maps are never transferred between epochs. */
export const makeSession = (
  epoch: string,
  index: DeclarationIndex.Index,
  resolution: NameResolution.Resolution,
  configuration = 'default',
): Session =>
  Object.freeze({
    _tag: 'SemanticSession',
    epoch,
    configuration,
    index,
    resolution,
    queries: SemanticQuery.make(`${epoch}\u0000${configuration}`),
  })

const declarationKey = (id: DeclarationFacts.CanonicalId): string => `${id.module}.${id.name}`

const bindingKey = (binding: NameResolution.Binding): string => {
  switch (binding._tag) {
    case 'IntrinsicActor':
      return `intrinsic:${binding.spelling}`
    case 'ModuleNamespace':
      return `module:${binding.module}`
    case 'LocalDeclaration':
    case 'ImportedMember':
      return `declaration:${declarationKey(binding.declaration)}`
    case 'Unavailable':
      return `unavailable:${binding.spelling}`
  }
}

const observeConfiguration = (self: Session, observe: SemanticQuery.Observe): void =>
  observe(
    Object.freeze({
      _tag: 'Configuration',
      key: 'semantic-session',
      value: self.configuration,
    }),
  )

const observeLookup = (
  self: Session,
  scope: NameResolution.ModuleScope,
  spelling: string,
  result: NameResolution.Lookup,
  observe: SemanticQuery.Observe,
): void => {
  observeConfiguration(self, observe)
  observe(
    Object.freeze({
      _tag: 'Namespace',
      module: scope.module,
      spelling,
      candidates: Object.freeze([
        ...scope.bindings.filter((binding) => binding.spelling === spelling).map(bindingKey),
        ...scope.conflicts
          .filter((conflict) => conflict.spelling === spelling)
          .flatMap((conflict) => conflict.bindings.map(bindingKey)),
      ]),
    }),
  )
  const binding = scope.bindings.find((candidate) => candidate.spelling === spelling)
  if (binding?._tag === 'ImportedMember' || binding?._tag === 'ModuleNamespace')
    observe(
      Object.freeze({
        _tag: 'ImportSelection',
        module: scope.module,
        binding: bindingKey(binding),
      }),
    )
  if (result._tag === 'Resolved' && result.declaration.canonical._tag === 'Canonical')
    observe(
      Object.freeze({
        _tag: 'Header',
        declaration: declarationKey(result.declaration.canonical.id),
      }),
    )
}

const scopeKey = (scope: NameResolution.ModuleScope): string =>
  `${scope.module}:${scope.bindings.map(bindingKey).join(',')}:${scope.conflicts
    .map((conflict) => `${conflict.spelling}=${conflict.bindings.map(bindingKey).join('|')}`)
    .join(',')}`

const nameRequest = (
  self: Session,
  scope: NameResolution.ModuleScope,
  spelling: string,
): SemanticQuery.Request<NameResolution.Lookup> =>
  Object.freeze({
    _tag: 'SemanticQueryRequest',
    key: `ResolveName:${scopeKey(scope)}:${spelling}`,
    execute: (observe: SemanticQuery.Observe) => {
      const result = NameResolution.lookup(scope, self.index, spelling)
      observeLookup(self, scope, spelling, result, observe)
      return result
    },
  })

/** Resolves one name through the session's authoritative observed provider. */
export const resolveName = (
  self: Session,
  scope: NameResolution.ModuleScope,
  spelling: string,
): NameResolution.Lookup => {
  const result = SemanticQuery.query(self.queries, nameRequest(self, scope, spelling))
  return result._tag === 'Completed'
    ? result.completed.answer
    : Object.freeze({ _tag: 'Unavailable', spelling })
}

/** Executes the name provider without consulting or publishing the completed-answer store. */
export const resolveNameFresh = (
  self: Session,
  scope: NameResolution.ModuleScope,
  spelling: string,
): NameResolution.Lookup => {
  const result = SemanticQuery.fresh(self.queries, nameRequest(self, scope, spelling))
  return result._tag === 'Completed'
    ? result.completed.answer
    : Object.freeze({ _tag: 'Unavailable', spelling })
}

/** Resolves a qualified path while preserving the unqualified qualifier query edge. */
export const resolveQualifiedName = (
  self: Session,
  scope: NameResolution.ModuleScope,
  namespace: string,
  member: string,
  anchor: import('./AuthoredHir.js').Anchor,
): NameResolution.Lookup => {
  resolveName(self, scope, namespace)
  const key = `ResolveName:${scopeKey(scope)}:${namespace}.${member}`
  const result = SemanticQuery.query(
    self.queries,
    Object.freeze({
      _tag: 'SemanticQueryRequest',
      key,
      execute: (observe: SemanticQuery.Observe) => {
        const answer = NameResolution.lookupQualified(scope, self.index, namespace, member, anchor)
        observeLookup(self, scope, namespace, answer, observe)
        return answer
      },
    }),
  )
  return result._tag === 'Completed'
    ? result.completed.answer
    : Object.freeze({ _tag: 'Unavailable', spelling: member })
}

/** Resolves an inherent or contract-associated member through the declaration query session. */
export const resolveAssociatedName = (
  self: Session,
  owner: DeclarationFacts.MemberFact,
  member: string,
  requestingModule: string,
): ReturnType<typeof NameResolution.lookupAssociated> => {
  const ownerId =
    owner.canonical._tag === 'Canonical'
      ? declarationKey(owner.canonical.id)
      : `${owner.id.sourceId}:${owner.id.ordinal}`
  const result = SemanticQuery.query(
    self.queries,
    Object.freeze({
      _tag: 'SemanticQueryRequest',
      key: `ResolveAssociatedName:${ownerId}:${requestingModule}:${member}`,
      execute: (observe: SemanticQuery.Observe) => {
        observeConfiguration(self, observe)
        observe(Object.freeze({ _tag: 'Header', declaration: ownerId }))
        const answer = NameResolution.lookupAssociated(self.index, owner, member, requestingModule)
        observe(
          Object.freeze({
            _tag: 'Query',
            key: `AssociatedCandidate:${ownerId}:${member}:${answer._tag}`,
          }),
        )
        return answer
      },
    }),
  )
  return result._tag === 'Completed' ? result.completed.answer : Object.freeze({ _tag: 'Missing' })
}

/** The public header fact addressed by `Semantic.typeOf`. */
export type TypeOf =
  | { readonly _tag: 'Resolved'; readonly declaration: DeclarationFacts.MemberFact }
  | { readonly _tag: 'Unavailable'; readonly declaration: DeclarationFacts.CanonicalId }

const typeRequest = (
  self: Session,
  declaration: DeclarationFacts.CanonicalId,
): SemanticQuery.Request<TypeOf> =>
  Object.freeze({
    _tag: 'SemanticQueryRequest',
    key: `TypeOf:${declarationKey(declaration)}`,
    execute: (observe: SemanticQuery.Observe) => {
      observeConfiguration(self, observe)
      observe(Object.freeze({ _tag: 'Header', declaration: declarationKey(declaration) }))
      const fact = DeclarationFacts.byCanonical(self.index, declaration)
      if (fact === undefined) return Object.freeze({ _tag: 'Unavailable', declaration })
      if (fact._tag === 'AliasDeclaration')
        observe(Object.freeze({ _tag: 'Alias', declaration: declarationKey(declaration) }))
      const headers = self.index.modules.find((module) => module.module === declaration.module)
      for (const conformance of headers?.conformances ?? [])
        observe(
          Object.freeze({
            _tag: 'Conformance',
            key: `${declaration.module}:${conformance.ordinal}`,
          }),
        )
      return Object.freeze({ _tag: 'Resolved', declaration: fact })
    },
  })

/** Reads one named declaration's completed public header/signature without demanding its body. */
export const typeOf = (self: Session, declaration: DeclarationFacts.CanonicalId): TypeOf => {
  const result = SemanticQuery.query(self.queries, typeRequest(self, declaration))
  return result._tag === 'Completed'
    ? result.completed.answer
    : Object.freeze({ _tag: 'Unavailable', declaration })
}

/** Executes the declared-type provider without consulting or publishing completed answers. */
export const typeOfFresh = (self: Session, declaration: DeclarationFacts.CanonicalId): TypeOf => {
  const result = SemanticQuery.fresh(self.queries, typeRequest(self, declaration))
  return result._tag === 'Completed'
    ? result.completed.answer
    : Object.freeze({ _tag: 'Unavailable', declaration })
}

/** Returns the immutable observations of a completed request for reuse validation and inspection. */
export const observations = (
  self: Session,
  key: string,
): ReadonlyArray<SemanticQuery.Observation> =>
  SemanticQuery.completed(self.queries, key)?.observations ?? Object.freeze([])

/** Inputs shared by fresh and reusable declaration-body checking. */
export interface BodyInput {
  readonly query?: BodyQuery.BodyQuery
  readonly authored: AuthoredLowering.Lowered
  readonly headers: DeclarationFacts.ModuleHeaders
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
  readonly session: Session
  readonly declaration: DeclarationFacts.DeclarationFact
  readonly trace?: CompilerTrace.CompilerTrace
}

/** Checks or reuses one declaration body and returns its complete checked unit. */
export const checkBody = (input: BodyInput): Elaboration.CheckedUnit => {
  const trace = input.trace ?? CompilerTrace.none
  const context = SemanticContext.make(input.authored)
  const build = (): BodyQuery.Built => {
    const hiddenFunctions: Array<ExpressionAnalysis.FunctionAnalysis> = []
    const analysis = StatementAnalysis.analyzeFunctionBody(
      context,
      input.declaration,
      input.headers.declarations,
      Object.freeze({
        semantic: input.session,
        scope: input.scope,
        index: input.index,
        hiddenFunctions,
      }),
    )
    const own = Elaboration.checkedBody(
      context,
      input.index,
      analysis.fact,
      undefined,
      undefined,
      analysis.builder,
    )
    return {
      unit: Object.freeze({
        bodies: Object.freeze([
          own,
          ...hiddenFunctions.map((hidden) =>
            Elaboration.checkedBody(
              context,
              input.index,
              hidden.fact,
              own.artifact,
              undefined,
              hidden.builder,
            ),
          ),
        ]),
        diagnostics: analysis.diagnostics,
      }),
    }
  }
  const request: SemanticQuery.Request<Elaboration.CheckedUnit> = Object.freeze({
    _tag: 'SemanticQueryRequest',
    key: `CheckBody:${input.declaration.owner.module}:${input.declaration.id.ordinal}`,
    execute: (observe: SemanticQuery.Observe) => {
      observeConfiguration(input.session, observe)
      observe(
        Object.freeze({
          _tag: 'Header',
          declaration: `${input.declaration.owner.module}:${input.declaration.id.ordinal}`,
        }),
      )
      return input.query === undefined
        ? trace('Semantic.checkBody.execute', () => build().unit)
        : BodyQuery.check(
            input.query,
            context,
            input.authored,
            input.scope,
            input.declaration,
            build,
            trace,
          )
    },
  })
  return trace(
    'Semantic.checkBody',
    () => {
      const result = SemanticQuery.query(input.session.queries, request)
      if (result._tag === 'Cycle')
        throw new RangeError(`Recursive body query: ${result.cycle.path.join(' -> ')}`)
      return result.completed.answer
    },
    {
      module: input.declaration.owner.module,
      declaration: input.declaration.id.ordinal,
    },
  )
}

/** Runs the body provider without consulting or publishing this session's completed answer. */
export const checkBodyFresh = (input: BodyInput): Elaboration.CheckedUnit => {
  const isolated = makeSession(
    `${input.session.epoch}:fresh`,
    input.session.index,
    input.session.resolution,
    input.session.configuration,
  )
  const { query: _query, ...fresh } = input
  return checkBody({ ...fresh, session: isolated })
}

/** Evaluates one value-sensitive static application through its existing target-scoped store. */
export const evaluate = <A>(
  self: Session,
  evaluation: Evaluation.Evaluation<A>,
  application: Evaluation.Application,
  callback: Evaluation.EvaluationCallback<A>,
): Evaluation.ApplicationResult<A> => {
  const key = Evaluation.applicationKey(evaluation.environment, application)
  SemanticQuery.observe(self.queries, Object.freeze({ _tag: 'Query', key: `Evaluate:${key}` }))
  return Evaluation.evaluate(evaluation, application, callback)
}

/** Evaluates a nested static application while preserving its source-level parent trace. */
export const evaluateFrom = <A>(
  self: Session,
  evaluation: Evaluation.Evaluation<A>,
  application: Evaluation.Application,
  parentTrace: Evaluation.Trace,
  callback: Evaluation.EvaluationCallback<A>,
): Evaluation.ApplicationResult<A> => {
  const key = Evaluation.applicationKey(evaluation.environment, application)
  SemanticQuery.observe(self.queries, Object.freeze({ _tag: 'Query', key: `Evaluate:${key}` }))
  return Evaluation.evaluateFrom(evaluation, application, parentTrace, callback)
}
