import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import { analyzeExpression } from './ExpressionAnalysis.js'
import type * as Hir from './Hir.js'
import * as NameResolution from './NameResolution.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as SourceSpan from './SourceSpan.js'
import { analyzeFunctionBody } from './StatementAnalysis.js'
import * as StaticEvaluation from './StaticEvaluation.js'
import type * as StaticValue from './StaticValue.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Target from './Target.js'
import * as Type from './Type.js'

/** The specialization fields needed before an executable instance is admitted. */
export interface ApplicationKey {
  readonly declaration: DeclarationFacts.CanonicalId
  readonly typeArguments: ReadonlyArray<string>
  readonly evidence: ReadonlyArray<string>
  readonly contractRow: ReadonlyArray<string>
  readonly staticArguments: ReadonlyArray<StaticValue.Value>
}

export interface ResidualBody {
  readonly _tag: 'ResidualBody'
  readonly function: Hir.HirFunction
  readonly fact: Elaboration.FunctionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export type Result =
  | ResidualBody
  | {
      readonly _tag: 'StaticFailure'
      readonly failure: StaticEvaluation.StaticFailure
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
    }

interface State {
  readonly target: Target.Target
  readonly environment: StaticEvaluation.TargetEnvironment
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly resolution: NameResolution.Resolution
  readonly index: DeclarationIndex.Index
  readonly evaluator: StaticEvaluation.Evaluation<StaticValue.Value>
  readonly residuals: StaticEvaluation.Evaluation<ResidualBody>
  readonly staticResultOrigins: Map<string, StaticEvaluation.TextOrigin>
}

const stateSymbol: unique symbol = Symbol('Residualization.state')

/** One target-scoped residualization coordinator. */
export interface Coordinator {
  readonly _tag: 'ResidualizationCoordinator'
  readonly [stateSymbol]: State
}

export const make = (
  target: Target.Target,
  results: ReadonlyMap<string, Elaboration.Result>,
  resolution: NameResolution.Resolution,
  index: DeclarationIndex.Index,
  limits: StaticEvaluation.Limits = StaticEvaluation.defaultLimits,
): Coordinator =>
  Object.freeze({
    _tag: 'ResidualizationCoordinator',
    [stateSymbol]: {
      target,
      environment: StaticEvaluation.targetEnvironment(target),
      results,
      resolution,
      index,
      evaluator: StaticEvaluation.make<StaticValue.Value>(target, limits),
      residuals: StaticEvaluation.make<ResidualBody>(target, limits),
      staticResultOrigins: new Map<string, StaticEvaluation.TextOrigin>(),
    },
  })

const declarationOf = (
  self: Coordinator,
  identity: DeclarationFacts.CanonicalId,
): DeclarationFacts.DeclarationFact | undefined => {
  const declaration = DeclarationFacts.byCanonical(self[stateSymbol].index, identity)
  if (declaration?._tag === 'FunctionDeclaration') return declaration
  return self[stateSymbol].results
    .get(identity.module)
    ?.hir.functions.find(
      (candidate) =>
        candidate.declaration.canonical._tag === 'Canonical' &&
        candidate.declaration.canonical.id.module === identity.module &&
        candidate.declaration.canonical.id.name === identity.name,
    )?.declaration
}

const moduleInput = (
  self: Coordinator,
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
    syntax: declaration.syntax,
  })

const bindStaticParameters = (
  declaration: DeclarationFacts.DeclarationFact,
  arguments_: ReadonlyArray<StaticValue.Value>,
  argumentSpans: ReadonlyArray<SourceSpan.SourceSpan | undefined> = Object.freeze([]),
):
  | {
      readonly values: Map<string, StaticValue.Value>
      readonly valueSpans: Map<string, SourceSpan.SourceSpan>
      readonly valueOrigins: Map<string, StaticEvaluation.TextOrigin>
    }
  | undefined => {
  const parameters = declaration.parameters.filter((parameter) => parameter.phase === 'Static')
  if (parameters.length !== arguments_.length) return undefined
  const values = new Map(
    parameters.flatMap((parameter, ordinal) => {
      const value = arguments_.at(ordinal)
      return value === undefined
        ? []
        : [[StaticEvaluation.localValueKey(parameter), value] as const]
    }),
  )
  const valueSpans = new Map(
    parameters.flatMap((parameter, ordinal) => {
      const span = argumentSpans.at(ordinal)
      return span === undefined ? [] : [[StaticEvaluation.localValueKey(parameter), span] as const]
    }),
  )
  const valueOrigins = new Map(
    parameters.map(
      (parameter, ordinal) =>
        [
          StaticEvaluation.localValueKey(parameter),
          StaticEvaluation.parameterTextOrigin(ordinal),
        ] as const,
    ),
  )
  return Object.freeze({ values, valueSpans, valueOrigins })
}

const resolveTextOrigin = (
  origin: StaticEvaluation.TextOrigin | undefined,
  arguments_: ReadonlyArray<StaticEvaluation.TextOrigin | undefined>,
): StaticEvaluation.TextOrigin | undefined =>
  origin?._tag === 'ParameterTextOrigin' ? arguments_.at(origin.ordinal) : origin

const resolveTextSpan = (
  origin: StaticEvaluation.TextOrigin | undefined,
  arguments_: ReadonlyArray<SourceSpan.SourceSpan | undefined>,
): SourceSpan.SourceSpan | undefined =>
  origin?._tag === 'SourceTextOrigin' ? origin.span : arguments_.at(origin?.ordinal ?? -1)

const evaluateStaticFunction = (
  self: Coordinator,
  declaration: DeclarationFacts.DeclarationFact,
  arguments_: ReadonlyArray<StaticValue.Value>,
  argumentSpans: ReadonlyArray<SourceSpan.SourceSpan | undefined>,
  argumentOrigins: ReadonlyArray<StaticEvaluation.TextOrigin | undefined>,
  span: Parameters<StaticEvaluation.FactEvaluationContext['call']>[4],
  parentTrace: StaticEvaluation.Trace,
  identity: Parameters<StaticEvaluation.FactEvaluationContext['call']>[6],
): StaticEvaluation.FactCallResult => {
  if (declaration.canonical._tag !== 'Canonical')
    return Object.freeze({
      outcome: StaticEvaluation.failed(
        StaticEvaluation.phaseViolation(
          'StaticEvaluation.call',
          'static callee has no canonical identity',
          span,
          parentTrace,
        ),
      ),
    })
  const application: StaticEvaluation.Application = Object.freeze({
    declaration: declaration.canonical.id,
    typeArguments: identity.typeArguments,
    evidence: identity.evidence,
    contractRow: identity.contractRow,
    staticArguments: arguments_,
    span,
  })
  const result = StaticEvaluation.evaluateApplicationFrom(
    self[stateSymbol].evaluator,
    application,
    parentTrace,
    (evaluation) => {
      const input = moduleInput(self, declaration)
      const bindings = bindStaticParameters(declaration, arguments_, argumentSpans)
      if (input === undefined || bindings === undefined)
        return StaticEvaluation.failed(
          StaticEvaluation.phaseViolation(
            'StaticEvaluation.call',
            'static application does not match its declaration',
            span,
            evaluation.trace,
          ),
        )
      const call: StaticEvaluation.FactEvaluationContext['call'] = (
        callee,
        nestedArguments,
        nestedArgumentSpans,
        nestedArgumentOrigins,
        callSpan,
        trace,
        nestedIdentity,
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
        )
      const staticContext = {
        environment: self[stateSymbol].environment,
        values: bindings.values,
        valueSpans: bindings.valueSpans,
        valueOrigins: bindings.valueOrigins,
        expressionSpans: new Map<Elaboration.ExpressionFact, SourceSpan.SourceSpan>(),
        expressionOrigins: new Map<Elaboration.ExpressionFact, StaticEvaluation.TextOrigin>(),
        returnedTextSpan: { value: undefined },
        returnedTextOrigin: { value: undefined },
        trace: evaluation.trace,
        call,
        constant: (
          constant: DeclarationFacts.ConstantFact,
          constantSpan: SourceSpan.SourceSpan,
          trace: StaticEvaluation.Trace,
        ) => evaluateConstantValue(self, constant, constantSpan, trace),
      }
      const analyzed = analyzeFunctionBody(
        input.result.syntax.source,
        declaration,
        input.declarations,
        Object.freeze({ scope: input.scope, index: self[stateSymbol].index }),
        staticContext,
      )
      let nestedStaticFailure: StaticEvaluation.StaticFailure | undefined
      Elaboration.visitStatementFacts(analyzed.fact.statements, {
        expression: (expression) => {
          if (
            nestedStaticFailure === undefined &&
            expression._tag === 'Call' &&
            expression.staticFailure !== undefined
          )
            nestedStaticFailure = expression.staticFailure
        },
      })
      if (nestedStaticFailure !== undefined) return StaticEvaluation.failed(nestedStaticFailure)
      const firstError = analyzed.diagnostics.find((diagnostic) => diagnostic.severity === 'error')
      if (firstError !== undefined)
        return StaticEvaluation.failed(
          StaticEvaluation.phaseViolation(
            'StaticEvaluation.call',
            firstError.message,
            firstError.span,
            evaluation.trace,
          ),
        )
      const value = StaticEvaluation.evaluateStatements(analyzed.fact.statements, {
        ...staticContext,
        step: () => evaluation.step(),
      })
      if (value._tag === 'Complete') {
        if (staticContext.returnedTextOrigin.value !== undefined)
          self[stateSymbol].staticResultOrigins.set(
            StaticEvaluation.applicationKey(self[stateSymbol].environment, evaluation.application),
            staticContext.returnedTextOrigin.value,
          )
        const retained = evaluation.retain(value.value)
        if (retained !== undefined) return StaticEvaluation.failed(retained)
      }
      return value
    },
  )
  if (result._tag === 'Failed')
    return Object.freeze({ outcome: StaticEvaluation.failed(result.failure) })
  const cachedOrigin = self[stateSymbol].staticResultOrigins.get(result.key)
  const textOrigin = resolveTextOrigin(cachedOrigin, argumentOrigins)
  const textSpan = resolveTextSpan(cachedOrigin, argumentSpans)
  return Object.freeze({
    outcome: StaticEvaluation.complete(result.value),
    ...(textSpan === undefined ? {} : { textSpan }),
    ...(textOrigin === undefined ? {} : { textOrigin }),
  })
}

const staticValueType = (value: StaticValue.Value): Type.Type | undefined => {
  if (value._tag === 'BooleanValue') return 'bool'
  if (value._tag === 'CharacterValue') return 'char'
  if (value._tag === 'IntegerValue' || value._tag === 'FloatValue') return value.type
  if (value._tag === 'TextValue') return Type.string
  return undefined
}

function evaluateConstantValue(
  self: Coordinator,
  declaration: DeclarationFacts.ConstantFact,
  span: SourceSpan.SourceSpan,
  parentTrace: StaticEvaluation.Trace,
): StaticEvaluation.Outcome<StaticValue.Value> {
  if (declaration.canonical._tag !== 'Canonical')
    return StaticEvaluation.failed(
      StaticEvaluation.phaseViolation(
        'StaticEvaluation.constant',
        'constant has no canonical identity',
        span,
        parentTrace,
      ),
    )
  const application: StaticEvaluation.Application = Object.freeze({
    declaration: declaration.canonical.id,
    typeArguments: Object.freeze([]),
    evidence: Object.freeze([]),
    contractRow: Object.freeze([]),
    staticArguments: Object.freeze([]),
    span,
  })
  const result = StaticEvaluation.evaluateApplicationFrom(
    self[stateSymbol].evaluator,
    application,
    parentTrace,
    (evaluation) => {
      const input = moduleInput(self, declaration)
      if (input === undefined || declaration.declaredType._tag !== 'Resolved')
        return StaticEvaluation.failed(
          StaticEvaluation.phaseViolation(
            'StaticEvaluation.constant',
            'constant declaration is unavailable',
            declaration.initializer.span,
            evaluation.trace,
          ),
        )
      const call: StaticEvaluation.FactEvaluationContext['call'] = (
        callee,
        arguments_,
        argumentSpans,
        argumentOrigins,
        callSpan,
        trace,
        identity,
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
        )
      const constant: NonNullable<StaticEvaluation.FactEvaluationContext['constant']> = (
        nested,
        nestedSpan,
        trace,
      ) => evaluateConstantValue(self, nested, nestedSpan, trace)
      const staticContext = Object.freeze({
        environment: self[stateSymbol].environment,
        values: new Map<string, StaticValue.Value>(),
        valueSpans: new Map<string, SourceSpan.SourceSpan>(),
        valueOrigins: new Map<string, StaticEvaluation.TextOrigin>(),
        expressionSpans: new Map<Elaboration.ExpressionFact, SourceSpan.SourceSpan>(),
        expressionOrigins: new Map<Elaboration.ExpressionFact, StaticEvaluation.TextOrigin>(),
        trace: evaluation.trace,
        call,
        constant,
      })
      const analyzed = analyzeExpression(
        input.result.syntax.source,
        declaration.initializer,
        input.declarations,
        constantHost(declaration),
        Object.freeze({
          parameters: Object.freeze([]),
          bindings: Object.freeze([]),
          patternBindings: Object.freeze([]),
        }),
        Object.freeze({ scope: input.scope, index: self[stateSymbol].index, staticContext }),
        declaration.declaredType.type,
      )
      const firstError = analyzed?.diagnostics.find((diagnostic) => diagnostic.severity === 'error')
      if (analyzed === undefined || firstError !== undefined)
        return StaticEvaluation.failed(
          StaticEvaluation.phaseViolation(
            'StaticEvaluation.constant',
            firstError?.message ?? 'constant initializer cannot be analyzed',
            firstError?.span ?? declaration.initializer.span,
            evaluation.trace,
          ),
        )
      const value = StaticEvaluation.evaluateFact(analyzed.fact, {
        ...staticContext,
        step: () => evaluation.step(),
      })
      if (value._tag === 'Failed') return value
      const actual = staticValueType(value.value)
      if (actual === undefined || !Type.equals(actual, declaration.declaredType.type))
        return StaticEvaluation.failed(
          StaticEvaluation.phaseViolation(
            'StaticEvaluation.constant',
            `initializer produced ${actual === undefined ? 'an unsupported aggregate' : Type.display(actual)} instead of ${Type.display(declaration.declaredType.type)}`,
            declaration.initializer.span,
            evaluation.trace,
          ),
        )
      const retained = evaluation.retain(value.value)
      return retained === undefined ? value : StaticEvaluation.failed(retained)
    },
  )
  return result._tag === 'Complete'
    ? StaticEvaluation.complete(result.value)
    : StaticEvaluation.failed(result.failure)
}

/** Evaluates one explicitly typed primitive constant for this coordinator's selected target. */
export const evaluateConstant = (
  self: Coordinator,
  declaration: DeclarationFacts.ConstantFact,
): StaticEvaluation.Outcome<StaticValue.Value> =>
  evaluateConstantValue(self, declaration, declaration.initializer.span, Object.freeze([]))

const containsSyntaxKind = (node: SyntaxTree.Node, kind: SyntaxTree.NodeKind): boolean =>
  node.kind === kind ||
  node.children.some((child) => SyntaxTree.isNode(child) && containsSyntaxKind(child, kind))

/**
 * Reports whether one runtime application needs a target/static-argument-specific body. Ordinary
 * functions retain their already-elaborated HIR so residualization cannot perturb unrelated
 * wrapper inlining, capture identities, or ownership facts.
 */
export const requiresSelection = (self: Coordinator, key: ApplicationKey): boolean => {
  const declaration = declarationOf(self, key.declaration)
  const input = declaration === undefined ? undefined : moduleInput(self, declaration)
  const fact = input?.result.functions.find(
    (candidate) => candidate.declaration.id.ordinal === declaration?.id.ordinal,
  )
  if (declaration === undefined) return true
  if (
    key.staticArguments.length > 0 ||
    declaration.parameters.some((parameter) => parameter.phase === 'Static') ||
    containsSyntaxKind(declaration.syntax, 'StaticConditionalStatement')
  )
    return true
  if (fact === undefined) return false
  let required = false
  Elaboration.visitStatementFacts(fact.statements, {
    statement: (statement) => {
      if (statement._tag === 'BindStatement' && statement.binding.phase === 'Static')
        required = true
    },
    expression: (expression) => {
      if (
        (expression._tag === 'Constant' && expression.value === undefined) ||
        expression._tag === 'CompileError' ||
        (expression._tag === 'Call' &&
          expression.reference._tag === 'Resolved' &&
          (expression.reference.declaration.phase === 'Static' ||
            expression.reference.declaration.parameters.some(
              (parameter) => parameter.phase === 'Static',
            )))
      )
        required = true
    },
  })
  return required
}

/** Produces one concrete residual HIR body for a demanded runtime application. */
export const residualize = (self: Coordinator, key: ApplicationKey): Result => {
  const declaration = declarationOf(self, key.declaration)
  const input = declaration === undefined ? undefined : moduleInput(self, declaration)
  const bindings =
    declaration === undefined ? undefined : bindStaticParameters(declaration, key.staticArguments)
  if (declaration === undefined || input === undefined || bindings === undefined) {
    const fallbackSource = self[stateSymbol].results.get(key.declaration.module)?.syntax.source
    const span =
      declaration?.syntax.span ??
      (fallbackSource === undefined
        ? undefined
        : SourceSpan.make(fallbackSource, 0, 0).pipe((value) =>
            value._tag === 'Some' ? value.value : undefined,
          ))
    if (span === undefined)
      throw new RangeError(`Residualization lost source ${key.declaration.module}`)
    const failure = StaticEvaluation.phaseViolation(
      'Residualization.residualize',
      'application does not match one runtime declaration',
      span,
      Object.freeze([]),
    )
    return Object.freeze({ _tag: 'StaticFailure', failure, diagnostics: Object.freeze([]) })
  }
  if (!requiresSelection(self, key)) {
    const fact = input.result.functions.find(
      (candidate) => candidate.declaration.id.ordinal === declaration.id.ordinal,
    )
    const fn = input.result.hir.functions.find(
      (candidate) =>
        candidate.declaration.canonical._tag === 'Canonical' &&
        candidate.declaration.canonical.id.module === key.declaration.module &&
        candidate.declaration.canonical.id.name === key.declaration.name,
    )
    if (fact !== undefined && fn !== undefined)
      return Object.freeze({
        _tag: 'ResidualBody',
        function: fn,
        fact,
        diagnostics: Object.freeze([]),
      })
  }
  const application: StaticEvaluation.Application = Object.freeze({
    declaration: key.declaration,
    typeArguments: key.typeArguments,
    evidence: key.evidence,
    contractRow: key.contractRow,
    staticArguments: key.staticArguments,
    span: declaration.syntax.span,
  })
  const evaluated = StaticEvaluation.evaluateApplication(
    self[stateSymbol].residuals,
    application,
    (evaluation) => {
      const call: StaticEvaluation.FactEvaluationContext['call'] = (
        callee,
        arguments_,
        argumentSpans,
        argumentOrigins,
        span,
        trace,
        identity,
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
        )
      const constant: NonNullable<StaticEvaluation.FactEvaluationContext['constant']> = (
        declaration,
        span,
        trace,
      ) => evaluateConstantValue(self, declaration, span, trace)
      const analyzed = analyzeFunctionBody(
        input.result.syntax.source,
        declaration,
        input.declarations,
        Object.freeze({ scope: input.scope, index: self[stateSymbol].index }),
        Object.freeze({
          environment: self[stateSymbol].environment,
          values: bindings.values,
          valueSpans: bindings.valueSpans,
          valueOrigins: bindings.valueOrigins,
          expressionSpans: new Map<Elaboration.ExpressionFact, SourceSpan.SourceSpan>(),
          expressionOrigins: new Map<Elaboration.ExpressionFact, StaticEvaluation.TextOrigin>(),
          trace: evaluation.trace,
          call,
          constant,
        }),
      )
      let nodes = 0
      Elaboration.visitStatementFacts(analyzed.fact.statements, {
        statement: () => {
          nodes += 1
        },
        expression: () => {
          nodes += 1
        },
      })
      const growthFailure = evaluation.growResidual(Math.max(1, nodes))
      if (growthFailure !== undefined) return StaticEvaluation.failed(growthFailure)
      return StaticEvaluation.complete(
        Object.freeze({
          _tag: 'ResidualBody' as const,
          function: Elaboration.residualHirFunction(analyzed.fact),
          fact: analyzed.fact,
          diagnostics: analyzed.diagnostics,
        }),
      )
    },
  )
  return evaluated._tag === 'Complete'
    ? evaluated.value
    : Object.freeze({
        _tag: 'StaticFailure',
        failure: evaluated.failure,
        diagnostics: Object.freeze([]),
      })
}
