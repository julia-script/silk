import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Elaboration from './Elaboration.js'
import type * as NameResolution from './NameResolution.js'
import * as PhaseReport from './PhaseReport.js'
import * as SemanticOccurrence from './SemanticOccurrence.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Type from './Type.js'

/** One available anonymous expression type cached for position fallback. */
export interface AnonymousExpression {
  readonly span: SourceSpan.SourceSpan
  readonly type: Type.Type
}

/** Tooling indexes and appended observations derived once from frontend semantic facts. */
export interface FrontendTooling {
  readonly semanticOccurrences: SemanticOccurrence.Index
  readonly anonymousExpressions: ReadonlyMap<string, ReadonlyArray<AnonymousExpression>>
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
}

const nestedExpressions = (
  expression: Elaboration.ExpressionFact,
): ReadonlyArray<Elaboration.ExpressionFact> => {
  const nested: ReadonlyArray<Elaboration.ExpressionFact> = (() => {
    switch (expression._tag) {
      case 'Move':
      case 'Borrow':
      case 'FieldProjection':
        return [expression.subject]
      case 'PlaceReplace':
        return [expression.destination, expression.value]
      case 'IndexProjection':
        return [expression.subject, expression.index]
      case 'ArrayLiteral':
        return expression.elements.map((element) => element.expression)
      case 'StructLiteral':
        return expression.initializers.map((initializer) => initializer.expression)
      case 'Grouped':
        return [expression.expression]
      case 'Run':
        return [expression.subject]
      case 'EffectCatch':
        return [expression.protected, expression.handler]
      case 'EffectRetry':
        return [expression.protected, expression.retries]
      case 'EffectProvide':
        return [expression.protected]
      case 'EffectProvideWith':
        return [expression.protected, expression.acquisition]
      case 'EffectTransform':
        return [expression.protected, expression.callback]
      case 'CallableSection':
        return expression.captures.map((capture) => capture.expression)
      case 'CallableApply':
        return [expression.callee, ...expression.arguments.map((argument) => argument.expression)]
      case 'Operator':
      case 'Call':
        return expression.arguments.map((argument) => argument.expression)
      case 'Match':
        return [
          expression.scrutinee,
          ...expression.arms.flatMap((arm) => [
            ...(arm.guard === undefined ? [] : [arm.guard]),
            arm.result,
          ]),
        ]
      default:
        return []
    }
  })()
  return Object.freeze([expression, ...nested.flatMap((candidate) => nestedExpressions(candidate))])
}

/** Returns every expression nested under one statement in deterministic source order. */
export const statementExpressions = (
  statement: Elaboration.StatementFact,
): ReadonlyArray<Elaboration.ExpressionFact> => {
  switch (statement._tag) {
    case 'UnsafeStatement':
      return Object.freeze(statement.statements.flatMap(statementExpressions))
    case 'BindStatement':
      return nestedExpressions(statement.binding.initializer)
    case 'ReturnStatement':
      return nestedExpressions(statement.expression)
    case 'FailStatement':
    case 'DropStatement':
      return nestedExpressions(statement.expression)
    case 'IfStatement':
      return Object.freeze([
        ...nestedExpressions(statement.condition),
        ...statement.taken.flatMap(statementExpressions),
        ...statement.otherwise.flatMap(statementExpressions),
      ])
    case 'WriteStatement':
      return Object.freeze([
        ...nestedExpressions(statement.destination),
        ...nestedExpressions(statement.value),
      ])
    case 'WhileStatement':
      return Object.freeze([
        ...nestedExpressions(statement.condition),
        ...statement.body.flatMap(statementExpressions),
      ])
    case 'BreakStatement':
    case 'ContinueStatement':
      return Object.freeze([])
  }
}

const anonymousExpressionIndex = (
  results: ReadonlyMap<string, Elaboration.Result>,
): ReadonlyMap<string, ReadonlyArray<AnonymousExpression>> => {
  const modules = new Map<string, ReadonlyArray<AnonymousExpression>>()
  for (const [module, result] of results) {
    const found = new Map<string, AnonymousExpression>()
    for (const fn of result.functions)
      for (const statement of fn.statements)
        for (const expression of statementExpressions(statement)) {
          if (expression.type._tag !== 'Available') continue
          const span = SyntaxTree.span(expression.syntax)
          found.set(
            `${span.start}:${span.end}`,
            Object.freeze({ span, type: expression.type.type }),
          )
        }
    modules.set(
      module,
      Object.freeze(
        [...found.values()].sort(
          (left, right) =>
            left.span.start - right.span.start ||
            left.span.end - left.span.start - (right.span.end - right.span.start),
        ),
      ),
    )
  }
  return modules
}

/** Builds compiler-owned editor indexes once for one completed frontend. */
export const make = (frontend: {
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
}): FrontendTooling => {
  const report = [...frontend.report]
  const occurrences = PhaseReport.measure(
    'semantic-occurrences',
    frontend.results.size,
    () => SemanticOccurrence.make(frontend.results, frontend.index, frontend.resolution),
    (value) =>
      [...value.modules.values()].reduce((sum, module) => sum + module.occurrences.length, 0),
  )
  report.push(occurrences.report)
  const expressions = PhaseReport.measure(
    'anonymous-expressions',
    frontend.results.size,
    () => anonymousExpressionIndex(frontend.results),
    (value) => [...value.values()].reduce((sum, entries) => sum + entries.length, 0),
  )
  report.push(expressions.report)
  return Object.freeze({
    semanticOccurrences: occurrences.value,
    anonymousExpressions: expressions.value,
    report: Object.freeze(report),
  })
}
