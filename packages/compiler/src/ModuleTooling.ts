import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Elaboration from './Elaboration.js'
import type * as ModuleSemantics from './ModuleSemantics.js'
import type * as NameResolution from './NameResolution.js'
import * as SemanticOccurrence from './SemanticOccurrence.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Type from './Type.js'

/** One available anonymous expression type cached for position fallback. */
export interface AnonymousExpression {
  readonly span: SourceSpan.SourceSpan
  readonly type: Type.Type
}

/** One module's immutable editor indexes and their exact semantic input. */
export interface ModuleTooling {
  readonly _tag: 'ModuleTooling'
  readonly module: string
  readonly semantics: ModuleSemantics.ModuleSemantics
  readonly semanticOccurrences: SemanticOccurrence.ModuleIndex
  readonly anonymousExpressions: ReadonlyArray<AnonymousExpression>
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
      case 'EffectBindRequirement':
        return [expression.protected]
      case 'CallableSection':
        return expression.captures.map((capture) => capture.expression)
      case 'CallableApply':
        return [expression.callee, ...expression.arguments.map((argument) => argument.expression)]
      case 'Operator':
      case 'ShortCircuit':
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
    case 'ExpressionStatement':
      return nestedExpressions(statement.expression)
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

/** Builds one module's anonymous-expression entries. */
export const anonymousExpressionIndex = (
  semantics: ModuleSemantics.ModuleSemantics,
): ReadonlyArray<AnonymousExpression> => {
  const found = new Map<string, AnonymousExpression>()
  for (const fn of semantics.elaboration.functions)
    for (const statement of fn.statements)
      for (const expression of statementExpressions(statement)) {
        if (expression.type._tag !== 'Available') continue
        const span = SyntaxTree.span(expression.syntax)
        found.set(`${span.start}:${span.end}`, Object.freeze({ span, type: expression.type.type }))
      }
  return Object.freeze(
    [...found.values()].sort(
      (left, right) =>
        left.span.start - right.span.start ||
        left.span.end - left.span.start - (right.span.end - right.span.start),
    ),
  )
}

/** Builds one module's semantic-occurrence index. */
export const semanticOccurrenceIndex = (
  semantics: ModuleSemantics.ModuleSemantics,
  index: DeclarationIndex.Index,
  resolution: NameResolution.Resolution,
): SemanticOccurrence.ModuleIndex =>
  SemanticOccurrence.makeModule(semantics.module, semantics.elaboration, index, resolution)

/** Builds one module's editor indexes from one closed semantic artifact. */
export const make = (
  semantics: ModuleSemantics.ModuleSemantics,
  index: DeclarationIndex.Index,
  resolution: NameResolution.Resolution,
): ModuleTooling =>
  Object.freeze({
    _tag: 'ModuleTooling',
    module: semantics.module,
    semantics,
    semanticOccurrences: semanticOccurrenceIndex(semantics, index, resolution),
    anonymousExpressions: anonymousExpressionIndex(semantics),
  })

/** Closes already-built module indexes into one reusable tooling artifact. */
export const fromIndexes = (
  semantics: ModuleSemantics.ModuleSemantics,
  semanticOccurrences: SemanticOccurrence.ModuleIndex,
  anonymousExpressions: ReadonlyArray<AnonymousExpression>,
): ModuleTooling =>
  Object.freeze({
    _tag: 'ModuleTooling',
    module: semantics.module,
    semantics,
    semanticOccurrences,
    anonymousExpressions,
  })
