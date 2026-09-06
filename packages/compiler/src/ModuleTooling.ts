import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Elaboration from './Elaboration.js'
import type * as ModuleSemantics from './ModuleSemantics.js'
import type * as NameResolution from './NameResolution.js'
import * as Presentation from './Presentation.js'
import * as SemanticOccurrence from './SemanticOccurrence.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Type from './Type.js'

/** One available anonymous expression type cached for position fallback. */
export interface AnonymousExpression {
  readonly span: SourceSpan.SourceSpan
  readonly type: Type.Type
  readonly presentation?: Presentation.Presentation
}

/** One module's immutable editor indexes and their exact semantic input. */
export interface ModuleTooling {
  readonly _tag: 'ModuleTooling'
  readonly module: string
  readonly semantics: ModuleSemantics.ModuleSemantics
  readonly semanticOccurrences: SemanticOccurrence.ModuleIndex
  readonly anonymousExpressions: ReadonlyArray<AnonymousExpression>
}

/** Returns every expression nested under one statement in deterministic source order. */
export const statementExpressions = (
  statement: Elaboration.StatementFact,
): ReadonlyArray<Elaboration.ExpressionFact> => {
  const expressions: Array<Elaboration.ExpressionFact> = []
  Elaboration.visitStatementFacts([statement], {
    expression: (expression) => expressions.push(expression),
  })
  return Object.freeze(expressions)
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
        found.set(
          `${span.start}:${span.end}`,
          Object.freeze({
            span,
            type: expression.type.type,
            ...(expression._tag === 'CallableSection' && expression.anonymous !== undefined
              ? { presentation: Presentation.anonymousCallable(expression, expression.anonymous) }
              : {}),
          }),
        )
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
  conditions: ReadonlyArray<Elaboration.ExpressionFact> = [],
): SemanticOccurrence.ModuleIndex =>
  SemanticOccurrence.makeModule(
    semantics.module,
    semantics.elaboration,
    index,
    resolution,
    conditions,
  )

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
