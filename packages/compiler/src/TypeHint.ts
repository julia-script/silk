import * as Elaboration from './Elaboration.js'
import type * as NameResolution from './NameResolution.js'
import type * as Presentation from './Presentation.js'
import * as PresentationRenderer from './Presentation.js'
import * as SourceSpan from './SourceSpan.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'

/** One inferred local-binding type anchored to its exact declared name. */
export interface BindingTypeHint {
  readonly _tag: 'BindingTypeHint'
  readonly span: SourceSpan.SourceSpan
  readonly presentation: Presentation.Presentation
}

/** One or more omitted provider selectors sharing one call-site insertion point. */
export interface ProviderSelectorTypeHint {
  readonly _tag: 'ProviderSelectorTypeHint'
  readonly span: SourceSpan.SourceSpan
  readonly selected: ReadonlyArray<Type.Requirement>
  readonly presentation: Presentation.Presentation
}

export type TypeHint = BindingTypeHint | ProviderSelectorTypeHint

const compareText = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0

const selectorSyntax = (expression: Elaboration.ExpressionFact): SyntaxTree.Node | undefined => {
  if (expression._tag === 'Call') return expression.syntax
  if (expression._tag !== 'CallableApply') return undefined
  if (expression.provenance._tag === 'DirectCallableApplication') return expression.syntax
  return expression.provenance.callable._tag === 'CallableSection'
    ? expression.provenance.callable.syntax
    : undefined
}

const selectorFacts = (
  expression: Elaboration.ExpressionFact,
): ReadonlyArray<Elaboration.InferredProviderSelector> =>
  expression._tag === 'Call'
    ? expression.contract._tag === 'Compatible'
      ? expression.contract.inferredProviderSelectors
      : Object.freeze([])
    : expression._tag === 'CallableApply'
      ? expression.inferredProviderSelectors
      : Object.freeze([])

/** Projects available inferred editor facts into one half-open byte range. */
export const make = (
  functions: ReadonlyArray<Elaboration.FunctionFact>,
  module: string,
  scope: NameResolution.ModuleScope | undefined,
  start: number,
  end: number,
): ReadonlyArray<TypeHint> => {
  const seen = new Set<string>()
  const hints: Array<TypeHint> = []
  for (const binding of functions.flatMap((fn) => fn.bindings)) {
    if (binding.name._tag !== 'Present' || binding.inferredType._tag !== 'Available') continue
    const span = binding.name.token.span
    if (span.start < start || span.end > end) continue
    const key = `${span.sourceId}:${span.start}:${span.end}`
    if (seen.has(key)) continue
    seen.add(key)
    hints.push(
      Object.freeze({
        _tag: 'BindingTypeHint',
        span,
        presentation: PresentationRenderer.expressionType(binding.inferredType.type, module, scope),
      }),
    )
  }

  const selectorGroups = new Map<
    string,
    {
      readonly span: SourceSpan.SourceSpan
      readonly selectors: Map<string, Elaboration.InferredProviderSelector>
    }
  >()
  for (const fn of functions)
    Elaboration.visitStatementFacts(fn.statements, {
      expression: (expression) => {
        const selectors = selectorFacts(expression)
        if (selectors.length === 0) return
        const syntax = selectorSyntax(expression)
        if (syntax?.kind !== 'CallExpression' || !SyntaxTree.isAvailableSyntax(syntax)) return
        const arguments_ = SyntaxTree.directNode(syntax, 'ArgumentList')
        const leftParenthesis =
          arguments_ === undefined
            ? undefined
            : SyntaxTree.directToken(arguments_, 'LeftParenthesis')
        if (leftParenthesis === undefined) return
        const span = SourceSpan.fromOffsets(
          leftParenthesis.span.sourceId,
          leftParenthesis.span.start,
          leftParenthesis.span.start,
        )
        if (span === undefined || span.start < start || span.end > end) return
        const key = `${span.sourceId}:${span.start}`
        const group = selectorGroups.get(key) ?? Object.freeze({ span, selectors: new Map() })
        for (const selector of selectors) {
          if (!Type.isNominal(selector.selected.capability)) continue
          group.selectors.set(
            `${Type.key(selector.parameter)}:${Type.key(selector.selected.capability)}@${selector.selected.role}`,
            selector,
          )
        }
        selectorGroups.set(key, group)
      },
    })
  for (const group of selectorGroups.values()) {
    const selectors = [...group.selectors.values()].sort(
      (left, right) =>
        left.parameter.ordinal - right.parameter.ordinal ||
        compareText(Type.key(left.selected.capability), Type.key(right.selected.capability)),
    )
    const selected = Object.freeze(selectors.map((selector) => selector.selected))
    if (selected.length === 0) continue
    const text = selectors
      .flatMap((selector) =>
        Type.isNominal(selector.selected.capability)
          ? [
              PresentationRenderer.providerSelector(
                Object.freeze({
                  capability: selector.selected.capability,
                  role: selector.selected.role,
                }),
                module,
                scope,
              ).text,
            ]
          : [],
      )
      .join(', ')
    hints.push(
      Object.freeze({
        _tag: 'ProviderSelectorTypeHint',
        span: group.span,
        selected,
        presentation: Object.freeze({ _tag: 'ExpressionTypePresentation', text }),
      }),
    )
  }
  hints.sort(
    (left, right) =>
      left.span.start - right.span.start ||
      left.span.end - right.span.end ||
      compareText(left._tag, right._tag),
  )
  return Object.freeze(hints)
}
