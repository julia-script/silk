import * as Elaboration from './Elaboration.js'
import type * as NameResolution from './NameResolution.js'
import * as SemanticDisplay from './SemanticDisplay.js'
import * as SourceSpan from './SourceSpan.js'
import type * as AuthoredHir from './AuthoredHir.js'
import type * as SemanticContext from './SemanticContext.js'
import * as Type from './Type.js'

/** One inferred local-binding type anchored to its exact declared name. */
export interface BindingTypeHint {
  readonly _tag: 'BindingTypeHint'
  readonly span: SourceSpan.SourceSpan
  readonly presentation: SemanticDisplay.Presentation
}

/** One or more omitted provider selectors sharing one call-site insertion point. */
export interface ProviderSelectorTypeHint {
  readonly _tag: 'ProviderSelectorTypeHint'
  readonly span: SourceSpan.SourceSpan
  readonly selected: ReadonlyArray<Type.Requirement>
  readonly presentation: SemanticDisplay.Presentation
}

export type TypeHint = BindingTypeHint | ProviderSelectorTypeHint

const compareText = (left: string, right: string): number => {
  if (left < right) return -1
  if (left > right) return 1
  return 0
}

/**
 * The callee an omitted provider selector would be spelled after. The hint is inserted at the
 * end of that reference, which is where the authored argument list opens.
 */
/** The named end of one reference path, which is where the authored argument list opens. */
const pathAnchor = (path: Elaboration.ReferencePathFact): AuthoredHir.Anchor =>
  path._tag === 'ReferencePath' ? path.memberAnchor : path.anchor

const selectorCallee = (expression: Elaboration.ExpressionFact): AuthoredHir.Anchor | undefined => {
  if (expression._tag === 'Call') return pathAnchor(expression.path)
  if (expression._tag !== 'CallableApply') return undefined
  if (expression.provenance._tag === 'DirectCallableApplication') return expression.callee.anchor
  const callable = expression.provenance.callable
  return callable._tag === 'CallableSection' ? pathAnchor(callable.path) : undefined
}

const selectorFacts = (
  expression: Elaboration.ExpressionFact,
): ReadonlyArray<Elaboration.InferredProviderSelector> => {
  if (expression._tag === 'Call') {
    if (expression.contract._tag === 'Compatible') {
      return expression.contract.inferredProviderSelectors
    }
    return Object.freeze([])
  }
  if (expression._tag === 'CallableApply') return expression.inferredProviderSelectors
  return Object.freeze([])
}

/** Projects available inferred editor facts into one half-open byte range. */
export const make = (
  context: SemanticContext.SemanticContext,
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
    const span = context.spanOf(binding.name.anchor)
    if (span.start < start || span.end > end) continue
    const key = `${span.sourceId}:${span.start}:${span.end}`
    if (seen.has(key)) continue
    seen.add(key)
    hints.push(
      Object.freeze({
        _tag: 'BindingTypeHint',
        span,
        presentation: SemanticDisplay.expressionType(binding.inferredType.type, module, scope),
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
        const callee = selectorCallee(expression)
        if (callee === undefined) return
        const calleeSpan = context.spanOf(callee)
        const span = SourceSpan.fromOffsets(calleeSpan.sourceId, calleeSpan.end, calleeSpan.end)
        if (span === undefined || span.start < start || span.start >= end) return
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
              SemanticDisplay.providerSelector(
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
