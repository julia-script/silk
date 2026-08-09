import type * as Elaboration from './Elaboration.js'
import type * as NameResolution from './NameResolution.js'
import type * as Presentation from './Presentation.js'
import * as PresentationRenderer from './Presentation.js'
import type * as SourceSpan from './SourceSpan.js'

/** One inferred local-binding type anchored to its exact declared name. */
export interface TypeHint {
  readonly _tag: 'TypeHint'
  readonly span: SourceSpan.SourceSpan
  readonly presentation: Presentation.Presentation
}

/** Projects available inferred local types into one half-open byte range. */
export const make = (
  bindings: ReadonlyArray<Elaboration.BindingDeclarationFact>,
  module: string,
  scope: NameResolution.ModuleScope | undefined,
  start: number,
  end: number,
): ReadonlyArray<TypeHint> => {
  const seen = new Set<string>()
  const hints: Array<TypeHint> = []
  for (const binding of bindings) {
    if (binding.name._tag !== 'Present' || binding.inferredType._tag !== 'Available') continue
    const span = binding.name.token.span
    if (span.start < start || span.end > end) continue
    const key = `${span.sourceId}:${span.start}:${span.end}`
    if (seen.has(key)) continue
    seen.add(key)
    hints.push(
      Object.freeze({
        _tag: 'TypeHint',
        span,
        presentation: PresentationRenderer.expressionType(binding.inferredType.type, module, scope),
      }),
    )
  }
  hints.sort((left, right) => left.span.start - right.span.start || left.span.end - right.span.end)
  return Object.freeze(hints)
}
