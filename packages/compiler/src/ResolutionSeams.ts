import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Type from './Type.js'

/** Type and exact-item resolution boundaries supplied while completing declaration headers. */
export interface ResolutionSeams {
  readonly type: DeclarationIndex.TypeResolver
  readonly item: DeclarationIndex.ItemResolver
  readonly representationBindings?: ReadonlyMap<string, Type.Parameter>
}

/** Constructs one pair of declaration-resolution boundaries. */
export const make = (
  type: DeclarationIndex.TypeResolver,
  item: DeclarationIndex.ItemResolver,
): ResolutionSeams => Object.freeze({ type, item })

/** Adds one resolved opaque binder without reconstructing the underlying resolution boundaries. */
export const withRepresentationBinding = (
  self: ResolutionSeams,
  unresolved: Type.Parameter,
  resolved: Type.Parameter,
): ResolutionSeams => {
  const representationBindings = new Map(self.representationBindings)
  representationBindings.set(Type.key(unresolved), resolved)
  return Object.freeze({ ...self, representationBindings })
}
