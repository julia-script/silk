import type * as Mir from './Mir.js'

/** Runtime inputs consumed by an Effect execution operation. */
export const operationInputs = (
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
  >,
): ReadonlyArray<Mir.LocalId> =>
  operation._tag === 'RunEffect'
    ? Object.freeze(operation.arguments)
    : Object.freeze([operation.effect, ...operation.arguments])
