import type * as Mir from './Mir.js'

export type IntegerPredicate =
  | 'eq'
  | 'ne'
  | 'slt'
  | 'sle'
  | 'sgt'
  | 'sge'
  | 'ult'
  | 'ule'
  | 'ugt'
  | 'uge'

/** Selects the one LLVM integer predicate for a Silk comparison. */
export const comparisonPredicate = (
  operation: Mir.BinaryOperator,
  unsigned: boolean,
): IntegerPredicate | undefined => {
  switch (operation) {
    case 'Equals':
      return 'eq'
    case 'NotEquals':
      return 'ne'
    case 'LessThan':
      return unsigned ? 'ult' : 'slt'
    case 'LessOrEqual':
      return unsigned ? 'ule' : 'sle'
    case 'GreaterThan':
      return unsigned ? 'ugt' : 'sgt'
    case 'GreaterOrEqual':
      return unsigned ? 'uge' : 'sge'
    default:
      return undefined
  }
}
