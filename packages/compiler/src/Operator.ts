import * as Scalar from './Scalar.js'
import type * as Token from './Token.js'

/** Prefix operators supported by the bootstrap expression grammar. */
export type Prefix = 'Negate' | 'Not' | 'BitNot'

/** Infix operators supported by the bootstrap expression grammar. */
export type Infix =
  | 'Multiply'
  | 'Divide'
  | 'Remainder'
  | 'Add'
  | 'Subtract'
  | 'LessThan'
  | 'LessOrEqual'
  | 'GreaterThan'
  | 'GreaterOrEqual'
  | 'Equals'
  | 'NotEquals'
  | 'BitAnd'
  | 'BitOr'
  | 'BitXor'
  | 'And'
  | 'Or'

/**
 * The two short-circuit operators. They take and give `bool` and never reach an actor operation:
 * the right operand is conditional, so operator elaboration lowers them to a conditional instead
 * of to a call that evaluates both operands.
 */
export type ShortCircuit = 'And' | 'Or'

/** Operators an interface operation may supply through an explicit declaration marker. */
export type Eligible = Exclude<Prefix | Infix, ShortCircuit>

/** Token kinds retained after the contextual `operator` marker, including rejected control forms. */
export const declarationTokenKinds: ReadonlyArray<Token.TokenKind> = Object.freeze([
  'Minus',
  'Bang',
  'Tilde',
  'Star',
  'Slash',
  'Percent',
  'Plus',
  'Ampersand',
  'Caret',
  'Pipe',
  'Less',
  'LessEqual',
  'Greater',
  'GreaterEqual',
  'EqualEqual',
  'BangEqual',
  'AmpersandAmpersand',
  'PipePipe',
  'PipeGreater',
  'Equals',
  'RunKeyword',
])

/** True when a token can be retained for operator-marker validation. */
export const isDeclarationToken = (kind: Token.TokenKind): boolean =>
  declarationTokenKinds.includes(kind)

/** Resolves a valid eager marker using the operation arity to disambiguate `-`. */
export const declaration = (kind: Token.TokenKind, arity: number): Eligible | undefined => {
  let selected: Prefix | Infix | undefined
  if (arity === 1) {
    selected = prefix(kind)
  } else {
    if (arity === 2) {
      selected = infix(kind)?.operator
    } else {
      selected = undefined
    }
  }
  return selected === undefined || isShortCircuit(selected) ? undefined : selected
}

/** True only for the operators whose right operand is conditionally evaluated. */
export const isShortCircuit = (self: Prefix | Infix): self is ShortCircuit =>
  self === 'And' || self === 'Or'

/** True when an eager infix operator always returns `bool` rather than its operand type. */
export const isPredicate = (
  self: Prefix | Infix,
): self is 'LessThan' | 'LessOrEqual' | 'GreaterThan' | 'GreaterOrEqual' | 'Equals' | 'NotEquals' =>
  self === 'LessThan' ||
  self === 'LessOrEqual' ||
  self === 'GreaterThan' ||
  self === 'GreaterOrEqual' ||
  self === 'Equals' ||
  self === 'NotEquals'

/** The associativity used when parsing one infix precedence level. */
export type Associativity = 'Left' | 'None'

/** Closed syntax metadata for one infix operator. */
export interface InfixInfo {
  readonly operator: Infix
  readonly spelling: string
  readonly precedence: number
  readonly associativity: Associativity
}

/** A compiler-known actor selected by operator elaboration. */
export type Actor = Scalar.Spelling | 'string'

/** The canonical actor operation represented by one surface operator. */
export interface Target {
  readonly actor: string
  readonly operation: string
}

const infixByToken: Readonly<Partial<Record<Token.TokenKind, InfixInfo>>> = Object.freeze({
  Star: Object.freeze({
    operator: 'Multiply',
    spelling: '*',
    precedence: 50,
    associativity: 'Left',
  }),
  Slash: Object.freeze({
    operator: 'Divide',
    spelling: '/',
    precedence: 50,
    associativity: 'Left',
  }),
  Percent: Object.freeze({
    operator: 'Remainder',
    spelling: '%',
    precedence: 50,
    associativity: 'Left',
  }),
  Plus: Object.freeze({ operator: 'Add', spelling: '+', precedence: 40, associativity: 'Left' }),
  Minus: Object.freeze({
    operator: 'Subtract',
    spelling: '-',
    precedence: 40,
    associativity: 'Left',
  }),
  Ampersand: Object.freeze({
    operator: 'BitAnd',
    spelling: '&',
    precedence: 37,
    associativity: 'Left',
  }),
  Caret: Object.freeze({
    operator: 'BitXor',
    spelling: '^',
    precedence: 35,
    associativity: 'Left',
  }),
  Pipe: Object.freeze({
    operator: 'BitOr',
    spelling: '|',
    precedence: 33,
    associativity: 'Left',
  }),
  Less: Object.freeze({
    operator: 'LessThan',
    spelling: '<',
    precedence: 30,
    associativity: 'None',
  }),
  LessEqual: Object.freeze({
    operator: 'LessOrEqual',
    spelling: '<=',
    precedence: 30,
    associativity: 'None',
  }),
  Greater: Object.freeze({
    operator: 'GreaterThan',
    spelling: '>',
    precedence: 30,
    associativity: 'None',
  }),
  GreaterEqual: Object.freeze({
    operator: 'GreaterOrEqual',
    spelling: '>=',
    precedence: 30,
    associativity: 'None',
  }),
  EqualEqual: Object.freeze({
    operator: 'Equals',
    spelling: '==',
    precedence: 20,
    associativity: 'None',
  }),
  BangEqual: Object.freeze({
    operator: 'NotEquals',
    spelling: '!=',
    precedence: 20,
    associativity: 'None',
  }),
  AmpersandAmpersand: Object.freeze({
    operator: 'And',
    spelling: '&&',
    precedence: 18,
    associativity: 'Left',
  }),
  PipePipe: Object.freeze({
    operator: 'Or',
    spelling: '||',
    precedence: 16,
    associativity: 'Left',
  }),
})

/** Returns the prefix operator represented by one token kind. */
export const prefix = (kind: Token.TokenKind): Prefix | undefined => {
  if (kind === 'Minus') return 'Negate'
  if (kind === 'Bang') return 'Not'
  if (kind === 'Tilde') return 'BitNot'
  return undefined
}

/** Returns the immutable infix metadata represented by one token kind. */
export const infix = (kind: Token.TokenKind): InfixInfo | undefined => infixByToken[kind]

/** Returns the canonical source spelling of one prefix operator. */
export const prefixSpelling = (self: Prefix): string => {
  if (self === 'Negate') {
    return '-'
  }
  if (self === 'Not') {
    return '!'
  }
  return '~'
}

/** Returns the canonical source spelling of one eager operator. */
export const spelling = (self: Eligible): string => {
  if (self === 'Negate' || self === 'Not' || self === 'BitNot') return prefixSpelling(self)
  for (const info of Object.values(infixByToken)) if (info?.operator === self) return info.spelling
  throw new RangeError(`Operator table has no spelling for ${self}`)
}

const operationByOperator: Readonly<
  Record<Exclude<Prefix | Infix, 'Equals' | 'NotEquals' | ShortCircuit>, string>
> = Object.freeze({
  Negate: 'negate',
  Not: 'not',
  Multiply: 'multiply',
  Divide: 'divide',
  Remainder: 'remainder',
  Add: 'add',
  Subtract: 'subtract',
  LessThan: 'lessThan',
  LessOrEqual: 'lessOrEqual',
  GreaterThan: 'greaterThan',
  GreaterOrEqual: 'greaterOrEqual',
  BitAnd: 'bitAnd',
  BitOr: 'bitOr',
  BitXor: 'bitXor',
  BitNot: 'bitNot',
})

/**
 * True only when the selected scalar itself declares the operation the operator spells.
 *
 * An operand type carries only the operations its catalog entry lists: `bool` has no ordering,
 * an unsigned integer has no `negate`, a float has no `bitAnd`, and `char` has neither
 * arithmetic nor the bitwise operations. An operator over such an operand selects the default
 * integer instead, so analysis reports the same operand-type diagnostic the named function
 * reports rather than looking up an actor operation that does not exist.
 */
const declaresOperation = (self: Scalar.Scalar, operation: string): boolean =>
  self.operations.some((candidate) => candidate.spelling === operation)

/**
 * Returns the canonical actor operation for an operator and its selected equality actor.
 *
 * The short-circuit operators have no actor operation: their right operand is conditional, so
 * they never reach this table.
 */
export const target = (
  self: Exclude<Prefix | Infix, ShortCircuit>,
  equalityActor: Actor = Scalar.defaultInteger.spelling,
): Target => {
  if (self === 'Equals' || self === 'NotEquals') {
    if (equalityActor === 'string') {
      return Object.freeze({ actor: 'Intrinsic', operation: 'stringEqualsExact' })
    }
    return Object.freeze({
      actor: equalityActor,
      operation: self === 'Equals' ? 'equals' : 'notEquals',
    })
  }
  if (equalityActor === 'string') {
    return Object.freeze({
      actor: Scalar.defaultInteger.spelling,
      operation: operationByOperator[self],
    })
  }
  const selected = Scalar.find(equalityActor)
  const operation = operationByOperator[self]
  let actor = equalityActor
  if (self === 'Not') actor = Scalar.boolean.spelling
  else if (selected === undefined || !declaresOperation(selected, operation))
    actor = Scalar.defaultInteger.spelling
  return Object.freeze({ actor, operation })
}

/** The binding power of prefix operators. */
export const prefixPrecedence = 60

/** The binding power of the left-associative pipeline operator. */
export const pipelinePrecedence = 10
