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

/** True only for the operators whose right operand is conditionally evaluated. */
export const isShortCircuit = (self: Prefix | Infix): self is ShortCircuit =>
  self === 'And' || self === 'Or'

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
export const prefixSpelling = (self: Prefix): string =>
  self === 'Negate' ? '-' : self === 'Not' ? '!' : '~'

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
  return Object.freeze({
    actor:
      self === 'Not'
        ? Scalar.boolean.spelling
        : selected === undefined || !declaresOperation(selected, operation)
          ? Scalar.defaultInteger.spelling
          : equalityActor,
    operation,
  })
}

/** The binding power of prefix operators. */
export const prefixPrecedence = 60

/** The binding power of the left-associative pipeline operator. */
export const pipelinePrecedence = 10
