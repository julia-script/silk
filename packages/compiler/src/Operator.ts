import * as Scalar from './Scalar.js'
import type * as Token from './Token.js'

/** Prefix operators supported by the bootstrap expression grammar. */
export type Prefix = 'Negate' | 'Not'

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
export type Actor = Scalar.Spelling

/** The canonical actor operation represented by one surface operator. */
export interface Target {
  readonly actor: Actor
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
})

/** Returns the prefix operator represented by one token kind. */
export const prefix = (kind: Token.TokenKind): Prefix | undefined => {
  if (kind === 'Minus') return 'Negate'
  if (kind === 'Bang') return 'Not'
  return undefined
}

/** Returns the immutable infix metadata represented by one token kind. */
export const infix = (kind: Token.TokenKind): InfixInfo | undefined => infixByToken[kind]

/** Returns the canonical source spelling of one prefix operator. */
export const prefixSpelling = (self: Prefix): string => (self === 'Negate' ? '-' : '!')

const operationByOperator: Readonly<
  Record<Exclude<Prefix | Infix, 'Equals' | 'NotEquals'>, string>
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
})

/** Returns the canonical actor operation for an operator and its selected equality actor. */
export const target = (
  self: Prefix | Infix,
  equalityActor: Actor = Scalar.defaultInteger.spelling,
): Target => {
  if (self === 'Equals' || self === 'NotEquals') {
    return Object.freeze({
      actor: equalityActor,
      operation: self === 'Equals' ? 'equals' : 'notEquals',
    })
  }
  return Object.freeze({
    actor:
      self === 'Not'
        ? Scalar.boolean.spelling
        : Scalar.find(equalityActor)?.category !== 'Integer' ||
            (self === 'Negate' && Scalar.find(equalityActor)?.signedness !== 'Signed')
          ? Scalar.defaultInteger.spelling
          : equalityActor,
    operation: operationByOperator[self],
  })
}

/** The binding power of prefix operators. */
export const prefixPrecedence = 60

/** The binding power of the left-associative pipeline operator. */
export const pipelinePrecedence = 10
