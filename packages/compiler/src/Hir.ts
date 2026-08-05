import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as SourceSpan from './SourceSpan.js'

/**
 * HIR: the resolved, typed semantic representation of elaborated bodies. Core operations carry
 * their resolved type and exact source provenance; unknown facts stay explicit unavailable
 * states and never masquerade as typed operations.
 */

/** A normalized function contract: ordered parameter types and the result type. */
export interface Contract {
  readonly _tag: 'Contract'
  readonly parameters: ReadonlyArray<DeclarationIndex.SemanticType>
  readonly result: DeclarationIndex.SemanticType
}

/** The normalized or explicitly unavailable contract of one declaration. */
export type ContractFact =
  | Contract
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

/** A deterministic binding identity local to its declaring function's statement order. */
export interface BindingId {
  readonly _tag: 'HirBinding'
  readonly function: DeclarationIndex.DeclarationId
  readonly ordinal: number
}

/** The closed built-in operation vocabulary of the compiler-known actors. */
export type BuiltinOperation =
  | 'Add'
  | 'Subtract'
  | 'Multiply'
  | 'Divide'
  | 'Remainder'
  | 'Equals'
  | 'NotEquals'
  | 'LessThan'
  | 'LessOrEqual'
  | 'GreaterThan'
  | 'GreaterOrEqual'
  | 'Not'

/** One typed core semantic operation with exact source provenance. */
export type Expression =
  | {
      readonly _tag: 'IntegerLiteral'
      readonly value: number
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'BooleanLiteral'
      readonly value: boolean
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'ParameterReference'
      readonly parameter: DeclarationIndex.ParameterId
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'BindingReference'
      readonly binding: BindingId
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Move'
      readonly subject: Expression
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Call'
      readonly target: DeclarationIndex.CanonicalId
      readonly arguments: ReadonlyArray<Expression>
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'BuiltinCall'
      readonly operation: BuiltinOperation
      readonly arguments: ReadonlyArray<Expression>
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Unavailable'
      readonly span: SourceSpan.SourceSpan
      readonly cause?: Diagnostic.Identity
    }

/** One elaborated body statement in source order. */
export type Statement =
  | {
      readonly _tag: 'Bind'
      readonly binding: BindingId
      readonly name: string | undefined
      readonly initializer: Expression
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'If'
      readonly condition: Expression
      readonly taken: ReadonlyArray<Statement>
      readonly otherwise: ReadonlyArray<Statement>
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Return'
      readonly expression: Expression
      readonly span: SourceSpan.SourceSpan
    }

/** One elaborated function: its header, normalized contract, and desugared body statements. */
export interface HirFunction {
  readonly _tag: 'HirFunction'
  readonly declaration: DeclarationIndex.DeclarationFact
  readonly contract: ContractFact
  readonly statements: ReadonlyArray<Statement>
}

/** The return statement's expression — every body ends in exactly one. */
export const returned = (self: HirFunction): Expression => {
  const last = self.statements.at(-1)
  if (last === undefined || last._tag !== 'Return') {
    throw new RangeError('HIR body must end in a return statement')
  }
  return last.expression
}

/** Every expression directly carried by one statement, nesting through conditionals. */
export const statementExpressions = (statement: Statement): ReadonlyArray<Expression> => {
  switch (statement._tag) {
    case 'Bind':
      return [statement.initializer]
    case 'Return':
      return [statement.expression]
    case 'If':
      return [
        statement.condition,
        ...statement.taken.flatMap(statementExpressions),
        ...statement.otherwise.flatMap(statementExpressions),
      ]
  }
}

/** Tests whether any expression in the body is an explicit unavailable state. */
export const hasUnavailable = (self: HirFunction): boolean => {
  const walk = (expression: Expression): boolean => {
    switch (expression._tag) {
      case 'Unavailable':
        return true
      case 'Move':
        return walk(expression.subject)
      case 'Call':
      case 'BuiltinCall':
        return expression.arguments.some(walk)
      default:
        return false
    }
  }
  return self.statements.flatMap(statementExpressions).some(walk)
}

/** The first unavailable expression's cause and span, if the body has one. */
export const firstUnavailable = (
  self: HirFunction,
): { readonly span: SourceSpan.SourceSpan; readonly cause?: Diagnostic.Identity } | undefined => {
  const walk = (
    expression: Expression,
  ): { readonly span: SourceSpan.SourceSpan; readonly cause?: Diagnostic.Identity } | undefined => {
    switch (expression._tag) {
      case 'Unavailable':
        return expression
      case 'Move':
        return walk(expression.subject)
      case 'Call':
      case 'BuiltinCall': {
        for (const argument of expression.arguments) {
          const found = walk(argument)
          if (found !== undefined) return found
        }
        return undefined
      }
      default:
        return undefined
    }
  }
  for (const expression of self.statements.flatMap(statementExpressions)) {
    const found = walk(expression)
    if (found !== undefined) return found
  }
  return undefined
}

/** One module's elaborated HIR. */
export interface Module {
  readonly _tag: 'HirModule'
  readonly module: string
  readonly functions: ReadonlyArray<HirFunction>
}

/** Normalizes one header's contract, or keeps it explicitly unavailable with its cause. */
export const contractOf = (declaration: DeclarationIndex.DeclarationFact): ContractFact => {
  const parameters: Array<DeclarationIndex.SemanticType> = []
  for (const parameter of declaration.parameters) {
    if (parameter.declaredType._tag !== 'Resolved') {
      return Object.freeze({
        _tag: 'Unavailable',
        ...(parameter.declaredType._tag === 'Unresolved' &&
        parameter.declaredType.cause !== undefined
          ? { cause: parameter.declaredType.cause }
          : {}),
      })
    }
    parameters.push(parameter.declaredType.type)
  }
  if (declaration.returnType._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'Unavailable',
      ...(declaration.returnType._tag === 'Unresolved' && declaration.returnType.cause !== undefined
        ? { cause: declaration.returnType.cause }
        : {}),
    })
  }
  return Object.freeze({
    _tag: 'Contract',
    parameters: Object.freeze(parameters),
    result: declaration.returnType.type,
  })
}

const spanText = (span: SourceSpan.SourceSpan): string => `[${span.start}, ${span.end})`

const identityLabel = (declaration: DeclarationIndex.DeclarationFact): string => {
  switch (declaration.canonical._tag) {
    case 'Canonical':
      return `${declaration.canonical.id.module}.${declaration.canonical.id.name}`
    case 'Duplicate':
      return `duplicate:${declaration.canonical.original.module}.${declaration.canonical.original.name}#${declaration.id.ordinal}`
    case 'Unidentified':
      return `unidentified#${declaration.id.ordinal}`
  }
}

const contractText = (contract: ContractFact): string =>
  contract._tag === 'Contract'
    ? `(${contract.parameters.join(', ')}) -> ${contract.result}`
    : 'contract-unavailable'

const encodeExpression = (expression: Expression, depth: number): string => {
  const indent = '  '.repeat(depth)
  switch (expression._tag) {
    case 'IntegerLiteral':
      return `${indent}literal ${expression.value} : ${expression.type} ${spanText(expression.span)}`
    case 'BooleanLiteral':
      return `${indent}literal ${expression.value} : ${expression.type} ${spanText(expression.span)}`
    case 'ParameterReference':
      return `${indent}param fn${expression.parameter.function.ordinal}.p${expression.parameter.ordinal} : ${expression.type} ${spanText(expression.span)}`
    case 'BindingReference':
      return `${indent}binding fn${expression.binding.function.ordinal}.b${expression.binding.ordinal} : ${expression.type} ${spanText(expression.span)}`
    case 'Move':
      return [
        `${indent}move : ${expression.type} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'Call':
      return [
        `${indent}call ${expression.target.module}.${expression.target.name} : ${expression.type} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'BuiltinCall':
      return [
        `${indent}builtin I32.${expression.operation} : ${expression.type} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'Unavailable':
      return `${indent}unavailable ${spanText(expression.span)}`
  }
}

const encodeStatement = (statement: Statement, depth: number): string => {
  const indent = '  '.repeat(depth)
  switch (statement._tag) {
    case 'Bind':
      return [
        `${indent}bind b${statement.binding.ordinal} ${statement.name ?? '?'} ${spanText(statement.span)}`,
        encodeExpression(statement.initializer, depth + 1),
      ].join('\n')
    case 'If':
      return [
        `${indent}if ${spanText(statement.span)}`,
        encodeExpression(statement.condition, depth + 1),
        `${indent}then`,
        ...statement.taken.map((inner) => encodeStatement(inner, depth + 1)),
        ...(statement.otherwise.length === 0
          ? []
          : [
              `${indent}else`,
              ...statement.otherwise.map((inner) => encodeStatement(inner, depth + 1)),
            ]),
      ].join('\n')
    case 'Return':
      return [
        `${indent}return ${spanText(statement.span)}`,
        encodeExpression(statement.expression, depth + 1),
      ].join('\n')
  }
}

/**
 * Deterministic textual encoding of one module's completed HIR for debugging, inspection, and
 * golden tests. No compatibility promise attaches to this format.
 */
export const encode = (self: Module): string =>
  [
    `hir-module ${self.module}`,
    ...self.functions.flatMap((fn) => [
      `fn ${identityLabel(fn.declaration)} ${contractText(fn.contract)}`,
      ...fn.statements.map((statement) => encodeStatement(statement, 1)),
    ]),
    '',
  ].join('\n')
