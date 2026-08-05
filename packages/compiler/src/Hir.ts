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

/** One typed core semantic operation with exact source provenance. */
export type Expression =
  | {
      readonly _tag: 'IntegerLiteral'
      readonly value: number
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
      readonly _tag: 'Call'
      readonly target: DeclarationIndex.CanonicalId
      readonly arguments: ReadonlyArray<Expression>
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Unavailable'
      readonly span: SourceSpan.SourceSpan
      readonly cause?: Diagnostic.Identity
    }

/** One elaborated function: its header, normalized contract, and desugared body expression. */
export interface HirFunction {
  readonly _tag: 'HirFunction'
  readonly declaration: DeclarationIndex.DeclarationFact
  readonly contract: ContractFact
  readonly body: Expression
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
    case 'ParameterReference':
      return `${indent}param fn${expression.parameter.function.ordinal}.p${expression.parameter.ordinal} : ${expression.type} ${spanText(expression.span)}`
    case 'Call':
      return [
        `${indent}call ${expression.target.module}.${expression.target.name} : ${expression.type} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'Unavailable':
      return `${indent}unavailable ${spanText(expression.span)}`
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
      encodeExpression(fn.body, 1),
    ]),
    '',
  ].join('\n')
