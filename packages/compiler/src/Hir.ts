import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import type * as TypeCompatibility from './TypeCompatibility.js'

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

/** A canonical source-ordered region identity local to one function. */
export interface RegionId {
  readonly _tag: 'HirRegion'
  readonly function: DeclarationIndex.DeclarationId
  readonly ordinal: number
}

/** A canonical lexical loop identity local to one function. */
export interface LoopId {
  readonly _tag: 'HirLoop'
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
  | 'Negate'
  | 'Equals'
  | 'NotEquals'
  | 'LessThan'
  | 'LessOrEqual'
  | 'GreaterThan'
  | 'GreaterOrEqual'
  | 'Not'

export type BoundsMode =
  | { readonly _tag: 'Proven'; readonly index: number; readonly length: number }
  | { readonly _tag: 'Runtime'; readonly length: number }

/** One selector in a writable place, retained in source evaluation order. */
export type WriteSelector =
  | {
      readonly _tag: 'Field'
      readonly field: DeclarationIndex.FieldId
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Index'
      readonly index: Expression
      readonly array: Type.FixedArray
      readonly bounds: BoundsMode
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }

/** One complete typed replacement rooted in a mutable binding. */
export interface WritePlace {
  readonly _tag: 'WritePlace'
  readonly root: BindingId
  readonly selectors: ReadonlyArray<WriteSelector>
  readonly type: DeclarationIndex.SemanticType
  readonly span: SourceSpan.SourceSpan
}

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
      readonly _tag: 'UnionConvert'
      readonly source: Expression
      readonly sourceType: Type.Nominal | Type.StructuralUnion | Type.Never
      readonly target: Type.StructuralUnion
      readonly conversion: 'Inject' | 'Widen'
      readonly mappings: ReadonlyArray<TypeCompatibility.MemberMapping>
      readonly access: 'Copy' | 'Owned'
      readonly context: 'Return' | 'Argument' | 'StructField' | 'ArrayElement' | 'Assignment'
      readonly expectedAt: SourceSpan.SourceSpan
      readonly type: Type.StructuralUnion
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Construct'
      readonly nominal: Type.Nominal
      /** Field identities in language evaluation order; `fields` remains canonical storage order. */
      readonly evaluationOrder: ReadonlyArray<DeclarationIndex.FieldId>
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationIndex.FieldId
        readonly value: Expression
      }>
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'ArrayConstruct'
      readonly elements: ReadonlyArray<Expression>
      readonly type: Type.FixedArray
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Project'
      readonly subject: Expression
      readonly nominal: Type.Nominal
      readonly field: DeclarationIndex.FieldId
      readonly access: 'CopyRead' | 'ConsumeRequested'
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'IndexPlace'
      readonly subject: Expression
      readonly index: Expression
      readonly array: Type.FixedArray
      readonly access: 'CopyRead' | 'ConsumeRequested'
      readonly bounds: BoundsMode
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
      readonly _tag: 'UnavailableStatement'
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Bind'
      readonly binding: BindingId
      readonly name: string | undefined
      readonly mutability: 'Immutable' | 'Mutable'
      readonly initializer: Expression
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'If'
      readonly condition: Expression
      readonly taken: ReadonlyArray<Statement>
      readonly otherwise: ReadonlyArray<Statement>
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Write'
      readonly place: WritePlace
      readonly value: Expression
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'While'
      readonly loop: LoopId
      readonly parent?: LoopId
      readonly condition: Expression
      readonly body: ReadonlyArray<Statement>
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Break'
      readonly target: LoopId
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Continue'
      readonly target: LoopId
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Return'
      readonly expression: Expression
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }

/** One elaborated function: its header, normalized contract, and desugared body statements. */
export interface HirFunction {
  readonly _tag: 'HirFunction'
  readonly declaration: DeclarationIndex.DeclarationFact
  readonly contract: ContractFact
  readonly entryRegion: RegionId
  readonly regionOrder: ReadonlyArray<RegionId>
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
    case 'UnavailableStatement':
      return []
    case 'Bind':
      return [statement.initializer]
    case 'Write':
      return [
        ...statement.place.selectors.flatMap((selector) =>
          selector._tag === 'Index' ? [selector.index] : [],
        ),
        statement.value,
      ]
    case 'Return':
      return [statement.expression]
    case 'Break':
    case 'Continue':
      return []
    case 'While':
      return [statement.condition, ...statement.body.flatMap(statementExpressions)]
    case 'If':
      return [
        statement.condition,
        ...statement.taken.flatMap(statementExpressions),
        ...statement.otherwise.flatMap(statementExpressions),
      ]
  }
}

/** One expression and all of its semantic children in deterministic preorder. */
export const expressionTree = (expression: Expression): ReadonlyArray<Expression> => {
  const children: ReadonlyArray<Expression> = (() => {
    switch (expression._tag) {
      case 'Move':
      case 'Project':
      case 'UnionConvert':
        return [expression._tag === 'UnionConvert' ? expression.source : expression.subject]
      case 'IndexPlace':
        return [expression.subject, expression.index]
      case 'Construct':
        return expression.fields.map((field) => field.value)
      case 'ArrayConstruct':
        return expression.elements
      case 'Call':
      case 'BuiltinCall':
        return expression.arguments
      default:
        return []
    }
  })()
  return Object.freeze([expression, ...children.flatMap(expressionTree)])
}

/** Tests whether any expression in the body is an explicit unavailable state. */
export const hasUnavailable = (self: HirFunction): boolean => {
  const walk = (expression: Expression): boolean => {
    switch (expression._tag) {
      case 'Unavailable':
        return true
      case 'Move':
      case 'Project':
        return walk(expression.subject)
      case 'UnionConvert':
        return walk(expression.source)
      case 'IndexPlace':
        return walk(expression.subject) || walk(expression.index)
      case 'Construct':
        return expression.fields.some((field) => walk(field.value))
      case 'ArrayConstruct':
        return expression.elements.some(walk)
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
      case 'Project':
        return walk(expression.subject)
      case 'UnionConvert':
        return walk(expression.source)
      case 'IndexPlace':
        return walk(expression.subject) ?? walk(expression.index)
      case 'Construct': {
        for (const field of expression.fields) {
          const found = walk(field.value)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'ArrayConstruct': {
        for (const element of expression.elements) {
          const found = walk(element)
          if (found !== undefined) return found
        }
        return undefined
      }
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
    ? `(${contract.parameters.map(Type.encode).join(', ')}) -> ${Type.encode(contract.result)}`
    : 'contract-unavailable'

const encodeExpression = (expression: Expression, depth: number): string => {
  const indent = '  '.repeat(depth)
  switch (expression._tag) {
    case 'IntegerLiteral':
      return `${indent}literal ${expression.value} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'BooleanLiteral':
      return `${indent}literal ${expression.value} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'ParameterReference':
      return `${indent}param fn${expression.parameter.function.ordinal}.p${expression.parameter.ordinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'BindingReference':
      return `${indent}binding fn${expression.binding.function.ordinal}.b${expression.binding.ordinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'Move':
      return [
        `${indent}move : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'UnionConvert':
      return [
        `${indent}union-${expression.conversion.toLowerCase()} ${Type.encode(expression.sourceType)} -> ${Type.encode(expression.target)} access=${expression.access} context=${expression.context} expected=${spanText(expression.expectedAt)} ${spanText(expression.span)}`,
        `${indent}  mapping ${expression.mappings.map((mapping) => `${Type.encode(mapping.source)}#${mapping.sourceOrdinal}->${Type.encode(mapping.target)}#${mapping.targetOrdinal}`).join(', ') || 'empty'}`,
        encodeExpression(expression.source, depth + 1),
      ].join('\n')
    case 'Construct':
      return [
        `${indent}construct ${Type.encode(expression.nominal)} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        `${indent}  evaluation-order ${expression.evaluationOrder.map((field) => `#${field.ordinal}`).join(', ') || 'empty'}`,
        ...expression.fields.map(
          ({ field, value }) =>
            `${indent}  field #${field.ordinal}\n${encodeExpression(value, depth + 2)}`,
        ),
      ].join('\n')
    case 'ArrayConstruct':
      return [
        `${indent}construct-array ${Type.encode(expression.type)} elements=${expression.elements.length} ${spanText(expression.span)}`,
        ...expression.elements.map(
          (element, index) =>
            `${indent}  element #${index}\n${encodeExpression(element, depth + 2)}`,
        ),
      ].join('\n')
    case 'Project':
      return [
        `${indent}project ${expression.access} ${Type.encode(expression.nominal)}.#${expression.field.ordinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'IndexPlace':
      return [
        `${indent}index ${expression.access} ${Type.encode(expression.array)} bounds=${
          expression.bounds._tag === 'Runtime'
            ? `runtime:${expression.bounds.length}`
            : `proven:${expression.bounds.index}/${expression.bounds.length}`
        } : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
        encodeExpression(expression.index, depth + 1),
      ].join('\n')
    case 'Call':
      return [
        `${indent}call ${expression.target.module}.${expression.target.name} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'BuiltinCall':
      return [
        `${indent}builtin I32.${expression.operation} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'Unavailable':
      return `${indent}unavailable ${spanText(expression.span)}`
  }
}

const encodeStatement = (statement: Statement, depth: number): string => {
  const indent = '  '.repeat(depth)
  switch (statement._tag) {
    case 'UnavailableStatement':
      return `${indent}unavailable-statement r${statement.region.ordinal} ${spanText(statement.span)}`
    case 'Bind':
      return [
        `${indent}bind ${statement.mutability.toLowerCase()} b${statement.binding.ordinal} ${statement.name ?? '?'} r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.initializer, depth + 1),
      ].join('\n')
    case 'Write':
      return [
        `${indent}write b${statement.place.root.ordinal}${statement.place.selectors
          .map((selector) =>
            selector._tag === 'Field'
              ? `.#${selector.field.ordinal}`
              : `[${selector.bounds._tag === 'Proven' ? selector.bounds.index : 'runtime'}/${selector.array.length}]`,
          )
          .join(
            '',
          )} : ${Type.encode(statement.place.type)} r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.value, depth + 1),
      ].join('\n')
    case 'If':
      return [
        `${indent}if r${statement.region.ordinal} ${spanText(statement.span)}`,
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
    case 'While':
      return [
        `${indent}while loop${statement.loop.ordinal} r${statement.region.ordinal}${statement.parent === undefined ? '' : ` parent=loop${statement.parent.ordinal}`} ${spanText(statement.span)}`,
        encodeExpression(statement.condition, depth + 1),
        ...statement.body.map((inner) => encodeStatement(inner, depth + 1)),
      ].join('\n')
    case 'Break':
    case 'Continue':
      return `${indent}${statement._tag.toLowerCase()} loop${statement.target.ordinal} r${statement.region.ordinal} ${spanText(statement.span)}`
    case 'Return':
      return [
        `${indent}return r${statement.region.ordinal} ${spanText(statement.span)}`,
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
      `fn ${identityLabel(fn.declaration)} ${contractText(fn.contract)} entry=r${fn.entryRegion.ordinal} regions=${fn.regionOrder.map((region) => `r${region.ordinal}`).join(',')}`,
      ...fn.statements.map((statement) => encodeStatement(statement, 1)),
    ]),
    '',
  ].join('\n')
