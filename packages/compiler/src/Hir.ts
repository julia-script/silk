import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import * as Match from './Match.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/**
 * HIR: the resolved, typed semantic representation of elaborated bodies. Core operations carry
 * their resolved type and exact source provenance; unknown facts stay explicit unavailable
 * states and never masquerade as typed operations.
 */

/** A normalized function contract: ordered parameter types and the result type. */
export interface Contract {
  readonly _tag: 'Contract'
  readonly functionKind?: 'Ordinary' | 'Effect'
  readonly parameters: ReadonlyArray<DeclarationIndex.SemanticType>
  readonly result: DeclarationIndex.SemanticType
  readonly failures?: ReadonlyArray<Type.Nominal>
  readonly requirements?: ReadonlyArray<Type.Requirement>
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

/** Compiler-only identity for one explicit call argument borrow. */
export interface BorrowId {
  readonly _tag: 'BorrowId'
  readonly function: DeclarationIndex.DeclarationId
  readonly callSpan: SourceSpan.SourceSpan
  readonly ordinal: number
}

/** Hidden nominal identity for one source `effect {}` construction site. */
export interface EffectSiteId {
  readonly _tag: 'EffectSiteId'
  readonly function: DeclarationIndex.DeclarationId
  readonly span: SourceSpan.SourceSpan
}

/** Hidden nominal identity for one automatic callable-section construction site. */
export interface CallableSiteId {
  readonly _tag: 'CallableSiteId'
  readonly function: DeclarationIndex.DeclarationId
  readonly span: SourceSpan.SourceSpan
}

export type CallableTarget =
  | {
      readonly _tag: 'DeclarationCallableTarget'
      readonly declaration: DeclarationIndex.CanonicalId
    }
  | {
      readonly _tag: 'BuiltinCallableTarget'
      readonly actor: string
      readonly operation: BuiltinOperation
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
  | 'LayoutMake'
  | 'LayoutOf'
  | 'LayoutRepeat'
  | 'SystemAllocatorMake'
  | 'AllocatorAllocate'
  | 'UnitMake'
  | 'RawBufferFrom'
  | 'RawBufferSlot'
  | 'RawBufferCount'
  | 'SlotWrite'
  | 'SlotTake'
  | 'SlotCopy'
  | 'SlotDrop'

export type BoundsMode =
  | { readonly _tag: 'Proven'; readonly index: number; readonly length: number }
  | { readonly _tag: 'Runtime'; readonly length: number }

export type SliceRoot =
  | { readonly _tag: 'BindingSliceRoot'; readonly binding: BindingId }
  | {
      readonly _tag: 'ParameterSliceRoot'
      readonly parameter: DeclarationIndex.ParameterId
    }

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
export interface OwnedWritePlace {
  readonly _tag: 'WritePlace'
  readonly root: BindingId
  readonly selectors: ReadonlyArray<WriteSelector>
  readonly type: DeclarationIndex.SemanticType
  readonly span: SourceSpan.SourceSpan
}

export type BorrowedWriteSelector =
  | {
      readonly _tag: 'SliceIndex'
      readonly index: Expression
      readonly slice: Type.Slice
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | Extract<WriteSelector, { readonly _tag: 'Field' }>

export interface BorrowedWritePlace {
  readonly _tag: 'BorrowedWritePlace'
  readonly root: DeclarationIndex.ParameterId
  /** The borrowed root: an exclusive slice, or an exclusive reference written through. */
  readonly slice: Type.Slice | Type.Reference
  readonly selectors: ReadonlyArray<BorrowedWriteSelector>
  readonly type: DeclarationIndex.SemanticType
  readonly span: SourceSpan.SourceSpan
}

export type WritePlace = OwnedWritePlace | BorrowedWritePlace

/** One typed core semantic operation with exact source provenance. */
export type Expression =
  | {
      readonly _tag: 'IntegerLiteral'
      readonly value: number | bigint
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
      readonly _tag: 'PatternBindingReference'
      readonly binding: Match.BindingId
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
      /** Atomically reads one writable place and stores a replacement, yielding the old value. */
      readonly _tag: 'Replace'
      readonly place: WritePlace
      readonly value: Expression
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
      readonly context:
        | 'Return'
        | 'Argument'
        | 'StructField'
        | 'ArrayElement'
        | 'Assignment'
        | 'MatchArm'
      readonly expectedAt: SourceSpan.SourceSpan
      readonly type: Type.StructuralUnion
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Match'
      readonly id: Match.MatchId
      readonly access: Match.Access
      readonly scrutinee: Expression
      readonly members: ReadonlyArray<Type.Nominal>
      readonly arms: ReadonlyArray<{
        readonly id: Match.ArmId
        readonly member?: Type.Nominal
        readonly universal: boolean
        readonly bindings: ReadonlyArray<{
          readonly id: Match.BindingId
          readonly name?: string
          /** Absent for a whole-member binding, which owns the entire payload. */
          readonly field?: DeclarationIndex.FieldId
          readonly path: ReadonlyArray<DeclarationIndex.FieldId>
          readonly type: DeclarationIndex.SemanticType
          readonly access: Match.Access
          readonly span: SourceSpan.SourceSpan
        }>
        readonly cleanup: ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>>
        readonly guard?: Expression
        readonly result: Expression
        readonly before: ReadonlyArray<Type.Nominal>
        readonly after: ReadonlyArray<Type.Nominal>
        readonly reachable: boolean
        readonly span: SourceSpan.SourceSpan
      }>
      readonly type: DeclarationIndex.SemanticType
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
      readonly borrowAccess?: Type.Slice['access']
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
      readonly _tag: 'SliceBorrow'
      readonly borrow: BorrowId
      readonly root: SliceRoot
      readonly source: Type.FixedArray | Type.Slice
      readonly access: Type.Slice['access']
      readonly reborrow: boolean
      readonly suspendsParent: boolean
      readonly type: Type.Slice
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'ValueBorrow'
      readonly borrow: BorrowId
      readonly root: SliceRoot
      readonly source: Type.Type
      readonly access: Type.Reference['access']
      readonly type: Type.Reference
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'SliceLength'
      readonly slice: Expression
      readonly type: 'I32'
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'SliceIndexPlace'
      readonly slice: Expression
      readonly index: Expression
      readonly access: Type.Slice['access']
      readonly sourceType: Type.Slice
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Call'
      readonly target: DeclarationIndex.CanonicalId
      readonly typeArguments: ReadonlyArray<Type.Type>
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'FunctionItem'
      readonly target: CallableTarget
      readonly type: Type.Callable
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'CallableSection'
      readonly site: CallableSiteId
      readonly target: CallableTarget
      readonly omittedParameter: 0
      readonly captures: ReadonlyArray<{
        readonly ordinal: number
        readonly parameterOrdinal: number
        readonly value: Expression
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }>
      readonly typeArguments: ReadonlyArray<Type.Type>
      readonly substitution: ReadonlyMap<string, Type.Type>
      readonly retainedDependencies: ReadonlyArray<number>
      readonly mode: Type.CallableMode
      readonly type: Type.Callable
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'CallableApply'
      readonly callee: Expression
      readonly arguments: ReadonlyArray<Expression>
      readonly access: Type.CallableMode
      readonly substitution: ReadonlyMap<string, Type.Type>
      readonly evaluation: 'CalleeThenArguments' | 'LeftThenCallable'
      readonly realization: 'Environment' | 'DirectErasedSection'
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectConstruct'
      readonly target: DeclarationIndex.CanonicalId
      readonly typeArguments: ReadonlyArray<Type.Type>
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectBlock'
      readonly site: EffectSiteId
      readonly statements: ReadonlyArray<Statement>
      readonly captures: ReadonlyArray<{
        readonly binding?: BindingId
        readonly parameter?: DeclarationIndex.ParameterId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
        readonly span: SourceSpan.SourceSpan
      }>
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Run'
      readonly subject: Expression
      readonly type: DeclarationIndex.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectCatch'
      readonly protected: Expression
      readonly handled: Type.Nominal
      readonly handler: Expression
      readonly handlerEffect: Type.Effect
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectRetry'
      readonly protected: Expression
      readonly retries: Expression
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectTransform'
      readonly operation: 'Map' | 'FlatMap' | 'Tap'
      readonly protected: Expression
      readonly callback: Expression
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectProvide'
      readonly protected: Expression
      readonly provider: {
        readonly binding?: BindingId
        readonly parameter?: DeclarationIndex.ParameterId
        readonly capability: Type.Nominal
        readonly providerType: Type.Nominal
        readonly witness: DeclarationIndex.ConformanceWitness
        readonly role: string
        readonly access: 'Shared' | 'Exclusive' | 'Take'
        readonly span: SourceSpan.SourceSpan
      }
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectProvideWith'
      readonly protected: Expression
      readonly acquisition: Expression
      readonly capability: Type.Nominal
      readonly role: string
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'BuiltinCall'
      readonly operation: BuiltinOperation
      readonly typeArguments: ReadonlyArray<Type.Type>
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      readonly heldLoans: ReadonlyArray<BorrowId>
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
      readonly _tag: 'Unsafe'
      readonly statements: ReadonlyArray<Statement>
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
  | {
      readonly _tag: 'Fail'
      readonly expression: Expression
      readonly failure: Type.Nominal
      readonly transfer: 'Copy' | 'Move'
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Drop'
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
    case 'Unsafe':
      return statement.statements.flatMap(statementExpressions)
    case 'Bind':
      return [statement.initializer]
    case 'Write':
      return [
        ...statement.place.selectors.flatMap((selector) =>
          selector._tag === 'Index' || selector._tag === 'SliceIndex' ? [selector.index] : [],
        ),
        statement.value,
      ]
    case 'Return':
      return [statement.expression]
    case 'Fail':
    case 'Drop':
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

/** The semantic children of one expression, including a lazy block's statement expressions. */
export const expressionChildren = (expression: Expression): ReadonlyArray<Expression> =>
  (() => {
    switch (expression._tag) {
      case 'Move':
      case 'Project':
      case 'UnionConvert':
        return [expression._tag === 'UnionConvert' ? expression.source : expression.subject]
      case 'Replace':
        return [expression.value]
      case 'IndexPlace':
        return [expression.subject, expression.index]
      case 'SliceLength':
        return [expression.slice]
      case 'SliceIndexPlace':
        return [expression.slice, expression.index]
      case 'Construct':
        return expression.fields.map((field) => field.value)
      case 'ArrayConstruct':
        return expression.elements
      case 'Call':
      case 'EffectConstruct':
      case 'BuiltinCall':
        return expression.arguments
      case 'CallableSection':
        return expression.captures.map((capture) => capture.value)
      case 'CallableApply':
        return expression.evaluation === 'LeftThenCallable'
          ? [...expression.arguments, expression.callee]
          : [expression.callee, ...expression.arguments]
      case 'EffectBlock':
        return expression.statements.flatMap(statementExpressions)
      case 'Run':
        return [expression.subject]
      case 'EffectCatch':
        return [expression.protected, expression.handler]
      case 'EffectRetry':
        return [expression.protected, expression.retries]
      case 'EffectTransform':
        return [expression.protected, expression.callback]
      case 'EffectProvide':
        return [expression.protected]
      case 'EffectProvideWith':
        return [expression.protected, expression.acquisition]
      case 'Match':
        return [
          expression.scrutinee,
          ...expression.arms.flatMap((arm) => [
            ...(arm.guard === undefined ? [] : [arm.guard]),
            arm.result,
          ]),
        ]
      default:
        return []
    }
  })()

/** One expression and all of its semantic children in deterministic preorder. */
export const expressionTree = (expression: Expression): ReadonlyArray<Expression> => {
  const children = expressionChildren(expression)
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
      case 'Replace':
        return walk(expression.value)
      case 'UnionConvert':
        return walk(expression.source)
      case 'IndexPlace':
        return walk(expression.subject) || walk(expression.index)
      case 'SliceLength':
        return walk(expression.slice)
      case 'SliceIndexPlace':
        return walk(expression.slice) || walk(expression.index)
      case 'Construct':
        return expression.fields.some((field) => walk(field.value))
      case 'ArrayConstruct':
        return expression.elements.some(walk)
      case 'Call':
      case 'EffectConstruct':
      case 'BuiltinCall':
        return expression.arguments.some(walk)
      case 'CallableSection':
        return expression.captures.some((capture) => walk(capture.value))
      case 'CallableApply':
        return walk(expression.callee) || expression.arguments.some(walk)
      case 'Run':
        return walk(expression.subject)
      case 'EffectCatch':
        return walk(expression.protected) || walk(expression.handler)
      case 'EffectRetry':
        return walk(expression.protected) || walk(expression.retries)
      case 'EffectTransform':
        return walk(expression.protected) || walk(expression.callback)
      case 'EffectProvide':
        return walk(expression.protected)
      case 'EffectProvideWith':
        return walk(expression.protected) || walk(expression.acquisition)
      case 'Match':
        return (
          walk(expression.scrutinee) ||
          expression.arms.some(
            (arm) => (arm.guard !== undefined && walk(arm.guard)) || walk(arm.result),
          )
        )
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
      case 'Replace':
        return walk(expression.value)
      case 'UnionConvert':
        return walk(expression.source)
      case 'IndexPlace':
        return walk(expression.subject) ?? walk(expression.index)
      case 'SliceLength':
        return walk(expression.slice)
      case 'SliceIndexPlace':
        return walk(expression.slice) ?? walk(expression.index)
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
      case 'EffectConstruct':
      case 'BuiltinCall': {
        for (const argument of expression.arguments) {
          const found = walk(argument)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'CallableSection': {
        for (const capture of expression.captures) {
          const found = walk(capture.value)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'CallableApply': {
        const callee = walk(expression.callee)
        if (callee !== undefined) return callee
        for (const argument of expression.arguments) {
          const found = walk(argument)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'Run':
        return walk(expression.subject)
      case 'EffectCatch':
        return walk(expression.protected) ?? walk(expression.handler)
      case 'EffectRetry':
        return walk(expression.protected) ?? walk(expression.retries)
      case 'EffectTransform':
        return walk(expression.protected) ?? walk(expression.callback)
      case 'EffectProvide':
        return walk(expression.protected)
      case 'EffectProvideWith':
        return walk(expression.protected) ?? walk(expression.acquisition)
      case 'Match': {
        const scrutinee = walk(expression.scrutinee)
        if (scrutinee !== undefined) return scrutinee
        for (const arm of expression.arms) {
          const guard = arm.guard === undefined ? undefined : walk(arm.guard)
          if (guard !== undefined) return guard
          const result = walk(arm.result)
          if (result !== undefined) return result
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

export type VerificationIssue =
  | { readonly _tag: 'CyclicExpression'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidMatchArmOrder'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidMatchCoverage'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidMatchGuard'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidMatchResult'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidPatternBinding'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidSliceBorrow'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidValueBorrow'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidSliceOperation'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidLoanEnd'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidBorrowedWrite'; readonly span: SourceSpan.SourceSpan }

const sameMembers = (
  left: ReadonlyArray<Type.Nominal>,
  right: ReadonlyArray<Type.Nominal>,
): boolean =>
  left.length === right.length &&
  left.every((member, index) => Type.equals(member, right[index] ?? member))

/** Verifies acyclic expression ownership and the canonical facts carried by structured matches. */
export const verify = (self: Module): ReadonlyArray<VerificationIssue> => {
  const issues: Array<VerificationIssue> = []
  const active = new Set<Expression>()
  const borrowKey = (borrow: BorrowId): string =>
    `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`
  const walk = (expression: Expression): void => {
    if (active.has(expression)) {
      issues.push(Object.freeze({ _tag: 'CyclicExpression', span: expression.span }))
      return
    }
    active.add(expression)
    if (expression._tag === 'SliceBorrow') {
      const element = Type.isFixedArray(expression.source)
        ? expression.source.element
        : expression.source.element
      if (
        !Type.equals(expression.type, Type.slice(expression.access, element)) ||
        expression.reborrow !== Type.isSlice(expression.source) ||
        (expression.reborrow && expression.root._tag !== 'ParameterSliceRoot') ||
        (Type.isSlice(expression.source) &&
          expression.source.access === 'Shared' &&
          expression.access === 'Exclusive')
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceBorrow', span: expression.span }))
      }
    }
    if (expression._tag === 'ValueBorrow') {
      if (
        !Type.equals(expression.type, Type.reference(expression.access, expression.source)) ||
        expression.type.access !== expression.access
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidValueBorrow', span: expression.span }))
      }
    }
    if (expression._tag === 'SliceLength') {
      if (expression.slice._tag === 'Unavailable' || !Type.isSlice(expression.slice.type)) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceOperation', span: expression.span }))
      }
    }
    if (expression._tag === 'SliceIndexPlace') {
      if (
        expression.slice._tag === 'Unavailable' ||
        !Type.equals(expression.slice.type, expression.sourceType) ||
        expression.index._tag === 'Unavailable' ||
        !Type.equals(expression.index.type, 'I32') ||
        expression.access !== expression.sourceType.access ||
        !Type.equals(expression.type, expression.sourceType.element)
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceOperation', span: expression.span }))
      }
    }
    if (expression._tag === 'Project' && expression.borrowAccess !== undefined) {
      const subjectType =
        'type' in expression.subject && typeof expression.subject.type === 'object'
          ? expression.subject.type
          : undefined
      const inherited =
        expression.subject._tag === 'SliceIndexPlace'
          ? expression.subject.access
          : expression.subject._tag === 'Project'
            ? expression.subject.borrowAccess
            : subjectType !== undefined && Type.isReference(subjectType)
              ? subjectType.access
              : undefined
      if (inherited !== expression.borrowAccess) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceOperation', span: expression.span }))
      }
    }
    if (expression._tag === 'Call' || expression._tag === 'BuiltinCall') {
      const begins = expression.arguments
        .flatMap(expressionTree)
        .filter(
          (
            candidate,
          ): candidate is Extract<Expression, { readonly _tag: 'SliceBorrow' | 'ValueBorrow' }> =>
            (candidate._tag === 'SliceBorrow' || candidate._tag === 'ValueBorrow') &&
            (expression._tag === 'BuiltinCall' ||
              (candidate.borrow.callSpan.start === expression.span.start &&
                candidate.borrow.callSpan.end === expression.span.end)),
        )
        .map((candidate) => borrowKey(candidate.borrow))
      const ends = [
        ...expression.loanEnds,
        ...(expression._tag === 'BuiltinCall' ? expression.heldLoans : []),
      ].map(borrowKey)
      if (
        begins.length !== ends.length ||
        begins.some((begin, ordinal) => begin !== ends.at(ordinal)) ||
        new Set(ends).size !== ends.length
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidLoanEnd', span: expression.span }))
      }
    }
    if (expression._tag === 'Match') {
      const coverage = Match.cover(
        expression.members,
        expression.arms.map((arm) =>
          Object.freeze({
            ...(arm.member === undefined ? {} : { member: arm.member }),
            universal: arm.universal,
            guarded: arm.guard !== undefined,
          }),
        ),
      )
      for (const [index, arm] of expression.arms.entries()) {
        const transition = coverage.transitions.at(index)
        if (arm.id.ordinal !== index) {
          issues.push(Object.freeze({ _tag: 'InvalidMatchArmOrder', span: arm.span }))
        }
        if (
          transition === undefined ||
          transition.reachable !== arm.reachable ||
          !sameMembers(transition.before, arm.before) ||
          !sameMembers(transition.after, arm.after)
        ) {
          issues.push(Object.freeze({ _tag: 'InvalidMatchCoverage', span: arm.span }))
        }
        if (
          arm.guard !== undefined &&
          (arm.guard._tag === 'Unavailable' || !Type.equals(arm.guard.type, 'Bool'))
        ) {
          issues.push(Object.freeze({ _tag: 'InvalidMatchGuard', span: arm.guard.span }))
        }
        if (
          arm.result._tag === 'Unavailable' ||
          !TypeCompatibility.isCompatible(TypeCompatibility.check(arm.result.type, expression.type))
        ) {
          issues.push(Object.freeze({ _tag: 'InvalidMatchResult', span: arm.result.span }))
        }
        for (const binding of arm.bindings) {
          if (
            binding.id.arm.ordinal !== index ||
            binding.path.length === 0 ||
            binding.access !== expression.access
          ) {
            issues.push(Object.freeze({ _tag: 'InvalidPatternBinding', span: binding.span }))
          }
        }
      }
    }
    const children = expressionChildren(expression)
    for (const child of children) walk(child)
    active.delete(expression)
  }
  for (const fn of self.functions) {
    const statements = (body: ReadonlyArray<Statement>): void => {
      for (const statement of body) {
        if (statement._tag === 'Write' && statement.place._tag === 'BorrowedWritePlace') {
          const [first, ...rest] = statement.place.selectors
          const wellFormed = Type.isReference(statement.place.slice)
            ? statement.place.slice.access === 'Exclusive' &&
              statement.place.selectors.length > 0 &&
              statement.place.selectors.every((selector) => selector._tag === 'Field')
            : statement.place.slice.access === 'Exclusive' &&
              first?._tag === 'SliceIndex' &&
              Type.equals(first.slice, statement.place.slice) &&
              !rest.some((selector) => selector._tag !== 'Field')
          if (!wellFormed) {
            issues.push(Object.freeze({ _tag: 'InvalidBorrowedWrite', span: statement.place.span }))
          }
        }
        if (statement._tag === 'Unsafe') statements(statement.statements)
        else if (statement._tag === 'If') {
          statements(statement.taken)
          statements(statement.otherwise)
        } else if (statement._tag === 'While') statements(statement.body)
      }
    }
    statements(fn.statements)
    for (const expression of fn.statements.flatMap(statementExpressions)) walk(expression)
  }
  return Object.freeze(issues)
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
    ...(declaration.functionKind === 'Effect'
      ? {
          functionKind: 'Effect' as const,
          failures: declaration.failureRow.failures,
          requirements: declaration.requirementRow.requirements,
        }
      : {}),
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
    case 'PatternBindingReference':
      return `${indent}pattern-binding a${expression.binding.arm.ordinal}.b${expression.binding.ordinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'Move':
      return [
        `${indent}move : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'Replace':
      return [
        `${indent}replace : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.value, depth + 1),
      ].join('\n')
    case 'Run':
      return [
        `${indent}run : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'EffectCatch':
      return [
        `${indent}effect-catch ${Type.encode(expression.handled)} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.protected, depth + 1),
        encodeExpression(expression.handler, depth + 1),
      ].join('\n')
    case 'EffectRetry':
      return [
        `${indent}effect-retry : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.protected, depth + 1),
        encodeExpression(expression.retries, depth + 1),
      ].join('\n')
    case 'EffectTransform':
      return [
        `${indent}effect-${expression.operation.toLowerCase()} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.protected, depth + 1),
        encodeExpression(expression.callback, depth + 1),
      ].join('\n')
    case 'EffectProvide':
      return [
        `${indent}effect-provide ${Type.encode(expression.provider.capability)}@${expression.provider.role} ${expression.provider.access.toLowerCase()} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.protected, depth + 1),
      ].join('\n')
    case 'EffectProvideWith':
      return [
        `${indent}effect-provide-with ${Type.encode(expression.capability)}@${expression.role} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.protected, depth + 1),
        encodeExpression(expression.acquisition, depth + 1),
      ].join('\n')
    case 'EffectBlock':
      return [
        `${indent}effect-block site=fn${expression.site.function.ordinal}@${expression.site.span.start} access=${expression.type.access.toLowerCase()} captures=${expression.captures.map((capture) => `${capture.binding === undefined ? `p${capture.parameter?.ordinal ?? '?'}` : `b${capture.binding.ordinal}`}:${capture.access.toLowerCase()}`).join(',') || 'none'} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.statements.map((statement) => encodeStatement(statement, depth + 1)),
      ].join('\n')
    case 'UnionConvert':
      return [
        `${indent}union-${expression.conversion.toLowerCase()} ${Type.encode(expression.sourceType)} -> ${Type.encode(expression.target)} access=${expression.access} context=${expression.context} expected=${spanText(expression.expectedAt)} ${spanText(expression.span)}`,
        `${indent}  mapping ${expression.mappings.map((mapping) => `${Type.encode(mapping.source)}#${mapping.sourceOrdinal}->${Type.encode(mapping.target)}#${mapping.targetOrdinal}`).join(', ') || 'empty'}`,
        encodeExpression(expression.source, depth + 1),
      ].join('\n')
    case 'Match':
      return [
        `${indent}match ${expression.access.toLowerCase()} members=${expression.members.map(Type.encode).join(',') || 'none'} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        `${indent}  scrutinee`,
        encodeExpression(expression.scrutinee, depth + 2),
        ...expression.arms.flatMap((arm) => [
          `${indent}  arm #${arm.id.ordinal} ${arm.universal ? '_' : arm.member === undefined ? 'unknown' : Type.encode(arm.member)} reachable=${arm.reachable} before=${arm.before.map(Type.encode).join(',') || 'empty'} after=${arm.after.map(Type.encode).join(',') || 'empty'} ${spanText(arm.span)}`,
          ...arm.bindings.map(
            (binding) =>
              `${indent}    binding #${binding.id.ordinal} ${binding.name ?? '?'} path=${binding.path.map((field) => `#${field.ordinal}`).join('.') || 'root'} access=${binding.access} : ${Type.encode(binding.type)} ${spanText(binding.span)}`,
          ),
          `${indent}    cleanup ${arm.cleanup.map((path) => path.map((field) => `#${field.ordinal}`).join('.') || 'payload').join(',') || 'none'}`,
          ...(arm.guard === undefined
            ? []
            : [`${indent}    guard`, encodeExpression(arm.guard, depth + 3)]),
          `${indent}    result`,
          encodeExpression(arm.result, depth + 3),
        ]),
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
    case 'SliceBorrow':
      return `${indent}${expression.reborrow ? 'reborrow-slice' : 'borrow-slice'} l${expression.borrow.ordinal} ${expression.access.toLowerCase()} ${expression.root._tag === 'BindingSliceRoot' ? `b${expression.root.binding.ordinal}` : `p${expression.root.parameter.ordinal}`} source=${Type.encode(expression.source)} : ${Type.encode(expression.type)} suspended=${expression.suspendsParent} ${spanText(expression.span)}`
    case 'ValueBorrow':
      return `${indent}borrow-value l${expression.borrow.ordinal} ${expression.access.toLowerCase()} ${expression.root._tag === 'BindingSliceRoot' ? `b${expression.root.binding.ordinal}` : `p${expression.root.parameter.ordinal}`} source=${Type.encode(expression.source)} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'SliceLength':
      return [
        `${indent}slice-length : I32 ${spanText(expression.span)}`,
        encodeExpression(expression.slice, depth + 1),
      ].join('\n')
    case 'SliceIndexPlace':
      return [
        `${indent}slice-index ${expression.access.toLowerCase()} bounds=runtime : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.slice, depth + 1),
        encodeExpression(expression.index, depth + 1),
      ].join('\n')
    case 'FunctionItem':
      return `${indent}function-item ${
        expression.target._tag === 'DeclarationCallableTarget'
          ? `${expression.target.declaration.module}.${expression.target.declaration.name}`
          : `${expression.target.actor}.${expression.target.operation}`
      } : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'CallableSection':
      return [
        `${indent}callable-section site=fn${expression.site.function.ordinal}@${expression.site.span.start} mode=${expression.mode.toLowerCase()} omitted=p0 target=${
          expression.target._tag === 'DeclarationCallableTarget'
            ? `${expression.target.declaration.module}.${expression.target.declaration.name}`
            : `${expression.target.actor}.${expression.target.operation}`
        } : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.captures.map(
          (capture) =>
            `${indent}  capture #${capture.ordinal}->p${capture.parameterOrdinal} ${capture.access.toLowerCase()}\n${encodeExpression(capture.value, depth + 2)}`,
        ),
      ].join('\n')
    case 'CallableApply':
      return [
        `${indent}callable-apply access=${expression.access.toLowerCase()} evaluation=${expression.evaluation} realization=${expression.realization} substitution=${[...expression.substitution.entries()].map(([parameter, type]) => `${parameter}=${Type.encode(type)}`).join(',') || 'none'} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...(expression.evaluation === 'LeftThenCallable'
          ? [
              ...expression.arguments.map(
                (argument, ordinal) =>
                  `${indent}  argument #${ordinal}\n${encodeExpression(argument, depth + 2)}`,
              ),
              `${indent}  callee`,
              encodeExpression(expression.callee, depth + 2),
            ]
          : [
              `${indent}  callee`,
              encodeExpression(expression.callee, depth + 2),
              ...expression.arguments.map(
                (argument, ordinal) =>
                  `${indent}  argument #${ordinal}\n${encodeExpression(argument, depth + 2)}`,
              ),
            ]),
      ].join('\n')
    case 'Call':
    case 'EffectConstruct':
      return [
        `${indent}${expression._tag === 'EffectConstruct' ? 'effect-' : ''}call ${expression.target.module}.${expression.target.name}${
          expression.typeArguments.length === 0
            ? ''
            : `<${expression.typeArguments.map(Type.encode).join(', ')}>`
        } : ${Type.encode(expression.type)} loan-ends=${expression.loanEnds.map((loan) => `l${loan.ordinal}`).join(',') || 'none'} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'BuiltinCall': {
      const first = expression.arguments.at(0)
      const actor =
        first === undefined || first._tag === 'Unavailable' ? '?' : Type.encode(first.type)
      return [
        `${indent}builtin ${actor}.${expression.operation} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    }
    case 'Unavailable':
      return `${indent}unavailable ${spanText(expression.span)}`
  }
}

const encodeStatement = (statement: Statement, depth: number): string => {
  const indent = '  '.repeat(depth)
  switch (statement._tag) {
    case 'UnavailableStatement':
      return `${indent}unavailable-statement r${statement.region.ordinal} ${spanText(statement.span)}`
    case 'Unsafe':
      return [
        `${indent}unsafe r${statement.region.ordinal} ${spanText(statement.span)}`,
        ...statement.statements.map((inner) => encodeStatement(inner, depth + 1)),
      ].join('\n')
    case 'Bind':
      return [
        `${indent}bind ${statement.mutability.toLowerCase()} b${statement.binding.ordinal} ${statement.name ?? '?'} r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.initializer, depth + 1),
      ].join('\n')
    case 'Write':
      return [
        `${indent}write ${statement.place._tag === 'WritePlace' ? `b${statement.place.root.ordinal}` : `slice-p${statement.place.root.ordinal}`}${statement.place.selectors
          .map((selector) =>
            selector._tag === 'Field'
              ? `.#${selector.field.ordinal}`
              : selector._tag === 'SliceIndex'
                ? '[runtime/slice]'
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
    case 'Fail':
      return [
        `${indent}fail ${Type.encode(statement.failure)} r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.expression, depth + 1),
      ].join('\n')
    case 'Drop':
      return [
        `${indent}drop r${statement.region.ordinal} ${spanText(statement.span)}`,
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
      `fn ${identityLabel(fn.declaration)}${
        fn.declaration.typeParameters.length === 0
          ? ''
          : `<${fn.declaration.typeParameters.map((parameter) => Type.key(parameter.type)).join(',')}>`
      } ${contractText(fn.contract)} entry=r${fn.entryRegion.ordinal} regions=${fn.regionOrder.map((region) => `r${region.ordinal}`).join(',')}`,
      ...fn.statements.map((statement) => encodeStatement(statement, 1)),
    ]),
    '',
  ].join('\n')
