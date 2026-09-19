import type * as AuthoredHir from './AuthoredHir.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as Lifetime from './Lifetime.js'
import type * as Match from './Match.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as Tir from './Tir.js'
import * as Type from './Type.js'

/**
 * Exactly what loan analysis reads of a body.
 *
 * Loan analysis asks a small number of questions of each expression: what kind it is, where it was
 * written, its type, which local it names, how a borrow was formed. This is that vocabulary and
 * nothing else, so the analysis depends on these questions rather than on one body representation.
 */
export type ExpressionType =
  | { readonly _tag: 'Available'; readonly type: DeclarationFacts.SemanticType }
  | { readonly _tag: 'Unavailable' }

interface Base {
  readonly type: ExpressionType
  readonly anchor: AuthoredHir.Anchor
}

/** A local as loan analysis names it: its id, its spelling and, for a parameter, its declared type. */
export type Local =
  | {
      readonly _tag: 'BindingFact'
      readonly id: Tir.BindingId
      readonly name: Name
      readonly inferredType: ExpressionType
    }
  | {
      readonly _tag: 'PatternBinding'
      readonly id: Match.BindingId
      readonly name: Name
      readonly type: ExpressionType
    }
  | {
      readonly _tag: 'ParameterDeclaration'
      readonly id: DeclarationFacts.ParameterId
      readonly name: Name
      readonly declaredType: DeclarationFacts.DeclaredTypeFact
    }

export type Name =
  | { readonly _tag: 'Present'; readonly spelling: string }
  | { readonly _tag: 'Unavailable' }

export type Reference =
  | {
      readonly _tag: 'ResolvedBinding'
      readonly binding: Extract<Local, { readonly _tag: 'BindingFact' }>
      readonly spelling: string
    }
  | {
      readonly _tag: 'ResolvedPattern'
      readonly binding: Extract<Local, { readonly _tag: 'PatternBinding' }>
      readonly spelling: string
    }
  | {
      readonly _tag: 'Resolved'
      readonly parameter: Extract<Local, { readonly _tag: 'ParameterDeclaration' }>
      readonly spelling: string
    }
  | { readonly _tag: 'Missing' | 'Ambiguous' | 'Unavailable' | 'Conflict' | 'Inaccessible' }

/** One step of a borrowed place. Only an index step evaluates anything. */
export type Selector =
  | {
      readonly _tag: 'Field'
      readonly field: DeclarationFacts.FieldId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Index'
      readonly index: Expression
      readonly array: Type.FixedArray
      readonly bounds:
        | { readonly _tag: 'Proven'; readonly index: number; readonly length: number }
        | { readonly _tag: 'Runtime'; readonly length: number }
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'SliceIndex'
      readonly index: Expression
      readonly slice: Type.Slice
      readonly span: SourceSpan.SourceSpan
    }

export type BorrowRoot =
  | {
      readonly _tag: 'BindingRoot'
      readonly binding: { readonly id: Tir.BindingId }
      readonly path: ReadonlyArray<Selector>
    }
  | {
      readonly _tag: 'ParameterRoot'
      readonly parameter: { readonly id: DeclarationFacts.ParameterId }
      readonly path: ReadonlyArray<Selector>
    }
  | {
      readonly _tag: 'PatternRoot'
      readonly binding: { readonly id: Match.BindingId }
      readonly path: ReadonlyArray<Selector>
    }
  | {
      readonly _tag: 'TemporaryRoot'
      readonly owner: Tir.TemporaryOwnerId
      readonly path: ReadonlyArray<Selector>
    }

export type Formation =
  | { readonly _tag: 'FixedArrayBorrow'; readonly root: BorrowRoot }
  | { readonly _tag: 'ValueBorrow'; readonly root: BorrowRoot }
  | {
      readonly _tag: 'SliceReborrow' | 'ValueReborrow'
      readonly root: BorrowRoot
      readonly suspendsParent: boolean
    }
  | { readonly _tag: 'Unavailable' }

export interface Argument {
  readonly id: { readonly ordinal: number }
  readonly expression: Expression
  readonly type: ExpressionType
}

export type ArmBody =
  | { readonly _tag: 'Expression'; readonly expression: Expression }
  | { readonly _tag: 'Block'; readonly statements: ReadonlyArray<Statement> }

export type Expression =
  | (Base & {
      readonly _tag:
        | 'Integer'
        | 'Duration'
        | 'Floating'
        | 'Boolean'
        | 'Character'
        | 'Constant'
        | 'ForeignStatic'
        | 'StaticText'
        | 'Unit'
        | 'EnumMember'
        | 'FunctionItem'
    })
  | (Base & { readonly _tag: 'Identifier'; readonly reference: Reference })
  | (Base & { readonly _tag: 'Move' | 'ReferentProjection'; readonly subject: Expression })
  | (Base & {
      readonly _tag: 'FieldProjection'
      readonly subject: Expression
      readonly state:
        | { readonly _tag: 'Resolved'; readonly field: { readonly id: DeclarationFacts.FieldId } }
        | { readonly _tag: 'SliceLength' | 'Unavailable' }
    })
  | (Base & {
      readonly _tag: 'IndexProjection'
      readonly subject: Expression
      readonly index: Expression
      readonly array?: Type.FixedArray
      readonly bounds:
        | { readonly _tag: 'Proven'; readonly index: number; readonly length: number }
        | { readonly _tag: 'Runtime'; readonly length: number }
        | { readonly _tag: 'Invalid' | 'Unavailable' | 'RuntimeSlice' }
    })
  | (Base & {
      readonly _tag: 'Borrow'
      readonly access: Type.BorrowAccess
      readonly subject: Expression
      readonly formation: Formation
    })
  | (Base & {
      readonly _tag: 'StructLiteral' | 'UnionVariant'
      readonly initializers: ReadonlyArray<{ readonly expression: Expression }>
    })
  | (Base & {
      readonly _tag: 'ArrayLiteral'
      readonly elements: ReadonlyArray<{ readonly expression: Expression }>
    })
  | (Base & {
      readonly _tag: 'Match'
      readonly scrutinee: Expression
      readonly arms: ReadonlyArray<{ readonly guard?: Expression; readonly body: ArmBody }>
    })
  | (Base & {
      readonly _tag: 'Operator'
      readonly arguments: ReadonlyArray<Argument>
      /** The declared operand types, when the operator is a bound interface operation. */
      readonly interfaceOperation?: {
        readonly contract: {
          readonly operands: ReadonlyArray<{
            readonly type: DeclarationFacts.DeclaredTypeFact
          }>
        }
      }
    })
  | (Base & { readonly _tag: 'ShortCircuit'; readonly arguments: ReadonlyArray<Argument> })
  | (Base & {
      readonly _tag: 'Call'
      readonly arguments: ReadonlyArray<Argument>
      readonly reference: { readonly _tag: string; readonly operation?: unknown }
    })
  | (Base & {
      readonly _tag: 'ForeignApply'
      readonly callee: Expression
      readonly arguments: ReadonlyArray<Argument>
      readonly evaluation: 'CalleeThenArguments' | 'LeftThenCallable'
    })
  | (Base & {
      readonly _tag: 'CallableApply'
      readonly callee: Expression
      readonly arguments: ReadonlyArray<Argument>
      readonly mode: Type.CallableMode
      readonly staged?: unknown
      readonly provenance: { readonly _tag: string }
    })
  | (Base & {
      readonly _tag: 'CallableSection'
      readonly captures: ReadonlyArray<{
        readonly ordinal: number
        readonly expression: Expression
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }>
    })
  | (Base & {
      readonly _tag: 'EffectBlock'
      readonly captures: ReadonlyArray<{
        readonly reference: Local
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
        readonly span: SourceSpan.SourceSpan
        readonly expression?: { readonly anchor: AuthoredHir.Anchor }
      }>
      readonly bindings: ReadonlyArray<Binding>
      readonly statements: ReadonlyArray<Statement>
    })
  | (Base & { readonly _tag: 'Run'; readonly subject: Expression })
  | (Base & {
      readonly _tag: 'EffectCatch'
      readonly protected: Expression
      readonly handler: Expression
    })
  | (Base & {
      readonly _tag: 'EffectBindRequirement'
      readonly protected: Expression
      readonly provider?: {
        readonly reference: Exclude<Local, { readonly _tag: 'PatternBinding' }>
        readonly captureAccess: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
        readonly span: SourceSpan.SourceSpan
      }
    })
  | (Base & {
      readonly _tag: 'PlaceReplace'
      readonly destination: Expression
      readonly value: Expression
    })
  | (Base & { readonly _tag: 'EnumValue'; readonly argument: Expression })
  | (Base & { readonly _tag: 'CompileError'; readonly message: Expression })

export interface Binding {
  readonly id: Tir.BindingId
  readonly initializer: Expression
  readonly inferredType: ExpressionType
}

interface Selection {
  readonly source: Expression
  readonly loanEnd: SourceSpan.SourceSpan
  readonly anchor: AuthoredHir.Anchor
}

export type Statement =
  | {
      readonly _tag: 'UnsafeStatement'
      readonly statements: ReadonlyArray<Statement>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | { readonly _tag: 'BindStatement'; readonly binding: Binding; readonly region: Tir.RegionId }
  | {
      readonly _tag: 'PatternBindStatement'
      readonly selection: Selection
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'ExpressionStatement' | 'ReturnStatement' | 'FailStatement' | 'DropStatement'
      readonly expression: Expression
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'IfStatement'
      readonly condition: Expression
      readonly taken: ReadonlyArray<Statement>
      readonly otherwise: ReadonlyArray<Statement>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'IfLetStatement'
      readonly selection: Selection
      readonly taken: ReadonlyArray<Statement>
      readonly otherwise: ReadonlyArray<Statement>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'WriteStatement'
      readonly destination: Expression
      readonly value: Expression
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'WhileStatement'
      readonly condition: Expression
      readonly body: ReadonlyArray<Statement>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'BreakStatement' | 'ContinueStatement'
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }

/** One body as loan analysis reads it. */
export interface Body {
  readonly declaration: DeclarationFacts.DeclarationFact
  readonly statements: ReadonlyArray<Statement>
  readonly bindings: ReadonlyArray<Binding>
  readonly lifetimeFlow?: import('./LifetimeFlow.js').LifetimeFlow
}

/** The expressions one expression evaluates directly, in the order loan analysis visits them. */
export const children = (self: Expression): ReadonlyArray<Expression> => {
  switch (self._tag) {
    case 'Move':
    case 'ReferentProjection':
    case 'FieldProjection':
    case 'Borrow':
    case 'Run':
      return [self.subject]
    case 'IndexProjection':
      return [self.subject, self.index]
    case 'StructLiteral':
    case 'UnionVariant':
      return self.initializers.map((initializer) => initializer.expression)
    case 'ArrayLiteral':
      return self.elements.map((element) => element.expression)
    case 'Match':
      return [
        self.scrutinee,
        ...self.arms.flatMap((arm) => [
          ...(arm.guard === undefined ? [] : [arm.guard]),
          ...(arm.body._tag === 'Expression' ? [arm.body.expression] : []),
        ]),
      ]
    case 'Operator':
    case 'ShortCircuit':
    case 'Call':
      return self.arguments.map((argument) => argument.expression)
    case 'ForeignApply':
    case 'CallableApply':
      return [self.callee, ...self.arguments.map((argument) => argument.expression)]
    case 'CallableSection':
      return self.captures.map((capture) => capture.expression)
    case 'EffectCatch':
      return [self.protected, self.handler]
    case 'EffectBindRequirement':
      return [self.protected]
    case 'PlaceReplace':
      return [self.destination, self.value]
    case 'EnumValue':
      return [self.argument]
    case 'CompileError':
      return [self.message]
    default:
      return []
  }
}

const statementExpressions = (statement: Statement): ReadonlyArray<Expression> => {
  switch (statement._tag) {
    case 'BindStatement':
      return [statement.binding.initializer]
    case 'PatternBindStatement':
    case 'IfLetStatement':
      return [statement.selection.source]
    case 'ExpressionStatement':
    case 'ReturnStatement':
    case 'FailStatement':
    case 'DropStatement':
      return [statement.expression]
    case 'IfStatement':
    case 'WhileStatement':
      return [statement.condition]
    case 'WriteStatement':
      return [statement.destination, statement.value]
    default:
      return []
  }
}

const visitExpression = (self: Expression, visit: (expression: Expression) => void): void => {
  visit(self)
  if (self._tag === 'Match') {
    visitExpression(self.scrutinee, visit)
    for (const arm of self.arms) {
      if (arm.guard !== undefined) visitExpression(arm.guard, visit)
      if (arm.body._tag === 'Expression') visitExpression(arm.body.expression, visit)
      else visitExpressions(arm.body.statements, visit)
    }
    return
  }
  if (self._tag === 'EffectBlock') {
    visitExpressions(self.statements, visit)
    return
  }
  for (const child of children(self)) visitExpression(child, visit)
}

/** Visits every expression of a body in source order, nested blocks included. */
export const visitExpressions = (
  self: ReadonlyArray<Statement>,
  visit: (expression: Expression) => void,
): void => {
  for (const statement of self) {
    for (const expression of statementExpressions(statement)) visitExpression(expression, visit)
    if (statement._tag === 'UnsafeStatement') visitExpressions(statement.statements, visit)
    else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
      visitExpressions(statement.taken, visit)
      visitExpressions(statement.otherwise, visit)
    } else if (statement._tag === 'WhileStatement') visitExpressions(statement.body, visit)
  }
}

/** Arguments whose access capability, or owned payload, is retained by the selected result. */
export const retainedResultArguments = (
  self: Expression,
  assumptions: Lifetime.Assumptions,
): ReadonlyArray<Argument> => {
  if (
    (self._tag !== 'Call' && self._tag !== 'CallableApply' && self._tag !== 'Operator') ||
    self.type._tag !== 'Available'
  )
    return []
  const result = self.type.type
  return self.arguments.filter((argument) => {
    if (argument.type._tag !== 'Available') return false
    const source = argument.type.type
    if (Type.isReference(source) || Type.isSlice(source))
      return Type.storageLifetimes(result).some(
        (output) =>
          output._tag !== 'StaticLifetime' &&
          Lifetime.outlives(assumptions, source.lifetime, output),
      )
    return retainsLifetimes(source, result, assumptions)
  })
}

export const retainsLifetimes = (
  source: DeclarationFacts.SemanticType,
  result: DeclarationFacts.SemanticType,
  assumptions: Lifetime.Assumptions,
): boolean => {
  const required = Type.storageLifetimes(result).filter(
    (lifetime) => lifetime._tag !== 'StaticLifetime',
  )
  return (
    required.length > 0 &&
    Type.storageLifetimes(source).some((lifetime) =>
      required.some((output) => Lifetime.outlives(assumptions, lifetime, output)),
    )
  )
}
