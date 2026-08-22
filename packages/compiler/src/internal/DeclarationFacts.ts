import type * as Constraint from '../Constraint.js'
import type * as DeclarationIndex from '../DeclarationIndex.js'
import type * as SyntaxTree from '../SyntaxTree.js'
import type * as Type from '../Type.js'

/** One function declaration header and its syntax-owned semantic facts. */
export interface DeclarationFact {
  readonly _tag: 'FunctionDeclaration'
  readonly id: DeclarationIndex.DeclarationId
  readonly canonical: DeclarationIndex.CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly functionKind: 'Ordinary' | 'Effect'
  readonly unsafe: boolean
  readonly typeParameters: ReadonlyArray<DeclarationIndex.TypeParameterFact>
  readonly parameterCount: number
  readonly parameters: ReadonlyArray<DeclarationIndex.ParameterFact>
  readonly name: DeclarationIndex.DeclaredName
  readonly returnType: DeclarationIndex.ReturnTypeFact
  readonly opaqueResult?: DeclarationIndex.OpaqueResultFact
  readonly failureRow: DeclarationIndex.FailureRowFact
  readonly requirementRow: DeclarationIndex.RequirementRowFact
  readonly constraints: ReadonlyArray<DeclarationIndex.ConstraintFact>
  readonly constraintContracts: ReadonlyArray<Constraint.Constraint>
  readonly conformanceImplementation?: {
    readonly ordinal: number
    readonly operation: string
    readonly self: Type.Parameter
  }
  readonly syntax: SyntaxTree.Node
}
