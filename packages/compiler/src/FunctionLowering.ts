import { cleanupForLocal, generated } from './CleanupEmission.js'
import type * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import type * as Layout from './Layout.js'
import type { ExecutableEffectType, ProvidedRequirement } from './Lower.js'
import { borrowKey, local, mirType } from './Lower.js'
import type * as Mir from './Mir.js'
import type * as OpaqueRealization from './OpaqueRealization.js'
import type * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import type { GeneratedEffectRunner } from './ValueType.js'
import {
  representedValueType,
  storedCallableValueType,
  storedEffectValueType,
} from './ValueType.js'

export class FunctionLowering {
  readonly regions: Array<Mir.Region | undefined> = []
  readonly localTypes: Array<Mir.Type> = []
  readonly bindingLocals = new Map<number, Mir.LocalId>()
  readonly parameterLocals = new Map<number, Mir.LocalId>()
  readonly effectRecipes = new Map<number, Hir.Expression>()
  readonly callableRecipes = new Map<number, Hir.Expression>()
  readonly effectLoanEnds = new Map<number, ReadonlyArray<Hir.BorrowId>>()
  readonly realizedRecipeBorrows = new Set<string>()
  readonly issuedBorrowKeys: Set<string>
  readonly patternLocals = new Map<string, Mir.LocalId>()
  readonly loanLocals = new Map<string, Mir.LocalId>()
  readonly loanIds = new Map<string, Hir.BorrowId>()
  readonly loanParents = new Map<string, string>()
  readonly slotLoans = new Map<number, ReadonlyArray<Hir.BorrowId>>()
  readonly callableDefinitions = new Map<
    number,
    Extract<Mir.Operation, { readonly _tag: 'MakeCallable' }>
  >()
  readonly temporaryBorrowOwners = new Map<
    string,
    {
      readonly local: Mir.LocalId
      readonly cleanup: CleanupPlan.CleanupPlan
      readonly span: SourceSpan.SourceSpan
    }
  >()
  private operations: Array<Mir.Operation> = []
  private syntheticBorrowOrdinal = 0
  private replayBorrowSubstitution: Map<string, Hir.BorrowId> | undefined

  constructor(
    readonly layout: Layout.Plan,
    readonly index: DeclarationIndex.Index,
    parameterTypes: ReadonlyArray<Mir.Type>,
    readonly ownership: Ownership.FunctionOwnership | undefined,
    readonly substitution: Type.Substitution,
    readonly effectOutcome: Type.Effect | undefined,
    readonly owner: Instances.Instance,
    readonly instances: ReadonlyArray<Instances.Instance>,
    readonly calls: ReadonlyArray<Instances.CallInstance>,
    readonly effectResults: ReadonlyMap<string, ExecutableEffectType>,
    readonly generatedRunners: Array<GeneratedEffectRunner>,
    readonly opaqueRealizations: OpaqueRealization.Catalog,
    readonly providedRequirements: ReadonlyArray<ProvidedRequirement> = Object.freeze([]),
  ) {
    this.issuedBorrowKeys = new Set((ownership?.loans ?? []).map((loan) => borrowKey(loan.id)))
    this.localTypes.push(...parameterTypes)
    parameterTypes.forEach((_, ordinal) => {
      this.parameterLocals.set(ordinal, local(ordinal))
    })
  }

  reserve(): Mir.RegionId {
    const id = Object.freeze({ _tag: 'Region' as const, ordinal: this.regions.length })
    this.regions.push(undefined)
    return id
  }

  freshSyntheticBorrow(span: SourceSpan.SourceSpan): Hir.BorrowId {
    while (true) {
      const borrow: Hir.BorrowId = Object.freeze({
        _tag: 'BorrowId',
        function: this.owner.function.declaration.id,
        callSpan: span,
        ordinal: this.syntheticBorrowOrdinal,
      })
      this.syntheticBorrowOrdinal += 1
      const key = borrowKey(borrow)
      if (this.issuedBorrowKeys.has(key)) continue
      this.issuedBorrowKeys.add(key)
      return borrow
    }
  }

  withRecipeReplay<A>(body: () => A): A {
    if (this.replayBorrowSubstitution !== undefined) return body()
    this.replayBorrowSubstitution = new Map()
    try {
      return body()
    } finally {
      this.replayBorrowSubstitution = undefined
    }
  }

  beginRecipeBorrow(authored: Hir.BorrowId): Hir.BorrowId {
    if (this.replayBorrowSubstitution === undefined) return authored
    const key = borrowKey(authored)
    const existing = this.replayBorrowSubstitution.get(key)
    if (existing !== undefined) return existing
    const realized = this.realizedRecipeBorrows.has(key)
      ? this.freshSyntheticBorrow(authored.callSpan)
      : authored
    this.issuedBorrowKeys.add(borrowKey(realized))
    this.realizedRecipeBorrows.add(key)
    this.replayBorrowSubstitution.set(key, realized)
    return realized
  }

  recipeBorrow(authored: Hir.BorrowId): Hir.BorrowId {
    return this.replayBorrowSubstitution?.get(borrowKey(authored)) ?? authored
  }

  publish(region: Mir.Region): void {
    this.regions[region.id.ordinal] = region
  }

  capture<A>(body: () => A): readonly [A, ReadonlyArray<Mir.Operation>] {
    const previous = this.operations
    this.operations = []
    const result = body()
    const operations = Object.freeze([...this.operations])
    this.operations = previous
    return [result, operations]
  }

  alloc(type: Mir.Type): Mir.LocalId {
    const id = local(this.localTypes.length)
    this.localTypes.push(type)
    return id
  }

  emit(operation: Mir.Operation): void {
    this.operations.push(operation)
    if (operation._tag === 'BeginLoan') {
      const key = borrowKey(operation.borrow)
      const parent = [...this.loanLocals.entries()].find(
        ([, slice]) => slice.ordinal === operation.root.ordinal,
      )
      if (parent !== undefined) this.loanParents.set(key, parent[0])
      this.loanIds.set(key, operation.borrow)
    } else if (operation._tag === 'EndLoan') {
      this.loanIds.set(borrowKey(operation.borrow), operation.borrow)
      const temporary = this.temporaryBorrowOwners.get(borrowKey(operation.borrow))
      if (temporary !== undefined) {
        const localType = this.localTypes.at(temporary.local.ordinal)
        if (localType !== undefined) {
          this.operations.push(
            Object.freeze({
              _tag: 'Drop',
              local: temporary.local,
              cleanup: cleanupForLocal(this, temporary.cleanup, localType),
              provenance: generated(temporary.span),
            }),
          )
        }
        this.temporaryBorrowOwners.delete(borrowKey(operation.borrow))
      }
    }
    if (operation._tag === 'MakeCallable')
      this.callableDefinitions.set(operation.destination.ordinal, operation)
    if (operation._tag === 'Move') {
      const definition = this.callableDefinitions.get(operation.source.ordinal)
      if (definition !== undefined)
        this.callableDefinitions.set(operation.destination.ordinal, definition)
    }
  }

  type(type: Type.Type): Mir.Type | undefined {
    const specialized = Type.substitute(type, this.substitution)
    return (
      storedCallableValueType(this.layout, specialized) ??
      storedEffectValueType(this.layout, specialized) ??
      representedValueType(this.layout, this.opaqueRealizations, type, this.substitution) ??
      mirType(specialized, new Map(), this.layout)
    )
  }

  semantic(type: Type.Type): Type.Type {
    return Type.substitute(type, this.substitution)
  }

  semanticArgument(argument: Type.GenericArgument): Type.GenericArgument {
    return Type.substituteGenericArgument(argument, this.substitution)
  }

  call(
    span: SourceSpan.SourceSpan,
    implementation?: DeclarationFacts.CanonicalId,
  ): Instances.CallInstance | undefined {
    const exact = this.calls.filter(
      (call) =>
        Instances.keyText(call.owner) === Instances.keyText(this.owner.key) &&
        call.span.sourceId === span.sourceId &&
        call.span.start === span.start &&
        call.span.end === span.end,
    )
    const selected =
      implementation === undefined
        ? exact
        : exact.filter(
            (call) =>
              call.target.declaration.module === implementation.module &&
              call.target.declaration.name === implementation.name,
          )
    return selected.length === 1 ? selected.at(0) : undefined
  }
}
