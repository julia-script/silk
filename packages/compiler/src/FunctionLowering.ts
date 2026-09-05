import { cleanupForLocal, generated, indexExits } from './CleanupEmission.js'
import type { ExitIndex } from './CleanupEmission.js'
import type * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import type * as Layout from './Layout.js'
import type { ExecutableEffectType, ProvidedRequirement } from './Lower.js'
import { borrowKey, local, mirType } from './Lower.js'
import type * as Mir from './Mir.js'
import type * as MovePath from './MovePath.js'
import type * as OpaqueRealization from './OpaqueRealization.js'
import type * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'
import * as StaticValue from './StaticValue.js'
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
  readonly initializationFlags = new Map<
    string,
    ReadonlyArray<{ readonly path: MovePath.Path; readonly local: Mir.LocalId }>
  >()
  initializationStarted = false
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
  readonly expressionLocals = new Map<string, Mir.LocalId>()
  readonly matchCleanupLocals = new Map<string, Mir.LocalId>()
  readonly extractedRegions = new Set<number>()
  readonly exits: ExitIndex
  ownerLoop: Mir.LoopId | undefined
  activeRequirements: ReadonlyArray<ProvidedRequirement> | undefined
  private operations: Array<Mir.Operation> = []
  private syntheticBorrowOrdinal = 0
  private replayBorrowSubstitution: Map<string, Hir.BorrowId> | undefined
  private readonly directBorrowSubstitution = new Map<string, Hir.BorrowId>()

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
    this.exits = indexExits(ownership)
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
    const key = borrowKey(authored)
    if (this.replayBorrowSubstitution === undefined) {
      const realized = this.realizedRecipeBorrows.has(key)
        ? this.freshSyntheticBorrow(authored.callSpan)
        : authored
      this.issuedBorrowKeys.add(borrowKey(realized))
      this.realizedRecipeBorrows.add(key)
      this.directBorrowSubstitution.set(key, realized)
      return realized
    }
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
    const key = borrowKey(authored)
    return (
      this.replayBorrowSubstitution?.get(key) ?? this.directBorrowSubstitution.get(key) ?? authored
    )
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

  /** Captures an eager region graph without changing the enclosing operation sequence. */
  captureExecution(
    body: () => { readonly entry: Mir.RegionId; readonly result?: Mir.LocalId } | undefined,
  ): Mir.Execution | undefined {
    const first = this.regions.length
    const result = body()
    if (result === undefined) return undefined
    const regions: Array<Mir.Region> = []
    for (let ordinal = first; ordinal < this.regions.length; ordinal += 1) {
      const region = this.regions.at(ordinal)
      if (region === undefined) {
        if (!this.extractedRegions.has(ordinal)) return undefined
        continue
      }
      regions.push(region)
      this.extractedRegions.add(ordinal)
      this.regions[ordinal] = undefined
    }
    return Object.freeze({ ...result, regions: Object.freeze(regions) })
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
    // A staged section's captures are only the tail of its environment, so it never serves as a
    // complete definition for the erased checked-scalar fast path.
    if (operation._tag === 'MakeCallable' && operation.base === undefined)
      this.callableDefinitions.set(operation.destination.ordinal, operation)
    if (operation._tag === 'Move') {
      const definition = this.callableDefinitions.get(operation.source.ordinal)
      if (definition !== undefined)
        this.callableDefinitions.set(operation.destination.ordinal, definition)
    }
  }

  type(type: Type.Type): Mir.Type | undefined {
    const specialized = Type.substitute(
      type,
      this.substitution,
      this.owner.specialization.compatibility,
    )
    return (
      storedCallableValueType(this.layout, specialized) ??
      storedEffectValueType(this.layout, specialized) ??
      representedValueType(this.layout, this.opaqueRealizations, type, this.substitution) ??
      mirType(specialized, new Map(), this.layout)
    )
  }

  semantic(type: Type.Type): Type.Type {
    return Type.substitute(type, this.substitution, this.owner.specialization.compatibility)
  }

  semanticArgument(argument: Type.GenericArgument): Type.GenericArgument {
    return Type.substituteGenericArgument(
      argument,
      this.substitution,
      this.owner.specialization.compatibility,
    )
  }

  call(
    span: SourceSpan.SourceSpan,
    implementation?: DeclarationFacts.CanonicalId,
    typeArguments?: ReadonlyArray<Type.GenericArgument>,
    staticArguments?: ReadonlyArray<StaticValue.Value>,
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
    const expected = typeArguments?.filter((argument) => !Type.isHiddenExecutableArgument(argument))
    const specialized =
      expected === undefined
        ? selected
        : selected.filter((call) => {
            const actual = call.target.typeArguments.filter(
              (argument) => !Type.isHiddenExecutableArgument(argument),
            )
            return (
              actual.length === expected.length &&
              actual.every((argument, ordinal) => {
                const wanted = expected.at(ordinal)
                return wanted !== undefined && Type.equalsGenericArgument(argument, wanted)
              })
            )
          })
    const staticallySpecialized =
      staticArguments === undefined || staticArguments.length === 0
        ? specialized
        : specialized.filter(
            (call) =>
              call.target.staticArguments.length === staticArguments.length &&
              call.target.staticArguments.every((argument, ordinal) => {
                const wanted = staticArguments.at(ordinal)
                return wanted !== undefined && StaticValue.equals(argument, wanted)
              }),
          )
    return staticallySpecialized.length === 1 ? staticallySpecialized.at(0) : undefined
  }
}
