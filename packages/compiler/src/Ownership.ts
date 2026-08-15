import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
import type * as Match from './Match.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/**
 * The ownership and scope phase over typed HIR. It runs once per declaration and is a producer:
 * ownership facts plus the target-neutral cleanup plan MIR lowering consumes to insert drops.
 * Bindings cover parameters and `let` statements; an explicit `move` consumes its binding even
 * for copyable types, and later uses are `OWN0001` violations.
 */

/** The ownership category of one binding. Nominal structs are whole-value move-only owners. */
export type OwnershipCategory =
  | { readonly _tag: 'Copyable' }
  | { readonly _tag: 'MoveOnly'; readonly type: DeclarationIndex.SemanticType }

/** Where one binding was introduced: a parameter or a `let` statement. */
export type BindingSite =
  | { readonly _tag: 'Parameter'; readonly parameter: DeclarationIndex.ParameterId }
  | { readonly _tag: 'Let'; readonly binding: Hir.BindingId }
  | { readonly _tag: 'Pattern'; readonly binding: Match.BindingId }

/** One binding's ownership fact: site, category, live range, and consuming move if any. */
export interface BindingFact {
  readonly _tag: 'Binding'
  readonly site: BindingSite
  readonly name: string | undefined
  readonly mutability: 'Immutable' | 'Mutable'
  readonly category: OwnershipCategory
  readonly type?: DeclarationIndex.SemanticType
  readonly cleanup: CleanupPlan
  readonly liveFrom: SourceSpan.SourceSpan
  readonly liveTo: SourceSpan.SourceSpan
  readonly movedAt?: SourceSpan.SourceSpan
}

/** One ordered release of an owned binding at a structured exit. */
export interface Release {
  readonly _tag: 'Release'
  readonly binding: BindingFact
  readonly fields: ReadonlyArray<DeclarationIndex.FieldId>
  readonly cleanup: CleanupPlan
}

/** A deterministic compiler-only identity for one direct-call or delayed-effect slice loan. */
export type BorrowId = Hir.BorrowId

export interface LoanFact {
  readonly _tag: 'Loan'
  readonly id: BorrowId
  readonly root: BindingSite
  readonly access: Type.Slice['access']
  readonly origin:
    | 'FixedArrayBorrow'
    | 'SliceReborrow'
    | 'ValueBorrow'
    | 'EffectCapture'
    | 'CallableCapture'
    | 'ReturnedView'
  readonly parent?: BindingSite
  readonly suspendsParent: boolean
  readonly startRegion: Hir.RegionId
  readonly endRegion: Hir.RegionId
  readonly startSpan: SourceSpan.SourceSpan
  readonly endSpan: SourceSpan.SourceSpan
}

export interface BorrowedReplacementFact {
  readonly _tag: 'BorrowedReplacement'
  readonly root: DeclarationIndex.ParameterId
  readonly region: Hir.RegionId
  readonly type: DeclarationIndex.SemanticType
  readonly displacedCleanup: CleanupPlan
  readonly span: SourceSpan.SourceSpan
}

/** One compiler-planned slot in a concrete callable section environment. */
export interface CallableEnvironmentSlot {
  readonly ordinal: number
  readonly parameterOrdinal: number
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
  readonly type?: DeclarationIndex.SemanticType
  readonly cleanup: CleanupPlan
}

/** Ownership facts for one hidden callable section environment. */
export interface CallableEnvironmentFact {
  readonly _tag: 'CallableEnvironment'
  readonly site: Hir.CallableSiteId
  readonly mode: Type.CallableMode
  readonly slots: ReadonlyArray<CallableEnvironmentSlot>
  readonly retainedDependencies: ReadonlyArray<number>
  readonly dropOrder: ReadonlyArray<number>
  readonly span: SourceSpan.SourceSpan
}

/** The symbolic recursive cleanup of one complete logical owner. */
export type CleanupPlan =
  | { readonly _tag: 'NoCleanup'; readonly type: DeclarationIndex.SemanticType }
  | { readonly _tag: 'ParameterCleanup'; readonly type: Type.Parameter }
  | {
      readonly _tag: 'AllocationCleanup'
      readonly type: Type.Nominal
      /** Compiler-private proof that exactly one reclaim obligation is still active. */
      readonly ticket: 'ActiveReclaimTicket'
    }
  | {
      readonly _tag: 'RawBufferCleanup'
      readonly type: Type.Nominal
      readonly allocation: Extract<CleanupPlan, { readonly _tag: 'AllocationCleanup' }>
    }
  | {
      readonly _tag: 'HookCleanup'
      readonly type: Type.Nominal
      /** The hidden hook function synthesized from the owning Drop conformance. */
      readonly hook: DeclarationIndex.CanonicalId
      /** Concrete arguments for the impl's type parameters at this instantiation. */
      readonly typeArguments: ReadonlyArray<Type.Type>
      /** Field cleanup runs after the hook returns. */
      readonly inner: CleanupPlan
    }
  | {
      readonly _tag: 'StructCleanup'
      readonly type: Type.Nominal
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationIndex.FieldId
        readonly cleanup: CleanupPlan
      }>
    }
  | {
      readonly _tag: 'ArrayCleanup'
      readonly type: Type.FixedArray
      readonly length: number
      readonly element: CleanupPlan
    }
  | {
      readonly _tag: 'UnionCleanup'
      readonly type: Type.StructuralUnion
      readonly cases: ReadonlyArray<{
        readonly member: Type.Nominal
        readonly ordinal: number
        readonly cleanup: CleanupPlan
      }>
    }
  | {
      readonly _tag: 'CallableCleanup'
      readonly type: Type.Callable
      readonly site: Hir.CallableSiteId
      /** Moved environment slots in deterministic last-captured, first-released order. */
      readonly slots: ReadonlyArray<{
        readonly ordinal: number
        readonly cleanup: CleanupPlan
      }>
    }
  | {
      readonly _tag: 'EffectCleanup'
      readonly type: Type.Effect
      readonly site: Hir.EffectSiteId
      /** Owned environment slots in deterministic last-captured, first-released order. */
      readonly slots: ReadonlyArray<{
        readonly ordinal: number
        readonly laneOffset: number
        readonly laneCount: number
        readonly cleanup: CleanupPlan
      }>
    }

/** One structured exit path with its ordered (last-acquired, first-released) releases. */
export interface ExitPlan {
  readonly _tag: 'Exit'
  readonly kind:
    | 'Return'
    | 'ScopeEnd'
    | 'ArmEnd'
    | 'LoopFallthrough'
    | 'Break'
    | 'Continue'
    | 'Propagation'
  readonly span: SourceSpan.SourceSpan
  readonly region?: Hir.RegionId
  readonly arm?: 'Taken' | 'Otherwise'
  readonly target?: Hir.LoopId
  readonly loanEnds: ReadonlyArray<BorrowId>
  readonly releases: ReadonlyArray<Release>
}

/** The finite owner-liveness states used to establish one deterministic loop header. */
export interface LoopFixedPoint {
  readonly _tag: 'LoopFixedPoint'
  readonly loop: Hir.LoopId
  readonly span: SourceSpan.SourceSpan
  readonly incoming: ReadonlyArray<BindingSite>
  readonly repeating: ReadonlyArray<ReadonlyArray<BindingSite>>
  readonly following: ReadonlyArray<BindingSite>
  readonly compatible: boolean
  readonly iterations: number
}

/** The closed outcome of checking one function. */
export type Verdict =
  | { readonly _tag: 'Satisfied' }
  | { readonly _tag: 'Violation'; readonly cause: Diagnostic.Identity }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

/** One function's ownership facts and its target-neutral cleanup plan. */
export interface FunctionOwnership {
  readonly _tag: 'FunctionOwnership'
  readonly declaration: DeclarationIndex.DeclarationFact
  /**
   * Bindings this function's own statements introduce: parameters, `let` statements, and match
   * patterns the enclosing flow reaches. Deliberately excludes deferred effect bodies, so it is
   * not the whole set of bindings the function owns — an `effect fn` is entirely a deferred
   * body, and publishes little beyond its parameters here. Read {@link allBindings} instead
   * whenever completeness matters; reach for this field only to ask the narrower question of
   * what the enclosing flow itself introduced.
   */
  readonly bindings: ReadonlyArray<BindingFact>
  /**
   * Bindings owned by deferred effect bodies: published separately because their releases lower
   * through the body's compiled runner, not through the enclosing function's statements. An
   * `effect fn`'s whole body is deferred, so its `let` and pattern bindings arrive here rather
   * than in {@link FunctionOwnership.bindings}.
   */
  readonly deferredBindings: ReadonlyArray<BindingFact>
  readonly exits: ReadonlyArray<ExitPlan>
  readonly fixedPoints: ReadonlyArray<LoopFixedPoint>
  readonly matches: ReadonlyArray<MatchOwnership>
  readonly callables: ReadonlyArray<CallableEnvironmentFact>
  readonly loans: ReadonlyArray<LoanFact>
  readonly borrowedReplacements: ReadonlyArray<BorrowedReplacementFact>
  readonly verdict: Verdict
}

/**
 * Every binding one function owns, enclosing statements and deferred effect bodies alike. The
 * two fact sets are published apart because their releases lower through different bodies, so a
 * consumer asking "what does this function own?" — cleanup emission, drop lowering — must join
 * them rather than read {@link FunctionOwnership.bindings} and silently miss an `effect fn`'s
 * entire body.
 */
export const allBindings = (
  ownership: FunctionOwnership | undefined,
): ReadonlyArray<BindingFact> =>
  ownership === undefined
    ? Object.freeze([])
    : Object.freeze([...ownership.bindings, ...ownership.deferredBindings])

export interface MatchOwnership {
  readonly _tag: 'MatchOwnership'
  readonly id: Match.MatchId
  readonly access: Match.Access
  readonly span: SourceSpan.SourceSpan
  readonly arms: ReadonlyArray<{
    readonly id: Match.ArmId
    readonly member?: Type.Nominal
    readonly universal: boolean
    readonly provisionalGuard: boolean
    readonly bindings: ReadonlyArray<BindingSite>
    readonly cleanup: ReadonlyArray<{
      readonly path: ReadonlyArray<DeclarationIndex.FieldId>
      readonly cleanup: CleanupPlan
    }>
  }>
}

/** One module's ownership fact table and its phase diagnostics. */
export interface ModuleOwnership {
  readonly _tag: 'OwnershipFacts'
  readonly module: string
  readonly functions: ReadonlyArray<FunctionOwnership>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const satisfied: Verdict = Object.freeze({ _tag: 'Satisfied' })

const copyable: OwnershipCategory = Object.freeze({ _tag: 'Copyable' })

const categoryOf = (type: DeclarationIndex.SemanticType | undefined): OwnershipCategory =>
  type === undefined ||
  Type.isBuiltin(type) ||
  Type.isString(type) ||
  Type.isNever(type) ||
  Type.isOutOfMemory(type)
    ? copyable
    : Type.isSlice(type)
      ? type.access === 'Shared'
        ? copyable
        : Object.freeze({ _tag: 'MoveOnly', type })
      : Type.isReference(type)
        ? type.access === 'Shared'
          ? copyable
          : Object.freeze({ _tag: 'MoveOnly', type })
        : Type.isFixedArray(type)
          ? categoryOf(type.element)._tag === 'Copyable'
            ? copyable
            : Object.freeze({ _tag: 'MoveOnly', type })
          : Type.isEffect(type) && type.access === 'Shared'
            ? copyable
            : Type.isCallable(type) && type.mode === 'Shared'
              ? copyable
              : Object.freeze({ _tag: 'MoveOnly', type })

const siteKey = (site: BindingSite): string =>
  site._tag === 'Parameter'
    ? `p${site.parameter.ordinal}`
    : site._tag === 'Let'
      ? `b${site.binding.ordinal}`
      : `m${site.binding.arm.match.span.start}.a${site.binding.arm.ordinal}.p${site.binding.ordinal}`

interface MutableBinding {
  readonly site: BindingSite
  readonly name: string | undefined
  readonly mutability: 'Immutable' | 'Mutable'
  readonly liveFrom: SourceSpan.SourceSpan
  readonly category: OwnershipCategory
  readonly type?: DeclarationIndex.SemanticType
  readonly cleanup?: CleanupPlan
  liveTo: SourceSpan.SourceSpan
  movedAt?: SourceSpan.SourceSpan
  readonly matchAccess?: Match.Access
}

interface CheckState {
  readonly index: DeclarationIndex.Index
  readonly bindings: Map<string, MutableBinding>
  readonly order: Array<MutableBinding>
  readonly diagnostics: Array<Diagnostic.Diagnostic>
  readonly matches: Array<MatchOwnership>
  readonly callables: Array<CallableEnvironmentFact>
}

const useSite = (expression: Hir.Expression): BindingSite | undefined => {
  switch (expression._tag) {
    case 'ParameterReference':
      return Object.freeze({ _tag: 'Parameter', parameter: expression.parameter })
    case 'BindingReference':
      return Object.freeze({ _tag: 'Let', binding: expression.binding })
    case 'PatternBindingReference':
      return Object.freeze({ _tag: 'Pattern', binding: expression.binding })
    default:
      return undefined
  }
}

const placeSite = (expression: Hir.Expression): BindingSite | undefined => {
  if (expression._tag === 'Project' || expression._tag === 'IndexPlace') {
    return placeSite(expression.subject)
  }
  return useSite(expression)
}

/**
 * Names the dedicated extraction rejection when a place resolves to a field holding a concrete
 * callable representation. Extracting one would leave the aggregate owning a partially released
 * environment, so its captures could be cleaned twice; consuming invocation takes the whole
 * aggregate instead. Every other partial move keeps the general struct-field rejection.
 */
const representationFieldExtraction = (
  place: Hir.Expression,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic | undefined => {
  if (place._tag !== 'Project') return undefined
  const type = place.type
  if (!Type.isRepresented(type) || !Type.isCallable(type.contract)) return undefined
  return Diagnostic.representationFieldExtraction(
    Type.encode(place.nominal),
    `#${place.field.ordinal}`,
    Type.encode(type.contract),
    span,
  )
}

const checkUse = (
  state: CheckState,
  live: Set<string>,
  site: BindingSite,
  span: SourceSpan.SourceSpan,
  consuming: boolean,
): void => {
  const key = siteKey(site)
  const binding = state.bindings.get(key)
  if (binding === undefined) return
  if (!live.has(key)) {
    state.diagnostics.push(
      Diagnostic.useAfterMove(binding.name ?? '?', binding.movedAt ?? binding.liveTo, span),
    )
    return
  }
  if (consuming) {
    binding.movedAt ??= span
    binding.liveTo = span
    live.delete(key)
  }
}

const callableEnvironment = (
  state: CheckState,
  expression: Extract<Hir.Expression, { readonly _tag: 'CallableSection' }>,
): CallableEnvironmentFact => {
  const slots = Object.freeze(
    expression.captures.map((capture): CallableEnvironmentSlot => {
      const type = capture.value._tag === 'Unavailable' ? undefined : capture.value.type
      return Object.freeze({
        ordinal: capture.ordinal,
        parameterOrdinal: capture.parameterOrdinal,
        access: capture.access,
        ...(type === undefined ? {} : { type }),
        cleanup:
          capture.access === 'Take' && type !== undefined
            ? cleanupPlan(state.index, type)
            : Object.freeze({
                _tag: 'NoCleanup' as const,
                type: type ?? ('i32' as const),
              }),
      })
    }),
  )
  return Object.freeze({
    _tag: 'CallableEnvironment',
    site: expression.site,
    mode: expression.mode,
    slots,
    retainedDependencies: expression.retainedDependencies,
    dropOrder: Object.freeze(
      [...slots]
        .reverse()
        .filter((slot) => slot.cleanup._tag !== 'NoCleanup')
        .map((slot) => slot.ordinal),
    ),
    span: expression.span,
  })
}

const callableCleanup = (environment: CallableEnvironmentFact, type: Type.Callable): CleanupPlan =>
  Object.freeze({
    _tag: 'CallableCleanup',
    type,
    site: environment.site,
    slots: Object.freeze(
      [...environment.slots]
        .reverse()
        .flatMap((slot) =>
          slot.cleanup._tag === 'NoCleanup'
            ? []
            : [Object.freeze({ ordinal: slot.ordinal, cleanup: slot.cleanup })],
        ),
    ),
  })

const checkExpression = (
  state: CheckState,
  live: Set<string>,
  expression: Hir.Expression,
  consuming: boolean,
  guard = false,
  escaping = false,
): void => {
  const argumentConsumes = (argument: Hir.Expression): boolean =>
    argument._tag === 'Unavailable'
      ? true
      : Type.isEffect(argument.type)
        ? argument.type.access === 'Take'
        : Type.isCallable(argument.type)
          ? argument.type.mode === 'Take'
          : true
  switch (expression._tag) {
    case 'ParameterReference':
    case 'BindingReference': {
      const site = useSite(expression)
      if (site === undefined) return
      const binding = state.bindings.get(siteKey(site))
      if (consuming && binding?.category._tag === 'MoveOnly') {
        state.diagnostics.push(
          Diagnostic.explicitMoveRequired(binding.name ?? '?', expression.span),
        )
      }
      checkUse(state, live, site, expression.span, false)
      return
    }
    case 'PatternBindingReference': {
      const site = useSite(expression)
      if (site === undefined) return
      const binding = state.bindings.get(siteKey(site))
      const moveOnly = binding?.category._tag === 'MoveOnly'
      if (guard && consuming && moveOnly) {
        state.diagnostics.push(
          Diagnostic.guardConsumesPattern(binding?.name ?? '?', expression.span),
        )
        checkUse(state, live, site, expression.span, false)
        return
      }
      if (
        (binding?.matchAccess === 'Shared' || binding?.matchAccess === 'Exclusive') &&
        moveOnly &&
        (consuming || escaping)
      ) {
        state.diagnostics.push(Diagnostic.matchBorrowEscape(binding.name ?? '?', expression.span))
        checkUse(state, live, site, expression.span, false)
        return
      }
      checkUse(
        state,
        live,
        site,
        expression.span,
        binding?.matchAccess === 'Move' && consuming && moveOnly,
      )
      return
    }
    case 'Move': {
      if (expression.subject._tag === 'Project' || expression.subject._tag === 'IndexPlace') {
        checkExpression(state, live, expression.subject, false, guard, escaping)
        state.diagnostics.push(
          representationFieldExtraction(expression.subject, expression.span) ??
            Diagnostic.partialMove(expression.span),
        )
        return
      }
      const site = useSite(expression.subject)
      if (site?._tag === 'Pattern') {
        checkExpression(state, live, expression.subject, true, guard, escaping)
      } else if (site !== undefined) checkUse(state, live, site, expression.span, true)
      else checkExpression(state, live, expression.subject, true, guard, escaping)
      return
    }
    case 'UnionConvert':
      checkExpression(
        state,
        live,
        expression.source,
        expression.access === 'Owned',
        guard,
        escaping,
      )
      return
    case 'ShortCircuit':
      // Both operands are checked as if both evaluate. The right operand carries no move and no
      // effect site, so treating it as evaluated only over-approximates reads, never releases.
      checkExpression(state, live, expression.left, false, guard, escaping)
      checkExpression(state, live, expression.right, false, guard, escaping)
      return
    case 'Construct': {
      const fields = new Map(
        expression.fields.map((field) => [field.field.ordinal, field.value] as const),
      )
      for (const field of expression.evaluationOrder) {
        const value = fields.get(field.ordinal)
        if (value !== undefined) checkExpression(state, live, value, true, guard, escaping)
      }
      return
    }
    case 'ArrayConstruct': {
      for (const element of expression.elements)
        checkExpression(state, live, element, true, guard, escaping)
      return
    }
    case 'Project': {
      checkExpression(state, live, expression.subject, false, guard, escaping)
      if (consuming && categoryOf(expression.type)._tag === 'MoveOnly') {
        state.diagnostics.push(
          representationFieldExtraction(expression, expression.span) ??
            Diagnostic.partialMove(expression.span),
        )
      }
      return
    }
    case 'IndexPlace': {
      checkExpression(state, live, expression.subject, false, guard, escaping)
      checkExpression(state, live, expression.index, false, guard, escaping)
      if (consuming && categoryOf(expression.type)._tag === 'MoveOnly') {
        state.diagnostics.push(Diagnostic.partialMove(expression.span))
      }
      return
    }
    case 'SliceBorrow':
    case 'ValueBorrow': {
      const site: BindingSite =
        expression.root._tag === 'BindingSliceRoot'
          ? Object.freeze({ _tag: 'Let', binding: expression.root.binding })
          : expression.root._tag === 'ParameterSliceRoot'
            ? Object.freeze({ _tag: 'Parameter', parameter: expression.root.parameter })
            : Object.freeze({ _tag: 'Pattern', binding: expression.root.binding })
      checkUse(state, live, site, expression.span, false)
      return
    }
    case 'SliceLength':
      checkExpression(state, live, expression.slice, false, guard, escaping)
      return
    case 'SliceIndexPlace': {
      checkExpression(state, live, expression.slice, false, guard, escaping)
      checkExpression(state, live, expression.index, false, guard, escaping)
      if (consuming && categoryOf(expression.type)._tag === 'MoveOnly') {
        state.diagnostics.push(Diagnostic.borrowedMove(expression.span))
      }
      return
    }
    case 'FunctionItem':
      return
    case 'CallableSection': {
      const environment = callableEnvironment(state, expression)
      if (
        !state.callables.some((candidate) =>
          Hir.sameExecutableSite(candidate.site, expression.site),
        )
      ) {
        state.callables.push(environment)
      }
      for (const capture of expression.captures) {
        checkExpression(state, live, capture.value, capture.access === 'Take', guard, escaping)
      }
      return
    }
    case 'CallableApply': {
      const checkCallee = (): void => {
        const site = useSite(expression.callee)
        if (site !== undefined && expression.access === 'Take') {
          checkUse(state, live, site, expression.callee.span, true)
          return
        }
        checkExpression(
          state,
          live,
          expression.callee,
          expression.access === 'Take',
          guard,
          escaping,
        )
      }
      const checkArguments = (): void => {
        for (const argument of expression.arguments) {
          checkExpression(state, live, argument, argumentConsumes(argument), guard, escaping)
        }
      }
      if (expression.evaluation === 'LeftThenCallable') {
        checkArguments()
        checkCallee()
      } else {
        checkCallee()
        checkArguments()
      }
      return
    }
    case 'BuiltinCall':
    case 'BoundOperationCall': {
      for (const argument of expression.arguments)
        checkExpression(state, live, argument, false, guard, escaping)
      return
    }
    case 'Call': {
      for (const argument of expression.arguments)
        checkExpression(state, live, argument, argumentConsumes(argument), guard, escaping)
      return
    }
    case 'EffectConstruct': {
      for (const argument of expression.arguments)
        checkExpression(state, live, argument, argumentConsumes(argument), guard, escaping)
      return
    }
    case 'ServiceEffectConstruct': {
      for (const argument of expression.arguments)
        checkExpression(state, live, argument, argumentConsumes(argument), guard, escaping)
      return
    }
    case 'EffectBlock': {
      for (const capture of expression.captures) {
        const site: BindingSite | undefined =
          capture.binding !== undefined
            ? Object.freeze({ _tag: 'Let', binding: capture.binding })
            : capture.parameter !== undefined
              ? Object.freeze({ _tag: 'Parameter', parameter: capture.parameter })
              : undefined
        if (site !== undefined) checkUse(state, live, site, capture.span, capture.access === 'Take')
      }
      return
    }
    case 'EffectBindRequirement': {
      checkExpression(state, live, expression.protected, false, guard, escaping)
      const site: BindingSite | undefined =
        expression.provider.binding !== undefined
          ? Object.freeze({ _tag: 'Let', binding: expression.provider.binding })
          : expression.provider.parameter !== undefined
            ? Object.freeze({ _tag: 'Parameter', parameter: expression.provider.parameter })
            : undefined
      if (site !== undefined)
        checkUse(state, live, site, expression.provider.span, expression.provider.access === 'Take')
      return
    }
    case 'Run': {
      const site = useSite(expression.subject)
      if (
        site !== undefined &&
        expression.subject._tag !== 'Unavailable' &&
        Type.isEffect(expression.subject.type) &&
        expression.subject.type.access === 'Take'
      ) {
        checkUse(state, live, site, expression.span, true)
      } else checkExpression(state, live, expression.subject, false, guard, escaping)
      return
    }
    case 'Match': {
      const scrutineeSite = placeSite(expression.scrutinee)
      const scrutineeType =
        expression.scrutinee._tag === 'Unavailable' ? undefined : expression.scrutinee.type
      const scrutineeBinding =
        scrutineeSite === undefined ? undefined : state.bindings.get(siteKey(scrutineeSite))
      if (expression.access === 'Copy') {
        checkExpression(state, live, expression.scrutinee, false, guard, false)
        if (categoryOf(scrutineeType)._tag === 'MoveOnly') {
          state.diagnostics.push(
            Diagnostic.explicitMoveRequired(scrutineeBinding?.name ?? '?', expression.span),
          )
        }
      } else if (expression.access === 'Move') {
        if (scrutineeSite === undefined) {
          checkExpression(state, live, expression.scrutinee, true, guard, false)
        } else {
          checkUse(state, live, scrutineeSite, expression.span, true)
        }
      } else {
        checkExpression(state, live, expression.scrutinee, false, guard, false)
        if (expression.access === 'Exclusive') {
          if (scrutineeSite === undefined) {
            state.diagnostics.push(
              Diagnostic.invalidMatchScrutineePlace('Exclusive', expression.span),
            )
          } else if (scrutineeBinding?.mutability !== 'Mutable') {
            state.diagnostics.push(
              Diagnostic.exclusiveMatchRequiresMutable(
                scrutineeBinding?.name ?? '?',
                expression.span,
              ),
            )
          }
        }
      }

      const afterScrutinee = new Set(live)
      const continuing: Array<Set<string>> = []
      const armFacts: Array<MatchOwnership['arms'][number]> = []
      for (const arm of expression.arms) {
        const armLive = new Set(afterScrutinee)
        const sites: Array<BindingSite> = []
        for (const pattern of arm.bindings) {
          const site: BindingSite = Object.freeze({ _tag: 'Pattern', binding: pattern.id })
          const mutable: MutableBinding = {
            site,
            name: pattern.name,
            mutability: pattern.access === 'Exclusive' ? 'Mutable' : 'Immutable',
            liveFrom: pattern.span,
            liveTo: arm.span,
            category: categoryOf(pattern.type),
            type: pattern.type,
            matchAccess: pattern.access,
          }
          const key = siteKey(site)
          state.bindings.set(key, mutable)
          state.order.push(mutable)
          armLive.add(key)
          sites.push(site)
        }
        if (arm.guard !== undefined) checkExpression(state, armLive, arm.guard, false, true, false)
        checkExpression(state, armLive, arm.result, consuming, guard, true)
        const cleanup =
          expression.access === 'Move'
            ? [
                ...arm.cleanup.flatMap((path) => {
                  const type = cleanupTypeAtPath(state.index, arm.member ?? scrutineeType, path)
                  return type === undefined
                    ? []
                    : [Object.freeze({ path, cleanup: cleanupPlan(state.index, type) })]
                }),
                ...arm.bindings.flatMap((binding) => {
                  const site: BindingSite = Object.freeze({ _tag: 'Pattern', binding: binding.id })
                  return armLive.has(siteKey(site)) && categoryOf(binding.type)._tag === 'MoveOnly'
                    ? [
                        Object.freeze({
                          path: binding.path,
                          cleanup: cleanupPlan(state.index, binding.type),
                        }),
                      ]
                    : []
                }),
              ]
            : []
        armFacts.push(
          Object.freeze({
            id: arm.id,
            ...(arm.member === undefined ? {} : { member: arm.member }),
            universal: arm.universal,
            provisionalGuard: arm.guard !== undefined,
            bindings: Object.freeze(sites),
            cleanup: Object.freeze(cleanup),
          }),
        )
        for (const site of sites) armLive.delete(siteKey(site))
        continuing.push(armLive)
      }
      if (continuing.length > 0) {
        const intersection = new Set(
          [...(continuing.at(0) ?? [])].filter((site) =>
            continuing.every((candidate) => candidate.has(site)),
          ),
        )
        live.clear()
        for (const site of intersection) live.add(site)
      }
      state.matches.push(
        Object.freeze({
          _tag: 'MatchOwnership',
          id: expression.id,
          access: expression.access,
          span: expression.span,
          arms: Object.freeze(armFacts),
        }),
      )
      return
    }
    case 'Replace': {
      // The write half mirrors assignment: index selectors evaluate, a projected root must be
      // usable, and a value consuming the root itself is an overlapping assignment. The place
      // stays initialized throughout, so no partial move is recorded.
      for (const selector of expression.place.selectors) {
        if (selector._tag === 'Index' || selector._tag === 'SliceIndex') {
          checkExpression(state, live, selector.index, false)
        }
      }
      const rootSite: BindingSite =
        expression.place._tag === 'WritePlace'
          ? Object.freeze({ _tag: 'Let', binding: expression.place.root })
          : expression.place.root._tag === 'BindingSliceRoot'
            ? Object.freeze({ _tag: 'Let', binding: expression.place.root.binding })
            : Object.freeze({ _tag: 'Parameter', parameter: expression.place.root.parameter })
      const rootKey = siteKey(rootSite)
      const root = state.bindings.get(rootKey)
      const wasLive = live.has(rootKey)
      if (!wasLive && expression.place.selectors.length > 0 && root !== undefined) {
        checkUse(state, live, rootSite, expression.place.span, false)
      }
      checkExpression(state, live, expression.value, true)
      if (wasLive && !live.has(rootKey)) {
        state.diagnostics.push(Diagnostic.overlappingAssignment(root?.name ?? '?', expression.span))
      }
      return
    }
    default:
      return
  }
}

/**
 * The expressions one statement evaluates in its own scope: control-flow bodies are excluded
 * because the statement walker recurses into them itself, and would otherwise observe the same
 * expression twice.
 */
const statementRootExpressions = (statement: Hir.Statement): ReadonlyArray<Hir.Expression> => {
  switch (statement._tag) {
    case 'Bind':
      return [statement.initializer]
    case 'Evaluate':
      return [statement.expression]
    case 'Return':
    case 'Fail':
    case 'Drop':
      return [statement.expression]
    case 'Write':
      return [
        ...statement.place.selectors.flatMap((selector) =>
          selector._tag === 'Index' || selector._tag === 'SliceIndex' ? [selector.index] : [],
        ),
        statement.value,
      ]
    case 'If':
    case 'While':
      return [statement.condition]
    default:
      return []
  }
}

/** Effect blocks owned by this expression, stopping at each block: nested blocks belong to it. */
const deferredBlocks = (
  expression: Hir.Expression,
): ReadonlyArray<Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }>> =>
  expression._tag === 'EffectBlock'
    ? [expression]
    : Hir.expressionChildren(expression).flatMap(deferredBlocks)

/**
 * Run sites in this expression that can propagate a typed failure, stopping at effect blocks:
 * a deferred body's runs propagate out of its own compiled function, not out of this one.
 *
 * A run is fallible when its effect type carries failures OR an open failure row: inside a
 * generic body the caller's failures arrive as a row *parameter*, so `failures` is empty while
 * `failureParameters` is not, and specialization can substitute a row that really does fail.
 * Reading `failures` alone leaves every owner live at such a run without a release.
 */
const fallibleRunSites = (
  expression: Hir.Expression,
): ReadonlyArray<Extract<Hir.Expression, { readonly _tag: 'Run' }>> => {
  if (expression._tag === 'EffectBlock') return []
  const nested = Hir.expressionChildren(expression).flatMap(fallibleRunSites)
  if (expression._tag !== 'Run') return nested
  const subjectType: unknown = 'type' in expression.subject ? expression.subject.type : undefined
  const fallible =
    typeof subjectType === 'object' &&
    subjectType !== null &&
    (subjectType as { readonly _tag?: string })._tag === 'EffectType' &&
    ((subjectType as Type.Effect).failures.length > 0 ||
      (subjectType as Type.Effect).failureParameters.length > 0)
  return fallible ? [expression, ...nested] : nested
}

/**
 * Owner sites this expression consumes by move, keyed like the liveness set.
 *
 * A propagation exit is published before the statement is walked, so the live set still holds
 * every owner the run's own operands move away — `run f(move owned)` would otherwise release
 * `owned` here as well as in the callee that now owns it. Excluding the operands keeps the
 * exit to the owners that genuinely survive the run and are stranded by the failure.
 */
const consumedSites = (expression: Hir.Expression): ReadonlyArray<string> => {
  const nested = Hir.expressionChildren(expression).flatMap(consumedSites)
  if (expression._tag !== 'Move') return nested
  const site = useSite(expression.subject)
  return site === undefined ? nested : [siteKey(site), ...nested]
}

interface LoanAnalysis {
  readonly loans: ReadonlyArray<LoanFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const borrowSite = (root: Elaboration.BorrowRootFact): BindingSite =>
  root._tag === 'BindingRoot'
    ? Object.freeze({ _tag: 'Let', binding: root.binding.id })
    : root._tag === 'ParameterRoot'
      ? Object.freeze({ _tag: 'Parameter', parameter: root.parameter.id })
      : Object.freeze({ _tag: 'Pattern', binding: root.binding.id })

const sameSite = (left: BindingSite, right: BindingSite): boolean =>
  siteKey(left) === siteKey(right)

const borrowedPlaceAccess = (
  expression: Elaboration.ExpressionFact,
): Type.Slice['access'] | undefined => {
  if (expression._tag === 'Grouped') return borrowedPlaceAccess(expression.expression)
  if (expression._tag === 'IndexProjection' || expression._tag === 'FieldProjection') {
    return expression.borrowAccess
  }
  return undefined
}

const analyzeLoans = (fn: Elaboration.FunctionFact): LoanAnalysis => {
  const loans: Array<LoanFact> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []

  const directSite = (
    expression: Elaboration.ExpressionFact,
  ): { readonly site: BindingSite; readonly spelling: string } | undefined => {
    if (expression._tag === 'Grouped') return directSite(expression.expression)
    if (expression._tag !== 'Identifier') return undefined
    if (expression.reference._tag === 'ResolvedBinding') {
      return Object.freeze({
        site: Object.freeze({ _tag: 'Let', binding: expression.reference.binding.id }),
        spelling: expression.reference.spelling,
      })
    }
    if (expression.reference._tag === 'Resolved') {
      return Object.freeze({
        site: Object.freeze({ _tag: 'Parameter', parameter: expression.reference.parameter.id }),
        spelling: expression.reference.spelling,
      })
    }
    return undefined
  }

  const runEnds = new Map<
    number,
    { readonly region: Hir.RegionId; readonly span: SourceSpan.SourceSpan }
  >()
  const callableEnds = new Map<
    number,
    { readonly region: Hir.RegionId; readonly span: SourceSpan.SourceSpan }
  >()
  const slotEnds = new Map<
    number,
    { readonly region: Hir.RegionId; readonly span: SourceSpan.SourceSpan }
  >()
  const viewEnds = new Map<
    number,
    { readonly region: Hir.RegionId; readonly span: SourceSpan.SourceSpan }
  >()
  const scanRunEnds = (expression: Elaboration.ExpressionFact, region: Hir.RegionId): void => {
    switch (expression._tag) {
      case 'Run': {
        const site = directSite(expression.subject)?.site
        if (site?._tag === 'Let') {
          const previous = runEnds.get(site.binding.ordinal)
          if (previous === undefined || previous.span.end < expression.syntax.span.end) {
            runEnds.set(site.binding.ordinal, { region, span: expression.syntax.span })
          }
        }
        scanRunEnds(expression.subject, region)
        return
      }
      case 'Move':
        scanRunEnds(expression.subject, region)
        return
      case 'Grouped':
        scanRunEnds(expression.expression, region)
        return
      case 'Borrow':
      case 'FieldProjection':
        scanRunEnds(expression.subject, region)
        return
      case 'IndexProjection':
        scanRunEnds(expression.subject, region)
        scanRunEnds(expression.index, region)
        return
      case 'StructLiteral':
        for (const initializer of expression.initializers)
          scanRunEnds(initializer.expression, region)
        return
      case 'ArrayLiteral':
        for (const element of expression.elements) scanRunEnds(element.expression, region)
        return
      case 'Match':
        scanRunEnds(expression.scrutinee, region)
        for (const arm of expression.arms) {
          if (arm.guard !== undefined) scanRunEnds(arm.guard, region)
          scanRunEnds(arm.result, region)
        }
        return
      case 'Operator':
      case 'ShortCircuit':
      case 'Call':
        for (const argument of expression.arguments) scanRunEnds(argument.expression, region)
        return
      case 'CallableApply':
        if (expression.provenance._tag === 'PipelineCallableApplication') {
          for (const argument of expression.arguments) scanRunEnds(argument.expression, region)
          scanRunEnds(expression.callee, region)
        } else {
          scanRunEnds(expression.callee, region)
          for (const argument of expression.arguments) scanRunEnds(argument.expression, region)
        }
        return
      case 'CallableSection':
        for (const capture of expression.captures) scanRunEnds(capture.expression, region)
        return
      case 'EffectBlock':
      case 'FunctionItem':
        return
      case 'Integer':
      case 'Boolean':
        return
      case 'Identifier': {
        const site = directSite(expression)?.site
        if (
          site?._tag === 'Let' &&
          expression.type._tag === 'Available' &&
          Type.isSlot(expression.type.type)
        ) {
          const previous = slotEnds.get(site.binding.ordinal)
          if (previous === undefined || previous.span.end < expression.syntax.span.end)
            slotEnds.set(site.binding.ordinal, { region, span: expression.syntax.span })
        }
        if (
          site?._tag === 'Let' &&
          expression.type._tag === 'Available' &&
          Type.containsViewBorrow(expression.type.type)
        ) {
          const previous = viewEnds.get(site.binding.ordinal)
          if (previous === undefined || previous.span.end < expression.syntax.span.end) {
            viewEnds.set(site.binding.ordinal, { region, span: expression.syntax.span })
          }
        }
        return
      }
    }
  }
  const scanStatementRunEnds = (facts: ReadonlyArray<Elaboration.StatementFact>): void => {
    for (const statement of facts) {
      switch (statement._tag) {
        case 'UnsafeStatement':
          scanStatementRunEnds(statement.statements)
          break
        case 'BindStatement':
          scanRunEnds(statement.binding.initializer, statement.region)
          break
        case 'ExpressionStatement':
          scanRunEnds(statement.expression, statement.region)
          break
        case 'IfStatement':
          scanRunEnds(statement.condition, statement.region)
          scanStatementRunEnds(statement.taken)
          scanStatementRunEnds(statement.otherwise)
          break
        case 'WriteStatement':
          scanRunEnds(statement.destination, statement.region)
          scanRunEnds(statement.value, statement.region)
          break
        case 'WhileStatement':
          scanRunEnds(statement.condition, statement.region)
          scanStatementRunEnds(statement.body)
          break
        case 'ReturnStatement':
        case 'FailStatement':
          scanRunEnds(statement.expression, statement.region)
          break
        case 'DropStatement': {
          scanRunEnds(statement.expression, statement.region)
          const site = directSite(statement.expression)?.site
          if (site?._tag === 'Let') {
            callableEnds.set(site.binding.ordinal, {
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          break
        }
        case 'BreakStatement':
        case 'ContinueStatement':
          break
      }
    }
  }
  scanStatementRunEnds(fn.statements)

  const returnedArgumentOrdinal = (expression: Elaboration.ExpressionFact): number | undefined => {
    return Elaboration.returnedBorrowArgument(expression)?.id.ordinal
  }

  const viewRoots = new Map<number, BindingSite>()
  const viewAliases = new Map<number, number>()
  const sourceSite = (expression: Elaboration.ExpressionFact): BindingSite | undefined => {
    if (expression._tag === 'Grouped') return sourceSite(expression.expression)
    if (expression._tag === 'Borrow' && expression.formation._tag !== 'Unavailable') {
      const site = borrowSite(expression.formation.root)
      return site._tag === 'Let' ? (viewRoots.get(site.binding.ordinal) ?? site) : site
    }
    const direct = directSite(expression)?.site
    if (direct !== undefined) {
      return direct._tag === 'Let' ? (viewRoots.get(direct.binding.ordinal) ?? direct) : direct
    }
    const ordinal = returnedArgumentOrdinal(expression)
    if (ordinal !== undefined && expression._tag === 'Call') {
      const argument = expression.arguments.at(ordinal)
      return argument === undefined ? undefined : sourceSite(argument.expression)
    }
    return undefined
  }

  for (const binding of fn.bindings) {
    if (
      binding.inferredType._tag !== 'Available' ||
      !Type.containsViewBorrow(binding.inferredType.type) ||
      returnedArgumentOrdinal(binding.initializer) === undefined
    ) {
      continue
    }
    const returnedOrdinal = returnedArgumentOrdinal(binding.initializer)
    if (returnedOrdinal !== undefined && binding.initializer._tag === 'Call') {
      const argument = binding.initializer.arguments.at(returnedOrdinal)
      const source = argument === undefined ? undefined : directSite(argument.expression)?.site
      if (source?._tag === 'Let') viewAliases.set(binding.id.ordinal, source.binding.ordinal)
    }
    const root = sourceSite(binding.initializer)
    if (root !== undefined) viewRoots.set(binding.id.ordinal, root)
  }

  let propagatedViewEnd = true
  while (propagatedViewEnd) {
    propagatedViewEnd = false
    for (const [alias, source] of viewAliases) {
      const ending = viewEnds.get(alias)
      const previous = viewEnds.get(source)
      if (ending !== undefined && (previous === undefined || previous.span.end < ending.span.end)) {
        viewEnds.set(source, ending)
        propagatedViewEnd = true
      }
    }
  }

  const delayedLoansAt = (span: SourceSpan.SourceSpan): ReadonlyArray<LoanFact> =>
    loans.filter(
      (loan) =>
        loan.startSpan.sourceId === span.sourceId &&
        loan.startSpan.end <= span.start &&
        span.end <= loan.endSpan.end &&
        loan.endSpan.end > loan.startSpan.end,
    )

  const checkDirectAccess = (
    expression: Elaboration.ExpressionFact,
    active: ReadonlyArray<LoanFact>,
    access: 'Read' | 'Write' | 'Move',
  ): void => {
    const direct = directSite(expression)
    if (direct === undefined) return
    const conflict = active.find(
      (loan) =>
        sameSite(loan.root, direct.site) && (access !== 'Read' || loan.access === 'Exclusive'),
    )
    if (conflict !== undefined) {
      diagnostics.push(
        Diagnostic.ownerAccessDuringLoan(
          direct.spelling,
          access,
          conflict.startSpan,
          expression.syntax.span,
        ),
      )
    }
  }

  const naturalAccess = (expression: Elaboration.ExpressionFact): 'Read' | 'Move' =>
    expression.type._tag === 'Available' && categoryOf(expression.type.type)._tag === 'MoveOnly'
      ? 'Move'
      : 'Read'

  const inspect = (
    expression: Elaboration.ExpressionFact,
    region: Hir.RegionId,
    active: ReadonlyArray<LoanFact>,
    access: 'Read' | 'Write' | 'Move' = 'Read',
    delayedEnd?: { readonly region: Hir.RegionId; readonly span: SourceSpan.SourceSpan },
  ): void => {
    switch (expression._tag) {
      case 'Integer':
      case 'Boolean':
        return
      case 'Identifier':
        checkDirectAccess(
          expression,
          [...active, ...delayedLoansAt(expression.syntax.span)],
          access,
        )
        return
      case 'Borrow':
        return
      case 'Move':
        inspect(expression.subject, region, active, 'Move', delayedEnd)
        return
      case 'Grouped':
        inspect(expression.expression, region, active, access, delayedEnd)
        return
      case 'FieldProjection':
        inspect(expression.subject, region, active, access)
        return
      case 'IndexProjection':
        inspect(expression.subject, region, active, access)
        inspect(expression.index, region, active, 'Read')
        return
      case 'StructLiteral':
        for (const initializer of expression.initializers) {
          inspect(initializer.expression, region, active, naturalAccess(initializer.expression))
        }
        return
      case 'ArrayLiteral':
        for (const element of expression.elements) {
          inspect(element.expression, region, active, naturalAccess(element.expression))
        }
        return
      case 'Match':
        inspect(expression.scrutinee, region, active, naturalAccess(expression.scrutinee))
        for (const arm of expression.arms) {
          if (arm.guard !== undefined) inspect(arm.guard, region, active, 'Read')
          inspect(arm.result, region, active, access)
        }
        return
      case 'Operator':
      case 'ShortCircuit':
        for (const argument of expression.arguments) {
          inspect(argument.expression, region, active, 'Read')
        }
        return
      case 'FunctionItem':
        return
      case 'CallableSection': {
        const captureActive: Array<LoanFact> = [
          ...active,
          ...delayedLoansAt(expression.syntax.span),
        ]
        for (const capture of expression.captures) {
          const candidate = capture.expression
          if (capture.access !== 'Shared' && capture.access !== 'Exclusive') {
            inspect(candidate, region, captureActive, naturalAccess(candidate))
            continue
          }
          const root =
            candidate._tag === 'Borrow' && candidate.formation._tag !== 'Unavailable'
              ? borrowSite(candidate.formation.root)
              : directSite(candidate)?.site
          if (root === undefined) {
            inspect(candidate, region, captureActive, 'Read')
            continue
          }
          const conflict = captureActive.find(
            (loan) =>
              sameSite(loan.root, root) &&
              (loan.access === 'Exclusive' || capture.access === 'Exclusive'),
          )
          if (conflict !== undefined) {
            diagnostics.push(
              Diagnostic.conflictingSliceLoan(
                conflict.access,
                capture.access,
                conflict.startSpan,
                candidate.syntax.span,
              ),
            )
          }
          const loan: LoanFact = Object.freeze({
            _tag: 'Loan',
            id: Object.freeze({
              _tag: 'BorrowId',
              function: fn.declaration.id,
              callSpan: expression.syntax.span,
              ordinal: capture.ordinal,
            }),
            root,
            access: capture.access,
            origin: 'CallableCapture',
            suspendsParent: false,
            startRegion: region,
            endRegion: delayedEnd?.region ?? region,
            startSpan: candidate.syntax.span,
            endSpan: delayedEnd?.span ?? expression.syntax.span,
          })
          loans.push(loan)
          captureActive.push(loan)
        }
        return
      }
      case 'CallableApply': {
        const inspectCallee = (): void =>
          inspect(
            expression.callee,
            region,
            active,
            expression.mode === 'Take'
              ? 'Move'
              : expression.mode === 'Exclusive'
                ? 'Write'
                : 'Read',
            delayedEnd,
          )
        const inspectArguments = (): void => {
          for (const argument of expression.arguments) {
            inspect(
              argument.expression,
              region,
              active,
              naturalAccess(argument.expression),
              argument.type._tag === 'Available' && Type.isEffect(argument.type.type)
                ? delayedEnd
                : undefined,
            )
          }
        }
        if (expression.provenance._tag === 'PipelineCallableApplication') {
          inspectArguments()
          inspectCallee()
        } else {
          inspectCallee()
          inspectArguments()
        }
        return
      }
      case 'Call': {
        const callActive: Array<LoanFact> = [...active, ...delayedLoansAt(expression.syntax.span)]
        const consumesSlot =
          expression.reference._tag === 'ResolvedBuiltin' &&
          (expression.reference.operation === 'SlotWrite' ||
            expression.reference.operation === 'SlotTake' ||
            expression.reference.operation === 'SlotCopy' ||
            expression.reference.operation === 'SlotDrop')
        for (const [argumentOrdinal, argument] of expression.arguments.entries()) {
          const candidate = argument.expression
          const returnedOrdinal = returnedArgumentOrdinal(expression)
          if (candidate._tag !== 'Borrow' || candidate.formation._tag === 'Unavailable') {
            const preservesEffectLifetime =
              argument.type._tag === 'Available' && Type.isEffect(argument.type.type)
            inspect(
              candidate,
              region,
              callActive,
              naturalAccess(candidate),
              preservesEffectLifetime
                ? delayedEnd
                : returnedOrdinal === argumentOrdinal
                  ? (delayedEnd ?? Object.freeze({ region, span: expression.syntax.span }))
                  : consumesSlot
                    ? Object.freeze({ region, span: expression.syntax.span })
                    : undefined,
            )
            continue
          }
          const directRoot = borrowSite(candidate.formation.root)
          const root =
            directRoot._tag === 'Let'
              ? (viewRoots.get(directRoot.binding.ordinal) ?? directRoot)
              : directRoot
          const conflict = callActive.find(
            (loan) =>
              sameSite(loan.root, root) &&
              (loan.access === 'Exclusive' || candidate.access === 'Exclusive'),
          )
          if (conflict !== undefined) {
            diagnostics.push(
              Diagnostic.conflictingSliceLoan(
                conflict.access,
                candidate.access,
                conflict.startSpan,
                candidate.syntax.span,
              ),
            )
          }
          const loan: LoanFact = Object.freeze({
            _tag: 'Loan',
            id: Object.freeze({
              _tag: 'BorrowId',
              function: fn.declaration.id,
              callSpan: expression.syntax.span,
              ordinal: argumentOrdinal,
            }),
            root,
            access: candidate.access,
            origin: returnedOrdinal === argumentOrdinal ? 'ReturnedView' : candidate.formation._tag,
            ...(candidate.formation._tag === 'SliceReborrow'
              ? { parent: root, suspendsParent: candidate.formation.suspendsParent }
              : { suspendsParent: false }),
            startRegion: region,
            endRegion: delayedEnd?.region ?? region,
            startSpan: candidate.syntax.span,
            endSpan: delayedEnd?.span ?? expression.syntax.span,
          })
          loans.push(loan)
          callActive.push(loan)
        }
        return
      }
      case 'EffectBlock': {
        const captureActive: Array<LoanFact> = [
          ...active,
          ...delayedLoansAt(expression.syntax.span),
        ]
        for (const [ordinal, capture] of expression.captures.entries()) {
          const root: BindingSite =
            capture.reference._tag === 'BindingFact'
              ? Object.freeze({ _tag: 'Let', binding: capture.reference.id })
              : Object.freeze({ _tag: 'Parameter', parameter: capture.reference.id })
          const candidateAccess = capture.access === 'Exclusive' ? 'Exclusive' : 'Shared'
          const conflict = captureActive.find(
            (loan) =>
              sameSite(loan.root, root) &&
              (loan.access === 'Exclusive' || candidateAccess === 'Exclusive'),
          )
          if (conflict !== undefined) {
            diagnostics.push(
              Diagnostic.conflictingSliceLoan(
                conflict.access,
                candidateAccess,
                conflict.startSpan,
                capture.span,
              ),
            )
          }
          if (capture.access !== 'Shared' && capture.access !== 'Exclusive') continue
          const loan: LoanFact = Object.freeze({
            _tag: 'Loan',
            id: Object.freeze({
              _tag: 'BorrowId',
              function: fn.declaration.id,
              callSpan: expression.syntax.span,
              ordinal,
            }),
            root,
            access: capture.access,
            origin: 'EffectCapture',
            suspendsParent: false,
            startRegion: region,
            endRegion: delayedEnd?.region ?? region,
            startSpan: capture.span,
            endSpan: delayedEnd?.span ?? expression.syntax.span,
          })
          loans.push(loan)
          captureActive.push(loan)
        }
        return
      }
      case 'Run':
        inspect(
          expression.subject,
          region,
          active,
          'Read',
          Object.freeze({ region, span: expression.syntax.span }),
        )
        return
      case 'EffectBindRequirement': {
        inspect(expression.protected, region, active, 'Read', delayedEnd)
        const provider = expression.provider
        if (provider === undefined || provider.access === 'Take') return
        const root: BindingSite =
          provider.reference._tag === 'BindingFact'
            ? Object.freeze({ _tag: 'Let', binding: provider.reference.id })
            : Object.freeze({ _tag: 'Parameter', parameter: provider.reference.id })
        const conflict = active.find(
          (loan) =>
            sameSite(loan.root, root) &&
            (loan.access === 'Exclusive' || provider.access === 'Exclusive'),
        )
        if (conflict !== undefined)
          diagnostics.push(
            Diagnostic.conflictingSliceLoan(
              conflict.access,
              provider.access,
              conflict.startSpan,
              provider.span,
            ),
          )
        loans.push(
          Object.freeze({
            _tag: 'Loan',
            id: Object.freeze({
              _tag: 'BorrowId',
              function: fn.declaration.id,
              callSpan: expression.syntax.span,
              ordinal: 0,
            }),
            root,
            access: provider.access,
            origin: 'EffectCapture',
            suspendsParent: false,
            startRegion: region,
            endRegion: delayedEnd?.region ?? region,
            startSpan: provider.span,
            endSpan: delayedEnd?.span ?? expression.syntax.span,
          }),
        )
        return
      }
    }
  }

  const statements = (facts: ReadonlyArray<Elaboration.StatementFact>): void => {
    for (const statement of facts) {
      switch (statement._tag) {
        case 'UnsafeStatement':
          statements(statement.statements)
          break
        case 'BindStatement': {
          const initializerType = statement.binding.initializer.type
          inspect(
            statement.binding.initializer,
            statement.region,
            [],
            'Read',
            initializerType._tag === 'Available' && Type.isEffect(initializerType.type)
              ? (runEnds.get(statement.binding.id.ordinal) ?? {
                  region: statement.region,
                  span: fn.declaration.syntax.span,
                })
              : initializerType._tag === 'Available' && Type.isCallable(initializerType.type)
                ? (callableEnds.get(statement.binding.id.ordinal) ?? {
                    region: statement.region,
                    span: fn.declaration.syntax.span,
                  })
                : initializerType._tag === 'Available' && Type.isSlot(initializerType.type)
                  ? (slotEnds.get(statement.binding.id.ordinal) ?? {
                      region: statement.region,
                      span: fn.declaration.syntax.span,
                    })
                  : initializerType._tag === 'Available' &&
                      Type.containsViewBorrow(initializerType.type)
                    ? (viewEnds.get(statement.binding.id.ordinal) ?? {
                        region: statement.region,
                        span: statement.binding.initializer.syntax.span,
                      })
                    : undefined,
          )
          break
        }
        case 'ExpressionStatement':
          inspect(statement.expression, statement.region, [], naturalAccess(statement.expression))
          break
        case 'IfStatement':
          inspect(statement.condition, statement.region, [])
          statements(statement.taken)
          statements(statement.otherwise)
          break
        case 'WriteStatement':
          inspect(statement.destination, statement.region, [], 'Write')
          inspect(statement.value, statement.region, [], naturalAccess(statement.value))
          break
        case 'WhileStatement':
          inspect(statement.condition, statement.region, [])
          statements(statement.body)
          break
        case 'ReturnStatement':
          inspect(statement.expression, statement.region, [], naturalAccess(statement.expression))
          break
        case 'FailStatement':
          inspect(statement.expression, statement.region, [], 'Move')
          break
        case 'DropStatement':
          inspect(statement.expression, statement.region, [], 'Move')
          break
        case 'BreakStatement':
        case 'ContinueStatement':
          break
      }
    }
  }
  statements(fn.statements)
  return Object.freeze({
    loans: Object.freeze(loans),
    diagnostics: Object.freeze(diagnostics),
  })
}

interface CheckedFunction {
  readonly ownership: FunctionOwnership
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface ExitDescriptor {
  readonly kind: ExitPlan['kind']
  readonly span: SourceSpan.SourceSpan
  readonly region?: Hir.RegionId
  readonly arm?: 'Taken' | 'Otherwise'
  readonly target?: Hir.LoopId
  readonly sites: ReadonlyArray<string>
}

const cleanupFields = (
  index: DeclarationIndex.Index,
  type: Type.Nominal,
  seen = new Set<string>(),
): ReadonlyArray<DeclarationIndex.FieldId> => {
  const key = Type.key(type)
  if (seen.has(key)) return Object.freeze([])
  const nextSeen = new Set(seen).add(key)
  const declaration = DeclarationIndex.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') return Object.freeze([])
  return Object.freeze(
    declaration.fields.flatMap((field) => {
      if (field.declaredType._tag !== 'Resolved') return [field.id]
      const nested = field.declaredType.type
      return Type.isNominal(nested)
        ? [field.id, ...cleanupFields(index, nested, nextSeen)]
        : [field.id]
    }),
  )
}

export const cleanupPlan = (
  index: DeclarationIndex.Index,
  type: DeclarationIndex.SemanticType,
  seen = new Set<string>(),
): CleanupPlan => {
  if (Type.isBuiltin(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isString(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isNever(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isParameter(type)) return Object.freeze({ _tag: 'ParameterCleanup', type })
  if (Type.isFailureProjection(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isSlice(type) || Type.isReference(type))
    return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isEffect(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.equals(type, Type.allocation))
    return Object.freeze({
      _tag: 'AllocationCleanup',
      type: Type.allocation,
      ticket: 'ActiveReclaimTicket',
    })
  if (Type.isRawBuffer(type))
    return Object.freeze({
      _tag: 'RawBufferCleanup',
      type,
      allocation: Object.freeze({
        _tag: 'AllocationCleanup',
        type: Type.allocation,
        ticket: 'ActiveReclaimTicket',
      }),
    })
  if (Type.isFixedArray(type)) {
    if (type.length === 0) return Object.freeze({ _tag: 'NoCleanup', type })
    return Object.freeze({
      _tag: 'ArrayCleanup',
      type,
      length: type.length,
      element: cleanupPlan(index, type.element, seen),
    })
  }
  if (Type.isUnion(type)) {
    return Object.freeze({
      _tag: 'UnionCleanup',
      type,
      cases: Object.freeze(
        type.members.map((member, ordinal) =>
          Object.freeze({
            member,
            ordinal,
            cleanup: cleanupPlan(index, member, seen),
          }),
        ),
      ),
    })
  }
  if (Type.isCallable(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isRepresented(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  const key = Type.key(type)
  if (seen.has(key)) return Object.freeze({ _tag: 'NoCleanup', type })
  const declaration = DeclarationIndex.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') {
    return Object.freeze({ _tag: 'NoCleanup', type })
  }
  const substitution =
    Type.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const nextSeen = new Set(seen).add(key)
  const structPlan: CleanupPlan = Object.freeze({
    _tag: 'StructCleanup',
    type,
    fields: Object.freeze(
      declaration.fields.map((field) =>
        Object.freeze({
          field: field.id,
          cleanup:
            field.declaredType._tag === 'Resolved'
              ? cleanupPlan(index, Type.substitute(field.declaredType.type, substitution), nextSeen)
              : Object.freeze({ _tag: 'NoCleanup' as const, type: 'i32' as const }),
        }),
      ),
    ),
  })
  // A source Drop conformance runs its hook before automatic field cleanup.
  const witness = DeclarationIndex.witness(index, type, Type.dropCapability)
  if (witness?._tag !== 'SourceConformanceWitness') return structPlan
  const conformance = index.modules
    .find((module) => module.module === witness.module)
    ?.conformances.find((candidate) => candidate.ordinal === witness.ordinal)
  if (conformance?.provider._tag !== 'Resolved') return structPlan
  const inferred = new Map<string, Type.Type>()
  if (!Type.infer(conformance.provider.type, type, inferred)) return structPlan
  return Object.freeze({
    _tag: 'HookCleanup',
    type,
    hook: Object.freeze({
      _tag: 'CanonicalDeclarationId' as const,
      module: witness.module,
      name: `drop@impl#${witness.ordinal}`,
    }),
    typeArguments: Object.freeze(
      conformance.typeParameters.map(
        (parameter) => inferred.get(Type.key(parameter.type)) ?? parameter.type,
      ),
    ),
    inner: structPlan,
  })
}

const cleanupTypeAtPath = (
  index: DeclarationIndex.Index,
  root: DeclarationIndex.SemanticType | undefined,
  path: ReadonlyArray<DeclarationIndex.FieldId>,
): DeclarationIndex.SemanticType | undefined => {
  let current = root
  for (const fieldId of path) {
    if (current === undefined || !Type.isNominal(current)) return undefined
    const declaration = DeclarationIndex.byCanonical(index, {
      _tag: 'CanonicalDeclarationId',
      module: current.module,
      name: current.name,
    })
    if (declaration?._tag !== 'StructDeclaration') return undefined
    const substitution = Type.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      current.arguments,
    )
    if (substitution === undefined) return undefined
    const field = declaration.fields.find(
      (candidate) =>
        candidate.id.struct.ordinal === fieldId.struct.ordinal &&
        candidate.id.ordinal === fieldId.ordinal,
    )
    current =
      field?.declaredType._tag === 'Resolved'
        ? Type.substitute(field.declaredType.type, substitution)
        : undefined
  }
  return current
}

/** Substitutes one checked symbolic cleanup proof into a concrete instance. */
export const specializeCleanup = (
  cleanup: CleanupPlan,
  substitution: Type.Substitution,
  resolveConcrete?: (type: Type.Type) => CleanupPlan,
): CleanupPlan => {
  const type = Type.substitute(cleanup.type, substitution)
  switch (cleanup._tag) {
    case 'NoCleanup':
      return Object.freeze({ _tag: 'NoCleanup', type })
    case 'ParameterCleanup':
      return Type.isParameter(type)
        ? Object.freeze({ _tag: 'ParameterCleanup', type })
        : (resolveConcrete?.(type) ?? Object.freeze({ _tag: 'NoCleanup', type }))
    case 'AllocationCleanup':
      return Type.equals(type, Type.allocation)
        ? Object.freeze({
            _tag: 'AllocationCleanup',
            type: Type.allocation,
            ticket: 'ActiveReclaimTicket',
          })
        : Object.freeze({ _tag: 'NoCleanup', type })
    case 'RawBufferCleanup':
      return Type.isRawBuffer(type)
        ? Object.freeze({
            _tag: 'RawBufferCleanup',
            type,
            allocation: cleanup.allocation,
          })
        : Object.freeze({ _tag: 'NoCleanup', type })
    case 'HookCleanup':
      if (!Type.isNominal(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'HookCleanup',
        type,
        hook: cleanup.hook,
        typeArguments: Object.freeze(
          cleanup.typeArguments.map((argument) => Type.substitute(argument, substitution)),
        ),
        inner: specializeCleanup(cleanup.inner, substitution, resolveConcrete),
      })
    case 'StructCleanup':
      if (!Type.isNominal(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'StructCleanup',
        type,
        fields: Object.freeze(
          cleanup.fields.map((field) =>
            Object.freeze({
              field: field.field,
              cleanup: specializeCleanup(field.cleanup, substitution, resolveConcrete),
            }),
          ),
        ),
      })
    case 'ArrayCleanup':
      if (!Type.isFixedArray(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'ArrayCleanup',
        type,
        length: type.length,
        element: specializeCleanup(cleanup.element, substitution, resolveConcrete),
      })
    case 'UnionCleanup':
      if (!Type.isUnion(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'UnionCleanup',
        type,
        cases: Object.freeze(
          cleanup.cases.map((entry, ordinal) => {
            const member = type.members.at(ordinal)
            return Object.freeze({
              member: member ?? entry.member,
              ordinal: entry.ordinal,
              cleanup: specializeCleanup(entry.cleanup, substitution, resolveConcrete),
            })
          }),
        ),
      })
    case 'CallableCleanup':
      if (!Type.isCallable(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'CallableCleanup',
        type,
        site: cleanup.site,
        slots: Object.freeze(
          cleanup.slots.map((slot) =>
            Object.freeze({
              ordinal: slot.ordinal,
              cleanup: specializeCleanup(slot.cleanup, substitution, resolveConcrete),
            }),
          ),
        ),
      })
    case 'EffectCleanup':
      if (!Type.isEffect(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'EffectCleanup',
        type,
        site: cleanup.site,
        slots: Object.freeze(
          cleanup.slots.map((slot) =>
            Object.freeze({
              ordinal: slot.ordinal,
              laneOffset: slot.laneOffset,
              laneCount: slot.laneCount,
              cleanup: specializeCleanup(slot.cleanup, substitution, resolveConcrete),
            }),
          ),
        ),
      })
  }
}

const borrowedReplacements = (
  fn: Elaboration.FunctionFact,
  index: DeclarationIndex.Index,
): ReadonlyArray<BorrowedReplacementFact> => {
  const replacements: Array<BorrowedReplacementFact> = []
  const walk = (statements: ReadonlyArray<Elaboration.StatementFact>): void => {
    for (const statement of statements) {
      if (statement._tag === 'IfStatement') {
        walk(statement.taken)
        walk(statement.otherwise)
        continue
      }
      if (statement._tag === 'WhileStatement') {
        walk(statement.body)
        continue
      }
      if (
        statement._tag !== 'WriteStatement' ||
        statement.root?._tag !== 'ParameterDeclaration' ||
        statement.destination.type._tag !== 'Available' ||
        borrowedPlaceAccess(statement.destination) !== 'Exclusive' ||
        !statement.compatible
      ) {
        continue
      }
      replacements.push(
        Object.freeze({
          _tag: 'BorrowedReplacement',
          root: statement.root.id,
          region: statement.region,
          type: statement.destination.type.type,
          displacedCleanup: cleanupPlan(index, statement.destination.type.type),
          span: statement.syntax.span,
        }),
      )
    }
  }
  walk(fn.statements)
  return Object.freeze(replacements)
}

const checkFunction = (
  fn: Hir.HirFunction,
  index: DeclarationIndex.Index,
  semantic?: Elaboration.FunctionFact,
): CheckedFunction => {
  const declaration = fn.declaration
  const loanAnalysis =
    semantic === undefined
      ? Object.freeze({ loans: Object.freeze([]), diagnostics: Object.freeze([]) })
      : analyzeLoans(semantic)
  const replacements =
    semantic === undefined ? Object.freeze([]) : borrowedReplacements(semantic, index)
  const state: CheckState = {
    index,
    bindings: new Map(),
    order: [],
    diagnostics: [...loanAnalysis.diagnostics],
    matches: [],
    callables: [],
  }

  const initialLive = new Set<string>()
  for (const parameter of declaration.parameters) {
    const type =
      parameter.declaredType._tag === 'Resolved' ? parameter.declaredType.type : undefined
    const binding: MutableBinding = {
      site: Object.freeze({ _tag: 'Parameter', parameter: parameter.id }),
      name: parameter.name._tag === 'Present' ? parameter.name.spelling : undefined,
      mutability:
        type !== undefined &&
        (Type.isSlice(type) || Type.isReference(type)) &&
        type.access === 'Exclusive'
          ? 'Mutable'
          : 'Immutable',
      liveFrom: parameter.syntax.span,
      liveTo: declaration.syntax.span,
      category: categoryOf(type),
      ...(type === undefined ? {} : { type }),
    }
    const key = siteKey(binding.site)
    state.bindings.set(key, binding)
    state.order.push(binding)
    initialLive.add(key)
  }

  const exits: Array<ExitDescriptor> = []
  /** Bindings local to deferred effect bodies: resolvable for releases, never published. */
  const deferredReleaseOrder: Array<MutableBinding> = []
  const continueStates = new Map<number, Array<Set<string>>>()
  const breakStates = new Map<number, Array<Set<string>>>()
  const fixedPoints: Array<{
    readonly loop: Hir.LoopId
    readonly span: SourceSpan.SourceSpan
    readonly incoming: Set<string>
    readonly repeating: ReadonlyArray<Set<string>>
    readonly following: Set<string>
    readonly compatible: boolean
    readonly iterations: number
  }> = []
  const appendLoopState = (
    states: Map<number, Array<Set<string>>>,
    loop: Hir.LoopId,
    live: Set<string>,
  ): void => {
    const existing = states.get(loop.ordinal)
    if (existing === undefined) states.set(loop.ordinal, [new Set(live)])
    else existing.push(new Set(live))
  }
  const sameLive = (left: ReadonlySet<string>, right: ReadonlySet<string>): boolean =>
    left.size === right.size && [...left].every((site) => right.has(site))
  const intersection = (states: ReadonlyArray<ReadonlySet<string>>): Set<string> => {
    const [first, ...rest] = states
    return new Set([...(first ?? [])].filter((site) => rest.every((state) => state.has(site))))
  }
  const frameSitesInnerFirst = (
    frames: ReadonlyArray<ReadonlyArray<string>>,
    live: ReadonlySet<string>,
  ): ReadonlyArray<string> =>
    [...frames].reverse().flatMap((frame) => [...frame].reverse().filter((site) => live.has(site)))

  const walkStatements = (
    statements: ReadonlyArray<Hir.Statement>,
    enclosingSpan: SourceSpan.SourceSpan,
    initial: Set<string>,
    frames: Array<Array<string>>,
    loopScopes: ReadonlyArray<{ readonly loop: Hir.LoopId; readonly frame: number }> = [],
  ): { readonly returned: boolean; readonly live: Set<string> } => {
    let live = initial
    for (const statement of statements) {
      // A fallible run can propagate its typed failure out of this function, and the owners
      // still live at that point must release on the way out. The exit is keyed by the run
      // expression's span so lowering can attach the releases to the run operation.
      for (const run of statementRootExpressions(statement).flatMap(fallibleRunSites)) {
        const consumed = new Set(consumedSites(run.subject))
        exits.push(
          Object.freeze({
            kind: 'Propagation' as const,
            span: run.span,
            sites: frameSitesInnerFirst(frames, live).filter((site) => !consumed.has(site)),
          }),
        )
      }
      // A lazy effect body is walked with its execution deferred: its moves never feed the
      // enclosing flow, and its loop, match, and binding facts are published by lowering
      // through its own compiled body rather than through these facts. Its exit plans DO
      // survive — the body's compiled runner reuses this function's span-keyed exit plans to
      // emit automatic cleanup, and the outer body never looks up a body-statement span.
      for (const block of statementRootExpressions(statement).flatMap(deferredBlocks)) {
        const bodyLive = new Set(live)
        const bodyFrame: Array<string> = []
        for (const capture of block.captures) {
          const site: BindingSite | undefined =
            capture.binding !== undefined
              ? Object.freeze({ _tag: 'Let', binding: capture.binding })
              : capture.parameter !== undefined
                ? Object.freeze({ _tag: 'Parameter', parameter: capture.parameter })
                : undefined
          if (site === undefined) continue
          bodyLive.add(siteKey(site))
          // A taken capture is owned by the body, so a failure inside it releases the value.
          if (capture.access === 'Take') bodyFrame.push(siteKey(site))
        }
        const marks = {
          exits: exits.length,
          fixedPoints: fixedPoints.length,
          order: state.order.length,
          matches: state.matches.length,
          callables: state.callables.length,
        }
        walkStatements(block.statements, block.span, bodyLive, [bodyFrame])
        deferredReleaseOrder.push(...state.order.slice(marks.order))
        fixedPoints.length = marks.fixedPoints
        state.order.length = marks.order
        state.matches.length = marks.matches
        state.callables.length = marks.callables
      }
      if (statement._tag === 'Unsafe') {
        const scopeFrames = [...frames.map((frame) => [...frame]), []]
        const result = walkStatements(
          statement.statements,
          statement.span,
          new Set(live),
          scopeFrames,
          loopScopes,
        )
        const frame = scopeFrames.at(-1) ?? []
        if (result.returned) return result
        if (frame.length > 0) {
          exits.push(
            Object.freeze({
              kind: 'ScopeEnd' as const,
              span: statement.span,
              region: statement.region,
              sites: Object.freeze([...frame].reverse().filter((site) => result.live.has(site))),
            }),
          )
        }
        for (const site of frame) result.live.delete(site)
        live = result.live
        continue
      }
      if (statement._tag === 'Bind') {
        checkExpression(state, live, statement.initializer, true)
        const type =
          statement.initializer._tag === 'Unavailable' ? undefined : statement.initializer.type
        const environment =
          statement.initializer._tag === 'CallableSection'
            ? callableEnvironment(state, statement.initializer)
            : undefined
        const binding: MutableBinding = {
          site: Object.freeze({ _tag: 'Let', binding: statement.binding }),
          name: statement.name,
          mutability: statement.mutability,
          liveFrom: statement.span,
          liveTo: enclosingSpan,
          category: categoryOf(type),
          ...(type === undefined ? {} : { type }),
          ...(environment === undefined || type === undefined || !Type.isCallable(type)
            ? {}
            : { cleanup: callableCleanup(environment, type) }),
        }
        const key = siteKey(binding.site)
        state.bindings.set(key, binding)
        state.order.push(binding)
        frames.at(-1)?.push(key)
        live.add(key)
        continue
      }
      if (statement._tag === 'Evaluate') {
        checkExpression(state, live, statement.expression, true)
        continue
      }
      if (statement._tag === 'If') {
        checkExpression(state, live, statement.condition, false)
        const continuing: Array<Set<string>> = []
        for (const [arm, body] of [
          ['Taken', statement.taken],
          ['Otherwise', statement.otherwise],
        ] as const) {
          const armFrames = [...frames.map((frame) => [...frame]), []]
          const result = walkStatements(body, statement.span, new Set(live), armFrames, loopScopes)
          const frame = armFrames.at(-1) ?? []
          if (!result.returned && frame.length > 0) {
            exits.push(
              Object.freeze({
                kind: 'ArmEnd' as const,
                span: statement.span,
                region: statement.region,
                arm,
                sites: Object.freeze([...frame].reverse().filter((site) => result.live.has(site))),
              }),
            )
          }
          if (!result.returned) {
            for (const site of frame) result.live.delete(site)
            continuing.push(result.live)
          }
        }
        if (continuing.length === 0) return Object.freeze({ returned: true, live })
        live = new Set(
          [...(continuing.at(0) ?? [])].filter((site) =>
            continuing.every((candidate) => candidate.has(site)),
          ),
        )
        continue
      }
      if (statement._tag === 'Write') {
        for (const selector of statement.place.selectors) {
          if (selector._tag === 'Index' || selector._tag === 'SliceIndex') {
            checkExpression(state, live, selector.index, false)
          }
        }
        const rootSite: BindingSite =
          statement.place._tag === 'WritePlace'
            ? Object.freeze({ _tag: 'Let', binding: statement.place.root })
            : statement.place.root._tag === 'BindingSliceRoot'
              ? Object.freeze({ _tag: 'Let', binding: statement.place.root.binding })
              : Object.freeze({ _tag: 'Parameter', parameter: statement.place.root.parameter })
        const rootKey = siteKey(rootSite)
        const root = state.bindings.get(rootKey)
        const wasLive = live.has(rootKey)
        if (!wasLive && statement.place.selectors.length > 0 && root !== undefined) {
          checkUse(state, live, rootSite, statement.place.span, false)
        }
        checkExpression(state, live, statement.value, true)
        if (wasLive && !live.has(rootKey)) {
          state.diagnostics.push(
            Diagnostic.overlappingAssignment(root?.name ?? '?', statement.span),
          )
        } else if (
          statement.place._tag === 'WritePlace' &&
          statement.place.selectors.length === 0
        ) {
          live.add(rootKey)
        }
        continue
      }
      if (statement._tag === 'While') {
        checkExpression(state, live, statement.condition, false)
        const incoming = new Set(live)
        const previousContinues = continueStates.get(statement.loop.ordinal)?.length ?? 0
        const previousBreaks = breakStates.get(statement.loop.ordinal)?.length ?? 0
        const loopFrames = [...frames.map((frame) => [...frame]), []]
        const loopResult = walkStatements(
          statement.body,
          statement.span,
          new Set(live),
          loopFrames,
          [...loopScopes, { loop: statement.loop, frame: loopFrames.length - 1 }],
        )
        const loopFrame = loopFrames.at(-1) ?? []
        const repeating: Array<Set<string>> = [
          ...(continueStates.get(statement.loop.ordinal)?.slice(previousContinues) ?? []),
        ]
        if (!loopResult.returned) {
          exits.push(
            Object.freeze({
              kind: 'LoopFallthrough' as const,
              span: statement.span,
              region: statement.region,
              target: statement.loop,
              sites: Object.freeze(
                [...loopFrame].reverse().filter((site) => loopResult.live.has(site)),
              ),
            }),
          )
          repeating.push(new Set(loopResult.live))
        }
        for (const candidate of repeating) {
          for (const site of loopFrame) candidate.delete(site)
        }
        const compatible = repeating.every((candidate) => sameLive(candidate, incoming))
        if (!compatible) {
          state.diagnostics.push(
            Diagnostic.incompatibleLoopHeader(statement.loop.ordinal, statement.span),
          )
        }
        const exitsFromLoop = breakStates.get(statement.loop.ordinal)?.slice(previousBreaks) ?? []
        for (const candidate of exitsFromLoop) {
          for (const site of loopFrame) candidate.delete(site)
        }
        live = intersection([incoming, ...exitsFromLoop])
        fixedPoints.push({
          loop: statement.loop,
          span: statement.span,
          incoming,
          repeating: Object.freeze(repeating.map((candidate) => new Set(candidate))),
          following: new Set(live),
          compatible,
          iterations: repeating.length === 0 ? 1 : 2,
        })
        continue
      }
      if (statement._tag === 'Break' || statement._tag === 'Continue') {
        const targetScope = [...loopScopes]
          .reverse()
          .find((scope) => scope.loop.ordinal === statement.target.ordinal)
        const transferFrames =
          targetScope === undefined ? [frames.at(-1) ?? []] : frames.slice(targetScope.frame)
        const transferSites = [...transferFrames].reverse().flatMap((frame) => [...frame].reverse())
        const sites = Object.freeze(transferSites.filter((site) => live.has(site)))
        exits.push(
          Object.freeze({
            kind: statement._tag,
            span: statement.span,
            region: statement.region,
            target: statement.target,
            sites,
          }),
        )
        const next = new Set(live)
        for (const site of transferSites) next.delete(site)
        appendLoopState(
          statement._tag === 'Break' ? breakStates : continueStates,
          statement.target,
          next,
        )
        return Object.freeze({ returned: true, live })
      }
      if (statement._tag === 'Drop') {
        checkExpression(state, live, statement.expression, true)
        continue
      }
      if (statement._tag === 'UnavailableStatement') {
        continue
      }
      const returnsBorrow =
        statement._tag === 'Return' &&
        fn.contract._tag === 'Contract' &&
        Type.containsViewBorrow(fn.contract.result)
      checkExpression(state, live, statement.expression, !returnsBorrow)
      exits.push(
        Object.freeze({
          kind: 'Return' as const,
          span: statement.span,
          region: statement.region,
          sites: frameSitesInnerFirst(frames, live),
        }),
      )
      return Object.freeze({ returned: true, live })
    }
    return Object.freeze({ returned: false, live })
  }

  const rootFrame = state.order
    .filter((binding) => binding.category._tag === 'MoveOnly')
    .map((binding) => siteKey(binding.site))
  const result = walkStatements(fn.statements, declaration.syntax.span, initialLive, [rootFrame])
  if (!result.returned) {
    exits.push(
      Object.freeze({
        kind: 'Return' as const,
        span: fn.statements.at(-1)?.span ?? declaration.syntax.span,
        sites: frameSitesInnerFirst([rootFrame], result.live),
      }),
    )
  }

  const bindingFactOf = (binding: MutableBinding): BindingFact =>
    Object.freeze({
      _tag: 'Binding',
      site: binding.site,
      name: binding.name,
      mutability: binding.mutability,
      category: binding.category,
      ...(binding.type === undefined ? {} : { type: binding.type }),
      cleanup:
        binding.cleanup ??
        (binding.type === undefined
          ? Object.freeze({ _tag: 'NoCleanup' as const, type: 'i32' as const })
          : cleanupPlan(index, binding.type)),
      liveFrom: binding.liveFrom,
      liveTo: binding.liveTo,
      ...(binding.movedAt === undefined ? {} : { movedAt: binding.movedAt }),
    })
  const bindings = Object.freeze(state.order.map(bindingFactOf))
  const deferredBindings = Object.freeze(deferredReleaseOrder.map(bindingFactOf))
  const bindingBySite = new Map(
    [...bindings, ...deferredBindings].map((binding) => [siteKey(binding.site), binding] as const),
  )

  const exitPlans = Object.freeze(
    exits.map(
      (exit): ExitPlan =>
        Object.freeze({
          _tag: 'Exit' as const,
          kind: exit.kind,
          span: exit.span,
          ...(exit.region === undefined ? {} : { region: exit.region }),
          ...(exit.arm === undefined ? {} : { arm: exit.arm }),
          ...(exit.target === undefined ? {} : { target: exit.target }),
          loanEnds: Object.freeze(
            loanAnalysis.loans
              .filter(
                (loan) =>
                  exit.region !== undefined && loan.endRegion.ordinal === exit.region.ordinal,
              )
              .map((loan) => loan.id),
          ),
          releases: Object.freeze(
            exit.sites.flatMap((site): ReadonlyArray<Release> => {
              const fact = bindingBySite.get(site)
              if (fact === undefined) return []
              return [
                Object.freeze({
                  _tag: 'Release' as const,
                  binding: fact,
                  fields:
                    fact.category._tag === 'MoveOnly' && Type.isNominal(fact.category.type)
                      ? cleanupFields(index, fact.category.type)
                      : Object.freeze([]),
                  cleanup: fact.cleanup,
                }),
              ]
            }),
          ),
        }),
    ),
  )

  const firstUnavailable = Hir.firstUnavailable(fn)
  const violation = state.diagnostics.at(0)
  const verdict: Verdict =
    fn.contract._tag === 'Unavailable'
      ? Object.freeze({
          _tag: 'Unavailable',
          ...(fn.contract.cause === undefined ? {} : { cause: fn.contract.cause }),
        })
      : firstUnavailable !== undefined
        ? Object.freeze({
            _tag: 'Unavailable',
            ...(firstUnavailable.cause === undefined ? {} : { cause: firstUnavailable.cause }),
          })
        : violation !== undefined
          ? Object.freeze({ _tag: 'Violation', cause: Diagnostic.identity(violation) })
          : satisfied

  return Object.freeze({
    ownership: Object.freeze({
      _tag: 'FunctionOwnership' as const,
      declaration,
      bindings,
      deferredBindings,
      exits: exitPlans,
      fixedPoints: Object.freeze(
        fixedPoints.map((point) => {
          const sites = (keys: ReadonlySet<string>): ReadonlyArray<BindingSite> =>
            Object.freeze(
              [...keys].flatMap((key): ReadonlyArray<BindingSite> => {
                const binding = bindingBySite.get(key)
                return binding === undefined ? [] : [binding.site]
              }),
            )
          return Object.freeze({
            _tag: 'LoopFixedPoint' as const,
            loop: point.loop,
            span: point.span,
            incoming: sites(point.incoming),
            repeating: Object.freeze(point.repeating.map(sites)),
            following: sites(point.following),
            compatible: point.compatible,
            iterations: point.iterations,
          })
        }),
      ),
      matches: Object.freeze(state.matches),
      callables: Object.freeze(state.callables),
      loans: loanAnalysis.loans,
      borrowedReplacements: replacements,
      verdict,
    }),
    diagnostics: Object.freeze([...state.diagnostics]),
  })
}

/** Checks every declaration of one elaborated module once, producing its ownership facts. */
export const checkModule = (
  result: Elaboration.Result,
  index: DeclarationIndex.Index,
): ModuleOwnership => {
  const checked = result.hir.functions.map((fn, ordinal) =>
    checkFunction(fn, index, result.functions.at(ordinal)),
  )
  return Object.freeze({
    _tag: 'OwnershipFacts',
    module: result.syntax.source.id,
    functions: Object.freeze(checked.map((entry) => entry.ownership)),
    diagnostics: Object.freeze(
      checked.flatMap((entry) => entry.diagnostics).sort(Diagnostic.compare),
    ),
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

const verdictText = (verdict: Verdict): string => {
  switch (verdict._tag) {
    case 'Satisfied':
      return 'satisfied'
    case 'Violation':
      return 'violation'
    case 'Unavailable':
      return 'unavailable'
  }
}

const siteText = (site: BindingSite): string =>
  site._tag === 'Parameter'
    ? `p${site.parameter.ordinal}`
    : site._tag === 'Let'
      ? `b${site.binding.ordinal}`
      : `m${site.binding.arm.match.span.start}.a${site.binding.arm.ordinal}.p${site.binding.ordinal}`

const cleanupText = (cleanup: CleanupPlan): string => {
  if (cleanup._tag === 'NoCleanup') return `none:${Type.encode(cleanup.type)}`
  if (cleanup._tag === 'ParameterCleanup') return `parameter:${Type.key(cleanup.type)}`
  if (cleanup._tag === 'AllocationCleanup')
    return `allocation:${Type.encode(cleanup.type)} ticket=${cleanup.ticket}`
  if (cleanup._tag === 'RawBufferCleanup')
    return `raw-buffer:${Type.encode(cleanup.type)} owner=(${cleanupText(cleanup.allocation)})`
  if (cleanup._tag === 'ArrayCleanup') {
    return `array:${Type.encode(cleanup.type)} length=${cleanup.length} element=(${cleanupText(cleanup.element)})`
  }
  if (cleanup._tag === 'UnionCleanup') {
    return `union:${Type.encode(cleanup.type)} cases=${cleanup.cases
      .map(
        (member) =>
          `${member.ordinal}:${Type.encode(member.member)}(${cleanupText(member.cleanup)})`,
      )
      .join(',')}`
  }
  if (cleanup._tag === 'CallableCleanup') {
    return `callable:${Type.encode(cleanup.type)} site=${Hir.executableSiteLabel(cleanup.site)} slots=${cleanup.slots.map((slot) => `#${slot.ordinal}(${cleanupText(slot.cleanup)})`).join(',') || 'none'}`
  }
  if (cleanup._tag === 'EffectCleanup') {
    return `effect:${Type.encode(cleanup.type)} site=${Hir.executableSiteLabel(cleanup.site)} slots=${cleanup.slots.map((slot) => `#${slot.ordinal}(${cleanupText(slot.cleanup)})`).join(',') || 'none'}`
  }
  if (cleanup._tag === 'HookCleanup') {
    return `hook:${Type.encode(cleanup.type)} target=${cleanup.hook.module}.${cleanup.hook.name}${
      cleanup.typeArguments.length === 0
        ? ''
        : `<${cleanup.typeArguments.map(Type.encode).join(',')}>`
    } inner=(${cleanupText(cleanup.inner)})`
  }
  return `struct:${Type.encode(cleanup.type)} fields=${cleanup.fields
    .map((field) => `#${field.field.ordinal}(${cleanupText(field.cleanup)})`)
    .join(',')}`
}

/**
 * Deterministic textual encoding of one module's ownership facts and cleanup plans for
 * debugging, inspection, and golden tests. No compatibility promise attaches to this format.
 */
export const encode = (self: ModuleOwnership): string =>
  [
    `ownership-module ${self.module}`,
    ...self.functions.flatMap((fn) => [
      `fn ${identityLabel(fn.declaration)} ${verdictText(fn.verdict)}`,
      ...fn.bindings.map((binding) => {
        const category =
          binding.category._tag === 'Copyable'
            ? 'copyable'
            : `move-only ${Type.encode(binding.category.type)}`
        return `  binding ${siteText(binding.site)} ${binding.name ?? '?'} ${category} live ${spanText(binding.liveFrom)}..${spanText(binding.liveTo)}${binding.movedAt === undefined ? '' : ` moved ${spanText(binding.movedAt)}`}`
      }),
      ...fn.loans.map(
        (loan) =>
          `  loan l${loan.id.ordinal} ${loan.access.toLowerCase()} ${siteText(loan.root)} ${loan.origin === 'SliceReborrow' ? `reborrow parent=${loan.parent === undefined ? '?' : siteText(loan.parent)} suspended=${loan.suspendsParent}` : loan.origin === 'ReturnedView' ? 'returned-view' : 'array'} region=${loan.startRegion.ordinal}->${loan.endRegion.ordinal} ${spanText(loan.startSpan)}..${spanText(loan.endSpan)}`,
      ),
      ...fn.borrowedReplacements.map(
        (replacement) =>
          `  borrowed-replace p${replacement.root.ordinal} region=${replacement.region.ordinal} type=${Type.encode(replacement.type)} cleanup=${cleanupText(replacement.displacedCleanup)} ${spanText(replacement.span)}`,
      ),
      ...fn.callables.map(
        (callable) =>
          `  callable ${Hir.executableSiteLabel(callable.site)} ${callable.mode.toLowerCase()} slots=${callable.slots.map((slot) => `#${slot.ordinal}:p${slot.parameterOrdinal}:${slot.access.toLowerCase()}:${slot.type === undefined ? '?' : Type.encode(slot.type)}:${cleanupText(slot.cleanup)}`).join(',') || 'none'} retained=${callable.retainedDependencies.join(',') || 'none'} drop=${callable.dropOrder.map((ordinal) => `#${ordinal}`).join(',') || 'none'}`,
      ),
      ...fn.exits.map((exit) => {
        const label = (() => {
          switch (exit.kind) {
            case 'Return':
              return 'return'
            case 'ArmEnd':
              return `arm-end ${exit.arm === 'Otherwise' ? 'otherwise' : 'taken'}`
            case 'LoopFallthrough':
              return `loop${exit.target?.ordinal ?? '?'} fallthrough`
            case 'Break':
              return `break loop${exit.target?.ordinal ?? '?'}`
            case 'Continue':
              return `continue loop${exit.target?.ordinal ?? '?'}`
            case 'Propagation':
              return 'propagation'
          }
        })()
        const loanEnds = exit.loanEnds.map((loan) => `l${loan.ordinal}`).join(',') || 'none'
        return exit.releases.length === 0
          ? `  exit ${label} ${spanText(exit.span)} loan-ends ${loanEnds} releases none`
          : [
              `  exit ${label} ${spanText(exit.span)} loan-ends ${loanEnds}`,
              ...exit.releases.map(
                (release) =>
                  `    release ${siteText(release.binding.site)}${release.fields.length === 0 ? '' : ` fields ${release.fields.map((field) => `#${field.ordinal}`).join(',')}`}${release.cleanup._tag === 'ArrayCleanup' || release.cleanup._tag === 'CallableCleanup' ? ` cleanup ${cleanupText(release.cleanup)}` : ''}`,
              ),
            ].join('\n')
      }),
      ...fn.fixedPoints.map(
        (point) =>
          `  loop${point.loop.ordinal} fixed-point ${point.compatible ? 'compatible' : 'incompatible'} iterations=${point.iterations} incoming=${point.incoming.map(siteText).join(',') || 'none'} repeating=${point.repeating.map((state) => `[${state.map(siteText).join(',')}]`).join(',') || 'none'} following=${point.following.map(siteText).join(',') || 'none'}`,
      ),
      ...fn.matches.map((match) =>
        [
          `  match ${match.access.toLowerCase()} ${spanText(match.span)}`,
          ...match.arms.map(
            (arm) =>
              `    arm #${arm.id.ordinal} ${arm.universal ? '_' : arm.member === undefined ? 'unknown' : Type.encode(arm.member)} guard=${arm.provisionalGuard} bindings=${arm.bindings.map(siteText).join(',') || 'none'} cleanup=${arm.cleanup.map((entry) => `${entry.path.map((field) => `#${field.ordinal}`).join('.') || 'payload'}(${cleanupText(entry.cleanup)})`).join(',') || 'none'}`,
          ),
        ].join('\n'),
      ),
    ]),
    '',
  ].join('\n')
