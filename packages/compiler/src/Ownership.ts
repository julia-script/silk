import * as CleanupPlan from './CleanupPlan.js'
import * as ConformanceProof from './ConformanceProof.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as ExecutionAffinity from './ExecutionAffinity.js'
import * as FieldRealization from './FieldRealization.js'
import * as Hir from './Hir.js'
import { equal as setEqual } from './internal/SetOf.js'
import * as LocalSharedOwnership from './LocalSharedOwnership.js'
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
  | { readonly _tag: 'MoveOnly'; readonly type: DeclarationFacts.SemanticType }
  | { readonly _tag: 'Unavailable' }

/** Where one binding was introduced: a parameter or a `let` statement. */
export type BindingSite =
  | { readonly _tag: 'Parameter'; readonly parameter: DeclarationFacts.ParameterId }
  | { readonly _tag: 'Let'; readonly binding: Hir.BindingId }
  | { readonly _tag: 'Pattern'; readonly binding: Match.BindingId }
  | { readonly _tag: 'Temporary'; readonly owner: Hir.TemporaryOwnerId }

/** One binding's ownership fact: site, category, live range, and consuming move if any. */
export interface BindingFact {
  readonly _tag: 'Binding'
  readonly site: BindingSite
  readonly name: string | undefined
  readonly mutability: 'Immutable' | 'Mutable'
  readonly category: OwnershipCategory
  readonly executionAffinity: ExecutionAffinity.ExecutionAffinity
  readonly localSharedObligations: LocalSharedOwnership.ObligationPlan
  readonly type?: DeclarationFacts.SemanticType
  readonly cleanup: CleanupPlan.CleanupPlan
  readonly liveFrom: SourceSpan.SourceSpan
  readonly liveTo: SourceSpan.SourceSpan
  readonly movedAt?: SourceSpan.SourceSpan
}

/** One ordered release of an owned binding at a structured exit. */
export interface Release {
  readonly _tag: 'Release'
  readonly binding: BindingFact
  readonly fields: ReadonlyArray<DeclarationFacts.FieldId>
  readonly cleanup: CleanupPlan.CleanupPlan
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
    | 'InterfaceOperand'
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
  readonly root: DeclarationFacts.ParameterId
  readonly region: Hir.RegionId
  readonly type: DeclarationFacts.SemanticType
  readonly displacedCleanup: CleanupPlan.CleanupPlan
  readonly span: SourceSpan.SourceSpan
}

/** One compiler-planned slot in a concrete callable section environment. */
export interface CallableEnvironmentSlot {
  readonly ordinal: number
  readonly parameterOrdinal: number
  readonly access: Type.CaptureAccess
  readonly type?: DeclarationFacts.SemanticType
  readonly cleanup: CleanupPlan.CleanupPlan
  readonly executionAffinity: ExecutionAffinity.ExecutionAffinity
  readonly localSharedObligations: LocalSharedOwnership.ObligationPlan
}

/** Ownership facts for one hidden callable section environment. */
export interface CallableEnvironmentFact {
  readonly _tag: 'CallableEnvironment'
  readonly site: Hir.CallableSiteId
  readonly mode: Type.CallableMode
  readonly slots: ReadonlyArray<CallableEnvironmentSlot>
  readonly executionAffinity: ExecutionAffinity.ExecutionAffinity
  readonly localSharedObligations: LocalSharedOwnership.ObligationPlan
  readonly retainedDependencies: ReadonlyArray<number>
  readonly dropOrder: ReadonlyArray<number>
  readonly span: SourceSpan.SourceSpan
}

/**
 * Returns owned entries in deterministic last-acquired-first-released order,
 * deduplicated by ordinal so no release is double-issued.
 */
export const inReleaseOrder = <T extends { readonly ordinal: number }>(
  entries: ReadonlyArray<T>,
): ReadonlyArray<T> =>
  Object.freeze(
    [...entries]
      .reverse()
      .filter(
        (entry, ordinal, all) =>
          all.findIndex((candidate) => candidate.ordinal === entry.ordinal) === ordinal,
      ),
  )

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
  readonly declaration: DeclarationFacts.DeclarationFact
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
    readonly member?: Type.Type
    readonly universal: boolean
    readonly provisionalGuard: boolean
    readonly bindings: ReadonlyArray<BindingSite>
    readonly cleanup: ReadonlyArray<{
      readonly path: ReadonlyArray<DeclarationFacts.FieldId>
      readonly cleanup: CleanupPlan.CleanupPlan
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

const categoryOf = (
  index: DeclarationIndex.Index,
  type: DeclarationFacts.SemanticType | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): OwnershipCategory =>
  type === undefined
    ? Object.freeze({ _tag: 'Unavailable' })
    : (Type.isEffect(type) && type.access === 'Shared') ||
        (Type.isCallable(type) && type.mode === 'Shared') ||
        ConformanceProof.copyType(index, type, assumptions)
      ? copyable
      : Object.freeze({ _tag: 'MoveOnly', type })

const siteKey = (site: BindingSite): string =>
  site._tag === 'Parameter'
    ? `p${site.parameter.ordinal}`
    : site._tag === 'Let'
      ? `b${site.binding.ordinal}`
      : site._tag === 'Pattern'
        ? `m${site.binding.arm.match.span.start}.a${site.binding.arm.ordinal}.p${site.binding.ordinal}`
        : `t${site.owner.span.sourceId}:${site.owner.span.start}:${site.owner.span.end}:${site.owner.ordinal}`

interface MutableBinding {
  readonly site: BindingSite
  readonly name: string | undefined
  readonly mutability: 'Immutable' | 'Mutable'
  readonly liveFrom: SourceSpan.SourceSpan
  readonly category: OwnershipCategory
  readonly type?: DeclarationFacts.SemanticType
  readonly cause?: Diagnostic.Identity
  readonly executionAffinity?: ExecutionAffinity.ExecutionAffinity
  readonly localSharedObligations?: LocalSharedOwnership.ObligationPlan
  readonly cleanup?: CleanupPlan.CleanupPlan
  liveTo: SourceSpan.SourceSpan
  movedAt?: SourceSpan.SourceSpan
  readonly matchAccess?: Match.Access
}

interface CheckState {
  readonly index: DeclarationIndex.Index
  readonly copyAssumptions: ReadonlySet<string>
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

const retainedBinding = (
  state: CheckState,
  expression: Hir.Expression,
): MutableBinding | undefined => {
  const source =
    expression._tag === 'Move'
      ? expression.subject
      : expression._tag === 'UnionConvert'
        ? expression.source
        : expression
  const site = useSite(source)
  return site === undefined ? undefined : state.bindings.get(siteKey(site))
}

const borrowRootType = (state: CheckState, expression: Hir.Expression): Type.Type | undefined => {
  if (expression._tag !== 'SliceBorrow' && expression._tag !== 'ValueBorrow') return undefined
  if (expression.root._tag === 'TemporarySliceRoot')
    return expression.root.value._tag === 'Unavailable' ? undefined : expression.root.value.type
  const site: BindingSite =
    expression.root._tag === 'BindingSliceRoot'
      ? Object.freeze({ _tag: 'Let', binding: expression.root.binding })
      : expression.root._tag === 'ParameterSliceRoot'
        ? Object.freeze({ _tag: 'Parameter', parameter: expression.root.parameter })
        : Object.freeze({ _tag: 'Pattern', binding: expression.root.binding })
  return state.bindings.get(siteKey(site))?.type
}

/**
 * The callable contract one place stores, when the place is a nominal field holding a callable.
 *
 * A monomorphic body projects a `Represented` field and a generic body projects the field's
 * declaration-owned representation bound; both name the same contract, and neither is read from the
 * construction that filled the field. A field of any other type stores no callable.
 */
const storedCallableContract = (place: Hir.Expression): Type.Callable | undefined => {
  if (place._tag !== 'Project') return undefined
  const type = place.type
  if (Type.isRepresented(type)) return Type.isCallable(type.contract) ? type.contract : undefined
  return Type.isCallable(type) ? type : undefined
}

/** The Effect contract one place stores, when the place is a represented nominal field. */
const storedEffectContract = (place: Hir.Expression): Type.Effect | undefined => {
  if (place._tag !== 'Project') return undefined
  const type = place.type
  if (Type.isRepresented(type))
    return Type.isEffect(type.representation.requiredBound)
      ? type.representation.requiredBound
      : undefined
  return undefined
}

/** The root expression one place projects from, which owns or borrows the whole aggregate. */
const placeRoot = (place: Hir.Expression): Hir.Expression =>
  place._tag === 'Project' || place._tag === 'IndexPlace' ? placeRoot(place.subject) : place

/**
 * The strongest aggregate receiver access one place offers the callable it stores.
 *
 * A whole owner offers take access; any borrow the place travels through weakens the whole place to
 * that borrow's access, because the stored environment is only ever reached through it.
 */
const receiverAccess = (
  state: CheckState,
  place: Hir.Expression,
): FieldRealization.ReceiverAccess => {
  if (place._tag !== 'Project' && place._tag !== 'IndexPlace') {
    const site = useSite(place)
    const matchAccess =
      site === undefined ? undefined : state.bindings.get(siteKey(site))?.matchAccess
    return matchAccess === 'Shared' || matchAccess === 'Exclusive' ? matchAccess : 'Take'
  }
  const subject = place.subject
  const subjectType = subject._tag === 'Unavailable' ? undefined : subject.type
  const through: FieldRealization.ReceiverAccess =
    subjectType !== undefined && (Type.isReference(subjectType) || Type.isSlice(subjectType))
      ? subjectType.access
      : 'Take'
  return FieldRealization.weakerAccess(receiverAccess(state, place.subject), through)
}

/**
 * Rejects invoking a stored callable through an aggregate receiver too weak for its mode. The rule
 * itself lives on the shared realization actor, so this pre-specialization rejection and the runtime
 * invocation it protects can never disagree about which receiver admits which mode.
 */
const storedCallableInvocationAccess = (
  state: CheckState,
  callee: Hir.Expression,
  access: Type.CallableMode,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic | undefined => {
  if (callee._tag !== 'Project') return undefined
  const contract = storedCallableContract(callee)
  if (contract === undefined) return undefined
  const receiver = receiverAccess(state, callee)
  if (FieldRealization.admitsMode(receiver, access)) return undefined
  return Diagnostic.storedCallableInvocationAccess(
    Type.encode(callee.nominal),
    `#${callee.field.ordinal}`,
    Type.encode(contract),
    receiver,
    access,
    span,
  )
}

/** Rejects running a stored Effect through aggregate access weaker than its representation bound. */
const storedEffectRunAccess = (
  state: CheckState,
  subject: Hir.Expression,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic | undefined => {
  if (subject._tag !== 'Project') return undefined
  const contract = storedEffectContract(subject)
  if (contract === undefined) return undefined
  const receiver = receiverAccess(state, subject)
  if (FieldRealization.admitsMode(receiver, contract.access)) return undefined
  return Diagnostic.storedEffectRunAccess(
    Type.encode(subject.nominal),
    `#${subject.field.ordinal}`,
    Type.encode(contract),
    receiver,
    contract.access,
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
      const cause = capture.value._tag === 'Unavailable' ? capture.value.cause : undefined
      const retained = retainedBinding(state, capture.value)
      const root = borrowRootType(state, capture.value)
      return Object.freeze({
        ordinal: capture.ordinal,
        parameterOrdinal: capture.parameterOrdinal,
        access: capture.access,
        ...(type === undefined ? {} : { type }),
        executionAffinity:
          type === undefined
            ? ExecutionAffinity.ofEnvironment(state.index, [
                Object.freeze(cause === undefined ? {} : { cause }),
              ])
            : root !== undefined
              ? ExecutionAffinity.ofBorrow(state.index, type, root)
              : (retained?.executionAffinity ?? ExecutionAffinity.ofType(state.index, type)),
        localSharedObligations:
          capture.access === 'Take' && type !== undefined
            ? (retained?.localSharedObligations ?? LocalSharedOwnership.ofType(state.index, type))
            : LocalSharedOwnership.none,
        cleanup:
          capture.access === 'Take' && type !== undefined
            ? CleanupPlan.cleanupPlan(state.index, type)
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
    executionAffinity: ExecutionAffinity.join(slots.map((slot) => slot.executionAffinity)),
    localSharedObligations: LocalSharedOwnership.combine(
      slots.map((slot) => slot.localSharedObligations),
    ),
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

const executableEnvironment = (
  state: CheckState,
  expression: Hir.Expression,
):
  | {
      readonly affinity: ExecutionAffinity.ExecutionAffinity
      readonly obligations: LocalSharedOwnership.ObligationPlan
    }
  | undefined => {
  if (expression._tag === 'CallableSection') {
    const environment = callableEnvironment(state, expression)
    return Object.freeze({
      affinity: environment.executionAffinity,
      obligations: environment.localSharedObligations,
    })
  }
  const retained = retainedBinding(state, expression)
  if (retained?.executionAffinity !== undefined && retained.localSharedObligations !== undefined)
    return Object.freeze({
      affinity: retained.executionAffinity,
      obligations: retained.localSharedObligations,
    })
  if (expression._tag === 'EffectBlock') {
    const captures = expression.captures.map((capture) => {
      const site: BindingSite | undefined =
        capture.binding !== undefined
          ? Object.freeze({ _tag: 'Let', binding: capture.binding })
          : capture.parameter !== undefined
            ? Object.freeze({ _tag: 'Parameter', parameter: capture.parameter })
            : undefined
      return Object.freeze({
        access: capture.access,
        binding: site === undefined ? undefined : state.bindings.get(siteKey(site)),
      })
    })
    return Object.freeze({
      affinity: ExecutionAffinity.join(
        captures.map(
          ({ binding }) =>
            binding?.executionAffinity ??
            (binding?.type === undefined
              ? ExecutionAffinity.ofEnvironment(state.index, [
                  Object.freeze(binding?.cause === undefined ? {} : { cause: binding.cause }),
                ])
              : ExecutionAffinity.ofType(state.index, binding.type)),
        ),
      ),
      obligations: LocalSharedOwnership.combine(
        captures.map(({ access, binding }) =>
          access !== 'Take'
            ? LocalSharedOwnership.none
            : (binding?.localSharedObligations ??
              (binding?.type === undefined
                ? LocalSharedOwnership.ofEnvironment(state.index, [
                    Object.freeze(
                      binding?.cause === undefined
                        ? { access: 'Take' as const }
                        : { access: 'Take' as const, cause: binding.cause },
                    ),
                  ])
                : LocalSharedOwnership.ofType(state.index, binding.type))),
        ),
      ),
    })
  }
  const components: ReadonlyArray<{
    readonly access: Type.CaptureAccess
    readonly type?: Type.Type
    readonly cause?: Diagnostic.Identity
  }> =
    expression._tag === 'EffectConstruct' || expression._tag === 'ServiceEffectConstruct'
      ? expression.arguments.map((argument) =>
          Object.freeze({
            access: 'Take' as const,
            ...(argument._tag === 'Unavailable'
              ? argument.cause === undefined
                ? {}
                : { cause: argument.cause }
              : { type: argument.type }),
          }),
        )
      : Object.freeze([])
  if (components.length === 0) return undefined
  return Object.freeze({
    affinity: ExecutionAffinity.ofEnvironment(state.index, components),
    obligations: LocalSharedOwnership.ofEnvironment(state.index, components),
  })
}

const callableCleanup = (
  environment: CallableEnvironmentFact,
  type: Type.Callable,
): CleanupPlan.CleanupPlan =>
  Object.freeze({
    _tag: 'CallableCleanup',
    type,
    environment: Object.freeze({ _tag: 'CallableEnvironmentSite', site: environment.site }),
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

/**
 * Checks every operand a place evaluates except its root binding, which the caller uses once with
 * the access the whole place demands. Splitting the walk keeps one use per root, so a consuming
 * invocation cannot report the root twice.
 */
const checkPlaceInterior = (
  state: CheckState,
  live: Set<string>,
  place: Hir.Expression,
  guard: boolean,
  escaping: boolean,
): void => {
  if (place._tag === 'Project') {
    checkPlaceInterior(state, live, place.subject, guard, escaping)
    return
  }
  if (place._tag === 'IndexPlace') {
    checkPlaceInterior(state, live, place.subject, guard, escaping)
    checkExpression(state, live, place.index, false, guard, escaping)
    return
  }
  if (useSite(place) !== undefined) return
  checkExpression(state, live, place, false, guard, escaping)
}

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
        state.diagnostics.push(Diagnostic.partialMove(expression.span))
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
      if (
        consuming &&
        categoryOf(state.index, expression.type, state.copyAssumptions)._tag === 'MoveOnly'
      ) {
        state.diagnostics.push(Diagnostic.partialMove(expression.span))
      }
      return
    }
    case 'IndexPlace': {
      checkExpression(state, live, expression.subject, false, guard, escaping)
      checkExpression(state, live, expression.index, false, guard, escaping)
      if (
        consuming &&
        categoryOf(state.index, expression.type, state.copyAssumptions)._tag === 'MoveOnly'
      ) {
        state.diagnostics.push(Diagnostic.partialMove(expression.span))
      }
      return
    }
    case 'SliceBorrow':
    case 'ValueBorrow': {
      if (expression.root._tag === 'TemporarySliceRoot') {
        checkExpression(state, live, expression.root.value, true, guard, escaping)
        return
      }
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
      if (
        consuming &&
        categoryOf(state.index, expression.type, state.copyAssumptions)._tag === 'MoveOnly'
      ) {
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
      const stored = storedCallableInvocationAccess(
        state,
        expression.callee,
        expression.access,
        expression.span,
      )
      if (stored !== undefined) state.diagnostics.push(stored)
      const checkCallee = (): void => {
        if (storedCallableContract(expression.callee) !== undefined) {
          // A stored callable is invoked through its aggregate, never extracted from it. A
          // consuming invocation takes the whole owner in one use of its root, exactly as invoking
          // a take-once callable binding does; every weaker mode only reads the place. The
          // extraction rejection stays for `move parser.decode`, which really does try to leave the
          // aggregate holding a partially released environment.
          const rootSite = placeSite(expression.callee)
          if (expression.access !== 'Take' || stored !== undefined || rootSite === undefined) {
            checkExpression(state, live, expression.callee, false, guard, escaping)
            return
          }
          checkPlaceInterior(state, live, expression.callee, guard, escaping)
          checkUse(state, live, rootSite, placeRoot(expression.callee).span, true)
          return
        }
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
    case 'BuiltinCall': {
      for (const [ordinal, argument] of expression.arguments.entries()) {
        const operand = expression.interfaceOperation?.contract.operands.at(ordinal)
        const type = operand?.type._tag === 'Resolved' ? operand.type.type : undefined
        checkExpression(
          state,
          live,
          argument,
          type !== undefined && !Type.isReference(type) && !Type.isSlice(type),
          guard,
          escaping,
        )
      }
      return
    }
    case 'BoundOperationCall': {
      for (const [ordinal, argument] of expression.arguments.entries()) {
        const operand = expression.contract.operands.at(ordinal)
        const type = operand?.type._tag === 'Resolved' ? operand.type.type : undefined
        checkExpression(
          state,
          live,
          argument,
          type !== undefined && !Type.isReference(type) && !Type.isSlice(type),
          guard,
          escaping,
        )
      }
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
        checkUse(
          state,
          live,
          site,
          expression.provider.span,
          expression.provider.captureAccess === 'Take',
        )
      return
    }
    case 'EffectCatch':
      // The sealed primitive has the same owned operands as its ordinary callable contract.
      // Visiting both here preserves take-once use checking after elaboration replaces the call
      // with dedicated HIR.
      checkExpression(
        state,
        live,
        expression.protected,
        argumentConsumes(expression.protected),
        guard,
        escaping,
      )
      checkExpression(
        state,
        live,
        expression.handler,
        argumentConsumes(expression.handler),
        guard,
        escaping,
      )
      return
    case 'Run': {
      const stored = storedEffectRunAccess(state, expression.subject, expression.span)
      if (stored !== undefined) state.diagnostics.push(stored)
      const storedContract = storedEffectContract(expression.subject)
      const rootSite = placeSite(expression.subject)
      if (storedContract?.access === 'Take' && stored === undefined && rootSite !== undefined) {
        // A stored consuming Effect owns its environment through the aggregate. Running it moves
        // the complete root in one use, including through nested field projections; the Effect
        // field is never extracted as an independently owned value.
        checkPlaceInterior(state, live, expression.subject, guard, escaping)
        checkUse(state, live, rootSite, placeRoot(expression.subject).span, true)
        return
      }
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
        if (categoryOf(state.index, scrutineeType, state.copyAssumptions)._tag === 'MoveOnly') {
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
            category: categoryOf(state.index, pattern.type, state.copyAssumptions),
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
                  const type = CleanupPlan.cleanupTypeAtPath(
                    state.index,
                    arm.member ?? scrutineeType,
                    path,
                  )
                  return type === undefined
                    ? []
                    : [Object.freeze({ path, cleanup: CleanupPlan.cleanupPlan(state.index, type) })]
                }),
                ...arm.bindings.flatMap((binding) => {
                  const site: BindingSite = Object.freeze({ _tag: 'Pattern', binding: binding.id })
                  return armLive.has(siteKey(site)) &&
                    categoryOf(state.index, binding.type, state.copyAssumptions)._tag === 'MoveOnly'
                    ? [
                        Object.freeze({
                          path: binding.path,
                          cleanup: CleanupPlan.cleanupPlan(state.index, binding.type),
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
    case 'PatternBind':
      return [statement.selection.subject]
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
    case 'IfLet':
      return [statement.selection.subject]
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
 * A run is fallible when its effect type carries concrete or symbolic failures. In a generic body
 * the caller's failures arrive as ordinary symbolic union members, so concrete
 * members alone are insufficient to determine whether the run may propagate.
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
    !Type.isNever(Type.failureType(subjectType as Type.Effect))
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
      : root._tag === 'PatternRoot'
        ? Object.freeze({ _tag: 'Pattern', binding: root.binding.id })
        : Object.freeze({ _tag: 'Temporary', owner: root.owner })

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

const analyzeLoans = (
  fn: Elaboration.FunctionFact,
  index: DeclarationIndex.Index,
  copyAssumptions: ReadonlySet<string>,
): LoanAnalysis => {
  const loans: Array<LoanFact> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []

  const directSite = (
    expression: Elaboration.ExpressionFact,
  ): { readonly site: BindingSite; readonly spelling: string } | undefined => {
    if (expression._tag === 'Grouped') return directSite(expression.expression)
    if (expression._tag === 'Move') return directSite(expression.subject)
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

  const movedExecutableBindings = (
    expression: Elaboration.ExpressionFact,
  ): ReadonlyArray<number> => {
    if (expression._tag === 'Grouped') return movedExecutableBindings(expression.expression)
    if (expression._tag === 'Move') {
      const site = directSite(expression.subject)?.site
      return site?._tag === 'Let' &&
        expression.subject.type._tag === 'Available' &&
        (Type.isEffect(expression.subject.type.type) ||
          Type.containsExecutableRepresentation(expression.subject.type.type))
        ? Object.freeze([site.binding.ordinal])
        : movedExecutableBindings(expression.subject)
    }
    if (expression._tag === 'StructLiteral')
      return Object.freeze(
        expression.initializers.flatMap((initializer) =>
          movedExecutableBindings(initializer.expression),
        ),
      )
    if (expression._tag === 'ArrayLiteral')
      return Object.freeze(
        expression.elements.flatMap((element) => movedExecutableBindings(element.expression)),
      )
    if (expression._tag === 'EffectResult') return movedExecutableBindings(expression.protected)
    if (expression._tag === 'EffectCatch')
      return Object.freeze([
        ...movedExecutableBindings(expression.protected),
        ...movedExecutableBindings(expression.handler),
      ])
    if (expression._tag === 'Call')
      return Object.freeze(
        expression.arguments.flatMap((argument) => movedExecutableBindings(argument.expression)),
      )
    if (expression._tag === 'CallableApply')
      return Object.freeze([
        ...movedExecutableBindings(expression.callee),
        ...expression.arguments.flatMap((argument) => movedExecutableBindings(argument.expression)),
      ])
    return Object.freeze([])
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
        const bindings = [
          ...(site?._tag === 'Let' ? [site.binding.ordinal] : []),
          ...movedExecutableBindings(expression.subject),
        ]
        for (const binding of new Set(bindings)) {
          const previous = runEnds.get(binding)
          if (previous === undefined || previous.span.end < expression.syntax.span.end) {
            runEnds.set(binding, { region, span: expression.syntax.span })
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
        {
          const site = directSite(expression.callee)?.site
          if (site?._tag === 'Let') {
            const previous = callableEnds.get(site.binding.ordinal)
            if (previous === undefined || previous.span.end <= expression.syntax.span.end) {
              callableEnds.set(site.binding.ordinal, {
                region,
                span: expression.syntax.span,
              })
            }
          }
        }
        return
      case 'CallableSection':
        for (const capture of expression.captures) scanRunEnds(capture.expression, region)
        return
      case 'EffectCatch':
        scanRunEnds(expression.protected, region)
        scanRunEnds(expression.handler, region)
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
          Type.isCallable(expression.type.type)
        ) {
          // A later non-invocation use may store or escape the callable. The CallableApply case
          // records the same occurrence again after visiting its callee, so only a last known
          // invocation (or explicit drop) shortens the capture loan.
          callableEnds.delete(site.binding.ordinal)
        }
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
        case 'PatternBindStatement':
          scanRunEnds(statement.selection.source, statement.region)
          break
        case 'ExpressionStatement':
          scanRunEnds(statement.expression, statement.region)
          break
        case 'IfStatement':
          scanRunEnds(statement.condition, statement.region)
          scanStatementRunEnds(statement.taken)
          scanStatementRunEnds(statement.otherwise)
          break
        case 'IfLetStatement':
          scanRunEnds(statement.selection.source, statement.region)
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

  const executableAliases = new Map<number, Set<number>>()
  for (const binding of fn.bindings) {
    const directAlias = directSite(binding.initializer)?.site
    const callableAlias =
      directAlias?._tag === 'Let' &&
      binding.inferredType._tag === 'Available' &&
      Type.isCallable(binding.inferredType.type)
        ? [directAlias.binding.ordinal]
        : []
    for (const source of new Set([
      ...movedExecutableBindings(binding.initializer),
      ...callableAlias,
    ])) {
      const destinations = executableAliases.get(source)
      if (destinations === undefined) executableAliases.set(source, new Set([binding.id.ordinal]))
      else destinations.add(binding.id.ordinal)
    }
  }
  let propagatedExecutableEnd = true
  while (propagatedExecutableEnd) {
    propagatedExecutableEnd = false
    for (const [source, destinations] of executableAliases) {
      for (const destination of destinations) {
        const runEnding = runEnds.get(destination)
        const previousRunEnding = runEnds.get(source)
        if (
          runEnding !== undefined &&
          (previousRunEnding === undefined || previousRunEnding.span.end < runEnding.span.end)
        ) {
          runEnds.set(source, runEnding)
          propagatedExecutableEnd = true
        }
        const ending = callableEnds.get(destination)
        const previousEnding = callableEnds.get(source)
        if (
          ending === undefined ||
          (previousEnding !== undefined && previousEnding.span.end >= ending.span.end)
        )
          continue
        callableEnds.set(source, ending)
        propagatedExecutableEnd = true
      }
    }
  }

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
    const directBorrow =
      binding.initializer._tag === 'Borrow' && binding.initializer.formation._tag !== 'Unavailable'
    if (
      binding.inferredType._tag !== 'Available' ||
      !Type.containsViewBorrow(binding.inferredType.type) ||
      (!directBorrow && returnedArgumentOrdinal(binding.initializer) === undefined)
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
    expression.type._tag === 'Available' &&
    categoryOf(index, expression.type.type, copyAssumptions)._tag === 'MoveOnly'
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
      case 'Borrow': {
        if (expression.formation._tag === 'Unavailable') return
        const directRoot = borrowSite(expression.formation.root)
        const root =
          directRoot._tag === 'Let'
            ? (viewRoots.get(directRoot.binding.ordinal) ?? directRoot)
            : directRoot
        const extended = [...active, ...delayedLoansAt(expression.syntax.span)]
        const conflict = extended.find(
          (loan) =>
            sameSite(loan.root, root) &&
            (loan.access === 'Exclusive' || expression.access === 'Exclusive'),
        )
        if (conflict !== undefined) {
          diagnostics.push(
            Diagnostic.conflictingSliceLoan(
              conflict.access,
              expression.access,
              conflict.startSpan,
              expression.syntax.span,
            ),
          )
          return
        }
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
            access: expression.access,
            origin:
              expression.formation._tag === 'FixedArrayBorrow'
                ? 'FixedArrayBorrow'
                : expression.formation._tag === 'SliceReborrow'
                  ? 'SliceReborrow'
                  : 'ValueBorrow',
            suspendsParent:
              expression.formation._tag === 'SliceReborrow' && expression.formation.suspendsParent,
            startRegion: region,
            endRegion: delayedEnd?.region ?? region,
            startSpan: expression.syntax.span,
            endSpan: delayedEnd?.span ?? expression.syntax.span,
          }),
        )
        return
      }
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
      // A value stored in an aggregate outlives the expression that built it, so a delayed end the
      // enclosing binding carries reaches the captures stored inside it too. Without this, a
      // borrow captured by a stored callable would be released while the aggregate still holds it.
      case 'StructLiteral':
        for (const initializer of expression.initializers) {
          inspect(
            initializer.expression,
            region,
            active,
            naturalAccess(initializer.expression),
            delayedEnd,
          )
        }
        return
      case 'ArrayLiteral':
        for (const element of expression.elements) {
          inspect(element.expression, region, active, naturalAccess(element.expression), delayedEnd)
        }
        return
      case 'Match':
        inspect(expression.scrutinee, region, active, naturalAccess(expression.scrutinee))
        for (const arm of expression.arms) {
          if (arm.guard !== undefined) inspect(arm.guard, region, active, 'Read')
          inspect(arm.result, region, active, access)
        }
        return
      case 'Operator': {
        const callActive: Array<LoanFact> = [...active]
        for (const [ordinal, argument] of expression.arguments.entries()) {
          const candidate = argument.expression
          const operand = expression.interfaceOperation?.contract.operands.at(ordinal)
          const operandType = operand?.type._tag === 'Resolved' ? operand.type.type : undefined
          if (
            operandType === undefined ||
            (!Type.isReference(operandType) && !Type.isSlice(operandType))
          ) {
            inspect(candidate, region, callActive, naturalAccess(candidate))
            continue
          }
          const direct = directSite(candidate)
          if (direct === undefined) {
            inspect(
              candidate,
              region,
              callActive,
              operandType.access === 'Exclusive' ? 'Write' : 'Read',
            )
            continue
          }
          const root =
            direct.site._tag === 'Let'
              ? (viewRoots.get(direct.site.binding.ordinal) ?? direct.site)
              : direct.site
          const conflict = callActive.find(
            (loan) =>
              sameSite(loan.root, root) &&
              (loan.access === 'Exclusive' || operandType.access === 'Exclusive'),
          )
          if (conflict !== undefined)
            diagnostics.push(
              Diagnostic.conflictingSliceLoan(
                conflict.access,
                operandType.access,
                conflict.startSpan,
                candidate.syntax.span,
              ),
            )
          const loan: LoanFact = Object.freeze({
            _tag: 'Loan',
            id: Object.freeze({
              _tag: 'BorrowId',
              function: fn.declaration.id,
              callSpan: expression.syntax.span,
              ordinal,
            }),
            root,
            access: operandType.access,
            origin: 'InterfaceOperand',
            suspendsParent: false,
            startRegion: region,
            endRegion: region,
            startSpan: candidate.syntax.span,
            endSpan: expression.syntax.span,
          })
          loans.push(loan)
          callActive.push(loan)
        }
        return
      }
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
          const directRoot =
            candidate._tag === 'Borrow' && candidate.formation._tag !== 'Unavailable'
              ? borrowSite(candidate.formation.root)
              : directSite(candidate)?.site
          const root =
            directRoot?._tag === 'Let'
              ? (viewRoots.get(directRoot.binding.ordinal) ?? directRoot)
              : directRoot
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
        if (
          provider === undefined ||
          provider.captureAccess === 'Copy' ||
          provider.captureAccess === 'Take'
        )
          return
        const root: BindingSite =
          provider.reference._tag === 'BindingFact'
            ? Object.freeze({ _tag: 'Let', binding: provider.reference.id })
            : Object.freeze({ _tag: 'Parameter', parameter: provider.reference.id })
        const conflict = active.find(
          (loan) =>
            sameSite(loan.root, root) &&
            (loan.access === 'Exclusive' || provider.captureAccess === 'Exclusive'),
        )
        if (conflict !== undefined)
          diagnostics.push(
            Diagnostic.conflictingSliceLoan(
              conflict.access,
              provider.captureAccess,
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
            access: provider.captureAccess,
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
      case 'EffectCatch':
        // Catch retains both operands until the resulting Effect runs, just like an ordinary call
        // returning an Effect. Propagate the delayed end so nested borrowed captures remain live.
        inspect(
          expression.protected,
          region,
          active,
          naturalAccess(expression.protected),
          delayedEnd,
        )
        inspect(expression.handler, region, active, naturalAccess(expression.handler), delayedEnd)
        return
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
              ? (runEnds.get(statement.binding.id.ordinal) ??
                  callableEnds.get(statement.binding.id.ordinal) ?? {
                    region: statement.region,
                    span: fn.declaration.syntax.span,
                  })
              : // A binding that stores an executable holds its captured borrows for as long as it
                // holds that environment, whether it is the binding's own value or sits in a field
                // of the aggregate it names.
                initializerType._tag === 'Available' &&
                  (Type.isCallable(initializerType.type) ||
                    Type.containsExecutableRepresentation(initializerType.type))
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
        case 'PatternBindStatement':
          inspect(
            statement.selection.source,
            statement.region,
            [],
            naturalAccess(statement.selection.source),
            { region: statement.region, span: statement.selection.loanEnd },
          )
          break
        case 'IfStatement':
          inspect(statement.condition, statement.region, [])
          statements(statement.taken)
          statements(statement.otherwise)
          break
        case 'IfLetStatement':
          inspect(
            statement.selection.source,
            statement.region,
            [],
            naturalAccess(statement.selection.source),
            { region: statement.region, span: statement.selection.loanEnd },
          )
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

const borrowedReplacements = (
  fn: Elaboration.FunctionFact,
  index: DeclarationIndex.Index,
): ReadonlyArray<BorrowedReplacementFact> => {
  const replacements: Array<BorrowedReplacementFact> = []
  const walk = (statements: ReadonlyArray<Elaboration.StatementFact>): void => {
    for (const statement of statements) {
      if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
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
          displacedCleanup: CleanupPlan.cleanupPlan(index, statement.destination.type.type),
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
  const copyAssumptions = new Set(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.bounds.some(
        (bound) =>
          bound._tag === 'ResolvedBound' &&
          Type.equals(bound.application.capability, Type.copyCapability),
      )
        ? [Type.key(parameter.type)]
        : [],
    ),
  )
  const loanAnalysis =
    semantic === undefined
      ? Object.freeze({ loans: Object.freeze([]), diagnostics: Object.freeze([]) })
      : analyzeLoans(semantic, index, copyAssumptions)
  const replacements =
    semantic === undefined ? Object.freeze([]) : borrowedReplacements(semantic, index)
  const state: CheckState = {
    index,
    copyAssumptions,
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
    const cause = 'cause' in parameter.declaredType ? parameter.declaredType.cause : undefined
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
      category: categoryOf(index, type, copyAssumptions),
      executionAffinity: ExecutionAffinity.ofDeclaredType(index, parameter.declaredType),
      localSharedObligations: LocalSharedOwnership.ofDeclaredType(index, parameter.declaredType),
      ...(type === undefined ? {} : { type }),
      ...(cause === undefined ? {} : { cause }),
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
  const sameLive = setEqual
  const intersection = (states: ReadonlyArray<ReadonlySet<string>>): Set<string> => {
    const [first, ...rest] = states
    return new Set([...(first ?? [])].filter((site) => rest.every((state) => state.has(site))))
  }
  const frameSitesInnerFirst = (
    frames: ReadonlyArray<ReadonlyArray<string>>,
    live: ReadonlySet<string>,
  ): ReadonlyArray<string> =>
    [...frames].reverse().flatMap((frame) => [...frame].reverse().filter((site) => live.has(site)))

  const checkPatternSubject = (selection: Hir.PatternSelection, live: Set<string>): void => {
    const subjectType =
      selection.subject._tag === 'Unavailable' ? undefined : selection.subject.type
    const subjectSite = placeSite(selection.subject)
    const subjectBinding =
      subjectSite === undefined ? undefined : state.bindings.get(siteKey(subjectSite))
    if (selection.access === 'Copy') {
      checkExpression(state, live, selection.subject, false)
      if (categoryOf(index, subjectType, copyAssumptions)._tag === 'MoveOnly')
        state.diagnostics.push(
          Diagnostic.explicitMoveRequired(subjectBinding?.name ?? '?', selection.span),
        )
      return
    }
    if (selection.access === 'Move') {
      if (subjectSite === undefined) checkExpression(state, live, selection.subject, true)
      else checkUse(state, live, subjectSite, selection.span, true)
      return
    }
    checkExpression(state, live, selection.subject, false)
    if (selection.access === 'Exclusive') {
      if (subjectSite === undefined)
        state.diagnostics.push(Diagnostic.invalidMatchScrutineePlace('Exclusive', selection.span))
      else if (subjectBinding?.mutability !== 'Mutable')
        state.diagnostics.push(
          Diagnostic.exclusiveMatchRequiresMutable(subjectBinding?.name ?? '?', selection.span),
        )
    }
  }

  const introducePatternBindings = (
    selection: Hir.PatternSelection,
    live: Set<string>,
    frame: Array<string>,
    liveTo: SourceSpan.SourceSpan,
  ): ReadonlyArray<BindingSite> => {
    const sites: Array<BindingSite> = []
    for (const pattern of selection.bindings) {
      const site: BindingSite = Object.freeze({ _tag: 'Pattern', binding: pattern.id })
      const mutable: MutableBinding = {
        site,
        name: pattern.name,
        mutability: pattern.access === 'Exclusive' ? 'Mutable' : 'Immutable',
        liveFrom: pattern.span,
        liveTo,
        category: categoryOf(index, pattern.type, copyAssumptions),
        type: pattern.type,
        matchAccess: pattern.access,
      }
      const key = siteKey(site)
      state.bindings.set(key, mutable)
      state.order.push(mutable)
      frame.push(key)
      live.add(key)
      sites.push(site)
    }
    return Object.freeze(sites)
  }

  const patternSelectionCleanup = (
    selection: Hir.PatternSelection,
    live: ReadonlySet<string>,
    includeBindings: boolean,
  ): MatchOwnership['arms'][number]['cleanup'] =>
    selection.access === 'Move'
      ? Object.freeze([
          ...selection.cleanup.flatMap((path) => {
            const subjectType =
              selection.subject._tag === 'Unavailable' ? undefined : selection.subject.type
            const type = CleanupPlan.cleanupTypeAtPath(
              index,
              selection.member ?? subjectType ?? 'never',
              path,
            )
            return type === undefined
              ? []
              : [Object.freeze({ path, cleanup: CleanupPlan.cleanupPlan(index, type) })]
          }),
          ...(includeBindings
            ? selection.bindings.flatMap((binding) => {
                const site: BindingSite = Object.freeze({ _tag: 'Pattern', binding: binding.id })
                return live.has(siteKey(site)) &&
                  categoryOf(index, binding.type, copyAssumptions)._tag === 'MoveOnly'
                  ? [
                      Object.freeze({
                        path: binding.path,
                        cleanup: CleanupPlan.cleanupPlan(index, binding.type),
                      }),
                    ]
                  : []
              })
            : []),
        ])
      : Object.freeze([])

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
        const retained =
          environment === undefined
            ? executableEnvironment(state, statement.initializer)
            : Object.freeze({
                affinity: environment.executionAffinity,
                obligations: environment.localSharedObligations,
              })
        const cause =
          statement.initializer._tag === 'Unavailable' ? statement.initializer.cause : undefined
        const binding: MutableBinding = {
          site: Object.freeze({ _tag: 'Let', binding: statement.binding }),
          name: statement.name,
          mutability: statement.mutability,
          liveFrom: statement.span,
          liveTo: enclosingSpan,
          category: categoryOf(index, type, copyAssumptions),
          ...(retained === undefined
            ? {}
            : {
                executionAffinity: retained.affinity,
                localSharedObligations: retained.obligations,
              }),
          ...(type === undefined ? {} : { type }),
          ...(cause === undefined ? {} : { cause }),
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
      if (statement._tag === 'PatternBind') {
        checkPatternSubject(statement.selection, live)
        const frame = frames.at(-1) ?? []
        const sites = introducePatternBindings(statement.selection, live, frame, enclosingSpan)
        state.matches.push(
          Object.freeze({
            _tag: 'MatchOwnership',
            id: statement.selection.id,
            access: statement.selection.access,
            span: statement.span,
            arms: Object.freeze([
              Object.freeze({
                id: statement.selection.arm,
                ...(statement.selection.member === undefined
                  ? {}
                  : { member: statement.selection.member }),
                universal: statement.selection.universal,
                provisionalGuard: false,
                bindings: sites,
                cleanup: patternSelectionCleanup(statement.selection, live, false),
              }),
            ]),
          }),
        )
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
      if (statement._tag === 'IfLet') {
        checkPatternSubject(statement.selection, live)
        const continuing: Array<Set<string>> = []
        let selectedSites: ReadonlyArray<BindingSite> = Object.freeze([])
        for (const [arm, body] of [
          ['Taken', statement.taken],
          ['Otherwise', statement.otherwise],
        ] as const) {
          const armFrames = [...frames.map((frame) => [...frame]), []]
          const armLive = new Set(live)
          if (arm === 'Taken')
            selectedSites = introducePatternBindings(
              statement.selection,
              armLive,
              armFrames.at(-1) ?? [],
              statement.span,
            )
          const result = walkStatements(body, statement.span, armLive, armFrames, loopScopes)
          const frame = armFrames.at(-1) ?? []
          if (!result.returned && frame.length > 0)
            exits.push(
              Object.freeze({
                kind: 'ArmEnd' as const,
                span: statement.span,
                region: statement.region,
                arm,
                sites: Object.freeze([...frame].reverse().filter((site) => result.live.has(site))),
              }),
            )
          if (!result.returned) {
            for (const site of frame) result.live.delete(site)
            continuing.push(result.live)
          }
        }
        state.matches.push(
          Object.freeze({
            _tag: 'MatchOwnership',
            id: statement.selection.id,
            access: statement.selection.access,
            span: statement.span,
            arms: Object.freeze([
              Object.freeze({
                id: statement.selection.arm,
                ...(statement.selection.member === undefined
                  ? {}
                  : { member: statement.selection.member }),
                universal: statement.selection.universal,
                provisionalGuard: false,
                bindings: selectedSites,
                cleanup: patternSelectionCleanup(statement.selection, live, false),
              }),
            ]),
          }),
        )
        if (continuing.length === 0) return Object.freeze({ returned: true, live })
        live = intersection(continuing)
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
      executionAffinity:
        binding.executionAffinity ??
        (binding.type === undefined
          ? ExecutionAffinity.ofEnvironment(index, [
              Object.freeze(binding.cause === undefined ? {} : { cause: binding.cause }),
            ])
          : ExecutionAffinity.ofType(index, binding.type)),
      localSharedObligations:
        binding.localSharedObligations ??
        (binding.type === undefined
          ? LocalSharedOwnership.ofEnvironment(index, [
              Object.freeze(
                binding.cause === undefined
                  ? { access: 'Take' as const }
                  : { access: 'Take' as const, cause: binding.cause },
              ),
            ])
          : LocalSharedOwnership.ofType(index, binding.type)),
      ...(binding.type === undefined ? {} : { type: binding.type }),
      cleanup:
        binding.cleanup ??
        (binding.type === undefined
          ? Object.freeze({ _tag: 'NoCleanup' as const, type: 'i32' as const })
          : CleanupPlan.cleanupPlan(index, binding.type)),
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
                      ? CleanupPlan.cleanupFields(index, fact.category.type)
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
