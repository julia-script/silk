import type { ExitIndex } from './CleanupEmission.js'
import {
  authored,
  cleanupForLocal,
  concreteCleanup,
  delayedLoopLoans,
  effectContract,
  emitReleases,
  generated,
  lowerBorrowedWriteSelectors,
  lowerWriteSelectors,
  ownerFields,
  specializedCleanup,
  terminalLoopLoanEndings,
  withDelayedFailureLoanEndings,
  withoutLoanEndings,
} from './CleanupEmission.js'
import type { LoweredExpression } from './EffectLowering.js'
import {
  borrowedWriteRoot,
  endReturnedViewLoans,
  ownedWriteRoot,
  retainedEffectLoans,
} from './EffectLowering.js'
import type {} from './EntryAssembly.js'
import type {} from './Forwarding.js'
import {
  callableRecipe,
  delayedEffectState,
  effectRecipe,
  inlineForwardedRequirement,
  movedEffectRecipe,
  restoreDelayedEffectState,
} from './Forwarding.js'
import type { FunctionLowering } from './FunctionLowering.js'
import type * as Hir from './Hir.js'
import * as Layout from './Layout.js'
import type { DelayedEffectState } from './Lower.js'
import { borrowKey, i32, patternKey, spanKey } from './Lower.js'
import type {} from './LowerExpression.js'
import { lowerExpression } from './LowerExpression.js'
import * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as Ownership from './Ownership.js'
import * as Type from './Type.js'
import { effectValueByIdentity, instanceText } from './ValueType.js'

export interface LoweredPatternSelection {
  readonly result: Mir.LocalId
  readonly bindings: ReadonlyArray<Match.BindingId>
}

export const lowerPatternSelection = (
  fn: FunctionLowering,
  selection: Hir.PatternSelection,
  result: 'Unit' | 'Bool',
): LoweredPatternSelection | undefined => {
  if (selection.subject._tag === 'Unavailable') return undefined
  const subject = lowerExpression(fn, selection.subject)
  const semanticSubject = fn.semantic(selection.subject.type)
  const subjectType = fn.type(selection.subject.type)
  // Statement selections need only one compiler-private branch bit. Keeping that result as bool
  // avoids manufacturing a source-visible unit value and gives both statement forms one lowering.
  const resultSemantic = 'bool' as const
  const resultType = fn.type(resultSemantic)
  if (subject === undefined || subjectType === undefined || resultType === undefined)
    return undefined
  if (selection.members.some((member) => member._tag === 'EnumMember')) return undefined
  const specializeMember = (candidate: Match.CoverageIdentity): Match.CoverageIdentity => {
    if (candidate._tag === 'StructuralTypeMember')
      return Match.structuralMember(fn.semantic(candidate.type))
    if (candidate._tag !== 'NominalUnionVariant') return candidate
    const type = fn.semantic(candidate.type)
    return Type.isNominal(type)
      ? Match.nominalUnionVariant(
          fn.semantic(candidate.root),
          type,
          candidate.variant,
          candidate.variantOrdinal,
        )
      : candidate
  }
  const members = Object.freeze(selection.members.map(specializeMember))
  const member = selection.member === undefined ? undefined : specializeMember(selection.member)
  const literal = (value: boolean): LoweredExpression | undefined =>
    lowerExpression(
      fn,
      Object.freeze({
        _tag: 'BooleanLiteral' as const,
        value,
        type: 'bool' as const,
        span: selection.span,
      }),
    )
  const bindingIds = Object.freeze(selection.bindings.map((binding) => binding.id))

  if (result === 'Unit' && !selection.irrefutable) return undefined
  const subjectShape = Layout.callingShape(fn.layout, semanticSubject)
  const resultShape = Layout.callingShape(fn.layout, resultSemantic)
  if (subjectShape === undefined || resultShape === undefined) return undefined
  const ownership = fn.ownership?.matches.find(
    (candidate) =>
      candidate.id.span.start === selection.id.span.start &&
      candidate.id.span.end === selection.id.span.end,
  )
  const selectedBindings: Array<Mir.MatchBinding> = []
  for (const binding of selection.bindings) {
    const type = fn.type(binding.type)
    if (type === undefined) return undefined
    const destination = fn.alloc(type)
    fn.patternLocals.set(patternKey(binding.id), destination)
    selectedBindings.push(
      Object.freeze({
        id: binding.id,
        destination,
        path: binding.path,
        type,
        access: binding.access,
        provenance: authored(binding.span),
      }),
    )
  }
  const [selectedResult, selectedOperations] = fn.capture(() => literal(true))
  if (selectedResult === undefined) return undefined
  const emptyCoverage: ReadonlyArray<Match.CoverageIdentity> = Object.freeze([])
  const selectedAfter = selection.universal
    ? emptyCoverage
    : Object.freeze(
        members.filter((candidate) => member === undefined || !Match.selects(member, candidate)),
      )
  const ownedArm = ownership?.arms.find(
    (candidate) => candidate.id.ordinal === selection.arm.ordinal,
  )
  const finalizedSelectedOperations = [...selectedOperations]
  const cleanup: Array<Mir.MatchArm['selected']['cleanup'][number]> = []
  for (const release of ownedArm?.cleanup ?? []) {
    const plan = specializedCleanup(fn, release.cleanup)
    if (plan._tag === 'NoCleanup') continue
    if (release.path.length === 0 && Type.equals(plan.type, semanticSubject)) {
      finalizedSelectedOperations.push(
        Object.freeze({
          _tag: 'Drop',
          local: subject.result,
          cleanup: plan,
          provenance: authored(selection.span),
        }),
      )
      continue
    }
    const type = fn.type(plan.type)
    if (type === undefined) return undefined
    cleanup.push(
      Object.freeze({
        destination: fn.alloc(type),
        path: release.path,
        cleanup: plan,
      }),
    )
  }
  const selectedArm: Mir.MatchArm = Object.freeze({
    id: selection.arm,
    ...(member === undefined ? {} : { member }),
    universal: selection.universal,
    before: members,
    after: selectedAfter,
    bindings: Object.freeze(selectedBindings),
    selected: Object.freeze({
      access: selection.access,
      operations: Object.freeze(finalizedSelectedOperations),
      result: selectedResult.result,
      cleanup: Object.freeze(cleanup),
      endBorrow: false,
    }),
    provenance: authored(selection.span),
  })
  const fallbackId: Match.ArmId = Object.freeze({
    _tag: 'MatchArmId',
    match: selection.id,
    ordinal: 1,
  })
  const [fallbackResult, fallbackOperations] = fn.capture(() => literal(false))
  if (fallbackResult === undefined) return undefined
  const fallbackArm: Mir.MatchArm = Object.freeze({
    id: fallbackId,
    universal: true,
    before: selectedAfter,
    after: Object.freeze([]),
    bindings: Object.freeze([]),
    selected: Object.freeze({
      access: selection.access,
      operations: fallbackOperations,
      result: fallbackResult.result,
      cleanup: Object.freeze([]),
      endBorrow: false,
    }),
    provenance: authored(selection.span),
  })
  const needsFallback = result !== 'Unit' && !selection.universal && !selection.irrefutable
  const arms = needsFallback
    ? Object.freeze([selectedArm, fallbackArm])
    : Object.freeze([selectedArm])
  const destination = fn.alloc(resultType)
  fn.emit(
    Object.freeze({
      _tag: 'Match' as const,
      id: selection.id,
      destination,
      scrutinee: subject.result,
      scrutineeType: subjectType,
      scrutineeShape: subjectShape,
      access: selection.access,
      retainsBindings: true,
      members,
      decisions: Object.freeze(
        members.map((candidate) => {
          let candidates: ReadonlyArray<Match.ArmId>
          if (selection.universal) {
            candidates = [selection.arm]
          } else if (member === undefined || !Match.selects(member, candidate)) {
            candidates = [fallbackId]
          } else {
            candidates = needsFallback ? [selection.arm, fallbackId] : [selection.arm]
          }
          return Object.freeze({ member: candidate, candidates: Object.freeze(candidates) })
        }),
      ),
      arms,
      type: resultType,
      resultShape,
      provenance: authored(selection.span),
    }),
  )
  return Object.freeze({ result: destination, bindings: bindingIds })
}

export const lowerSequence = (
  fn: FunctionLowering,
  statements: ReadonlyArray<Hir.Statement>,
  exits: ExitIndex,
  ownerLoop: Mir.LoopId | undefined,
  terminal: Mir.Outcome,
  reserved?: Mir.RegionId,
  armExit?: Ownership.ExitPlan,
): Mir.RegionId | undefined => {
  const id = reserved ?? fn.reserve()
  const [statement, ...rest] = statements
  if (statement === undefined) {
    const [, releases] = fn.capture(() => emitReleases(fn, armExit))
    if (releases.length > 0) {
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome: terminal,
        }),
      )
    } else {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: terminal,
        }),
      )
    }
    return id
  }

  if (statement._tag === 'UnavailableStatement') {
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations: Object.freeze([]),
        outcome: Object.freeze({
          _tag: 'Trap',
          reason: 'unavailable statement',
          provenance: generated(statement.span),
        }),
      }),
    )
    return id
  }

  if (statement._tag === 'Bind') {
    const initializerType =
      'type' in statement.initializer ? fn.semantic(statement.initializer.type) : undefined
    const transferredEffect = movedEffectRecipe(fn, statement.initializer)
    if (
      transferredEffect !== undefined &&
      initializerType !== undefined &&
      Type.isEffect(initializerType)
    ) {
      fn.effectRecipes.delete(transferredEffect.source)
      fn.effectLoanEnds.delete(transferredEffect.source)
      fn.effectRecipes.set(statement.binding.ordinal, transferredEffect.recipe)
      if (transferredEffect.loanEnds.length > 0)
        fn.effectLoanEnds.set(statement.binding.ordinal, transferredEffect.loanEnds)
      const following = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: following,
            provenance: generated(statement.span),
          }),
        }),
      )
      return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
        ? undefined
        : id
    }
    const staticCallable = callableRecipe(fn, statement.initializer)
    const staticCallableType =
      staticCallable === undefined ? undefined : fn.semantic(staticCallable.type)
    let callableSchema: Type.CallableSchema | undefined
    if (staticCallableType !== undefined && Type.isCallable(staticCallableType)) {
      callableSchema = staticCallableType.schema
    } else if (initializerType !== undefined && Type.isCallable(initializerType)) {
      callableSchema = initializerType.schema
    }
    if (
      staticCallable !== undefined &&
      callableSchema !== undefined &&
      (callableSchema.binders.length > 0 ||
        callableSchema.constraints.length > 0 ||
        callableSchema.evidence.length > 0)
    ) {
      fn.callableRecipes.set(statement.binding.ordinal, statement.initializer)
      const following = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: following,
            provenance: generated(statement.span),
          }),
        }),
      )
      return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
        ? undefined
        : id
    }
    const forwardedRequirement = inlineForwardedRequirement(fn, statement.initializer)
    const forwardedResultEffect =
      forwardedRequirement === undefined
        ? undefined
        : fn.call(
            statement.initializer.span,
            undefined,
            statement.initializer._tag === 'EffectConstruct'
              ? statement.initializer.typeArguments.map((argument) => fn.semanticArgument(argument))
              : undefined,
            statement.initializer._tag === 'EffectConstruct'
              ? statement.initializer.staticArguments
              : undefined,
          )?.resultEffect
    const protectedRecipe =
      forwardedRequirement === undefined
        ? undefined
        : effectRecipe(fn, forwardedRequirement.binding.protected)
    const forwardedRequirementNeedsRecipe =
      forwardedRequirement !== undefined &&
      (forwardedResultEffect === undefined ||
        effectValueByIdentity(fn.layout, forwardedResultEffect) === undefined ||
        protectedRecipe?._tag === 'ServiceEffectConstruct' ||
        protectedRecipe !== forwardedRequirement.binding.protected)
    if (
      forwardedRequirementNeedsRecipe ||
      statement.initializer._tag === 'ServiceEffectConstruct' ||
      (statement.initializer._tag === 'EffectConstruct' &&
        fn.call(
          statement.initializer.span,
          undefined,
          statement.initializer.typeArguments.map((argument) => fn.semanticArgument(argument)),
          statement.initializer.staticArguments,
        )?.resultEffect === undefined &&
        fn.effectResults.get(
          instanceText(
            statement.initializer.target,
            statement.initializer.typeArguments.map((argument) => fn.semanticArgument(argument)),
            fn.call(
              statement.initializer.span,
              undefined,
              statement.initializer.typeArguments.map((argument) => fn.semanticArgument(argument)),
              statement.initializer.staticArguments,
            )?.target.staticArguments ?? statement.initializer.staticArguments,
          ),
        ) === undefined) ||
      statement.initializer._tag === 'EffectBindRequirement' ||
      (statement.initializer._tag === 'Match' &&
        effectContract(initializerType ?? 'never') !== undefined) ||
      (statement.initializer._tag === 'BuiltinCall' && Type.isEffect(statement.initializer.type))
    ) {
      fn.effectRecipes.set(statement.binding.ordinal, statement.initializer)
      const following = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: following,
            provenance: generated(statement.span),
          }),
        }),
      )
      return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
        ? undefined
        : id
    }
    const [initializer, operations] = fn.capture(() => {
      const lowered = lowerExpression(fn, statement.initializer)
      if (lowered === undefined) return undefined
      const destination = fn.alloc(fn.localTypes.at(lowered.result.ordinal) ?? i32)
      fn.emit(
        Object.freeze({
          _tag: 'Move',
          destination,
          source: lowered.result,
          provenance: authored(statement.span),
        }),
      )
      const heldLoans = fn.slotLoans.get(lowered.result.ordinal)
      if (heldLoans !== undefined) {
        fn.slotLoans.delete(lowered.result.ordinal)
        fn.slotLoans.set(destination.ordinal, heldLoans)
      }
      fn.bindingLocals.set(statement.binding.ordinal, destination)
      const destinationType = fn.localTypes.at(destination.ordinal)
      if (destinationType?._tag === 'EffectValue') {
        const retained = retainedEffectLoans(fn, statement.initializer)
        if (retained.length > 0) fn.effectLoanEnds.set(statement.binding.ordinal, retained)
      }
      return destination
    })
    if (initializer === undefined) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'PatternBind') {
    const [selection, operations] = fn.capture(() =>
      lowerPatternSelection(fn, statement.selection, 'Unit'),
    )
    if (selection === undefined) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Evaluate') {
    const [evaluated, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (evaluated === undefined) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Write') {
    const place = statement.place
    const root =
      place._tag === 'BorrowedWritePlace'
        ? borrowedWriteRoot(fn, place.root)
        : ownedWriteRoot(fn, place.root)
    const rootType = root === undefined ? undefined : fn.localTypes.at(root.ordinal)
    // A whole callable binding keeps its exact representation; the written value carries the same
    // identity once analysis admitted the write.
    const type =
      fn.type(place.type) ??
      (rootType?._tag === 'CallableValue' && place.selectors.length === 0 ? rootType : undefined)
    const [written, operations] = fn.capture(() => {
      if (root === undefined || rootType === undefined || type === undefined) return false
      const selectors =
        place._tag === 'BorrowedWritePlace'
          ? lowerBorrowedWriteSelectors(fn, place.selectors)
          : lowerWriteSelectors(fn, place.selectors)
      if (selectors === undefined) return false
      fn.emit(
        Object.freeze({
          _tag: 'CheckPlace',
          root,
          selectors,
          type,
          provenance: authored(place.span),
        }),
      )
      const value = lowerExpression(fn, statement.value)
      if (value === undefined) return false
      fn.emit(
        Object.freeze({
          _tag: 'WritePlace',
          root,
          selectors,
          source: value.result,
          rootType,
          type,
          mutable: true,
          replacement: Mir.isCopy(fn.layout, fn.semantic(statement.place.type)) ? 'Copy' : 'Owned',
          commit: 'AfterCleanup',
          provenance: authored(statement.span),
        }),
      )
      endReturnedViewLoans(fn, statement.span)
      return true
    })
    if (!written) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Drop') {
    const droppedExpression =
      statement.expression._tag === 'Move' ? statement.expression.subject : statement.expression
    const droppedRecipe = callableRecipe(fn, droppedExpression)
    const droppedRecipeType =
      droppedRecipe === undefined ? undefined : fn.semantic(droppedRecipe.type)
    if (
      droppedRecipeType !== undefined &&
      Type.isCallable(droppedRecipeType) &&
      droppedRecipeType.schema !== undefined
    ) {
      if (droppedExpression._tag === 'BindingReference')
        fn.callableRecipes.delete(droppedExpression.binding.ordinal)
      const following = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: following,
            provenance: generated(statement.span),
          }),
        }),
      )
      return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
        ? undefined
        : id
    }
    const [lowered, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (lowered === undefined) return undefined
    const localType = fn.localTypes.at(lowered.result.ordinal)
    if (localType === undefined) return undefined
    const droppedBinding =
      droppedExpression._tag === 'BindingReference' ? droppedExpression.binding.ordinal : undefined
    const bindingFact =
      droppedBinding !== undefined
        ? Ownership.allBindings(fn.ownership).find(
            (binding) =>
              binding.site._tag === 'Let' && binding.site.binding.ordinal === droppedBinding,
          )
        : undefined
    const ownershipLoanReleases = (fn.ownership?.loans ?? []).flatMap((loan) => {
      if (loan.endSpan.start !== statement.span.start || loan.endSpan.end !== statement.span.end) {
        return []
      }
      const slice = fn.loanLocals.get(borrowKey(loan.id))
      if (slice === undefined) return []
      fn.loanLocals.delete(borrowKey(loan.id))
      return [
        Object.freeze({
          _tag: 'EndLoan' as const,
          borrow: loan.id,
          slice,
          provenance: generated(statement.span),
        }),
      ]
    })
    const retainedLoanReleases = (
      droppedBinding === undefined ? [] : (fn.effectLoanEnds.get(droppedBinding) ?? [])
    ).flatMap((borrow) => {
      const slice = fn.loanLocals.get(borrowKey(borrow))
      if (slice === undefined) return []
      fn.loanLocals.delete(borrowKey(borrow))
      return [
        Object.freeze({
          _tag: 'EndLoan' as const,
          borrow,
          slice,
          provenance: generated(statement.span),
        }),
      ]
    })
    if (droppedBinding !== undefined) fn.effectLoanEnds.delete(droppedBinding)
    const loanReleases = [...ownershipLoanReleases, ...retainedLoanReleases].filter(
      (release, ordinal, releases) =>
        releases.findIndex(
          (candidate) => borrowKey(candidate.borrow) === borrowKey(release.borrow),
        ) === ordinal,
    )
    const cleanup = fn.reserve()
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: cleanup,
          provenance: generated(statement.span),
        }),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'CleanupRegion',
        id: cleanup,
        ...ownerFields(ownerLoop),
        releases: Object.freeze([
          ...loanReleases,
          Object.freeze({
            _tag: 'Drop',
            local: lowered.result,
            cleanup: cleanupForLocal(
              fn,
              bindingFact === undefined
                ? concreteCleanup(fn, Mir.semanticType(localType))
                : bindingFact.cleanup,
              localType,
            ),
            provenance: authored(statement.span),
          }),
        ]),
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Unsafe') {
    const body = fn.reserve()
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations: Object.freeze([]),
        outcome: Object.freeze({
          _tag: 'Forward',
          target: body,
          provenance: authored(statement.span),
        }),
      }),
    )
    const forward = Object.freeze({
      _tag: 'Forward' as const,
      target: following,
      provenance: generated(statement.span),
    })
    if (
      lowerSequence(
        fn,
        statement.statements,
        exits,
        ownerLoop,
        forward,
        body,
        exits.scopeEnds.get(spanKey(statement.span)),
      ) === undefined ||
      lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
    )
      return undefined
    return id
  }

  if (statement._tag === 'If' || statement._tag === 'IfLet') {
    const conditional = fn.reserve()
    const taken = fn.reserve()
    const otherwise = fn.reserve()
    const following = fn.reserve()
    const [condition, operations] = fn.capture(() =>
      statement._tag === 'If'
        ? lowerExpression(fn, statement.condition)
        : lowerPatternSelection(fn, statement.selection, 'Bool'),
    )
    if (condition === undefined) return undefined
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: conditional,
          provenance: generated(statement.span),
        }),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'ConditionalRegion',
        id: conditional,
        ...ownerFields(ownerLoop),
        condition: condition.result,
        taken,
        otherwise,
        following,
        provenance: authored(statement.span),
      }),
    )
    const branchState = delayedEffectState(fn)
    const beforeTaken = new Set(
      fn.regions.flatMap((region) => (region === undefined ? [] : [region.id.ordinal])),
    )
    const loweredTaken = lowerSequence(
      fn,
      statement.taken,
      exits,
      ownerLoop,
      Object.freeze({
        _tag: 'Forward',
        target: following,
        provenance: generated(statement.span),
      }),
      taken,
      exits.armEnds.get(`${spanKey(statement.span)}:Taken`),
    )
    if (loweredTaken === undefined) return undefined
    const takenState = delayedEffectState(fn)
    const takenRegions = fn.regions.flatMap((region) =>
      region !== undefined && !beforeTaken.has(region.id.ordinal) ? [region.id.ordinal] : [],
    )
    if (statement._tag === 'IfLet')
      for (const binding of statement.selection.bindings)
        fn.patternLocals.delete(patternKey(binding.id))
    restoreDelayedEffectState(fn, branchState)
    const beforeOtherwise = new Set(
      fn.regions.flatMap((region) => (region === undefined ? [] : [region.id.ordinal])),
    )
    const loweredOtherwise = lowerSequence(
      fn,
      statement.otherwise,
      exits,
      ownerLoop,
      Object.freeze({
        _tag: 'Forward',
        target: following,
        provenance: generated(statement.span),
      }),
      otherwise,
      exits.armEnds.get(`${spanKey(statement.span)}:Otherwise`),
    )
    if (loweredOtherwise === undefined) return undefined
    const otherwiseState = delayedEffectState(fn)
    const otherwiseRegions = fn.regions.flatMap((region) =>
      region !== undefined && !beforeOtherwise.has(region.id.ordinal) ? [region.id.ordinal] : [],
    )
    restoreDelayedEffectState(fn, branchState)
    const branchEndings = (
      state: DelayedEffectState,
      other: DelayedEffectState,
    ): ReadonlyArray<Extract<Mir.Operation, { readonly _tag: 'EndLoan' }>> =>
      Object.freeze(
        (fn.ownership?.loans ?? []).flatMap((loan) => {
          const key = borrowKey(loan.id)
          const held = state.loanLocals.get(key)
          return held !== undefined && !other.loanLocals.has(key)
            ? [
                Object.freeze({
                  _tag: 'EndLoan' as const,
                  borrow: loan.id,
                  slice: held,
                  provenance: generated(statement.span),
                }),
              ]
            : []
        }),
      )
    const takenEndings = branchEndings(takenState, otherwiseState)
    const otherwiseEndings = branchEndings(otherwiseState, takenState)
    for (const key of branchState.loanLocals.keys()) {
      if (!takenState.loanLocals.has(key) || !otherwiseState.loanLocals.has(key))
        fn.loanLocals.delete(key)
    }
    for (const [regions, releases] of [
      [takenRegions, takenEndings],
      [otherwiseRegions, otherwiseEndings],
    ] as const)
      if (releases.length > 0) {
        const branchEnd = fn.reserve()
        for (const ordinal of regions) {
          const region = fn.regions.at(ordinal)
          if (
            region === undefined ||
            (region._tag !== 'OperationRegion' && region._tag !== 'CleanupRegion') ||
            region.outcome._tag !== 'Forward' ||
            region.outcome.target.ordinal !== following.ordinal
          )
            continue
          fn.regions[ordinal] = Object.freeze({
            ...region,
            outcome: Object.freeze({ ...region.outcome, target: branchEnd }),
          })
        }
        fn.publish(
          Object.freeze({
            _tag: 'CleanupRegion',
            id: branchEnd,
            ...ownerFields(ownerLoop),
            releases,
            outcome: Object.freeze({
              _tag: 'Forward',
              target: following,
              provenance: generated(statement.span),
            }),
          }),
        )
      }
    if (lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined)
      return undefined
    return id
  }

  if (statement._tag === 'While') {
    const loop: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: statement.loop.ordinal })
    const conditionId = fn.reserve()
    const bodyId = fn.reserve()
    const following = fn.reserve()
    const entryState = delayedEffectState(fn)
    const [condition, conditionOperations] = fn.capture(() =>
      lowerExpression(fn, statement.condition),
    )
    if (condition === undefined) return undefined
    fn.publish(
      Object.freeze({
        _tag: 'LoopRegion',
        id,
        ...ownerFields(ownerLoop),
        loop,
        ...(ownerLoop === undefined ? {} : { parent: ownerLoop }),
        condition: conditionId,
        conditionValue: condition.result,
        body: bodyId,
        following,
        provenance: authored(statement.span),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id: conditionId,
        ownerLoop: loop,
        operations: conditionOperations,
        outcome: Object.freeze({ _tag: 'Yield', provenance: generated(statement.span) }),
      }),
    )
    const repeat = Object.freeze({
      _tag: 'Repeat' as const,
      loop,
      provenance: generated(statement.span),
    })
    const loopState = delayedEffectState(fn)
    const loweredBody = lowerSequence(
      fn,
      statement.body,
      exits,
      loop,
      repeat,
      bodyId,
      exits.loopFallthroughs.get(statement.loop.ordinal),
    )
    if (loweredBody === undefined) return undefined
    const bodyState = delayedEffectState(fn)
    const delayedLoanKeys = new Set(
      [...entryState.loanLocals.keys()].filter(
        (key) => !loopState.loanLocals.has(key) || !bodyState.loanLocals.has(key),
      ),
    )
    const delayedLoans = delayedLoopLoans(fn, delayedLoanKeys, entryState)
    if (delayedLoanKeys.size > 0) {
      const loopFamily = new Set<number>([loop.ordinal])
      let changed = true
      while (changed) {
        changed = false
        for (const region of fn.regions) {
          if (
            region?._tag !== 'LoopRegion' ||
            region.parent === undefined ||
            !loopFamily.has(region.parent.ordinal) ||
            loopFamily.has(region.loop.ordinal)
          )
            continue
          loopFamily.add(region.loop.ordinal)
          changed = true
        }
      }
      for (const region of fn.regions) {
        if (region === undefined || !loopFamily.has(region.ownerLoop?.ordinal ?? -1)) continue
        if (region._tag === 'OperationRegion') {
          const operations = withDelayedFailureLoanEndings(
            fn,
            withoutLoanEndings(region.operations, delayedLoanKeys),
            delayedLoans,
          )
          const terminalEndings =
            region.outcome._tag === 'Return' || region.outcome._tag === 'Trap'
              ? terminalLoopLoanEndings(delayedLoans, region.outcome)
              : []
          fn.regions[region.id.ordinal] = Object.freeze({
            ...region,
            operations: Object.freeze([...operations, ...terminalEndings]),
          })
        } else if (region._tag === 'CleanupRegion') {
          const releases = withoutLoanEndings(region.releases, delayedLoanKeys).flatMap(
            (release) => (release._tag === 'Drop' || release._tag === 'EndLoan' ? [release] : []),
          )
          const terminalEndings =
            region.outcome._tag === 'Return' || region.outcome._tag === 'Trap'
              ? terminalLoopLoanEndings(delayedLoans, region.outcome)
              : []
          fn.regions[region.id.ordinal] = Object.freeze({
            ...region,
            releases: Object.freeze([...terminalEndings, ...releases]),
          })
        }
      }
    }
    restoreDelayedEffectState(fn, loopState)
    for (const key of delayedLoanKeys) {
      const held = entryState.loanLocals.get(key)
      if (held !== undefined) fn.loanLocals.set(key, held)
    }
    const continuation = delayedLoans.length === 0 ? following : fn.reserve()
    if (delayedLoans.length > 0) {
      const [, releases] = fn.capture(() => {
        for (const loan of delayedLoans) {
          const slice = fn.loanLocals.get(loan.key)
          if (slice === undefined) continue
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan',
              borrow: loan.borrow,
              slice,
              provenance: generated(statement.span),
            }),
          )
          fn.loanLocals.delete(loan.key)
        }
      })
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id: following,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: continuation,
            provenance: generated(statement.span),
          }),
        }),
      )
    }
    if (lowerSequence(fn, rest, exits, ownerLoop, terminal, continuation, armExit) === undefined)
      return undefined
    return id
  }

  if (statement._tag === 'Break' || statement._tag === 'Continue') {
    const target: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: statement.target.ordinal })
    const outcome: Mir.Outcome = Object.freeze({
      _tag: statement._tag === 'Break' ? ('Exit' as const) : ('Repeat' as const),
      loop: target,
      provenance: authored(statement.span),
    })
    const [, releases] = fn.capture(() =>
      emitReleases(fn, exits.transfers.get(spanKey(statement.span))),
    )
    if (releases.length === 0) {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome,
        }),
      )
    } else {
      const cleanup = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: cleanup,
            provenance: generated(statement.span),
          }),
        }),
      )
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id: cleanup,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome,
        }),
      )
    }
    return id
  }

  if (statement._tag === 'Fail') {
    const specializedFailure = fn.semantic(statement.failure)
    if (Type.isNever(specializedFailure)) {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Trap',
            reason: 'unreachable failure of never',
            provenance: generated(statement.span),
          }),
        }),
      )
      return id
    }
    const [failedValue, operations] = fn.capture(() => {
      const failed = lowerExpression(fn, statement.expression)
      const outcomeType = fn.effectOutcome === undefined ? undefined : fn.type(fn.effectOutcome)
      if (failed === undefined || outcomeType?._tag !== 'EffectOutcome') return undefined
      const destination = fn.alloc(outcomeType)
      if (!Type.isUnion(specializedFailure)) {
        const tag = Type.failureMembers(outcomeType.type).findIndex((failure) =>
          Type.equals(failure, specializedFailure),
        )
        if (tag < 0) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'PackEffectOutcome' as const,
            destination,
            source: failed.result,
            tag: tag + 1,
            type: outcomeType,
            provenance: authored(statement.span),
          }),
        )
      } else {
        const sourceType = fn.type(specializedFailure)
        if (sourceType?._tag !== 'Union') return undefined
        const mappings = specializedFailure.members.flatMap((member, source) => {
          const target = Type.failureMembers(outcomeType.type).findIndex((failure) =>
            Type.equals(failure, member),
          )
          return target < 0 ? [] : [Object.freeze({ source, target: target + 1 })]
        })
        if (mappings.length !== specializedFailure.members.length) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'PackEffectFailureUnion' as const,
            destination,
            source: failed.result,
            sourceType,
            mappings: Object.freeze(mappings),
            type: outcomeType,
            provenance: authored(statement.span),
          }),
        )
      }
      return destination
    })
    if (failedValue === undefined) return undefined
    const failureOutcome: Mir.Outcome = Object.freeze({
      _tag: 'Return',
      value: failedValue,
      provenance: authored(statement.span),
    })
    const [, releases] = fn.capture(() =>
      emitReleases(fn, exits.returns.get(spanKey(statement.span))),
    )
    if (releases.length === 0) {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations,
          outcome: failureOutcome,
        }),
      )
    } else {
      const cleanup = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations,
          outcome: Object.freeze({
            _tag: 'Forward',
            target: cleanup,
            provenance: generated(statement.span),
          }),
        }),
      )
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id: cleanup,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome: failureOutcome,
        }),
      )
    }
    return id
  }

  const [returnedValue, operations] = fn.capture(() => {
    const returned = lowerExpression(fn, statement.expression)
    if (returned === undefined) return undefined
    if (fn.effectOutcome === undefined) return returned.result
    const outcomeType = fn.type(fn.effectOutcome)
    if (outcomeType?._tag !== 'EffectOutcome') return undefined
    const destination = fn.alloc(outcomeType)
    fn.emit(
      Object.freeze({
        _tag: 'PackEffectOutcome',
        destination,
        source: returned.result,
        tag: 0,
        type: outcomeType,
        provenance: authored(statement.span),
      }),
    )
    return destination
  })
  if (returnedValue === undefined) return undefined
  const returnOutcome: Mir.Outcome = Object.freeze({
    _tag: 'Return',
    value: returnedValue,
    provenance: authored(statement.span),
  })
  const [, releases] = fn.capture(() =>
    emitReleases(fn, exits.returns.get(spanKey(statement.span))),
  )
  if (releases.length === 0) {
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: returnOutcome,
      }),
    )
  } else {
    const cleanup = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: cleanup,
          provenance: generated(statement.span),
        }),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'CleanupRegion',
        id: cleanup,
        ...ownerFields(ownerLoop),
        releases: Object.freeze(
          releases.flatMap((operation) =>
            operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
          ),
        ),
        outcome: returnOutcome,
      }),
    )
  }
  return id
}
