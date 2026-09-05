import type { ExitIndex } from './CleanupEmission.js'
import * as CleanupPlan from './CleanupPlan.js'
import {
  authored,
  lowerBorrowedWritePlace,
  cleanupForLocal,
  concreteCleanup,
  delayedLoopLoans,
  effectContract,
  emitReleases,
  emitInitializationTransition,
  generated,
  initializationFor,
  initializeBinding,
  lowerWriteSelectors,
  lowerOwnershipPath,
  ownershipLocal,
  ownerFields,
  prepareInitialization,
  specializedCleanup,
  transitionAt,
  terminalLoopLoanEndings,
  withDelayedFailureLoanEndings,
  withoutLoanEndings,
} from './CleanupEmission.js'
import type { LoweredExpression } from './EffectLowering.js'
import {
  borrowedWriteRoot,
  endLoans,
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
import { lowerExpression, lowerExecution } from './LowerExpression.js'
import * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as MovePath from './MovePath.js'
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
): LoweredPatternSelection | 'Transferred' | undefined => {
  if (selection.subject._tag === 'Unavailable') return undefined
  const subject = lowerExpression(fn, selection.subject)
  if (subject === 'Transferred') return subject
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
  if (selectedResult === undefined || selectedResult === 'Transferred') return selectedResult
  const emptyCoverage: ReadonlyArray<Match.CoverageIdentity> = Object.freeze([])
  const selectedAfter = selection.universal
    ? emptyCoverage
    : Object.freeze(
        members.filter(
          (candidate) => member === undefined || !Match.selects(member, candidate, 'Runtime'),
        ),
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
  const selectedExecution = lowerExecution(fn, selection.span, () => {
    for (const operation of finalizedSelectedOperations) fn.emit(operation)
    return selectedResult
  })
  if (selectedExecution === undefined) return undefined
  const selectedArm: Mir.MatchArm = Object.freeze({
    id: selection.arm,
    ...(member === undefined ? {} : { member }),
    universal: selection.universal,
    before: members,
    after: selectedAfter,
    bindings: Object.freeze(selectedBindings),
    cleanupBindings: Object.freeze([]),
    selected: Object.freeze({
      access: selection.access,
      execution: selectedExecution,
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
  const fallbackExecution = lowerExecution(fn, selection.span, () => literal(false))
  if (fallbackExecution === undefined) return undefined
  const fallbackArm: Mir.MatchArm = Object.freeze({
    id: fallbackId,
    universal: true,
    before: selectedAfter,
    after: Object.freeze([]),
    bindings: Object.freeze([]),
    cleanupBindings: Object.freeze([]),
    selected: Object.freeze({
      access: selection.access,
      execution: fallbackExecution,
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
          } else if (member === undefined || !Match.selects(member, candidate, 'Runtime')) {
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
  // Iterative: a sequence is as long as the author wrote it, so its length must not become
  // JavaScript stack depth. Only nested blocks recurse.
  let region = id
  const initialization = prepareInitialization(fn)
  if (initialization.length > 0) {
    region = fn.reserve()
    fn.publish({
      _tag: 'OperationRegion',
      id,
      operations: initialization,
      outcome: {
        _tag: 'Forward',
        target: region,
        provenance: generated(fn.owner.function.declaration.syntax.span),
      },
    })
  }
  for (const statement of statements) {
    const previousLoop = fn.ownerLoop
    fn.ownerLoop = ownerLoop
    const following = lowerStatement(fn, statement, exits, ownerLoop, terminal, region)
    fn.ownerLoop = previousLoop
    if (following === undefined) return undefined
    if (following === 'Terminated') return id
    region = following
  }
  const [, releases] = fn.capture(() => emitReleases(fn, armExit))
  if (releases.length > 0) {
    fn.publish(
      Object.freeze({
        _tag: 'CleanupRegion',
        id: region,
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
        id: region,
        ...ownerFields(ownerLoop),
        operations: Object.freeze([]),
        outcome: terminal,
      }),
    )
  }
  return id
}

/**
 * Lowers one statement into the region `id`. Returns the reserved region the next statement
 * continues into, `'Terminated'` when the statement ends its sequence, or `undefined` when the
 * statement cannot be lowered.
 */
const lowerStatement = (
  fn: FunctionLowering,
  statement: Hir.Statement,
  exits: ExitIndex,
  ownerLoop: Mir.LoopId | undefined,
  terminal: Mir.Outcome,
  id: Mir.RegionId,
): Mir.RegionId | 'Terminated' | undefined => {
  const transferred = (operations: ReadonlyArray<Mir.Operation>): 'Terminated' => {
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Trap',
          reason: 'unreachable expression continuation',
          provenance: generated(statement.span),
        }),
      }),
    )
    return 'Terminated'
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
    return 'Terminated'
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
      return following
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
      (callableSchema.constraints.length > 0 || callableSchema.evidence.length > 0)
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
      return following
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
      return following
    }
    const [initializer, operations] = fn.capture(() => {
      const lowered = lowerExpression(fn, statement.initializer)
      if (lowered === 'Transferred') return lowered
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
      initializeBinding(fn, { _tag: 'Let', binding: statement.binding }, statement.span)
      const destinationType = fn.localTypes.at(destination.ordinal)
      if (destinationType?._tag === 'EffectValue') {
        const retained = retainedEffectLoans(fn, statement.initializer)
        if (retained.length > 0) fn.effectLoanEnds.set(statement.binding.ordinal, retained)
      }
      return destination
    })
    if (initializer === 'Transferred') return transferred(operations)
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
    return following
  }

  if (statement._tag === 'PatternBind') {
    const [selection, operations] = fn.capture(() =>
      lowerPatternSelection(fn, statement.selection, 'Unit'),
    )
    if (selection === 'Transferred') return transferred(operations)
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
    return following
  }

  if (statement._tag === 'Evaluate') {
    const [evaluated, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (evaluated === 'Transferred') return transferred(operations)
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
    return following
  }

  if (statement._tag === 'Write') {
    const place = statement.place
    const transition = transitionAt(fn, statement.span, 'Write')
    let root: Mir.LocalId | undefined
    if (place._tag === 'BorrowedWritePlace') root = borrowedWriteRoot(fn, place.root)
    else if (transition !== undefined) root = ownershipLocal(fn, transition.root)
    else root = ownedWriteRoot(fn, place.root)
    let rootType = root === undefined ? undefined : fn.localTypes.at(root.ordinal)
    // A whole callable binding keeps its exact representation; the written value carries the same
    // identity once analysis admitted the write.
    const type =
      fn.type(place.type) ??
      (rootType?._tag === 'CallableValue' && place.selectors.length === 0 ? rootType : undefined)
    const [written, operations] = fn.capture(() => {
      if (root === undefined || rootType === undefined || type === undefined) return false
      let selectors: ReadonlyArray<Mir.PlaceSelector> | 'Transferred' | undefined
      if (place._tag === 'BorrowedWritePlace') {
        const lowered = lowerBorrowedWritePlace(fn, root, place.selectors, place.type, place.span)
        if (lowered === 'Transferred') return lowered
        if (lowered === undefined) return false
        root = lowered.root
        selectors = lowered.selectors
      } else if (transition !== undefined)
        selectors = lowerOwnershipPath(fn, root, transition.path, place.span)
      else selectors = lowerWriteSelectors(fn, place.selectors)
      if (selectors === 'Transferred') return selectors
      if (selectors === undefined) return false
      rootType = fn.localTypes.at(root.ordinal)
      if (rootType === undefined) return false
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
      if (value === 'Transferred') return value
      if (value === undefined) return false
      // A live displaced value leaves the place as the replacement commits (the verifier pairs
      // a consuming read with its write), then cleans exactly once through its own plan.
      const replacement = fn.ownership?.replacements.find(
        (candidate) =>
          candidate.span.start === statement.span.start &&
          candidate.span.end === statement.span.end,
      )
      const displacedCleanup =
        replacement === undefined ? undefined : cleanupForLocal(fn, replacement.cleanup, type)
      if (place._tag === 'WritePlace' && displacedCleanup !== undefined) {
        const initialization =
          replacement === undefined || transition === undefined
            ? undefined
            : initializationFor(fn, transition.root, replacement.initialization, transition.path)
        fn.emit({
          _tag: 'Drop',
          local: root,
          selectors,
          cleanup: displacedCleanup,
          ...(initialization === undefined ? {} : { initialization }),
          provenance: authored(statement.span),
        })
      }
      const displaced =
        place._tag === 'BorrowedWritePlace' &&
        displacedCleanup !== undefined &&
        CleanupPlan.hasEffect(displacedCleanup)
          ? fn.alloc(type)
          : undefined
      if (displaced !== undefined) {
        fn.emit(
          Object.freeze({
            _tag: 'ReadPlace',
            destination: displaced,
            root,
            selectors,
            type,
            consume: true,
            provenance: authored(statement.span),
          }),
        )
      }
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
      if (displaced !== undefined && displacedCleanup !== undefined) {
        fn.emit(
          Object.freeze({
            _tag: 'Drop',
            local: displaced,
            cleanup: displacedCleanup,
            provenance: authored(statement.span),
          }),
        )
      }
      if (transition !== undefined) emitInitializationTransition(fn, transition)
      endReturnedViewLoans(fn, statement.span)
      return true
    })
    if (written === 'Transferred') return transferred(operations)
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
    return following
  }

  if (statement._tag === 'Drop') {
    const transition = transitionAt(fn, statement.span, 'Drop')
    if (transition !== undefined && ownershipLocal(fn, transition.root) !== undefined) {
      const root = ownershipLocal(fn, transition.root)
      let type: Mir.Type | undefined
      if (transition.path.length === 0 && root !== undefined) type = fn.localTypes.at(root.ordinal)
      else if ('type' in statement.expression) type = fn.type(statement.expression.type)
      if (root === undefined || type === undefined) return undefined
      const selectors = lowerOwnershipPath(fn, root, transition.path, statement.span)
      if (selectors === undefined) return undefined
      let selected = transition.before
      for (const selector of transition.path)
        selected =
          selected.children.find(
            (child) => MovePath.key([child.selector]) === MovePath.key([selector]),
          )?.state ?? MovePath.make(selected.initialization)
      const initialization = initializationFor(fn, transition.root, selected, transition.path)
      const [, operations] = fn.capture(() => {
        const authoredEndings = (fn.ownership?.loans ?? [])
          .filter((loan) => spanKey(loan.endSpan) === spanKey(statement.span))
          .map((loan) => loan.id)
        const retainedEndings =
          transition.root._tag === 'Let' && transition.path.length === 0
            ? (fn.effectLoanEnds.get(transition.root.binding.ordinal) ?? [])
            : []
        endLoans(fn, [...authoredEndings, ...retainedEndings], statement.span)
        if (transition.root._tag === 'Let' && transition.path.length === 0)
          fn.effectLoanEnds.delete(transition.root.binding.ordinal)
        fn.emit({
          _tag: 'Drop',
          local: root,
          selectors,
          cleanup: cleanupForLocal(fn, concreteCleanup(fn, Mir.semanticType(type)), type),
          ...(initialization === undefined ? {} : { initialization }),
          provenance: authored(statement.span),
        })
        emitInitializationTransition(fn, transition)
        endReturnedViewLoans(fn, statement.span)
      })
      const following = fn.reserve()
      fn.publish({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: { _tag: 'Forward', target: following, provenance: generated(statement.span) },
      })
      return following
    }
    const droppedExpression =
      statement.expression._tag === 'Move' ? statement.expression.subject : statement.expression
    const droppedRecipe = callableRecipe(fn, droppedExpression)
    const droppedRecipeType =
      droppedRecipe === undefined ? undefined : fn.semantic(droppedRecipe.type)
    if (
      droppedRecipeType !== undefined &&
      Type.isCallable(droppedRecipeType) &&
      droppedRecipeType.schema !== undefined &&
      (droppedRecipeType.schema.constraints.length > 0 ||
        droppedRecipeType.schema.evidence.length > 0)
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
      return following
    }
    const [lowered, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (lowered === 'Transferred') return transferred(operations)
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
    return following
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
      ) === undefined
    )
      return undefined
    return following
  }

  if (statement._tag === 'If' || statement._tag === 'IfLet') {
    const [condition, operations] = fn.capture(() =>
      statement._tag === 'If'
        ? lowerExpression(fn, statement.condition)
        : lowerPatternSelection(fn, statement.selection, 'Bool'),
    )
    if (condition === 'Transferred') return transferred(operations)
    if (condition === undefined) return undefined
    const conditional = fn.reserve()
    const taken = fn.reserve()
    const otherwise = fn.reserve()
    const following = fn.reserve()
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
    return following
  }

  if (statement._tag === 'While') {
    const loop: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: statement.loop.ordinal })
    const entryState = delayedEffectState(fn)
    const [condition, conditionOperations] = fn.capture(() =>
      lowerExpression(fn, statement.condition),
    )
    if (condition === 'Transferred') return transferred(conditionOperations)
    if (condition === undefined) return undefined
    const conditionId = fn.reserve()
    const bodyId = fn.reserve()
    const following = fn.reserve()
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
    return continuation
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
    return 'Terminated'
  }

  if (statement._tag === 'Fail') {
    const specializedFailure = fn.semantic(statement.failure)
    const [failedValue, operations] = fn.capture(() => {
      const failed = lowerExpression(fn, statement.expression)
      if (failed === 'Transferred') return failed
      if (failed !== undefined && Type.isNever(specializedFailure)) return 'Transferred'
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
    if (failedValue === 'Transferred') return transferred(operations)
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
    return 'Terminated'
  }

  const [returnedValue, operations] = fn.capture(() => {
    const returned = lowerExpression(fn, statement.expression)
    if (returned === 'Transferred') return returned
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
  if (returnedValue === 'Transferred') return transferred(operations)
  if (returnedValue === undefined) return undefined
  const [, releases] = fn.capture(() =>
    emitReleases(fn, exits.returns.get(spanKey(statement.span))),
  )
  const returnedType = fn.localTypes.at(returnedValue.ordinal)
  // A Copy read produces its return value before scope cleanup ends the source local's storage.
  const copyBeforeCleanup =
    returnedType !== undefined &&
    Mir.isCopy(fn.layout, Mir.semanticType(returnedType)) &&
    releases.some(
      (operation) => operation._tag === 'Drop' && operation.local.ordinal === returnedValue.ordinal,
    )
  const result = copyBeforeCleanup ? fn.alloc(returnedType) : returnedValue
  const returnOperations = copyBeforeCleanup
    ? Object.freeze([
        ...operations,
        Object.freeze({
          _tag: 'Move' as const,
          destination: result,
          source: returnedValue,
          provenance: authored(statement.span),
        }),
      ])
    : operations
  const returnOutcome: Mir.Outcome = Object.freeze({
    _tag: 'Return',
    value: result,
    provenance: authored(statement.span),
  })
  if (releases.length === 0) {
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations: returnOperations,
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
        operations: returnOperations,
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
  return 'Terminated'
}
