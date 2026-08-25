import * as CleanupPlan from './CleanupPlan.js'
import { endLoans } from './EffectLowering.js'
import type {} from './EntryAssembly.js'
import type {} from './Forwarding.js'
import type { FunctionLowering } from './FunctionLowering.js'
import type * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import type { DelayedEffectState } from './Lower.js'
import { borrowKey, patternKey, spanKey } from './Lower.js'
import type {} from './LowerExpression.js'
import { lowerExpression } from './LowerExpression.js'
import * as Mir from './Mir.js'
import * as Ownership from './Ownership.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import {
  callableValueByIdentity,
  effectValueByIdentity,
  representedValueType,
  storedCallableValueType,
  storedEffectValueType,
} from './ValueType.js'

export interface ExitIndex {
  readonly returns: ReadonlyMap<string, Ownership.ExitPlan>
  readonly scopeEnds: ReadonlyMap<string, Ownership.ExitPlan>
  readonly armEnds: ReadonlyMap<string, Ownership.ExitPlan>
  readonly loopFallthroughs: ReadonlyMap<number, Ownership.ExitPlan>
  readonly transfers: ReadonlyMap<string, Ownership.ExitPlan>
}

export const indexExits = (plan: Ownership.FunctionOwnership | undefined): ExitIndex => {
  const returns = new Map<string, Ownership.ExitPlan>()
  const scopeEnds = new Map<string, Ownership.ExitPlan>()
  const armEnds = new Map<string, Ownership.ExitPlan>()
  const loopFallthroughs = new Map<number, Ownership.ExitPlan>()
  const transfers = new Map<string, Ownership.ExitPlan>()
  for (const exit of plan?.exits ?? []) {
    switch (exit.kind) {
      case 'Return':
        returns.set(spanKey(exit.span), exit)
        break
      case 'ScopeEnd':
        scopeEnds.set(spanKey(exit.span), exit)
        break
      case 'ArmEnd':
        armEnds.set(`${spanKey(exit.span)}:${exit.arm ?? 'Taken'}`, exit)
        break
      case 'LoopFallthrough':
        if (exit.target !== undefined) loopFallthroughs.set(exit.target.ordinal, exit)
        break
      case 'Break':
      case 'Continue':
        transfers.set(spanKey(exit.span), exit)
        break
    }
  }
  return { returns, scopeEnds, armEnds, loopFallthroughs, transfers }
}

export const concreteCleanup = (
  fn: FunctionLowering,
  type: Type.Type,
  seen = new Set<string>(),
): CleanupPlan.CleanupPlan => {
  const specialized = Type.substitute(type, fn.substitution)
  const resolveRepresented = (candidate: Type.Type): CleanupPlan.CleanupPlan | undefined => {
    const concrete = Type.substitute(candidate, fn.substitution)
    if (!Type.isRepresented(concrete)) return undefined
    const value =
      storedCallableValueType(fn.layout, concrete) ??
      storedEffectValueType(fn.layout, concrete) ??
      representedValueType(fn.layout, fn.opaqueRealizations, concrete, new Map())
    if (value?._tag === 'CallableValue') {
      if (value.storage?._tag === 'StoredCallableField') {
        return CleanupPlan.realizedCallableCleanup(fn.index, value.storage.realization)
      }
      return callableLocalCleanup(fn, value)
    }
    if (value?._tag === 'EffectValue') {
      return effectLocalCleanup(fn, value, new Set())
    }
    if (value?._tag === 'EffectComposite') {
      return Object.freeze({
        _tag: 'EffectCompositeCleanup' as const,
        type: value.type,
        alternatives: Object.freeze(
          value.alternatives.map((alternative) => effectLocalCleanup(fn, alternative, new Set())),
        ),
      })
    }
    return undefined
  }
  const realized = resolveRepresented(specialized)
  if (realized !== undefined) return realized
  return CleanupPlan.specializeCleanup(
    CleanupPlan.cleanupPlan(fn.index, specialized, seen),
    new Map(),
    (nested) => resolveRepresented(nested) ?? CleanupPlan.cleanupPlan(fn.index, nested, seen),
  )
}

export function effectLocalCleanup(
  fn: FunctionLowering,
  effectValue: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  seen: ReadonlySet<string>,
): CleanupPlan.CleanupPlan {
  const identity =
    effectValue.storage?.realization.runnerIdentity ??
    Instances.effectIdentity(effectValue.environment.instance, effectValue.site)
  if (seen.has(identity)) return Object.freeze({ _tag: 'NoCleanup', type: effectValue.type })
  const next = new Set(seen).add(identity)
  let laneOffset = 0
  const slots = effectValue.environment.fields.flatMap((field, ordinal) => {
    const nested =
      field.effectIdentity === undefined
        ? undefined
        : effectValueByIdentity(fn.layout, field.effectIdentity)
    const callable =
      field.callableIdentity === undefined || !Type.isCallable(field.type)
        ? undefined
        : callableValueByIdentity(fn.layout, field.callableIdentity, field.type)
    // Offsets must mirror the runner ABI exactly, so the count comes from the same Layout
    // helper that materializes the environment lanes for backends.
    const laneCount = Layout.effectFieldLanes(fn.layout, field).length
    const currentOffset = laneOffset
    laneOffset += laneCount
    const realizationOrdinal =
      effectValue.storage?.realization.environment.at(ordinal)?.ordinal ?? ordinal
    const storedOwned =
      effectValue.storage?.realization.cleanup.unrunLanes.includes(realizationOrdinal) ?? false
    if (effectValue.storage === undefined ? field.representation === 'Borrow' : !storedOwned)
      return []
    let fieldCleanup: CleanupPlan.CleanupPlan
    if (callable === undefined) {
      if (nested === undefined) {
        fieldCleanup = concreteCleanup(fn, field.type)
      } else {
        fieldCleanup = effectLocalCleanup(fn, nested, next)
      }
    } else {
      fieldCleanup = callableLocalCleanup(fn, callable)
    }
    return fieldCleanup._tag === 'NoCleanup' && effectValue.storage === undefined
      ? []
      : [
          Object.freeze({
            ordinal: realizationOrdinal,
            laneOffset: currentOffset,
            laneCount,
            cleanup: fieldCleanup,
          }),
        ]
  })
  const releaseSlots = Ownership.inReleaseOrder(slots)
  return releaseSlots.length === 0
    ? Object.freeze({ _tag: 'NoCleanup', type: effectValue.type })
    : Object.freeze({
        _tag: 'EffectCleanup',
        type: effectValue.type,
        site: effectValue.site,
        slots: Object.freeze(releaseSlots),
      })
}

export const specializedCleanup = (
  fn: FunctionLowering,
  cleanup: CleanupPlan.CleanupPlan,
): CleanupPlan.CleanupPlan =>
  CleanupPlan.specializeCleanup(cleanup, fn.substitution, (type) => concreteCleanup(fn, type))

export const cleanupForLocal = (
  fn: FunctionLowering,
  cleanup: CleanupPlan.CleanupPlan,
  localType: Mir.Type,
): CleanupPlan.CleanupPlan => {
  const specialized = specializedCleanup(fn, cleanup)
  if (localType._tag === 'EffectValue') {
    return effectLocalCleanup(fn, localType, new Set())
  }
  if (localType._tag === 'EffectComposite') {
    return Object.freeze({
      _tag: 'EffectCompositeCleanup',
      type: localType.type,
      alternatives: Object.freeze(
        localType.alternatives.map((alternative) => effectLocalCleanup(fn, alternative, new Set())),
      ),
    })
  }
  if (localType._tag !== 'CallableValue') {
    return specialized
  }
  if (localType.storage === undefined) return callableLocalCleanup(fn, localType)
  if (specialized._tag !== 'CallableCleanup') return specialized
  const fields = localType.environment?.fields ?? []
  return Object.freeze({
    _tag: 'CallableCleanup',
    type: localType.type,
    environment:
      localType.environment === undefined
        ? specialized.environment
        : Object.freeze({
            _tag: 'CallableEnvironmentIdentity',
            identity: Instances.callableEnvironmentIdentity(localType.environment.callable),
          }),
    slots: Object.freeze(
      specialized.slots.flatMap((slot) => {
        const field = fields.find((candidate) => candidate.ordinal === slot.ordinal)
        return field === undefined
          ? []
          : [Object.freeze({ ordinal: slot.ordinal, cleanup: concreteCleanup(fn, field.type) })]
      }),
    ),
  })
}

/**
 * The Drop operations a propagating failure must execute before it leaves this function:
 * every owner the ownership phase saw live at the run site, resolved to this function's
 * locals. Sites without a local here belong to a different compiled body and are skipped.
 */
export const propagationReleases = (
  fn: FunctionLowering,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Mir.DropOperation> => {
  const exit = fn.ownership?.exits.find(
    (candidate) =>
      candidate.kind === 'Propagation' &&
      candidate.span.start === span.start &&
      candidate.span.end === span.end,
  )
  if (exit === undefined) return Object.freeze([])
  return Object.freeze(
    exit.releases.flatMap((release): ReadonlyArray<Mir.DropOperation> => {
      if (release.cleanup._tag === 'NoCleanup') return []
      const site = release.binding.site
      let local: Mir.LocalId | undefined
      if (site._tag === 'Let') {
        local = fn.bindingLocals.get(site.binding.ordinal)
      } else if (site._tag === 'Parameter') {
        local = fn.parameterLocals.get(site.parameter.ordinal)
      } else if (site._tag === 'Pattern') {
        local = fn.patternLocals.get(patternKey(site.binding))
      } else {
        local = undefined
      }
      const localType = local === undefined ? undefined : fn.localTypes.at(local.ordinal)
      if (local === undefined || localType === undefined) return []
      return [
        Object.freeze({
          _tag: 'Drop' as const,
          local,
          cleanup: cleanupForLocal(fn, release.binding.cleanup, localType),
          provenance: generated(span),
        }),
      ]
    }),
  )
}

/**
 * The caller-owned loans live at a run site. A typed failure exits before the success-path
 * operations that normally end them, so the run carries an explicit failure-only cleanup path.
 */
export const dependencyOrderedLoanEndings = (
  fn: FunctionLowering,
  endings: ReadonlyArray<Mir.EndLoanOperation>,
): ReadonlyArray<Mir.EndLoanOperation> => {
  const depth = (key: string): number => {
    let current = fn.loanParents.get(key)
    const seen = new Set<string>([key])
    let result = 0
    while (current !== undefined && !seen.has(current)) {
      seen.add(current)
      result += 1
      current = fn.loanParents.get(current)
    }
    return result
  }
  return Object.freeze(
    [...endings].sort((left, right) => {
      const leftKey = borrowKey(left.borrow)
      const rightKey = borrowKey(right.borrow)
      return depth(rightKey) - depth(leftKey) || leftKey.localeCompare(rightKey)
    }),
  )
}

export const propagationLoanEnds = (
  fn: FunctionLowering,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Mir.EndLoanOperation> =>
  dependencyOrderedLoanEndings(
    fn,
    [...fn.loanLocals.entries()].flatMap(([key, slice]): ReadonlyArray<Mir.EndLoanOperation> => {
      const borrow = fn.loanIds.get(key)
      return borrow === undefined
        ? []
        : [
            Object.freeze({
              _tag: 'EndLoan' as const,
              borrow,
              slice,
              provenance: generated(span),
            }),
          ]
    }),
  )

export const callableLocalCleanup = (
  fn: FunctionLowering,
  localType: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>,
): CleanupPlan.CleanupPlan => {
  const environment = localType.environment
  if (environment === undefined || localType.site === undefined)
    return Object.freeze({ _tag: 'NoCleanup', type: localType.type })
  return Object.freeze({
    _tag: 'CallableCleanup',
    type: localType.type,
    environment: Object.freeze({
      _tag: 'CallableEnvironmentIdentity',
      identity: Instances.callableEnvironmentIdentity(environment.callable),
    }),
    slots: Object.freeze(
      Ownership.inReleaseOrder(environment.fields).flatMap((field) =>
        field.access === 'Take' && !Mir.isCopy(fn.layout, field.type)
          ? [Object.freeze({ ordinal: field.ordinal, cleanup: concreteCleanup(fn, field.type) })]
          : [],
      ),
    ),
  })
}

export const emitReleases = (fn: FunctionLowering, exit: Ownership.ExitPlan | undefined): void => {
  for (const borrow of exit?.loanEnds ?? []) {
    const slice = fn.loanLocals.get(borrowKey(borrow))
    if (slice === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'EndLoan',
        borrow,
        slice,
        provenance: generated(exit?.span ?? borrow.callSpan),
      }),
    )
    fn.loanLocals.delete(borrowKey(borrow))
  }
  if (exit?.kind === 'Return') {
    endLoans(
      fn,
      (fn.ownership?.loans ?? []).map((loan) => loan.id),
      exit.span,
    )
  }
  for (const release of exit?.releases ?? []) {
    if (release.binding.site._tag !== 'Let') continue
    const ordinal = release.binding.site.binding.ordinal
    endLoans(fn, fn.effectLoanEnds.get(ordinal) ?? [], exit?.span ?? release.binding.liveTo)
    fn.effectLoanEnds.delete(ordinal)
  }
  for (const release of exit?.releases ?? []) {
    const site = release.binding.site
    let dropped: Mir.LocalId | undefined
    if (site._tag === 'Parameter') {
      dropped = fn.parameterLocals.get(site.parameter.ordinal)
    } else if (site._tag === 'Temporary') {
      dropped = undefined
    } else if (site._tag === 'Pattern') {
      dropped = fn.patternLocals.get(patternKey(site.binding))
    } else {
      dropped = fn.bindingLocals.get(site.binding.ordinal)
    }
    if (dropped === undefined) continue
    const localType = fn.localTypes.at(dropped.ordinal)
    if (localType === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'Drop',
        local: dropped,
        cleanup: cleanupForLocal(fn, release.cleanup, localType),
        provenance: Object.freeze({ span: release.binding.liveFrom, generated: true }),
      }),
    )
  }
}

export const effectContract = (type: Type.Type): Type.Effect | undefined => {
  const contract = Type.isRepresented(type) ? type.contract : type
  return Type.isEffect(contract) ? contract : undefined
}

export const ownerFields = (
  ownerLoop: Mir.LoopId | undefined,
): { readonly ownerLoop?: Mir.LoopId } => (ownerLoop === undefined ? {} : { ownerLoop })

export const generated = (span: SourceSpan.SourceSpan): Mir.Provenance =>
  Object.freeze({ span, generated: true })

export const authored = (span: SourceSpan.SourceSpan): Mir.Provenance =>
  Object.freeze({ span, generated: false })

export const lowerWriteSelectors = (
  fn: FunctionLowering,
  selectors: ReadonlyArray<Hir.WriteSelector>,
): ReadonlyArray<Mir.PlaceSelector> | undefined => {
  const lowered: Array<Mir.PlaceSelector> = []
  for (const selector of selectors) {
    if (selector._tag === 'Field') {
      lowered.push(
        Object.freeze({
          _tag: 'FieldSelector',
          field: selector.field,
          provenance: authored(selector.span),
        }),
      )
      continue
    }
    const index =
      selector.bounds._tag === 'Proven'
        ? Object.freeze({ _tag: 'Proven' as const, value: selector.bounds.index })
        : (() => {
            const expression = lowerExpression(fn, selector.index)
            return expression === undefined
              ? undefined
              : Object.freeze({ _tag: 'Runtime' as const, local: expression.result })
          })()
    if (index === undefined) return undefined
    lowered.push(
      Object.freeze({
        _tag: 'ElementSelector',
        length: selector.array.length,
        index,
        provenance: authored(selector.span),
      }),
    )
  }
  return Object.freeze(lowered)
}

export const lowerBorrowSelectors = (
  fn: FunctionLowering,
  selectors: ReadonlyArray<Hir.BorrowSelector>,
): ReadonlyArray<Mir.PlaceSelector> | undefined => {
  const lowered: Array<Mir.PlaceSelector> = []
  for (const selector of selectors) {
    if (selector._tag === 'Field') {
      lowered.push(
        Object.freeze({
          _tag: 'FieldSelector',
          field: selector.field,
          provenance: authored(selector.span),
        }),
      )
      continue
    }
    if (selector._tag === 'SliceIndex') {
      const index = lowerExpression(fn, selector.index)
      if (index === undefined) return undefined
      lowered.push(
        Object.freeze({
          _tag: 'SliceElementSelector',
          index: index.result,
          access: selector.slice.access,
          provenance: authored(selector.span),
        }),
      )
      continue
    }
    const index =
      selector.bounds._tag === 'Proven'
        ? Object.freeze({ _tag: 'Proven' as const, value: selector.bounds.index })
        : (() => {
            const expression = lowerExpression(fn, selector.index)
            return expression === undefined
              ? undefined
              : Object.freeze({ _tag: 'Runtime' as const, local: expression.result })
          })()
    if (index === undefined) return undefined
    lowered.push(
      Object.freeze({
        _tag: 'ElementSelector',
        length: selector.array.length,
        index,
        provenance: authored(selector.span),
      }),
    )
  }
  return Object.freeze(lowered)
}

export const lowerBorrowedWriteSelectors = (
  fn: FunctionLowering,
  selectors: ReadonlyArray<Hir.BorrowedWriteSelector>,
): ReadonlyArray<Mir.PlaceSelector> | undefined => {
  const lowered: Array<Mir.PlaceSelector> = []
  for (const selector of selectors) {
    if (selector._tag === 'Field') {
      lowered.push(
        Object.freeze({
          _tag: 'FieldSelector',
          field: selector.field,
          provenance: authored(selector.span),
        }),
      )
      continue
    }
    const index = lowerExpression(fn, selector.index)
    if (index === undefined) return undefined
    lowered.push(
      Object.freeze({
        _tag: 'SliceElementSelector',
        index: index.result,
        access: selector.slice.access,
        provenance: authored(selector.span),
      }),
    )
  }
  return Object.freeze(lowered)
}

export const withoutLoanEndings = (
  operations: ReadonlyArray<Mir.Operation>,
  loans: ReadonlySet<string>,
): ReadonlyArray<Mir.Operation> =>
  Object.freeze(
    operations.flatMap((operation): ReadonlyArray<Mir.Operation> => {
      if (operation._tag === 'EndLoan' && loans.has(borrowKey(operation.borrow))) return []
      if (operation._tag === 'ShortCircuit')
        return [
          Object.freeze({
            ...operation,
            right: Object.freeze({
              ...operation.right,
              operations: withoutLoanEndings(operation.right.operations, loans),
            }),
          }),
        ]
      if (operation._tag === 'Match')
        return [
          Object.freeze({
            ...operation,
            arms: Object.freeze(
              operation.arms.map((arm) =>
                Object.freeze({
                  ...arm,
                  ...(arm.guard === undefined
                    ? {}
                    : {
                        guard: Object.freeze({
                          ...arm.guard,
                          operations: withoutLoanEndings(arm.guard.operations, loans),
                        }),
                      }),
                  selected: Object.freeze({
                    ...arm.selected,
                    operations: withoutLoanEndings(arm.selected.operations, loans),
                  }),
                }),
              ),
            ),
          }),
        ]
      return [operation]
    }),
  )

export interface DelayedLoopLoan {
  readonly key: string
  readonly borrow: Hir.BorrowId
  readonly slice: Mir.LocalId
}

export const delayedLoopLoans = (
  fn: FunctionLowering,
  keys: ReadonlySet<string>,
  entry: DelayedEffectState,
): ReadonlyArray<DelayedLoopLoan> => {
  return Object.freeze(
    [...keys].flatMap((key): ReadonlyArray<DelayedLoopLoan> => {
      const borrow = fn.loanIds.get(key)
      const slice = entry.loanLocals.get(key)
      return borrow === undefined || slice === undefined
        ? []
        : [Object.freeze({ key, borrow, slice })]
    }),
  )
}

export const terminalLoopLoanEndings = (
  loans: ReadonlyArray<DelayedLoopLoan>,
  outcome: Extract<Mir.Outcome, { readonly _tag: 'Return' | 'Trap' }>,
): ReadonlyArray<Extract<Mir.Operation, { readonly _tag: 'EndLoan' }>> =>
  Object.freeze(
    loans.map((loan) =>
      Object.freeze({
        _tag: 'EndLoan' as const,
        borrow: loan.borrow,
        slice: loan.slice,
        provenance: generated(outcome.provenance.span),
      }),
    ),
  )

/** Adds loop-entry loans to failure exits after their authored success endings move past the loop. */
export const withDelayedFailureLoanEndings = (
  fn: FunctionLowering,
  operations: ReadonlyArray<Mir.Operation>,
  loans: ReadonlyArray<DelayedLoopLoan>,
): ReadonlyArray<Mir.Operation> =>
  Object.freeze(
    operations.map((operation): Mir.Operation => {
      if (operation._tag === 'ShortCircuit')
        return Object.freeze({
          ...operation,
          right: Object.freeze({
            ...operation.right,
            operations: withDelayedFailureLoanEndings(fn, operation.right.operations, loans),
          }),
        })
      if (operation._tag === 'Match')
        return Object.freeze({
          ...operation,
          arms: Object.freeze(
            operation.arms.map((arm) =>
              Object.freeze({
                ...arm,
                ...(arm.guard === undefined
                  ? {}
                  : {
                      guard: Object.freeze({
                        ...arm.guard,
                        operations: withDelayedFailureLoanEndings(fn, arm.guard.operations, loans),
                      }),
                    }),
                selected: Object.freeze({
                  ...arm.selected,
                  operations: withDelayedFailureLoanEndings(fn, arm.selected.operations, loans),
                }),
              }),
            ),
          ),
        })
      if (
        (operation._tag !== 'RunEffect' &&
          operation._tag !== 'RunEffectValue' &&
          operation._tag !== 'RunStaticEffect') ||
        Type.failureMembers(operation.outcomeType.type).length === 0
      )
        return operation
      const existing = new Set(
        (operation.failureLoanEnds ?? []).map((ending) => borrowKey(ending.borrow)),
      )
      const appended = loans.flatMap(
        (loan): ReadonlyArray<Mir.EndLoanOperation> =>
          existing.has(loan.key)
            ? []
            : [
                Object.freeze({
                  _tag: 'EndLoan' as const,
                  borrow: loan.borrow,
                  slice: loan.slice,
                  provenance: generated(operation.provenance.span),
                }),
              ],
      )
      return appended.length === 0
        ? operation
        : Object.freeze({
            ...operation,
            failureLoanEnds: dependencyOrderedLoanEndings(fn, [
              ...(operation.failureLoanEnds ?? []),
              ...appended,
            ]),
          })
    }),
  )
