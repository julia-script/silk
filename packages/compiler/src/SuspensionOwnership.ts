import * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as ExecutionAffinity from './ExecutionAffinity.js'
import type * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as SetOf from './internal/SetOf.js'
import * as Layout from './Layout.js'
import * as LocalSharedOwnership from './LocalSharedOwnership.js'
import * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import * as Ownership from './Ownership.js'
import * as ProvisionalMir from './ProvisionalMir.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/** Post-normalization MIR-local ownership facts for provisional complete-or-relay control. */

export type BorrowIdentity =
  | { readonly _tag: 'MirLoan'; readonly borrow: Hir.BorrowId }
  | { readonly _tag: 'BorrowedParameter'; readonly parameterOrdinal: number }
  | { readonly _tag: 'BorrowedLocal'; readonly local: Mir.LocalId }

export type Access =
  | { readonly _tag: 'Copy' }
  | {
      readonly _tag: 'BorrowedDependency'
      readonly access: 'Shared' | 'Exclusive'
      readonly root: Mir.LocalId
      readonly loan: BorrowIdentity
    }
  | { readonly _tag: 'AffineTransfer'; readonly cleanup: CleanupPlan.CleanupPlan }

export interface Slot {
  readonly ordinal: number
  readonly local: Mir.LocalId
  readonly type: Mir.Type
  readonly access: Access
  readonly executionAffinity: ExecutionAffinity.ExecutionAffinity
  readonly localSharedObligations: LocalSharedOwnership.ObligationPlan
}

export interface Release {
  readonly local: Mir.LocalId
  readonly cleanup: CleanupPlan.CleanupPlan
}

export interface ResumePlan {
  readonly restores: ReadonlyArray<number>
  readonly loanEnds: ReadonlyArray<BorrowIdentity>
  readonly releases: ReadonlyArray<Release>
}

export interface Plan {
  readonly _tag: 'SuspensionOwnershipPlan'
  readonly point: ProvisionalMir.ControlId
  readonly function: Instances.InstanceKey
  readonly region: Mir.RegionId
  readonly span: SourceSpan.SourceSpan
  readonly frame: 'StatefulRelay'
  readonly slots: ReadonlyArray<Slot>
  readonly success: ResumePlan
  readonly failure: ResumePlan
}

export interface Violation {
  readonly _tag: 'SuspensionOwnershipViolation'
  readonly function: Instances.InstanceKey
  readonly span: SourceSpan.SourceSpan
  readonly detail: string
}

export interface Module {
  readonly _tag: 'SuspensionOwnershipModule'
  readonly module: string
  readonly plans: ReadonlyArray<Plan>
  readonly violations: ReadonlyArray<Violation>
}

const operationDefinitions = (operation: Mir.Operation): ReadonlySet<number> => {
  const definitions = new Set<number>()
  for (const nested of Mir.operationTree(operation)) {
    if ('destination' in nested) definitions.add(nested.destination.ordinal)
    if (
      nested._tag === 'RunEffect' ||
      nested._tag === 'RunEffectValue' ||
      nested._tag === 'RunStaticEffect' ||
      nested._tag === 'ReifyEffect' ||
      nested._tag === 'CloseEffectEntry'
    )
      definitions.add(nested.outcome.ordinal)
    if (nested._tag === 'Match')
      for (const arm of nested.arms)
        for (const binding of arm.bindings) definitions.add(binding.destination.ordinal)
  }
  return definitions
}

const operationInputs = (operation: Mir.Operation): ReadonlySet<number> => {
  const definitions = operationDefinitions(operation)
  return new Set(
    Mir.operationTree(operation)
      .flatMap((nested) =>
        nested._tag === 'Drop' && nested.cleanup._tag === 'NoCleanup'
          ? []
          : MirVerification.operationLocals(nested),
      )
      .map((local) => local.ordinal)
      .filter((local) => !definitions.has(local)),
  )
}

const transferOperation = (
  operation: Mir.Operation,
  liveAfter: ReadonlySet<number>,
): Set<number> => {
  const definitions = operationDefinitions(operation)
  return SetOf.union(
    operationInputs(operation),
    new Set([...liveAfter].filter((local) => !definitions.has(local))),
  )
}

const outcomeUses = (region: Mir.Region): ReadonlySet<number> => {
  if (region._tag === 'ConditionalRegion') return new Set([region.condition.ordinal])
  if (region._tag === 'LoopRegion') return new Set([region.conditionValue.ordinal])
  return region.outcome._tag === 'Return' ? new Set([region.outcome.value.ordinal]) : new Set()
}

const regionOperations = (region: Mir.Region): ReadonlyArray<Mir.Operation> =>
  region._tag === 'OperationRegion'
    ? region.operations
    : region._tag === 'CleanupRegion'
      ? region.releases
      : []

const transferSequence = (
  operations: ReadonlyArray<Mir.Operation>,
  liveAfter: ReadonlySet<number>,
): Set<number> => {
  let live = new Set(liveAfter)
  for (let ordinal = operations.length - 1; ordinal >= 0; ordinal -= 1) {
    const operation = operations.at(ordinal)
    if (operation !== undefined) live = transferOperation(operation, live)
  }
  return live
}

const liveness = (fn: Mir.MirFunction): ReadonlyMap<Mir.Operation, ReadonlySet<number>> => {
  const edges = Mir.controlEdges(fn)
  const liveIn = new Map<number, Set<number>>(
    fn.regions.map((region) => [region.id.ordinal, new Set()]),
  )
  let changed = true
  while (changed) {
    changed = false
    for (const region of [...fn.regions].reverse()) {
      const successors = edges
        .filter((edge) => edge.from.ordinal === region.id.ordinal)
        .flatMap((edge) => [...(liveIn.get(edge.to.ordinal) ?? [])])
      const before = transferSequence(
        regionOperations(region),
        SetOf.union(new Set(successors), outcomeUses(region)),
      )
      const current = liveIn.get(region.id.ordinal) ?? new Set()
      if (!SetOf.equal(before, current)) {
        liveIn.set(region.id.ordinal, before)
        changed = true
      }
    }
  }

  const liveAfter = new Map<Mir.Operation, ReadonlySet<number>>()
  const analyzeSequence = (
    operations: ReadonlyArray<Mir.Operation>,
    following: ReadonlySet<number>,
  ): Set<number> => {
    let live = new Set(following)
    for (let ordinal = operations.length - 1; ordinal >= 0; ordinal -= 1) {
      const operation = operations.at(ordinal)
      if (operation === undefined) continue
      liveAfter.set(operation, new Set(live))
      if (operation._tag === 'Match') {
        const outer = new Set(
          [...live].filter((local) => !operationDefinitions(operation).has(local)),
        )
        for (const arm of operation.arms) {
          const selected = analyzeSequence(
            arm.selected.operations,
            SetOf.union(outer, new Set([arm.selected.result.ordinal])),
          )
          if (arm.guard !== undefined)
            analyzeSequence(
              arm.guard.operations,
              SetOf.union(selected, new Set([arm.guard.result.ordinal])),
            )
        }
      } else if (operation._tag === 'ShortCircuit') {
        const outer = new Set(
          [...live].filter((local) => !operationDefinitions(operation).has(local)),
        )
        analyzeSequence(
          operation.right.operations,
          SetOf.union(outer, new Set([operation.right.result.ordinal])),
        )
      }
      live = transferOperation(operation, live)
    }
    return live
  }

  for (const region of fn.regions) {
    const successors = edges
      .filter((edge) => edge.from.ordinal === region.id.ordinal)
      .flatMap((edge) => [...(liveIn.get(edge.to.ordinal) ?? [])])
    analyzeSequence(regionOperations(region), SetOf.union(new Set(successors), outcomeUses(region)))
  }
  return liveAfter
}

const definitionMap = (fn: Mir.MirFunction): ReadonlyMap<number, Mir.Operation> =>
  new Map(
    MirVerification.operations(fn).flatMap((operation) =>
      'destination' in operation ? [[operation.destination.ordinal, operation] as const] : [],
    ),
  )

const borrowOf = (
  fn: Mir.MirFunction,
  definitions: ReadonlyMap<number, Mir.Operation>,
  local: Mir.LocalId,
  seen = new Set<number>(),
): Extract<Access, { readonly _tag: 'BorrowedDependency' }> => {
  const type = fn.localTypes.at(local.ordinal)
  const access =
    type?._tag === 'EffectBorrow'
      ? type.access
      : type?._tag === 'Reference' || type?._tag === 'Slice'
        ? type.type.access
        : 'Shared'
  if (seen.has(local.ordinal))
    return Object.freeze({
      _tag: 'BorrowedDependency',
      access,
      root: local,
      loan: Object.freeze({ _tag: 'BorrowedLocal', local }),
    })
  const next = new Set(seen).add(local.ordinal)
  const definition = definitions.get(local.ordinal)
  if (definition?._tag === 'BeginLoan') {
    const parent = borrowOf(fn, definitions, definition.root, next)
    return Object.freeze({
      _tag: 'BorrowedDependency',
      access: definition.access,
      root: parent.root,
      loan: Object.freeze({ _tag: 'MirLoan', borrow: definition.borrow }),
    })
  }
  if (
    definition?._tag === 'Move' ||
    definition?._tag === 'ConvertUnion' ||
    definition?._tag === 'Project'
  )
    return borrowOf(fn, definitions, definition.source, next)
  if (local.ordinal < fn.parameterCount)
    return Object.freeze({
      _tag: 'BorrowedDependency',
      access,
      root: local,
      loan: Object.freeze({ _tag: 'BorrowedParameter', parameterOrdinal: local.ordinal }),
    })
  return Object.freeze({
    _tag: 'BorrowedDependency',
    access,
    root: local,
    loan: Object.freeze({ _tag: 'BorrowedLocal', local }),
  })
}

const releasesOf = (operation: Mir.Operation): ReadonlyArray<Release> =>
  operation._tag === 'RunEffect' ||
  operation._tag === 'RunEffectValue' ||
  operation._tag === 'RunStaticEffect'
    ? Object.freeze(
        (operation.releases ?? []).map((release) =>
          Object.freeze({ local: release.local, cleanup: release.cleanup }),
        ),
      )
    : Object.freeze([])

const accessOf = (
  program: Mir.Module,
  index: DeclarationIndex.Index,
  fn: Mir.MirFunction,
  definitions: ReadonlyMap<number, Mir.Operation>,
  local: Mir.LocalId,
  type: Mir.Type,
): Access =>
  type._tag === 'Reference' || type._tag === 'Slice' || type._tag === 'EffectBorrow'
    ? borrowOf(fn, definitions, local)
    : Mir.isCopy(program.layout, Mir.semanticType(type))
      ? Object.freeze({ _tag: 'Copy' })
      : Object.freeze({
          _tag: 'AffineTransfer',
          cleanup: CleanupPlan.cleanupPlan(index, Mir.semanticType(type)),
        })

const affinityOf = (
  index: DeclarationIndex.Index,
  fn: Mir.MirFunction,
  type: Mir.Type,
  access: Access,
): ExecutionAffinity.ExecutionAffinity => {
  const retained =
    type._tag === 'EffectValue'
      ? ExecutionAffinity.ofEnvironment(
          index,
          type.environment.fields.map((field) => Object.freeze({ type: field.type })),
        )
      : type._tag === 'CallableValue' && type.environment !== undefined
        ? ExecutionAffinity.ofEnvironment(
            index,
            type.environment.fields.map((field) => Object.freeze({ type: field.type })),
          )
        : ExecutionAffinity.ofType(index, Mir.semanticType(type))
  if (access._tag !== 'BorrowedDependency') return retained
  const root = fn.localTypes.at(access.root.ordinal)
  return root === undefined
    ? retained
    : ExecutionAffinity.join([retained, ExecutionAffinity.ofType(index, Mir.semanticType(root))])
}

const obligationsOf = (
  index: DeclarationIndex.Index,
  type: Mir.Type,
): LocalSharedOwnership.ObligationPlan =>
  type._tag === 'EffectValue'
    ? LocalSharedOwnership.ofEnvironment(
        index,
        type.environment.fields.map((field) =>
          Object.freeze({ access: field.access, type: field.type }),
        ),
      )
    : type._tag === 'CallableValue' && type.environment !== undefined
      ? LocalSharedOwnership.ofEnvironment(
          index,
          type.environment.fields.map((field) =>
            Object.freeze({ access: field.access, type: field.type }),
          ),
        )
      : LocalSharedOwnership.ofType(index, Mir.semanticType(type))

const planFor = (
  program: Mir.Module,
  index: DeclarationIndex.Index,
  fn: Mir.MirFunction,
  region: Mir.Region,
  operation: Mir.Operation,
  live: ReadonlySet<number>,
  control: ProvisionalMir.RunControl,
): Plan => {
  const definitions = definitionMap(fn)
  const operationDefined = operationDefinitions(operation)
  const slots = Object.freeze(
    [...new Set(live)]
      .filter((ordinal) => !operationDefined.has(ordinal))
      .sort((left, right) => left - right)
      .flatMap((ordinal) => {
        const type = fn.localTypes.at(ordinal)
        if (type === undefined) return []
        const local = Object.freeze({ _tag: 'Local' as const, ordinal })
        const access = accessOf(program, index, fn, definitions, local, type)
        const executionAffinity = affinityOf(index, fn, type, access)
        const localSharedObligations = obligationsOf(index, type)
        const runtimeLanes =
          type._tag === 'EffectValue'
            ? Layout.effectEnvironmentLanes(program.layout, type.environment)
            : type._tag === 'CallableValue'
              ? type.environment === undefined
                ? []
                : Layout.callableEnvironmentLanes(program.layout, type.environment)
              : (Layout.callingShape(program.layout, Mir.semanticType(type))?.lanes ?? [])
        if (
          runtimeLanes.length === 0 &&
          executionAffinity._tag === 'Unrestricted' &&
          localSharedObligations._tag === 'NoLocalSharedObligation'
        )
          return []
        return [
          Object.freeze({
            ordinal: 0,
            local,
            type,
            access,
            executionAffinity,
            localSharedObligations,
          }),
        ]
      })
      .map((slot, ordinal) => Object.freeze({ ...slot, ordinal })),
  )
  const releases = releasesOf(operation)
  const borrowed = slots.filter(
    (
      slot,
    ): slot is Slot & {
      readonly access: Extract<Access, { readonly _tag: 'BorrowedDependency' }>
    } => slot.access._tag === 'BorrowedDependency',
  )
  const affine = slots.filter(
    (
      slot,
    ): slot is Slot & { readonly access: Extract<Access, { readonly _tag: 'AffineTransfer' }> } =>
      slot.access._tag === 'AffineTransfer',
  )
  const affineReleases = affine
    .filter((slot) => !releases.some((release) => release.local.ordinal === slot.local.ordinal))
    .map((slot) =>
      Object.freeze({
        local: slot.local,
        cleanup: slot.access.cleanup,
        ordinal: slot.local.ordinal,
      }),
    )
  const releaseOrder = Object.freeze([...releases, ...Ownership.inReleaseOrder(affineReleases)])
  const loanEnds = Object.freeze(Ownership.inReleaseOrder(borrowed).map((slot) => slot.access.loan))
  return Object.freeze({
    _tag: 'SuspensionOwnershipPlan',
    point: control.id,
    function: fn.instance,
    region: region.id,
    span: operation.provenance.span,
    // Every suspendable invocation owns one frame. Header-only states are retained because even a
    // final `run` may adapt the child's represented Effect outcome to the caller's result shape.
    frame: 'StatefulRelay',
    slots,
    success: Object.freeze({
      restores: Object.freeze(slots.map((slot) => slot.ordinal)),
      loanEnds: Object.freeze([]),
      releases: Object.freeze([]),
    }),
    failure: Object.freeze({ restores: Object.freeze([]), loanEnds, releases: releaseOrder }),
  })
}

const comparePlan = (left: Plan, right: Plan): number =>
  Instances.keyText(left.function).localeCompare(Instances.keyText(right.function)) ||
  left.span.sourceId.localeCompare(right.span.sourceId) ||
  left.span.start - right.span.start ||
  left.point.ordinal - right.point.ordinal

/** Plans exact post-normalization local ownership for every matched complete-or-relay control. */
export const plan = (
  program: Mir.Module,
  provisional: ProvisionalMir.Module,
  index: DeclarationIndex.Index,
): Module => {
  const plans: Array<Plan> = []
  const violations: Array<Violation> = []
  for (const fn of program.functions) {
    const live = liveness(fn)
    for (const region of fn.regions) {
      for (const operation of regionOperations(region).flatMap(Mir.operationTree)) {
        if (
          operation._tag !== 'RunEffect' &&
          operation._tag !== 'RunEffectValue' &&
          operation._tag !== 'ReifyEffect'
        )
          continue
        if (
          ProvisionalMir.classificationOfRun(
            provisional,
            fn.instance,
            operation.provenance.span,
          ) === 'Synchronous'
        )
          continue
        const control = ProvisionalMir.controlOfRun(
          provisional,
          fn.instance,
          operation.provenance.span,
        )
        if (control === undefined) {
          violations.push(
            Object.freeze({
              _tag: 'SuspensionOwnershipViolation',
              function: fn.instance,
              span: operation.provenance.span,
              detail: 'suspendable MIR run has no exact provisional control',
            }),
          )
          continue
        }
        plans.push(
          planFor(program, index, fn, region, operation, live.get(operation) ?? new Set(), control),
        )
      }
    }
  }
  return Object.freeze({
    _tag: 'SuspensionOwnershipModule',
    module: program.module,
    plans: Object.freeze(plans.sort(comparePlan)),
    violations: Object.freeze(violations),
  })
}

const borrowText = (borrow: BorrowIdentity): string =>
  borrow._tag === 'MirLoan'
    ? `loan:${borrow.borrow.function.sourceId}:${borrow.borrow.callSpan.start}:${borrow.borrow.ordinal}`
    : borrow._tag === 'BorrowedParameter'
      ? `parameter:${borrow.parameterOrdinal}`
      : `local:${borrow.local.ordinal}`

/** Deterministic inspection encoding used by fresh-lowering tests. */
export const encode = (self: Module): string =>
  [
    `suspension-ownership ${self.module}`,
    ...self.plans.flatMap((plan_) => [
      `plan ${Instances.keyText(plan_.function)}@${plan_.span.sourceId}:${plan_.span.start}:${plan_.point.ordinal} frame=${plan_.frame.toLowerCase()}`,
      ...plan_.slots.map((slot) =>
        slot.access._tag === 'Copy'
          ? `  slot ${slot.ordinal} %${slot.local.ordinal} copy ${Type.encode(Mir.semanticType(slot.type))} affinity=${ExecutionAffinity.encode(slot.executionAffinity)} obligations=${LocalSharedOwnership.encode(slot.localSharedObligations)}`
          : slot.access._tag === 'BorrowedDependency'
            ? `  slot ${slot.ordinal} %${slot.local.ordinal} borrow:${slot.access.access.toLowerCase()} root=%${slot.access.root.ordinal} ${borrowText(slot.access.loan)} ${Type.encode(Mir.semanticType(slot.type))} affinity=${ExecutionAffinity.encode(slot.executionAffinity)} obligations=${LocalSharedOwnership.encode(slot.localSharedObligations)}`
            : `  slot ${slot.ordinal} %${slot.local.ordinal} move:${slot.access.cleanup._tag} ${Type.encode(Mir.semanticType(slot.type))} affinity=${ExecutionAffinity.encode(slot.executionAffinity)} obligations=${LocalSharedOwnership.encode(slot.localSharedObligations)}`,
      ),
    ]),
    ...self.violations.map(
      (violation) =>
        `violation ${Instances.keyText(violation.function)}@${violation.span.sourceId}:${violation.span.start} ${violation.detail}`,
    ),
    '',
  ].join('\n')
