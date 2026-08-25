import type { ControlProvenance } from './Backend.js'
import type * as Layout from './Layout.js'
import * as Match from './Match.js'
import * as Mir from './Mir.js'
import type * as SilkType from './Type.js'

export type LinearTerminator =
  | { readonly _tag: 'Return'; readonly value: Mir.LocalId; readonly provenance: Mir.Provenance }
  | Extract<Mir.Operation, { readonly _tag: 'PropagateEffectFailure' }>
  | { readonly _tag: 'Jump'; readonly target: Mir.RegionId; readonly provenance: Mir.Provenance }
  | {
      readonly _tag: 'Branch'
      readonly condition: Mir.LocalId
      readonly taken: Mir.RegionId
      readonly otherwise: Mir.RegionId
      readonly provenance: Mir.Provenance
    }
  | {
      readonly _tag: 'MatchBranch'
      readonly scrutinee: Mir.LocalId
      readonly memberOrdinal: number
      readonly taken: Mir.RegionId
      readonly otherwise: Mir.RegionId
      readonly provenance: Mir.Provenance
    }
  | {
      readonly _tag: 'EnumMatchBranch'
      readonly scrutinee: Mir.LocalId
      readonly discriminant: bigint
      readonly type: Extract<Mir.Type, { readonly _tag: 'Enum' }>
      readonly representation: Extract<Layout.Representation, { readonly _tag: 'ScalarEnum' }>
      readonly taken: Mir.RegionId
      readonly otherwise: Mir.RegionId
      readonly provenance: Mir.Provenance
    }
  | { readonly _tag: 'Trap'; readonly reason: string; readonly provenance: Mir.Provenance }

export type LinearOperation =
  | Exclude<Mir.Operation, { readonly _tag: 'Match' | 'ShortCircuit' | 'PropagateEffectFailure' }>
  | {
      readonly _tag: 'BindMatch'
      readonly scrutinee: Mir.LocalId
      readonly shape: Layout.CallingShape
      readonly member: SilkType.Type
      readonly binding: Mir.MatchBinding
      readonly provenance: Mir.Provenance
    }

export const isLinearOperation = (
  operation: Mir.Operation | LinearOperation,
): operation is LinearOperation =>
  operation._tag !== 'Match' &&
  operation._tag !== 'ShortCircuit' &&
  operation._tag !== 'PropagateEffectFailure'

export const linearOperations = (
  operations: ReadonlyArray<Mir.Operation | LinearOperation>,
): ReadonlyArray<LinearOperation> => {
  const linear = operations.filter(isLinearOperation)
  if (linear.length !== operations.length)
    throw new RangeError('LLVM control expansion retained a structured operation')
  return Object.freeze(linear)
}

export interface LinearBlock {
  readonly id: Mir.RegionId
  readonly origin: Mir.RegionId
  readonly kind: 'Normal' | 'Cleanup'
  readonly operations: ReadonlyArray<LinearOperation>
  readonly terminator: LinearTerminator
}

export interface StructuredBlock {
  readonly id: Mir.RegionId
  readonly origin: Mir.RegionId
  readonly kind: 'Normal' | 'Cleanup'
  readonly operations: ReadonlyArray<Mir.Operation>
  readonly terminator: LinearTerminator
}

export const destinationOf = (operation: LinearOperation): Mir.LocalId | undefined => {
  switch (operation._tag) {
    case 'Literal':
    case 'EnumConstant':
    case 'EnumValue':
    case 'EnumEquality':
    case 'StaticString':
    case 'StringFromUtf8Unchecked':
    case 'StringUtf8Bytes':
    case 'StringByteLength':
    case 'StringEqualsExact':
    case 'Binary':
    case 'ConvertInteger':
    case 'CheckedScalar':
    case 'Move':
    case 'BeginLoan':
    case 'SliceLength':
    case 'ConvertUnion':
    case 'Call':
    case 'MakeEffect':
    case 'MakeCallable':
    case 'ApplyCallable':
    case 'PackEffectOutcome':
    case 'PackEffectFailureUnion':
    case 'UnpackEffectSuccess':
    case 'RunEffect':
    case 'RunEffectValue':
    case 'RunStaticEffect':
    case 'ReifyEffect':
    case 'CloseEffectEntry':
    case 'Construct':
    case 'ConstructArray':
    case 'Project':
    case 'ReadPlace':
    case 'ValidateLayout':
    case 'RepeatLayout':
    case 'Allocate':
    case 'HostWrite':
    case 'OsCall':
    case 'RawBufferFrom':
    case 'SharedFromAllocation':
    case 'ExecutionFromAllocation':
    case 'ExecutionDrive':
    case 'ExecutionWake':
    case 'ExecutionPark':
    case 'SharedClone':
    case 'SharedWithMut':
    case 'RawBufferCount':
    case 'RawBufferSlot':
    case 'RawBufferRead':
    case 'RawBufferView':
    case 'RawBufferCopy':
    case 'RawBufferFill':
    case 'SlotWrite':
    case 'SlotTake':
    case 'SlotCopy':
    case 'SlotDrop':
      return operation.destination
    case 'BindMatch':
      return operation.binding.destination
    case 'CheckPlace':
    case 'WritePlace':
    case 'EndLoan':
    case 'Drop':
      return undefined
  }
}

export const opensRuntimeContinuation = (operation: LinearOperation): boolean =>
  operation._tag === 'Allocate' ||
  operation._tag === 'HostWrite' ||
  operation._tag === 'OsCall' ||
  operation._tag === 'RawBufferFrom' ||
  operation._tag === 'SharedFromAllocation' ||
  operation._tag === 'ExecutionFromAllocation' ||
  operation._tag === 'ExecutionDrive' ||
  operation._tag === 'ExecutionWake' ||
  operation._tag === 'ExecutionPark' ||
  operation._tag === 'SharedClone' ||
  operation._tag === 'SharedWithMut' ||
  operation._tag === 'RawBufferSlot' ||
  operation._tag === 'RawBufferRead' ||
  operation._tag === 'RawBufferView' ||
  operation._tag === 'RawBufferCopy' ||
  operation._tag === 'RawBufferFill' ||
  operation._tag === 'RunEffect' ||
  operation._tag === 'RunEffectValue' ||
  operation._tag === 'RunStaticEffect' ||
  operation._tag === 'ReifyEffect' ||
  operation._tag === 'CloseEffectEntry' ||
  (operation._tag === 'Binary' &&
    operation.operator !== 'Equals' &&
    operation.operator !== 'NotEquals' &&
    operation.operator !== 'LessThan' &&
    operation.operator !== 'LessOrEqual' &&
    operation.operator !== 'GreaterThan' &&
    operation.operator !== 'GreaterOrEqual') ||
  ((operation._tag === 'ReadPlace' ||
    operation._tag === 'CheckPlace' ||
    operation._tag === 'WritePlace') &&
    operation.selectors.some(
      (selector) =>
        selector._tag === 'SliceElementSelector' ||
        (selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'),
    ))

export const expandMatches = (
  fn: Mir.MirFunction,
  input: ReadonlyArray<StructuredBlock>,
): ReadonlyArray<LinearBlock> => {
  let nextRegion = Math.max(-1, ...fn.regions.map((region) => region.id.ordinal)) + 1
  const reserve = (): Mir.RegionId => Object.freeze({ _tag: 'Region', ordinal: nextRegion++ })
  const blocks: Array<LinearBlock> = []
  const jump = (target: Mir.RegionId, provenance: Mir.Provenance): LinearTerminator =>
    Object.freeze({ _tag: 'Jump', target, provenance })

  const lowerSequence = (
    id: Mir.RegionId,
    origin: Mir.RegionId,
    kind: LinearBlock['kind'],
    operations: ReadonlyArray<Mir.Operation | LinearOperation>,
    terminator: LinearTerminator,
  ): void => {
    const specialIndex = operations.findIndex(
      (operation) =>
        operation._tag === 'Match' ||
        operation._tag === 'ShortCircuit' ||
        operation._tag === 'PropagateEffectFailure',
    )
    if (specialIndex < 0) {
      blocks.push(
        Object.freeze({
          id,
          origin,
          kind,
          operations: linearOperations(operations),
          terminator,
        }),
      )
      return
    }
    const special = operations.at(specialIndex)
    if (special?._tag === 'PropagateEffectFailure') {
      blocks.push(
        Object.freeze({
          id,
          origin,
          kind,
          operations: linearOperations(operations.slice(0, specialIndex)),
          terminator: special,
        }),
      )
      return
    }
    if (special?._tag === 'ShortCircuit') {
      const following = reserve()
      const evaluateRight = reserve()
      const decided = reserve()
      blocks.push(
        Object.freeze({
          id,
          origin,
          kind,
          operations: linearOperations(operations.slice(0, specialIndex)),
          // `&&` reaches its right operand on a true left operand; `||` on a false one. The other
          // edge writes the operator's decided value without evaluating the right operand at all.
          terminator: Object.freeze({
            _tag: 'Branch',
            condition: special.left,
            taken: special.operator === 'And' ? evaluateRight : decided,
            otherwise: special.operator === 'And' ? decided : evaluateRight,
            provenance: special.provenance,
          }),
        }),
      )
      lowerSequence(following, origin, kind, operations.slice(specialIndex + 1), terminator)
      lowerSequence(
        evaluateRight,
        origin,
        'Normal',
        [
          ...special.right.operations,
          Object.freeze({
            _tag: 'Move' as const,
            destination: special.destination,
            source: special.right.result,
            provenance: special.provenance,
          }),
        ],
        jump(following, special.provenance),
      )
      blocks.push(
        Object.freeze({
          id: decided,
          origin,
          kind: 'Normal',
          operations: Object.freeze([
            Object.freeze({
              _tag: 'Literal' as const,
              destination: special.destination,
              type: special.type,
              value: special.operator === 'And' ? 0 : 1,
              provenance: special.provenance,
            }),
          ]),
          terminator: jump(following, special.provenance),
        }),
      )
      return
    }
    if (special?._tag !== 'Match')
      throw new RangeError('LLVM control expansion lost its special operation')
    const match = special
    const dispatch = reserve()
    const following = reserve()
    blocks.push(
      Object.freeze({
        id,
        origin,
        kind,
        operations: linearOperations(operations.slice(0, specialIndex)),
        terminator: jump(dispatch, match.provenance),
      }),
    )
    lowerSequence(following, origin, kind, operations.slice(specialIndex + 1), terminator)

    const trap = reserve()
    blocks.push(
      Object.freeze({
        id: trap,
        origin,
        kind: 'Normal',
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Trap',
          reason: 'exhaustive match rejected every candidate',
          provenance: match.provenance,
        }),
      }),
    )

    const candidateEntry = (
      member: Match.CoverageIdentity,
      candidates: ReadonlyArray<Match.ArmId>,
      ordinal: number,
    ): Mir.RegionId => {
      const entry = reserve()
      const candidate = candidates.at(ordinal)
      if (candidate === undefined) {
        blocks.push(
          Object.freeze({
            id: entry,
            origin,
            kind: 'Normal',
            operations: Object.freeze([]),
            terminator: jump(trap, match.provenance),
          }),
        )
        return entry
      }
      const arm = match.arms.find((item) => item.id.ordinal === candidate.ordinal)
      if (arm === undefined) throw new RangeError('LLVM match expansion lost a candidate arm')
      const bindings: ReadonlyArray<LinearOperation> = Object.freeze(
        arm.bindings.map((binding) =>
          Object.freeze({
            _tag: 'BindMatch' as const,
            scrutinee: match.scrutinee,
            shape: match.scrutineeShape,
            member: Match.sourceType(member),
            binding,
            provenance: binding.provenance,
          }),
        ),
      )
      const selected = reserve()
      lowerSequence(
        selected,
        origin,
        'Normal',
        [
          ...arm.selected.operations,
          Object.freeze({
            _tag: 'Move' as const,
            destination: match.destination,
            source: arm.selected.result,
            provenance: arm.provenance,
          }),
        ],
        jump(following, arm.provenance),
      )
      if (arm.guard === undefined) {
        lowerSequence(entry, origin, 'Normal', bindings, jump(selected, arm.provenance))
      } else {
        const fallback = candidateEntry(member, candidates, ordinal + 1)
        lowerSequence(
          entry,
          origin,
          'Normal',
          [...bindings, ...arm.guard.operations],
          Object.freeze({
            _tag: 'Branch',
            condition: arm.guard.result,
            taken: selected,
            otherwise: fallback,
            provenance: arm.provenance,
          }),
        )
      }
      return entry
    }

    const decisionEntries = match.decisions.map((decision) =>
      candidateEntry(decision.member, decision.candidates, 0),
    )
    if (match.scrutineeType._tag === 'Enum') {
      const enumType = match.scrutineeType
      const dispatchIds = match.decisions.map((_, ordinal) =>
        ordinal === 0 ? dispatch : reserve(),
      )
      match.decisions.forEach((decision, ordinal) => {
        if (decision.member._tag !== 'EnumMember')
          throw new RangeError('Verified scalar enum match lost its member identity')
        const member = decision.member.member
        const declared = enumType.representation.members.find(
          (candidate) =>
            candidate.member.enum.module === member.enum.module &&
            candidate.member.enum.name === member.enum.name &&
            candidate.member.name === member.name,
        )
        if (declared === undefined)
          throw new RangeError('Verified scalar enum match lost its declared discriminant')
        blocks.push(
          Object.freeze({
            id: dispatchIds.at(ordinal) ?? dispatch,
            origin,
            kind: 'Normal',
            operations: Object.freeze([]),
            terminator: Object.freeze({
              _tag: 'EnumMatchBranch',
              scrutinee: match.scrutinee,
              discriminant: declared.discriminant,
              type: enumType,
              representation: enumType.representation,
              taken: decisionEntries.at(ordinal) ?? trap,
              otherwise: dispatchIds.at(ordinal + 1) ?? trap,
              provenance: match.provenance,
            }),
          }),
        )
      })
      return
    }
    if (match.scrutineeType._tag !== 'Union') {
      const selected = decisionEntries.at(0) ?? trap
      blocks.push(
        Object.freeze({
          id: dispatch,
          origin,
          kind: 'Normal',
          operations: Object.freeze([]),
          terminator: jump(selected, match.provenance),
        }),
      )
      return
    }
    const dispatchIds = match.decisions.map((_, ordinal) => (ordinal === 0 ? dispatch : reserve()))
    match.decisions.forEach((_, ordinal) => {
      blocks.push(
        Object.freeze({
          id: dispatchIds.at(ordinal) ?? dispatch,
          origin,
          kind: 'Normal',
          operations: Object.freeze([]),
          terminator: Object.freeze({
            _tag: 'MatchBranch',
            scrutinee: match.scrutinee,
            memberOrdinal: ordinal,
            taken: decisionEntries.at(ordinal) ?? trap,
            otherwise: dispatchIds.at(ordinal + 1) ?? trap,
            provenance: match.provenance,
          }),
        }),
      )
    })
  }

  for (const block of input) {
    lowerSequence(block.id, block.origin, block.kind, block.operations, block.terminator)
  }
  const byId = new Map(blocks.map((block) => [block.id.ordinal, block] as const))
  const visited = new Set<number>()
  const ordered: Array<LinearBlock> = []
  const visit = (id: Mir.RegionId): void => {
    if (visited.has(id.ordinal)) return
    visited.add(id.ordinal)
    const block = byId.get(id.ordinal)
    if (block === undefined) return
    ordered.push(block)
    const terminator = block.terminator
    if (terminator._tag === 'Jump') visit(terminator.target)
    if (
      terminator._tag === 'Branch' ||
      terminator._tag === 'MatchBranch' ||
      terminator._tag === 'EnumMatchBranch'
    ) {
      visit(terminator.taken)
      visit(terminator.otherwise)
    }
  }
  visit(fn.entry)
  for (const block of [...blocks].sort((left, right) => left.id.ordinal - right.id.ordinal)) {
    visit(block.id)
  }
  return Object.freeze(ordered)
}

/** LLVM-private flattening of the compiler-owned DAG. Repeat is the only source of a back-edge. */
export const linearize = (fn: Mir.MirFunction): ReadonlyArray<LinearBlock> => {
  const loops = new Map(
    fn.regions.flatMap((region) =>
      region._tag === 'LoopRegion' ? [[region.loop.ordinal, region] as const] : [],
    ),
  )
  const conditionOwners = new Map(
    fn.regions.flatMap((region) =>
      region._tag === 'LoopRegion' ? [[region.condition.ordinal, region] as const] : [],
    ),
  )
  const outcome = (region: Mir.OperationRegion | Mir.CleanupRegion): LinearTerminator => {
    const value = region.outcome
    switch (value._tag) {
      case 'Forward':
      case 'Return':
      case 'Trap':
        return value._tag === 'Forward'
          ? Object.freeze({ _tag: 'Jump', target: value.target, provenance: value.provenance })
          : value
      case 'Repeat': {
        const loop = loops.get(value.loop.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer lost repeat loop')
        return Object.freeze({ _tag: 'Jump', target: loop.id, provenance: value.provenance })
      }
      case 'Exit': {
        const loop = loops.get(value.loop.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer lost exit loop')
        return Object.freeze({ _tag: 'Jump', target: loop.following, provenance: value.provenance })
      }
      case 'Yield': {
        const loop = conditionOwners.get(region.id.ordinal)
        if (loop === undefined) throw new RangeError('LLVM linearizer found unowned yield')
        return Object.freeze({
          _tag: 'Branch',
          condition: loop.conditionValue,
          taken: loop.body,
          otherwise: loop.following,
          provenance: value.provenance,
        })
      }
    }
  }
  const raw = Mir.topologicalRegions(fn).map((region): StructuredBlock => {
    if (region._tag === 'ConditionalRegion') {
      return Object.freeze({
        id: region.id,
        origin: region.id,
        kind: 'Normal',
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Branch',
          condition: region.condition,
          taken: region.taken,
          otherwise: region.otherwise,
          provenance: region.provenance,
        }),
      })
    }
    if (region._tag === 'LoopRegion') {
      return Object.freeze({
        id: region.id,
        origin: region.id,
        kind: 'Normal',
        operations: Object.freeze([]),
        terminator: Object.freeze({
          _tag: 'Jump',
          target: region.condition,
          provenance: region.provenance,
        }),
      })
    }
    return Object.freeze({
      id: region.id,
      origin: region.id,
      kind: region._tag === 'CleanupRegion' ? 'Cleanup' : 'Normal',
      operations: region._tag === 'CleanupRegion' ? region.releases : region.operations,
      terminator: outcome(region),
    })
  })
  const incoming = new Map<number, number>()
  for (const edge of Mir.controlEdges(fn)) {
    incoming.set(edge.to.ordinal, (incoming.get(edge.to.ordinal) ?? 0) + 1)
  }
  const byId = new Map(raw.map((block) => [block.id.ordinal, block] as const))
  const regionsById = new Map(fn.regions.map((region) => [region.id.ordinal, region] as const))
  const inlined = new Set<number>()
  const blocks = raw.map((block): StructuredBlock => {
    let operations = [...block.operations]
    let terminator = block.terminator
    const seen = new Set<number>()
    while (terminator._tag === 'Jump' && !seen.has(terminator.target.ordinal)) {
      const target = byId.get(terminator.target.ordinal)
      const targetRegion = regionsById.get(terminator.target.ordinal)
      const inlineable =
        target !== undefined &&
        incoming.get(target.id.ordinal) === 1 &&
        (target.kind === 'Cleanup' || targetRegion?._tag === 'ConditionalRegion')
      if (!inlineable) break
      seen.add(target.id.ordinal)
      inlined.add(target.id.ordinal)
      operations = [...operations, ...target.operations]
      terminator = target.terminator
    }
    return Object.freeze({ ...block, operations: Object.freeze(operations), terminator })
  })
  const referenced = new Set<number>([fn.entry.ordinal])
  // Structural incoming counts do not include every edge introduced by outcome lowering. Keep an
  // inline candidate when the rewritten graph still names it; otherwise a valid branch can target
  // a block removed by this private optimization.
  for (const block of blocks) {
    const terminator = block.terminator
    if (terminator._tag === 'Jump') referenced.add(terminator.target.ordinal)
    if (
      terminator._tag === 'Branch' ||
      terminator._tag === 'MatchBranch' ||
      terminator._tag === 'EnumMatchBranch'
    ) {
      referenced.add(terminator.taken.ordinal)
      referenced.add(terminator.otherwise.ordinal)
    }
  }
  return expandMatches(
    fn,
    Object.freeze(
      blocks.filter((block) => !inlined.has(block.id.ordinal) || referenced.has(block.id.ordinal)),
    ),
  )
}

export const llvmControl = (program: Mir.Module): ReadonlyArray<ControlProvenance> =>
  Object.freeze(
    program.functions.flatMap((fn) =>
      (() => {
        const linear = linearize(fn)
        const originOf = (target: Mir.RegionId): Mir.RegionId | undefined =>
          linear.find((candidate) => candidate.id.ordinal === target.ordinal)?.origin
        return linear.map((block): ControlProvenance => {
          const terminator = block.terminator
          const targets =
            terminator._tag === 'Jump'
              ? [originOf(terminator.target)]
              : terminator._tag === 'Branch' ||
                  terminator._tag === 'MatchBranch' ||
                  terminator._tag === 'EnumMatchBranch'
                ? [originOf(terminator.taken), originOf(terminator.otherwise)]
                : []
          const canonicalTargets = Object.freeze(
            targets.flatMap((target) =>
              target === undefined || target.ordinal === block.origin.ordinal ? [] : [target],
            ),
          )
          return Object.freeze({
            _tag: 'BackendControlProvenance',
            backend: 'LLVM',
            function: fn.id,
            instance: fn.instance,
            region: block.origin,
            construct:
              terminator._tag === 'Jump'
                ? 'LlvmJump'
                : terminator._tag === 'Branch' ||
                    terminator._tag === 'MatchBranch' ||
                    terminator._tag === 'EnumMatchBranch'
                  ? 'LlvmBranch'
                  : terminator._tag === 'Return' || terminator._tag === 'PropagateEffectFailure'
                    ? 'LlvmReturn'
                    : 'LlvmTrap',
            targets: canonicalTargets,
            span: terminator.provenance.span,
          })
        })
      })(),
    ),
  )
