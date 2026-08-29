import type * as FuncActor from '@silklang/wasm/Func'
import type * as Global from '@silklang/wasm/Global'
import * as Instr from '@silklang/wasm/Instr'
import type * as Memory from '@silklang/wasm/Memory'
import * as CleanupPlan from './CleanupPlan.js'
import { alignUp } from './internal/Align.js'
import * as LayoutPlan from './Layout.js'
import * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import * as SilkType from './Type.js'
import { carriesBorrowAddress } from './WasmLanes.js'

export interface FrameRoot {
  readonly local: number
  readonly offset: number
  readonly type: Exclude<Mir.Type, { readonly _tag: 'EffectBorrow' | 'EffectOutcome' }>
}

export interface FramePlan {
  readonly roots: ReadonlyMap<number, FrameRoot>
  /**
   * The roots whose address leaves the frame — borrow formations and borrow-shaped captures.
   * A callee reaches exactly these through the pointer it was handed, so exactly these have to
   * reload after a call. The remaining roots are addressed only by the cleanup sequence that
   * materializes and reloads them itself, and reloading one after a call would overwrite a live
   * value with frame bytes nothing ever wrote.
   */
  readonly escaping: ReadonlySet<number>
  /** Address-taken frame roots reachable through each MIR local's stored pointer lanes. */
  readonly localRoots: ReadonlyMap<number, ReadonlySet<number>>
  /** Depth-indexed canonical variant storage used while cleaning widened nominal-union carriers. */
  readonly nominalUnionCleanupScratch: ReadonlyArray<{
    readonly offset: number
    readonly size: number
    readonly alignment: number
  }>
  readonly size: number
  readonly alignment: number
}

export interface OperationCleanupEntry {
  readonly cleanup: CleanupPlan.CleanupPlan
  readonly local?: Mir.LocalId
}

export const operationCleanupEntries = (
  operation: Mir.Operation,
): ReadonlyArray<OperationCleanupEntry> => {
  switch (operation._tag) {
    case 'Drop':
      return Object.freeze([Object.freeze({ cleanup: operation.cleanup, local: operation.local })])
    case 'SlotDrop':
      return Object.freeze([Object.freeze({ cleanup: operation.cleanup })])
    case 'CheckedScalar':
      return Object.freeze([
        Object.freeze({ cleanup: operation.presentCleanup, local: operation.present }),
        Object.freeze({ cleanup: operation.absentCleanup, local: operation.absent }),
      ])
    case 'SharedWithMut':
      return Object.freeze([
        Object.freeze({ cleanup: operation.useCleanup, local: operation.use }),
        Object.freeze({ cleanup: operation.conflictCleanup, local: operation.onConflict }),
      ])
    case 'ExecutionPark':
      return Object.freeze([
        Object.freeze({ cleanup: operation.guardCleanup, local: operation.guard }),
        Object.freeze({ cleanup: operation.registerCleanup, local: operation.register }),
      ])
    case 'PropagateEffectFailure':
    case 'RunEffect':
    case 'RunEffectValue':
    case 'RunEffectComposite':
    case 'RunStaticEffect':
      return Object.freeze(
        (operation.releases ?? []).map((release) =>
          Object.freeze({ cleanup: release.cleanup, local: release.local }),
        ),
      )
    case 'CloseEffectEntry':
      return Object.freeze(
        operation.failures.map((failure) =>
          Object.freeze({ cleanup: failure.cleanup, local: failure.payload }),
        ),
      )
    case 'Match':
      return Object.freeze(
        operation.arms.flatMap((arm) =>
          arm.selected.cleanup.map((entry) =>
            Object.freeze({ cleanup: entry.cleanup, local: entry.destination }),
          ),
        ),
      )
    default:
      return Object.freeze([])
  }
}

export const operationCleanupPlans = (
  operation: Mir.Operation,
): ReadonlyArray<CleanupPlan.CleanupPlan> =>
  Object.freeze(operationCleanupEntries(operation).map((entry) => entry.cleanup))

const nominalUnionScratchRequirements = (
  cleanups: ReadonlyArray<CleanupPlan.CleanupPlan>,
  plan: LayoutPlan.Plan,
): ReadonlyArray<{ readonly size: number; readonly alignment: number }> => {
  const requirements: Array<{ size: number; alignment: number }> = []
  const visit = (cleanup: CleanupPlan.CleanupPlan, depth: number): void => {
    switch (cleanup._tag) {
      case 'HookCleanup':
        visit(cleanup.inner, depth)
        return
      case 'StructCleanup':
        for (const field of cleanup.fields) visit(field.cleanup, depth)
        return
      case 'NominalUnionCleanup': {
        const entry = LayoutPlan.entry(plan, cleanup.type)
        if (entry?.representation._tag !== 'NominalUnion') return
        const materialization = LayoutPlan.nominalUnionMaterialization(entry.representation)
        const current = requirements.at(depth)
        requirements[depth] = {
          size: Math.max(current?.size ?? 0, materialization.size),
          alignment: Math.max(current?.alignment ?? 1, materialization.alignment),
        }
        for (const variant of cleanup.variants)
          for (const field of variant.fields) visit(field.cleanup, depth + 1)
        return
      }
      case 'ArrayCleanup':
        visit(cleanup.element, depth)
        return
      case 'UnionCleanup':
        for (const entry of cleanup.cases) visit(entry.cleanup, depth)
        return
      case 'CallableCleanup':
      case 'EffectCleanup':
        for (const slot of cleanup.slots) visit(slot.cleanup, depth)
        return
      case 'EffectCompositeCleanup':
        for (const alternative of cleanup.alternatives) visit(alternative, depth)
        return
      case 'RawBufferCleanup':
      case 'LocalSharedCoreCleanup':
      case 'ExecutionCleanup':
      case 'WakeCleanup':
        visit(cleanup.allocation, depth)
        return
      default:
        return
    }
  }
  for (const cleanup of cleanups) visit(cleanup, 0)
  return Object.freeze(requirements.map((requirement) => Object.freeze(requirement)))
}

export const framePlan = (
  fn: Mir.MirFunction,
  plan: LayoutPlan.Plan,
  additionalCleanups: ReadonlyArray<CleanupPlan.CleanupPlan> = Object.freeze([]),
): FramePlan => {
  const formations = MirVerification.operations(fn).filter(
    (operation): operation is Extract<Mir.Operation, { readonly _tag: 'BeginLoan' }> =>
      operation._tag === 'BeginLoan',
  )
  const escaping = new Set([
    ...formations.flatMap((operation) =>
      operation.sourceType._tag === 'Slice' ||
      fn.localTypes.at(operation.root.ordinal)?._tag === 'Reference' ||
      fn.localTypes.at(operation.root.ordinal)?._tag === 'EffectBorrow'
        ? []
        : [operation.root.ordinal],
    ),
    ...MirVerification.operations(fn).flatMap((operation) =>
      operation._tag === 'MakeEffect' || operation._tag === 'MakeCallable'
        ? operation.captures.flatMap((capture, ordinal) =>
            (operation._tag === 'MakeEffect'
              ? operation.type.environment.fields.at(ordinal)?.representation === 'Borrow'
              : capture.access === 'Shared' || capture.access === 'Exclusive') &&
            fn.localTypes.at(capture.source.ordinal)?._tag !== 'EffectBorrow'
              ? [capture.source.ordinal]
              : [],
          )
        : [],
    ),
  ])
  const rootOrdinals = new Set([
    ...escaping,
    // A hook-bearing release passes `&mut self` into its hook, so its MIR owner needs frame storage.
    ...MirVerification.operations(fn).flatMap((operation) =>
      operationCleanupEntries(operation).flatMap((entry) =>
        entry.local !== undefined &&
        CleanupPlan.hasHook(entry.cleanup) &&
        fn.localTypes.at(entry.local.ordinal)?._tag !== 'EffectBorrow'
          ? [entry.local.ordinal]
          : [],
      ),
    ),
  ])
  const roots = new Map<number, FrameRoot>()
  let cursor = 0
  let alignment = 1
  for (const local of [...rootOrdinals].sort((left, right) => left - right)) {
    const type = fn.localTypes.at(local)
    let storage: { readonly size: number; readonly alignment: number } | undefined
    if (type?._tag === 'CallableValue' && type.environment !== undefined) {
      storage = Object.freeze({
        size: type.environment.size,
        alignment: type.environment.alignment,
      })
    } else if (type?._tag === 'EffectValue') {
      storage = Object.freeze({
        size: type.environment.size,
        alignment: type.environment.alignment,
      })
    } else if (type?._tag === 'EffectComposite') {
      storage = Object.freeze({
        size: type.alternatives.reduce(
          (maximum, alternative) => Math.max(maximum, alternative.environment.size),
          0,
        ),
        alignment: type.alternatives.reduce(
          (maximum, alternative) => Math.max(maximum, alternative.environment.alignment),
          1,
        ),
      })
    } else if (type !== undefined) {
      storage = LayoutPlan.entry(plan, Mir.semanticType(type))
    }
    if (
      type === undefined ||
      type._tag === 'EffectBorrow' ||
      type._tag === 'EffectOutcome' ||
      storage === undefined
    ) {
      throw new RangeError(
        `Wasm frame ${fn.id.module}.${fn.id.name} lost address-taken root %${local}${type === undefined ? '' : ` (${SilkType.encode(Mir.semanticType(type))})`}`,
      )
    }
    cursor = alignUp(cursor, storage.alignment)
    roots.set(local, Object.freeze({ local, offset: cursor, type }))
    cursor += storage.size
    alignment = Math.max(alignment, storage.alignment)
  }
  const localRoots = new Map<number, Set<number>>(
    [...escaping].map((local) => [local, new Set([local])] as const),
  )
  const include = (destination: Mir.LocalId, sources: ReadonlyArray<Mir.LocalId>): boolean => {
    const destinationType = fn.localTypes.at(destination.ordinal)
    if (destinationType === undefined || !carriesBorrowAddress(plan, destinationType)) return false
    const selected = localRoots.get(destination.ordinal) ?? new Set<number>()
    const previous = selected.size
    for (const source of sources) {
      for (const root of localRoots.get(source.ordinal) ?? []) selected.add(root)
    }
    if (selected.size > 0) localRoots.set(destination.ordinal, selected)
    return selected.size !== previous
  }
  const operations = MirVerification.operations(fn)
  let changed = true
  while (changed) {
    changed = false
    for (const operation of operations) {
      switch (operation._tag) {
        case 'BeginLoan':
          changed = include(operation.destination, [operation.root]) || changed
          break
        case 'Move':
        case 'Project':
          changed = include(operation.destination, [operation.source]) || changed
          break
        case 'ReadPlace':
          changed = include(operation.destination, [operation.root]) || changed
          break
        case 'WritePlace':
          changed = include(operation.root, [operation.source]) || changed
          break
        case 'Construct':
        case 'ConstructUnionVariant':
          changed =
            include(
              operation.destination,
              operation.fields.map((field) => field.value),
            ) || changed
          break
        case 'ConstructArray':
          changed = include(operation.destination, operation.elements) || changed
          break
        case 'MakeEffect':
        case 'MakeCallable':
          changed =
            include(
              operation.destination,
              operation.captures.map((capture) => capture.source),
            ) || changed
          break
        case 'ApplyCallable':
          changed =
            include(operation.destination, [
              ...(operation.callable === undefined ? [] : [operation.callable]),
              ...operation.captures.map((capture) => capture.source),
              ...operation.arguments,
            ]) || changed
          break
        case 'Call':
          changed = include(operation.destination, operation.arguments) || changed
          break
        case 'RunEffectValue':
          changed =
            include(operation.destination, [operation.effect, ...operation.arguments]) || changed
          break
        case 'RunStaticEffect':
          changed =
            include(operation.destination, [
              ...operation.captures.map((capture) => capture.source),
              ...operation.arguments,
            ]) || changed
          break
        case 'CatchEffect':
          changed =
            include(operation.destination, [operation.effect, ...operation.arguments]) || changed
          break
        case 'Match':
          for (const arm of operation.arms) {
            for (const binding of arm.bindings) {
              changed = include(binding.destination, [operation.scrutinee]) || changed
            }
            changed = include(operation.destination, [arm.selected.result]) || changed
          }
          break
        case 'ConvertUnion':
        case 'UnpackEffectSuccess':
          changed = include(operation.destination, [operation.source]) || changed
          break
        case 'SlotTake':
        case 'SlotCopy':
          changed = include(operation.destination, [operation.slot]) || changed
          break
        case 'RawBufferView':
          changed = include(operation.destination, [operation.buffer]) || changed
          break
        default:
          break
      }
    }
  }
  const nominalUnionCleanupScratch = nominalUnionScratchRequirements(
    [...operations.flatMap(operationCleanupPlans), ...additionalCleanups],
    plan,
  ).map((requirement) => {
    cursor = alignUp(cursor, requirement.alignment)
    const scratch = Object.freeze({ ...requirement, offset: cursor })
    cursor += requirement.size
    alignment = Math.max(alignment, requirement.alignment)
    return scratch
  })
  const frozenLocalRoots = new Map(
    [...localRoots].map(([local, reachable]) => [local, new Set(reachable)] as const),
  )
  return Object.freeze({
    roots,
    escaping,
    localRoots: frozenLocalRoots,
    nominalUnionCleanupScratch: Object.freeze(nominalUnionCleanupScratch),
    size: alignUp(cursor, alignment),
    alignment,
  })
}

/** Non-trapping comparisons map straight onto wasm's `i32` relational operators. */
export const heapHeaderBytes = 16
/** Size classes hold payload capacities `1 << (4 + index)`, so class 0 is 16 bytes. */
export const heapClassShift = 4
export const heapClassCount = 27
/** Blocks too large or too over-aligned for a class share one irregular list. */
export const heapIrregularClass = heapClassCount
export const heapLargestClassBytes = 1 << (heapClassShift + heapClassCount - 1)
/** The free-list head table sits at the first heap page; wasm memory starts it zeroed. */
export const heapTableBase = 65536
export const heapBase =
  heapTableBase + Math.ceil(((heapIrregularClass + 1) * 4) / heapHeaderBytes) * heapHeaderBytes

/**
 * The shadow stack may not reach the allocator's region.
 *
 * The two bump regions share one linear memory and point at each other: the shadow stack starts at
 * the end of static data and grows up, while the heap starts at the fixed `heapTableBase` and grows
 * up from there. Without a bound between them a deep enough chain of frames walks over the
 * allocator's free-list table and then over live blocks, and nothing traps at the overwrite — the
 * corrupted bytes are read back later, by whichever guard happens to see them first (#134).
 *
 * `memory.grow` cannot stand in for this bound. Growth appends pages above everything, so the stack
 * reaches the heap long before a reservation runs past the memory that is mapped.
 */
export const stackLimit = heapTableBase

/**
 * Linear memory opens with a 16-byte hole so that address 0 is never a live object. The backend
 * spends its first word on a status word: a deliberate trap writes the reason there before
 * `unreachable`, so a host that catches the trap can name what happened instead of inferring it
 * from a byte offset. Memory is exported already, and it survives the trap.
 */
export interface MemoryContext {
  readonly memory: Memory.Memory
  readonly stackPointer: Global.Global
  /** Where the shadow stack starts, so a report can rewind the pointer to it. */
  readonly stackBase: number
  /**
   * The first address the shadow stack may not reach, or `undefined` when this module has no heap
   * for it to run into and only the wrap and `memory.grow` guards apply.
   */
  readonly stackLimit: number | undefined
  readonly heapPointer: Global.Global
  readonly frame: FramePlan
  readonly plan: LayoutPlan.Plan
  readonly staticOffsets: ReadonlyMap<string, number>
  readonly standardWrite?: FuncActor.Func
  /** `(bytes, alignment) -> payload address`, answering 0 when the request cannot be served. */
  readonly heapAllocate?: FuncActor.Func
  /** `(header address) -> ()`, returning one block to its size class. Ignores a null header. */
  readonly heapRelease?: FuncActor.Func
}

/** Grows memory just enough to cover `[0, cursor)`, or executes `refuse` on host refusal. */
export const growToCover = (
  memory: Memory.Memory,
  cursor: number,
  refuse: ReadonlyArray<Instr.Instr>,
): ReadonlyArray<Instr.Instr> => {
  const pagesForCursor: ReadonlyArray<Instr.Instr> = [
    Instr.localGet(cursor),
    Instr.i32Const(1),
    Instr.op('i32.sub'),
    Instr.i32Const(16),
    Instr.op('i32.shr_u'),
    Instr.i32Const(1),
    Instr.op('i32.add'),
  ]
  return [
    ...pagesForCursor,
    Instr.memorySize(memory),
    Instr.op('i32.gt_u'),
    Instr.ifElse(
      Instr.emptyBlockType,
      [
        ...pagesForCursor,
        Instr.memorySize(memory),
        Instr.op('i32.sub'),
        Instr.memoryGrow(memory),
        Instr.i32Const(-1),
        Instr.op('i32.eq'),
        Instr.ifElse(Instr.emptyBlockType, refuse, []),
      ],
      [],
    ),
  ]
}

/**
 * The body of the synthesized allocator. It serves a request from the head of the matching free
 * list when that block fits, and otherwise carves a fresh block off the bump region, growing
 * memory only when the region runs past what is already mapped.
 */
export const heapAllocateBody = (
  memory: Memory.Memory,
  heapPointer: Global.Global,
): ReadonlyArray<Instr.Instr> => {
  const [bytes, alignment, align, klass, capacity, listAddress, block, payload, cursor] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8,
  ]
  const load = (address: number, offset = 0): ReadonlyArray<Instr.Instr> => [
    Instr.localGet(address),
    Instr.memoryAccess('i32.load', memory, { offset }),
  ]
  const store = (
    address: number,
    value: ReadonlyArray<Instr.Instr>,
    offset = 0,
  ): ReadonlyArray<Instr.Instr> => [
    Instr.localGet(address),
    ...value,
    Instr.memoryAccess('i32.store', memory, { offset }),
  ]
  const refuse: ReadonlyArray<Instr.Instr> = [Instr.i32Const(0), Instr.op('return')]
  return [
    // align = max(alignment, 16). A block start is always 16-aligned, and a zero alignment — which
    // `Layout` construction already rejects — must never widen the mask into every address.
    Instr.i32Const(heapHeaderBytes),
    Instr.localGet(alignment),
    Instr.localGet(alignment),
    Instr.i32Const(heapHeaderBytes),
    Instr.op('i32.lt_u'),
    Instr.op('select'),
    Instr.localSet(align),
    // A request is classified when the block's own 16-byte alignment already satisfies it and its
    // capacity fits the largest class. Everything else shares the irregular list.
    Instr.localGet(align),
    Instr.i32Const(heapHeaderBytes),
    Instr.op('i32.eq'),
    Instr.localGet(bytes),
    Instr.i32Const(heapLargestClassBytes),
    Instr.op('i32.le_u'),
    Instr.op('i32.and'),
    Instr.ifElse(
      Instr.emptyBlockType,
      [
        // klass = bytes <= 16 ? 0 : ceil(log2(bytes)) - 4, capacity = 1 << (klass + 4)
        Instr.i32Const(0),
        Instr.i32Const(32),
        Instr.localGet(bytes),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.op('i32.clz'),
        Instr.op('i32.sub'),
        Instr.i32Const(heapClassShift),
        Instr.op('i32.sub'),
        Instr.localGet(bytes),
        Instr.i32Const(1 << heapClassShift),
        Instr.op('i32.le_u'),
        Instr.op('select'),
        Instr.localSet(klass),
        Instr.i32Const(1),
        Instr.localGet(klass),
        Instr.i32Const(heapClassShift),
        Instr.op('i32.add'),
        Instr.op('i32.shl'),
        Instr.localSet(capacity),
        Instr.localGet(klass),
        Instr.i32Const(2),
        Instr.op('i32.shl'),
        Instr.i32Const(heapTableBase),
        Instr.op('i32.add'),
        Instr.localSet(listAddress),
      ],
      [
        Instr.i32Const(-1),
        Instr.localSet(klass),
        Instr.localGet(bytes),
        Instr.localSet(capacity),
        Instr.i32Const(heapTableBase + heapIrregularClass * 4),
        Instr.localSet(listAddress),
      ],
    ),
    // Reuse the head of the list when it serves the request. Every classified block in a list has
    // the class's capacity and a 16-aligned payload, so only an irregular head needs measuring.
    ...load(listAddress),
    Instr.localTee(block),
    Instr.ifElse(
      Instr.emptyBlockType,
      [
        Instr.localGet(klass),
        Instr.i32Const(0),
        Instr.op('i32.ge_s'),
        ...load(block),
        Instr.localGet(bytes),
        Instr.op('i32.ge_u'),
        Instr.localGet(block),
        Instr.i32Const(heapHeaderBytes),
        Instr.op('i32.add'),
        Instr.localGet(align),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.op('i32.and'),
        Instr.op('i32.eqz'),
        Instr.op('i32.and'),
        Instr.op('i32.or'),
        Instr.ifElse(
          Instr.emptyBlockType,
          [
            ...store(listAddress, load(block, 8)),
            ...store(block, [Instr.i32Const(0)], 8),
            Instr.localGet(block),
            Instr.i32Const(heapHeaderBytes),
            Instr.op('i32.add'),
            Instr.op('return'),
          ],
          [],
        ),
      ],
      [],
    ),
    // Carve a fresh block. The bump cursor stays 16-aligned so every block start is, which is what
    // keeps a classified payload's 16-byte alignment an invariant rather than a coincidence.
    Instr.globalGet(heapPointer),
    Instr.i32Const(heapHeaderBytes - 1),
    Instr.op('i32.add'),
    Instr.i32Const(-heapHeaderBytes),
    Instr.op('i32.and'),
    Instr.localSet(cursor),
    Instr.localGet(cursor),
    Instr.i32Const(heapHeaderBytes),
    Instr.op('i32.add'),
    Instr.localGet(align),
    Instr.i32Const(1),
    Instr.op('i32.sub'),
    Instr.op('i32.add'),
    Instr.i32Const(0),
    Instr.localGet(align),
    Instr.op('i32.sub'),
    Instr.op('i32.and'),
    Instr.localTee(payload),
    Instr.localGet(cursor),
    Instr.op('i32.lt_u'),
    Instr.ifElse(Instr.emptyBlockType, refuse, []),
    Instr.localGet(payload),
    Instr.i32Const(heapHeaderBytes),
    Instr.op('i32.sub'),
    Instr.localSet(block),
    Instr.localGet(payload),
    Instr.localGet(capacity),
    Instr.op('i32.add'),
    Instr.i32Const(heapHeaderBytes - 1),
    Instr.op('i32.add'),
    Instr.localTee(cursor),
    Instr.localGet(payload),
    Instr.op('i32.lt_u'),
    Instr.ifElse(Instr.emptyBlockType, refuse, []),
    Instr.localGet(cursor),
    Instr.i32Const(-heapHeaderBytes),
    Instr.op('i32.and'),
    Instr.localSet(cursor),
    ...growToCover(memory, cursor, refuse),
    Instr.localGet(cursor),
    Instr.globalSet(heapPointer),
    ...store(block, [Instr.localGet(capacity)]),
    ...store(block, [Instr.localGet(klass)], 4),
    ...store(block, [Instr.i32Const(0)], 8),
    ...store(block, [Instr.i32Const(0)], 12),
    Instr.localGet(payload),
  ]
}

/**
 * The body of the synthesized release. It pushes one block onto the head of its size class's free
 * list and returns. A null header is a no-op, which is what lets a union's conditional cleanup
 * select the inactive case's reclaim context to zero instead of branching around the call — the
 * same shortcut the LLVM backend takes through libc `free`.
 */
export const heapReleaseBody = (memory: Memory.Memory): ReadonlyArray<Instr.Instr> => {
  const [header, listAddress] = [0, 1]
  const klass: ReadonlyArray<Instr.Instr> = [
    Instr.localGet(header),
    Instr.memoryAccess('i32.load', memory, { offset: 4 }),
  ]
  return [
    Instr.localGet(header),
    Instr.op('i32.eqz'),
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('return')], []),
    // An unsigned comparison folds the irregular block's -1 class in with any out-of-range index,
    // so a header can never steer the push past the end of the head table.
    ...klass,
    Instr.i32Const(heapIrregularClass),
    ...klass,
    Instr.i32Const(heapClassCount),
    Instr.op('i32.lt_u'),
    Instr.op('select'),
    Instr.i32Const(2),
    Instr.op('i32.shl'),
    Instr.i32Const(heapTableBase),
    Instr.op('i32.add'),
    Instr.localSet(listAddress),
    Instr.localGet(header),
    Instr.localGet(listAddress),
    Instr.memoryAccess('i32.load', memory),
    Instr.memoryAccess('i32.store', memory, { offset: 8 }),
    Instr.localGet(listAddress),
    Instr.localGet(header),
    Instr.memoryAccess('i32.store', memory),
  ]
}

/** Local names reach the `name` custom section, so release builds declare them unnamed. */
