import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as SilkType from './Type.js'

const alignUp = (offset: number, alignment: number): number =>
  Math.ceil(offset / alignment) * alignment

const pointKey = (point: Mir.SuspensionPointId): string =>
  [
    point.owner.declaration.module,
    point.owner.declaration.name,
    ...point.owner.typeArguments.map(SilkType.genericArgumentKey),
    ...point.owner.contractRow,
    point.sourceId,
    point.spanStart,
    point.spanEnd,
    point.ordinal,
  ].join('\u0000')

const storageOf = (
  program: Mir.Module,
  slot: Mir.ContinuationSlot,
): { readonly size: number; readonly alignment: number } | undefined => {
  if (slot.access._tag === 'BorrowedDependency' || slot.type._tag === 'EffectBorrow')
    return Object.freeze({
      size: program.layout.target.pointerSize,
      alignment: program.layout.target.pointerAlignment,
    })
  if (slot.type._tag === 'EffectValue')
    return Object.freeze({
      size: slot.type.environment.size,
      alignment: slot.type.environment.alignment,
    })
  if (slot.type._tag === 'CallableValue') {
    const view = slot.type.environment?.view
    return Object.freeze({
      size: view?.size ?? program.layout.target.pointerSize * 2,
      alignment: view?.alignment ?? program.layout.target.pointerAlignment,
    })
  }
  const entry = Layout.entry(program.layout, Mir.semanticType(slot.type))
  return entry === undefined
    ? undefined
    : Object.freeze({ size: entry.size, alignment: entry.alignment })
}

const planDescriptor = (
  program: Mir.Module,
  fn: Mir.MirFunction,
  descriptor: Mir.ContinuationDescriptor,
): Mir.ContinuationTargetLayout | undefined => {
  const wordSize = program.layout.target.pointerSize
  const wordAlignment = program.layout.target.pointerAlignment
  const roles: ReadonlyArray<Mir.ContinuationHeaderRole> = Object.freeze([
    'Parent',
    'Resume',
    'Reclaim',
    'ReclaimContext',
  ])
  const header = Object.freeze(
    roles.map((role, ordinal) =>
      Object.freeze({
        _tag: 'ContinuationHeaderField' as const,
        role,
        offset: ordinal * wordSize,
        size: wordSize,
        alignment: wordAlignment,
      }),
    ),
  )
  let cursor: number = header.length * wordSize
  let frameAlignment: number = wordAlignment
  const payload: Array<Mir.ContinuationPayloadField> = []
  for (const slot of descriptor.layout.slots) {
    const storage = storageOf(program, slot)
    if (storage === undefined || storage.alignment < 1 || storage.size < 0) return undefined
    const offset = alignUp(cursor, storage.alignment)
    payload.push(
      Object.freeze({
        _tag: 'ContinuationPayloadField',
        slot: slot.ordinal,
        local: slot.local,
        type: slot.type,
        access: slot.access,
        offset,
        size: storage.size,
        alignment: storage.alignment,
        padding: offset - cursor,
      }),
    )
    cursor = offset + storage.size
    frameAlignment = Math.max(frameAlignment, storage.alignment)
  }
  const size = alignUp(cursor, frameAlignment)
  return Object.freeze({
    _tag: 'ContinuationTargetLayout',
    point: descriptor.point,
    function: fn.instance,
    size,
    alignment: frameAlignment,
    header,
    payload: Object.freeze(payload),
    tailPadding: size - cursor,
    initialization: Object.freeze(
      descriptor.layout.prefixRollbacks.map((rollback) =>
        Object.freeze({
          initialized: rollback.initialized,
          fields: Object.freeze(
            descriptor.layout.initializationOrder.slice(0, rollback.initialized),
          ),
          rollback,
        }),
      ),
    ),
    acquisition: Object.freeze({
      _tag: 'ContinuationAcquisition',
      allocator: Object.freeze({
        _tag: 'IncomingTransferAllocator',
        capability: SilkType.allocator,
        role: 'DefaultRole',
        access: 'Exclusive',
      }),
      request: Object.freeze({ bytes: size, alignment: frameAlignment }),
      loan: Object.freeze({
        _tag: 'ContinuationAllocatorLoan',
        access: 'Exclusive',
        ends: 'BeforeInitializationAndPublication',
      }),
      retainedAuthority: ['Reclaim', 'ReclaimContext'] as const,
      order: [
        'Request',
        'EndAllocatorLoan',
        'Initialize',
        'Publish',
        'ChildReborrow',
        'ChildStart',
      ] as const,
    }),
    publication: Object.freeze({ _tag: 'AfterCompleteInitialization' }),
  })
}

/** Plans target storage only for frame-producing relays; origins and tail relays allocate nothing. */
export const plan = (program: Mir.Module): Mir.ContinuationLayoutPlan | undefined => {
  const entries = program.functions.flatMap((fn) =>
    (fn.suspension?.regions ?? []).flatMap((region) => {
      const descriptor =
        region._tag === 'RunSuspendableEffectRegion' ? region.relay.continuation : undefined
      if (descriptor === undefined) return []
      const entry = planDescriptor(program, fn, descriptor)
      return entry === undefined ? [] : [entry]
    }),
  )
  if (entries.length === 0) return undefined
  entries.sort((left, right) => pointKey(left.point).localeCompare(pointKey(right.point)))
  return Object.freeze({
    _tag: 'ContinuationLayoutPlan',
    target: program.layout.target,
    entries: Object.freeze(entries),
  })
}

/** Attaches a physical target plan without changing the target-neutral continuation descriptors. */
export const apply = (program: Mir.Module): Mir.Module => {
  const continuations = plan(program)
  return continuations === undefined ? program : Object.freeze({ ...program, continuations })
}

/** Stable compact evidence used by tests and later backend artifact checks. */
export const summary = (self: Mir.ContinuationLayoutPlan): string =>
  self.entries
    .map(
      (entry) =>
        `${pointKey(entry.point)} size=${entry.size} align=${entry.alignment} allocator=incoming-transfer:${SilkType.encode(entry.acquisition.allocator.capability)}@${entry.acquisition.allocator.role} request=${entry.acquisition.request.bytes}/${entry.acquisition.request.alignment} loan-end=${entry.acquisition.loan.ends} authority=${entry.acquisition.retainedAuthority.join('+')} order=${entry.acquisition.order.join('>')} header=${entry.header
          .map((field) => `${field.role}@${field.offset}:${field.size}/${field.alignment}`)
          .join(',')} payload=${entry.payload
          .map(
            (field) =>
              `${field.slot}:l${field.local.ordinal}@${field.offset}:${field.size}/${field.alignment}+${field.padding}`,
          )
          .join(',')} init=${entry.initialization
          .map((prefix) => `${prefix.initialized}[${prefix.fields.join(',')}]`)
          .join(',')} tail=${entry.tailPadding}`,
    )
    .join('\n')
