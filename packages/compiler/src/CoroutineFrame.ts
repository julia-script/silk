import * as Instances from './Instances.js'
import { alignUp } from './internal/Align.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as SilkType from './Type.js'

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
  slot: Mir.CoroutineFrameSlot,
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

const planState = (
  program: Mir.Module,
  state: Mir.CoroutineFrameState,
  headerSize: number,
): Mir.CoroutineFrameTargetStateLayout | undefined => {
  let cursor = headerSize
  let alignment: number = program.layout.target.pointerAlignment
  const payload: Array<Mir.CoroutineFramePayloadField> = []
  for (const slot of state.slots) {
    const storage = storageOf(program, slot)
    if (storage === undefined || storage.alignment < 1 || storage.size < 0) return undefined
    const offset = alignUp(cursor, storage.alignment)
    payload.push(
      Object.freeze({
        _tag: 'CoroutineFramePayloadField',
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
    alignment = Math.max(alignment, storage.alignment)
  }
  const size = alignUp(cursor, alignment)
  return Object.freeze({
    _tag: 'CoroutineFrameTargetStateLayout',
    point: state.point,
    size,
    alignment,
    payload: Object.freeze(payload),
    tailPadding: size - cursor,
  })
}

const planDescriptor = (
  program: Mir.Module,
  descriptor: Mir.CoroutineFrameDescriptor,
): Mir.CoroutineFrameTargetLayout | undefined => {
  const wordSize = program.layout.target.pointerSize
  const wordAlignment = program.layout.target.pointerAlignment
  const roles: ReadonlyArray<Mir.CoroutineFrameHeaderRole> = Object.freeze(['Parent', 'State'])
  const header = Object.freeze(
    roles.map((role, ordinal) =>
      Object.freeze({
        _tag: 'CoroutineFrameHeaderField' as const,
        role,
        offset: ordinal * wordSize,
        size: wordSize,
        alignment: wordAlignment,
      }),
    ),
  )
  const states = descriptor.states.flatMap((state) => {
    const planned = planState(program, state, header.length * wordSize)
    return planned === undefined ? [] : [planned]
  })
  if (states.length !== descriptor.states.length) return undefined
  states.sort((left, right) => pointKey(left.point).localeCompare(pointKey(right.point)))
  const alignment = Math.max(wordAlignment, ...states.map((state) => state.alignment))
  const size = alignUp(
    Math.max(header.length * wordSize, ...states.map((state) => state.size)),
    alignment,
  )
  return Object.freeze({
    _tag: 'CoroutineFrameTargetLayout',
    function: descriptor.function,
    size,
    alignment,
    header,
    states: Object.freeze(states),
  })
}

/** Plans one maximum target layout for each frame-producing specialized invocation. */
export const plan = (program: Mir.Module): Mir.CoroutineFramePlan | undefined => {
  const entries = program.functions.flatMap((fn) => {
    const descriptor = fn.suspension?.frame
    if (descriptor === undefined) return []
    const entry = planDescriptor(program, descriptor)
    return entry === undefined ? [] : [entry]
  })
  if (entries.length === 0) return undefined
  entries.sort((left, right) =>
    Instances.keyText(left.function).localeCompare(Instances.keyText(right.function)),
  )
  return Object.freeze({
    _tag: 'CoroutineFramePlan',
    target: program.layout.target,
    entries: Object.freeze(entries),
  })
}

/** Finds one mutually-exclusive state layout inside its invocation's maximum frame. */
export const stateLayout = (
  program: Mir.Module,
  point: Mir.SuspensionPointId,
): Mir.CoroutineFrameTargetStateLayout | undefined =>
  program.coroutineFrames?.entries
    .find(
      (entry) =>
        entry.function.declaration.module === point.owner.declaration.module &&
        entry.function.declaration.name === point.owner.declaration.name &&
        entry.function.typeArguments.map(SilkType.genericArgumentKey).join(',') ===
          point.owner.typeArguments.map(SilkType.genericArgumentKey).join(','),
    )
    ?.states.find((state) => pointKey(state.point) === pointKey(point))

/** Attaches physical target plans without changing target-neutral frame descriptors. */
export const apply = (program: Mir.Module): Mir.Module => {
  const coroutineFrames = plan(program)
  return coroutineFrames === undefined ? program : Object.freeze({ ...program, coroutineFrames })
}

/** Stable compact evidence used by tests and later backend artifact checks. */
export const summary = (self: Mir.CoroutineFramePlan): string =>
  self.entries
    .map(
      (entry) =>
        `${Instances.keyText(entry.function)} size=${entry.size} align=${entry.alignment} storage=private-execution-stack header=${entry.header
          .map((field) => `${field.role}@${field.offset}:${field.size}/${field.alignment}`)
          .join(',')} states=${entry.states
          .map(
            (state) =>
              `${pointKey(state.point)}[${state.payload
                .map(
                  (field) =>
                    `${field.slot}:l${field.local.ordinal}@${field.offset}:${field.size}/${field.alignment}+${field.padding}`,
                )
                .join(',')}]`,
          )
          .join(';')}`,
    )
    .join('\n')
