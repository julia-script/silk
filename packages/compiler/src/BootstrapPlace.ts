import type { SliceValue, Value } from './BootstrapValue.js'
import type * as Mir from './Mir.js'

export interface Access {
  readonly readIndex: (local: Mir.LocalId) => bigint
  readonly sliceElement: (slice: SliceValue, index: number) => Value
  readonly replaceSliceElement: (slice: SliceValue, index: number, value: Value) => Value
}

export type WalkResult =
  | {
      readonly _tag: 'Resolved'
      readonly selected: Value
      readonly indexes: ReadonlyArray<number>
    }
  | {
      readonly _tag: 'OutOfBounds'
      readonly index: bigint
      readonly length: number
      readonly selector: Mir.PlaceSelector
    }

/** Walks one checked place, retaining the concrete indexes needed by a later write. */
export const walkPlace = (
  root: Value,
  selectors: ReadonlyArray<Mir.PlaceSelector>,
  capturedIndexes: ReadonlyArray<number>,
  access: Access,
): WalkResult => {
  let selected = root
  const indexes: Array<number> = []
  for (const [ordinal, selector] of selectors.entries()) {
    if (selector._tag === 'FieldSelector') {
      if (selected._tag !== 'AggregateValue')
        throw new RangeError('MIR verifier allowed a field selector on a non-struct value')
      const field = selected.fields.find(
        (candidate) =>
          candidate.field.ordinal === selector.field.ordinal &&
          candidate.field.struct.sourceId === selector.field.struct.sourceId &&
          candidate.field.struct.ordinal === selector.field.struct.ordinal,
      )
      if (field === undefined) throw new RangeError('MIR verifier allowed a missing field selector')
      selected = field.value
      indexes.push(selector.field.ordinal)
      continue
    }
    if (selector._tag === 'SliceElementSelector') {
      if (selected._tag !== 'SliceValue' && selected._tag !== 'StaticViewValue')
        throw new RangeError('MIR verifier allowed a slice selector on a non-slice value')
      const exactIndex = BigInt(capturedIndexes.at(ordinal) ?? access.readIndex(selector.index))
      if (exactIndex < 0n || exactIndex >= BigInt(selected.length))
        return Object.freeze({
          _tag: 'OutOfBounds',
          index: exactIndex,
          length: selected.length,
          selector,
        })
      const index = Number(exactIndex)
      if (selected._tag === 'StaticViewValue') {
        const byte = selected.bytes.at(index)
        if (byte === undefined) throw new RangeError('MIR static view range exceeds its bytes')
        selected = Object.freeze({ _tag: 'IntegerValue', type: 'u8', value: BigInt(byte) })
      } else selected = access.sliceElement(selected, index)
      indexes.push(index)
      continue
    }
    if (selected._tag !== 'ArrayValue')
      throw new RangeError('MIR verifier allowed an element selector on a non-array value')
    const index =
      capturedIndexes.at(ordinal) ??
      (selector.index._tag === 'Proven'
        ? selector.index.value
        : Number(access.readIndex(selector.index.local)))
    if (index < 0 || !Number.isSafeInteger(index) || index >= selector.length)
      return Object.freeze({
        _tag: 'OutOfBounds',
        index: BigInt(index),
        length: selector.length,
        selector,
      })
    const element = selected.elements.at(index)
    if (element === undefined)
      throw new RangeError('MIR verifier allowed an incomplete array value')
    indexes.push(index)
    selected = element
  }
  return Object.freeze({ _tag: 'Resolved', selected, indexes: Object.freeze(indexes) })
}

/** Replaces one value at indexes previously established by `walkPlace`. */
export const replacePlaceByIndexes = (
  current: Value,
  selectors: ReadonlyArray<Mir.PlaceSelector>,
  indexes: ReadonlyArray<number>,
  replacement: Value,
  access: Access,
  depth = 0,
): Value => {
  const selector = selectors.at(depth)
  if (selector === undefined) return replacement
  const ordinal = indexes.at(depth)
  if (ordinal === undefined) throw new RangeError('Checked place omitted one selector index')
  if (selector._tag === 'FieldSelector') {
    if (current._tag !== 'AggregateValue') throw new RangeError('Invalid aggregate replacement')
    return Object.freeze({
      _tag: 'AggregateValue',
      type: current.type,
      fields: Object.freeze(
        current.fields.map((field) =>
          field.field.ordinal === selector.field.ordinal
            ? Object.freeze({
                field: field.field,
                value: replacePlaceByIndexes(
                  field.value,
                  selectors,
                  indexes,
                  replacement,
                  access,
                  depth + 1,
                ),
              })
            : field,
        ),
      ),
    })
  }
  if (selector._tag === 'SliceElementSelector') {
    if (current._tag !== 'SliceValue') throw new RangeError('Invalid slice replacement')
    const previous = access.sliceElement(current, ordinal)
    return access.replaceSliceElement(
      current,
      ordinal,
      replacePlaceByIndexes(previous, selectors, indexes, replacement, access, depth + 1),
    )
  }
  if (current._tag !== 'ArrayValue') throw new RangeError('Invalid array replacement')
  return Object.freeze({
    _tag: 'ArrayValue',
    type: current.type,
    elements: Object.freeze(
      current.elements.map((element, index) =>
        index === ordinal
          ? replacePlaceByIndexes(element, selectors, indexes, replacement, access, depth + 1)
          : element,
      ),
    ),
  })
}

/** Selects a previously checked structural path without consulting evaluator state. */
export const selectStored = (
  root: Value,
  selectors: NonNullable<SliceValue['selectors']>,
  indexes: ReadonlyArray<number>,
): Value => {
  const unsupported = (): never => {
    throw new RangeError('Stored place cannot evaluate a dynamic or nested slice selector')
  }
  const selected = walkPlace(root, selectors, indexes, {
    readIndex: unsupported,
    sliceElement: unsupported,
    replaceSliceElement: unsupported,
  })
  if (selected._tag === 'OutOfBounds')
    throw new RangeError('Stored slice place is outside its checked bounds')
  return selected.selected
}

/** Structural selector identity used by checked-place index tables. */
export const selectorKey = (selectors: ReadonlyArray<Mir.PlaceSelector>): string =>
  selectors
    .map((selector) =>
      selector._tag === 'FieldSelector'
        ? `field:${selector.field.struct.sourceId}:${selector.field.struct.ordinal}:${selector.field.ordinal}`
        : selector._tag === 'SliceElementSelector'
          ? `slice:${selector.index.ordinal}`
          : `array:${selector.index._tag === 'Proven' ? selector.index.value : selector.index.local.ordinal}`,
    )
    .join('/')
