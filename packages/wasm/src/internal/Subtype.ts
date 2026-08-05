/**
 * The specification's subtype judgment over value types, heap types, and composite types.
 *
 * Abstract hierarchies: `none <: i31/struct/array <: eq <: any`, `nofunc <: func`,
 * `noextern <: extern`, `noexn <: exn`; concrete types classify into a hierarchy by their
 * composite kind and relate to each other through declared supertype chains.
 *
 * @internal
 */
import type * as Type from '../Type.js'
import * as ValType from '../ValType.js'
import type * as ModuleState from './ModuleState.js'

type Hierarchy = 'any' | 'func' | 'extern' | 'exn'

const hierarchyOfKind: Record<ValType.AbstractHeapKind, Hierarchy> = {
  any: 'any',
  eq: 'any',
  i31: 'any',
  struct: 'any',
  array: 'any',
  none: 'any',
  func: 'func',
  nofunc: 'func',
  extern: 'extern',
  noextern: 'extern',
  exn: 'exn',
  noexn: 'exn',
}

const bottoms = new Set<ValType.AbstractHeapKind>(['none', 'nofunc', 'noextern', 'noexn'])

/** Proper abstract ancestors of each non-bottom abstract kind, including itself. */
const ancestors: Partial<
  Record<ValType.AbstractHeapKind, ReadonlyArray<ValType.AbstractHeapKind>>
> = {
  any: ['any'],
  eq: ['eq', 'any'],
  i31: ['i31', 'eq', 'any'],
  struct: ['struct', 'eq', 'any'],
  array: ['array', 'eq', 'any'],
  func: ['func'],
  extern: ['extern'],
  exn: ['exn'],
}

/**
 * The type index of a concrete handle.
 *
 * ponytail: linear scan over the type table; index concrete handles in a map if type-heavy
 * modules make subtyping measurably slow.
 */
const indexOfType = (state: TypeTable, handle: Type.Type): number =>
  state.typeHandles.indexOf(handle)

interface TypeTable {
  readonly types: ReadonlyArray<ModuleState.SubType>
  readonly typeHandles: ReadonlyArray<Type.Type>
}

const classify = (composite: ModuleState.CompositeType): ValType.AbstractHeapKind => {
  switch (composite._tag) {
    case 'Func':
      return 'func'
    case 'Struct':
      return 'struct'
    case 'Array':
      return 'array'
  }
}

/** @internal */
export const heapMatches = (
  state: TypeTable,
  sub: ValType.HeapType,
  sup: ValType.HeapType,
): boolean => {
  if (ValType.heapTypeEquals(sub, sup)) return true
  if (sub._tag === 'Abstract') {
    if (bottoms.has(sub.kind)) {
      const hierarchy = hierarchyOfKind[sub.kind]
      if (sup._tag === 'Abstract') return hierarchyOfKind[sup.kind] === hierarchy
      const composite = state.types[indexOfType(state, sup.type)]?.composite
      return composite !== undefined && hierarchyOfKind[classify(composite)] === hierarchy
    }
    if (sup._tag !== 'Abstract') return false
    return (ancestors[sub.kind] ?? []).includes(sup.kind)
  }
  const subIndex = indexOfType(state, sub.type)
  const subEntry = state.types[subIndex]
  if (subEntry === undefined) return false
  if (sup._tag === 'Abstract') {
    return (ancestors[classify(subEntry.composite)] ?? []).includes(sup.kind)
  }
  const supIndex = indexOfType(state, sup.type)
  let current: number | undefined = subIndex
  while (current !== undefined) {
    if (current === supIndex) return true
    current = state.types[current]?.supertype
  }
  return false
}

/** @internal */
export const matches = (state: TypeTable, sub: ValType.ValType, sup: ValType.ValType): boolean => {
  if (sub._tag === 'Ref' && sup._tag === 'Ref') {
    if (sub.nullable && !sup.nullable) return false
    return heapMatches(state, sub.heapType, sup.heapType)
  }
  return sub._tag === sup._tag
}

const storageMatches = (
  state: TypeTable,
  sub: ModuleState.StorageType,
  sup: ModuleState.StorageType,
): boolean => {
  const subPacked = '_tag' in sub && sub._tag === 'Packed'
  const supPacked = '_tag' in sup && sup._tag === 'Packed'
  if (subPacked || supPacked) {
    return subPacked && supPacked && sub.width === sup.width
  }
  return matches(state, sub as ValType.ValType, sup as ValType.ValType)
}

const fieldMatches = (
  state: TypeTable,
  sub: ModuleState.FieldType,
  sup: ModuleState.FieldType,
): boolean => {
  if (sub.mutable !== sup.mutable) return false
  if (sub.mutable) {
    return (
      storageMatches(state, sub.storage, sup.storage) &&
      storageMatches(state, sup.storage, sub.storage)
    )
  }
  return storageMatches(state, sub.storage, sup.storage)
}

/**
 * Structural matching between a definition and its declared supertype's composite.
 *
 * @internal
 */
export const compositeMatches = (
  state: TypeTable,
  sub: ModuleState.CompositeType,
  sup: ModuleState.CompositeType,
): boolean => {
  if (sub._tag === 'Func' && sup._tag === 'Func') {
    return (
      sub.params.length === sup.params.length &&
      sub.results.length === sup.results.length &&
      sup.params.every((param, index) => {
        const subParam = sub.params[index]
        return subParam !== undefined && matches(state, param, subParam)
      }) &&
      sub.results.every((result, index) => {
        const supResult = sup.results[index]
        return supResult !== undefined && matches(state, result, supResult)
      })
    )
  }
  if (sub._tag === 'Struct' && sup._tag === 'Struct') {
    return (
      sub.fields.length >= sup.fields.length &&
      sup.fields.every((field, index) => {
        const subField = sub.fields[index]
        return subField !== undefined && fieldMatches(state, subField, field)
      })
    )
  }
  if (sub._tag === 'Array' && sup._tag === 'Array') {
    return fieldMatches(state, sub.field, sup.field)
  }
  return false
}
