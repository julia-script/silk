import * as Effect from 'effect/Effect'
import type * as Result from 'effect/Result'
import type * as Semaphore from 'effect/Semaphore'
import type * as Builder from '../Builder.js'
import type * as DataActor from '../Data.js'
import type * as Elem from '../Elem.js'
import type * as Func from '../Func.js'
import type * as Global from '../Global.js'
import type * as Instr from '../Instr.js'
import type * as Memory from '../Memory.js'
import type * as Table from '../Table.js'
import type * as Tag from '../Tag.js'
import type * as Type from '../Type.js'
import type * as ValType from '../ValType.js'
import { invalidState, type WasmError } from '../WasmError.js'
import type * as OwnedHandle from './OwnedHandle.js'

export interface FuncType {
  readonly params: ReadonlyArray<ValType.ValType>
  readonly results: ReadonlyArray<ValType.ValType>
}

export type StorageType =
  | ValType.ValType
  | { readonly _tag: 'Packed'; readonly width: 'i8' | 'i16' }

export interface FieldType {
  readonly storage: StorageType
  readonly mutable: boolean
}

export type CompositeType =
  | {
      readonly _tag: 'Func'
      readonly params: ReadonlyArray<ValType.ValType>
      readonly results: ReadonlyArray<ValType.ValType>
    }
  | { readonly _tag: 'Struct'; readonly fields: ReadonlyArray<FieldType> }
  | { readonly _tag: 'Array'; readonly field: FieldType }

export interface SubType {
  readonly composite: CompositeType
  readonly supertype: number | undefined
  readonly final: boolean
  readonly name: string | undefined
}

export interface RecGroup {
  readonly start: number
  readonly length: number
}

/** Unwraps a function composite at a type index, or undefined for non-function types. */
export const funcTypeAt = (types: ReadonlyArray<SubType>, index: number): FuncType | undefined => {
  const composite = types[index]?.composite
  return composite?._tag === 'Func'
    ? { params: composite.params, results: composite.results }
    : undefined
}

export interface Limits {
  readonly min: bigint
  readonly max: bigint | undefined
}

export type AddressType = 'i32' | 'i64'

export interface ImportSource {
  readonly module: string
  readonly field: string
}

export interface LocalDeclaration {
  readonly type: ValType.ValType
  readonly name: string | undefined
}

export interface FuncDefinition {
  readonly locals: ReadonlyArray<LocalDeclaration>
  readonly body: ReadonlyArray<Instr.Instr>
  /** Entry indices of functions referenced by `ref.func` inside the body. */
  readonly refFuncs: ReadonlySet<number>
  /** Whether the body uses `memory.init` or `data.drop`, requiring a data count section. */
  readonly usesDataCount: boolean
}

export interface FuncEntry {
  readonly typeIndex: number
  readonly name: string | undefined
  readonly importSource: ImportSource | undefined
  definition: FuncDefinition | undefined
}

export interface TableEntry {
  readonly refType: ValType.RefType
  readonly limits: Limits
  readonly addressType: AddressType
  readonly name: string | undefined
  readonly importSource: ImportSource | undefined
}

export interface MemoryEntry {
  readonly limits: Limits
  readonly shared: boolean
  readonly addressType: AddressType
  readonly name: string | undefined
  readonly importSource: ImportSource | undefined
}

export interface TagEntry {
  readonly typeIndex: number
  readonly name: string | undefined
  readonly importSource: ImportSource | undefined
}

export interface GlobalEntry {
  readonly valType: ValType.ValType
  readonly mutable: boolean
  readonly name: string | undefined
  readonly importSource: ImportSource | undefined
  readonly init: ReadonlyArray<Instr.Instr> | undefined
}

export type ElemMode =
  | { readonly _tag: 'Active'; readonly table: number; readonly offset: ReadonlyArray<Instr.Instr> }
  | { readonly _tag: 'Passive' }
  | { readonly _tag: 'Declarative' }

export interface ElemEntry {
  readonly refType: ValType.RefType
  readonly mode: ElemMode
  readonly items: ReadonlyArray<ReadonlyArray<Instr.Instr>>
  readonly name: string | undefined
}

export type DataMode =
  | {
      readonly _tag: 'Active'
      readonly memory: number
      readonly offset: ReadonlyArray<Instr.Instr>
    }
  | { readonly _tag: 'Passive' }

export interface DataEntry {
  readonly mode: DataMode
  readonly bytes: Uint8Array
  readonly name: string | undefined
}

export type ExportKind = 'func' | 'table' | 'memory' | 'global' | 'tag'

export interface ExportEntry {
  readonly name: string
  readonly kind: ExportKind
  readonly entryIndex: number
}

export interface MutableState {
  moduleName: string | undefined
  types: Array<SubType>
  typeHandles: Array<Type.Type>
  typeKeys: Map<string, number>
  recGroups: Array<RecGroup>
  funcs: Array<FuncEntry>
  funcHandles: Array<Func.Func>
  tables: Array<TableEntry>
  tableHandles: Array<Table.Table>
  memories: Array<MemoryEntry>
  memoryHandles: Array<Memory.Memory>
  globals: Array<GlobalEntry>
  globalHandles: Array<Global.Global>
  tags: Array<TagEntry>
  tagHandles: Array<Tag.Tag>
  elems: Array<ElemEntry>
  elemHandles: Array<Elem.Elem>
  datas: Array<DataEntry>
  dataHandles: Array<DataActor.Data>
  exports: Array<ExportEntry>
  exportNames: Set<string>
  start: number | undefined
}

export interface State {
  readonly owner: OwnedHandle.Owner
  readonly gate: Semaphore.Semaphore
  readonly value: MutableState
}

export interface Snapshot {
  readonly owner: OwnedHandle.Owner
  readonly moduleName: string | undefined
  readonly types: ReadonlyArray<SubType>
  readonly typeHandles: ReadonlyArray<Type.Type>
  readonly recGroups: ReadonlyArray<RecGroup>
  readonly funcs: ReadonlyArray<FuncEntry>
  readonly tables: ReadonlyArray<TableEntry>
  readonly memories: ReadonlyArray<MemoryEntry>
  readonly globals: ReadonlyArray<GlobalEntry>
  readonly tags: ReadonlyArray<TagEntry>
  readonly elems: ReadonlyArray<ElemEntry>
  readonly datas: ReadonlyArray<DataEntry>
  readonly exports: ReadonlyArray<ExportEntry>
  readonly start: number | undefined
}

const states = new WeakMap<Builder.Builder, State>()

/** @internal */
export const register = (self: Builder.Builder, state: State): void => {
  states.set(self, state)
}

/** @internal */
const lookup = Effect.fnUntraced(function* (
  self: Builder.Builder,
  operation: string,
): Effect.fn.Return<State, WasmError> {
  const state = states.get(self)
  if (state === undefined) {
    return yield* Effect.fail(
      invalidState({ operation, message: 'Unknown WebAssembly builder value', state: self }),
    )
  }
  return state
})

/** @internal */
export const mutate = Effect.fnUntraced(function* <A>(
  self: Builder.Builder,
  operation: string,
  transition: (state: MutableState, owner: OwnedHandle.Owner) => Result.Result<A, WasmError>,
): Effect.fn.Return<A, WasmError> {
  return yield* Effect.flatMap(lookup(self, operation), (state) =>
    state.gate.withPermit(
      Effect.suspend(() => Effect.fromResult(transition(state.value, state.owner))),
    ),
  )
})

/** @internal */
export const snapshot = Effect.fnUntraced(function* (
  self: Builder.Builder,
  operation: string,
): Effect.fn.Return<Snapshot, WasmError> {
  return yield* Effect.flatMap(lookup(self, operation), (state) =>
    state.gate.withPermit(
      Effect.sync(() => ({
        owner: state.owner,
        moduleName: state.value.moduleName,
        types: Object.freeze([...state.value.types]),
        typeHandles: Object.freeze([...state.value.typeHandles]),
        recGroups: Object.freeze([...state.value.recGroups]),
        funcs: Object.freeze([...state.value.funcs]),
        tables: Object.freeze([...state.value.tables]),
        memories: Object.freeze([...state.value.memories]),
        globals: Object.freeze([...state.value.globals]),
        tags: Object.freeze([...state.value.tags]),
        elems: Object.freeze([...state.value.elems]),
        datas: Object.freeze([...state.value.datas]),
        exports: Object.freeze([...state.value.exports]),
        start: state.value.start,
      })),
    ),
  )
})
