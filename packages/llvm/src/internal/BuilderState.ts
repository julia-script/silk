import * as Effect from 'effect/Effect'
import type * as Result from 'effect/Result'
import type * as Semaphore from 'effect/Semaphore'
import type * as Attribute from '../Attribute.js'
import type * as Builder from '../Builder.js'
import type * as ByteString from '../ByteString.js'
import type * as Constant from '../Constant.js'
import type * as DataLayout from '../DataLayout.js'
import type * as Global from '../Global.js'
import { invalidState, type LlvmError } from '../LlvmError.js'
import type * as Type from '../Type.js'
import type * as AttributeDescription from './AttributeDescription.js'
import type * as ConstantDescription from './ConstantDescription.js'
import type * as GlobalDescription from './GlobalDescription.js'
import * as GlobalTable from './GlobalTable.js'
import type * as MetadataDescription from './MetadataDescription.js'
import * as MetadataTable from './MetadataTable.js'
import type * as OwnedHandle from './OwnedHandle.js'
import * as Table from './Table.js'
import type * as TypeDescription from './TypeDescription.js'

export interface MutableState {
  strip: boolean
  moduleName: ByteString.ByteString
  sourceFilename: ByteString.ByteString
  targetTriple: ByteString.ByteString
  dataLayout: ByteString.ByteString
  layout: DataLayout.DataLayout
  moduleAssembly: Array<ByteString.ByteString>
  strings: Array<ByteString.ByteString>
  stringKeys: Map<string, number>
  types: Table.Table<TypeDescription.Description, Type.Type>
  namedTypes: Map<string, number>
  attributes: Table.Table<AttributeDescription.Description, Attribute.Attribute>
  attributeSets: Table.Table<ReadonlyArray<number>, Attribute.Set>
  functionAttributeSets: Table.Table<
    AttributeDescription.FunctionSetDescription,
    Attribute.FunctionSet
  >
  constants: Table.Table<ConstantDescription.Description, Constant.Constant>
  globals: GlobalTable.GlobalTable
  buildingFunctions: Set<number>
  metadata: MetadataTable.MetadataTable
}

export interface State {
  readonly owner: OwnedHandle.Owner
  readonly gate: Semaphore.Semaphore
  readonly value: MutableState
}

export interface Snapshot {
  readonly owner: OwnedHandle.Owner
  readonly strip: boolean
  readonly moduleName: ByteString.ByteString
  readonly sourceFilename: ByteString.ByteString
  readonly targetTriple: ByteString.ByteString
  readonly dataLayout: ByteString.ByteString
  readonly layout: DataLayout.DataLayout
  readonly moduleAssembly: ReadonlyArray<ByteString.ByteString>
  readonly strings: ReadonlyArray<ByteString.ByteString>
  readonly types: ReadonlyArray<TypeDescription.Description>
  readonly typeHandles: ReadonlyArray<Type.Type>
  readonly attributes: ReadonlyArray<AttributeDescription.Description>
  readonly attributeSets: ReadonlyArray<ReadonlyArray<number>>
  readonly functionAttributeSets: ReadonlyArray<AttributeDescription.FunctionSetDescription>
  readonly constants: ReadonlyArray<ConstantDescription.Description>
  readonly constantHandles: ReadonlyArray<Constant.Constant>
  readonly globals: ReadonlyArray<GlobalDescription.GlobalDescription>
  readonly globalHandles: ReadonlyArray<Global.Global>
  readonly variables: ReadonlyArray<GlobalDescription.VariableDescription>
  readonly aliases: ReadonlyArray<GlobalDescription.AliasDescription>
  readonly functions: ReadonlyArray<GlobalDescription.FunctionDescription>
  readonly metadata: ReadonlyArray<MetadataDescription.Entry>
  readonly namedMetadata: ReadonlyArray<MetadataDescription.Named>
  readonly globalMetadata: ReadonlyArray<ReadonlyArray<MetadataDescription.Attachment>>
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
): Effect.fn.Return<State, LlvmError> {
  const state = states.get(self)
  if (state === undefined) {
    return yield* Effect.fail(
      invalidState({ operation, message: 'Unknown LLVM builder value', state: self }),
    )
  }
  return state
})

/** @internal */
export const mutate = Effect.fnUntraced(function* <A>(
  self: Builder.Builder,
  operation: string,
  transition: (state: MutableState, owner: OwnedHandle.Owner) => Result.Result<A, LlvmError>,
): Effect.fn.Return<A, LlvmError> {
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
): Effect.fn.Return<Snapshot, LlvmError> {
  return yield* Effect.flatMap(lookup(self, operation), (state) =>
    state.gate.withPermit(
      Effect.sync(() => ({
        owner: state.owner,
        strip: state.value.strip,
        moduleName: state.value.moduleName,
        sourceFilename: state.value.sourceFilename,
        targetTriple: state.value.targetTriple,
        dataLayout: state.value.dataLayout,
        layout: state.value.layout,
        moduleAssembly: Object.freeze([...state.value.moduleAssembly]),
        strings: Object.freeze([...state.value.strings]),
        types: Table.freeze(state.value.types).descriptions,
        typeHandles: Table.freeze(state.value.types).handles,
        attributes: Table.freeze(state.value.attributes).descriptions,
        attributeSets: Table.freeze(state.value.attributeSets).descriptions,
        functionAttributeSets: Table.freeze(state.value.functionAttributeSets).descriptions,
        constants: Table.freeze(state.value.constants).descriptions,
        constantHandles: Table.freeze(state.value.constants).handles,
        globals: GlobalTable.freeze(state.value.globals).globals,
        globalHandles: GlobalTable.freeze(state.value.globals).globalHandles,
        variables: GlobalTable.freeze(state.value.globals).variables,
        aliases: GlobalTable.freeze(state.value.globals).aliases,
        functions: GlobalTable.freeze(state.value.globals).functions,
        metadata: MetadataTable.freeze(state.value.metadata).descriptions,
        namedMetadata: MetadataTable.freeze(state.value.metadata).named,
        globalMetadata: GlobalTable.freeze(state.value.globals).attachments,
      })),
    ),
  )
})
