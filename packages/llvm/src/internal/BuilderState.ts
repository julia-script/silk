import * as Effect from 'effect/Effect'
import type * as Semaphore from 'effect/Semaphore'
import type * as Alias from '../Alias.js'
import type * as Attribute from '../Attribute.js'
import type * as Builder from '../Builder.js'
import type * as ByteString from '../ByteString.js'
import type * as Constant from '../Constant.js'
import type * as DataLayout from '../DataLayout.js'
import type * as FunctionActor from '../Function.js'
import type * as Global from '../Global.js'
import { SilkError } from '../SilkError.js'
import type * as Type from '../Type.js'
import type * as Variable from '../Variable.js'
import type * as AttributeDescription from './AttributeDescription.js'
import type * as ConstantDescription from './ConstantDescription.js'
import type * as GlobalDescription from './GlobalDescription.js'
import type * as MetadataDescription from './MetadataDescription.js'
import type * as OwnedHandle from './OwnedHandle.js'
import type * as TypeDescription from './TypeDescription.js'

export interface MutableState {
  strip: boolean
  moduleName: ByteString.ByteString
  sourceFilename: ByteString.ByteString
  targetTriple: ByteString.ByteString
  dataLayout: ByteString.ByteString
  layout: DataLayout.DataLayout
  moduleAssembly: Array<ByteString.ByteString>
  nextHandle: number
  strings: Array<ByteString.ByteString>
  stringKeys: Map<string, number>
  types: Array<TypeDescription.Description>
  typeHandles: Array<Type.Type>
  typeKeys: Map<string, number>
  namedTypes: Map<string, number>
  attributes: Array<AttributeDescription.Description>
  attributeHandles: Array<Attribute.Attribute>
  attributeKeys: Map<string, number>
  attributeSets: Array<ReadonlyArray<number>>
  attributeSetHandles: Array<Attribute.Set>
  attributeSetKeys: Map<string, number>
  functionAttributeSets: Array<AttributeDescription.FunctionSetDescription>
  functionAttributeSetHandles: Array<Attribute.FunctionSet>
  functionAttributeSetKeys: Map<string, number>
  constants: Array<ConstantDescription.Description>
  constantHandles: Array<Constant.Constant>
  constantKeys: Map<string, number>
  globals: Array<GlobalDescription.GlobalDescription>
  globalHandles: Array<Global.Global>
  globalNames: Map<string, number>
  nextAnonymousGlobal: number
  variables: Array<GlobalDescription.VariableDescription>
  variableHandles: Array<Variable.Variable>
  aliases: Array<GlobalDescription.AliasDescription>
  aliasHandles: Array<Alias.Alias>
  functions: Array<GlobalDescription.FunctionDescription>
  functionHandles: Array<FunctionActor.Function>
  buildingFunctions: Set<number>
  metadata: Array<MetadataDescription.Entry>
  metadataHandles: Array<import('../Metadata.js').Metadata>
  metadataStringKeys: Map<string, number>
  metadataNodeKeys: Map<string, number>
  namedMetadata: Array<MetadataDescription.Named>
  namedMetadataKeys: Map<string, number>
  globalMetadata: Array<ReadonlyArray<MetadataDescription.Attachment>>
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
  readonly nextHandle: number
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
const lookup = (self: Builder.Builder, operation: string): Effect.Effect<State, SilkError> => {
  const state = states.get(self)
  return state === undefined
    ? Effect.fail(new SilkError({ operation, message: 'Unknown LLVM builder value', cause: self }))
    : Effect.succeed(state)
}

/** @internal */
export const mutate = <A>(
  self: Builder.Builder,
  operation: string,
  transition: (state: MutableState, owner: OwnedHandle.Owner) => A,
): Effect.Effect<A, SilkError> =>
  Effect.flatMap(lookup(self, operation), (state) =>
    state.gate.withPermit(
      Effect.try({
        try: () => transition(state.value, state.owner),
        catch: (cause) =>
          cause instanceof SilkError
            ? cause
            : new SilkError({ operation, message: 'LLVM builder mutation failed', cause }),
      }),
    ),
  )

/** @internal */
export const snapshot = (
  self: Builder.Builder,
  operation: string,
): Effect.Effect<Snapshot, SilkError> =>
  Effect.flatMap(lookup(self, operation), (state) =>
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
        nextHandle: state.value.nextHandle,
        strings: Object.freeze([...state.value.strings]),
        types: Object.freeze([...state.value.types]),
        typeHandles: Object.freeze([...state.value.typeHandles]),
        attributes: Object.freeze([...state.value.attributes]),
        attributeSets: Object.freeze([...state.value.attributeSets]),
        functionAttributeSets: Object.freeze([...state.value.functionAttributeSets]),
        constants: Object.freeze([...state.value.constants]),
        constantHandles: Object.freeze([...state.value.constantHandles]),
        globals: Object.freeze([...state.value.globals]),
        globalHandles: Object.freeze([...state.value.globalHandles]),
        variables: Object.freeze([...state.value.variables]),
        aliases: Object.freeze([...state.value.aliases]),
        functions: Object.freeze([...state.value.functions]),
        metadata: Object.freeze([...state.value.metadata]),
        namedMetadata: Object.freeze([...state.value.namedMetadata]),
        globalMetadata: Object.freeze(
          state.value.globalMetadata.map((attachments) => Object.freeze([...attachments])),
        ),
      })),
    ),
  )
