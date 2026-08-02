import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import { code as attributeKindCode } from './internal/AttributeKind.js'
import * as BitcodeSchema from './internal/BitcodeSchema.js'
import * as Bitstream from './internal/Bitstream.js'
import * as BuilderState from './internal/BuilderState.js'
import * as CanonicalKey from './internal/CanonicalKey.js'
import * as CoreSchema from './internal/CoreBitcodeSchema.js'
import * as DeclarationSchema from './internal/DeclarationBitcodeSchema.js'
import type * as FunctionBodyDescription from './internal/FunctionBodyDescription.js'
import type * as GlobalDescription from './internal/GlobalDescription.js'
import type * as MetadataDescription from './internal/MetadataDescription.js'
import * as MemoryAccess from './MemoryAccess.js'
import * as Metadata from './Metadata.js'
import { SilkError } from './SilkError.js'

/**
 * Identification record embedded in generated LLVM bitcode.
 *
 * @category encoding
 * @since 0.0.0
 */
export interface Producer {
  readonly name: string
  readonly major: number
  readonly minor: number
  readonly patch: number
}

/**
 * Optional controls for direct bitcode encoding.
 *
 * @category encoding
 * @since 0.0.0
 */
export interface Options {
  readonly producer?: Producer
}

const defaultProducer: Producer = {
  name: 'silk-effect',
  major: 0,
  minor: 0,
  patch: 0,
}

/** @internal */
const writeIdentification = (writer: Bitstream.Writer, producer: Producer): void => {
  const identification = Bitstream.enterBlock(writer, BitcodeSchema.Identification)
  const producerName = ByteString.fromString(
    `${producer.name} ${producer.major}.${producer.minor}.${producer.patch}`,
  )
  Bitstream.writeRecord(identification, BitcodeSchema.IdentificationVersion, [producerName.bytes])
  Bitstream.writeRecord(identification, BitcodeSchema.IdentificationEpoch, [0])
  Bitstream.endBlock(identification)
}

/** @internal */
const writeFoundationString = (
  block: Bitstream.BlockWriter,
  code: number,
  value: ByteString.ByteString,
): void => {
  if (ByteString.isEmpty(value)) return
  Bitstream.writeRecord(block, BitcodeSchema.ModuleString, [code, value.bytes])
}

/** @internal */
const encodeFoundation = (state: BuilderState.Snapshot, producer: Producer): Uint8Array => {
  const writer = Bitstream.make()
  Bitstream.writeBits(writer, BitcodeSchema.Magic, 32)
  writeIdentification(writer, producer)
  const module = Bitstream.enterBlock(writer, BitcodeSchema.Module)
  Bitstream.writeRecord(module, BitcodeSchema.ModuleVersion, [])
  writeFoundationString(module, BitcodeSchema.ModuleCode.Triple, state.targetTriple)
  writeFoundationString(module, BitcodeSchema.ModuleCode.DataLayout, state.dataLayout)
  writeFoundationString(module, BitcodeSchema.ModuleCode.SourceFilename, state.sourceFilename)
  for (const assembly of state.moduleAssembly) {
    writeFoundationString(module, BitcodeSchema.ModuleCode.Assembly, assembly)
  }
  Bitstream.endBlock(module)
  const stringTable = Bitstream.enterBlock(writer, BitcodeSchema.Strtab)
  Bitstream.writeRecord(stringTable, BitcodeSchema.StrtabBlob, [[]])
  Bitstream.endBlock(stringTable)
  return Bitstream.toUint8Array(writer)
}

/** @internal */
const bitWidth = (count: number): number => Math.max(1, Math.ceil(Math.log2(Math.max(2, count))))

const simpleTypeCode: Readonly<Record<string, number>> = Object.freeze({
  Void: 2,
  Float: 3,
  Double: 4,
  Label: 5,
  Half: 10,
  X86Fp80: 13,
  Fp128: 14,
  PpcFp128: 15,
  Metadata: 16,
  Token: 22,
  BFloat: 23,
  X86Amx: 24,
})

/** @internal */
const writeTypes = (
  module: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
  width: number,
): void => {
  if (state.types.length === 0) return
  const schema = DeclarationSchema.type(width)
  const block = Bitstream.enterBlock(module.writer, schema.block, module.abbrevWidth)
  Bitstream.writeRecord(block, schema.numEntry, [state.types.length])
  for (const description of state.types) {
    switch (description._tag) {
      case 'Simple': {
        const code = simpleTypeCode[description.tag]
        if (code === undefined) throw new Error(`unsupported simple type ${description.tag}`)
        Bitstream.writeRecord(block, schema.simple, [code])
        break
      }
      case 'Integer':
        Bitstream.writeRecord(block, schema.integer, [description.bitWidth])
        break
      case 'Pointer':
        Bitstream.writeRecord(block, schema.pointer, [description.addressSpace.value])
        break
      case 'Function':
        Bitstream.writeRecord(block, schema.functionType, [
          description.variadic ? 1 : 0,
          description.returnType,
          description.parameters,
        ])
        break
      case 'Vector':
        Bitstream.writeRecord(block, schema.vector, [description.length, description.child])
        break
      case 'Array':
        Bitstream.writeRecord(block, schema.array, [description.length, description.child])
        break
      case 'Structure':
        Bitstream.writeRecord(block, schema.structAnon, [
          description.packed ? 1 : 0,
          description.fields,
        ])
        break
      case 'NamedStructure':
        Bitstream.writeRecord(block, schema.structName, [description.name.bytes])
        if (description.body === undefined) {
          Bitstream.writeRecord(block, schema.opaque, [])
        } else {
          Bitstream.writeRecord(block, schema.structNamed, [
            description.body.packed ? 1 : 0,
            description.body.fields,
          ])
        }
        break
      case 'TargetExtension':
        Bitstream.writeRecord(block, schema.structName, [description.name.bytes])
        Bitstream.writeRecord(block, schema.target, [
          description.types.length,
          description.types,
          description.integers,
        ])
        break
    }
  }
  Bitstream.endBlock(block)
}

/** @internal */
const attributeName = (bytes: ByteString.ByteString): string =>
  bytes.bytes.map((byte) => String.fromCharCode(byte)).join('')

/** @internal */
const stringAttribute = (
  name: ByteString.ByteString,
  value: ByteString.ByteString | undefined,
): ReadonlyArray<Bitstream.Scalar> => [
  value === undefined ? 3 : 4,
  ...name.bytes,
  0,
  ...(value?.bytes ?? []),
  ...(value === undefined ? [] : [0]),
]

/** @internal */
const encodeAttribute = (
  description: BuilderState.Snapshot['attributes'][number],
): ReadonlyArray<Bitstream.Scalar> => {
  const name = attributeName(description.name)
  const code = attributeKindCode[name]
  if (description._tag === 'String') return stringAttribute(description.name, description.value)
  if (description._tag === 'IntegerList') {
    return stringAttribute(
      description.name,
      ByteString.fromString(description.values.map(String).join(',')),
    )
  }
  if (code === undefined) return stringAttribute(description.name, undefined)
  if (description._tag === 'Flag') return [0, code]
  if (description._tag === 'Integer') return [1, code, description.value]
  return [6, code, description.type]
}

interface AttributeGroups {
  readonly groupIdsByFunctionSet: ReadonlyArray<ReadonlyArray<number>>
}

/** @internal */
const writeAttributes = (
  module: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
): AttributeGroups => {
  const groups: Array<{ readonly set: number; readonly position: number }> = []
  const groupIndex = new Map<string, number>()
  const groupIdsByFunctionSet = state.functionAttributeSets.map((functionSet) => {
    const positioned = [
      { set: functionSet.functionAttributes, position: 0xffff_ffff },
      { set: functionSet.returnAttributes, position: 0 },
      ...functionSet.parameterAttributes.map((set, index) => ({ set, position: index + 1 })),
    ]
    const ids: Array<number> = []
    for (const entry of positioned) {
      const values = state.attributeSets[entry.set]
      if (values === undefined || values.length === 0) continue
      const key = `${entry.set}:${entry.position}`
      let id = groupIndex.get(key)
      if (id === undefined) {
        id = groups.length
        groupIndex.set(key, id)
        groups.push(entry)
      }
      ids.push(id)
    }
    return Object.freeze(ids)
  })

  if (groups.length > 0) {
    const block = Bitstream.enterBlock(
      module.writer,
      DeclarationSchema.paramattrGroup,
      module.abbrevWidth,
    )
    for (let id = 0; id < groups.length; id += 1) {
      const group = groups[id]
      if (group === undefined) continue
      const attributes = state.attributeSets[group.set] ?? []
      const values: Array<Bitstream.Scalar> = [id, group.position]
      for (const attributeIndex of attributes) {
        const description = state.attributes[attributeIndex]
        if (description === undefined) throw new Error('missing attribute')
        values.push(...encodeAttribute(description))
      }
      Bitstream.writeUnabbreviatedRecord(block, 3, values)
    }
    Bitstream.endBlock(block)
  }

  if (state.functionAttributeSets.length > 0) {
    const block = Bitstream.enterBlock(
      module.writer,
      DeclarationSchema.paramattr,
      module.abbrevWidth,
    )
    for (const ids of groupIdsByFunctionSet) {
      Bitstream.writeRecord(block, DeclarationSchema.paramattrEntry, [ids])
    }
    Bitstream.endBlock(block)
  }
  return { groupIdsByFunctionSet }
}

/** @internal */
const activeGlobal = (
  state: BuilderState.Snapshot,
  kind: GlobalDescription.GlobalDescription['kind'],
  actorIndex: number,
): GlobalDescription.GlobalDescription | undefined =>
  state.globals.find(
    (global) =>
      !global.deleted &&
      global.replacement === undefined &&
      global.kind === kind &&
      global.actorIndex === actorIndex,
  )

interface GlobalOrder {
  readonly entries: ReadonlyArray<{
    readonly global: GlobalDescription.GlobalDescription
    readonly globalIndex: number
  }>
  readonly valueIndex: ReadonlyMap<number, number>
  readonly strtab: ReadonlyMap<number, { readonly offset: number; readonly size: number }>
  readonly bytes: ReadonlyArray<number>
}

/** @internal */
const buildGlobalOrder = (state: BuilderState.Snapshot): GlobalOrder => {
  const entries: Array<GlobalOrder['entries'][number]> = []
  const add = (global: GlobalDescription.GlobalDescription | undefined): void => {
    if (global === undefined) return
    const globalIndex = state.globals.indexOf(global)
    if (globalIndex >= 0) entries.push({ global, globalIndex })
  }
  state.variables.forEach((_value, index) => {
    add(activeGlobal(state, 'Variable', index))
  })
  state.functions.forEach((_value, index) => {
    add(activeGlobal(state, 'Function', index))
  })
  state.aliases.forEach((_value, index) => {
    add(activeGlobal(state, 'Alias', index))
  })
  const valueIndex = new Map<number, number>()
  const strtab = new Map<number, { readonly offset: number; readonly size: number }>()
  const bytes: Array<number> = []
  for (let index = 0; index < entries.length; index += 1) {
    const entry = entries[index]
    if (entry === undefined) continue
    valueIndex.set(entry.globalIndex, index)
    strtab.set(entry.globalIndex, { offset: bytes.length, size: entry.global.name.bytes.length })
    bytes.push(...entry.global.name.bytes)
  }
  return { entries: Object.freeze(entries), valueIndex, strtab, bytes: Object.freeze(bytes) }
}

/** @internal */
const alignmentCode = (alignment: GlobalDescription.Common['alignment']): number => {
  if (alignment.byteUnits === undefined) return 0
  let value = alignment.byteUnits
  let exponent = 0
  while (value > 1n) {
    value >>= 1n
    exponent += 1
  }
  return exponent + 1
}

const linkageCode: Readonly<Record<GlobalDescription.Linkage, number>> = Object.freeze({
  external: 0,
  weak: 1,
  appending: 2,
  internal: 3,
  linkonce: 4,
  extern_weak: 7,
  common: 8,
  private: 9,
  weak_odr: 10,
  linkonce_odr: 11,
  available_externally: 12,
})

const visibilityCode: Readonly<Record<GlobalDescription.Visibility, number>> = Object.freeze({
  default: 0,
  hidden: 1,
  protected: 2,
})

const dllCode: Readonly<Record<GlobalDescription.DllStorage, number>> = Object.freeze({
  default: 0,
  dllimport: 1,
  dllexport: 2,
})

const unnamedCode: Readonly<Record<GlobalDescription.UnnamedAddress, number>> = Object.freeze({
  none: 0,
  unnamed_addr: 1,
  local_unnamed_addr: 2,
})

const threadLocalCode: Readonly<Record<GlobalDescription.ThreadLocalModel, number>> = Object.freeze(
  {
    none: 0,
    generaldynamic: 1,
    localdynamic: 2,
    initialexec: 3,
    localexec: 4,
  },
)

/** @internal */
const sectionIndices = (
  module: Bitstream.BlockWriter,
  schema: DeclarationSchema.ModuleSchema,
  order: GlobalOrder,
): ReadonlyMap<string, number> => {
  const indices = new Map<string, number>()
  for (const { global } of order.entries) {
    if (ByteString.isEmpty(global.section)) continue
    const key = CanonicalKey.bytes(global.section)
    if (indices.has(key)) continue
    indices.set(key, indices.size + 1)
    Bitstream.writeRecord(module, schema.string, [5, global.section.bytes])
  }
  return indices
}

interface ConstantAdapter {
  readonly local: ReadonlyArray<number>
  readonly localIndex: ReadonlyMap<number, number>
  readonly valueIndex: (constant: number) => number
}

interface MetadataAdapter {
  readonly reachable: Metadata.Reachable
  readonly entries: ReadonlyArray<number>
  readonly indices: ReadonlyMap<number, number>
  readonly index: (metadata: number) => number
  readonly optional: (metadata: number | undefined) => number
}

/** @internal */
const buildMetadataAdapter = (state: BuilderState.Snapshot): MetadataAdapter => {
  const reachable = Metadata.reachable(state, 'Bitcode.encode')
  const strings = reachable.entries.filter((index) => state.metadata[index]?._tag === 'String')
  const nodes = reachable.entries.filter((index) => state.metadata[index]?._tag === 'Node')
  const entries = Object.freeze([...strings, ...nodes])
  const indices = new Map(entries.map((entry, index) => [entry, index]))
  const resolve = (metadata: number): number => {
    const seen = new Set<number>()
    let current = metadata
    while (true) {
      if (seen.has(current)) throw new Error('metadata forward-reference cycle')
      seen.add(current)
      const target = reachable.resolved.get(current)
      if (target === undefined) return current
      current = target
    }
  }
  const index = (metadata: number): number => {
    const value = indices.get(resolve(metadata))
    if (value === undefined) throw new Error(`metadata ${metadata} has no bitcode index`)
    return value
  }
  return {
    reachable,
    entries,
    indices,
    index,
    optional: (metadata) => (metadata === undefined ? 0 : index(metadata) + 1),
  }
}

/** @internal */
const buildConstantAdapter = (
  state: BuilderState.Snapshot,
  order: GlobalOrder,
): ConstantAdapter => {
  const local: Array<number> = []
  const localIndex = new Map<number, number>()
  state.constants.forEach((constant, index) => {
    if (constant._tag === 'Global') return
    localIndex.set(index, local.length)
    local.push(index)
  })
  const valueIndex = (constantIndex: number): number => {
    const constant = state.constants[constantIndex]
    if (constant === undefined) throw new Error('missing constant')
    if (constant._tag === 'Global') {
      const index = order.valueIndex.get(constant.global)
      if (index === undefined) throw new Error('global constant is not active')
      return index
    }
    const index = localIndex.get(constantIndex)
    if (index === undefined) throw new Error('local constant index is missing')
    return order.entries.length + index
  }
  return { local: Object.freeze(local), localIndex, valueIndex }
}

/** @internal */
const littleEndianInteger = (bytes: ByteString.ByteString): bigint => {
  let result = 0n
  for (let index = 0; index < bytes.bytes.length; index += 1) {
    result |= BigInt(bytes.bytes[index] ?? 0) << BigInt(index * 8)
  }
  return result
}

/** @internal */
const integerRecordValues = (
  bitPattern: bigint,
  bitWidth: number,
  signed: boolean,
): { readonly narrow: bigint | undefined; readonly wide: ReadonlyArray<bigint> } => {
  const modulus = 1n << BigInt(bitWidth)
  const sign = 1n << BigInt(bitWidth - 1)
  const value = signed && (bitPattern & sign) !== 0n ? bitPattern - modulus : bitPattern
  if (bitWidth <= 64 && value >= -(1n << 63n) && value <= (1n << 62n) - 1n) {
    return {
      narrow: value >= 0n ? value << 1n : (-value << 1n) | 1n,
      wide: [],
    }
  }
  const limbs: Array<bigint> = []
  const count = Math.ceil(bitWidth / 64)
  for (let index = 0; index < count; index += 1) {
    const limb = (bitPattern >> BigInt(index * 64)) & 0xffff_ffff_ffff_ffffn
    const signedLimb = (limb & (1n << 63n)) === 0n ? limb : limb - (1n << 64n)
    limbs.push(signedLimb >= 0n ? signedLimb << 1n : (-signedLimb << 1n) | 1n)
  }
  return { narrow: undefined, wide: Object.freeze(limbs) }
}

const castOpcode: Readonly<Record<string, number>> = Object.freeze({
  trunc: 0,
  ptrtoint: 9,
  inttoptr: 10,
  bitcast: 11,
  addrspacecast: 12,
})

const binaryOpcode: Readonly<Record<string, number>> = Object.freeze({
  add: 0,
  'add nsw': 0,
  'add nuw': 0,
  sub: 1,
  'sub nsw': 1,
  'sub nuw': 1,
  shl: 7,
  xor: 12,
})

/** @internal */
const writeConstants = (
  module: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
  width: number,
  adapter: ConstantAdapter,
): void => {
  if (adapter.local.length === 0) return
  const schema = DeclarationSchema.constants(width)
  const block = Bitstream.enterBlock(module.writer, schema.block, module.abbrevWidth)
  let currentType = -1
  for (const constantIndex of adapter.local) {
    const constant = state.constants[constantIndex]
    if (constant === undefined) throw new Error('missing constant')
    if (constant.type !== currentType) {
      Bitstream.writeRecord(block, schema.setType, [constant.type])
      currentType = constant.type
    }
    switch (constant._tag) {
      case 'Integer': {
        const type = state.types[constant.type]
        if (type?._tag !== 'Integer') throw new Error('integer constant has non-integer type')
        const encoded = integerRecordValues(constant.bitPattern, type.bitWidth, constant.signed)
        if (encoded.narrow === undefined) {
          Bitstream.writeUnabbreviatedRecord(block, 5, encoded.wide)
        } else {
          Bitstream.writeRecord(block, schema.integer, [encoded.narrow])
        }
        break
      }
      case 'Float': {
        const value = littleEndianInteger(constant.bits)
        if (constant.format === 'half' || constant.format === 'bfloat') {
          Bitstream.writeRecord(block, schema.half, [value])
        } else if (constant.format === 'float') {
          Bitstream.writeRecord(block, schema.float, [value])
        } else if (constant.format === 'double') {
          Bitstream.writeRecord(block, schema.double, [value])
        } else if (constant.format === 'x86_fp80') {
          Bitstream.writeRecord(block, schema.fp80, [value >> 16n, value & 0xffffn])
        } else {
          Bitstream.writeRecord(block, schema.fp128, [value & 0xffff_ffff_ffff_ffffn, value >> 64n])
        }
        break
      }
      case 'Special':
        Bitstream.writeRecord(
          block,
          constant.kind === 'undef'
            ? schema.undef
            : constant.kind === 'poison'
              ? schema.poison
              : schema.nullValue,
          [],
        )
        break
      case 'Aggregate':
        Bitstream.writeRecord(block, schema.aggregate, [constant.elements.map(adapter.valueIndex)])
        break
      case 'Splat': {
        const type = state.types[constant.type]
        const length = type?._tag === 'Vector' ? type.length : 0
        Bitstream.writeRecord(block, schema.aggregate, [
          Array.from({ length }, () => adapter.valueIndex(constant.value)),
        ])
        break
      }
      case 'BlockAddress': {
        const reference = state.constants[constant.function]
        const global = reference?._tag === 'Global' ? state.globals[reference.global] : undefined
        const fn = global?.kind === 'Function' ? state.functions[global.actorIndex] : undefined
        if (fn === undefined) throw new Error('blockaddress function is missing')
        Bitstream.writeUnabbreviatedRecord(block, 21, [
          fn.type,
          adapter.valueIndex(constant.function),
          constant.block,
        ])
        break
      }
      case 'FunctionReference': {
        const reference = state.constants[constant.function]
        const global = reference?._tag === 'Global' ? state.globals[reference.global] : undefined
        const fn = global?.kind === 'Function' ? state.functions[global.actorIndex] : undefined
        if (fn === undefined) throw new Error('function reference is missing')
        Bitstream.writeUnabbreviatedRecord(
          block,
          constant.kind === 'dso_local_equivalent' ? 27 : 29,
          [fn.type, adapter.valueIndex(constant.function)],
        )
        break
      }
      case 'String': {
        const trailingNull = constant.bytes.bytes.at(-1) === 0
        Bitstream.writeRecord(block, trailingNull ? schema.cString : schema.string, [
          trailingNull ? constant.bytes.bytes.slice(0, -1) : constant.bytes.bytes,
        ])
        break
      }
      case 'Cast':
        Bitstream.writeRecord(block, schema.cast, [
          castOpcode[constant.kind] ?? 0,
          constant.type,
          adapter.valueIndex(constant.value),
        ])
        break
      case 'Binary':
        Bitstream.writeRecord(block, schema.binary, [
          binaryOpcode[constant.kind] ?? 0,
          adapter.valueIndex(constant.left),
          adapter.valueIndex(constant.right),
        ])
        break
      case 'GetElementPtr': {
        const base = state.constants[constant.base]
        if (base === undefined) throw new Error('missing getelementptr base')
        const values: Array<Bitstream.Scalar> = [
          constant.sourceType,
          base.type,
          adapter.valueIndex(constant.base),
        ]
        for (const index of constant.indices) {
          const value = state.constants[index]
          if (value === undefined) throw new Error('missing getelementptr index')
          values.push(value.type, adapter.valueIndex(index))
        }
        Bitstream.writeUnabbreviatedRecord(block, constant.inbounds ? 20 : 12, values)
        break
      }
      case 'Assembly': {
        const flags =
          (constant.sideEffect ? 1 : 0) |
          (constant.alignStack ? 2 : 0) |
          (constant.intelDialect ? 4 : 0) |
          (constant.canThrow ? 8 : 0)
        Bitstream.writeUnabbreviatedRecord(block, 30, [
          constant.type,
          flags,
          constant.assembly.bytes.length,
          ...constant.assembly.bytes,
          constant.constraints.bytes.length,
          ...constant.constraints.bytes,
        ])
        break
      }
      case 'Global':
        break
    }
  }
  Bitstream.endBlock(block)
}

/** @internal */
const writeGlobals = (
  module: Bitstream.BlockWriter,
  schema: DeclarationSchema.ModuleSchema,
  state: BuilderState.Snapshot,
  order: GlobalOrder,
  sections: ReadonlyMap<string, number>,
  constants: ConstantAdapter,
): void => {
  for (const entry of order.entries) {
    const { global, globalIndex } = entry
    const name = order.strtab.get(globalIndex)
    if (name === undefined) throw new Error('missing strtab name')
    const section = ByteString.isEmpty(global.section)
      ? 0
      : (sections.get(CanonicalKey.bytes(global.section)) ?? 0)
    if (global.kind === 'Variable') {
      const variable = state.variables[global.actorIndex]
      if (variable === undefined) throw new Error('missing variable')
      Bitstream.writeRecord(module, schema.variable, [
        name.offset,
        name.size,
        variable.valueType,
        (global.addressSpace.value << 2) | 2 | (variable.constant ? 1 : 0),
        variable.initializer === undefined ? 0 : constants.valueIndex(variable.initializer) + 1,
        linkageCode[global.linkage],
        alignmentCode(global.alignment),
        section,
        visibilityCode[global.visibility],
        threadLocalCode[variable.threadLocal],
        unnamedCode[global.unnamedAddress],
        variable.externallyInitialized ? 1 : 0,
        dllCode[global.dllStorage],
        global.preemption === 'dso_local' ? 1 : 0,
      ])
    } else if (global.kind === 'Function') {
      const fn = state.functions[global.actorIndex]
      if (fn === undefined) throw new Error('missing function')
      Bitstream.writeRecord(module, schema.functionDeclaration, [
        name.offset,
        name.size,
        fn.type,
        fn.callingConvention,
        fn.body === undefined ? 1 : 0,
        linkageCode[global.linkage],
        fn.attributes === undefined ? 0 : fn.attributes + 1,
        alignmentCode(global.alignment),
        section,
        visibilityCode[global.visibility],
        unnamedCode[global.unnamedAddress],
        dllCode[global.dllStorage],
        global.preemption === 'dso_local' ? 1 : 0,
        global.addressSpace.value,
      ])
    } else {
      const alias = state.aliases[global.actorIndex]
      if (alias === undefined) throw new Error('missing alias')
      Bitstream.writeRecord(module, schema.alias, [
        name.offset,
        name.size,
        alias.valueType,
        global.addressSpace.value,
        constants.valueIndex(alias.aliasee),
        linkageCode[global.linkage],
        visibilityCode[global.visibility],
        dllCode[global.dllStorage],
        0,
        unnamedCode[global.unnamedAddress],
        global.preemption === 'dso_local' ? 1 : 0,
      ])
    }
  }
}

/** @internal */
const fastMathCode = (flags: FunctionBodyDescription.FastMath): number =>
  (flags.noNaNs ? 1 << 1 : 0) |
  (flags.noInfinities ? 1 << 2 : 0) |
  (flags.noSignedZeros ? 1 << 3 : 0) |
  (flags.allowReciprocal ? 1 << 4 : 0) |
  (flags.allowContract ? 1 << 5 : 0) |
  (flags.approximateFunctions ? 1 << 6 : 0) |
  (flags.allowReassociation ? 1 << 7 : 0)

interface FunctionIndex {
  readonly valueIds: ReadonlyMap<number, number>
  readonly instructionOffsets: ReadonlyMap<number, number>
  readonly moduleValues: number
}

/** @internal */
const functionIndex = (
  body: FunctionBodyDescription.Snapshot,
  moduleValues: number,
): FunctionIndex => {
  const valueIds = new Map<number, number>()
  for (let index = 0; index < body.arguments.length; index += 1) {
    const value = body.arguments[index]
    if (value !== undefined) valueIds.set(value, moduleValues + index)
  }
  const instructionOffsets = new Map<number, number>()
  let nextValue = moduleValues + body.arguments.length
  for (const block of body.blocks) {
    for (const instructionIndex of block.instructions) {
      instructionOffsets.set(instructionIndex, nextValue)
      const result = body.instructions[instructionIndex]?.result
      if (result !== undefined) {
        valueIds.set(result, nextValue)
        nextValue += 1
      }
    }
  }
  return { valueIds, instructionOffsets, moduleValues }
}

/** @internal */
const resolvedBodyOperand = (
  body: FunctionBodyDescription.Snapshot,
  operand: FunctionBodyDescription.Operand,
  seen: ReadonlySet<number> = new Set(),
): FunctionBodyDescription.Operand => {
  if (operand._tag === 'Constant') return operand
  const value = body.values[operand.value]
  if (value?.source._tag !== 'Forward' || value.source.resolved === undefined) return operand
  if (seen.has(operand.value)) throw new Error('forward value cycle')
  const next = new Set(seen)
  next.add(operand.value)
  return resolvedBodyOperand(body, value.source.resolved, next)
}

/** @internal */
const absoluteOperand = (
  body: FunctionBodyDescription.Snapshot,
  index: FunctionIndex,
  constants: ConstantAdapter,
  operand: FunctionBodyDescription.Operand,
): number => {
  const resolved = resolvedBodyOperand(body, operand)
  if (resolved._tag === 'Constant') return constants.valueIndex(resolved.constant)
  const value = index.valueIds.get(resolved.value)
  if (value === undefined) throw new Error('local value index is missing')
  return value
}

/** @internal */
const relativeOperand = (
  body: FunctionBodyDescription.Snapshot,
  index: FunctionIndex,
  constants: ConstantAdapter,
  instructionIndex: number,
  operand: FunctionBodyDescription.Operand,
): number => {
  const offset = index.instructionOffsets.get(instructionIndex)
  if (offset === undefined) throw new Error('instruction offset is missing')
  const value = offset - absoluteOperand(body, index, constants, operand)
  if (value < 0) throw new Error('non-phi instruction has a forward value reference')
  return value
}

/** @internal */
const bodyOperandType = (
  state: BuilderState.Snapshot,
  body: FunctionBodyDescription.Snapshot,
  operand: FunctionBodyDescription.Operand,
): number => {
  const resolved = resolvedBodyOperand(body, operand)
  const type =
    resolved._tag === 'Constant'
      ? state.constants[resolved.constant]?.type
      : body.values[resolved.value]?.type
  if (type === undefined) throw new Error('body operand type is missing')
  return type
}

/** @internal */
const signedRelativeOperand = (
  body: FunctionBodyDescription.Snapshot,
  index: FunctionIndex,
  constants: ConstantAdapter,
  instructionIndex: number,
  operand: FunctionBodyDescription.Operand,
): number => {
  const offset = index.instructionOffsets.get(instructionIndex)
  if (offset === undefined) throw new Error('instruction offset is missing')
  const difference = offset - absoluteOperand(body, index, constants, operand)
  const absolute = Math.abs(difference)
  return difference > 0 ? absolute * 2 : absolute * 2 + 1
}

/** @internal */
const writeFunctionInstruction = (
  block: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
  body: FunctionBodyDescription.Snapshot,
  index: FunctionIndex,
  constants: ConstantAdapter,
  operandBundleTags: ReadonlyMap<string, number>,
  instructionIndex: number,
  instruction: FunctionBodyDescription.Instruction,
): void => {
  const relative = (operand: FunctionBodyDescription.Operand): number =>
    relativeOperand(body, index, constants, instructionIndex, operand)
  switch (instruction._tag) {
    case 'Unary': {
      const math = fastMathCode(instruction.fastMath)
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.unary, [
        relative(instruction.operand),
        0,
        ...(math === 0 ? [] : [math]),
      ])
      break
    }
    case 'Binary': {
      const math = fastMathCode(instruction.fastMath)
      const integerFlags =
        (instruction.integerFlags.noUnsignedWrap ? 1 : 0) |
        (instruction.integerFlags.noSignedWrap ? 2 : 0) |
        (instruction.integerFlags.exact ? 1 : 0)
      const flags = instruction.kind.startsWith('f') ? math : integerFlags
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.binary, [
        relative(instruction.left),
        relative(instruction.right),
        CoreSchema.binaryOpcode[instruction.kind],
        ...(flags === 0 ? [] : [flags]),
      ])
      break
    }
    case 'Compare': {
      const math = fastMathCode(instruction.fastMath)
      const predicate =
        instruction.kind === 'integer'
          ? CoreSchema.integerPredicate[
              instruction.predicate as FunctionBodyDescription.IntegerPredicate
            ]
          : CoreSchema.floatingPredicate[
              instruction.predicate as FunctionBodyDescription.FloatingPredicate
            ]
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.compare, [
        relative(instruction.left),
        relative(instruction.right),
        predicate,
        ...(math === 0 ? [] : [math]),
      ])
      break
    }
    case 'Select': {
      const math = fastMathCode(instruction.fastMath)
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.select, [
        relative(instruction.onTrue),
        relative(instruction.onFalse),
        relative(instruction.condition),
        ...(math === 0 ? [] : [math]),
      ])
      break
    }
    case 'Cast': {
      const flags = (instruction.noUnsignedWrap ? 1 : 0) | (instruction.noSignedWrap ? 2 : 0)
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.cast, [
        relative(instruction.operand),
        instruction.destinationType,
        CoreSchema.castOpcode[instruction.kind],
        ...(flags === 0 ? [] : [flags]),
      ])
      break
    }
    case 'ExtractValue':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.extractValue, [
        relative(instruction.aggregate),
        ...instruction.indices,
      ])
      break
    case 'InsertValue':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.insertValue, [
        relative(instruction.aggregate),
        relative(instruction.element),
        ...instruction.indices,
      ])
      break
    case 'Alloca': {
      const alignment = MemoryAccess.alignmentCode(instruction.alignment)
      const flags =
        (alignment & 0x1f) |
        (instruction.inAlloca ? 1 << 5 : 0) |
        (1 << 6) |
        ((alignment >>> 5) << 8)
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.alloca, [
        instruction.allocationType,
        bodyOperandType(state, body, instruction.count),
        absoluteOperand(body, index, constants, instruction.count),
        flags,
        ...(instruction.addressSpace === 0 ? [] : [instruction.addressSpace]),
      ])
      break
    }
    case 'Load': {
      const values = [
        relative(instruction.pointer),
        instruction.valueType,
        MemoryAccess.alignmentCode(instruction.access.alignment),
        instruction.access.kind === 'volatile' ? 1 : 0,
      ]
      if (instruction.access.ordering !== 'none') {
        values.push(
          MemoryAccess.orderingCode[instruction.access.ordering],
          instruction.access.syncScope === 'singlethread' ? 0 : 1,
        )
      }
      Bitstream.writeUnabbreviatedRecord(
        block,
        instruction.access.ordering === 'none' ? CoreSchema.code.load : CoreSchema.code.loadAtomic,
        values,
      )
      break
    }
    case 'Store': {
      const values = [
        relative(instruction.pointer),
        relative(instruction.value),
        MemoryAccess.alignmentCode(instruction.access.alignment),
        instruction.access.kind === 'volatile' ? 1 : 0,
      ]
      if (instruction.access.ordering !== 'none') {
        values.push(
          MemoryAccess.orderingCode[instruction.access.ordering],
          instruction.access.syncScope === 'singlethread' ? 0 : 1,
        )
      }
      Bitstream.writeUnabbreviatedRecord(
        block,
        instruction.access.ordering === 'none'
          ? CoreSchema.code.store
          : CoreSchema.code.storeAtomic,
        values,
      )
      break
    }
    case 'GetElementPtr':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.getElementPtr, [
        instruction.inbounds ? 1 : 0,
        instruction.sourceType,
        relative(instruction.base),
        ...instruction.indices.map(relative),
      ])
      break
    case 'ExtractElement':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.extractElement, [
        relative(instruction.vector),
        relative(instruction.index),
      ])
      break
    case 'InsertElement':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.insertElement, [
        relative(instruction.vector),
        relative(instruction.element),
        relative(instruction.index),
      ])
      break
    case 'ShuffleVector':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.shuffleVector, [
        relative(instruction.left),
        relative(instruction.right),
        relative(instruction.mask),
      ])
      break
    case 'Fence':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.fence, [
        MemoryAccess.orderingCode[instruction.ordering],
        instruction.syncScope === 'singlethread' ? 0 : 1,
      ])
      break
    case 'CompareExchange':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.compareExchange, [
        relative(instruction.pointer),
        relative(instruction.comparison),
        relative(instruction.replacement),
        instruction.access.kind === 'volatile' ? 1 : 0,
        MemoryAccess.orderingCode[instruction.access.ordering],
        instruction.access.syncScope === 'singlethread' ? 0 : 1,
        MemoryAccess.orderingCode[instruction.failureOrdering],
        instruction.weak ? 1 : 0,
        MemoryAccess.alignmentCode(instruction.access.alignment),
      ])
      break
    case 'AtomicRmw':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.atomicRmw, [
        relative(instruction.pointer),
        relative(instruction.value),
        MemoryAccess.operationCode[instruction.operation],
        instruction.access.kind === 'volatile' ? 1 : 0,
        MemoryAccess.orderingCode[instruction.access.ordering],
        instruction.access.syncScope === 'singlethread' ? 0 : 1,
        MemoryAccess.alignmentCode(instruction.access.alignment),
      ])
      break
    case 'VaArg':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.vaArg, [
        bodyOperandType(state, body, instruction.list),
        relative(instruction.list),
        instruction.valueType,
      ])
      break
    case 'IndirectBranch':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.indirectBranch, [
        bodyOperandType(state, body, instruction.address),
        relative(instruction.address),
        ...instruction.destinations,
      ])
      break
    case 'Branch':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.branch, [instruction.destination])
      break
    case 'ConditionalBranch':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.branch, [
        instruction.onTrue,
        instruction.onFalse,
        relative(instruction.condition),
      ])
      break
    case 'Switch': {
      const resolved = resolvedBodyOperand(body, instruction.value)
      const type = resolved._tag === 'Constant' ? undefined : body.values[resolved.value]?.type
      const conditionType =
        type ??
        (resolved._tag === 'Constant'
          ? state.constants[resolved.constant]?.type
          : body.values[resolved.value]?.type)
      if (conditionType === undefined) throw new Error('switch condition type is missing')
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.switch, [
        conditionType ?? 0,
        relative(instruction.value),
        instruction.defaultBlock,
        ...instruction.cases.flatMap((entry) => [constants.valueIndex(entry.value), entry.block]),
      ])
      break
    }
    case 'Return':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.return, [
        relative(instruction.value),
      ])
      break
    case 'ReturnVoid':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.return, [])
      break
    case 'Unreachable':
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.unreachable, [])
      break
    case 'Phi': {
      const values: Array<Bitstream.Scalar> = [instruction.type]
      for (const incoming of instruction.incoming) {
        values.push(
          signedRelativeOperand(body, index, constants, instructionIndex, incoming.value),
          incoming.block,
        )
      }
      const math = fastMathCode(instruction.fastMath)
      if (math !== 0) values.push(math)
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.phi, values)
      break
    }
    case 'Call': {
      for (const bundle of instruction.operandBundles) {
        const tag = operandBundleTags.get(CanonicalKey.bytes(bundle.tag))
        if (tag === undefined) throw new Error('operand bundle tag is missing')
        Bitstream.writeUnabbreviatedRecord(block, 55, [tag, ...bundle.operands.map(relative)])
      }
      const math = fastMathCode(instruction.fastMath)
      const callType =
        (instruction.tail === 'tail' ? 1 : 0) |
        (instruction.callingConvention << 1) |
        (instruction.tail === 'musttail' ? 1 << 14 : 0) |
        (1 << 15) |
        (instruction.tail === 'notail' ? 1 << 16 : 0) |
        (math === 0 ? 0 : 1 << 17)
      Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.call, [
        instruction.attributes === undefined ? 0 : instruction.attributes + 1,
        callType,
        ...(math === 0 ? [] : [math]),
        instruction.functionType,
        relative(instruction.callee),
        ...instruction.arguments.map(relative),
      ])
      break
    }
  }
}

const metadataKindCode: Readonly<Record<MetadataDescription.Attachment['kind'], number>> =
  Object.freeze({ dbg: 0, prof: 2, unpredictable: 15 })

/** @internal */
const writeMetadataKinds = (
  module: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
  metadata: MetadataAdapter,
): void => {
  const hasAttachments =
    state.globalMetadata.some((attachments) => attachments.length > 0) ||
    state.functions.some(
      (fn) =>
        fn.body?.metadata.some((attachments) => attachments.length > 0) === true ||
        fn.body?.debugLocations.some((location) => location !== undefined) === true,
    )
  if (metadata.entries.length === 0 && !hasAttachments) return
  const block = Bitstream.enterBlock(
    module.writer,
    { id: 22, abbreviations: [] },
    module.abbrevWidth,
  )
  for (const [name, id] of [
    ['dbg', 0],
    ['prof', 2],
    ['unpredictable', 15],
  ] as const) {
    Bitstream.writeUnabbreviatedRecord(block, 6, [id, ...ByteString.fromString(name).bytes])
  }
  Bitstream.endBlock(block)
}

/** @internal */
const signedMetadataInteger = (value: bigint): bigint =>
  value >= 0n ? value << 1n : (-value << 1n) | 1n

/** @internal */
const writeMetadata = (
  module: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
  order: GlobalOrder,
  constants: ConstantAdapter,
  metadata: MetadataAdapter,
): void => {
  if (metadata.entries.length === 0 && state.namedMetadata.length === 0) return
  const block = Bitstream.enterBlock(
    module.writer,
    { id: 15, abbreviations: [] },
    module.abbrevWidth,
  )
  const optional = metadata.optional
  for (const entryIndex of metadata.entries) {
    const entry = state.metadata[entryIndex]
    if (entry === undefined) throw new Error('metadata entry is missing')
    if (entry._tag === 'String') {
      Bitstream.writeUnabbreviatedRecord(block, 1, entry.value.bytes)
      continue
    }
    if (entry._tag === 'Forward') throw new Error('unresolved metadata entry')
    const node = entry.value
    switch (node._tag) {
      case 'Tuple':
        Bitstream.writeUnabbreviatedRecord(block, node.distinct ? 5 : 3, [
          ...node.elements.map(optional),
        ])
        break
      case 'Constant': {
        const constant = state.constants[node.constant]
        if (constant === undefined) throw new Error('metadata constant is missing')
        Bitstream.writeUnabbreviatedRecord(block, 2, [
          constant.type,
          constants.valueIndex(node.constant),
        ])
        break
      }
      case 'Local':
        Bitstream.writeUnabbreviatedRecord(block, node.distinct ? 5 : 3, [])
        break
      case 'File':
        Bitstream.writeUnabbreviatedRecord(block, 16, [
          node.distinct ? 1 : 0,
          optional(node.filename),
          optional(node.directory),
          0,
          0,
        ])
        break
      case 'CompileUnit':
        Bitstream.writeUnabbreviatedRecord(block, 20, [
          1,
          12,
          optional(node.file),
          optional(node.producer),
          node.optimized ? 1 : 0,
          0,
          0,
          0,
          1,
          optional(node.enums),
          0,
          0,
          optional(node.globals),
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
        ])
        break
      case 'Subprogram':
        Bitstream.writeUnabbreviatedRecord(block, 21, [
          7,
          optional(node.file),
          optional(node.name),
          optional(node.linkageName),
          optional(node.file),
          node.line,
          optional(node.type),
          node.scopeLine,
          0,
          node.spFlags,
          0,
          node.diFlags,
          optional(node.compileUnit),
          0,
          0,
          0,
          0,
          0,
          0,
          0,
        ])
        break
      case 'LexicalBlock':
        Bitstream.writeUnabbreviatedRecord(block, 22, [
          node.distinct ? 1 : 0,
          optional(node.scope),
          optional(node.file),
          node.line,
          node.column,
        ])
        break
      case 'Location':
        Bitstream.writeUnabbreviatedRecord(block, 7, [
          node.distinct ? 1 : 0,
          node.line,
          node.column,
          optional(node.scope),
          optional(node.inlinedAt),
          0,
        ])
        break
      case 'BasicType': {
        const encoding: Readonly<Record<MetadataDescription.BasicEncoding, number>> = {
          boolean: 2,
          float: 4,
          signed: 5,
          unsigned: 7,
        }
        Bitstream.writeUnabbreviatedRecord(block, 15, [
          node.distinct ? 1 : 0,
          0x24,
          optional(node.name),
          node.sizeInBits,
          0,
          encoding[node.encoding],
          0,
        ])
        break
      }
      case 'CompositeType': {
        const tag: Readonly<Record<MetadataDescription.CompositeKind, number>> = {
          array: 0x01,
          enumeration: 0x04,
          structure: 0x13,
          union: 0x17,
          vector: 0x01,
        }
        Bitstream.writeUnabbreviatedRecord(block, 18, [
          (node.distinct ? 1 : 0) | 2,
          tag[node.kind],
          optional(node.name),
          optional(node.file),
          node.line,
          optional(node.scope),
          optional(node.underlyingType),
          node.sizeInBits,
          node.alignInBits,
          0,
          node.flags,
          optional(node.fields),
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
          0,
        ])
        break
      }
      case 'DerivedType': {
        const tag: Readonly<Record<MetadataDescription.DerivedKind, number>> = {
          member: 0x0d,
          pointer: 0x0f,
          typedef: 0x16,
        }
        Bitstream.writeUnabbreviatedRecord(block, 17, [
          node.distinct ? 1 : 0,
          tag[node.kind],
          optional(node.name),
          optional(node.file),
          node.line,
          optional(node.scope),
          optional(node.underlyingType),
          node.sizeInBits,
          node.alignInBits,
          node.offsetInBits,
          node.flags,
          0,
        ])
        break
      }
      case 'SubroutineType':
        Bitstream.writeUnabbreviatedRecord(block, 19, [
          (node.distinct ? 1 : 0) | 2,
          node.flags,
          optional(node.types),
          0,
        ])
        break
      case 'Enumerator':
        Bitstream.writeUnabbreviatedRecord(block, 14, [
          4 | (node.unsigned ? 2 : 0) | (node.distinct ? 1 : 0),
          node.bitWidth,
          optional(node.name),
          signedMetadataInteger(node.value),
        ])
        break
      case 'Subrange':
        Bitstream.writeUnabbreviatedRecord(block, 13, [
          (node.distinct ? 1 : 0) | 4,
          optional(node.count),
          optional(node.lowerBound),
          0,
          0,
        ])
        break
      case 'Expression':
        Bitstream.writeUnabbreviatedRecord(block, 29, [
          (node.distinct ? 1 : 0) | 6,
          ...node.elements,
        ])
        break
      case 'LocalVariable':
        Bitstream.writeUnabbreviatedRecord(block, 28, [
          (node.distinct ? 1 : 0) | 2,
          optional(node.scope),
          optional(node.name),
          optional(node.file),
          node.line,
          optional(node.type),
          node.argument,
          node.flags,
          0,
          0,
        ])
        break
      case 'GlobalVariable':
        Bitstream.writeUnabbreviatedRecord(block, 27, [
          (node.distinct ? 1 : 0) | 4,
          optional(node.scope),
          optional(node.name),
          optional(node.linkageName),
          optional(node.file),
          node.line,
          optional(node.type),
          node.local ? 1 : 0,
          1,
          0,
          0,
          0,
          0,
        ])
        break
      case 'GlobalVariableExpression':
        Bitstream.writeUnabbreviatedRecord(block, 37, [
          node.distinct ? 1 : 0,
          optional(node.variable),
          optional(node.expression),
        ])
        break
    }
  }
  for (const named of state.namedMetadata) {
    Bitstream.writeUnabbreviatedRecord(block, 4, named.name.bytes)
    Bitstream.writeUnabbreviatedRecord(block, 10, named.operands.map(metadata.index))
  }
  for (let globalIndex = 0; globalIndex < state.globalMetadata.length; globalIndex += 1) {
    const global = state.globals[globalIndex]
    if (global === undefined || global.deleted || global.replacement !== undefined) continue
    const fn = global.kind === 'Function' ? state.functions[global.actorIndex] : undefined
    if (fn?.body !== undefined) continue
    const value = order.valueIndex.get(globalIndex)
    if (value === undefined) continue
    for (const attachment of state.globalMetadata[globalIndex] ?? []) {
      Bitstream.writeUnabbreviatedRecord(block, 36, [
        value,
        metadataKindCode[attachment.kind],
        metadata.index(attachment.metadata),
      ])
    }
  }
  Bitstream.endBlock(block)
}

/** @internal */
const metadataNodeAt = (
  state: BuilderState.Snapshot,
  metadata: MetadataAdapter,
  index: number,
): MetadataDescription.Node => {
  const resolved = metadata.reachable.resolved.get(index) ?? index
  const entry = state.metadata[resolved]
  if (entry?._tag !== 'Node') throw new Error('debug location is not a metadata node')
  return entry.value
}

/** @internal */
const writeFunctionAttachments = (
  functionBlock: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
  globalIndex: number,
  body: FunctionBodyDescription.Snapshot,
  metadata: MetadataAdapter,
): void => {
  const globalAttachments = state.globalMetadata[globalIndex] ?? []
  const hasInstructionAttachments = body.metadata.some((attachments) => attachments.length > 0)
  if (globalAttachments.length === 0 && !hasInstructionAttachments) return
  const block = Bitstream.enterBlock(
    functionBlock.writer,
    { id: 16, abbreviations: [] },
    functionBlock.abbrevWidth,
    false,
  )
  for (const attachment of globalAttachments) {
    Bitstream.writeUnabbreviatedRecord(block, 11, [
      metadataKindCode[attachment.kind],
      metadata.index(attachment.metadata),
    ])
  }
  for (let instructionIndex = 0; instructionIndex < body.metadata.length; instructionIndex += 1) {
    for (const attachment of body.metadata[instructionIndex] ?? []) {
      Bitstream.writeUnabbreviatedRecord(block, 11, [
        instructionIndex,
        metadataKindCode[attachment.kind],
        metadata.index(attachment.metadata),
      ])
    }
  }
  Bitstream.endBlock(block)
}

/** @internal */
const writeFunctionBodies = (
  module: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
  order: GlobalOrder,
  constants: ConstantAdapter,
  operandBundleTags: ReadonlyMap<string, number>,
  metadata: MetadataAdapter,
): void => {
  const moduleValues = order.entries.length + constants.local.length
  for (const { global, globalIndex } of order.entries) {
    if (global.kind !== 'Function') continue
    const body = state.functions[global.actorIndex]?.body
    if (body === undefined) continue
    const index = functionIndex(body, moduleValues)
    const block = Bitstream.enterBlock(
      module.writer,
      CoreSchema.functionBlock,
      module.abbrevWidth,
      false,
    )
    Bitstream.writeUnabbreviatedRecord(block, CoreSchema.code.declareBlocks, [body.blocks.length])
    let activeDebugLocation: number | undefined
    for (const bodyBlock of body.blocks) {
      for (const instructionIndex of bodyBlock.instructions) {
        const instruction = body.instructions[instructionIndex]
        if (instruction === undefined) throw new Error('function block instruction is missing')
        writeFunctionInstruction(
          block,
          state,
          body,
          index,
          constants,
          operandBundleTags,
          instructionIndex,
          instruction,
        )
        const debugLocation = body.debugLocations[instructionIndex]
        if (debugLocation === undefined) {
          activeDebugLocation = undefined
        } else if (activeDebugLocation === debugLocation) {
          Bitstream.writeUnabbreviatedRecord(block, 33, [])
        } else {
          const location = metadataNodeAt(state, metadata, debugLocation)
          if (location._tag !== 'Location')
            throw new Error('debug location has the wrong node kind')
          Bitstream.writeUnabbreviatedRecord(block, 35, [
            location.line,
            location.column,
            metadata.optional(location.scope),
            metadata.optional(location.inlinedAt),
            0,
          ])
          activeDebugLocation = debugLocation
        }
      }
    }
    writeFunctionAttachments(block, state, globalIndex, body, metadata)
    Bitstream.endBlock(block)
  }
}

/** @internal */
const writeOperandBundleTags = (
  module: Bitstream.BlockWriter,
  state: BuilderState.Snapshot,
  order: GlobalOrder,
): ReadonlyMap<string, number> => {
  const tags = new Map<string, { readonly index: number; readonly value: ByteString.ByteString }>()
  for (const { global } of order.entries) {
    if (global.kind !== 'Function') continue
    const body = state.functions[global.actorIndex]?.body
    if (body === undefined) continue
    for (const instruction of body.instructions) {
      if (instruction._tag !== 'Call') continue
      for (const bundle of instruction.operandBundles) {
        const key = CanonicalKey.bytes(bundle.tag)
        if (!tags.has(key)) tags.set(key, { index: tags.size, value: bundle.tag })
      }
    }
  }
  if (tags.size > 0) {
    const block = Bitstream.enterBlock(
      module.writer,
      { id: 21, abbreviations: [] },
      module.abbrevWidth,
      false,
    )
    for (const { value } of tags.values()) {
      Bitstream.writeUnabbreviatedRecord(block, 1, value.bytes)
    }
    Bitstream.endBlock(block)
  }
  return new Map([...tags].map(([key, entry]) => [key, entry.index]))
}

/** @internal */
const writeDeclarationString = (
  block: Bitstream.BlockWriter,
  abbreviation: Bitstream.Abbreviation,
  code: number,
  value: ByteString.ByteString,
): void => {
  if (!ByteString.isEmpty(value)) Bitstream.writeRecord(block, abbreviation, [code, value.bytes])
}

/** @internal */
const encodeDeclarations = (state: BuilderState.Snapshot, producer: Producer): Uint8Array => {
  const writer = Bitstream.make()
  Bitstream.writeBits(writer, BitcodeSchema.Magic, 32)
  writeIdentification(writer, producer)
  const width = bitWidth(state.types.length)
  const moduleSchema = DeclarationSchema.module(width)
  const module = Bitstream.enterBlock(writer, moduleSchema.block)
  Bitstream.writeRecord(module, moduleSchema.version, [])
  writeDeclarationString(module, moduleSchema.string, 2, state.targetTriple)
  writeDeclarationString(module, moduleSchema.string, 3, state.dataLayout)
  writeDeclarationString(module, moduleSchema.string, 16, state.sourceFilename)
  for (const assembly of state.moduleAssembly) {
    writeDeclarationString(module, moduleSchema.string, 4, assembly)
  }
  writeTypes(module, state, width)
  writeAttributes(module, state)
  const order = buildGlobalOrder(state)
  const sections = sectionIndices(module, moduleSchema, order)
  const constants = buildConstantAdapter(state, order)
  writeGlobals(module, moduleSchema, state, order, sections, constants)
  writeConstants(module, state, width, constants)
  const metadata = buildMetadataAdapter(state)
  writeMetadataKinds(module, state, metadata)
  writeMetadata(module, state, order, constants, metadata)
  const operandBundleTags = writeOperandBundleTags(module, state, order)
  writeFunctionBodies(module, state, order, constants, operandBundleTags, metadata)
  Bitstream.endBlock(module)
  const stringTable = Bitstream.enterBlock(writer, BitcodeSchema.Strtab)
  Bitstream.writeRecord(stringTable, BitcodeSchema.StrtabBlob, [order.bytes])
  Bitstream.endBlock(stringTable)
  return Bitstream.toUint8Array(writer)
}

/** @internal */
const encodeSnapshot = (state: BuilderState.Snapshot, options: Options): Uint8Array => {
  const producer = options.producer ?? defaultProducer
  const hasDeclarations =
    state.types.length > 0 ||
    state.attributes.length > 0 ||
    state.constants.length > 0 ||
    state.globals.length > 0 ||
    state.metadata.length > 0 ||
    state.namedMetadata.length > 0
  return hasDeclarations ? encodeDeclarations(state, producer) : encodeFoundation(state, producer)
}

/**
 * Encodes the current module directly as deterministic LLVM bitcode bytes.
 *
 * **Details**
 *
 * Encoding snapshots the builder, resolves reachable metadata, and performs all byte packing in
 * memory. It never invokes Zig, LLVM tools, native libraries, or the filesystem.
 *
 * The builder is not consumed, and the result is a fresh `Uint8Array`. Options can replace the
 * producer identity written to the bitcode identification block.
 *
 * **Gotchas**
 *
 * Unresolved or invalid module state fails with {@link SilkError}.
 *
 * **Example** (Encoding a module)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Bitcode from '@silk-effect/llvm/Bitcode'
 * import * as Builder from '@silk-effect/llvm/Builder'
 *
 * const bytes = await Effect.runPromise(
 *   Effect.gen(function* () {
 *     const builder = yield* Builder.make()
 *     return yield* Bitcode.encode(builder)
 *   }),
 * )
 * // bytes.slice(0, 4) equals Uint8Array.of(0x42, 0x43, 0xC0, 0xDE)
 * ```
 *
 * @category encoding
 * @since 0.0.0
 */
export const encode = Effect.fn('Bitcode.encode')(function* (
  self: Builder.Builder,
  options: Options = {},
): Effect.fn.Return<Uint8Array, SilkError> {
  const state = yield* BuilderState.snapshot(self, 'Bitcode.encode')
  return yield* Effect.try({
    try: () => encodeSnapshot(state, options),
    catch: (cause) =>
      cause instanceof SilkError
        ? cause
        : new SilkError({
            operation: 'Bitcode.encode',
            message: 'LLVM bitcode encoding failed',
            cause,
          }),
  })
})
