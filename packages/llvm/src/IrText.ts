import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as ByteString from './ByteString.js'
import * as DIFlags from './DIFlags.js'
import * as DISPFlags from './DISPFlags.js'
import type * as AttributeDescription from './internal/AttributeDescription.js'
import * as BuilderState from './internal/BuilderState.js'
import type * as ConstantDescription from './internal/ConstantDescription.js'
import type * as FunctionBodyDescription from './internal/FunctionBodyDescription.js'
import type * as GlobalDescription from './internal/GlobalDescription.js'
import type * as MetadataDescription from './internal/MetadataDescription.js'
import type * as TypeDescription from './internal/TypeDescription.js'
import { type LlvmError, wrappedFailure } from './LlvmError.js'
import * as MemoryAccess from './MemoryAccess.js'
import * as Metadata from './Metadata.js'

/** @internal */
const quoted = (value: ByteString.ByteString): string => `"${ByteString.escapeForIr(value)}"`

/** @internal */
const identifier = (prefix: '@' | '%', value: ByteString.ByteString): string => {
  let ascii = ''
  for (const byte of value.bytes) ascii += String.fromCharCode(byte)
  return /^[-A-Za-z$._][-A-Za-z$._0-9]*$/.test(ascii) || /^\d+$/.test(ascii)
    ? `${prefix}${ascii}`
    : `${prefix}${quoted(value)}`
}

/** @internal */
const typeAt = (state: BuilderState.Snapshot, index: number): TypeDescription.Description => {
  const description = state.types[index]
  if (description === undefined) throw new Error(`missing type ${index}`)
  return description
}

/** @internal */
const renderType = (state: BuilderState.Snapshot, index: number): string => {
  const description = typeAt(state, index)
  if (description._tag === 'Simple') {
    switch (description.tag) {
      case 'Void':
        return 'void'
      case 'Half':
        return 'half'
      case 'BFloat':
        return 'bfloat'
      case 'Float':
        return 'float'
      case 'Double':
        return 'double'
      case 'X86Fp80':
        return 'x86_fp80'
      case 'Fp128':
        return 'fp128'
      case 'PpcFp128':
        return 'ppc_fp128'
      case 'Label':
        return 'label'
      case 'Metadata':
        return 'metadata'
      case 'X86Amx':
        return 'x86_amx'
      case 'Token':
        return 'token'
    }
  }
  switch (description._tag) {
    case 'Integer':
      return `i${description.bitWidth}`
    case 'Pointer':
      return description.addressSpace.value === 0
        ? 'ptr'
        : `ptr addrspace(${description.addressSpace.value})`
    case 'Function': {
      const parameters = description.parameters.map((parameter) => renderType(state, parameter))
      if (description.variadic) parameters.push('...')
      return `${renderType(state, description.returnType)} (${parameters.join(', ')})`
    }
    case 'Vector':
      return description.scalable
        ? `<vscale x ${description.length} x ${renderType(state, description.child)}>`
        : `<${description.length} x ${renderType(state, description.child)}>`
    case 'Array':
      return `[${description.length} x ${renderType(state, description.child)}]`
    case 'Structure': {
      const body = description.fields.map((field) => renderType(state, field)).join(', ')
      return description.packed ? `<{ ${body} }>` : `{ ${body} }`
    }
    case 'NamedStructure':
      return identifier('%', description.name)
    case 'TargetExtension': {
      const parameters = [
        quoted(description.name),
        ...description.types.map((type) => renderType(state, type)),
        ...description.integers.map((integer) => integer.toString()),
      ]
      return `target(${parameters.join(', ')})`
    }
  }
}

/** @internal */
const constantAt = (
  state: BuilderState.Snapshot,
  index: number,
): ConstantDescription.Description => {
  const description = state.constants[index]
  if (description === undefined) throw new Error(`missing constant ${index}`)
  return description
}

/** @internal */
const globalAt = (
  state: BuilderState.Snapshot,
  index: number,
): GlobalDescription.GlobalDescription => {
  const description = state.globals[index]
  if (description === undefined) throw new Error(`missing global ${index}`)
  return description
}

/** @internal */
const reverseHex = (value: ByteString.ByteString): string =>
  [...value.bytes]
    .reverse()
    .map((byte) => byte.toString(16).toUpperCase().padStart(2, '0'))
    .join('')

/** @internal */
const floatHex = (
  description: Extract<ConstantDescription.Description, { _tag: 'Float' }>,
): string => {
  const prefix =
    description.format === 'half'
      ? 'H'
      : description.format === 'bfloat'
        ? 'R'
        : description.format === 'x86_fp80'
          ? 'K'
          : description.format === 'fp128'
            ? 'L'
            : description.format === 'ppc_fp128'
              ? 'M'
              : ''
  if (description.format === 'float') {
    const source = ByteString.toUint8Array(description.bits)
    const sourceView = new DataView(source.buffer)
    const result = new Uint8Array(8)
    new DataView(result.buffer).setFloat64(0, sourceView.getFloat32(0, true), true)
    return `0x${reverseHex(ByteString.fromUint8Array(result))}`
  }
  return `0x${prefix}${reverseHex(description.bits)}`
}

/** @internal */
const renderTypedConstant = (state: BuilderState.Snapshot, index: number): string => {
  const description = constantAt(state, index)
  return `${renderType(state, description.type)} ${renderConstant(state, index)}`
}

/** @internal */
const renderConstant = (state: BuilderState.Snapshot, index: number): string => {
  const description = constantAt(state, index)
  switch (description._tag) {
    case 'Global':
      return identifier('@', globalAt(state, description.global).name)
    case 'Integer': {
      if (!description.signed) return description.bitPattern.toString()
      const type = typeAt(state, description.type)
      if (type._tag !== 'Integer') return description.bitPattern.toString()
      const sign = 1n << BigInt(type.bitWidth - 1)
      return (description.bitPattern & sign) === 0n
        ? description.bitPattern.toString()
        : (description.bitPattern - (1n << BigInt(type.bitWidth))).toString()
    }
    case 'Float':
      return floatHex(description)
    case 'Special':
      return description.kind
    case 'String':
      return `c${quoted(description.bytes)}`
    case 'Aggregate': {
      const values = description.elements
        .map((element) => renderTypedConstant(state, element))
        .join(', ')
      if (description.kind === 'array') return `[${values}]`
      if (description.kind === 'vector') return `<${values}>`
      return description.kind === 'packed-structure' ? `<{ ${values} }>` : `{ ${values} }`
    }
    case 'Splat': {
      const type = typeAt(state, description.type)
      const length = type._tag === 'Vector' ? type.length : 0
      if (type._tag === 'Vector' && type.scalable) {
        return `splat (${renderTypedConstant(state, description.value)})`
      }
      return `<${Array.from({ length }, () => renderTypedConstant(state, description.value)).join(', ')}>`
    }
    case 'BlockAddress': {
      const reference = state.constants[description.function]
      const global = reference?._tag === 'Global' ? state.globals[reference.global] : undefined
      const fn = global?.kind === 'Function' ? state.functions[global.actorIndex] : undefined
      const body = fn?.body
      if (
        global === undefined ||
        body === undefined ||
        body.blocks[description.block] === undefined
      ) {
        throw new Error('blockaddress references a missing function block')
      }
      return `blockaddress(${identifier('@', global.name)}, ${blockIdentifier(body, description.block)})`
    }
    case 'FunctionReference':
      return `${description.kind} ${renderConstant(state, description.function)}`
    case 'Cast':
      return `${description.kind} (${renderTypedConstant(state, description.value)} to ${renderType(state, description.type)})`
    case 'Binary':
      return `${description.kind} (${renderTypedConstant(state, description.left)}, ${renderTypedConstant(state, description.right)})`
    case 'GetElementPtr': {
      const inbounds = description.inbounds ? ' inbounds' : ''
      const indices = description.indices
        .map((constant, position) => {
          const rendered = renderTypedConstant(state, constant)
          return description.inrange === position ? `inrange(${rendered})` : rendered
        })
        .join(', ')
      return `getelementptr${inbounds} (${renderType(state, description.sourceType)}, ${renderTypedConstant(state, description.base)}${indices === '' ? '' : `, ${indices}`})`
    }
    case 'Assembly': {
      const flags = [
        description.sideEffect ? 'sideeffect' : '',
        description.alignStack ? 'alignstack' : '',
        description.intelDialect ? 'inteldialect' : '',
        description.canThrow ? 'unwind' : '',
      ].filter((flag) => flag !== '')
      return `asm${flags.length === 0 ? '' : ` ${flags.join(' ')}`} ${quoted(description.assembly)}, ${quoted(description.constraints)}`
    }
  }
}

/** @internal */
const attributeAt = (
  state: BuilderState.Snapshot,
  index: number,
): AttributeDescription.Description => {
  const description = state.attributes[index]
  if (description === undefined) throw new Error(`missing attribute ${index}`)
  return description
}

/** @internal */
const rawBytes = (value: ByteString.ByteString): string =>
  value.bytes.map((byte) => String.fromCharCode(byte)).join('')

/** @internal */
const renderAttribute = (state: BuilderState.Snapshot, index: number): string => {
  const description = attributeAt(state, index)
  const name = rawBytes(description.name)
  switch (description._tag) {
    case 'Flag':
      return name
    case 'Integer':
      if (name === 'align') return `align ${description.value}`
      if (name === 'memory') {
        const effects = ['none', 'read', 'write', 'readwrite']
        const values = [
          ['argmem', Number(description.value & 3n)],
          ['inaccessiblemem', Number((description.value >> 2n) & 3n)],
          ['other', Number((description.value >> 4n) & 3n)],
        ] as const
        const rendered = values
          .filter(([, effect]) => effect !== 0)
          .map(([location, effect]) => `${location}: ${effects[effect] ?? 'none'}`)
        return rendered.length === 0 ? 'memory(none)' : `memory(${rendered.join(', ')})`
      }
      return `${name}(${description.value})`
    case 'Type':
      return `${name}(${renderType(state, description.type)})`
    case 'String':
      return `${quoted(description.name)}${ByteString.isEmpty(description.value) ? '' : `=${quoted(description.value)}`}`
    case 'IntegerList':
      return `${name}(${description.values.join(',')})`
  }
}

/** @internal */
const renderAttributeSet = (state: BuilderState.Snapshot, index: number): string => {
  const attributes = state.attributeSets[index]
  return attributes === undefined
    ? ''
    : attributes.map((attribute) => renderAttribute(state, attribute)).join(' ')
}

/** @internal */
const globalPrefix = (description: GlobalDescription.GlobalDescription): string => {
  const values = [
    description.linkage === 'external' ? '' : description.linkage,
    description.preemption === 'dso_preemptable' ? '' : description.preemption,
    description.visibility === 'default' ? '' : description.visibility,
    description.dllStorage === 'default' ? '' : description.dllStorage,
    description.unnamedAddress === 'none' ? '' : description.unnamedAddress,
    description.addressSpace.value === 0 ? '' : `addrspace(${description.addressSpace.value})`,
  ].filter((value) => value !== '')
  return values.length === 0 ? '' : `${values.join(' ')} `
}

/** @internal */
const globalSuffix = (description: GlobalDescription.GlobalDescription): string => {
  const values = [
    ByteString.isEmpty(description.section) ? '' : `section ${quoted(description.section)}`,
    description.alignment.byteUnits === undefined ? '' : `align ${description.alignment.byteUnits}`,
  ].filter((value) => value !== '')
  return values.length === 0 ? '' : `, ${values.join(', ')}`
}

interface MetadataRender {
  readonly reachable: Metadata.Reachable
  readonly numbers: ReadonlyMap<number, number>
}

/** @internal */
const resolvedMetadataIndex = (context: MetadataRender, index: number): number => {
  const seen = new Set<number>()
  let current = index
  while (true) {
    if (seen.has(current)) throw new Error('metadata forward-reference cycle')
    seen.add(current)
    const target = context.reachable.resolved.get(current)
    if (target === undefined) return current
    current = target
  }
}

/** @internal */
const metadataEntry = (
  state: BuilderState.Snapshot,
  context: MetadataRender,
  index: number,
): { readonly index: number; readonly entry: MetadataDescription.Entry } => {
  const resolved = resolvedMetadataIndex(context, index)
  const entry = state.metadata[resolved]
  if (entry === undefined) throw new Error(`missing metadata ${resolved}`)
  return { index: resolved, entry }
}

/** @internal */
const metadataString = (
  state: BuilderState.Snapshot,
  context: MetadataRender,
  index: number | undefined,
): string | undefined => {
  if (index === undefined) return undefined
  const resolved = metadataEntry(state, context, index).entry
  if (resolved._tag !== 'String') throw new Error('expected metadata string')
  return quoted(resolved.value)
}

/** @internal */
const renderMetadataReference = (
  state: BuilderState.Snapshot,
  context: MetadataRender,
  index: number | undefined,
): string | undefined => {
  if (index === undefined) return undefined
  const resolved = metadataEntry(state, context, index)
  if (resolved.entry._tag === 'String') return `!${quoted(resolved.entry.value)}`
  if (resolved.entry._tag === 'Forward') throw new Error('unresolved metadata forward reference')
  if (resolved.entry.value._tag === 'Constant') {
    return renderTypedConstant(state, resolved.entry.value.constant)
  }
  if (resolved.entry.value._tag === 'Expression') {
    return `!DIExpression(${resolved.entry.value.elements.join(', ')})`
  }
  const number = context.numbers.get(resolved.index)
  if (number === undefined) throw new Error(`metadata ${resolved.index} has no text number`)
  return `!${number}`
}

/** @internal */
const metadataFields = (
  values: ReadonlyArray<readonly [string, string | number | bigint | boolean | undefined]>,
): string =>
  values
    .flatMap(([name, value]) => (value === undefined ? [] : [`${name}: ${String(value)}`]))
    .join(', ')

/** @internal */
const renderMetadataNode = (
  state: BuilderState.Snapshot,
  context: MetadataRender,
  node: MetadataDescription.Node,
): string => {
  const ref = (index: number | undefined): string | undefined =>
    renderMetadataReference(state, context, index)
  const string = (index: number | undefined): string | undefined =>
    metadataString(state, context, index)
  switch (node._tag) {
    case 'Tuple':
      return `!{${node.elements.map((element) => ref(element) ?? 'null').join(', ')}}`
    case 'Constant':
      return `!{${renderTypedConstant(state, node.constant)}}`
    case 'Local':
      return `!{!${quoted(node.label)}}`
    case 'File':
      return `!DIFile(${metadataFields([
        ['filename', string(node.filename)],
        ['directory', string(node.directory)],
      ])})`
    case 'CompileUnit':
      return `!DICompileUnit(${metadataFields([
        ['language', 'DW_LANG_C99'],
        ['file', ref(node.file)],
        ['producer', string(node.producer)],
        ['isOptimized', node.optimized],
        ['runtimeVersion', 0],
        ['emissionKind', 'FullDebug'],
        ['enums', ref(node.enums)],
        ['globals', ref(node.globals)],
      ])})`
    case 'Subprogram':
      return `!DISubprogram(${metadataFields([
        ['scope', ref(node.file)],
        ['name', string(node.name)],
        ['linkageName', string(node.linkageName)],
        ['file', ref(node.file)],
        ['line', node.line],
        ['type', ref(node.type)],
        ['scopeLine', node.scopeLine],
        ['flags', DIFlags.render({ bits: node.diFlags })],
        ['spFlags', DISPFlags.render({ bits: node.spFlags })],
        ['unit', ref(node.compileUnit)],
      ])})`
    case 'LexicalBlock':
      return `!DILexicalBlock(${metadataFields([
        ['scope', ref(node.scope)],
        ['file', ref(node.file)],
        ['line', node.line],
        ['column', node.column],
      ])})`
    case 'Location':
      return `!DILocation(${metadataFields([
        ['line', node.line],
        ['column', node.column],
        ['scope', ref(node.scope)],
        ['inlinedAt', ref(node.inlinedAt)],
      ])})`
    case 'BasicType': {
      const encoding: Readonly<Record<MetadataDescription.BasicEncoding, string>> = {
        boolean: 'DW_ATE_boolean',
        unsigned: 'DW_ATE_unsigned',
        signed: 'DW_ATE_signed',
        float: 'DW_ATE_float',
      }
      return `!DIBasicType(${metadataFields([
        ['name', string(node.name)],
        ['size', node.sizeInBits],
        ['encoding', encoding[node.encoding]],
      ])})`
    }
    case 'StringType': {
      const encoding: Readonly<Record<MetadataDescription.StringEncoding, string>> = {
        utf: 'DW_ATE_UTF',
      }
      return `!DIStringType(${metadataFields([
        ['name', string(node.name)],
        ['stringLength', ref(node.stringLength)],
        ['stringLengthExpression', ref(node.stringLengthExpression)],
        ['stringLocationExpression', ref(node.stringLocationExpression)],
        ['size', node.sizeInBits],
        ['align', node.alignInBits],
        ['encoding', encoding[node.encoding]],
      ])})`
    }
    case 'CompositeType': {
      const tag: Readonly<Record<MetadataDescription.CompositeKind, string>> = {
        structure: 'DW_TAG_structure_type',
        union: 'DW_TAG_union_type',
        enumeration: 'DW_TAG_enumeration_type',
        array: 'DW_TAG_array_type',
        vector: 'DW_TAG_array_type',
      }
      return `!DICompositeType(${metadataFields([
        ['tag', tag[node.kind]],
        ['name', string(node.name)],
        ['scope', ref(node.scope)],
        ['file', ref(node.file)],
        ['line', node.line],
        ['baseType', ref(node.underlyingType)],
        ['size', node.sizeInBits],
        ['align', node.alignInBits],
        ['flags', DIFlags.render({ bits: node.flags })],
        ['elements', ref(node.fields)],
      ])})`
    }
    case 'DerivedType': {
      const tag: Readonly<Record<MetadataDescription.DerivedKind, string>> = {
        pointer: 'DW_TAG_pointer_type',
        member: 'DW_TAG_member',
        typedef: 'DW_TAG_typedef',
      }
      return `!DIDerivedType(${metadataFields([
        ['tag', tag[node.kind]],
        ['name', string(node.name)],
        ['scope', ref(node.scope)],
        ['file', ref(node.file)],
        ['line', node.line],
        ['baseType', ref(node.underlyingType)],
        ['size', node.sizeInBits],
        ['align', node.alignInBits],
        ['offset', node.offsetInBits],
        ['flags', DIFlags.render({ bits: node.flags })],
      ])})`
    }
    case 'SubroutineType':
      return `!DISubroutineType(${metadataFields([
        ['types', ref(node.types)],
        ['flags', DIFlags.render({ bits: node.flags })],
      ])})`
    case 'Enumerator':
      return `!DIEnumerator(${metadataFields([
        ['name', string(node.name)],
        ['value', node.value],
        ['isUnsigned', node.unsigned ? true : undefined],
      ])})`
    case 'Subrange':
      return `!DISubrange(${metadataFields([
        ['count', ref(node.count)],
        ['lowerBound', ref(node.lowerBound)],
      ])})`
    case 'Expression':
      return `!DIExpression(${node.elements.join(', ')})`
    case 'LocalVariable':
      return `!DILocalVariable(${metadataFields([
        ['name', string(node.name)],
        ['arg', node.argument === 0 ? undefined : node.argument],
        ['scope', ref(node.scope)],
        ['file', ref(node.file)],
        ['line', node.line],
        ['type', ref(node.type)],
        ['flags', DIFlags.render({ bits: node.flags })],
      ])})`
    case 'GlobalVariable':
      return `!DIGlobalVariable(${metadataFields([
        ['name', string(node.name)],
        ['linkageName', string(node.linkageName)],
        ['scope', ref(node.scope)],
        ['file', ref(node.file)],
        ['line', node.line],
        ['type', ref(node.type)],
        ['isLocal', node.local],
        ['isDefinition', true],
      ])})`
    case 'GlobalVariableExpression':
      return `!DIGlobalVariableExpression(${metadataFields([
        ['var', ref(node.variable)],
        ['expr', ref(node.expression)],
      ])})`
  }
}

/** @internal */
const renderMetadataAttachments = (
  state: BuilderState.Snapshot,
  context: MetadataRender,
  attachments: ReadonlyArray<MetadataDescription.Attachment>,
): string =>
  attachments
    .map((attachment) =>
      attachment.kind === 'unpredictable'
        ? '!unpredictable !{}'
        : `!${attachment.kind} ${renderMetadataReference(state, context, attachment.metadata)}`,
    )
    .join(', ')

/** @internal */
const renderVariable = (
  state: BuilderState.Snapshot,
  global: GlobalDescription.GlobalDescription,
  context: MetadataRender,
  globalIndex: number,
): string => {
  const variable = state.variables[global.actorIndex]
  if (variable === undefined) throw new Error('missing variable')
  const threadLocal =
    variable.threadLocal === 'none'
      ? ''
      : variable.threadLocal === 'generaldynamic'
        ? 'thread_local '
        : `thread_local(${variable.threadLocal}) `
  const externallyInitialized = variable.externallyInitialized ? 'externally_initialized ' : ''
  const storage = variable.constant ? 'constant' : 'global'
  const value =
    variable.initializer === undefined
      ? `external ${threadLocal}${storage} ${renderType(state, variable.valueType)}`
      : `${globalPrefix(global)}${threadLocal}${externallyInitialized}${storage} ${renderTypedConstant(state, variable.initializer)}`
  const attachments = renderMetadataAttachments(
    state,
    context,
    state.globalMetadata[globalIndex] ?? [],
  )
  return `${identifier('@', global.name)} = ${value}${globalSuffix(global)}${attachments === '' ? '' : `, ${attachments}`}`
}

/** @internal */
const renderAlias = (
  state: BuilderState.Snapshot,
  global: GlobalDescription.GlobalDescription,
): string => {
  const alias = state.aliases[global.actorIndex]
  if (alias === undefined) throw new Error('missing alias')
  return `${identifier('@', global.name)} = ${globalPrefix(global)}alias ${renderType(state, alias.valueType)}, ${renderTypedConstant(state, alias.aliasee)}${globalSuffix(global)}`
}

/** @internal */
const functionSuffix = (description: GlobalDescription.GlobalDescription): string => {
  const values = [
    ByteString.isEmpty(description.section) ? '' : `section ${quoted(description.section)}`,
    description.alignment.byteUnits === undefined ? '' : `align ${description.alignment.byteUnits}`,
  ].filter((value) => value !== '')
  return values.length === 0 ? '' : ` ${values.join(' ')}`
}

/** @internal */
const localIdentifier = (body: FunctionBodyDescription.Snapshot, index: number): string => {
  const name = body.values[index]?.name
  return identifier(
    '%',
    name === undefined || ByteString.isEmpty(name) ? ByteString.fromString(`v${index}`) : name,
  )
}

/** @internal */
const blockIdentifier = (body: FunctionBodyDescription.Snapshot, index: number): string => {
  const name = body.blocks[index]?.name
  return identifier(
    '%',
    name === undefined || ByteString.isEmpty(name) ? ByteString.fromString(`bb${index}`) : name,
  )
}

/** @internal */
const blockLabel = (body: FunctionBodyDescription.Snapshot, index: number): string => {
  const name = body.blocks[index]?.name
  const value =
    name === undefined || ByteString.isEmpty(name) ? ByteString.fromString(`bb${index}`) : name
  return identifier('%', value).slice(1)
}

/** @internal */
const resolvedOperand = (
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
  return resolvedOperand(body, value.source.resolved, next)
}

/** @internal */
const operandType = (
  state: BuilderState.Snapshot,
  body: FunctionBodyDescription.Snapshot,
  operand: FunctionBodyDescription.Operand,
): number => {
  const resolved = resolvedOperand(body, operand)
  const type =
    resolved._tag === 'Constant'
      ? state.constants[resolved.constant]?.type
      : body.values[resolved.value]?.type
  if (type === undefined) throw new Error('missing operand type')
  return type
}

/** @internal */
const renderOperand = (
  state: BuilderState.Snapshot,
  body: FunctionBodyDescription.Snapshot,
  operand: FunctionBodyDescription.Operand,
): string => {
  const resolved = resolvedOperand(body, operand)
  return resolved._tag === 'Constant'
    ? renderConstant(state, resolved.constant)
    : localIdentifier(body, resolved.value)
}

/** @internal */
const renderTypedOperand = (
  state: BuilderState.Snapshot,
  body: FunctionBodyDescription.Snapshot,
  operand: FunctionBodyDescription.Operand,
): string =>
  `${renderType(state, operandType(state, body, operand))} ${renderOperand(state, body, operand)}`

/** @internal */
const fastMathFlags = (flags: FunctionBodyDescription.FastMath): string => {
  if (
    flags.allowReassociation &&
    flags.noNaNs &&
    flags.noInfinities &&
    flags.noSignedZeros &&
    flags.allowReciprocal &&
    flags.allowContract &&
    flags.approximateFunctions
  ) {
    return 'fast '
  }
  const values = [
    flags.allowReassociation ? 'reassoc' : '',
    flags.noNaNs ? 'nnan' : '',
    flags.noInfinities ? 'ninf' : '',
    flags.noSignedZeros ? 'nsz' : '',
    flags.allowReciprocal ? 'arcp' : '',
    flags.allowContract ? 'contract' : '',
    flags.approximateFunctions ? 'afn' : '',
  ].filter((value) => value !== '')
  return values.length === 0 ? '' : `${values.join(' ')} `
}

/** @internal */
const renderAlignment = (alignment: { readonly byteUnits: bigint | undefined }): string =>
  alignment.byteUnits === undefined ? '' : `, align ${alignment.byteUnits}`

/** @internal */
const renderAtomic = (access: FunctionBodyDescription.MemoryInfo): string =>
  access.ordering === 'none'
    ? ''
    : ` ${MemoryAccess.renderSyncScope(access.syncScope)}${MemoryAccess.renderOrdering(access.ordering)}`

/** @internal */
const renderCall = (
  state: BuilderState.Snapshot,
  body: FunctionBodyDescription.Snapshot,
  instruction: Extract<FunctionBodyDescription.Instruction, { readonly _tag: 'Call' }>,
): string => {
  const signature = typeAt(state, instruction.functionType)
  if (signature._tag !== 'Function') throw new Error('call has non-function type')
  const attributes =
    instruction.attributes === undefined
      ? undefined
      : state.functionAttributeSets[instruction.attributes]
  const returnAttributes =
    attributes === undefined ? '' : renderAttributeSet(state, attributes.returnAttributes)
  const args = instruction.arguments.map((operand, index) => {
    const parameterAttributes =
      attributes?.parameterAttributes[index] === undefined
        ? ''
        : renderAttributeSet(state, attributes.parameterAttributes[index])
    return `${renderType(state, operandType(state, body, operand))}${parameterAttributes === '' ? '' : ` ${parameterAttributes}`} ${renderOperand(state, body, operand)}`
  })
  const functionAttributes =
    attributes === undefined ? '' : renderAttributeSet(state, attributes.functionAttributes)
  const bundles = instruction.operandBundles.map(
    (bundle) =>
      `${quoted(bundle.tag)}(${bundle.operands.map((operand) => renderTypedOperand(state, body, operand)).join(', ')})`,
  )
  const tail = instruction.tail === 'none' ? '' : `${instruction.tail} `
  const convention =
    instruction.callingConvention === 0 ? '' : `cc ${instruction.callingConvention} `
  const result =
    instruction.result === undefined ? '' : `${localIdentifier(body, instruction.result)} = `
  return `${result}${tail}call ${fastMathFlags(instruction.fastMath)}${convention}${returnAttributes === '' ? '' : `${returnAttributes} `}${renderType(state, signature.returnType)} ${renderOperand(state, body, instruction.callee)}(${args.join(', ')})${functionAttributes === '' ? '' : ` ${functionAttributes}`}${bundles.length === 0 ? '' : ` [ ${bundles.join(', ')} ]`}`
}

/** @internal */
const renderInstruction = (
  state: BuilderState.Snapshot,
  body: FunctionBodyDescription.Snapshot,
  instruction: FunctionBodyDescription.Instruction,
): string => {
  const result =
    instruction.result === undefined ? '' : `${localIdentifier(body, instruction.result)} = `
  switch (instruction._tag) {
    case 'Unary':
      return `${result}${instruction.kind} ${fastMathFlags(instruction.fastMath)}${renderTypedOperand(state, body, instruction.operand)}`
    case 'Binary': {
      const flags = instruction.kind.startsWith('f')
        ? fastMathFlags(instruction.fastMath)
        : [
            instruction.integerFlags.noUnsignedWrap ? 'nuw' : '',
            instruction.integerFlags.noSignedWrap ? 'nsw' : '',
            instruction.integerFlags.exact ? 'exact' : '',
          ]
            .filter((value) => value !== '')
            .join(' ')
      return `${result}${instruction.kind}${flags === '' ? '' : ` ${flags}`} ${renderTypedOperand(state, body, instruction.left)}, ${renderOperand(state, body, instruction.right)}`
    }
    case 'Compare':
      return `${result}${instruction.kind === 'integer' ? 'icmp' : 'fcmp'} ${instruction.kind === 'floating' ? fastMathFlags(instruction.fastMath) : ''}${instruction.predicate} ${renderTypedOperand(state, body, instruction.left)}, ${renderOperand(state, body, instruction.right)}`
    case 'Select':
      return `${result}select ${fastMathFlags(instruction.fastMath)}${renderTypedOperand(state, body, instruction.condition)}, ${renderTypedOperand(state, body, instruction.onTrue)}, ${renderTypedOperand(state, body, instruction.onFalse)}`
    case 'Cast': {
      const flags = [
        instruction.noUnsignedWrap ? 'nuw' : '',
        instruction.noSignedWrap ? 'nsw' : '',
      ].filter((value) => value !== '')
      return `${result}${instruction.kind}${flags.length === 0 ? '' : ` ${flags.join(' ')}`} ${renderTypedOperand(state, body, instruction.operand)} to ${renderType(state, instruction.destinationType)}`
    }
    case 'ExtractValue':
      return `${result}extractvalue ${renderTypedOperand(state, body, instruction.aggregate)}, ${instruction.indices.join(', ')}`
    case 'InsertValue':
      return `${result}insertvalue ${renderTypedOperand(state, body, instruction.aggregate)}, ${renderTypedOperand(state, body, instruction.element)}, ${instruction.indices.join(', ')}`
    case 'Alloca': {
      const count = renderTypedOperand(state, body, instruction.count)
      const addressSpace =
        instruction.addressSpace === 0 ? '' : `, addrspace(${instruction.addressSpace})`
      return `${result}alloca${instruction.inAlloca ? ' inalloca' : ''} ${renderType(state, instruction.allocationType)}, ${count}${renderAlignment(instruction.alignment)}${addressSpace}`
    }
    case 'Load':
      return `${result}load${instruction.access.ordering === 'none' ? '' : ' atomic'} ${MemoryAccess.renderKind(instruction.access.kind)}${renderType(state, instruction.valueType)}, ${renderTypedOperand(state, body, instruction.pointer)}${renderAtomic(instruction.access)}${renderAlignment(instruction.access.alignment)}`
    case 'Store':
      return `store${instruction.access.ordering === 'none' ? '' : ' atomic'} ${MemoryAccess.renderKind(instruction.access.kind)}${renderTypedOperand(state, body, instruction.value)}, ${renderTypedOperand(state, body, instruction.pointer)}${renderAtomic(instruction.access)}${renderAlignment(instruction.access.alignment)}`
    case 'GetElementPtr': {
      const flags = [instruction.inbounds ? 'inbounds' : ''].filter((flag) => flag !== '').join(' ')
      const indices = instruction.indices
        .map((operand, position) => {
          const rendered = renderTypedOperand(state, body, operand)
          return instruction.inrange === position ? `inrange(${rendered})` : rendered
        })
        .join(', ')
      return `${result}getelementptr${flags === '' ? '' : ` ${flags}`} ${renderType(state, instruction.sourceType)}, ${renderTypedOperand(state, body, instruction.base)}, ${indices}`
    }
    case 'ExtractElement':
      return `${result}extractelement ${renderTypedOperand(state, body, instruction.vector)}, ${renderTypedOperand(state, body, instruction.index)}`
    case 'InsertElement':
      return `${result}insertelement ${renderTypedOperand(state, body, instruction.vector)}, ${renderTypedOperand(state, body, instruction.element)}, ${renderTypedOperand(state, body, instruction.index)}`
    case 'ShuffleVector':
      return `${result}shufflevector ${renderTypedOperand(state, body, instruction.left)}, ${renderTypedOperand(state, body, instruction.right)}, ${renderTypedOperand(state, body, instruction.mask)}`
    case 'Fence':
      return `fence ${MemoryAccess.renderSyncScope(instruction.syncScope)}${instruction.ordering}`
    case 'CompareExchange':
      return `${result}cmpxchg${instruction.weak ? ' weak' : ''} ${MemoryAccess.renderKind(instruction.access.kind)}${renderTypedOperand(state, body, instruction.pointer)}, ${renderTypedOperand(state, body, instruction.comparison)}, ${renderTypedOperand(state, body, instruction.replacement)} ${MemoryAccess.renderSyncScope(instruction.access.syncScope)}${instruction.access.ordering} ${instruction.failureOrdering}${renderAlignment(instruction.access.alignment)}`
    case 'AtomicRmw':
      return `${result}atomicrmw ${MemoryAccess.renderKind(instruction.access.kind)}${instruction.operation} ${renderTypedOperand(state, body, instruction.pointer)}, ${renderTypedOperand(state, body, instruction.value)} ${MemoryAccess.renderSyncScope(instruction.access.syncScope)}${instruction.access.ordering}${renderAlignment(instruction.access.alignment)}`
    case 'VaArg':
      return `${result}va_arg ${renderTypedOperand(state, body, instruction.list)}, ${renderType(state, instruction.valueType)}`
    case 'IndirectBranch':
      return `indirectbr ${renderTypedOperand(state, body, instruction.address)}, [${instruction.destinations.map((block) => `label ${blockIdentifier(body, block)}`).join(', ')}]`
    case 'Branch':
      return `br label ${blockIdentifier(body, instruction.destination)}`
    case 'ConditionalBranch':
      return `br ${renderTypedOperand(state, body, instruction.condition)}, label ${blockIdentifier(body, instruction.onTrue)}, label ${blockIdentifier(body, instruction.onFalse)}`
    case 'Switch': {
      const cases = instruction.cases
        .map(
          (entry) =>
            `  ${renderTypedConstant(state, entry.value)}, label ${blockIdentifier(body, entry.block)}`,
        )
        .join('\n')
      return `switch ${renderTypedOperand(state, body, instruction.value)}, label ${blockIdentifier(body, instruction.defaultBlock)} [${cases === '' ? '' : `\n${cases}\n`}]`
    }
    case 'Return':
      return `ret ${renderTypedOperand(state, body, instruction.value)}`
    case 'ReturnVoid':
      return 'ret void'
    case 'Unreachable':
      return 'unreachable'
    case 'Phi':
      return `${result}phi ${fastMathFlags(instruction.fastMath)}${renderType(state, instruction.type)} ${instruction.incoming
        .map(
          (entry) =>
            `[ ${renderOperand(state, body, entry.value)}, ${blockIdentifier(body, entry.block)} ]`,
        )
        .join(', ')}`
    case 'Call':
      return renderCall(state, body, instruction)
  }
}

/** @internal */
const renderBody = (
  state: BuilderState.Snapshot,
  body: FunctionBodyDescription.Snapshot,
  context: MetadataRender,
): string =>
  body.blocks
    .map((block, index) => {
      const instructions = block.instructions.map((instructionIndex) => {
        const instruction = body.instructions[instructionIndex]
        if (instruction === undefined) throw new Error(`missing instruction ${instructionIndex}`)
        const attachments: Array<MetadataDescription.Attachment> = [
          ...(body.debugLocations[instructionIndex] === undefined
            ? []
            : [
                Object.freeze({
                  kind: 'dbg' as const,
                  metadata: body.debugLocations[instructionIndex],
                }),
              ]),
          ...(body.metadata[instructionIndex] ?? []),
        ]
        const suffix = renderMetadataAttachments(state, context, attachments)
        return `  ${renderInstruction(state, body, instruction).replaceAll('\n', '\n  ')}${suffix === '' ? '' : `, ${suffix}`}`
      })
      return `${blockLabel(body, index)}:\n${instructions.join('\n')}`
    })
    .join('\n')

/** @internal */
const renderFunction = (
  state: BuilderState.Snapshot,
  global: GlobalDescription.GlobalDescription,
  context: MetadataRender,
  globalIndex: number,
): string => {
  const fn = state.functions[global.actorIndex]
  if (fn === undefined) throw new Error('missing function')
  const type = typeAt(state, fn.type)
  if (type._tag !== 'Function') throw new Error('function declaration has non-function type')
  const attributes =
    fn.attributes === undefined ? undefined : state.functionAttributeSets[fn.attributes]
  const body = fn.body
  const returnAttributes =
    attributes === undefined ? '' : renderAttributeSet(state, attributes.returnAttributes)
  const parameters = type.parameters.map((parameter, index) => {
    const parameterAttributes =
      attributes?.parameterAttributes[index] === undefined
        ? ''
        : renderAttributeSet(state, attributes.parameterAttributes[index])
    const value = body?.arguments[index]
    const name = value === undefined || body === undefined ? '' : ` ${localIdentifier(body, value)}`
    return `${renderType(state, parameter)}${parameterAttributes === '' ? '' : ` ${parameterAttributes}`}${name}`
  })
  if (type.variadic) parameters.push('...')
  const functionAttributes =
    attributes === undefined ? '' : renderAttributeSet(state, attributes.functionAttributes)
  const callingConvention = fn.callingConvention === 0 ? '' : `cc ${fn.callingConvention} `
  const garbageCollector = ByteString.isEmpty(fn.garbageCollector)
    ? ''
    : ` gc ${quoted(fn.garbageCollector)}`
  const metadata = renderMetadataAttachments(
    state,
    context,
    state.globalMetadata[globalIndex] ?? [],
  )
  const header = `${body === undefined ? 'declare' : 'define'} ${globalPrefix(global)}${callingConvention}${returnAttributes === '' ? '' : `${returnAttributes} `}${renderType(state, type.returnType)} ${identifier('@', global.name)}(${parameters.join(', ')})${functionAttributes === '' ? '' : ` ${functionAttributes}`}${garbageCollector}${functionSuffix(global)}${metadata === '' ? '' : ` ${metadata}`}`
  if (body === undefined) return header
  return `${header} {\n${renderBody(state, body, context)}\n}`
}

/** @internal */
const renderSnapshot = (state: BuilderState.Snapshot): string => {
  const lines: Array<string> = []
  const reachable = Metadata.reachable(state, 'IrText.render')
  const numberEntries = reachable.entries.filter((index) => {
    const entry = state.metadata[index]
    return (
      entry?._tag === 'Node' && entry.value._tag !== 'Constant' && entry.value._tag !== 'Expression'
    )
  })
  const context: MetadataRender = {
    reachable,
    numbers: new Map(numberEntries.map((index, number) => [index, number])),
  }

  if (!ByteString.isEmpty(state.moduleName)) {
    lines.push(`; ModuleID = '${ByteString.escapeForIr(state.moduleName)}'`)
  }
  if (!ByteString.isEmpty(state.sourceFilename)) {
    lines.push(`source_filename = ${quoted(state.sourceFilename)}`)
  }
  if (!ByteString.isEmpty(state.dataLayout)) {
    lines.push(`target datalayout = ${quoted(state.dataLayout)}`)
  }
  if (!ByteString.isEmpty(state.targetTriple)) {
    lines.push(`target triple = ${quoted(state.targetTriple)}`)
  }

  const assembly = state.moduleAssembly.flatMap(ByteString.splitLines)
  if (lines.length > 0 && assembly.length > 0) lines.push('')
  for (const fragment of assembly) lines.push(`module asm ${quoted(fragment)}`)

  const namedTypes = state.types.flatMap((description) => {
    if (description._tag !== 'NamedStructure') return []
    const body = description.body
    if (body === undefined) return [`${identifier('%', description.name)} = type opaque`]
    const fields = body.fields.map((field) => renderType(state, field)).join(', ')
    return [
      `${identifier('%', description.name)} = type ${body.packed ? `<{ ${fields} }>` : `{ ${fields} }`}`,
    ]
  })
  if (namedTypes.length > 0) {
    if (lines.length > 0) lines.push('')
    lines.push(...namedTypes)
  }

  const globals = state.globals.flatMap((global, globalIndex) => {
    if (global.deleted || global.replacement !== undefined) return []
    if (global.kind === 'Variable') return [renderVariable(state, global, context, globalIndex)]
    if (global.kind === 'Alias') return [renderAlias(state, global)]
    return [renderFunction(state, global, context, globalIndex)]
  })
  if (globals.length > 0) {
    if (lines.length > 0) lines.push('')
    lines.push(...globals)
  }

  const namedMetadata = state.namedMetadata.map(
    (named) =>
      `!${rawBytes(named.name)} = !{${named.operands
        .map((operand) => renderMetadataReference(state, context, operand))
        .join(', ')}}`,
  )
  const definitions = numberEntries.map((index) => {
    const resolved = metadataEntry(state, context, index)
    if (resolved.entry._tag !== 'Node') throw new Error('numbered metadata is not a node')
    const number = context.numbers.get(resolved.index)
    if (number === undefined) throw new Error('numbered metadata entry has no number')
    return `!${number} = ${resolved.entry.value.distinct ? 'distinct ' : ''}${renderMetadataNode(state, context, resolved.entry.value)}`
  })
  if (namedMetadata.length > 0 || definitions.length > 0) {
    if (lines.length > 0) lines.push('')
    lines.push(...namedMetadata, ...definitions)
  }

  return lines.length === 0 ? '' : `${lines.join('\n')}\n`
}

/**
 * Renders the current builder snapshot as deterministic textual LLVM IR.
 *
 * **Details**
 *
 * Names, escaping, declarations, instructions, attributes, and metadata use the same semantic
 * state as `Bitcode.encode`; rendering does not mutate or consume the builder.
 *
 * **Gotchas**
 *
 * Unresolved or invalid module state fails with {@link LlvmError}.
 *
 * **Example** (Rendering LLVM IR)
 *
 * Render module headers and declarations as LLVM assembly.
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/llvm/Builder'
 * import * as IrText from '@silk-effect/llvm/IrText'
 *
 * const text = await Effect.runPromise(Effect.gen(function* () {
 *   const builder = yield* Builder.make({ sourceFilename: 'example.ll' })
 *   return yield* IrText.render(builder)
 * }))
 * // text === 'source_filename = "example.ll"\n'
 * ```
 *
 * @category serialization
 * @since 0.0.0
 */
export const render = Effect.fn('IrText.render')(function* (
  self: Builder.Builder,
): Effect.fn.Return<string, LlvmError> {
  const state = yield* BuilderState.snapshot(self, 'IrText.render')
  return yield* Effect.try({
    try: () => renderSnapshot(state),
    catch: (cause) =>
      wrappedFailure({
        operation: 'IrText.render',
        message:
          cause instanceof Error
            ? `LLVM IR rendering failed: ${cause.message}`
            : 'LLVM IR rendering failed',
        cause: cause,
      }),
  })
})
