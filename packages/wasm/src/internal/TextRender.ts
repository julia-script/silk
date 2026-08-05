/**
 * Text rendering of committed module state as WebAssembly text format.
 *
 * Output goals: deterministic character-identical text for identical state, `$`-identifiers
 * where sanitizable names exist, and a parse (by the pinned oracle) that produces a module
 * byte-identical to {@link BinaryEncode.encodeModule}'s output.
 *
 * @internal
 */
import * as Result from 'effect/Result'
import type * as Instr from '../Instr.js'
import * as ValType from '../ValType.js'
import { invalidState, type WasmError } from '../WasmError.js'
import * as Handle from './Handle.js'
import type * as IndexSpace from './IndexSpace.js'
import * as InstructionTable from './InstructionTable.js'
import type * as ModuleState from './ModuleState.js'
import * as Utf8 from './Utf8.js'

class Abort {
  constructor(readonly error: WasmError) {}
}

const orAbort = <A>(result: Result.Result<A, WasmError>): A => {
  if (Result.isFailure(result)) throw new Abort(result.failure)
  return Result.getOrThrow(result)
}

/** Characters permitted in a text-format `$` identifier. */
const idPattern = /^[0-9A-Za-z!#$%&'*+\-./:<=>?@\\^_`|~]+$/

interface Space {
  /** Entry index to `$id` or emitted numeric index. */
  readonly refs: ReadonlyArray<string>
}

const identifier = (name: string): string =>
  idPattern.test(name) ? `$${name}` : `$${escapeName(name)}`

const assignIds = (
  names: ReadonlyArray<string | undefined>,
  emittedIndex: ReadonlyArray<number>,
): Space => {
  const refs = names.map((name, entryIndex) =>
    name === undefined ? String(emittedIndex[entryIndex] ?? entryIndex) : identifier(name),
  )
  return { refs }
}

interface Context {
  readonly snapshot: ModuleState.Snapshot
  readonly spaces: IndexSpace.Spaces
  readonly operation: string
  readonly funcs: Space
  readonly tables: Space
  readonly memories: Space
  readonly globals: Space
  readonly tags: Space
  readonly elems: Space
  readonly datas: Space
  readonly lines: Array<string>
}

const ref = (context: Context, space: Space, entryIndex: number): string =>
  space.refs[entryIndex] ??
  orAbort(
    Result.fail(
      invalidState({
        operation: context.operation,
        message: 'Entity reference is missing',
        state: entryIndex,
      }),
    ),
  )

const resolveRef = (
  context: Context,
  handle: Handle.Handle<'Func' | 'Table' | 'Memory' | 'Global' | 'Tag' | 'Elem' | 'Data' | 'Type'>,
  tag: 'Func' | 'Table' | 'Memory' | 'Global' | 'Tag' | 'Elem' | 'Data' | 'Type',
): { readonly text: string; readonly entryIndex: number; readonly emitted: number } => {
  const entryIndex = orAbort(Handle.resolve(context.snapshot.owner, handle, tag, context.operation))
  switch (tag) {
    case 'Func':
      return {
        text: ref(context, context.funcs, entryIndex),
        entryIndex,
        emitted: context.spaces.funcs[entryIndex] ?? entryIndex,
      }
    case 'Table':
      return {
        text: ref(context, context.tables, entryIndex),
        entryIndex,
        emitted: context.spaces.tables[entryIndex] ?? entryIndex,
      }
    case 'Memory':
      return {
        text: ref(context, context.memories, entryIndex),
        entryIndex,
        emitted: context.spaces.memories[entryIndex] ?? entryIndex,
      }
    case 'Global':
      return {
        text: ref(context, context.globals, entryIndex),
        entryIndex,
        emitted: context.spaces.globals[entryIndex] ?? entryIndex,
      }
    case 'Tag':
      return {
        text: ref(context, context.tags, entryIndex),
        entryIndex,
        emitted: context.spaces.tags[entryIndex] ?? entryIndex,
      }
    case 'Elem':
      return { text: ref(context, context.elems, entryIndex), entryIndex, emitted: entryIndex }
    case 'Data':
      return { text: ref(context, context.datas, entryIndex), entryIndex, emitted: entryIndex }
    case 'Type':
      return { text: String(entryIndex), entryIndex, emitted: entryIndex }
  }
}

const escapeString = (bytes: Uint8Array): string => {
  let out = ''
  for (const byte of bytes) {
    if (byte === 0x22) out += '\\"'
    else if (byte === 0x5c) out += '\\\\'
    else if (byte >= 0x20 && byte < 0x7f) out += String.fromCharCode(byte)
    else out += `\\${byte.toString(16).padStart(2, '0')}`
  }
  return `"${out}"`
}

const escapeName = (name: string): string => escapeString(Utf8.encode(name))

const floatText = (value: number): string => {
  if (Number.isNaN(value)) return 'nan'
  if (value === Number.POSITIVE_INFINITY) return 'inf'
  if (value === Number.NEGATIVE_INFINITY) return '-inf'
  if (Object.is(value, -0)) return '-0'
  return String(value)
}

const blockTypeText = (context: Context, blockType: Instr.BlockType): string => {
  switch (blockType._tag) {
    case 'Empty':
      return ''
    case 'Value':
      return ` (result ${ValType.text(blockType.type)})`
    case 'Func':
      return ` (type ${resolveRef(context, blockType.type, 'Type').text})`
  }
}

const heapTypeText = (refType: ValType.RefType): string => {
  switch (refType._tag) {
    case 'FuncRef':
      return 'func'
    case 'ExternRef':
      return 'extern'
    case 'ExnRef':
      return 'exn'
  }
}

const memArgText = (
  context: Context,
  mnemonic: string,
  naturalAlign: number,
  instr: {
    readonly memory: Parameters<typeof resolveRef>[1]
    readonly align: number
    readonly offset: number | bigint
  },
): string => {
  const memory = resolveRef(context, instr.memory, 'Memory')
  let text = mnemonic
  if (memory.emitted !== 0) text += ` ${memory.text}`
  if (BigInt(instr.offset) !== 0n) text += ` offset=${instr.offset}`
  if (instr.align !== naturalAlign) text += ` align=${2 ** instr.align}`
  return text
}

const instrText = (context: Context, instr: Instr.Instr): string => {
  switch (instr._tag) {
    case 'Op':
      return instr.mnemonic
    case 'I32Const':
      return `i32.const ${instr.value | 0}`
    case 'I64Const':
      return `i64.const ${BigInt.asIntN(64, instr.value)}`
    case 'F32Const':
      return `f32.const ${floatText(Math.fround(instr.value))}`
    case 'F64Const':
      return `f64.const ${floatText(instr.value)}`
    case 'LocalGet':
      return `local.get ${instr.local}`
    case 'LocalSet':
      return `local.set ${instr.local}`
    case 'LocalTee':
      return `local.tee ${instr.local}`
    case 'GlobalGet':
      return `global.get ${resolveRef(context, instr.global, 'Global').text}`
    case 'GlobalSet':
      return `global.set ${resolveRef(context, instr.global, 'Global').text}`
    case 'Call':
      return `call ${resolveRef(context, instr.func, 'Func').text}`
    case 'CallIndirect':
      return `call_indirect ${resolveRef(context, instr.table, 'Table').text} (type ${
        resolveRef(context, instr.type, 'Type').text
      })`
    case 'ReturnCall':
      return `return_call ${resolveRef(context, instr.func, 'Func').text}`
    case 'ReturnCallIndirect':
      return `return_call_indirect ${resolveRef(context, instr.table, 'Table').text} (type ${
        resolveRef(context, instr.type, 'Type').text
      })`
    case 'Br':
      return `br ${instr.depth}`
    case 'BrIf':
      return `br_if ${instr.depth}`
    case 'BrTable':
      return `br_table ${[...instr.depths, instr.defaultDepth].join(' ')}`
    case 'SelectTyped':
      return `select (result ${instr.types.map(ValType.text).join(' ')})`
    case 'RefNull':
      return `ref.null ${heapTypeText(instr.refType)}`
    case 'RefFunc':
      return `ref.func ${resolveRef(context, instr.func, 'Func').text}`
    case 'Throw':
      return `throw ${resolveRef(context, instr.tag, 'Tag').text}`
    case 'MemoryAccess':
      return memArgText(
        context,
        instr.mnemonic,
        InstructionTable.memoryAccessOps[instr.mnemonic].widthLog2,
        instr,
      )
    case 'V128Const':
      return `v128.const i8x16 ${instr.bytes.join(' ')}`
    case 'Shuffle':
      return `i8x16.shuffle ${instr.lanes.join(' ')}`
    case 'SimdLane':
      return `${instr.mnemonic} ${instr.lane}`
    case 'SimdMemoryAccess':
      return memArgText(
        context,
        instr.mnemonic,
        InstructionTable.simdMemoryAccessOps[instr.mnemonic].widthLog2,
        instr,
      )
    case 'SimdMemoryLane':
      return `${memArgText(
        context,
        instr.mnemonic,
        InstructionTable.simdLaneMemoryOps[instr.mnemonic].widthLog2,
        instr,
      )} ${instr.lane}`
    case 'AtomicAccess':
      return memArgText(
        context,
        instr.mnemonic,
        InstructionTable.atomicAccessOps[instr.mnemonic].widthLog2,
        instr,
      )
    case 'MemorySize': {
      const memory = resolveRef(context, instr.memory, 'Memory')
      return memory.emitted === 0 ? 'memory.size' : `memory.size ${memory.text}`
    }
    case 'MemoryGrow': {
      const memory = resolveRef(context, instr.memory, 'Memory')
      return memory.emitted === 0 ? 'memory.grow' : `memory.grow ${memory.text}`
    }
    case 'MemoryInit': {
      const memory = resolveRef(context, instr.memory, 'Memory')
      const data = resolveRef(context, instr.data, 'Data')
      return memory.emitted === 0
        ? `memory.init ${data.text}`
        : `memory.init ${memory.text} ${data.text}`
    }
    case 'DataDrop':
      return `data.drop ${resolveRef(context, instr.data, 'Data').text}`
    case 'MemoryCopy': {
      const destination = resolveRef(context, instr.destination, 'Memory')
      const source = resolveRef(context, instr.source, 'Memory')
      return destination.emitted === 0 && source.emitted === 0
        ? 'memory.copy'
        : `memory.copy ${destination.text} ${source.text}`
    }
    case 'MemoryFill': {
      const memory = resolveRef(context, instr.memory, 'Memory')
      return memory.emitted === 0 ? 'memory.fill' : `memory.fill ${memory.text}`
    }
    case 'TableGet':
      return `table.get ${resolveRef(context, instr.table, 'Table').text}`
    case 'TableSet':
      return `table.set ${resolveRef(context, instr.table, 'Table').text}`
    case 'TableSize':
      return `table.size ${resolveRef(context, instr.table, 'Table').text}`
    case 'TableGrow':
      return `table.grow ${resolveRef(context, instr.table, 'Table').text}`
    case 'TableFill':
      return `table.fill ${resolveRef(context, instr.table, 'Table').text}`
    case 'TableCopy':
      return `table.copy ${resolveRef(context, instr.destination, 'Table').text} ${
        resolveRef(context, instr.source, 'Table').text
      }`
    case 'TableInit':
      return `table.init ${resolveRef(context, instr.table, 'Table').text} ${
        resolveRef(context, instr.elem, 'Elem').text
      }`
    case 'ElemDrop':
      return `elem.drop ${resolveRef(context, instr.elem, 'Elem').text}`
    case 'Block':
    case 'Loop':
    case 'If':
    case 'TryTable':
      return orAbort(
        Result.fail(
          invalidState({
            operation: context.operation,
            message: 'Structured instructions are rendered by the body walker',
            state: instr._tag,
          }),
        ),
      )
  }
}

const hintAnnotation = (hint: Instr.BranchHint): string =>
  `(@metadata.code.branch_hint "\\${hint === 'likely' ? '01' : '00'}")`

const catchText = (context: Context, clause: Instr.Catch): string => {
  switch (clause._tag) {
    case 'Catch':
      return `(catch ${resolveRef(context, clause.tag, 'Tag').text} ${clause.depth})`
    case 'CatchRef':
      return `(catch_ref ${resolveRef(context, clause.tag, 'Tag').text} ${clause.depth})`
    case 'CatchAll':
      return `(catch_all ${clause.depth})`
    case 'CatchAllRef':
      return `(catch_all_ref ${clause.depth})`
  }
}

const renderBody = (context: Context, body: ReadonlyArray<Instr.Instr>, indent: string): void => {
  for (const instr of body) {
    switch (instr._tag) {
      case 'Block':
      case 'Loop': {
        const keyword = instr._tag === 'Block' ? 'block' : 'loop'
        context.lines.push(`${indent}${keyword}${blockTypeText(context, instr.blockType)}`)
        renderBody(context, instr.body, `${indent}  `)
        context.lines.push(`${indent}end`)
        break
      }
      case 'TryTable': {
        const catches = instr.catches.map((clause) => ` ${catchText(context, clause)}`).join('')
        context.lines.push(`${indent}try_table${blockTypeText(context, instr.blockType)}${catches}`)
        renderBody(context, instr.body, `${indent}  `)
        context.lines.push(`${indent}end`)
        break
      }
      case 'If': {
        if (instr.hint !== undefined) context.lines.push(indent + hintAnnotation(instr.hint))
        context.lines.push(`${indent}if${blockTypeText(context, instr.blockType)}`)
        renderBody(context, instr.thenBody, `${indent}  `)
        if (instr.elseBody.length > 0) {
          context.lines.push(`${indent}else`)
          renderBody(context, instr.elseBody, `${indent}  `)
        }
        context.lines.push(`${indent}end`)
        break
      }
      default:
        if (instr._tag === 'BrIf' && instr.hint !== undefined) {
          context.lines.push(indent + hintAnnotation(instr.hint))
        }
        context.lines.push(indent + instrText(context, instr))
    }
  }
}

const constExprText = (context: Context, expr: ReadonlyArray<Instr.Instr>): string =>
  expr.map((instr) => instrText(context, instr)).join(' ')

const limitsText = (limits: ModuleState.Limits): string =>
  limits.max === undefined ? `${limits.min}` : `${limits.min} ${limits.max}`

const memoryTypeText = (entry: {
  readonly limits: ModuleState.Limits
  readonly shared: boolean
  readonly addressType: ModuleState.AddressType
}): string => {
  const address = entry.addressType === 'i64' ? 'i64 ' : ''
  const shared = entry.shared ? ' shared' : ''
  return `${address}${limitsText(entry.limits)}${shared}`
}

const tableTypeText = (entry: {
  readonly limits: ModuleState.Limits
  readonly addressType: ModuleState.AddressType
  readonly refType: Parameters<typeof ValType.text>[0]
}): string => {
  const address = entry.addressType === 'i64' ? 'i64 ' : ''
  return `${address}${limitsText(entry.limits)} ${ValType.text(entry.refType)}`
}

const globalTypeText = (entry: {
  readonly valType: ValType.ValType
  readonly mutable: boolean
}): string => (entry.mutable ? `(mut ${ValType.text(entry.valType)})` : ValType.text(entry.valType))

/**
 * Renders validated module state as WebAssembly text.
 *
 * @internal
 */
export const renderModule = (
  snapshot: ModuleState.Snapshot,
  spaces: IndexSpace.Spaces,
  operation: string,
): Result.Result<string, WasmError> => {
  const context: Context = {
    snapshot,
    spaces,
    operation,
    funcs: assignIds(
      snapshot.funcs.map((entry) => entry.name),
      spaces.funcs,
    ),
    tables: assignIds(
      snapshot.tables.map((entry) => entry.name),
      spaces.tables,
    ),
    memories: assignIds(
      snapshot.memories.map((entry) => entry.name),
      spaces.memories,
    ),
    globals: assignIds(
      snapshot.globals.map((entry) => entry.name),
      spaces.globals,
    ),
    tags: assignIds(
      snapshot.tags.map((entry) => entry.name),
      spaces.tags,
    ),
    elems: assignIds(
      snapshot.elems.map((entry) => entry.name),
      snapshot.elems.map((_, index) => index),
    ),
    datas: assignIds(
      snapshot.datas.map((entry) => entry.name),
      snapshot.datas.map((_, index) => index),
    ),
    lines: [],
  }
  try {
    const { lines } = context
    const moduleId = snapshot.moduleName === undefined ? '' : ` ${identifier(snapshot.moduleName)}`
    lines.push(`(module${moduleId}`)

    for (const funcType of snapshot.types) {
      const params =
        funcType.params.length === 0
          ? ''
          : ` (param ${funcType.params.map(ValType.text).join(' ')})`
      const results =
        funcType.results.length === 0
          ? ''
          : ` (result ${funcType.results.map(ValType.text).join(' ')})`
      lines.push(`  (type (func${params}${results}))`)
    }

    const importable = (id: string): string => (id.startsWith('$') ? ` ${id}` : '')
    for (const entryIndex of spaces.funcOrder) {
      const entry = snapshot.funcs[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      const id = importable(ref(context, context.funcs, entryIndex))
      lines.push(
        `  (import ${escapeName(source.module)} ${escapeName(source.field)} ` +
          `(func${id} (type ${entry.typeIndex})))`,
      )
    }
    for (const entryIndex of spaces.tableOrder) {
      const entry = snapshot.tables[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      const id = importable(ref(context, context.tables, entryIndex))
      lines.push(
        `  (import ${escapeName(source.module)} ${escapeName(source.field)} ` +
          `(table${id} ${tableTypeText(entry)}))`,
      )
    }
    for (const entryIndex of spaces.memoryOrder) {
      const entry = snapshot.memories[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      const id = importable(ref(context, context.memories, entryIndex))
      lines.push(
        `  (import ${escapeName(source.module)} ${escapeName(source.field)} ` +
          `(memory${id} ${memoryTypeText(entry)}))`,
      )
    }
    for (const entryIndex of spaces.globalOrder) {
      const entry = snapshot.globals[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      const id = importable(ref(context, context.globals, entryIndex))
      lines.push(
        `  (import ${escapeName(source.module)} ${escapeName(source.field)} ` +
          `(global${id} ${globalTypeText(entry)}))`,
      )
    }
    for (const entryIndex of spaces.tagOrder) {
      const entry = snapshot.tags[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      const id = importable(ref(context, context.tags, entryIndex))
      lines.push(
        `  (import ${escapeName(source.module)} ${escapeName(source.field)} ` +
          `(tag${id} (type ${entry.typeIndex})))`,
      )
    }

    for (const entryIndex of spaces.funcOrder) {
      const entry = snapshot.funcs[entryIndex]
      if (entry === undefined || entry.importSource !== undefined) continue
      const definition = entry.definition
      if (definition === undefined) continue
      const id = importable(ref(context, context.funcs, entryIndex))
      lines.push(`  (func${id} (type ${entry.typeIndex})`)
      for (const local of definition.locals) {
        const localId = local.name === undefined ? '' : ` ${identifier(local.name)}`
        lines.push(`    (local${localId} ${ValType.text(local.type)})`)
      }
      renderBody(context, definition.body, '    ')
      lines.push('  )')
    }

    for (const entryIndex of spaces.tableOrder) {
      const entry = snapshot.tables[entryIndex]
      if (entry === undefined || entry.importSource !== undefined) continue
      const id = importable(ref(context, context.tables, entryIndex))
      lines.push(`  (table${id} ${tableTypeText(entry)})`)
    }
    for (const entryIndex of spaces.memoryOrder) {
      const entry = snapshot.memories[entryIndex]
      if (entry === undefined || entry.importSource !== undefined) continue
      const id = importable(ref(context, context.memories, entryIndex))
      lines.push(`  (memory${id} ${memoryTypeText(entry)})`)
    }
    for (const entryIndex of spaces.globalOrder) {
      const entry = snapshot.globals[entryIndex]
      if (entry === undefined || entry.importSource !== undefined) continue
      if (entry.init === undefined) continue
      const id = importable(ref(context, context.globals, entryIndex))
      lines.push(`  (global${id} ${globalTypeText(entry)} ${constExprText(context, entry.init)})`)
    }

    for (const entryIndex of spaces.tagOrder) {
      const entry = snapshot.tags[entryIndex]
      if (entry === undefined || entry.importSource !== undefined) continue
      const id = importable(ref(context, context.tags, entryIndex))
      lines.push(`  (tag${id} (type ${entry.typeIndex}))`)
    }

    for (const moduleExport of snapshot.exports) {
      const target = (() => {
        switch (moduleExport.kind) {
          case 'func':
            return `func ${ref(context, context.funcs, moduleExport.entryIndex)}`
          case 'table':
            return `table ${ref(context, context.tables, moduleExport.entryIndex)}`
          case 'memory':
            return `memory ${ref(context, context.memories, moduleExport.entryIndex)}`
          case 'global':
            return `global ${ref(context, context.globals, moduleExport.entryIndex)}`
          case 'tag':
            return `tag ${ref(context, context.tags, moduleExport.entryIndex)}`
        }
      })()
      lines.push(`  (export ${escapeName(moduleExport.name)} (${target}))`)
    }

    if (snapshot.start !== undefined) {
      lines.push(`  (start ${ref(context, context.funcs, snapshot.start)})`)
    }

    for (const [entryIndex, elem] of snapshot.elems.entries()) {
      const id = importable(ref(context, context.elems, entryIndex))
      const items = elem.items.map((item) => `(${constExprText(context, item)})`).join(' ')
      const itemsText = items.length === 0 ? '' : ` ${items}`
      const refTypeText = ValType.text(elem.refType)
      switch (elem.mode._tag) {
        case 'Active': {
          const tableIndex = spaces.tables[elem.mode.table] ?? 0
          const table =
            tableIndex === 0 ? '' : ` (table ${ref(context, context.tables, elem.mode.table)})`
          lines.push(
            `  (elem${id}${table} (offset ${constExprText(context, elem.mode.offset)}) ` +
              `${refTypeText}${itemsText})`,
          )
          break
        }
        case 'Passive':
          lines.push(`  (elem${id} ${refTypeText}${itemsText})`)
          break
        case 'Declarative':
          lines.push(`  (elem${id} declare ${refTypeText}${itemsText})`)
          break
      }
    }

    for (const [entryIndex, data] of snapshot.datas.entries()) {
      const id = importable(ref(context, context.datas, entryIndex))
      const bytes = escapeString(data.bytes)
      if (data.mode._tag === 'Passive') {
        lines.push(`  (data${id} ${bytes})`)
      } else {
        const memoryIndex = spaces.memories[data.mode.memory] ?? 0
        const memory =
          memoryIndex === 0 ? '' : ` (memory ${ref(context, context.memories, data.mode.memory)})`
        lines.push(
          `  (data${id}${memory} (offset ${constExprText(context, data.mode.offset)}) ${bytes})`,
        )
      }
    }

    lines.push(')')
    return Result.succeed(`${lines.join('\n')}\n`)
  } catch (failure) {
    if (failure instanceof Abort) return Result.fail(failure.error)
    throw failure
  }
}
