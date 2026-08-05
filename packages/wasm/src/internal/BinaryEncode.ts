/**
 * Binary encoding of committed module state: instruction sequences, sections, and the `name`
 * custom section. All handle resolution failures abort with a private non-yieldable failure
 * translated to a typed `Result` once at {@link encodeModule}.
 *
 * @internal
 */
import * as Result from 'effect/Result'
import type * as Instr from '../Instr.js'
import * as ValType from '../ValType.js'
import { invalidState, type WasmError } from '../WasmError.js'
import { ByteWriter } from './ByteWriter.js'
import * as Handle from './Handle.js'
import type * as IndexSpace from './IndexSpace.js'
import * as InstructionTable from './InstructionTable.js'
import type * as ModuleState from './ModuleState.js'

class Abort {
  constructor(readonly error: WasmError) {}
}

interface Context {
  readonly snapshot: ModuleState.Snapshot
  readonly spaces: IndexSpace.Spaces
  readonly operation: string
}

const abort = (context: Context, message: string): never => {
  throw new Abort(invalidState({ operation: context.operation, message, state: undefined }))
}

const orAbort = <A>(result: Result.Result<A, WasmError>): A => {
  if (Result.isFailure(result)) throw new Abort(result.failure)
  return Result.getOrThrow(result)
}

const spaceIndex = (
  context: Context,
  handle: Handle.Handle<'Func' | 'Table' | 'Memory' | 'Global' | 'Elem' | 'Data' | 'Type'>,
  tag: 'Func' | 'Table' | 'Memory' | 'Global' | 'Elem' | 'Data' | 'Type',
): number => {
  const entryIndex = orAbort(Handle.resolve(context.snapshot.owner, handle, tag, context.operation))
  switch (tag) {
    case 'Func':
      return context.spaces.funcs[entryIndex] ?? abort(context, 'Function index is missing')
    case 'Table':
      return context.spaces.tables[entryIndex] ?? abort(context, 'Table index is missing')
    case 'Memory':
      return context.spaces.memories[entryIndex] ?? abort(context, 'Memory index is missing')
    case 'Global':
      return context.spaces.globals[entryIndex] ?? abort(context, 'Global index is missing')
    case 'Elem':
    case 'Data':
    case 'Type':
      return entryIndex
  }
}

const refTypeByte = (refType: ValType.RefType): number => ValType.binary(refType)

const encodeBlockType = (
  context: Context,
  writer: ByteWriter,
  blockType: Instr.BlockType,
): void => {
  switch (blockType._tag) {
    case 'Empty':
      writer.byte(0x40)
      return
    case 'Value':
      writer.byte(ValType.binary(blockType.type))
      return
    case 'Func':
      writer.s33(spaceIndex(context, blockType.type, 'Type'))
      return
  }
}

const encodeMemArg = (
  writer: ByteWriter,
  align: number,
  offset: number | bigint,
  memoryIndex: number,
): void => {
  if (memoryIndex === 0) {
    writer.u32(align)
  } else {
    writer.u32(align | 0x40)
    writer.u32(memoryIndex)
  }
  writer.u64(BigInt(offset))
}

export const encodeInstr = (context: Context, writer: ByteWriter, instr: Instr.Instr): void => {
  const ops = InstructionTable.opcodes
  const fc = InstructionTable.fcOpcodes
  switch (instr._tag) {
    case 'Op':
      writer.raw(InstructionTable.plainOps[instr.mnemonic].opcode)
      return
    case 'I32Const':
      writer.byte(ops.i32Const).s32(instr.value | 0)
      return
    case 'I64Const':
      writer.byte(ops.i64Const).s64(instr.value)
      return
    case 'F32Const':
      writer.byte(ops.f32Const).f32(instr.value)
      return
    case 'F64Const':
      writer.byte(ops.f64Const).f64(instr.value)
      return
    case 'LocalGet':
      writer.byte(ops.localGet).u32(instr.local)
      return
    case 'LocalSet':
      writer.byte(ops.localSet).u32(instr.local)
      return
    case 'LocalTee':
      writer.byte(ops.localTee).u32(instr.local)
      return
    case 'GlobalGet':
      writer.byte(ops.globalGet).u32(spaceIndex(context, instr.global, 'Global'))
      return
    case 'GlobalSet':
      writer.byte(ops.globalSet).u32(spaceIndex(context, instr.global, 'Global'))
      return
    case 'Call':
      writer.byte(ops.call).u32(spaceIndex(context, instr.func, 'Func'))
      return
    case 'CallIndirect':
      writer
        .byte(ops.callIndirect)
        .u32(spaceIndex(context, instr.type, 'Type'))
        .u32(spaceIndex(context, instr.table, 'Table'))
      return
    case 'ReturnCall':
      writer.byte(ops.returnCall).u32(spaceIndex(context, instr.func, 'Func'))
      return
    case 'ReturnCallIndirect':
      writer
        .byte(ops.returnCallIndirect)
        .u32(spaceIndex(context, instr.type, 'Type'))
        .u32(spaceIndex(context, instr.table, 'Table'))
      return
    case 'Block':
    case 'Loop': {
      writer.byte(instr._tag === 'Block' ? ops.block : ops.loop)
      encodeBlockType(context, writer, instr.blockType)
      for (const nested of instr.body) encodeInstr(context, writer, nested)
      writer.byte(ops.end)
      return
    }
    case 'If': {
      writer.byte(ops.if)
      encodeBlockType(context, writer, instr.blockType)
      for (const nested of instr.thenBody) encodeInstr(context, writer, nested)
      if (instr.elseBody.length > 0) {
        writer.byte(ops.else)
        for (const nested of instr.elseBody) encodeInstr(context, writer, nested)
      }
      writer.byte(ops.end)
      return
    }
    case 'Br':
      writer.byte(ops.br).u32(instr.depth)
      return
    case 'BrIf':
      writer.byte(ops.brIf).u32(instr.depth)
      return
    case 'BrTable': {
      writer.byte(ops.brTable).u32(instr.depths.length)
      for (const depth of instr.depths) writer.u32(depth)
      writer.u32(instr.defaultDepth)
      return
    }
    case 'SelectTyped': {
      writer.byte(ops.selectTyped).u32(instr.types.length)
      for (const type of instr.types) writer.byte(ValType.binary(type))
      return
    }
    case 'RefNull':
      writer.byte(ops.refNull).byte(refTypeByte(instr.refType))
      return
    case 'RefFunc':
      writer.byte(ops.refFunc).u32(spaceIndex(context, instr.func, 'Func'))
      return
    case 'MemoryAccess': {
      const row = InstructionTable.memoryAccessOps[instr.mnemonic]
      writer.byte(row.opcode)
      encodeMemArg(writer, instr.align, instr.offset, spaceIndex(context, instr.memory, 'Memory'))
      return
    }
    case 'MemorySize':
      writer.byte(ops.memorySize).u32(spaceIndex(context, instr.memory, 'Memory'))
      return
    case 'MemoryGrow':
      writer.byte(ops.memoryGrow).u32(spaceIndex(context, instr.memory, 'Memory'))
      return
    case 'MemoryInit':
      writer
        .byte(ops.prefixFc)
        .u32(fc.memoryInit)
        .u32(spaceIndex(context, instr.data, 'Data'))
        .u32(spaceIndex(context, instr.memory, 'Memory'))
      return
    case 'DataDrop':
      writer
        .byte(ops.prefixFc)
        .u32(fc.dataDrop)
        .u32(spaceIndex(context, instr.data, 'Data'))
      return
    case 'MemoryCopy':
      writer
        .byte(ops.prefixFc)
        .u32(fc.memoryCopy)
        .u32(spaceIndex(context, instr.destination, 'Memory'))
        .u32(spaceIndex(context, instr.source, 'Memory'))
      return
    case 'MemoryFill':
      writer
        .byte(ops.prefixFc)
        .u32(fc.memoryFill)
        .u32(spaceIndex(context, instr.memory, 'Memory'))
      return
    case 'TableGet':
      writer.byte(ops.tableGet).u32(spaceIndex(context, instr.table, 'Table'))
      return
    case 'TableSet':
      writer.byte(ops.tableSet).u32(spaceIndex(context, instr.table, 'Table'))
      return
    case 'TableSize':
      writer
        .byte(ops.prefixFc)
        .u32(fc.tableSize)
        .u32(spaceIndex(context, instr.table, 'Table'))
      return
    case 'TableGrow':
      writer
        .byte(ops.prefixFc)
        .u32(fc.tableGrow)
        .u32(spaceIndex(context, instr.table, 'Table'))
      return
    case 'TableFill':
      writer
        .byte(ops.prefixFc)
        .u32(fc.tableFill)
        .u32(spaceIndex(context, instr.table, 'Table'))
      return
    case 'TableCopy':
      writer
        .byte(ops.prefixFc)
        .u32(fc.tableCopy)
        .u32(spaceIndex(context, instr.destination, 'Table'))
        .u32(spaceIndex(context, instr.source, 'Table'))
      return
    case 'TableInit':
      writer
        .byte(ops.prefixFc)
        .u32(fc.tableInit)
        .u32(spaceIndex(context, instr.elem, 'Elem'))
        .u32(spaceIndex(context, instr.table, 'Table'))
      return
    case 'ElemDrop':
      writer
        .byte(ops.prefixFc)
        .u32(fc.elemDrop)
        .u32(spaceIndex(context, instr.elem, 'Elem'))
      return
    case 'V128Const': {
      writer.byte(ops.prefixSimd).u32(ops.v128Const)
      for (const byte of instr.bytes) writer.byte(byte)
      return
    }
    case 'Shuffle': {
      writer.byte(ops.prefixSimd).u32(ops.i8x16Shuffle)
      for (const laneIndex of instr.lanes) writer.byte(laneIndex)
      return
    }
    case 'SimdLane': {
      const row = InstructionTable.simdLaneOps[instr.mnemonic]
      writer.byte(ops.prefixSimd).u32(row.subopcode).byte(instr.lane)
      return
    }
    case 'SimdMemoryAccess': {
      const row = InstructionTable.simdMemoryAccessOps[instr.mnemonic]
      writer.byte(ops.prefixSimd).u32(row.subopcode)
      encodeMemArg(writer, instr.align, instr.offset, spaceIndex(context, instr.memory, 'Memory'))
      return
    }
    case 'SimdMemoryLane': {
      const row = InstructionTable.simdLaneMemoryOps[instr.mnemonic]
      writer.byte(ops.prefixSimd).u32(row.subopcode)
      encodeMemArg(writer, instr.align, instr.offset, spaceIndex(context, instr.memory, 'Memory'))
      writer.byte(instr.lane)
      return
    }
    case 'AtomicAccess': {
      const row = InstructionTable.atomicAccessOps[instr.mnemonic]
      writer.byte(ops.prefixAtomic).u32(row.subopcode)
      encodeMemArg(writer, instr.align, instr.offset, spaceIndex(context, instr.memory, 'Memory'))
      return
    }
  }
  instr satisfies never
}

const encodeExpr = (
  context: Context,
  writer: ByteWriter,
  expr: ReadonlyArray<Instr.Instr>,
): void => {
  for (const instr of expr) encodeInstr(context, writer, instr)
  writer.byte(InstructionTable.opcodes.end)
}

const encodeLimits = (
  writer: ByteWriter,
  limits: ModuleState.Limits,
  shared: boolean,
  addressType: ModuleState.AddressType,
): void => {
  const flags =
    (limits.max === undefined ? 0 : 0x01) | (shared ? 0x02 : 0) | (addressType === 'i64' ? 0x04 : 0)
  writer.byte(flags).u64(limits.min)
  if (limits.max !== undefined) writer.u64(limits.max)
}

const section = (module: ByteWriter, id: number, contents: ByteWriter): void => {
  module.byte(id).sized(contents)
}

const vectorSection = (
  module: ByteWriter,
  id: number,
  count: number,
  write: (contents: ByteWriter) => void,
): void => {
  if (count === 0) return
  const contents = new ByteWriter()
  contents.u32(count)
  write(contents)
  section(module, id, contents)
}

const encodeCompressedLocals = (
  writer: ByteWriter,
  locals: ReadonlyArray<ModuleState.LocalDeclaration>,
): void => {
  const runs: Array<{ type: ValType.ValType; count: number }> = []
  for (const local of locals) {
    const last = runs.at(-1)
    if (last !== undefined && ValType.equals(last.type, local.type)) {
      last.count += 1
    } else {
      runs.push({ type: local.type, count: 1 })
    }
  }
  writer.u32(runs.length)
  for (const run of runs) {
    writer.u32(run.count).byte(ValType.binary(run.type))
  }
}

const encodeNameSection = (context: Context, module: ByteWriter): void => {
  const { snapshot, spaces } = context
  const contents = new ByteWriter()
  contents.name('name')
  let hasContent = false
  if (snapshot.moduleName !== undefined) {
    const subsection = new ByteWriter()
    subsection.name(snapshot.moduleName)
    contents.byte(0).sized(subsection)
    hasContent = true
  }
  const funcNames: Array<{ index: number; name: string }> = []
  for (const [entryIndex, entry] of snapshot.funcs.entries()) {
    const index = spaces.funcs[entryIndex]
    if (entry.name !== undefined && index !== undefined) {
      funcNames.push({ index, name: entry.name })
    }
  }
  funcNames.sort((left, right) => left.index - right.index)
  if (funcNames.length > 0) {
    const subsection = new ByteWriter()
    subsection.u32(funcNames.length)
    for (const entry of funcNames) subsection.u32(entry.index).name(entry.name)
    contents.byte(1).sized(subsection)
    hasContent = true
  }
  const namemap = (
    id: number,
    entries: ReadonlyArray<{ readonly name: string | undefined }>,
    emitted: ReadonlyArray<number>,
  ): void => {
    const names: Array<{ index: number; name: string }> = []
    for (const [entryIndex, entry] of entries.entries()) {
      const index = emitted[entryIndex]
      if (entry.name !== undefined && index !== undefined) {
        names.push({ index, name: entry.name })
      }
    }
    if (names.length === 0) return
    names.sort((left, right) => left.index - right.index)
    const subsection = new ByteWriter()
    subsection.u32(names.length)
    for (const entry of names) subsection.u32(entry.index).name(entry.name)
    contents.byte(id).sized(subsection)
    hasContent = true
  }

  const localNames: Array<{
    index: number
    locals: Array<{ index: number; name: string }>
  }> = []
  for (const [entryIndex, entry] of snapshot.funcs.entries()) {
    const funcIndex = spaces.funcs[entryIndex]
    const funcType = snapshot.types[entry.typeIndex]
    if (entry.definition === undefined || funcIndex === undefined || funcType === undefined) {
      continue
    }
    const named: Array<{ index: number; name: string }> = []
    for (const [localIndex, local] of entry.definition.locals.entries()) {
      if (local.name !== undefined) {
        named.push({ index: funcType.params.length + localIndex, name: local.name })
      }
    }
    if (named.length > 0) localNames.push({ index: funcIndex, locals: named })
  }
  localNames.sort((left, right) => left.index - right.index)
  if (localNames.length > 0) {
    const subsection = new ByteWriter()
    subsection.u32(localNames.length)
    for (const entry of localNames) {
      subsection.u32(entry.index).u32(entry.locals.length)
      for (const local of entry.locals) subsection.u32(local.index).name(local.name)
    }
    contents.byte(2).sized(subsection)
    hasContent = true
  }
  const identity = (entries: ReadonlyArray<unknown>): ReadonlyArray<number> =>
    entries.map((_, index) => index)
  namemap(5, snapshot.tables, spaces.tables)
  namemap(6, snapshot.memories, spaces.memories)
  namemap(7, snapshot.globals, spaces.globals)
  namemap(8, snapshot.elems, identity(snapshot.elems))
  namemap(9, snapshot.datas, identity(snapshot.datas))
  if (hasContent) section(module, 0, contents)
}

/**
 * Encodes validated module state as a complete binary module.
 *
 * @internal
 */
export const encodeModule = (
  snapshot: ModuleState.Snapshot,
  spaces: IndexSpace.Spaces,
  operation: string,
): Result.Result<Uint8Array, WasmError> => {
  const context: Context = { snapshot, spaces, operation }
  try {
    const module = new ByteWriter()
    module.raw([0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00])

    vectorSection(module, 1, snapshot.types.length, (contents) => {
      for (const funcType of snapshot.types) {
        contents.byte(0x60).u32(funcType.params.length)
        for (const param of funcType.params) contents.byte(ValType.binary(param))
        contents.u32(funcType.results.length)
        for (const result of funcType.results) contents.byte(ValType.binary(result))
      }
    })

    const imports: Array<(contents: ByteWriter) => void> = []
    for (const entryIndex of spaces.funcOrder) {
      const entry = snapshot.funcs[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      imports.push((contents) => {
        contents.name(source.module).name(source.field).byte(0x00).u32(entry.typeIndex)
      })
    }
    for (const entryIndex of spaces.tableOrder) {
      const entry = snapshot.tables[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      imports.push((contents) => {
        contents.name(source.module).name(source.field).byte(0x01)
        contents.byte(refTypeByte(entry.refType))
        encodeLimits(contents, entry.limits, false, entry.addressType)
      })
    }
    for (const entryIndex of spaces.memoryOrder) {
      const entry = snapshot.memories[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      imports.push((contents) => {
        contents.name(source.module).name(source.field).byte(0x02)
        encodeLimits(contents, entry.limits, entry.shared, entry.addressType)
      })
    }
    for (const entryIndex of spaces.globalOrder) {
      const entry = snapshot.globals[entryIndex]
      const source = entry?.importSource
      if (entry === undefined || source === undefined) continue
      imports.push((contents) => {
        contents.name(source.module).name(source.field).byte(0x03)
        contents.byte(ValType.binary(entry.valType)).byte(entry.mutable ? 0x01 : 0x00)
      })
    }
    vectorSection(module, 2, imports.length, (contents) => {
      for (const write of imports) write(contents)
    })

    const definedFuncs = spaces.funcOrder.filter(
      (entryIndex) => snapshot.funcs[entryIndex]?.importSource === undefined,
    )
    vectorSection(module, 3, definedFuncs.length, (contents) => {
      for (const entryIndex of definedFuncs) {
        const entry = snapshot.funcs[entryIndex]
        if (entry !== undefined) contents.u32(entry.typeIndex)
      }
    })

    const definedTables = spaces.tableOrder.filter(
      (entryIndex) => snapshot.tables[entryIndex]?.importSource === undefined,
    )
    vectorSection(module, 4, definedTables.length, (contents) => {
      for (const entryIndex of definedTables) {
        const entry = snapshot.tables[entryIndex]
        if (entry === undefined) continue
        contents.byte(refTypeByte(entry.refType))
        encodeLimits(contents, entry.limits, false, entry.addressType)
      }
    })

    const definedMemories = spaces.memoryOrder.filter(
      (entryIndex) => snapshot.memories[entryIndex]?.importSource === undefined,
    )
    vectorSection(module, 5, definedMemories.length, (contents) => {
      for (const entryIndex of definedMemories) {
        const entry = snapshot.memories[entryIndex]
        if (entry !== undefined)
          encodeLimits(contents, entry.limits, entry.shared, entry.addressType)
      }
    })

    const definedGlobals = spaces.globalOrder.filter(
      (entryIndex) => snapshot.globals[entryIndex]?.importSource === undefined,
    )
    vectorSection(module, 6, definedGlobals.length, (contents) => {
      for (const entryIndex of definedGlobals) {
        const entry = snapshot.globals[entryIndex]
        if (entry?.init === undefined) continue
        contents.byte(ValType.binary(entry.valType)).byte(entry.mutable ? 0x01 : 0x00)
        encodeExpr(context, contents, entry.init)
      }
    })

    vectorSection(module, 7, snapshot.exports.length, (contents) => {
      for (const moduleExport of snapshot.exports) {
        contents.name(moduleExport.name)
        switch (moduleExport.kind) {
          case 'func':
            contents.byte(0x00).u32(spaces.funcs[moduleExport.entryIndex] ?? 0)
            break
          case 'table':
            contents.byte(0x01).u32(spaces.tables[moduleExport.entryIndex] ?? 0)
            break
          case 'memory':
            contents.byte(0x02).u32(spaces.memories[moduleExport.entryIndex] ?? 0)
            break
          case 'global':
            contents.byte(0x03).u32(spaces.globals[moduleExport.entryIndex] ?? 0)
            break
        }
      }
    })

    if (snapshot.start !== undefined) {
      const contents = new ByteWriter()
      contents.u32(spaces.funcs[snapshot.start] ?? 0)
      section(module, 8, contents)
    }

    vectorSection(module, 9, snapshot.elems.length, (contents) => {
      for (const elem of snapshot.elems) {
        switch (elem.mode._tag) {
          case 'Active': {
            const tableIndex = spaces.tables[elem.mode.table] ?? 0
            const funcrefTableZero = tableIndex === 0 && elem.refType._tag === 'FuncRef'
            if (funcrefTableZero) {
              contents.u32(4)
              encodeExpr(context, contents, elem.mode.offset)
            } else {
              contents.u32(6).u32(tableIndex)
              encodeExpr(context, contents, elem.mode.offset)
              contents.byte(refTypeByte(elem.refType))
            }
            break
          }
          case 'Passive':
            contents.u32(5).byte(refTypeByte(elem.refType))
            break
          case 'Declarative':
            contents.u32(7).byte(refTypeByte(elem.refType))
            break
        }
        contents.u32(elem.items.length)
        for (const item of elem.items) encodeExpr(context, contents, item)
      }
    })

    const needsDataCount = snapshot.funcs.some((entry) => entry.definition?.usesDataCount === true)
    if (needsDataCount) {
      const contents = new ByteWriter()
      contents.u32(snapshot.datas.length)
      section(module, 12, contents)
    }

    const codeEntries = definedFuncs.flatMap((entryIndex) => {
      const definition = snapshot.funcs[entryIndex]?.definition
      return definition === undefined ? [] : [definition]
    })
    vectorSection(module, 10, codeEntries.length, (contents) => {
      for (const definition of codeEntries) {
        const body = new ByteWriter()
        encodeCompressedLocals(body, definition.locals)
        encodeExpr(context, body, definition.body)
        contents.sized(body)
      }
    })

    vectorSection(module, 11, snapshot.datas.length, (contents) => {
      for (const data of snapshot.datas) {
        if (data.mode._tag === 'Passive') {
          contents.u32(1)
        } else {
          const memoryIndex = spaces.memories[data.mode.memory] ?? 0
          if (memoryIndex === 0) {
            contents.u32(0)
          } else {
            contents.u32(2).u32(memoryIndex)
          }
          encodeExpr(context, contents, data.mode.offset)
        }
        contents.u32(data.bytes.length).raw(data.bytes)
      }
    })

    encodeNameSection(context, module)

    return Result.succeed(module.toUint8Array())
  } catch (failure) {
    if (failure instanceof Abort) return Result.fail(failure.error)
    throw failure
  }
}
