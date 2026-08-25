/**
 * Function body validation following the specification's validation algorithm: one value stack,
 * one control-frame stack, and per-frame unreachable mode for polymorphic typing after
 * stack-polymorphic instructions.
 *
 * The walker is synchronous and aborts with a private non-yieldable failure that the
 * {@link checkBody} boundary translates into a typed `Result` failure once.
 *
 * @internal
 */
import * as Result from 'effect/Result'
import type * as Instr from '../Instr.js'
import * as ValType from '../ValType.js'
import { validationFailed, type WasmError } from '../WasmError.js'
import * as Handle from './Handle.js'
import * as InstructionTable from './InstructionTable.js'
import * as ModuleState from './ModuleState.js'
import type * as OwnedHandle from './OwnedHandle.js'
import * as Subtype from './Subtype.js'

/** The bottom type produced while a control frame is unreachable. */
type StackType = ValType.ValType | 'unknown'

interface Frame {
  readonly opcode: 'func' | 'block' | 'loop' | 'if' | 'else'
  readonly startTypes: ReadonlyArray<ValType.ValType>
  readonly endTypes: ReadonlyArray<ValType.ValType>
  readonly height: number
  unreachable: boolean
}

class Abort {
  constructor(
    readonly error: WasmError | undefined,
    readonly message: string,
  ) {}
}

interface Context {
  readonly state: ModuleState.MutableState
  readonly owner: OwnedHandle.Owner
  readonly operation: string
  readonly localTypes: ReadonlyArray<ValType.ValType>
  readonly returnTypes: ReadonlyArray<ValType.ValType>
  readonly valStack: Array<StackType>
  readonly ctrlStack: Array<Frame>
  readonly refFuncs: Set<number>
  usesDataCount: boolean
}

const abort = (message: string): never => {
  throw new Abort(undefined, message)
}

const orAbort = <A>(result: Result.Result<A, WasmError>): A => {
  if (Result.isFailure(result)) {
    throw new Abort(result.failure, result.failure.message)
  }
  return Result.getOrThrow(result)
}

const typeText = (type: StackType): string => (type === 'unknown' ? 'unknown' : ValType.text(type))

const pushVal = (context: Context, type: StackType): void => {
  context.valStack.push(type)
}

const pushVals = (context: Context, types: ReadonlyArray<ValType.ValType>): void => {
  for (const type of types) pushVal(context, type)
}

const topFrame = (context: Context): Frame => {
  const frame = context.ctrlStack.at(-1)
  return frame ?? abort('The control frame stack is empty')
}

const popVal = (context: Context, expect?: ValType.ValType): StackType => {
  const frame = topFrame(context)
  if (context.valStack.length === frame.height) {
    if (frame.unreachable) return expect ?? 'unknown'
    return abort(
      expect === undefined
        ? 'The value stack is empty'
        : `The value stack is empty but ${ValType.text(expect)} was expected`,
    )
  }
  const actual = context.valStack.pop()
  if (actual === undefined) return abort('The value stack is empty')
  if (
    expect !== undefined &&
    actual !== 'unknown' &&
    !Subtype.matches(context.state, actual, expect)
  ) {
    return abort(`Expected ${ValType.text(expect)} on the stack but found ${typeText(actual)}`)
  }
  return actual
}

const popVals = (context: Context, types: ReadonlyArray<ValType.ValType>): void => {
  for (let index = types.length - 1; index >= 0; index -= 1) {
    const type = types[index]
    if (type !== undefined) popVal(context, type)
  }
}

const pushFrame = (
  context: Context,
  opcode: Frame['opcode'],
  startTypes: ReadonlyArray<ValType.ValType>,
  endTypes: ReadonlyArray<ValType.ValType>,
): void => {
  context.ctrlStack.push({
    opcode,
    startTypes,
    endTypes,
    height: context.valStack.length,
    unreachable: false,
  })
  pushVals(context, startTypes)
}

const popFrame = (context: Context): Frame => {
  const frame = topFrame(context)
  popVals(context, frame.endTypes)
  if (context.valStack.length !== frame.height) {
    return abort('A block left extra values on the stack')
  }
  context.ctrlStack.pop()
  return frame
}

const markUnreachable = (context: Context): void => {
  const frame = topFrame(context)
  context.valStack.length = frame.height
  frame.unreachable = true
}

const labelTypes = (frame: Frame): ReadonlyArray<ValType.ValType> =>
  frame.opcode === 'loop' ? frame.startTypes : frame.endTypes

const frameAt = (context: Context, depth: number): Frame => {
  if (!Number.isInteger(depth) || depth < 0 || depth >= context.ctrlStack.length) {
    return abort(`Branch depth ${depth} exceeds the enclosing block depth`)
  }
  const frame = context.ctrlStack[context.ctrlStack.length - 1 - depth]
  return frame ?? abort('The control frame stack is empty')
}

const blockTypes = (
  context: Context,
  blockType: Instr.BlockType,
): {
  readonly startTypes: ReadonlyArray<ValType.ValType>
  readonly endTypes: ReadonlyArray<ValType.ValType>
} => {
  switch (blockType._tag) {
    case 'Empty':
      return { startTypes: [], endTypes: [] }
    case 'Value':
      return { startTypes: [], endTypes: [blockType.type] }
    case 'Func': {
      const index = orAbort(
        Handle.resolve(context.owner, blockType.type, 'Type', context.operation),
      )
      const funcType = ModuleState.funcTypeAt(context.state.types, index)
      if (funcType === undefined) return abort('The block type must be a function type')
      return { startTypes: funcType.params, endTypes: funcType.results }
    }
  }
}

const funcTypeOf = (context: Context, index: number): ModuleState.FuncType => {
  const entry = context.state.funcs[index]
  const funcType =
    entry === undefined ? undefined : ModuleState.funcTypeAt(context.state.types, entry.typeIndex)
  return funcType ?? abort('Function table entry is missing')
}

const localType = (context: Context, index: number): ValType.ValType => {
  const type = Number.isInteger(index) && index >= 0 ? context.localTypes[index] : undefined
  return type ?? abort(`Local index ${index} is out of range`)
}

const globalEntry = (context: Context, handle: Parameters<typeof Handle.resolve>[1]) => {
  const index = orAbort(Handle.resolve(context.owner, handle, 'Global', context.operation))
  const entry = context.state.globals[index]
  return entry ?? abort('Global table entry is missing')
}

const tableEntry = (context: Context, handle: Parameters<typeof Handle.resolve>[1]) => {
  const index = orAbort(Handle.resolve(context.owner, handle, 'Table', context.operation))
  const entry = context.state.tables[index]
  return entry ?? abort('Table entry is missing')
}

const memoryEntry = (context: Context, handle: Parameters<typeof Handle.resolve>[1]) => {
  const index = orAbort(Handle.resolve(context.owner, handle, 'Memory', context.operation))
  const entry = context.state.memories[index]
  return entry ?? abort('Memory entry is missing')
}

const addressValType = (entry: {
  readonly addressType: ModuleState.AddressType
}): ValType.ValType => (entry.addressType === 'i64' ? ValType.i64 : ValType.i32)

const checkOffset = (
  offset: number | bigint,
  addressType: ModuleState.AddressType,
  mnemonic: string,
): undefined => {
  let value: bigint | undefined
  if (typeof offset === 'bigint') value = offset
  else if (Number.isInteger(offset)) value = BigInt(offset)
  const bound = addressType === 'i64' ? 2n ** 64n - 1n : 0xffffffffn
  if (value === undefined || value < 0n || value > bound) {
    abort(
      `The offset of ${mnemonic} must be an unsigned ${addressType === 'i64' ? 64 : 32}-bit integer`,
    )
  }
}

const tagEntry = (context: Context, handle: Parameters<typeof Handle.resolve>[1]) => {
  const index = orAbort(Handle.resolve(context.owner, handle, 'Tag', context.operation))
  const entry = context.state.tags[index]
  return entry ?? abort('Tag entry is missing')
}

const tagParams = (
  context: Context,
  entry: ModuleState.TagEntry,
): ReadonlyArray<ValType.ValType> => {
  const funcType = ModuleState.funcTypeAt(context.state.types, entry.typeIndex)
  return funcType?.params ?? abort('Tag type entry is missing')
}

const elemEntry = (context: Context, handle: Parameters<typeof Handle.resolve>[1]) => {
  const index = orAbort(Handle.resolve(context.owner, handle, 'Elem', context.operation))
  const entry = context.state.elems[index]
  return entry ?? abort('Element segment entry is missing')
}

const dataIndex = (context: Context, handle: Parameters<typeof Handle.resolve>[1]): number => {
  const index = orAbort(Handle.resolve(context.owner, handle, 'Data', context.operation))
  if (context.state.datas[index] === undefined) return abort('Data segment entry is missing')
  return index
}

const typeIndexOf = (context: Context, handle: Parameters<typeof Handle.resolve>[1]): number =>
  orAbort(Handle.resolve(context.owner, handle, 'Type', context.operation))

const typeHandleAt = (context: Context, index: number) =>
  context.state.typeHandles[index] ?? abort('Type table handle is missing')

const structAt = (context: Context, index: number) => {
  const composite = context.state.types[index]?.composite
  return composite?._tag === 'Struct' ? composite : abort('The type must be a struct type')
}

const arrayAt = (context: Context, index: number) => {
  const composite = context.state.types[index]?.composite
  return composite?._tag === 'Array' ? composite : abort('The type must be an array type')
}

const isPacked = (
  storage: ModuleState.StorageType,
): storage is { _tag: 'Packed'; width: 'i8' | 'i16' } =>
  '_tag' in storage && storage._tag === 'Packed'

const unpacked = (storage: ModuleState.StorageType): ValType.ValType =>
  isPacked(storage) ? ValType.i32 : (storage as ValType.ValType)

const storageDefaultable = (storage: ModuleState.StorageType): boolean =>
  isPacked(storage) || ValType.isDefaultable(storage as ValType.ValType)

const checkAccessSign = (
  storage: ModuleState.StorageType,
  sign: 's' | 'u' | undefined,
  mnemonic: string,
): undefined => {
  if (isPacked(storage) && sign === undefined) {
    abort(`Packed fields require ${mnemonic}_s or ${mnemonic}_u`)
  }
  if (!isPacked(storage) && sign !== undefined) {
    abort(`Only packed fields allow ${mnemonic}_s and ${mnemonic}_u`)
  }
}

const concreteRef = (context: Context, index: number, nullable: boolean): ValType.RefType =>
  nullable
    ? ValType.refNull(typeHandleAt(context, index))
    : ValType.ref(typeHandleAt(context, index))

const hierarchyTop = (context: Context, refType: ValType.RefType): ValType.AbstractHeapKind => {
  if (refType.heapType._tag === 'Abstract') {
    const kind = refType.heapType.kind
    if (kind === 'func' || kind === 'nofunc') return 'func'
    if (kind === 'extern' || kind === 'noextern') return 'extern'
    if (kind === 'exn' || kind === 'noexn') return 'exn'
    return 'any'
  }
  const index = orAbort(
    Handle.resolve(context.owner, refType.heapType.type, 'Type', context.operation),
  )
  const composite = context.state.types[index]?.composite
  if (composite === undefined) return abort('Type table entry is missing')
  return composite._tag === 'Func' ? 'func' : 'any'
}

/** Statically requires the popped operand (when known) to share the immediate's hierarchy. */
const popCastOperandStaticCheck = (
  context: Context,
  immediate: ValType.RefType,
  operand: StackType,
): void => {
  if (operand === 'unknown' || !ValType.isRefType(operand)) return
  if (hierarchyTop(context, immediate) !== hierarchyTop(context, operand)) {
    abort('The cast target must share the operand reference hierarchy')
  }
}

/** The static difference rt1 \ rt2 used by br_on_cast typing. */
const refDifference = (from: ValType.RefType, to: ValType.RefType): ValType.RefType => ({
  _tag: 'Ref',
  nullable: from.nullable && !to.nullable,
  heapType: from.heapType,
})

const checkSequence = (context: Context, body: ReadonlyArray<Instr.Instr>): void => {
  for (const instr of body) checkInstr(context, instr)
}

const checkBlock = (
  context: Context,
  opcode: 'block' | 'loop',
  blockType: Instr.BlockType,
  body: ReadonlyArray<Instr.Instr>,
): void => {
  const { startTypes, endTypes } = blockTypes(context, blockType)
  popVals(context, startTypes)
  pushFrame(context, opcode, startTypes, endTypes)
  checkSequence(context, body)
  popFrame(context)
  pushVals(context, endTypes)
}

const checkInstr = (context: Context, instr: Instr.Instr): undefined => {
  switch (instr._tag) {
    case 'Op': {
      const row = InstructionTable.plainOps[instr.mnemonic]
      if (row.typing._tag === 'Uniform') {
        popVals(context, row.typing.params)
        pushVals(context, row.typing.results)
        return
      }
      switch (instr.mnemonic) {
        case 'unreachable':
          markUnreachable(context)
          return
        case 'return':
          popVals(context, context.returnTypes)
          markUnreachable(context)
          return
        case 'drop':
          popVal(context)
          return
        case 'select': {
          popVal(context, ValType.i32)
          const first = popVal(context)
          const second = popVal(context)
          const known = first === 'unknown' ? second : first
          if (known !== 'unknown' && ValType.isRefType(known)) {
            return abort('The untyped select requires numeric or vector operands')
          }
          if (first !== 'unknown' && second !== 'unknown' && !ValType.equals(first, second)) {
            return abort(
              `The select operands must agree but found ${typeText(first)} and ${typeText(second)}`,
            )
          }
          pushVal(context, known)
          return
        }
        case 'ref.is_null': {
          const operand = popVal(context)
          if (operand !== 'unknown' && !ValType.isRefType(operand)) {
            return abort('ref.is_null requires a reference operand')
          }
          pushVal(context, ValType.i32)
          return
        }
        case 'throw_ref':
          popVal(context, ValType.exnref)
          markUnreachable(context)
          return
        case 'ref.as_non_null': {
          const operand = popVal(context)
          if (operand !== 'unknown' && !ValType.isRefType(operand)) {
            return abort('ref.as_non_null requires a reference operand')
          }
          pushVal(
            context,
            operand === 'unknown' || !ValType.isRefType(operand)
              ? 'unknown'
              : ValType.ref(operand.heapType),
          )
          return
        }
        default:
          return abort(`The instruction ${instr.mnemonic} has no typing rule`)
      }
    }
    case 'I32Const':
      if (!Number.isInteger(instr.value) || instr.value < -0x80000000 || instr.value > 0xffffffff) {
        return abort('The i32.const immediate must be a 32-bit integer')
      }
      pushVal(context, ValType.i32)
      return
    case 'I64Const':
      if (instr.value < -(2n ** 63n) || instr.value >= 2n ** 64n) {
        return abort('The i64.const immediate must be a 64-bit integer')
      }
      pushVal(context, ValType.i64)
      return
    case 'F32Const':
      pushVal(context, ValType.f32)
      return
    case 'F64Const':
      pushVal(context, ValType.f64)
      return
    case 'LocalGet':
      pushVal(context, localType(context, instr.local))
      return
    case 'LocalSet':
      popVal(context, localType(context, instr.local))
      return
    case 'LocalTee': {
      const type = localType(context, instr.local)
      popVal(context, type)
      pushVal(context, type)
      return
    }
    case 'GlobalGet':
      pushVal(context, globalEntry(context, instr.global).valType)
      return
    case 'GlobalSet': {
      const entry = globalEntry(context, instr.global)
      if (!entry.mutable) return abort('global.set requires a mutable global')
      popVal(context, entry.valType)
      return
    }
    case 'Call': {
      const index = orAbort(Handle.resolve(context.owner, instr.func, 'Func', context.operation))
      const funcType = funcTypeOf(context, index)
      popVals(context, funcType.params)
      pushVals(context, funcType.results)
      return
    }
    case 'CallIndirect': {
      const table = tableEntry(context, instr.table)
      if (!Subtype.matches(context.state, table.refType, ValType.funcref)) {
        return abort('call_indirect requires a funcref table')
      }
      const typeIndex = orAbort(
        Handle.resolve(context.owner, instr.type, 'Type', context.operation),
      )
      const funcType =
        ModuleState.funcTypeAt(context.state.types, typeIndex) ??
        abort('call_indirect requires a function type')
      popVal(context, ValType.i32)
      popVals(context, funcType.params)
      pushVals(context, funcType.results)
      return
    }
    case 'ReturnCall': {
      const index = orAbort(Handle.resolve(context.owner, instr.func, 'Func', context.operation))
      const funcType = funcTypeOf(context, index)
      checkTailResults(context, funcType.results)
      popVals(context, funcType.params)
      markUnreachable(context)
      return
    }
    case 'ReturnCallIndirect': {
      const table = tableEntry(context, instr.table)
      if (!Subtype.matches(context.state, table.refType, ValType.funcref)) {
        return abort('return_call_indirect requires a funcref table')
      }
      const typeIndex = orAbort(
        Handle.resolve(context.owner, instr.type, 'Type', context.operation),
      )
      const funcType =
        ModuleState.funcTypeAt(context.state.types, typeIndex) ??
        abort('return_call_indirect requires a function type')
      checkTailResults(context, funcType.results)
      popVal(context, ValType.i32)
      popVals(context, funcType.params)
      markUnreachable(context)
      return
    }
    case 'Block':
      checkBlock(context, 'block', instr.blockType, instr.body)
      return
    case 'Loop':
      checkBlock(context, 'loop', instr.blockType, instr.body)
      return
    case 'If': {
      const { startTypes, endTypes } = blockTypes(context, instr.blockType)
      popVal(context, ValType.i32)
      popVals(context, startTypes)
      pushFrame(context, 'if', startTypes, endTypes)
      checkSequence(context, instr.thenBody)
      popFrame(context)
      pushFrame(context, 'else', startTypes, endTypes)
      checkSequence(context, instr.elseBody)
      popFrame(context)
      pushVals(context, endTypes)
      return
    }
    case 'Br': {
      popVals(context, labelTypes(frameAt(context, instr.depth)))
      markUnreachable(context)
      return
    }
    case 'BrIf': {
      const types = labelTypes(frameAt(context, instr.depth))
      popVal(context, ValType.i32)
      popVals(context, types)
      pushVals(context, types)
      return
    }
    case 'BrTable': {
      popVal(context, ValType.i32)
      const defaultTypes = labelTypes(frameAt(context, instr.defaultDepth))
      for (const depth of instr.depths) {
        const types = labelTypes(frameAt(context, depth))
        if (types.length !== defaultTypes.length) {
          return abort('br_table labels must agree on arity')
        }
        popVals(context, types)
        pushVals(context, types)
      }
      popVals(context, defaultTypes)
      markUnreachable(context)
      return
    }
    case 'SelectTyped': {
      const type = instr.types.length === 1 ? instr.types[0] : undefined
      if (type === undefined) {
        return abort('The typed select requires exactly one result type')
      }
      popVal(context, ValType.i32)
      popVal(context, type)
      popVal(context, type)
      pushVal(context, type)
      return
    }
    case 'RefNull':
      pushVal(context, ValType.refNull(instr.heapType))
      return
    case 'RefFunc': {
      const index = orAbort(Handle.resolve(context.owner, instr.func, 'Func', context.operation))
      const entry = context.state.funcs[index] ?? abort('Function table entry is missing')
      context.refFuncs.add(index)
      pushVal(context, ValType.ref(typeHandleAt(context, entry.typeIndex)))
      return
    }
    case 'MemoryAccess': {
      const row = InstructionTable.memoryAccessOps[instr.mnemonic]
      const memory = memoryEntry(context, instr.memory)
      if (!Number.isInteger(instr.align) || instr.align < 0 || instr.align > row.widthLog2) {
        return abort(`The alignment of ${instr.mnemonic} must be between 0 and ${row.widthLog2}`)
      }
      checkOffset(instr.offset, memory.addressType, instr.mnemonic)
      const address = addressValType(memory)
      if (row.kind === 'load') {
        popVal(context, address)
        pushVal(context, row.valType)
      } else {
        popVal(context, row.valType)
        popVal(context, address)
      }
      return
    }
    case 'MemorySize': {
      pushVal(context, addressValType(memoryEntry(context, instr.memory)))
      return
    }
    case 'MemoryGrow': {
      const address = addressValType(memoryEntry(context, instr.memory))
      popVal(context, address)
      pushVal(context, address)
      return
    }
    case 'MemoryInit': {
      context.usesDataCount = true
      dataIndex(context, instr.data)
      const address = addressValType(memoryEntry(context, instr.memory))
      popVals(context, [address, ValType.i32, ValType.i32])
      return
    }
    case 'DataDrop':
      context.usesDataCount = true
      dataIndex(context, instr.data)
      return
    case 'MemoryCopy': {
      const destination = memoryEntry(context, instr.destination)
      const source = memoryEntry(context, instr.source)
      const count =
        destination.addressType === 'i64' && source.addressType === 'i64'
          ? ValType.i64
          : ValType.i32
      popVals(context, [addressValType(destination), addressValType(source), count])
      return
    }
    case 'MemoryFill': {
      const address = addressValType(memoryEntry(context, instr.memory))
      popVals(context, [address, ValType.i32, address])
      return
    }
    case 'TableGet': {
      const table = tableEntry(context, instr.table)
      popVal(context, addressValType(table))
      pushVal(context, table.refType)
      return
    }
    case 'TableSet': {
      const table = tableEntry(context, instr.table)
      popVal(context, table.refType)
      popVal(context, addressValType(table))
      return
    }
    case 'TableSize':
      pushVal(context, addressValType(tableEntry(context, instr.table)))
      return
    case 'TableGrow': {
      const table = tableEntry(context, instr.table)
      const address = addressValType(table)
      popVal(context, address)
      popVal(context, table.refType)
      pushVal(context, address)
      return
    }
    case 'TableFill': {
      const table = tableEntry(context, instr.table)
      const address = addressValType(table)
      popVal(context, address)
      popVal(context, table.refType)
      popVal(context, address)
      return
    }
    case 'TableCopy': {
      const destination = tableEntry(context, instr.destination)
      const source = tableEntry(context, instr.source)
      if (!ValType.equals(destination.refType, source.refType)) {
        return abort('table.copy requires tables with the same reference type')
      }
      const count =
        destination.addressType === 'i64' && source.addressType === 'i64'
          ? ValType.i64
          : ValType.i32
      popVals(context, [addressValType(destination), addressValType(source), count])
      return
    }
    case 'TableInit': {
      const elem = elemEntry(context, instr.elem)
      const table = tableEntry(context, instr.table)
      if (!ValType.equals(elem.refType, table.refType)) {
        return abort('table.init requires a segment matching the table reference type')
      }
      popVals(context, [addressValType(table), ValType.i32, ValType.i32])
      return
    }
    case 'ElemDrop':
      elemEntry(context, instr.elem)
      return
    case 'Throw': {
      const entry = tagEntry(context, instr.tag)
      popVals(context, tagParams(context, entry))
      markUnreachable(context)
      return
    }
    case 'TryTable': {
      const { startTypes, endTypes } = blockTypes(context, instr.blockType)
      // Catch labels are resolved as seen from the try_table instruction itself,
      // before its own frame is pushed.
      for (const clause of instr.catches) {
        const target = labelTypes(frameAt(context, clause.depth))
        const delivered: Array<ValType.ValType> = []
        if (clause._tag === 'Catch' || clause._tag === 'CatchRef') {
          delivered.push(...tagParams(context, tagEntry(context, clause.tag)))
        }
        if (clause._tag === 'CatchRef' || clause._tag === 'CatchAllRef') {
          delivered.push(ValType.exnref)
        }
        const matches =
          target.length === delivered.length &&
          target.every((type, position) => {
            const deliveredType = delivered[position]
            return deliveredType !== undefined && ValType.equals(type, deliveredType)
          })
        if (!matches) {
          return abort(
            `The ${clause._tag} clause requires a label accepting [${delivered
              .map(ValType.text)
              .join(', ')}]`,
          )
        }
      }
      popVals(context, startTypes)
      pushFrame(context, 'block', startTypes, endTypes)
      checkSequence(context, instr.body)
      popFrame(context)
      pushVals(context, endTypes)
      return
    }
    case 'StructNew': {
      const index = typeIndexOf(context, instr.type)
      const composite = structAt(context, index)
      if (instr.defaults) {
        for (const field of composite.fields) {
          if (!storageDefaultable(field.storage)) {
            return abort('struct.new_default requires all fields to be defaultable')
          }
        }
      } else {
        for (let position = composite.fields.length - 1; position >= 0; position -= 1) {
          const field = composite.fields[position]
          if (field !== undefined) popVal(context, unpacked(field.storage))
        }
      }
      pushVal(context, concreteRef(context, index, false))
      return
    }
    case 'StructGet': {
      const index = typeIndexOf(context, instr.type)
      const composite = structAt(context, index)
      const field = composite.fields[instr.field] ?? abort('Struct field index is out of range')
      checkAccessSign(field.storage, instr.sign, 'struct.get')
      popVal(context, concreteRef(context, index, true))
      pushVal(context, unpacked(field.storage))
      return
    }
    case 'StructSet': {
      const index = typeIndexOf(context, instr.type)
      const composite = structAt(context, index)
      const field = composite.fields[instr.field] ?? abort('Struct field index is out of range')
      if (!field.mutable) return abort('struct.set requires a mutable field')
      popVal(context, unpacked(field.storage))
      popVal(context, concreteRef(context, index, true))
      return
    }
    case 'ArrayNew': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      popVal(context, ValType.i32)
      if (instr.defaults) {
        if (!storageDefaultable(composite.field.storage)) {
          return abort('array.new_default requires a defaultable element type')
        }
      } else {
        popVal(context, unpacked(composite.field.storage))
      }
      pushVal(context, concreteRef(context, index, false))
      return
    }
    case 'ArrayNewFixed': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      if (!Number.isInteger(instr.length) || instr.length < 0 || instr.length > 0xffffffff) {
        return abort('array.new_fixed requires an unsigned 32-bit length')
      }
      for (let position = 0; position < instr.length; position += 1) {
        popVal(context, unpacked(composite.field.storage))
      }
      pushVal(context, concreteRef(context, index, false))
      return
    }
    case 'ArrayNewData': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      if (
        !isPacked(composite.field.storage) &&
        ValType.isRefType(composite.field.storage as ValType.ValType)
      ) {
        return abort('array.new_data requires a numeric element type')
      }
      dataIndex(context, instr.data)
      context.usesDataCount = true
      popVals(context, [ValType.i32, ValType.i32])
      pushVal(context, concreteRef(context, index, false))
      return
    }
    case 'ArrayNewElem': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      const element = elemEntry(context, instr.elem)
      if (isPacked(composite.field.storage))
        return abort('array.new_elem requires a reference element type')
      if (
        !Subtype.matches(context.state, element.refType, composite.field.storage as ValType.ValType)
      ) {
        return abort('array.new_elem requires the segment type to match the element type')
      }
      popVals(context, [ValType.i32, ValType.i32])
      pushVal(context, concreteRef(context, index, false))
      return
    }
    case 'ArrayGet': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      checkAccessSign(composite.field.storage, instr.sign, 'array.get')
      popVal(context, ValType.i32)
      popVal(context, concreteRef(context, index, true))
      pushVal(context, unpacked(composite.field.storage))
      return
    }
    case 'ArraySet': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      if (!composite.field.mutable) return abort('array.set requires a mutable element')
      popVal(context, unpacked(composite.field.storage))
      popVal(context, ValType.i32)
      popVal(context, concreteRef(context, index, true))
      return
    }
    case 'ArrayFill': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      if (!composite.field.mutable) return abort('array.fill requires a mutable element')
      popVal(context, ValType.i32)
      popVal(context, unpacked(composite.field.storage))
      popVal(context, ValType.i32)
      popVal(context, concreteRef(context, index, true))
      return
    }
    case 'ArrayCopy': {
      const destinationIndex = typeIndexOf(context, instr.destination)
      const sourceIndex = typeIndexOf(context, instr.source)
      const destination = arrayAt(context, destinationIndex)
      const source = arrayAt(context, sourceIndex)
      if (!destination.field.mutable) return abort('array.copy requires a mutable destination')
      const storagesMatch = isPacked(source.field.storage)
        ? isPacked(destination.field.storage) &&
          source.field.storage.width === destination.field.storage.width
        : !isPacked(destination.field.storage) &&
          Subtype.matches(
            context.state,
            source.field.storage as ValType.ValType,
            destination.field.storage as ValType.ValType,
          )
      if (!storagesMatch) return abort('array.copy requires compatible element types')
      popVal(context, ValType.i32)
      popVal(context, ValType.i32)
      popVal(context, concreteRef(context, sourceIndex, true))
      popVal(context, ValType.i32)
      popVal(context, concreteRef(context, destinationIndex, true))
      return
    }
    case 'ArrayInitData': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      if (!composite.field.mutable) return abort('array.init_data requires a mutable element')
      if (
        !isPacked(composite.field.storage) &&
        ValType.isRefType(composite.field.storage as ValType.ValType)
      ) {
        return abort('array.init_data requires a numeric element type')
      }
      dataIndex(context, instr.data)
      context.usesDataCount = true
      popVals(context, [ValType.i32, ValType.i32])
      popVal(context, ValType.i32)
      popVal(context, concreteRef(context, index, true))
      return
    }
    case 'ArrayInitElem': {
      const index = typeIndexOf(context, instr.type)
      const composite = arrayAt(context, index)
      const element = elemEntry(context, instr.elem)
      if (!composite.field.mutable) return abort('array.init_elem requires a mutable element')
      if (isPacked(composite.field.storage))
        return abort('array.init_elem requires a reference element type')
      if (
        !Subtype.matches(context.state, element.refType, composite.field.storage as ValType.ValType)
      ) {
        return abort('array.init_elem requires the segment type to match the element type')
      }
      popVals(context, [ValType.i32, ValType.i32])
      popVal(context, ValType.i32)
      popVal(context, concreteRef(context, index, true))
      return
    }
    case 'RefTest': {
      const operand = popVal(context)
      if (operand !== 'unknown') {
        if (!ValType.isRefType(operand)) return abort('ref.test requires a reference operand')
        if (!Subtype.matches(context.state, instr.refType, operand)) {
          return abort('ref.test requires a target type below the operand type')
        }
      }
      popCastOperandStaticCheck(context, instr.refType, operand)
      pushVal(context, ValType.i32)
      return
    }
    case 'RefCast': {
      const operand = popVal(context)
      if (operand !== 'unknown') {
        if (!ValType.isRefType(operand)) return abort('ref.cast requires a reference operand')
        if (!Subtype.matches(context.state, instr.refType, operand)) {
          return abort('ref.cast requires a target type below the operand type')
        }
      }
      popCastOperandStaticCheck(context, instr.refType, operand)
      pushVal(context, instr.refType)
      return
    }
    case 'BrOnCast': {
      if (!Subtype.matches(context.state, instr.to, instr.from)) {
        return abort('br_on_cast requires the target type below the source type')
      }
      popVal(context, instr.from)
      const label = labelTypes(frameAt(context, instr.depth))
      const last = label.at(-1) ?? abort('The branch label must accept the cast reference')
      const sent = instr.fail ? refDifference(instr.from, instr.to) : instr.to
      if (!Subtype.matches(context.state, sent, last)) {
        return abort('The branch label must accept the cast reference')
      }
      const rest = label.slice(0, -1)
      popVals(context, rest)
      pushVals(context, rest)
      pushVal(context, instr.fail ? instr.to : refDifference(instr.from, instr.to))
      return
    }
    case 'BrOnNull': {
      const operand = popVal(context)
      if (operand !== 'unknown' && !ValType.isRefType(operand)) {
        return abort('br_on_null requires a reference operand')
      }
      const label = labelTypes(frameAt(context, instr.depth))
      popVals(context, label)
      pushVals(context, label)
      pushVal(
        context,
        operand === 'unknown' || !ValType.isRefType(operand)
          ? 'unknown'
          : ValType.ref(operand.heapType),
      )
      return
    }
    case 'BrOnNonNull': {
      const operand = popVal(context)
      if (operand !== 'unknown' && !ValType.isRefType(operand)) {
        return abort('br_on_non_null requires a reference operand')
      }
      const label = labelTypes(frameAt(context, instr.depth))
      const last = label.at(-1) ?? abort('The branch label must accept the non-null reference')
      if (operand !== 'unknown' && ValType.isRefType(operand)) {
        if (!Subtype.matches(context.state, ValType.ref(operand.heapType), last)) {
          return abort('The branch label must accept the non-null reference')
        }
      }
      const rest = label.slice(0, -1)
      popVals(context, rest)
      pushVals(context, rest)
      return
    }
    case 'CallRef': {
      const index = typeIndexOf(context, instr.type)
      const funcType =
        ModuleState.funcTypeAt(context.state.types, index) ??
        abort('call_ref requires a function type')
      popVal(context, concreteRef(context, index, true))
      popVals(context, funcType.params)
      pushVals(context, funcType.results)
      return
    }
    case 'ReturnCallRef': {
      const index = typeIndexOf(context, instr.type)
      const funcType =
        ModuleState.funcTypeAt(context.state.types, index) ??
        abort('return_call_ref requires a function type')
      checkTailResults(context, funcType.results)
      popVal(context, concreteRef(context, index, true))
      popVals(context, funcType.params)
      markUnreachable(context)
      return
    }
    case 'V128Const': {
      if (
        instr.bytes.length !== 16 ||
        instr.bytes.some((byte) => !Number.isInteger(byte) || byte < 0 || byte > 0xff)
      ) {
        return abort('v128.const requires exactly 16 bytes in 0..255')
      }
      pushVal(context, ValType.v128)
      return
    }
    case 'Shuffle': {
      if (
        instr.lanes.length !== 16 ||
        instr.lanes.some((lane) => !Number.isInteger(lane) || lane < 0 || lane > 31)
      ) {
        return abort('i8x16.shuffle requires exactly 16 lane selectors in 0..31')
      }
      popVal(context, ValType.v128)
      popVal(context, ValType.v128)
      pushVal(context, ValType.v128)
      return
    }
    case 'SimdLane': {
      const row = InstructionTable.simdLaneOps[instr.mnemonic]
      if (!Number.isInteger(instr.lane) || instr.lane < 0 || instr.lane >= row.laneCount) {
        return abort(`The lane index of ${instr.mnemonic} must be below ${row.laneCount}`)
      }
      if (row.kind === 'extract') {
        popVal(context, ValType.v128)
        pushVal(context, row.scalar)
      } else {
        popVal(context, row.scalar)
        popVal(context, ValType.v128)
        pushVal(context, ValType.v128)
      }
      return
    }
    case 'SimdMemoryAccess': {
      const row = InstructionTable.simdMemoryAccessOps[instr.mnemonic]
      const memory = memoryEntry(context, instr.memory)
      if (!Number.isInteger(instr.align) || instr.align < 0 || instr.align > row.widthLog2) {
        return abort(`The alignment of ${instr.mnemonic} must be between 0 and ${row.widthLog2}`)
      }
      checkOffset(instr.offset, memory.addressType, instr.mnemonic)
      const address = addressValType(memory)
      if (row.kind === 'load') {
        popVal(context, address)
        pushVal(context, ValType.v128)
      } else {
        popVal(context, ValType.v128)
        popVal(context, address)
      }
      return
    }
    case 'SimdMemoryLane': {
      const row = InstructionTable.simdLaneMemoryOps[instr.mnemonic]
      const memory = memoryEntry(context, instr.memory)
      if (!Number.isInteger(instr.align) || instr.align < 0 || instr.align > row.widthLog2) {
        return abort(`The alignment of ${instr.mnemonic} must be between 0 and ${row.widthLog2}`)
      }
      if (!Number.isInteger(instr.lane) || instr.lane < 0 || instr.lane >= row.laneCount) {
        return abort(`The lane index of ${instr.mnemonic} must be below ${row.laneCount}`)
      }
      checkOffset(instr.offset, memory.addressType, instr.mnemonic)
      popVal(context, ValType.v128)
      popVal(context, addressValType(memory))
      if (row.kind === 'load') pushVal(context, ValType.v128)
      return
    }
    case 'AtomicAccess': {
      const row = InstructionTable.atomicAccessOps[instr.mnemonic]
      const memory = memoryEntry(context, instr.memory)
      if (instr.align !== row.widthLog2) {
        return abort(`The alignment of ${instr.mnemonic} must be exactly ${row.widthLog2}`)
      }
      checkOffset(instr.offset, memory.addressType, instr.mnemonic)
      const address = addressValType(memory)
      switch (row.kind) {
        case 'load':
          popVal(context, address)
          pushVal(context, row.valType)
          return
        case 'store':
          popVal(context, row.valType)
          popVal(context, address)
          return
        case 'rmw':
          popVal(context, row.valType)
          popVal(context, address)
          pushVal(context, row.valType)
          return
        case 'cmpxchg':
          popVal(context, row.valType)
          popVal(context, row.valType)
          popVal(context, address)
          pushVal(context, row.valType)
          return
        case 'wait':
          popVal(context, ValType.i64)
          popVal(context, row.valType)
          popVal(context, address)
          pushVal(context, ValType.i32)
          return
        case 'notify':
          popVal(context, ValType.i32)
          popVal(context, address)
          pushVal(context, ValType.i32)
          return
      }
      return
    }
  }
  instr satisfies never
}

const checkTailResults = (context: Context, results: ReadonlyArray<ValType.ValType>): undefined => {
  if (
    results.length !== context.returnTypes.length ||
    results.some((type, index) => {
      const expected = context.returnTypes[index]
      return expected === undefined || !ValType.equals(type, expected)
    })
  ) {
    abort('A tail call must target a function returning this function’s result types')
  }
}

export interface CheckedBody {
  /** Entry indices of functions referenced by `ref.func` inside the body. */
  readonly refFuncs: ReadonlySet<number>
  /** Whether the body uses `memory.init` or `data.drop`. */
  readonly usesDataCount: boolean
}

/**
 * Validates a function body against the specification's typing rules.
 *
 * @internal
 */
export const checkBody = (
  state: ModuleState.MutableState,
  owner: OwnedHandle.Owner,
  funcType: ModuleState.FuncType,
  locals: ReadonlyArray<ModuleState.LocalDeclaration>,
  body: ReadonlyArray<Instr.Instr>,
  operation: string,
): Result.Result<CheckedBody, WasmError> => {
  for (const local of locals) {
    if (!ValType.isDefaultable(local.type)) {
      return Result.fail(
        validationFailed({
          operation,
          message: 'Locals must be defaultable; non-nullable reference locals are not supported',
          detail: local,
        }),
      )
    }
  }
  const context: Context = {
    state,
    owner,
    operation,
    localTypes: [...funcType.params, ...locals.map((local) => local.type)],
    returnTypes: funcType.results,
    valStack: [],
    ctrlStack: [],
    refFuncs: new Set(),
    usesDataCount: false,
  }
  try {
    pushFrame(context, 'func', [], funcType.results)
    checkSequence(context, body)
    popFrame(context)
    return Result.succeed({ refFuncs: context.refFuncs, usesDataCount: context.usesDataCount })
  } catch (failure) {
    if (failure instanceof Abort) {
      return Result.fail(
        failure.error ??
          validationFailed({ operation, message: failure.message, detail: undefined }),
      )
    }
    throw failure
  }
}
