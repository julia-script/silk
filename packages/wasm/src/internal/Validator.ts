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
import type * as ModuleState from './ModuleState.js'
import type * as OwnedHandle from './OwnedHandle.js'

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
  if (expect !== undefined && actual !== 'unknown' && !ValType.equals(actual, expect)) {
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
      const funcType = context.state.types[index]
      if (funcType === undefined) return abort('Type table entry is missing')
      return { startTypes: funcType.params, endTypes: funcType.results }
    }
  }
}

const funcTypeOf = (context: Context, index: number): ModuleState.FuncType => {
  const entry = context.state.funcs[index]
  const funcType = entry === undefined ? undefined : context.state.types[entry.typeIndex]
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

const memoryIndex = (context: Context, handle: Parameters<typeof Handle.resolve>[1]): number => {
  const index = orAbort(Handle.resolve(context.owner, handle, 'Memory', context.operation))
  if (context.state.memories[index] === undefined) return abort('Memory entry is missing')
  return index
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
      if (table.refType._tag !== 'FuncRef') {
        return abort('call_indirect requires a funcref table')
      }
      const typeIndex = orAbort(
        Handle.resolve(context.owner, instr.type, 'Type', context.operation),
      )
      const funcType = context.state.types[typeIndex] ?? abort('Type table entry is missing')
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
      if (table.refType._tag !== 'FuncRef') {
        return abort('return_call_indirect requires a funcref table')
      }
      const typeIndex = orAbort(
        Handle.resolve(context.owner, instr.type, 'Type', context.operation),
      )
      const funcType = context.state.types[typeIndex] ?? abort('Type table entry is missing')
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
      pushVal(context, instr.refType)
      return
    case 'RefFunc': {
      const index = orAbort(Handle.resolve(context.owner, instr.func, 'Func', context.operation))
      if (context.state.funcs[index] === undefined) {
        return abort('Function table entry is missing')
      }
      context.refFuncs.add(index)
      pushVal(context, ValType.funcref)
      return
    }
    case 'MemoryAccess': {
      const row = InstructionTable.memoryAccessOps[instr.mnemonic]
      memoryIndex(context, instr.memory)
      if (!Number.isInteger(instr.align) || instr.align < 0 || instr.align > row.widthLog2) {
        return abort(`The alignment of ${instr.mnemonic} must be between 0 and ${row.widthLog2}`)
      }
      if (!Number.isInteger(instr.offset) || instr.offset < 0 || instr.offset > 0xffffffff) {
        return abort('The memory access offset must be an unsigned 32-bit integer')
      }
      if (row.kind === 'load') {
        popVal(context, ValType.i32)
        pushVal(context, row.valType)
      } else {
        popVal(context, row.valType)
        popVal(context, ValType.i32)
      }
      return
    }
    case 'MemorySize':
      memoryIndex(context, instr.memory)
      pushVal(context, ValType.i32)
      return
    case 'MemoryGrow':
      memoryIndex(context, instr.memory)
      popVal(context, ValType.i32)
      pushVal(context, ValType.i32)
      return
    case 'MemoryInit':
      dataIndex(context, instr.data)
      memoryIndex(context, instr.memory)
      popVals(context, [ValType.i32, ValType.i32, ValType.i32])
      return
    case 'DataDrop':
      dataIndex(context, instr.data)
      return
    case 'MemoryCopy':
      memoryIndex(context, instr.destination)
      memoryIndex(context, instr.source)
      popVals(context, [ValType.i32, ValType.i32, ValType.i32])
      return
    case 'MemoryFill':
      memoryIndex(context, instr.memory)
      popVals(context, [ValType.i32, ValType.i32, ValType.i32])
      return
    case 'TableGet': {
      const table = tableEntry(context, instr.table)
      popVal(context, ValType.i32)
      pushVal(context, table.refType)
      return
    }
    case 'TableSet': {
      const table = tableEntry(context, instr.table)
      popVal(context, table.refType)
      popVal(context, ValType.i32)
      return
    }
    case 'TableSize':
      tableEntry(context, instr.table)
      pushVal(context, ValType.i32)
      return
    case 'TableGrow': {
      const table = tableEntry(context, instr.table)
      popVal(context, ValType.i32)
      popVal(context, table.refType)
      pushVal(context, ValType.i32)
      return
    }
    case 'TableFill': {
      const table = tableEntry(context, instr.table)
      popVal(context, ValType.i32)
      popVal(context, table.refType)
      popVal(context, ValType.i32)
      return
    }
    case 'TableCopy': {
      const destination = tableEntry(context, instr.destination)
      const source = tableEntry(context, instr.source)
      if (!ValType.equals(destination.refType, source.refType)) {
        return abort('table.copy requires tables with the same reference type')
      }
      popVals(context, [ValType.i32, ValType.i32, ValType.i32])
      return
    }
    case 'TableInit': {
      const elem = elemEntry(context, instr.elem)
      const table = tableEntry(context, instr.table)
      if (!ValType.equals(elem.refType, table.refType)) {
        return abort('table.init requires a segment matching the table reference type')
      }
      popVals(context, [ValType.i32, ValType.i32, ValType.i32])
      return
    }
    case 'ElemDrop':
      elemEntry(context, instr.elem)
      return
  }
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
  const context: Context = {
    state,
    owner,
    operation,
    localTypes: [...funcType.params, ...locals.map((local) => local.type)],
    returnTypes: funcType.results,
    valStack: [],
    ctrlStack: [],
    refFuncs: new Set(),
  }
  try {
    pushFrame(context, 'func', [], funcType.results)
    checkSequence(context, body)
    popFrame(context)
    return Result.succeed({ refFuncs: context.refFuncs })
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
