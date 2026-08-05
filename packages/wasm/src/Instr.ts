/**
 * WebAssembly instructions as plain immutable data.
 *
 * Instructions are context-free values constructed without a builder or Effect: a function body
 * is an ordinary `ReadonlyArray<Instr>` that composes with array logic. Entity references inside
 * instructions are opaque handles; ownership and typing are validated when the body is committed
 * with `Func.define`, and handles resolve to numeric indices only at emission.
 *
 * @since 0.0.0
 */
import type * as Data from './Data.js'
import type * as Elem from './Elem.js'
import type * as Func from './Func.js'
import type * as Global from './Global.js'
import * as InstructionTable from './internal/InstructionTable.js'
import type * as Memory from './Memory.js'
import type * as Table from './Table.js'
import type * as Type from './Type.js'
import type * as ValType from './ValType.js'

/**
 * Mnemonic of an instruction that carries no immediate operand.
 *
 * @category instructions
 * @since 0.0.0
 */
export type PlainMnemonic =
  | 'unreachable'
  | 'nop'
  | 'return'
  | 'drop'
  | 'select'
  | 'ref.is_null'
  | 'i32.eqz'
  | 'i32.eq'
  | 'i32.ne'
  | 'i32.lt_s'
  | 'i32.lt_u'
  | 'i32.gt_s'
  | 'i32.gt_u'
  | 'i32.le_s'
  | 'i32.le_u'
  | 'i32.ge_s'
  | 'i32.ge_u'
  | 'i64.eqz'
  | 'i64.eq'
  | 'i64.ne'
  | 'i64.lt_s'
  | 'i64.lt_u'
  | 'i64.gt_s'
  | 'i64.gt_u'
  | 'i64.le_s'
  | 'i64.le_u'
  | 'i64.ge_s'
  | 'i64.ge_u'
  | 'f32.eq'
  | 'f32.ne'
  | 'f32.lt'
  | 'f32.gt'
  | 'f32.le'
  | 'f32.ge'
  | 'f64.eq'
  | 'f64.ne'
  | 'f64.lt'
  | 'f64.gt'
  | 'f64.le'
  | 'f64.ge'
  | 'i32.clz'
  | 'i32.ctz'
  | 'i32.popcnt'
  | 'i32.add'
  | 'i32.sub'
  | 'i32.mul'
  | 'i32.div_s'
  | 'i32.div_u'
  | 'i32.rem_s'
  | 'i32.rem_u'
  | 'i32.and'
  | 'i32.or'
  | 'i32.xor'
  | 'i32.shl'
  | 'i32.shr_s'
  | 'i32.shr_u'
  | 'i32.rotl'
  | 'i32.rotr'
  | 'i64.clz'
  | 'i64.ctz'
  | 'i64.popcnt'
  | 'i64.add'
  | 'i64.sub'
  | 'i64.mul'
  | 'i64.div_s'
  | 'i64.div_u'
  | 'i64.rem_s'
  | 'i64.rem_u'
  | 'i64.and'
  | 'i64.or'
  | 'i64.xor'
  | 'i64.shl'
  | 'i64.shr_s'
  | 'i64.shr_u'
  | 'i64.rotl'
  | 'i64.rotr'
  | 'f32.abs'
  | 'f32.neg'
  | 'f32.ceil'
  | 'f32.floor'
  | 'f32.trunc'
  | 'f32.nearest'
  | 'f32.sqrt'
  | 'f32.add'
  | 'f32.sub'
  | 'f32.mul'
  | 'f32.div'
  | 'f32.min'
  | 'f32.max'
  | 'f32.copysign'
  | 'f64.abs'
  | 'f64.neg'
  | 'f64.ceil'
  | 'f64.floor'
  | 'f64.trunc'
  | 'f64.nearest'
  | 'f64.sqrt'
  | 'f64.add'
  | 'f64.sub'
  | 'f64.mul'
  | 'f64.div'
  | 'f64.min'
  | 'f64.max'
  | 'f64.copysign'
  | 'i32.wrap_i64'
  | 'i32.trunc_f32_s'
  | 'i32.trunc_f32_u'
  | 'i32.trunc_f64_s'
  | 'i32.trunc_f64_u'
  | 'i64.extend_i32_s'
  | 'i64.extend_i32_u'
  | 'i64.trunc_f32_s'
  | 'i64.trunc_f32_u'
  | 'i64.trunc_f64_s'
  | 'i64.trunc_f64_u'
  | 'f32.convert_i32_s'
  | 'f32.convert_i32_u'
  | 'f32.convert_i64_s'
  | 'f32.convert_i64_u'
  | 'f32.demote_f64'
  | 'f64.convert_i32_s'
  | 'f64.convert_i32_u'
  | 'f64.convert_i64_s'
  | 'f64.convert_i64_u'
  | 'f64.promote_f32'
  | 'i32.reinterpret_f32'
  | 'i64.reinterpret_f64'
  | 'f32.reinterpret_i32'
  | 'f64.reinterpret_i64'
  | 'i32.extend8_s'
  | 'i32.extend16_s'
  | 'i64.extend8_s'
  | 'i64.extend16_s'
  | 'i64.extend32_s'
  | 'i32.trunc_sat_f32_s'
  | 'i32.trunc_sat_f32_u'
  | 'i32.trunc_sat_f64_s'
  | 'i32.trunc_sat_f64_u'
  | 'i64.trunc_sat_f32_s'
  | 'i64.trunc_sat_f32_u'
  | 'i64.trunc_sat_f64_s'
  | 'i64.trunc_sat_f64_u'

/**
 * Mnemonic of a memory load or store instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export type MemoryAccessMnemonic =
  | 'i32.load'
  | 'i64.load'
  | 'f32.load'
  | 'f64.load'
  | 'i32.load8_s'
  | 'i32.load8_u'
  | 'i32.load16_s'
  | 'i32.load16_u'
  | 'i64.load8_s'
  | 'i64.load8_u'
  | 'i64.load16_s'
  | 'i64.load16_u'
  | 'i64.load32_s'
  | 'i64.load32_u'
  | 'i32.store'
  | 'i64.store'
  | 'f32.store'
  | 'f64.store'
  | 'i32.store8'
  | 'i32.store16'
  | 'i64.store8'
  | 'i64.store16'
  | 'i64.store32'

/**
 * The type of a structured control-flow block.
 *
 * @category instructions
 * @since 0.0.0
 */
export type BlockType =
  | { readonly _tag: 'Empty' }
  | { readonly _tag: 'Value'; readonly type: ValType.ValType }
  | { readonly _tag: 'Func'; readonly type: Type.Type }

/**
 * A WebAssembly instruction as an immutable value.
 *
 * @category instructions
 * @since 0.0.0
 */
export type Instr =
  | { readonly _tag: 'Op'; readonly mnemonic: PlainMnemonic }
  | { readonly _tag: 'I32Const'; readonly value: number }
  | { readonly _tag: 'I64Const'; readonly value: bigint }
  | { readonly _tag: 'F32Const'; readonly value: number }
  | { readonly _tag: 'F64Const'; readonly value: number }
  | { readonly _tag: 'LocalGet'; readonly local: number }
  | { readonly _tag: 'LocalSet'; readonly local: number }
  | { readonly _tag: 'LocalTee'; readonly local: number }
  | { readonly _tag: 'GlobalGet'; readonly global: Global.Global }
  | { readonly _tag: 'GlobalSet'; readonly global: Global.Global }
  | { readonly _tag: 'Call'; readonly func: Func.Func }
  | {
      readonly _tag: 'CallIndirect'
      readonly table: Table.Table
      readonly type: Type.Type
    }
  | { readonly _tag: 'ReturnCall'; readonly func: Func.Func }
  | {
      readonly _tag: 'ReturnCallIndirect'
      readonly table: Table.Table
      readonly type: Type.Type
    }
  | {
      readonly _tag: 'Block'
      readonly blockType: BlockType
      readonly body: ReadonlyArray<Instr>
    }
  | {
      readonly _tag: 'Loop'
      readonly blockType: BlockType
      readonly body: ReadonlyArray<Instr>
    }
  | {
      readonly _tag: 'If'
      readonly blockType: BlockType
      readonly thenBody: ReadonlyArray<Instr>
      readonly elseBody: ReadonlyArray<Instr>
    }
  | { readonly _tag: 'Br'; readonly depth: number }
  | { readonly _tag: 'BrIf'; readonly depth: number }
  | {
      readonly _tag: 'BrTable'
      readonly depths: ReadonlyArray<number>
      readonly defaultDepth: number
    }
  | { readonly _tag: 'SelectTyped'; readonly types: ReadonlyArray<ValType.ValType> }
  | { readonly _tag: 'RefNull'; readonly refType: ValType.RefType }
  | { readonly _tag: 'RefFunc'; readonly func: Func.Func }
  | {
      readonly _tag: 'MemoryAccess'
      readonly mnemonic: MemoryAccessMnemonic
      readonly memory: Memory.Memory
      readonly align: number
      readonly offset: number
    }
  | { readonly _tag: 'MemorySize'; readonly memory: Memory.Memory }
  | { readonly _tag: 'MemoryGrow'; readonly memory: Memory.Memory }
  | {
      readonly _tag: 'MemoryInit'
      readonly data: Data.Data
      readonly memory: Memory.Memory
    }
  | { readonly _tag: 'DataDrop'; readonly data: Data.Data }
  | {
      readonly _tag: 'MemoryCopy'
      readonly destination: Memory.Memory
      readonly source: Memory.Memory
    }
  | { readonly _tag: 'MemoryFill'; readonly memory: Memory.Memory }
  | { readonly _tag: 'TableGet'; readonly table: Table.Table }
  | { readonly _tag: 'TableSet'; readonly table: Table.Table }
  | { readonly _tag: 'TableSize'; readonly table: Table.Table }
  | { readonly _tag: 'TableGrow'; readonly table: Table.Table }
  | { readonly _tag: 'TableFill'; readonly table: Table.Table }
  | {
      readonly _tag: 'TableCopy'
      readonly destination: Table.Table
      readonly source: Table.Table
    }
  | { readonly _tag: 'TableInit'; readonly elem: Elem.Elem; readonly table: Table.Table }
  | { readonly _tag: 'ElemDrop'; readonly elem: Elem.Elem }

const freeze = <A extends object>(value: A): A => Object.freeze(value)

/**
 * Constructs an instruction that carries no immediate operand.
 *
 * **Example** (Adding two stack values)
 *
 * ```ts
 * import * as Instr from '@silk-effect/wasm/Instr'
 *
 * const body = [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')]
 * ```
 *
 * @category constructors
 * @since 0.0.0
 */
export const op = (mnemonic: PlainMnemonic): Instr => freeze({ _tag: 'Op', mnemonic })

/**
 * Constructs `i32.const` with a 32-bit integer immediate.
 *
 * @category constructors
 * @since 0.0.0
 */
export const i32Const = (value: number): Instr => freeze({ _tag: 'I32Const', value })

/**
 * Constructs `i64.const` with a 64-bit integer immediate.
 *
 * @category constructors
 * @since 0.0.0
 */
export const i64Const = (value: bigint): Instr => freeze({ _tag: 'I64Const', value })

/**
 * Constructs `f32.const` with a 32-bit float immediate.
 *
 * @category constructors
 * @since 0.0.0
 */
export const f32Const = (value: number): Instr => freeze({ _tag: 'F32Const', value })

/**
 * Constructs `f64.const` with a 64-bit float immediate.
 *
 * @category constructors
 * @since 0.0.0
 */
export const f64Const = (value: number): Instr => freeze({ _tag: 'F64Const', value })

/**
 * Constructs `local.get` reading a parameter or local by index.
 *
 * @category constructors
 * @since 0.0.0
 */
export const localGet = (local: number): Instr => freeze({ _tag: 'LocalGet', local })

/**
 * Constructs `local.set` writing a parameter or local by index.
 *
 * @category constructors
 * @since 0.0.0
 */
export const localSet = (local: number): Instr => freeze({ _tag: 'LocalSet', local })

/**
 * Constructs `local.tee` writing a local while keeping the value on the stack.
 *
 * @category constructors
 * @since 0.0.0
 */
export const localTee = (local: number): Instr => freeze({ _tag: 'LocalTee', local })

/**
 * Constructs `global.get` reading a global.
 *
 * @category constructors
 * @since 0.0.0
 */
export const globalGet = (global: Global.Global): Instr => freeze({ _tag: 'GlobalGet', global })

/**
 * Constructs `global.set` writing a mutable global.
 *
 * @category constructors
 * @since 0.0.0
 */
export const globalSet = (global: Global.Global): Instr => freeze({ _tag: 'GlobalSet', global })

/**
 * Constructs `call` targeting an imported or defined function.
 *
 * @category constructors
 * @since 0.0.0
 */
export const call = (func: Func.Func): Instr => freeze({ _tag: 'Call', func })

/**
 * Constructs `call_indirect` through a table with an expected function type.
 *
 * @category constructors
 * @since 0.0.0
 */
export const callIndirect = (table: Table.Table, type: Type.Type): Instr =>
  freeze({ _tag: 'CallIndirect', table, type })

/**
 * Constructs `return_call`, the tail-calling form of `call`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const returnCall = (func: Func.Func): Instr => freeze({ _tag: 'ReturnCall', func })

/**
 * Constructs `return_call_indirect`, the tail-calling form of `call_indirect`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const returnCallIndirect = (table: Table.Table, type: Type.Type): Instr =>
  freeze({ _tag: 'ReturnCallIndirect', table, type })

/**
 * The empty block type `[] -> []`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const emptyBlockType: BlockType = Object.freeze({ _tag: 'Empty' })

/**
 * A block type producing one value.
 *
 * @category constructors
 * @since 0.0.0
 */
export const valueBlockType = (type: ValType.ValType): BlockType => freeze({ _tag: 'Value', type })

/**
 * A block type referencing an interned function type for multi-value blocks.
 *
 * @category constructors
 * @since 0.0.0
 */
export const funcBlockType = (type: Type.Type): BlockType => freeze({ _tag: 'Func', type })

/**
 * Constructs a `block` containing nested instructions.
 *
 * @category constructors
 * @since 0.0.0
 */
export const block = (blockType: BlockType, body: ReadonlyArray<Instr>): Instr =>
  freeze({ _tag: 'Block', blockType, body: Object.freeze([...body]) })

/**
 * Constructs a `loop` containing nested instructions.
 *
 * @category constructors
 * @since 0.0.0
 */
export const loop = (blockType: BlockType, body: ReadonlyArray<Instr>): Instr =>
  freeze({ _tag: 'Loop', blockType, body: Object.freeze([...body]) })

/**
 * Constructs an `if`/`else` with nested then and else instruction sequences.
 *
 * @category constructors
 * @since 0.0.0
 */
export const ifElse = (
  blockType: BlockType,
  thenBody: ReadonlyArray<Instr>,
  elseBody: ReadonlyArray<Instr> = [],
): Instr =>
  freeze({
    _tag: 'If',
    blockType,
    thenBody: Object.freeze([...thenBody]),
    elseBody: Object.freeze([...elseBody]),
  })

/**
 * Constructs `br` branching to an enclosing block by relative depth.
 *
 * @category constructors
 * @since 0.0.0
 */
export const br = (depth: number): Instr => freeze({ _tag: 'Br', depth })

/**
 * Constructs `br_if` conditionally branching by relative depth.
 *
 * @category constructors
 * @since 0.0.0
 */
export const brIf = (depth: number): Instr => freeze({ _tag: 'BrIf', depth })

/**
 * Constructs `br_table` selecting among branch depths by index.
 *
 * @category constructors
 * @since 0.0.0
 */
export const brTable = (depths: ReadonlyArray<number>, defaultDepth: number): Instr =>
  freeze({ _tag: 'BrTable', depths: Object.freeze([...depths]), defaultDepth })

/**
 * Constructs `select` with an explicit result-type annotation, required for reference types.
 *
 * @category constructors
 * @since 0.0.0
 */
export const selectTyped = (types: ReadonlyArray<ValType.ValType>): Instr =>
  freeze({ _tag: 'SelectTyped', types: Object.freeze([...types]) })

/**
 * Constructs `ref.null` producing a null reference of the given type.
 *
 * @category constructors
 * @since 0.0.0
 */
export const refNull = (refType: ValType.RefType): Instr => freeze({ _tag: 'RefNull', refType })

/**
 * Constructs `ref.func` producing a reference to a function.
 *
 * @category constructors
 * @since 0.0.0
 */
export const refFunc = (func: Func.Func): Instr => freeze({ _tag: 'RefFunc', func })

/**
 * Options accepted by {@link memoryAccess}.
 *
 * @category constructors
 * @since 0.0.0
 */
export interface MemoryAccessOptions {
  /** Static address offset in bytes. Defaults to zero. */
  readonly offset?: number
  /** Base-two logarithm of the alignment. Defaults to the access's natural alignment. */
  readonly align?: number
}

/**
 * Constructs a memory load or store against one memory with optional offset and alignment.
 *
 * **Example** (Loading from the second memory)
 *
 * ```ts
 * import * as Instr from '@silk-effect/wasm/Instr'
 * import type * as Memory from '@silk-effect/wasm/Memory'
 *
 * declare const scratch: Memory.Memory
 *
 * const load = Instr.memoryAccess('i32.load', scratch, { offset: 16 })
 * ```
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryAccess = (
  mnemonic: MemoryAccessMnemonic,
  memory: Memory.Memory,
  options: MemoryAccessOptions = {},
): Instr =>
  freeze({
    _tag: 'MemoryAccess',
    mnemonic,
    memory,
    align: options.align ?? InstructionTable.memoryAccessOps[mnemonic].widthLog2,
    offset: options.offset ?? 0,
  })

/**
 * Constructs `memory.size` for one memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memorySize = (memory: Memory.Memory): Instr => freeze({ _tag: 'MemorySize', memory })

/**
 * Constructs `memory.grow` for one memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryGrow = (memory: Memory.Memory): Instr => freeze({ _tag: 'MemoryGrow', memory })

/**
 * Constructs `memory.init` copying a passive data segment into a memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryInit = (data: Data.Data, memory: Memory.Memory): Instr =>
  freeze({ _tag: 'MemoryInit', data, memory })

/**
 * Constructs `data.drop` releasing a passive data segment.
 *
 * @category constructors
 * @since 0.0.0
 */
export const dataDrop = (data: Data.Data): Instr => freeze({ _tag: 'DataDrop', data })

/**
 * Constructs `memory.copy` between two memories, which may be the same memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryCopy = (destination: Memory.Memory, source: Memory.Memory): Instr =>
  freeze({ _tag: 'MemoryCopy', destination, source })

/**
 * Constructs `memory.fill` for one memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryFill = (memory: Memory.Memory): Instr => freeze({ _tag: 'MemoryFill', memory })

/**
 * Constructs `table.get`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableGet = (table: Table.Table): Instr => freeze({ _tag: 'TableGet', table })

/**
 * Constructs `table.set`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableSet = (table: Table.Table): Instr => freeze({ _tag: 'TableSet', table })

/**
 * Constructs `table.size`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableSize = (table: Table.Table): Instr => freeze({ _tag: 'TableSize', table })

/**
 * Constructs `table.grow`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableGrow = (table: Table.Table): Instr => freeze({ _tag: 'TableGrow', table })

/**
 * Constructs `table.fill`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableFill = (table: Table.Table): Instr => freeze({ _tag: 'TableFill', table })

/**
 * Constructs `table.copy` between two tables, which may be the same table.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableCopy = (destination: Table.Table, source: Table.Table): Instr =>
  freeze({ _tag: 'TableCopy', destination, source })

/**
 * Constructs `table.init` copying a passive element segment into a table.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableInit = (elem: Elem.Elem, table: Table.Table): Instr =>
  freeze({ _tag: 'TableInit', elem, table })

/**
 * Constructs `elem.drop` releasing a passive element segment.
 *
 * @category constructors
 * @since 0.0.0
 */
export const elemDrop = (elem: Elem.Elem): Instr => freeze({ _tag: 'ElemDrop', elem })
