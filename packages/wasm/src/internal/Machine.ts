/**
 * The direct-interpretation machine behind `Interp`.
 *
 * The machine executes committed `Instr` trees directly — no lowering, no compilation — with an
 * explicit call stack, per-frame value stacks, and per-block program counters. Because no JS call
 * stack is consumed by wasm control flow, execution can pause between any two instructions, which
 * is what makes the machine usable as a debugger engine.
 *
 * Scope: the full scalar instruction set (numeric, parametric, control, calls including tail
 * calls, memory, tables, segments, and untyped/function references). SIMD, atomics (except
 * `atomic.fence`), GC, and exception handling trap as unsupported.
 *
 * @internal
 */
import * as Result from 'effect/Result'
import type * as Instr from '../Instr.js'
import type * as ValType from '../ValType.js'
import * as Handle from './Handle.js'
import * as InstructionTable from './InstructionTable.js'
import * as ModuleState from './ModuleState.js'
import type * as OwnedHandle from './OwnedHandle.js'

/** Thrown inside the machine for a WebAssembly trap; never escapes the public surface. */
export class Trap {
  readonly _tag = 'Trap'
  constructor(readonly message: string) {}
}

export type RefValue =
  | { readonly _tag: 'NullRef' }
  | { readonly _tag: 'FuncRef'; readonly funcIndex: number }
  | { readonly _tag: 'HostRef'; readonly value: unknown }

export const nullRef: RefValue = Object.freeze({ _tag: 'NullRef' })

export const funcRef = (funcIndex: number): RefValue =>
  Object.freeze({ _tag: 'FuncRef', funcIndex })

/** A runtime value: i32/f32/f64 as number, i64 as bigint, references as RefValue. */
export type Value = number | bigint | RefValue

export interface Location {
  readonly funcIndex: number
  readonly funcName: string | undefined
  readonly instr: Instr.Instr
}

export type Status =
  | { readonly _tag: 'Paused'; readonly location: Location }
  | { readonly _tag: 'Done'; readonly results: ReadonlyArray<Value> }
  | { readonly _tag: 'Trapped'; readonly message: string }

export type HostFunc = (args: ReadonlyArray<Value>) => Value | ReadonlyArray<Value> | undefined

export interface Imports {
  readonly [module: string]: { readonly [field: string]: HostFunc | Value }
}

interface MemoryInstance {
  bytes: Uint8Array
  view: DataView
  readonly min: number
  readonly max: number
  readonly addressType: ModuleState.AddressType
}

interface TableInstance {
  elements: Array<RefValue>
  readonly max: number
  readonly addressType: ModuleState.AddressType
}

interface GlobalInstance {
  value: Value
  readonly mutable: boolean
}

export interface InstanceState {
  readonly owner: OwnedHandle.Owner
  readonly snapshot: ModuleState.Snapshot
  readonly hostFuncs: ReadonlyArray<HostFunc | undefined>
  readonly globals: ReadonlyArray<GlobalInstance>
  readonly memories: ReadonlyArray<MemoryInstance>
  readonly tables: ReadonlyArray<TableInstance>
  /** Passive data segment payloads; dropped segments become empty. */
  readonly datas: Array<Uint8Array>
  /** Evaluated element segment items; dropped segments become empty. */
  readonly elems: Array<ReadonlyArray<RefValue>>
  readonly handleCache: WeakMap<object, number>
}

const PAGE = 65536
const MAX_PAGES = 65536

const scratch = new DataView(new ArrayBuffer(8))

// ---------------------------------------------------------------------------
// Numeric helpers
// ---------------------------------------------------------------------------

const u32 = (x: number): number => x >>> 0

const i64 = (x: bigint): bigint => BigInt.asIntN(64, x)

const u64 = (x: bigint): bigint => BigInt.asUintN(64, x)

const popcnt32 = (x: number): number => {
  let v = x >>> 0
  let n = 0
  while (v !== 0) {
    v &= v - 1
    n += 1
  }
  return n
}

const ctz32 = (x: number): number => (x === 0 ? 32 : 31 - Math.clz32(x & -x))

const clz64 = (x: bigint): bigint => {
  const u = u64(x)
  return u === 0n ? 64n : BigInt(64 - u.toString(2).length)
}

const ctz64 = (x: bigint): bigint => {
  const u = u64(x)
  return u === 0n ? 64n : BigInt((u & -u).toString(2).length - 1)
}

const popcnt64 = (x: bigint): bigint => {
  let n = 0n
  for (const bit of u64(x).toString(2)) if (bit === '1') n += 1n
  return n
}

const rotl32 = (x: number, count: number): number => {
  const n = count & 31
  return n === 0 ? x | 0 : (x << n) | (x >>> (32 - n)) | 0
}

const rotr32 = (x: number, count: number): number => {
  const n = count & 31
  return n === 0 ? x | 0 : (x >>> n) | (x << (32 - n)) | 0
}

const rotl64 = (x: bigint, count: bigint): bigint => {
  const n = u64(count) & 63n
  const u = u64(x)
  return i64((u << n) | (u >> (64n - n)))
}

const rotr64 = (x: bigint, count: bigint): bigint => {
  const n = u64(count) & 63n
  const u = u64(x)
  return i64((u >> n) | (u << (64n - n)))
}

const signBit = (x: number): boolean => {
  scratch.setFloat64(0, x)
  return scratch.getUint8(0) >= 0x80
}

const fmin = (a: number, b: number): number => {
  if (Number.isNaN(a) || Number.isNaN(b)) return Number.NaN
  if (a < b) return a
  if (b < a) return b
  return signBit(a) ? a : b
}

const fmax = (a: number, b: number): number => {
  if (Number.isNaN(a) || Number.isNaN(b)) return Number.NaN
  if (a > b) return a
  if (b > a) return b
  return signBit(a) ? b : a
}

const copysign = (a: number, b: number): number => (signBit(b) ? -Math.abs(a) : Math.abs(a))

/** Round-half-to-even, preserving NaN, infinities, and negative zero. */
const nearest = (x: number): number => {
  if (!Number.isFinite(x) || Number.isInteger(x)) return x
  const floor = Math.floor(x)
  const fraction = x - floor
  const rounded =
    fraction < 0.5 ? floor : fraction > 0.5 ? floor + 1 : floor % 2 === 0 ? floor : floor + 1
  return rounded === 0 && x < 0 ? -0 : rounded
}

const truncChecked = (x: number, lo: number, hi: number): number => {
  if (Number.isNaN(x)) throw new Trap('invalid conversion to integer')
  const t = Math.trunc(x)
  if (t < lo || t > hi) throw new Trap('integer overflow')
  return t
}

const truncChecked64 = (x: number, unsigned: boolean): bigint => {
  if (Number.isNaN(x)) throw new Trap('invalid conversion to integer')
  const t = Math.trunc(x)
  if (unsigned ? t < 0 || t >= 2 ** 64 : t < -(2 ** 63) || t >= 2 ** 63) {
    throw new Trap('integer overflow')
  }
  return i64(BigInt(t))
}

const truncSat64 = (x: number, unsigned: boolean): bigint => {
  if (Number.isNaN(x)) return 0n
  const t = Math.trunc(x)
  if (unsigned) {
    if (t < 0) return 0n
    if (t >= 2 ** 64) return i64((1n << 64n) - 1n)
  } else {
    if (t < -(2 ** 63)) return -(1n << 63n)
    if (t >= 2 ** 63) return (1n << 63n) - 1n
  }
  return i64(BigInt(t))
}

const truncSat = (x: number, lo: number, hi: number): number => {
  if (Number.isNaN(x)) return 0
  return Math.min(hi, Math.max(lo, Math.trunc(x)))
}

const divS32 = (a: number, b: number): number => {
  if (b === 0) throw new Trap('integer divide by zero')
  if (a === -2147483648 && b === -1) throw new Trap('integer overflow')
  return Math.trunc(a / b) | 0
}

const divU32 = (a: number, b: number): number => {
  const ub = u32(b)
  if (ub === 0) throw new Trap('integer divide by zero')
  return Math.trunc(u32(a) / ub) | 0
}

const remS32 = (a: number, b: number): number => {
  if (b === 0) throw new Trap('integer divide by zero')
  return (a % b) | 0
}

const remU32 = (a: number, b: number): number => {
  const ub = u32(b)
  if (ub === 0) throw new Trap('integer divide by zero')
  return (u32(a) % ub) | 0
}

const divS64 = (a: bigint, b: bigint): bigint => {
  if (b === 0n) throw new Trap('integer divide by zero')
  if (a === -(1n << 63n) && b === -1n) throw new Trap('integer overflow')
  return a / b
}

const divU64 = (a: bigint, b: bigint): bigint => {
  if (b === 0n) throw new Trap('integer divide by zero')
  return i64(u64(a) / u64(b))
}

const remS64 = (a: bigint, b: bigint): bigint => {
  if (b === 0n) throw new Trap('integer divide by zero')
  return a % b
}

const remU64 = (a: bigint, b: bigint): bigint => {
  if (b === 0n) throw new Trap('integer divide by zero')
  return i64(u64(a) % u64(b))
}

const reinterpretF32ToI32 = (x: number): number => {
  scratch.setFloat32(0, x, true)
  return scratch.getInt32(0, true)
}

const reinterpretI32ToF32 = (x: number): number => {
  scratch.setInt32(0, x, true)
  return scratch.getFloat32(0, true)
}

const reinterpretF64ToI64 = (x: number): bigint => {
  scratch.setFloat64(0, x, true)
  return scratch.getBigInt64(0, true)
}

const reinterpretI64ToF64 = (x: bigint): number => {
  scratch.setBigInt64(0, x, true)
  return scratch.getFloat64(0, true)
}

// ---------------------------------------------------------------------------
// Values and types
// ---------------------------------------------------------------------------

export const defaultValue = (type: ValType.ValType): Value => {
  switch (type._tag) {
    case 'I32':
    case 'F32':
    case 'F64':
      return 0
    case 'I64':
      return 0n
    case 'Ref':
      return nullRef
    case 'V128':
      throw new Trap('unsupported value type v128')
  }
}

const isRef = (value: Value): value is RefValue =>
  typeof value === 'object' && value !== null && '_tag' in value

/**
 * Validates and normalizes a boundary value — a call argument or a host-function result —
 * against its declared type. Throws a Trap with a caller-readable message on mismatch.
 */
export const coerceValue = (type: ValType.ValType, value: Value, what: string): Value => {
  switch (type._tag) {
    case 'I32':
      if (typeof value !== 'number' || !Number.isFinite(value)) {
        throw new Trap(`${what} must be a finite number for i32`)
      }
      return value | 0
    case 'I64':
      if (typeof value !== 'bigint') throw new Trap(`${what} must be a bigint for i64`)
      return i64(value)
    case 'F32':
      if (typeof value !== 'number') throw new Trap(`${what} must be a number for f32`)
      return Math.fround(value)
    case 'F64':
      if (typeof value !== 'number') throw new Trap(`${what} must be a number for f64`)
      return value
    case 'Ref':
      if (!isRef(value)) throw new Trap(`${what} must be a reference value`)
      return value
    case 'V128':
      throw new Trap(`${what} has the unsupported type v128`)
  }
}

// ---------------------------------------------------------------------------
// Instantiation
// ---------------------------------------------------------------------------

export class LinkError {
  readonly _tag = 'LinkError'
  constructor(readonly message: string) {}
}

const limitNumber = (value: bigint | undefined, fallback: number): number =>
  value === undefined ? fallback : Number(value)

/** Evaluates a constant expression: consts, `global.get`, refs, and extended add/sub/mul. */
const evalConstExpr = (instance: InstanceState, expr: ReadonlyArray<Instr.Instr>): Value => {
  const stack: Array<Value> = []
  const pop = (): Value => {
    const value = stack.pop()
    if (value === undefined) throw new Trap('constant expression stack underflow')
    return value
  }
  for (const instr of expr) {
    switch (instr._tag) {
      case 'I32Const':
        stack.push(instr.value | 0)
        break
      case 'I64Const':
        stack.push(i64(instr.value))
        break
      case 'F32Const':
        stack.push(Math.fround(instr.value))
        break
      case 'F64Const':
        stack.push(instr.value)
        break
      case 'RefNull':
        stack.push(nullRef)
        break
      case 'RefFunc':
        stack.push(funcRef(resolveIndex(instance, instr.func, 'Func')))
        break
      case 'GlobalGet': {
        const index = resolveIndex(instance, instr.global, 'Global')
        const global = instance.globals[index]
        if (global === undefined) throw new Trap('unknown global in constant expression')
        stack.push(global.value)
        break
      }
      case 'Op': {
        const b = pop()
        const a = pop()
        switch (instr.mnemonic) {
          case 'i32.add':
            stack.push(((a as number) + (b as number)) | 0)
            break
          case 'i32.sub':
            stack.push(((a as number) - (b as number)) | 0)
            break
          case 'i32.mul':
            stack.push(Math.imul(a as number, b as number))
            break
          case 'i64.add':
            stack.push(i64((a as bigint) + (b as bigint)))
            break
          case 'i64.sub':
            stack.push(i64((a as bigint) - (b as bigint)))
            break
          case 'i64.mul':
            stack.push(i64((a as bigint) * (b as bigint)))
            break
          default:
            throw new Trap(`unsupported constant instruction ${instr.mnemonic}`)
        }
        break
      }
      default:
        throw new Trap(`unsupported constant instruction ${instr._tag}`)
    }
  }
  const result = stack.pop()
  if (result === undefined) throw new Trap('constant expression produced no value')
  return result
}

const resolveIndex = (instance: InstanceState, handle: object, tag: string): number => {
  const cached = instance.handleCache.get(handle)
  if (cached !== undefined) return cached
  const resolved = Handle.resolve(instance.owner, handle as Handle.Handle<string>, tag, 'Interp')
  if (Result.isFailure(resolved)) throw new Trap(`unresolvable ${tag} handle`)
  const index = Result.getOrThrow(resolved)
  instance.handleCache.set(handle, index)
  return index
}

/**
 * Builds runtime state from a snapshot: links imports, evaluates global initializers,
 * allocates memories and tables, applies active segments, and runs the start function.
 */
export const instantiate = (snapshot: ModuleState.Snapshot, imports: Imports): InstanceState => {
  const lookupImport = (source: ModuleState.ImportSource): HostFunc | Value | undefined =>
    imports[source.module]?.[source.field]

  const hostFuncs: Array<HostFunc | undefined> = snapshot.funcs.map((entry) => {
    if (entry.importSource === undefined) return undefined
    const provided = lookupImport(entry.importSource)
    if (typeof provided !== 'function') {
      throw new LinkError(
        `missing host function for import ${entry.importSource.module}.${entry.importSource.field}`,
      )
    }
    return provided
  })

  const globals: Array<GlobalInstance> = []
  const memories: Array<MemoryInstance> = snapshot.memories.map((entry) => {
    const min = limitNumber(entry.limits.min, 0)
    const bytes = new Uint8Array(min * PAGE)
    return {
      bytes,
      view: new DataView(bytes.buffer),
      min,
      max: Math.min(limitNumber(entry.limits.max, MAX_PAGES), MAX_PAGES),
      addressType: entry.addressType,
    }
  })
  const tables: Array<TableInstance> = snapshot.tables.map((entry) => ({
    elements: new Array<RefValue>(limitNumber(entry.limits.min, 0)).fill(nullRef),
    max: limitNumber(entry.limits.max, Number.POSITIVE_INFINITY),
    addressType: entry.addressType,
  }))

  const instance: InstanceState = {
    owner: snapshot.owner,
    snapshot,
    hostFuncs,
    globals,
    memories,
    tables,
    datas: snapshot.datas.map((entry) => entry.bytes),
    elems: [],
    handleCache: new WeakMap(),
  }

  for (const entry of snapshot.globals) {
    if (entry.importSource !== undefined) {
      const provided = lookupImport(entry.importSource)
      if (provided === undefined || typeof provided === 'function') {
        throw new LinkError(
          `missing host value for global import ${entry.importSource.module}.${entry.importSource.field}`,
        )
      }
      globals.push({
        value: coerceValue(
          entry.valType,
          provided,
          `global import ${entry.importSource.module}.${entry.importSource.field}`,
        ),
        mutable: entry.mutable,
      })
    } else {
      if (entry.init === undefined) throw new LinkError('global has no initializer')
      globals.push({ value: evalConstExpr(instance, entry.init), mutable: entry.mutable })
    }
  }

  for (const entry of snapshot.elems) {
    const items = entry.items.map((item) => {
      const value = evalConstExpr(instance, item)
      return isRef(value) ? value : nullRef
    })
    if (entry.mode._tag === 'Active') {
      const table = tables[entry.mode.table]
      if (table === undefined) throw new LinkError('active element segment targets unknown table')
      const offset = Number(evalConstExpr(instance, entry.mode.offset))
      if (offset + items.length > table.elements.length) {
        throw new Trap('out of bounds table access')
      }
      for (const [i, item] of items.entries()) table.elements[offset + i] = item
      instance.elems.push([])
    } else {
      instance.elems.push(entry.mode._tag === 'Declarative' ? [] : items)
    }
  }

  for (const [index, entry] of snapshot.datas.entries()) {
    if (entry.mode._tag !== 'Active') continue
    const memory = memories[entry.mode.memory]
    if (memory === undefined) throw new LinkError('active data segment targets unknown memory')
    const offset = Number(evalConstExpr(instance, entry.mode.offset))
    if (offset + entry.bytes.length > memory.bytes.length) {
      throw new Trap('out of bounds memory access')
    }
    memory.bytes.set(entry.bytes, offset)
    instance.datas[index] = new Uint8Array(0)
  }

  if (snapshot.start !== undefined) {
    const machine = new Machine(instance, snapshot.start, [])
    const status = machine.run(Number.POSITIVE_INFINITY)
    if (status._tag === 'Trapped') throw new Trap(status.message)
  }

  return instance
}

// ---------------------------------------------------------------------------
// The machine
// ---------------------------------------------------------------------------

interface Control {
  readonly kind: 'func' | 'block' | 'loop'
  readonly body: ReadonlyArray<Instr.Instr>
  pc: number
  /** Value stack height below this label's parameters. */
  readonly stackBase: number
  readonly paramCount: number
  readonly resultCount: number
}

export interface Frame {
  readonly funcIndex: number
  readonly locals: Array<Value>
  readonly stack: Array<Value>
  readonly control: Array<Control>
  readonly resultCount: number
}

export class Machine {
  readonly frames: Array<Frame> = []
  status: Status
  stepCount = 0

  constructor(
    readonly instance: InstanceState,
    funcIndex: number,
    args: ReadonlyArray<Value>,
  ) {
    this.status = { _tag: 'Done', results: [] }
    try {
      this.callFunction(funcIndex, [...args])
      this.normalize()
    } catch (error) {
      this.handleThrown(error)
    }
  }

  private funcType(funcIndex: number): ModuleState.FuncType {
    const entry = this.instance.snapshot.funcs[funcIndex]
    const type =
      entry === undefined
        ? undefined
        : ModuleState.funcTypeAt(this.instance.snapshot.types, entry.typeIndex)
    if (type === undefined) throw new Trap('unknown function')
    return type
  }

  private frame(): Frame {
    const frame = this.frames[this.frames.length - 1]
    if (frame === undefined) throw new Trap('no active frame')
    return frame
  }

  private push(value: Value): void {
    this.frame().stack.push(value)
  }

  private pop(): Value {
    const value = this.frame().stack.pop()
    if (value === undefined) throw new Trap('value stack underflow')
    return value
  }

  private popI32 = (): number => this.pop() as number
  private popI64 = (): bigint => this.pop() as bigint
  private popF = (): number => this.pop() as number
  private popRef(): RefValue {
    const value = this.pop()
    if (!isRef(value)) throw new Trap('expected a reference value')
    return value
  }

  private idx(handle: object, tag: string): number {
    return resolveIndex(this.instance, handle, tag)
  }

  private memory(handle: import('../Memory.js').Memory): MemoryInstance {
    const memory = this.instance.memories[this.idx(handle, 'Memory')]
    if (memory === undefined) throw new Trap('unknown memory')
    return memory
  }

  private table(handle: import('../Table.js').Table): TableInstance {
    const table = this.instance.tables[this.idx(handle, 'Table')]
    if (table === undefined) throw new Trap('unknown table')
    return table
  }

  /** Pops an address operand (i32 or i64 per the memory's address type) as a safe number. */
  private popAddress(): number {
    const value = this.pop()
    if (typeof value === 'bigint') {
      const unsigned = u64(value)
      if (unsigned > BigInt(Number.MAX_SAFE_INTEGER)) throw new Trap('out of bounds memory access')
      return Number(unsigned)
    }
    return u32(value as number)
  }

  private effectiveAddress(memory: MemoryInstance, offset: number | bigint, width: number): number {
    const address = this.popAddress() + Number(offset)
    if (address + width > memory.bytes.length) throw new Trap('out of bounds memory access')
    return address
  }

  private blockArity(blockType: Instr.BlockType): {
    readonly paramCount: number
    readonly resultCount: number
  } {
    switch (blockType._tag) {
      case 'Empty':
        return { paramCount: 0, resultCount: 0 }
      case 'Value':
        return { paramCount: 0, resultCount: 1 }
      case 'Func': {
        const typeIndex = this.idx(blockType.type, 'Type')
        const type = ModuleState.funcTypeAt(this.instance.snapshot.types, typeIndex)
        if (type === undefined) throw new Trap('block type is not a function type')
        return { paramCount: type.params.length, resultCount: type.results.length }
      }
    }
  }

  private enterBlock(
    kind: 'block' | 'loop',
    blockType: Instr.BlockType,
    body: ReadonlyArray<Instr.Instr>,
  ): void {
    const { paramCount, resultCount } = this.blockArity(blockType)
    const frame = this.frame()
    frame.control.push({
      kind,
      body,
      pc: 0,
      stackBase: frame.stack.length - paramCount,
      paramCount,
      resultCount,
    })
  }

  /** Transfers `count` values across a stack truncation to `base`. */
  private unwindStack(stack: Array<Value>, base: number, count: number): void {
    if (count === 0) {
      stack.length = base
      return
    }
    const kept = stack.slice(stack.length - count)
    stack.length = base
    stack.push(...kept)
  }

  private branch(depth: number): void {
    const frame = this.frame()
    const targetIndex = frame.control.length - 1 - depth
    const target = frame.control[targetIndex]
    if (target === undefined) throw new Trap('branch depth out of range')
    if (target.kind === 'loop') {
      this.unwindStack(frame.stack, target.stackBase, target.paramCount)
      frame.control.length = targetIndex + 1
      target.pc = 0
    } else if (target.kind === 'func') {
      this.returnFromFrame()
    } else {
      this.unwindStack(frame.stack, target.stackBase, target.resultCount)
      frame.control.length = targetIndex
    }
  }

  private returnFromFrame(): void {
    const frame = this.frames.pop()
    if (frame === undefined) throw new Trap('no active frame')
    const results = frame.stack.slice(frame.stack.length - frame.resultCount)
    const caller = this.frames[this.frames.length - 1]
    if (caller === undefined) {
      this.status = { _tag: 'Done', results }
    } else {
      caller.stack.push(...results)
    }
  }

  private callFunction(funcIndex: number, args: Array<Value>): void {
    const entry = this.instance.snapshot.funcs[funcIndex]
    if (entry === undefined) throw new Trap('unknown function')
    const type = this.funcType(funcIndex)
    const host = this.instance.hostFuncs[funcIndex]
    if (host !== undefined) {
      let results: Value | ReadonlyArray<Value> | undefined
      try {
        results = host(args)
      } catch (error) {
        throw error instanceof Trap ? error : new Trap(`host function failed: ${String(error)}`)
      }
      const list =
        results === undefined ? [] : Array.isArray(results) ? results : [results as Value]
      if (list.length !== type.results.length) {
        throw new Trap('host function returned the wrong number of results')
      }
      const coerced = type.results.map((resultType, index) =>
        coerceValue(resultType, list[index] as Value, `host result ${index}`),
      )
      const caller = this.frames[this.frames.length - 1]
      if (caller === undefined) {
        this.status = { _tag: 'Done', results: coerced }
      } else {
        caller.stack.push(...coerced)
      }
      return
    }
    if (entry.definition === undefined) throw new Trap('function has no body')
    const locals: Array<Value> = [...args]
    for (const local of entry.definition.locals) locals.push(defaultValue(local.type))
    this.frames.push({
      funcIndex,
      locals,
      stack: [],
      control: [
        {
          kind: 'func',
          body: entry.definition.body,
          pc: 0,
          stackBase: 0,
          paramCount: type.params.length,
          resultCount: type.results.length,
        },
      ],
      resultCount: type.results.length,
    })
  }

  private popArgs(funcIndex: number): Array<Value> {
    const type = this.funcType(funcIndex)
    const args: Array<Value> = new Array(type.params.length)
    for (let i = type.params.length - 1; i >= 0; i -= 1) args[i] = this.pop()
    return args
  }

  private callIndirectTarget(
    tableHandle: import('../Table.js').Table,
    typeHandle: import('../Type.js').Type,
  ): number {
    const table = this.table(tableHandle)
    const rawIndex = this.pop()
    const index = typeof rawIndex === 'bigint' ? Number(u64(rawIndex)) : u32(rawIndex as number)
    const element = table.elements[index]
    if (element === undefined) throw new Trap('undefined element')
    if (element._tag !== 'FuncRef') throw new Trap('uninitialized element')
    const expected = this.idx(typeHandle, 'Type')
    const entry = this.instance.snapshot.funcs[element.funcIndex]
    if (entry === undefined || entry.typeIndex !== expected) {
      throw new Trap('indirect call type mismatch')
    }
    return element.funcIndex
  }

  private callRefTarget(): number {
    const ref = this.popRef()
    if (ref._tag === 'NullRef') throw new Trap('null function reference')
    if (ref._tag !== 'FuncRef') throw new Trap('expected a function reference')
    return ref.funcIndex
  }

  /** Pops finished blocks and returned frames until a real instruction is current, or done. */
  private normalize(): void {
    while (this.frames.length > 0) {
      const frame = this.frame()
      const control = frame.control[frame.control.length - 1]
      if (control === undefined) throw new Trap('control stack underflow')
      if (control.pc < control.body.length) {
        this.status = { _tag: 'Paused', location: this.location() }
        return
      }
      if (control.kind === 'func') {
        this.returnFromFrame()
      } else {
        frame.control.pop()
      }
    }
  }

  location(): Location {
    const frame = this.frame()
    const control = frame.control[frame.control.length - 1]
    const instr = control?.body[control.pc]
    if (control === undefined || instr === undefined) throw new Trap('no current instruction')
    return {
      funcIndex: frame.funcIndex,
      funcName: this.instance.snapshot.funcs[frame.funcIndex]?.name,
      instr,
    }
  }

  /** The instruction about to execute, or undefined when not paused. */
  currentInstr(): Instr.Instr | undefined {
    return this.status._tag === 'Paused' ? this.status.location.instr : undefined
  }

  private handleThrown(error: unknown): void {
    if (error instanceof Trap) {
      this.status = { _tag: 'Trapped', message: error.message }
    } else {
      this.status = { _tag: 'Trapped', message: `internal interpreter failure: ${String(error)}` }
      throw error
    }
  }

  /** Executes exactly one instruction. Returns the resulting status. */
  step(): Status {
    if (this.status._tag !== 'Paused') return this.status
    try {
      const frame = this.frame()
      const control = frame.control[frame.control.length - 1]
      if (control === undefined) throw new Trap('control stack underflow')
      const instr = control.body[control.pc]
      if (instr === undefined) throw new Trap('no current instruction')
      control.pc += 1
      this.exec(instr)
      this.stepCount += 1
      this.normalize()
    } catch (error) {
      this.handleThrown(error)
    }
    return this.status
  }

  run(maxSteps: number, breakpoints?: ReadonlySet<object>): Status {
    let steps = 0
    while (this.status._tag === 'Paused') {
      if (steps >= maxSteps) return this.status
      this.step()
      steps += 1
      if (
        breakpoints !== undefined &&
        this.status._tag === 'Paused' &&
        breakpoints.has(this.status.location.instr)
      ) {
        return this.status
      }
    }
    return this.status
  }

  private exec(instr: Instr.Instr): void {
    switch (instr._tag) {
      case 'Op':
        this.execOp(instr.mnemonic)
        return
      case 'I32Const':
        this.push(instr.value | 0)
        return
      case 'I64Const':
        this.push(i64(instr.value))
        return
      case 'F32Const':
        this.push(Math.fround(instr.value))
        return
      case 'F64Const':
        this.push(instr.value)
        return
      case 'LocalGet': {
        const value = this.frame().locals[instr.local]
        if (value === undefined) throw new Trap('unknown local')
        this.push(value)
        return
      }
      case 'LocalSet':
        this.frame().locals[instr.local] = this.pop()
        return
      case 'LocalTee': {
        const value = this.pop()
        this.frame().locals[instr.local] = value
        this.push(value)
        return
      }
      case 'GlobalGet': {
        const global = this.instance.globals[this.idx(instr.global, 'Global')]
        if (global === undefined) throw new Trap('unknown global')
        this.push(global.value)
        return
      }
      case 'GlobalSet': {
        const global = this.instance.globals[this.idx(instr.global, 'Global')]
        if (global === undefined) throw new Trap('unknown global')
        if (!global.mutable) throw new Trap('global is immutable')
        global.value = this.pop()
        return
      }
      case 'Call': {
        const funcIndex = this.idx(instr.func, 'Func')
        this.callFunction(funcIndex, this.popArgs(funcIndex))
        return
      }
      case 'CallIndirect': {
        const funcIndex = this.callIndirectTarget(instr.table, instr.type)
        this.callFunction(funcIndex, this.popArgs(funcIndex))
        return
      }
      case 'CallRef': {
        const funcIndex = this.callRefTarget()
        this.callFunction(funcIndex, this.popArgs(funcIndex))
        return
      }
      case 'ReturnCall': {
        const funcIndex = this.idx(instr.func, 'Func')
        const args = this.popArgs(funcIndex)
        this.frames.pop()
        this.callFunction(funcIndex, args)
        return
      }
      case 'ReturnCallIndirect': {
        const funcIndex = this.callIndirectTarget(instr.table, instr.type)
        const args = this.popArgs(funcIndex)
        this.frames.pop()
        this.callFunction(funcIndex, args)
        return
      }
      case 'ReturnCallRef': {
        const funcIndex = this.callRefTarget()
        const args = this.popArgs(funcIndex)
        this.frames.pop()
        this.callFunction(funcIndex, args)
        return
      }
      case 'Block':
        this.enterBlock('block', instr.blockType, instr.body)
        return
      case 'Loop':
        this.enterBlock('loop', instr.blockType, instr.body)
        return
      case 'If': {
        const condition = this.popI32()
        this.enterBlock('block', instr.blockType, condition !== 0 ? instr.thenBody : instr.elseBody)
        return
      }
      case 'Br':
        this.branch(instr.depth)
        return
      case 'BrIf':
        if (this.popI32() !== 0) this.branch(instr.depth)
        return
      case 'BrTable': {
        const index = u32(this.popI32())
        this.branch(instr.depths[index] ?? instr.defaultDepth)
        return
      }
      case 'BrOnNull': {
        const ref = this.popRef()
        if (ref._tag === 'NullRef') {
          this.branch(instr.depth)
        } else {
          this.push(ref)
        }
        return
      }
      case 'BrOnNonNull': {
        const ref = this.popRef()
        if (ref._tag !== 'NullRef') {
          this.push(ref)
          this.branch(instr.depth)
        }
        return
      }
      case 'SelectTyped': {
        const condition = this.popI32()
        const onZero = this.pop()
        const otherwise = this.pop()
        this.push(condition !== 0 ? otherwise : onZero)
        return
      }
      case 'RefNull':
        this.push(nullRef)
        return
      case 'RefFunc':
        this.push(funcRef(this.idx(instr.func, 'Func')))
        return
      case 'MemoryAccess':
        this.execMemoryAccess(instr)
        return
      case 'MemorySize': {
        const memory = this.memory(instr.memory)
        const pages = Math.floor(memory.bytes.length / PAGE)
        this.push(memory.addressType === 'i64' ? BigInt(pages) : pages | 0)
        return
      }
      case 'MemoryGrow': {
        const memory = this.memory(instr.memory)
        const rawDelta = this.pop()
        const delta = typeof rawDelta === 'bigint' ? Number(u64(rawDelta)) : u32(rawDelta as number)
        const oldPages = Math.floor(memory.bytes.length / PAGE)
        const failed = memory.addressType === 'i64' ? -1n : -1
        if (oldPages + delta > memory.max) {
          this.push(failed)
          return
        }
        try {
          const grown = new Uint8Array((oldPages + delta) * PAGE)
          grown.set(memory.bytes)
          memory.bytes = grown
          memory.view = new DataView(grown.buffer)
        } catch {
          this.push(failed)
          return
        }
        this.push(memory.addressType === 'i64' ? BigInt(oldPages) : oldPages | 0)
        return
      }
      case 'MemoryInit': {
        const memory = this.memory(instr.memory)
        const data = this.instance.datas[this.idx(instr.data, 'Data')]
        if (data === undefined) throw new Trap('unknown data segment')
        const count = this.popAddress()
        const source = this.popAddress()
        const destination = this.popAddress()
        if (source + count > data.length || destination + count > memory.bytes.length) {
          throw new Trap('out of bounds memory access')
        }
        memory.bytes.set(data.subarray(source, source + count), destination)
        return
      }
      case 'DataDrop':
        this.instance.datas[this.idx(instr.data, 'Data')] = new Uint8Array(0)
        return
      case 'MemoryCopy': {
        const destinationMemory = this.memory(instr.destination)
        const sourceMemory = this.memory(instr.source)
        const count = this.popAddress()
        const source = this.popAddress()
        const destination = this.popAddress()
        if (
          source + count > sourceMemory.bytes.length ||
          destination + count > destinationMemory.bytes.length
        ) {
          throw new Trap('out of bounds memory access')
        }
        destinationMemory.bytes.set(
          sourceMemory.bytes.subarray(source, source + count),
          destination,
        )
        return
      }
      case 'MemoryFill': {
        const memory = this.memory(instr.memory)
        const count = this.popAddress()
        const value = this.popI32() & 0xff
        const destination = this.popAddress()
        if (destination + count > memory.bytes.length) throw new Trap('out of bounds memory access')
        memory.bytes.fill(value, destination, destination + count)
        return
      }
      case 'TableGet': {
        const table = this.table(instr.table)
        const index = this.popAddress()
        const element = table.elements[index]
        if (element === undefined) throw new Trap('out of bounds table access')
        this.push(element)
        return
      }
      case 'TableSet': {
        const table = this.table(instr.table)
        const value = this.popRef()
        const index = this.popAddress()
        if (index >= table.elements.length) throw new Trap('out of bounds table access')
        table.elements[index] = value
        return
      }
      case 'TableSize': {
        const table = this.table(instr.table)
        this.push(
          table.addressType === 'i64' ? BigInt(table.elements.length) : table.elements.length | 0,
        )
        return
      }
      case 'TableGrow': {
        const table = this.table(instr.table)
        const rawDelta = this.pop()
        const delta = typeof rawDelta === 'bigint' ? Number(u64(rawDelta)) : u32(rawDelta as number)
        const value = this.popRef()
        const oldSize = table.elements.length
        const failed = table.addressType === 'i64' ? -1n : -1
        if (oldSize + delta > table.max) {
          this.push(failed)
          return
        }
        for (let i = 0; i < delta; i += 1) table.elements.push(value)
        this.push(table.addressType === 'i64' ? BigInt(oldSize) : oldSize | 0)
        return
      }
      case 'TableFill': {
        const table = this.table(instr.table)
        const count = this.popAddress()
        const value = this.popRef()
        const start = this.popAddress()
        if (start + count > table.elements.length) throw new Trap('out of bounds table access')
        for (let i = 0; i < count; i += 1) table.elements[start + i] = value
        return
      }
      case 'TableCopy': {
        const destinationTable = this.table(instr.destination)
        const sourceTable = this.table(instr.source)
        const count = this.popAddress()
        const source = this.popAddress()
        const destination = this.popAddress()
        if (
          source + count > sourceTable.elements.length ||
          destination + count > destinationTable.elements.length
        ) {
          throw new Trap('out of bounds table access')
        }
        const slice = sourceTable.elements.slice(source, source + count)
        for (const [i, element] of slice.entries()) {
          destinationTable.elements[destination + i] = element
        }
        return
      }
      case 'TableInit': {
        const table = this.table(instr.table)
        const elem = this.instance.elems[this.idx(instr.elem, 'Elem')]
        if (elem === undefined) throw new Trap('unknown element segment')
        const count = this.popAddress()
        const source = this.popAddress()
        const destination = this.popAddress()
        if (source + count > elem.length || destination + count > table.elements.length) {
          throw new Trap('out of bounds table access')
        }
        for (let i = 0; i < count; i += 1) {
          const element = elem[source + i]
          if (element !== undefined) table.elements[destination + i] = element
        }
        return
      }
      case 'ElemDrop':
        this.instance.elems[this.idx(instr.elem, 'Elem')] = []
        return
      default:
        throw new Trap(`unsupported instruction ${instr._tag}`)
    }
  }

  private execMemoryAccess(instr: Extract<Instr.Instr, { readonly _tag: 'MemoryAccess' }>): void {
    const memory = this.memory(instr.memory)
    const width = 1 << InstructionTable.memoryAccessOps[instr.mnemonic].widthLog2
    switch (instr.mnemonic) {
      case 'i32.store':
      case 'i64.store':
      case 'f32.store':
      case 'f64.store':
      case 'i32.store8':
      case 'i32.store16':
      case 'i64.store8':
      case 'i64.store16':
      case 'i64.store32': {
        const value = this.pop()
        const address = this.effectiveAddress(memory, instr.offset, width)
        const view = memory.view
        switch (instr.mnemonic) {
          case 'i32.store':
            view.setInt32(address, value as number, true)
            return
          case 'i64.store':
            view.setBigInt64(address, value as bigint, true)
            return
          case 'f32.store':
            view.setFloat32(address, value as number, true)
            return
          case 'f64.store':
            view.setFloat64(address, value as number, true)
            return
          case 'i32.store8':
            view.setUint8(address, (value as number) & 0xff)
            return
          case 'i32.store16':
            view.setUint16(address, (value as number) & 0xffff, true)
            return
          case 'i64.store8':
            view.setUint8(address, Number(u64(value as bigint) & 0xffn))
            return
          case 'i64.store16':
            view.setUint16(address, Number(u64(value as bigint) & 0xffffn), true)
            return
          case 'i64.store32':
            view.setUint32(address, Number(u64(value as bigint) & 0xffffffffn), true)
            return
        }
        return
      }
      default: {
        const address = this.effectiveAddress(memory, instr.offset, width)
        const view = memory.view
        switch (instr.mnemonic) {
          case 'i32.load':
            this.push(view.getInt32(address, true))
            return
          case 'i64.load':
            this.push(view.getBigInt64(address, true))
            return
          case 'f32.load':
            this.push(view.getFloat32(address, true))
            return
          case 'f64.load':
            this.push(view.getFloat64(address, true))
            return
          case 'i32.load8_s':
            this.push(view.getInt8(address))
            return
          case 'i32.load8_u':
            this.push(view.getUint8(address))
            return
          case 'i32.load16_s':
            this.push(view.getInt16(address, true))
            return
          case 'i32.load16_u':
            this.push(view.getUint16(address, true))
            return
          case 'i64.load8_s':
            this.push(BigInt(view.getInt8(address)))
            return
          case 'i64.load8_u':
            this.push(BigInt(view.getUint8(address)))
            return
          case 'i64.load16_s':
            this.push(BigInt(view.getInt16(address, true)))
            return
          case 'i64.load16_u':
            this.push(BigInt(view.getUint16(address, true)))
            return
          case 'i64.load32_s':
            this.push(BigInt(view.getInt32(address, true)))
            return
          case 'i64.load32_u':
            this.push(BigInt(view.getUint32(address, true)))
            return
        }
      }
    }
  }

  private execOp(mnemonic: Instr.PlainMnemonic): void {
    switch (mnemonic) {
      case 'unreachable':
        throw new Trap('unreachable executed')
      case 'nop':
      case 'atomic.fence':
        return
      case 'return':
        this.returnFromFrame()
        return
      case 'drop':
        this.pop()
        return
      case 'select': {
        const condition = this.popI32()
        const onZero = this.pop()
        const otherwise = this.pop()
        this.push(condition !== 0 ? otherwise : onZero)
        return
      }
      case 'ref.is_null':
        this.push(this.popRef()._tag === 'NullRef' ? 1 : 0)
        return
      case 'ref.as_non_null': {
        const ref = this.popRef()
        if (ref._tag === 'NullRef') throw new Trap('null reference')
        this.push(ref)
        return
      }

      // i32 tests and comparisons
      case 'i32.eqz':
        this.push(this.popI32() === 0 ? 1 : 0)
        return
      case 'i32.eq': {
        const b = this.popI32()
        this.push(this.popI32() === b ? 1 : 0)
        return
      }
      case 'i32.ne': {
        const b = this.popI32()
        this.push(this.popI32() !== b ? 1 : 0)
        return
      }
      case 'i32.lt_s': {
        const b = this.popI32()
        this.push(this.popI32() < b ? 1 : 0)
        return
      }
      case 'i32.lt_u': {
        const b = u32(this.popI32())
        this.push(u32(this.popI32()) < b ? 1 : 0)
        return
      }
      case 'i32.gt_s': {
        const b = this.popI32()
        this.push(this.popI32() > b ? 1 : 0)
        return
      }
      case 'i32.gt_u': {
        const b = u32(this.popI32())
        this.push(u32(this.popI32()) > b ? 1 : 0)
        return
      }
      case 'i32.le_s': {
        const b = this.popI32()
        this.push(this.popI32() <= b ? 1 : 0)
        return
      }
      case 'i32.le_u': {
        const b = u32(this.popI32())
        this.push(u32(this.popI32()) <= b ? 1 : 0)
        return
      }
      case 'i32.ge_s': {
        const b = this.popI32()
        this.push(this.popI32() >= b ? 1 : 0)
        return
      }
      case 'i32.ge_u': {
        const b = u32(this.popI32())
        this.push(u32(this.popI32()) >= b ? 1 : 0)
        return
      }

      // i64 tests and comparisons
      case 'i64.eqz':
        this.push(this.popI64() === 0n ? 1 : 0)
        return
      case 'i64.eq': {
        const b = this.popI64()
        this.push(this.popI64() === b ? 1 : 0)
        return
      }
      case 'i64.ne': {
        const b = this.popI64()
        this.push(this.popI64() !== b ? 1 : 0)
        return
      }
      case 'i64.lt_s': {
        const b = this.popI64()
        this.push(this.popI64() < b ? 1 : 0)
        return
      }
      case 'i64.lt_u': {
        const b = u64(this.popI64())
        this.push(u64(this.popI64()) < b ? 1 : 0)
        return
      }
      case 'i64.gt_s': {
        const b = this.popI64()
        this.push(this.popI64() > b ? 1 : 0)
        return
      }
      case 'i64.gt_u': {
        const b = u64(this.popI64())
        this.push(u64(this.popI64()) > b ? 1 : 0)
        return
      }
      case 'i64.le_s': {
        const b = this.popI64()
        this.push(this.popI64() <= b ? 1 : 0)
        return
      }
      case 'i64.le_u': {
        const b = u64(this.popI64())
        this.push(u64(this.popI64()) <= b ? 1 : 0)
        return
      }
      case 'i64.ge_s': {
        const b = this.popI64()
        this.push(this.popI64() >= b ? 1 : 0)
        return
      }
      case 'i64.ge_u': {
        const b = u64(this.popI64())
        this.push(u64(this.popI64()) >= b ? 1 : 0)
        return
      }

      // float comparisons
      case 'f32.eq':
      case 'f64.eq':
        {
          const b = this.popF()
          this.push(this.popF() === b ? 1 : 0)
        }
        return
      case 'f32.ne':
      case 'f64.ne': {
        const b = this.popF()
        const a = this.popF()
        this.push(a === b ? 0 : 1)
        return
      }
      case 'f32.lt':
      case 'f64.lt': {
        const b = this.popF()
        this.push(this.popF() < b ? 1 : 0)
        return
      }
      case 'f32.gt':
      case 'f64.gt': {
        const b = this.popF()
        this.push(this.popF() > b ? 1 : 0)
        return
      }
      case 'f32.le':
      case 'f64.le': {
        const b = this.popF()
        this.push(this.popF() <= b ? 1 : 0)
        return
      }
      case 'f32.ge':
      case 'f64.ge': {
        const b = this.popF()
        this.push(this.popF() >= b ? 1 : 0)
        return
      }

      // i32 arithmetic
      case 'i32.clz':
        this.push(Math.clz32(u32(this.popI32())))
        return
      case 'i32.ctz':
        this.push(ctz32(this.popI32()))
        return
      case 'i32.popcnt':
        this.push(popcnt32(this.popI32()))
        return
      case 'i32.add': {
        const b = this.popI32()
        this.push((this.popI32() + b) | 0)
        return
      }
      case 'i32.sub': {
        const b = this.popI32()
        this.push((this.popI32() - b) | 0)
        return
      }
      case 'i32.mul': {
        const b = this.popI32()
        this.push(Math.imul(this.popI32(), b))
        return
      }
      case 'i32.div_s': {
        const b = this.popI32()
        this.push(divS32(this.popI32(), b))
        return
      }
      case 'i32.div_u': {
        const b = this.popI32()
        this.push(divU32(this.popI32(), b))
        return
      }
      case 'i32.rem_s': {
        const b = this.popI32()
        this.push(remS32(this.popI32(), b))
        return
      }
      case 'i32.rem_u': {
        const b = this.popI32()
        this.push(remU32(this.popI32(), b))
        return
      }
      case 'i32.and': {
        const b = this.popI32()
        this.push(this.popI32() & b)
        return
      }
      case 'i32.or': {
        const b = this.popI32()
        this.push(this.popI32() | b)
        return
      }
      case 'i32.xor': {
        const b = this.popI32()
        this.push(this.popI32() ^ b)
        return
      }
      case 'i32.shl': {
        const b = this.popI32()
        this.push((this.popI32() << (b & 31)) | 0)
        return
      }
      case 'i32.shr_s': {
        const b = this.popI32()
        this.push(this.popI32() >> (b & 31))
        return
      }
      case 'i32.shr_u': {
        const b = this.popI32()
        this.push((u32(this.popI32()) >>> (b & 31)) | 0)
        return
      }
      case 'i32.rotl': {
        const b = this.popI32()
        this.push(rotl32(this.popI32(), b))
        return
      }
      case 'i32.rotr': {
        const b = this.popI32()
        this.push(rotr32(this.popI32(), b))
        return
      }

      // i64 arithmetic
      case 'i64.clz':
        this.push(clz64(this.popI64()))
        return
      case 'i64.ctz':
        this.push(ctz64(this.popI64()))
        return
      case 'i64.popcnt':
        this.push(popcnt64(this.popI64()))
        return
      case 'i64.add': {
        const b = this.popI64()
        this.push(i64(this.popI64() + b))
        return
      }
      case 'i64.sub': {
        const b = this.popI64()
        this.push(i64(this.popI64() - b))
        return
      }
      case 'i64.mul': {
        const b = this.popI64()
        this.push(i64(this.popI64() * b))
        return
      }
      case 'i64.div_s': {
        const b = this.popI64()
        this.push(divS64(this.popI64(), b))
        return
      }
      case 'i64.div_u': {
        const b = this.popI64()
        this.push(divU64(this.popI64(), b))
        return
      }
      case 'i64.rem_s': {
        const b = this.popI64()
        this.push(remS64(this.popI64(), b))
        return
      }
      case 'i64.rem_u': {
        const b = this.popI64()
        this.push(remU64(this.popI64(), b))
        return
      }
      case 'i64.and': {
        const b = this.popI64()
        this.push(this.popI64() & b)
        return
      }
      case 'i64.or': {
        const b = this.popI64()
        this.push(this.popI64() | b)
        return
      }
      case 'i64.xor': {
        const b = this.popI64()
        this.push(this.popI64() ^ b)
        return
      }
      case 'i64.shl': {
        const b = u64(this.popI64()) & 63n
        this.push(i64(this.popI64() << b))
        return
      }
      case 'i64.shr_s': {
        const b = u64(this.popI64()) & 63n
        this.push(this.popI64() >> b)
        return
      }
      case 'i64.shr_u': {
        const b = u64(this.popI64()) & 63n
        this.push(i64(u64(this.popI64()) >> b))
        return
      }
      case 'i64.rotl': {
        const b = this.popI64()
        this.push(rotl64(this.popI64(), b))
        return
      }
      case 'i64.rotr': {
        const b = this.popI64()
        this.push(rotr64(this.popI64(), b))
        return
      }

      // f32 arithmetic
      case 'f32.abs':
        this.push(Math.fround(Math.abs(this.popF())))
        return
      case 'f32.neg':
        this.push(Math.fround(-this.popF()))
        return
      case 'f32.ceil':
        this.push(Math.fround(Math.ceil(this.popF())))
        return
      case 'f32.floor':
        this.push(Math.fround(Math.floor(this.popF())))
        return
      case 'f32.trunc':
        this.push(Math.fround(Math.trunc(this.popF())))
        return
      case 'f32.nearest':
        this.push(Math.fround(nearest(this.popF())))
        return
      case 'f32.sqrt':
        this.push(Math.fround(Math.sqrt(this.popF())))
        return
      case 'f32.add': {
        const b = this.popF()
        this.push(Math.fround(this.popF() + b))
        return
      }
      case 'f32.sub': {
        const b = this.popF()
        this.push(Math.fround(this.popF() - b))
        return
      }
      case 'f32.mul': {
        const b = this.popF()
        this.push(Math.fround(this.popF() * b))
        return
      }
      case 'f32.div': {
        const b = this.popF()
        this.push(Math.fround(this.popF() / b))
        return
      }
      case 'f32.min': {
        const b = this.popF()
        this.push(Math.fround(fmin(this.popF(), b)))
        return
      }
      case 'f32.max': {
        const b = this.popF()
        this.push(Math.fround(fmax(this.popF(), b)))
        return
      }
      case 'f32.copysign': {
        const b = this.popF()
        this.push(Math.fround(copysign(this.popF(), b)))
        return
      }

      // f64 arithmetic
      case 'f64.abs':
        this.push(Math.abs(this.popF()))
        return
      case 'f64.neg':
        this.push(-this.popF())
        return
      case 'f64.ceil':
        this.push(Math.ceil(this.popF()))
        return
      case 'f64.floor':
        this.push(Math.floor(this.popF()))
        return
      case 'f64.trunc':
        this.push(Math.trunc(this.popF()))
        return
      case 'f64.nearest':
        this.push(nearest(this.popF()))
        return
      case 'f64.sqrt':
        this.push(Math.sqrt(this.popF()))
        return
      case 'f64.add': {
        const b = this.popF()
        this.push(this.popF() + b)
        return
      }
      case 'f64.sub': {
        const b = this.popF()
        this.push(this.popF() - b)
        return
      }
      case 'f64.mul': {
        const b = this.popF()
        this.push(this.popF() * b)
        return
      }
      case 'f64.div': {
        const b = this.popF()
        this.push(this.popF() / b)
        return
      }
      case 'f64.min': {
        const b = this.popF()
        this.push(fmin(this.popF(), b))
        return
      }
      case 'f64.max': {
        const b = this.popF()
        this.push(fmax(this.popF(), b))
        return
      }
      case 'f64.copysign': {
        const b = this.popF()
        this.push(copysign(this.popF(), b))
        return
      }

      // conversions
      case 'i32.wrap_i64':
        this.push(Number(BigInt.asIntN(32, this.popI64())))
        return
      case 'i32.trunc_f32_s':
      case 'i32.trunc_f64_s':
        this.push(truncChecked(this.popF(), -2147483648, 2147483647) | 0)
        return
      case 'i32.trunc_f32_u':
      case 'i32.trunc_f64_u':
        this.push(truncChecked(this.popF(), 0, 4294967295) | 0)
        return
      case 'i64.extend_i32_s':
        this.push(BigInt(this.popI32()))
        return
      case 'i64.extend_i32_u':
        this.push(BigInt(u32(this.popI32())))
        return
      case 'i64.trunc_f32_s':
      case 'i64.trunc_f64_s':
        this.push(truncChecked64(this.popF(), false))
        return
      case 'i64.trunc_f32_u':
      case 'i64.trunc_f64_u':
        this.push(truncChecked64(this.popF(), true))
        return
      case 'f32.convert_i32_s':
        this.push(Math.fround(this.popI32()))
        return
      case 'f32.convert_i32_u':
        this.push(Math.fround(u32(this.popI32())))
        return
      case 'f32.convert_i64_s':
        this.push(Math.fround(Number(this.popI64())))
        return
      case 'f32.convert_i64_u':
        this.push(Math.fround(Number(u64(this.popI64()))))
        return
      case 'f32.demote_f64':
        this.push(Math.fround(this.popF()))
        return
      case 'f64.convert_i32_s':
        this.push(this.popI32())
        return
      case 'f64.convert_i32_u':
        this.push(u32(this.popI32()))
        return
      case 'f64.convert_i64_s':
        this.push(Number(this.popI64()))
        return
      case 'f64.convert_i64_u':
        this.push(Number(u64(this.popI64())))
        return
      case 'f64.promote_f32':
        this.push(this.popF())
        return
      case 'i32.reinterpret_f32':
        this.push(reinterpretF32ToI32(this.popF()))
        return
      case 'i64.reinterpret_f64':
        this.push(reinterpretF64ToI64(this.popF()))
        return
      case 'f32.reinterpret_i32':
        this.push(reinterpretI32ToF32(this.popI32()))
        return
      case 'f64.reinterpret_i64':
        this.push(reinterpretI64ToF64(this.popI64()))
        return
      case 'i32.extend8_s':
        this.push((this.popI32() << 24) >> 24)
        return
      case 'i32.extend16_s':
        this.push((this.popI32() << 16) >> 16)
        return
      case 'i64.extend8_s':
        this.push(BigInt.asIntN(8, this.popI64()))
        return
      case 'i64.extend16_s':
        this.push(BigInt.asIntN(16, this.popI64()))
        return
      case 'i64.extend32_s':
        this.push(BigInt.asIntN(32, this.popI64()))
        return
      case 'i32.trunc_sat_f32_s':
      case 'i32.trunc_sat_f64_s':
        this.push(truncSat(this.popF(), -2147483648, 2147483647) | 0)
        return
      case 'i32.trunc_sat_f32_u':
      case 'i32.trunc_sat_f64_u':
        this.push(truncSat(this.popF(), 0, 4294967295) | 0)
        return
      case 'i64.trunc_sat_f32_s':
      case 'i64.trunc_sat_f64_s':
        this.push(truncSat64(this.popF(), false))
        return
      case 'i64.trunc_sat_f32_u':
      case 'i64.trunc_sat_f64_u':
        this.push(truncSat64(this.popF(), true))
        return

      default:
        throw new Trap(`unsupported instruction ${mnemonic}`)
    }
  }
}
