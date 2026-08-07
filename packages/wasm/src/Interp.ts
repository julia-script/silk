/**
 * A direct WebAssembly interpreter with first-class debugging.
 *
 * The interpreter executes the builder's committed `Instr` trees directly — no binary encoding,
 * no lowering, no `WebAssembly.instantiate` — with an explicit call stack and per-block program
 * counters. Because wasm control flow consumes no JavaScript call stack, execution can pause
 * between any two instructions, which makes the interpreter usable as the engine of a browser
 * debugger: step, step over, step out, breakpoints on instruction values, and inspection of
 * frames, locals, value stacks, globals, and memory.
 *
 * Instantiation and whole-call execution are Effect operations like the rest of the package.
 * The debug-session operations (`step`, `resume`, inspection) are deliberately synchronous plain
 * functions: a debugger UI drives them in a tight interactive loop, and they neither perform
 * effects nor fail — a trap is an ordinary `Status` outcome, not an error channel.
 *
 * Supported: the full scalar instruction set — numeric, parametric, control flow, calls
 * (including tail calls, `call_indirect`, and `call_ref`), memory, tables, active and passive
 * segments, and untyped/function references. Unsupported (these trap with an `unsupported`
 * message): SIMD, atomics other than `atomic.fence`, GC struct/array/i31 instructions, and
 * exception handling. Imported memories and tables are instantiated fresh from their declared
 * limits rather than shared with the host.
 *
 * @since 0.0.0
 */
import * as Effect from 'effect/Effect'
import type * as Builder from './Builder.js'
import * as Machine from './internal/Machine.js'
import * as ModuleState from './internal/ModuleState.js'
import { invalidInput, type WasmError, wrappedFailure } from './WasmError.js'

/**
 * A runtime value crossing the interpreter boundary: `i32`/`f32`/`f64` as `number`, `i64` as
 * `bigint`, references as {@link RefValue}.
 *
 * @category interpreter
 * @since 0.0.0
 */
export type Value = Machine.Value

/**
 * An opaque runtime reference value: null, a function reference, or a host reference.
 *
 * @category interpreter
 * @since 0.0.0
 */
export type RefValue = Machine.RefValue

/**
 * A host function backing an imported function. Receives the popped arguments and returns the
 * results (an array, a single value, or nothing for a result-less type). Throwing traps the
 * calling machine.
 *
 * @category interpreter
 * @since 0.0.0
 */
export type HostFunc = Machine.HostFunc

/**
 * Import values keyed by module and field name: host functions for imported functions, plain
 * values for imported globals.
 *
 * @category interpreter
 * @since 0.0.0
 */
export type Imports = Machine.Imports

/**
 * Where a paused session stands: the function and the exact instruction value about to execute.
 *
 * The `instr` is the same immutable `Instr` value that was committed with `Func.define`, so a
 * debugger UI that kept its own map from source positions to instruction values can translate
 * locations by identity.
 *
 * @category debugging
 * @since 0.0.0
 */
export type Location = Machine.Location

/**
 * The outcome of a step or resume: still paused at a location, finished with results, or
 * trapped with a message.
 *
 * @category debugging
 * @since 0.0.0
 */
export type Status = Machine.Status

/**
 * An instantiated module: evaluated globals, allocated memories and tables, applied segments,
 * and a completed start function.
 *
 * @category interpreter
 * @since 0.0.0
 */
export interface Instance {
  readonly _tag: 'InterpInstance'
}

const instanceStates = new WeakMap<Instance, Machine.InstanceState>()

const stateOf = (instance: Instance): Machine.InstanceState => {
  const state = instanceStates.get(instance)
  if (state === undefined) throw new Error('Unknown Interp.Instance value')
  return state
}

/**
 * A paused, resumable call — the unit a debugger drives.
 *
 * @category debugging
 * @since 0.0.0
 */
export interface Session {
  readonly _tag: 'InterpSession'
}

interface SessionState {
  readonly machine: Machine.Machine
  readonly breakpoints: Set<object>
}

const sessionStates = new WeakMap<Session, SessionState>()

const sessionOf = (session: Session): SessionState => {
  const state = sessionStates.get(session)
  if (state === undefined) throw new Error('Unknown Interp.Session value')
  return state
}

const machineOf = (session: Session): Machine.Machine => sessionOf(session).machine

const breakpointsOf = (session: Session): Set<object> => sessionOf(session).breakpoints

/**
 * Options accepted by {@link instantiate}.
 *
 * @category interpreter
 * @since 0.0.0
 */
export interface InstantiateOptions {
  /** Host values for imported functions and globals, keyed by module and field name. */
  readonly imports?: Imports
}

/**
 * Instantiates the builder's committed module state for direct interpretation.
 *
 * **Details**
 *
 * Follows the specification's instantiation order: imports are linked, global initializers are
 * evaluated, memories and tables are allocated at their minimum sizes, active data and element
 * segments are bounds-checked and applied, and the start function (if any) runs to completion.
 * The builder is not consumed; instantiating again produces an independent instance, and the
 * builder can still declare more state or be emitted.
 *
 * **Gotchas**
 *
 * Every imported function needs a host function and every imported global needs a host value in
 * `options.imports`, or instantiation fails. Imported memories and tables are created fresh from
 * their declared limits — they are not shared with the host.
 *
 * **Example** (Instantiating and calling an exported function)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/wasm/Builder'
 * import * as ExportActor from '@silk-effect/wasm/Export'
 * import * as Func from '@silk-effect/wasm/Func'
 * import * as Instr from '@silk-effect/wasm/Instr'
 * import * as Interp from '@silk-effect/wasm/Interp'
 * import * as Type from '@silk-effect/wasm/Type'
 * import * as ValType from '@silk-effect/wasm/ValType'
 *
 * const results = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const signature = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
 *   const add = yield* Func.declare(builder, signature, { name: 'add' })
 *   yield* Func.define(builder, add, {
 *     body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')],
 *   })
 *   yield* ExportActor.func(builder, 'add', add)
 *   const instance = yield* Interp.instantiate(builder)
 *   return yield* Interp.invoke(instance, 'add', [2, 40])
 * })
 * ```
 *
 * @category interpreter
 * @since 0.0.0
 */
export const instantiate = Effect.fn('Interp.instantiate')(function* (
  builder: Builder.Builder,
  options: InstantiateOptions = {},
): Effect.fn.Return<Instance, WasmError> {
  const snapshot = yield* ModuleState.snapshot(builder, 'Interp.instantiate')
  return yield* Effect.try({
    try: (): Instance => {
      const state = Machine.instantiate(snapshot, options.imports ?? {})
      const instance: Instance = Object.freeze({ _tag: 'InterpInstance' })
      instanceStates.set(instance, state)
      return instance
    },
    catch: (error) =>
      error instanceof Machine.LinkError
        ? invalidInput({
            operation: 'Interp.instantiate',
            message: error.message,
            input: options.imports ?? {},
          })
        : wrappedFailure({
            operation: 'Interp.instantiate',
            message:
              error instanceof Machine.Trap
                ? `WebAssembly trap during instantiation: ${error.message}`
                : 'Instantiation failed',
            cause: error,
          }),
  })
})

const exportedFuncIndex = (
  state: Machine.InstanceState,
  name: string,
  operation: string,
): Effect.Effect<number, WasmError> => {
  const entry = state.snapshot.exports.find(
    (candidate) => candidate.name === name && candidate.kind === 'func',
  )
  return entry === undefined
    ? Effect.fail(
        invalidInput({ operation, message: `No exported function named "${name}"`, input: name }),
      )
    : Effect.succeed(entry.entryIndex)
}

/** Validates call arguments against the function's declared parameter types. */
const coerceArgs = (
  state: Machine.InstanceState,
  funcIndex: number,
  args: ReadonlyArray<Value>,
  operation: string,
): Effect.Effect<ReadonlyArray<Value>, WasmError> => {
  const entry = state.snapshot.funcs[funcIndex]
  const type =
    entry === undefined ? undefined : ModuleState.funcTypeAt(state.snapshot.types, entry.typeIndex)
  if (type === undefined) {
    return Effect.fail(invalidInput({ operation, message: 'Unknown function', input: funcIndex }))
  }
  if (args.length !== type.params.length) {
    return Effect.fail(
      invalidInput({
        operation,
        message: `The function expects ${type.params.length} argument(s), got ${args.length}`,
        input: args,
      }),
    )
  }
  return Effect.try({
    try: () =>
      type.params.map((param, index) =>
        Machine.coerceValue(param, args[index] as Value, `argument ${index}`),
      ),
    catch: (error) =>
      invalidInput({
        operation,
        message: error instanceof Machine.Trap ? error.message : String(error),
        input: args,
      }),
  })
}

const startMachine = (
  state: Machine.InstanceState,
  funcIndex: number,
  args: ReadonlyArray<Value>,
  operation: string,
): Effect.Effect<Machine.Machine, WasmError> =>
  Effect.try({
    try: () => new Machine.Machine(state, funcIndex, args),
    catch: (error) =>
      wrappedFailure({ operation, message: 'Failed to start the interpreter', cause: error }),
  })

/**
 * Options accepted by {@link invoke}.
 *
 * @category interpreter
 * @since 0.0.0
 */
export interface InvokeOptions {
  /** Upper bound on executed instructions; exceeding it fails. Defaults to unlimited. */
  readonly maxSteps?: number
}

/**
 * Calls an exported function and runs it to completion.
 *
 * **Details**
 *
 * A trap — division by zero, out-of-bounds access, `unreachable`, an indirect call type
 * mismatch — fails the effect with a `WasmError` whose message carries the trap reason. Use
 * {@link debug} instead when you want to observe execution rather than just its result.
 *
 * @category interpreter
 * @since 0.0.0
 */
export const invoke = Effect.fn('Interp.invoke')(function* (
  instance: Instance,
  name: string,
  args: ReadonlyArray<Value> = [],
  options: InvokeOptions = {},
): Effect.fn.Return<ReadonlyArray<Value>, WasmError> {
  const state = instanceStates.get(instance)
  if (state === undefined) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Interp.invoke',
        message: 'Unknown Interp.Instance value',
        input: instance,
      }),
    )
  }
  const funcIndex = yield* exportedFuncIndex(state, name, 'Interp.invoke')
  const coerced = yield* coerceArgs(state, funcIndex, args, 'Interp.invoke')
  const machine = yield* startMachine(state, funcIndex, coerced, 'Interp.invoke')
  const status = machine.run(options.maxSteps ?? Number.POSITIVE_INFINITY)
  switch (status._tag) {
    case 'Done':
      return status.results
    case 'Trapped':
      return yield* Effect.fail(
        wrappedFailure({
          operation: 'Interp.invoke',
          message: `WebAssembly trap: ${status.message}`,
          cause: status,
        }),
      )
    case 'Paused':
      return yield* Effect.fail(
        invalidInput({
          operation: 'Interp.invoke',
          message: `Execution exceeded ${options.maxSteps} steps`,
          input: options.maxSteps,
        }),
      )
  }
})

/**
 * Starts a debug session: a call to an exported function paused before its first instruction.
 *
 * **When to use**
 *
 * Use as the engine of an interactive debugger. Drive the returned session with the synchronous
 * operations — {@link step}, {@link stepOver}, {@link stepOut}, {@link resume} — and read it
 * with {@link status}, {@link callStack}, {@link locals}, and {@link valueStack}.
 *
 * **Gotchas**
 *
 * A session paused inside a host-function call does not exist: host functions execute atomically
 * within one step. Calling `debug` on an export that is itself an imported function completes in
 * the first step.
 *
 * **Example** (Stepping to a breakpoint)
 *
 * ```ts
 * import * as Effect from 'effect/Effect'
 * import * as Builder from '@silk-effect/wasm/Builder'
 * import * as ExportActor from '@silk-effect/wasm/Export'
 * import * as Func from '@silk-effect/wasm/Func'
 * import * as Instr from '@silk-effect/wasm/Instr'
 * import * as Interp from '@silk-effect/wasm/Interp'
 * import * as Type from '@silk-effect/wasm/Type'
 * import * as ValType from '@silk-effect/wasm/ValType'
 *
 * const paused = Effect.gen(function* () {
 *   const builder = yield* Builder.make()
 *   const signature = yield* Type.func(builder, [ValType.i32], [ValType.i32])
 *   const double = yield* Func.declare(builder, signature, { name: 'double' })
 *   const add = Instr.op('i32.add')
 *   yield* Func.define(builder, double, {
 *     body: [Instr.localGet(0), Instr.localGet(0), add],
 *   })
 *   yield* ExportActor.func(builder, 'double', double)
 *   const instance = yield* Interp.instantiate(builder)
 *   const session = yield* Interp.debug(instance, 'double', [21])
 *   Interp.setBreakpoint(session, add)
 *   return Interp.resume(session)
 * })
 * ```
 *
 * @category debugging
 * @since 0.0.0
 */
export const debug = Effect.fn('Interp.debug')(function* (
  instance: Instance,
  name: string,
  args: ReadonlyArray<Value> = [],
): Effect.fn.Return<Session, WasmError> {
  const state = instanceStates.get(instance)
  if (state === undefined) {
    return yield* Effect.fail(
      invalidInput({
        operation: 'Interp.debug',
        message: 'Unknown Interp.Instance value',
        input: instance,
      }),
    )
  }
  const funcIndex = yield* exportedFuncIndex(state, name, 'Interp.debug')
  const coerced = yield* coerceArgs(state, funcIndex, args, 'Interp.debug')
  const machine = yield* startMachine(state, funcIndex, coerced, 'Interp.debug')
  const session: Session = Object.freeze({ _tag: 'InterpSession' })
  sessionStates.set(session, { machine, breakpoints: new Set() })
  return session
})

/**
 * The session's current status without advancing it.
 *
 * @category debugging
 * @since 0.0.0
 */
export const status = (session: Session): Status => machineOf(session).status

/**
 * Executes exactly one instruction, stepping into calls.
 *
 * @category debugging
 * @since 0.0.0
 */
export const step = (session: Session): Status => machineOf(session).step()

/**
 * Executes one instruction, running any calls it makes to completion.
 *
 * @category debugging
 * @since 0.0.0
 */
export const stepOver = (session: Session): Status => {
  const machine = machineOf(session)
  const depth = machine.frames.length
  machine.step()
  while (machine.status._tag === 'Paused' && machine.frames.length > depth) {
    if (breakpointsOf(session).has(machine.status.location.instr)) return machine.status
    machine.step()
  }
  return machine.status
}

/**
 * Runs until the current function returns to its caller.
 *
 * @category debugging
 * @since 0.0.0
 */
export const stepOut = (session: Session): Status => {
  const machine = machineOf(session)
  const depth = machine.frames.length
  machine.step()
  while (machine.status._tag === 'Paused' && machine.frames.length >= depth) {
    if (breakpointsOf(session).has(machine.status.location.instr)) return machine.status
    machine.step()
  }
  return machine.status
}

/**
 * Options accepted by {@link resume}.
 *
 * @category debugging
 * @since 0.0.0
 */
export interface ResumeOptions {
  /**
   * Upper bound on executed instructions before returning with a still-paused status. Use to
   * keep a browser UI responsive by resuming in slices. Defaults to unlimited.
   */
  readonly maxSteps?: number
}

/**
 * Runs until a breakpoint, completion, a trap, or the step budget.
 *
 * **Details**
 *
 * Always executes at least one instruction, so resuming while paused on a breakpoint moves past
 * it instead of retriggering immediately.
 *
 * @category debugging
 * @since 0.0.0
 */
export const resume = (session: Session, options: ResumeOptions = {}): Status => {
  const machine = machineOf(session)
  return machine.run(options.maxSteps ?? Number.POSITIVE_INFINITY, breakpointsOf(session))
}

/**
 * Sets a breakpoint on an instruction value.
 *
 * **Details**
 *
 * Breakpoints match by value identity: execution pauses whenever this exact `Instr` value is
 * about to execute, in any function that contains it. Since each `Instr` constructor call
 * produces a distinct value, a compiler that keeps its emitted instructions can map source
 * positions to breakpoints directly. To break on function entry, set a breakpoint on the first
 * instruction of its body.
 *
 * @category debugging
 * @since 0.0.0
 */
export const setBreakpoint = (session: Session, instr: object): void => {
  breakpointsOf(session).add(instr)
}

/**
 * Clears a breakpoint previously set with {@link setBreakpoint}.
 *
 * @category debugging
 * @since 0.0.0
 */
export const clearBreakpoint = (session: Session, instr: object): void => {
  breakpointsOf(session).delete(instr)
}

/**
 * Clears every breakpoint of the session.
 *
 * @category debugging
 * @since 0.0.0
 */
export const clearBreakpoints = (session: Session): void => {
  breakpointsOf(session).clear()
}

/**
 * One frame of a paused session's call stack.
 *
 * @category debugging
 * @since 0.0.0
 */
export interface FrameInfo {
  readonly funcIndex: number
  readonly funcName: string | undefined
}

/**
 * The call stack of a paused session, innermost frame last.
 *
 * @category debugging
 * @since 0.0.0
 */
export const callStack = (session: Session): ReadonlyArray<FrameInfo> => {
  const machine = machineOf(session)
  return machine.frames.map((frame) => ({
    funcIndex: frame.funcIndex,
    funcName: machine.instance.snapshot.funcs[frame.funcIndex]?.name,
  }))
}

/**
 * One local variable of a frame: parameters first, declared locals after.
 *
 * @category debugging
 * @since 0.0.0
 */
export interface LocalInfo {
  readonly index: number
  readonly name: string | undefined
  readonly value: Value
}

/**
 * The locals of one frame of a paused session. `frameIndex` indexes {@link callStack} and
 * defaults to the innermost frame.
 *
 * @category debugging
 * @since 0.0.0
 */
export const locals = (session: Session, frameIndex?: number): ReadonlyArray<LocalInfo> => {
  const machine = machineOf(session)
  const frame = machine.frames[frameIndex ?? machine.frames.length - 1]
  if (frame === undefined) return []
  const entry = machine.instance.snapshot.funcs[frame.funcIndex]
  const type =
    entry === undefined ? undefined : machine.instance.snapshot.types[entry.typeIndex]?.composite
  const paramCount = type?._tag === 'Func' ? type.params.length : 0
  return frame.locals.map((value, index) => ({
    index,
    name: index < paramCount ? undefined : entry?.definition?.locals[index - paramCount]?.name,
    value,
  }))
}

/**
 * The value stack of one frame of a paused session, bottom first. `frameIndex` indexes
 * {@link callStack} and defaults to the innermost frame.
 *
 * @category debugging
 * @since 0.0.0
 */
export const valueStack = (session: Session, frameIndex?: number): ReadonlyArray<Value> => {
  const machine = machineOf(session)
  const frame = machine.frames[frameIndex ?? machine.frames.length - 1]
  return frame === undefined ? [] : [...frame.stack]
}

/**
 * One global of an instance, in declaration order.
 *
 * @category debugging
 * @since 0.0.0
 */
export interface GlobalInfo {
  readonly name: string | undefined
  readonly mutable: boolean
  readonly value: Value
}

/**
 * Every global of the instance with its current value, in declaration order.
 *
 * @category debugging
 * @since 0.0.0
 */
export const globals = (instance: Instance): ReadonlyArray<GlobalInfo> => {
  const state = stateOf(instance)
  return state.globals.map((global, index) => ({
    name: state.snapshot.globals[index]?.name,
    mutable: global.mutable,
    value: global.value,
  }))
}

/**
 * The current size in pages of a memory, by declaration order index. Returns `undefined` for an
 * unknown index.
 *
 * @category debugging
 * @since 0.0.0
 */
export const memoryPageCount = (instance: Instance, memoryIndex = 0): number | undefined => {
  const memory = stateOf(instance).memories[memoryIndex]
  return memory === undefined ? undefined : Math.floor(memory.bytes.length / 65536)
}

/**
 * Copies a byte range out of a memory, by declaration order index. The range is clamped to the
 * memory's current size.
 *
 * @category debugging
 * @since 0.0.0
 */
export const readMemory = (
  instance: Instance,
  memoryIndex: number,
  offset: number,
  length: number,
): Uint8Array => {
  const memory = stateOf(instance).memories[memoryIndex]
  if (memory === undefined) return new Uint8Array(0)
  const start = Math.max(0, Math.min(offset, memory.bytes.length))
  const end = Math.max(start, Math.min(offset + length, memory.bytes.length))
  return memory.bytes.slice(start, end)
}

/**
 * The instructions executed by the session so far — a step counter for profiling and tests.
 *
 * @category debugging
 * @since 0.0.0
 */
export const stepCount = (session: Session): number => machineOf(session).stepCount

/**
 * A null reference value for host use — as an argument, a host-function result, or an imported
 * global's value.
 *
 * @category interpreter
 * @since 0.0.0
 */
export const nullRef: RefValue = Machine.nullRef
