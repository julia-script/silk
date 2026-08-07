import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Binary from '../src/Binary.js'
import * as Builder from '../src/Builder.js'
import * as DataActor from '../src/Data.js'
import * as Elem from '../src/Elem.js'
import * as ExportActor from '../src/Export.js'
import * as Func from '../src/Func.js'
import * as Global from '../src/Global.js'
import * as Import from '../src/Import.js'
import * as Instr from '../src/Instr.js'
import * as Interp from '../src/Interp.js'
import * as Memory from '../src/Memory.js'
import * as Table from '../src/Table.js'
import * as Type from '../src/Type.js'
import * as ValType from '../src/ValType.js'
import type { WasmError } from '../src/WasmError.js'

const failure = <A>(effect: Effect.Effect<A, WasmError>) =>
  effect.pipe(
    Effect.map(() => undefined),
    Effect.catchTag('WasmError', (error) => Effect.succeed(error)),
  )

it.effect('interprets basic arithmetic', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const signature = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
    const add = yield* Func.declare(builder, signature, { name: 'add' })
    yield* Func.define(builder, add, {
      body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')],
    })
    yield* ExportActor.func(builder, 'add', add)
    const instance = yield* Interp.instantiate(builder)
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'add', [2, 40]), [42])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'add', [2147483647, 1]), [-2147483648])
  }),
)

it.effect('interprets loops, branches, and locals (iterative factorial)', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const signature = yield* Type.func(builder, [ValType.i64], [ValType.i64])
    const factorial = yield* Func.declare(builder, signature, { name: 'factorial' })
    // result = 1; while (n > 1) { result *= n; n -= 1 }
    yield* Func.define(builder, factorial, {
      locals: [{ type: ValType.i64, name: 'result' }],
      body: [
        Instr.i64Const(1n),
        Instr.localSet(1),
        Instr.block(Instr.emptyBlockType, [
          Instr.loop(Instr.emptyBlockType, [
            Instr.localGet(0),
            Instr.i64Const(1n),
            Instr.op('i64.le_s'),
            Instr.brIf(1),
            Instr.localGet(1),
            Instr.localGet(0),
            Instr.op('i64.mul'),
            Instr.localSet(1),
            Instr.localGet(0),
            Instr.i64Const(1n),
            Instr.op('i64.sub'),
            Instr.localSet(0),
            Instr.br(0),
          ]),
        ]),
        Instr.localGet(1),
      ],
    })
    yield* ExportActor.func(builder, 'factorial', factorial)
    const instance = yield* Interp.instantiate(builder)
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'factorial', [20n]), [
      2432902008176640000n,
    ])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'factorial', [0n]), [1n])
  }),
)

it.effect('interprets recursion and if/else (fibonacci)', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const signature = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const fib = yield* Func.declare(builder, signature, { name: 'fib' })
    yield* Func.define(builder, fib, {
      body: [
        Instr.localGet(0),
        Instr.i32Const(2),
        Instr.op('i32.lt_s'),
        Instr.ifElse(
          Instr.valueBlockType(ValType.i32),
          [Instr.localGet(0)],
          [
            Instr.localGet(0),
            Instr.i32Const(1),
            Instr.op('i32.sub'),
            Instr.call(fib),
            Instr.localGet(0),
            Instr.i32Const(2),
            Instr.op('i32.sub'),
            Instr.call(fib),
            Instr.op('i32.add'),
          ],
        ),
      ],
    })
    yield* ExportActor.func(builder, 'fib', fib)
    const instance = yield* Interp.instantiate(builder)
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'fib', [15]), [610])
  }),
)

it.effect('interprets br_table and select', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const signature = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const classify = yield* Func.declare(builder, signature, { name: 'classify' })
    // returns 10 for 0, 20 for 1, 99 otherwise
    yield* Func.define(builder, classify, {
      body: [
        Instr.block(Instr.emptyBlockType, [
          Instr.block(Instr.emptyBlockType, [
            Instr.block(Instr.emptyBlockType, [Instr.localGet(0), Instr.brTable([0, 1], 2)]),
            Instr.i32Const(10),
            Instr.op('return'),
          ]),
          Instr.i32Const(20),
          Instr.op('return'),
        ]),
        Instr.i32Const(99),
      ],
    })
    const pick = yield* Func.declare(builder, signature, { name: 'pick' })
    yield* Func.define(builder, pick, {
      body: [Instr.i32Const(-1), Instr.i32Const(1), Instr.localGet(0), Instr.op('select')],
    })
    yield* ExportActor.func(builder, 'classify', classify)
    yield* ExportActor.func(builder, 'pick', pick)
    const instance = yield* Interp.instantiate(builder)
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'classify', [0]), [10])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'classify', [1]), [20])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'classify', [7]), [99])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'pick', [5]), [-1])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'pick', [0]), [1])
  }),
)

it.effect('interprets memory: segments, loads, stores, grow, fill, copy, init', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const memory = yield* Memory.make(builder, { min: 1, max: 4 })
    yield* DataActor.active(builder, memory, [Instr.i32Const(16)], new Uint8Array([1, 2, 3, 4]))
    const seed = yield* DataActor.passive(builder, new Uint8Array([9, 8, 7]))
    const signature = yield* Type.func(builder, [], [ValType.i32])
    const run = yield* Func.declare(builder, signature, { name: 'run' })
    yield* Func.define(builder, run, {
      body: [
        // grow by one page; expect previous size 1
        Instr.i32Const(1),
        Instr.memoryGrow(memory),
        Instr.op('drop'),
        // fill [32, 34) with 5
        Instr.i32Const(32),
        Instr.i32Const(5),
        Instr.i32Const(2),
        Instr.memoryFill(memory),
        // copy the active segment bytes from 16 to 40
        Instr.i32Const(40),
        Instr.i32Const(16),
        Instr.i32Const(4),
        Instr.memoryCopy(memory, memory),
        // init from the passive segment at 48
        Instr.i32Const(48),
        Instr.i32Const(0),
        Instr.i32Const(3),
        Instr.memoryInit(seed, memory),
        // sum: mem[16] + mem[33] + mem[43] + mem[48] + memory.size
        Instr.i32Const(16),
        Instr.memoryAccess('i32.load8_u', memory),
        Instr.i32Const(33),
        Instr.memoryAccess('i32.load8_u', memory),
        Instr.op('i32.add'),
        Instr.i32Const(43),
        Instr.memoryAccess('i32.load8_u', memory),
        Instr.op('i32.add'),
        Instr.i32Const(48),
        Instr.memoryAccess('i32.load8_u', memory),
        Instr.op('i32.add'),
        Instr.memorySize(memory),
        Instr.op('i32.add'),
      ],
    })
    yield* ExportActor.func(builder, 'run', run)
    const instance = yield* Interp.instantiate(builder)
    // 1 + 5 + 4 + 9 + 2 pages
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'run'), [21])
    assert.strictEqual(Interp.memoryPageCount(instance), 2)
    assert.deepStrictEqual(Array.from(Interp.readMemory(instance, 0, 16, 4)), [1, 2, 3, 4])
    assert.deepStrictEqual(Array.from(Interp.readMemory(instance, 0, 40, 4)), [1, 2, 3, 4])
  }),
)

it.effect('interprets globals and exposes them for inspection', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const counter = yield* Global.make(builder, ValType.i32, true, [Instr.i32Const(7)], {
      name: 'counter',
    })
    const signature = yield* Type.func(builder, [], [ValType.i32])
    const bump = yield* Func.declare(builder, signature, { name: 'bump' })
    yield* Func.define(builder, bump, {
      body: [
        Instr.globalGet(counter),
        Instr.i32Const(1),
        Instr.op('i32.add'),
        Instr.globalSet(counter),
        Instr.globalGet(counter),
      ],
    })
    yield* ExportActor.func(builder, 'bump', bump)
    const instance = yield* Interp.instantiate(builder)
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'bump'), [8])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'bump'), [9])
    assert.deepStrictEqual(Interp.globals(instance), [{ name: 'counter', mutable: true, value: 9 }])
  }),
)

it.effect('interprets call_indirect through an active element segment', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const binOp = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
    const add = yield* Func.declare(builder, binOp, { name: 'add' })
    yield* Func.define(builder, add, {
      body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')],
    })
    const mul = yield* Func.declare(builder, binOp, { name: 'mul' })
    yield* Func.define(builder, mul, {
      body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.mul')],
    })
    const table = yield* Table.make(builder, ValType.funcref, { min: 2 })
    yield* Elem.active(builder, table, [Instr.i32Const(0)], [add, mul])
    const dispatchType = yield* Type.func(
      builder,
      [ValType.i32, ValType.i32, ValType.i32],
      [ValType.i32],
    )
    const dispatch = yield* Func.declare(builder, dispatchType, { name: 'dispatch' })
    yield* Func.define(builder, dispatch, {
      body: [
        Instr.localGet(1),
        Instr.localGet(2),
        Instr.localGet(0),
        Instr.callIndirect(table, binOp),
      ],
    })
    yield* ExportActor.func(builder, 'dispatch', dispatch)
    const instance = yield* Interp.instantiate(builder)
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'dispatch', [0, 6, 7]), [13])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'dispatch', [1, 6, 7]), [42])
    // out-of-bounds table index traps
    const trapped = yield* failure(Interp.invoke(instance, 'dispatch', [9, 1, 1]))
    assert.isDefined(trapped)
    assert.include(trapped.message, 'trap')
  }),
)

it.effect('calls host imports and reads imported globals', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const unary = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const twice = yield* Import.func(builder, 'env', 'twice', unary)
    const base = yield* Import.global(builder, 'env', 'base', ValType.i32, false)
    const run = yield* Func.declare(builder, unary, { name: 'run' })
    yield* Func.define(builder, run, {
      body: [Instr.localGet(0), Instr.call(twice), Instr.globalGet(base), Instr.op('i32.add')],
    })
    yield* ExportActor.func(builder, 'run', run)
    const instance = yield* Interp.instantiate(builder, {
      imports: {
        env: {
          twice: (args) => ((args[0] as number) * 2) | 0,
          base: 100,
        },
      },
    })
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'run', [21]), [142])
    const missing = yield* failure(Interp.instantiate(builder))
    assert.isDefined(missing)
    assert.include(missing.message, 'env.twice')
  }),
)

it.effect('reports traps as typed errors', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const memory = yield* Memory.make(builder, { min: 1 })
    const signature = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
    const div = yield* Func.declare(builder, signature, { name: 'div' })
    yield* Func.define(builder, div, {
      body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.div_s')],
    })
    const load = yield* Func.declare(builder, signature, { name: 'load' })
    yield* Func.define(builder, load, {
      body: [
        Instr.localGet(0),
        Instr.memoryAccess('i32.load', memory),
        Instr.op('drop'),
        Instr.op('unreachable'),
      ],
    })
    yield* ExportActor.func(builder, 'div', div)
    yield* ExportActor.func(builder, 'load', load)
    const instance = yield* Interp.instantiate(builder)
    const divByZero = yield* failure(Interp.invoke(instance, 'div', [1, 0]))
    assert.include(divByZero?.message, 'divide by zero')
    const overflow = yield* failure(Interp.invoke(instance, 'div', [-2147483648, -1]))
    assert.include(overflow?.message, 'integer overflow')
    const outOfBounds = yield* failure(Interp.invoke(instance, 'load', [65533, 0]))
    assert.include(outOfBounds?.message, 'out of bounds')
    const unreachable = yield* failure(Interp.invoke(instance, 'load', [0, 0]))
    assert.include(unreachable?.message, 'unreachable')
  }),
)

it.effect('steps, pauses on breakpoints, and inspects frames, locals, and stacks', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const signature = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const fib = yield* Func.declare(builder, signature, { name: 'fib' })
    const baseCase = Instr.localGet(0)
    yield* Func.define(builder, fib, {
      body: [
        Instr.localGet(0),
        Instr.i32Const(2),
        Instr.op('i32.lt_s'),
        Instr.ifElse(
          Instr.valueBlockType(ValType.i32),
          [baseCase],
          [
            Instr.localGet(0),
            Instr.i32Const(1),
            Instr.op('i32.sub'),
            Instr.call(fib),
            Instr.localGet(0),
            Instr.i32Const(2),
            Instr.op('i32.sub'),
            Instr.call(fib),
            Instr.op('i32.add'),
          ],
        ),
      ],
    })
    yield* ExportActor.func(builder, 'fib', fib)
    const instance = yield* Interp.instantiate(builder)

    // Single-stepping: session starts paused at the first instruction.
    const session = yield* Interp.debug(instance, 'fib', [5])
    const initial = Interp.status(session)
    assert.strictEqual(initial._tag, 'Paused')
    if (initial._tag === 'Paused') {
      assert.strictEqual(initial.location.funcName, 'fib')
      assert.strictEqual(initial.location.instr._tag, 'LocalGet')
    }
    assert.deepStrictEqual(
      Interp.locals(session).map((local) => local.value),
      [5],
    )
    Interp.step(session)
    assert.deepStrictEqual(Interp.valueStack(session), [5])

    // Breakpoint on the base-case instruction: first hit is the deepest recursive call.
    Interp.setBreakpoint(session, baseCase)
    const atBreak = Interp.resume(session)
    assert.strictEqual(atBreak._tag, 'Paused')
    if (atBreak._tag === 'Paused') assert.strictEqual(atBreak.location.instr, baseCase)
    const stack = Interp.callStack(session)
    assert.isAbove(stack.length, 1)
    assert.isTrue(stack.every((frame) => frame.funcName === 'fib'))
    assert.deepStrictEqual(
      Interp.locals(session).map((local) => local.value),
      [1],
    )

    // Clearing the breakpoint and resuming runs to completion.
    Interp.clearBreakpoints(session)
    const done = Interp.resume(session)
    assert.deepStrictEqual(done, { _tag: 'Done', results: [5] })
    assert.isAbove(Interp.stepCount(session), 10)

    // step over: a call executes in one observable move.
    const over = yield* Interp.debug(instance, 'fib', [6])
    let guard = 0
    while (guard < 1000) {
      const current = Interp.status(over)
      if (current._tag !== 'Paused') break
      assert.strictEqual(Interp.callStack(over).length, 1)
      Interp.stepOver(over)
      guard += 1
    }
    assert.deepStrictEqual(Interp.status(over), { _tag: 'Done', results: [8] })

    // resume with a step budget stays paused.
    const sliced = yield* Interp.debug(instance, 'fib', [20])
    const paused = Interp.resume(sliced, { maxSteps: 50 })
    assert.strictEqual(paused._tag, 'Paused')
    assert.strictEqual(Interp.stepCount(sliced), 50)
  }),
)

it.effect('steps out of a nested call', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const signature = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const inner = yield* Func.declare(builder, signature, { name: 'inner' })
    yield* Func.define(builder, inner, {
      body: [Instr.localGet(0), Instr.i32Const(1), Instr.op('i32.add')],
    })
    const outer = yield* Func.declare(builder, signature, { name: 'outer' })
    yield* Func.define(builder, outer, {
      body: [Instr.localGet(0), Instr.call(inner), Instr.i32Const(10), Instr.op('i32.mul')],
    })
    yield* ExportActor.func(builder, 'outer', outer)
    const instance = yield* Interp.instantiate(builder)
    const session = yield* Interp.debug(instance, 'outer', [3])
    Interp.step(session) // local.get
    Interp.step(session) // call -> now paused inside inner
    assert.strictEqual(Interp.callStack(session).length, 2)
    assert.strictEqual(Interp.callStack(session)[1]?.funcName, 'inner')
    const back = Interp.stepOut(session)
    assert.strictEqual(back._tag, 'Paused')
    assert.strictEqual(Interp.callStack(session).length, 1)
    assert.deepStrictEqual(Interp.valueStack(session), [4])
    assert.deepStrictEqual(Interp.resume(session), { _tag: 'Done', results: [40] })
  }),
)

it.effect('runs the start function and evaluates constant expressions', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const offset = yield* Global.make(builder, ValType.i32, false, [
      Instr.i32Const(30),
      Instr.i32Const(12),
      Instr.op('i32.add'),
    ])
    const answer = yield* Global.make(builder, ValType.i32, true, [Instr.i32Const(0)], {
      name: 'answer',
    })
    const empty = yield* Type.func(builder, [], [])
    const init = yield* Func.declare(builder, empty)
    yield* Func.define(builder, init, {
      body: [Instr.globalGet(offset), Instr.globalSet(answer)],
    })
    yield* Func.start(builder, init)
    const instance = yield* Interp.instantiate(builder)
    assert.deepStrictEqual(Interp.globals(instance)[1], {
      name: 'answer',
      mutable: true,
      value: 42,
    })
  }),
)

it.effect('matches native WebAssembly on mixed numeric workloads', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32Sig = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
    const scramble32 = yield* Func.declare(builder, i32Sig, { name: 'scramble32' })
    yield* Func.define(builder, scramble32, {
      body: [
        Instr.localGet(0),
        Instr.localGet(1),
        Instr.op('i32.mul'),
        Instr.localGet(0),
        Instr.op('i32.rotl'),
        Instr.localGet(1),
        Instr.op('i32.xor'),
        Instr.op('i32.clz'),
        Instr.localGet(0),
        Instr.op('i32.sub'),
        Instr.localGet(1),
        Instr.op('i32.shr_u'),
        Instr.localGet(0),
        Instr.op('i32.popcnt'),
        Instr.op('i32.add'),
      ],
    })
    const i64Sig = yield* Type.func(builder, [ValType.i64, ValType.i64], [ValType.i64])
    const scramble64 = yield* Func.declare(builder, i64Sig, { name: 'scramble64' })
    yield* Func.define(builder, scramble64, {
      body: [
        Instr.localGet(0),
        Instr.localGet(1),
        Instr.op('i64.mul'),
        Instr.localGet(1),
        Instr.op('i64.rotr'),
        Instr.localGet(0),
        Instr.op('i64.xor'),
        Instr.op('i64.ctz'),
        Instr.localGet(0),
        Instr.op('i64.add'),
        Instr.localGet(1),
        Instr.i64Const(1n),
        Instr.op('i64.or'),
        Instr.op('i64.div_u'),
      ],
    })
    const f64Sig = yield* Type.func(builder, [ValType.f64, ValType.f64], [ValType.f64])
    const scrambleF = yield* Func.declare(builder, f64Sig, { name: 'scrambleF' })
    yield* Func.define(builder, scrambleF, {
      body: [
        Instr.localGet(0),
        Instr.localGet(1),
        Instr.op('f64.div'),
        Instr.op('f64.nearest'),
        Instr.localGet(1),
        Instr.op('f64.copysign'),
        Instr.localGet(0),
        Instr.op('f64.min'),
        Instr.op('f64.sqrt'),
      ],
    })
    yield* ExportActor.func(builder, 'scramble32', scramble32)
    yield* ExportActor.func(builder, 'scramble64', scramble64)
    yield* ExportActor.func(builder, 'scrambleF', scrambleF)

    const bytes = yield* Binary.encode(builder)
    const native = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.slice().buffer as ArrayBuffer),
    )
    const exports = native.instance.exports as Record<string, CallableFunction>
    const instance = yield* Interp.instantiate(builder)

    const i32Cases: Array<[number, number]> = [
      [0, 0],
      [1, -1],
      [-2147483648, 2147483647],
      [123456789, -987654321],
      [42, 7],
    ]
    for (const [a, b] of i32Cases) {
      assert.deepStrictEqual(
        yield* Interp.invoke(instance, 'scramble32', [a, b]),
        [exports.scramble32?.(a, b)],
        `scramble32(${a}, ${b})`,
      )
    }
    const i64Cases: Array<[bigint, bigint]> = [
      [0n, 0n],
      [1n, -1n],
      [-9223372036854775808n, 9223372036854775807n],
      [1234567890123456789n, -987654321987654321n],
    ]
    for (const [a, b] of i64Cases) {
      assert.deepStrictEqual(
        yield* Interp.invoke(instance, 'scramble64', [a, b]),
        [exports.scramble64?.(a, b)],
        `scramble64(${a}, ${b})`,
      )
    }
    const f64Cases: Array<[number, number]> = [
      [1.5, 2.5],
      [-3.75, 0.5],
      [Number.MAX_SAFE_INTEGER, 3],
      [0.1, -0.3],
      [-0, 5],
    ]
    for (const [a, b] of f64Cases) {
      assert.deepStrictEqual(
        yield* Interp.invoke(instance, 'scrambleF', [a, b]),
        [exports.scrambleF?.(a, b)],
        `scrambleF(${a}, ${b})`,
      )
    }
  }),
)

it.effect('runs tail calls without growing the frame stack', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const signature = yield* Type.func(builder, [ValType.i32], [ValType.i32])
    const even = yield* Func.declare(builder, signature, { name: 'even' })
    const odd = yield* Func.declare(builder, signature, { name: 'odd' })
    const parity = (next: Func.Func, base: number): ReadonlyArray<Instr.Instr> => [
      Instr.localGet(0),
      Instr.op('i32.eqz'),
      Instr.ifElse(
        Instr.valueBlockType(ValType.i32),
        [Instr.i32Const(base)],
        [Instr.localGet(0), Instr.i32Const(1), Instr.op('i32.sub'), Instr.returnCall(next)],
      ),
    ]
    yield* Func.define(builder, even, { body: parity(odd, 1) })
    yield* Func.define(builder, odd, { body: parity(even, 0) })
    yield* ExportActor.func(builder, 'even', even)
    const instance = yield* Interp.instantiate(builder)
    // 50_000 mutual tail calls: a frame-pushing interpreter would hold 50_001 frames here.
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'even', [50_000]), [1])
    const session = yield* Interp.debug(instance, 'even', [999])
    let maxFrames = 0
    while (Interp.status(session)._tag === 'Paused') {
      maxFrames = Math.max(maxFrames, Interp.callStack(session).length)
      Interp.step(session)
    }
    assert.strictEqual(maxFrames, 1)
    assert.deepStrictEqual(Interp.status(session), { _tag: 'Done', results: [0] })
  }),
)

it.effect('interprets runtime table ops, br_on_null, and elem.drop', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const binOp = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
    const add = yield* Func.declare(builder, binOp, { name: 'add' })
    yield* Func.define(builder, add, {
      body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')],
    })
    const mul = yield* Func.declare(builder, binOp, { name: 'mul' })
    yield* Func.define(builder, mul, {
      body: [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.mul')],
    })
    const table = yield* Table.make(builder, ValType.funcref, { min: 1, max: 10 })
    const ops = yield* Elem.passive(builder, ValType.funcref, [add, mul])

    const run = yield* Func.declare(builder, binOp, { name: 'run' })
    yield* Func.define(builder, run, {
      body: [
        // grow by 2 slots of null: size 1 -> 3
        Instr.refNull('func'),
        Instr.i32Const(2),
        Instr.tableGrow(table),
        Instr.op('drop'),
        // init [add, mul] at slot 1
        Instr.i32Const(1),
        Instr.i32Const(0),
        Instr.i32Const(2),
        Instr.tableInit(ops, table),
        // copy slot 2 (mul) into slot 0
        Instr.i32Const(0),
        Instr.i32Const(2),
        Instr.i32Const(1),
        Instr.tableCopy(table, table),
        // mul(a, b) through slot 0
        Instr.localGet(0),
        Instr.localGet(1),
        Instr.i32Const(0),
        Instr.callIndirect(table, binOp),
        // + table.size * 1000
        Instr.tableSize(table),
        Instr.i32Const(1000),
        Instr.op('i32.mul'),
        Instr.op('i32.add'),
        // fill slots 1..2 with add, then add(10, 20) through slot 1
        Instr.i32Const(1),
        Instr.refFunc(add),
        Instr.i32Const(2),
        Instr.tableFill(table),
        Instr.i32Const(10),
        Instr.i32Const(20),
        Instr.i32Const(1),
        Instr.callIndirect(table, binOp),
        Instr.op('i32.add'),
      ],
    })

    const nullary = yield* Type.func(builder, [], [ValType.i32])
    const branchOnNull = yield* Func.declare(builder, nullary, { name: 'branch_on_null' })
    yield* Func.define(builder, branchOnNull, {
      body: [
        Instr.block(Instr.valueBlockType(ValType.i32), [
          Instr.i32Const(1),
          Instr.refNull('func'),
          Instr.brOnNull(0),
          Instr.op('drop'),
          Instr.i32Const(2),
          Instr.op('i32.add'),
        ]),
      ],
    })
    const keepNonNull = yield* Func.declare(builder, nullary, { name: 'keep_non_null' })
    yield* Func.define(builder, keepNonNull, {
      body: [
        Instr.block(Instr.valueBlockType(ValType.i32), [
          Instr.i32Const(1),
          Instr.refFunc(add),
          Instr.brOnNull(0),
          Instr.op('drop'),
          Instr.i32Const(2),
          Instr.op('i32.add'),
        ]),
      ],
    })
    const dropThenInit = yield* Func.declare(builder, nullary, { name: 'drop_then_init' })
    yield* Func.define(builder, dropThenInit, {
      body: [
        Instr.elemDrop(ops),
        Instr.i32Const(0),
        Instr.i32Const(0),
        Instr.i32Const(1),
        Instr.tableInit(ops, table),
        Instr.i32Const(0),
      ],
    })
    yield* ExportActor.func(builder, 'run', run)
    yield* ExportActor.func(builder, 'branch_on_null', branchOnNull)
    yield* ExportActor.func(builder, 'keep_non_null', keepNonNull)
    yield* ExportActor.func(builder, 'drop_then_init', dropThenInit)

    const instance = yield* Interp.instantiate(builder)
    // mul(6, 7) + 3 * 1000 + add(10, 20)
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'run', [6, 7]), [3072])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'branch_on_null'), [1])
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'keep_non_null'), [3])
    const dropped = yield* failure(Interp.invoke(instance, 'drop_then_init'))
    assert.include(dropped?.message, 'out of bounds')
  }),
)

it.effect('rejects mistyped boundary values as WasmError, not defects', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i64Sig = yield* Type.func(builder, [ValType.i64], [ValType.i64])
    const echo = yield* Func.declare(builder, i64Sig, { name: 'echo' })
    yield* Func.define(builder, echo, { body: [Instr.localGet(0)] })
    const i32Sig = yield* Type.func(builder, [], [ValType.i32])
    const viaHost = yield* Import.func(builder, 'env', 'answer', i32Sig)
    const callHost = yield* Func.declare(builder, i32Sig, { name: 'call_host' })
    yield* Func.define(builder, callHost, { body: [Instr.call(viaHost)] })
    yield* ExportActor.func(builder, 'echo', echo)
    yield* ExportActor.func(builder, 'call_host', callHost)
    const instance = yield* Interp.instantiate(builder, {
      // A host function returning a bigint where the type promises i32.
      imports: { env: { answer: () => 42n } },
    })
    // number where i64 is expected
    const wrongType = yield* failure(Interp.invoke(instance, 'echo', [5]))
    assert.include(wrongType?.message, 'bigint')
    // wrong arity
    const wrongArity = yield* failure(Interp.invoke(instance, 'echo', [5n, 6n]))
    assert.include(wrongArity?.message, 'argument')
    // mistyped host result surfaces as a trap
    const badHost = yield* failure(Interp.invoke(instance, 'call_host'))
    assert.include(badHost?.message, 'host result')
    // valid call still works
    assert.deepStrictEqual(yield* Interp.invoke(instance, 'echo', [7n]), [7n])
  }),
)
