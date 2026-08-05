/**
 * The fixture inventory: representative modules covering every baseline feature area.
 * Each entry builds one module through the public API of the built package.
 */
import * as Effect from 'effect/Effect'
import {
  Binary,
  Builder,
  Data,
  Elem,
  Export,
  Func,
  Global,
  Import,
  Instr,
  Memory,
  Table,
  Type,
  ValType,
  WatText,
} from '../dist/index.js'

const build = (name, program) => ({
  name,
  run: () =>
    Effect.runPromise(
      Effect.gen(function* () {
        const builder = yield* program
        return {
          bytes: yield* Binary.encode(builder),
          text: yield* WatText.render(builder),
        }
      }),
    ),
})

export const fixtures = [
  build(
    'empty',
    Effect.gen(function* () {
      return yield* Builder.make()
    }),
  ),

  build(
    'arithmetic',
    Effect.gen(function* () {
      const builder = yield* Builder.make({ moduleName: 'arithmetic' })
      const binaryType = yield* Type.func(builder, [ValType.i32, ValType.i32], [ValType.i32])
      const ops = ['i32.add', 'i32.sub', 'i32.mul', 'i32.div_s', 'i32.rem_u', 'i32.xor']
      for (const mnemonic of ops) {
        const func = yield* Func.declare(builder, binaryType, {
          name: mnemonic.replace('.', '_'),
        })
        yield* Func.define(builder, func, {
          body: [Instr.localGet(0), Instr.localGet(1), Instr.op(mnemonic)],
        })
        yield* Export.func(builder, mnemonic, func)
      }
      const convertType = yield* Type.func(builder, [ValType.f64], [ValType.i64])
      const convert = yield* Func.declare(builder, convertType, { name: 'convert' })
      yield* Func.define(builder, convert, {
        locals: [{ type: ValType.f32, name: 'narrow' }],
        body: [
          Instr.localGet(0),
          Instr.op('f32.demote_f64'),
          Instr.localTee(1),
          Instr.op('f32.sqrt'),
          Instr.op('i64.trunc_sat_f32_s'),
          Instr.localGet(0),
          Instr.op('i64.trunc_sat_f64_u'),
          Instr.op('i64.add'),
          Instr.op('i64.extend32_s'),
        ],
      })
      yield* Export.func(builder, 'convert', convert)
      const wide = yield* Func.declare(builder, yield* Type.func(builder, [], [ValType.i64]), {
        name: 'wide',
      })
      yield* Func.define(builder, wide, {
        body: [
          Instr.i64Const(0x7fffffffffffffffn),
          Instr.i64Const(-1n),
          Instr.op('i64.xor'),
          Instr.f64Const(2.5),
          Instr.op('i64.trunc_f64_s'),
          Instr.op('i64.or'),
        ],
      })
      yield* Export.func(builder, 'wide', wide)
      return builder
    }),
  ),

  build(
    'control-flow',
    Effect.gen(function* () {
      const builder = yield* Builder.make({ moduleName: 'control-flow' })
      const type = yield* Type.func(builder, [ValType.i32], [ValType.i32])
      const classify = yield* Func.declare(builder, type, { name: 'classify' })
      yield* Func.define(builder, classify, {
        body: [
          Instr.block(Instr.valueBlockType(ValType.i32), [
            Instr.block(Instr.emptyBlockType, [
              Instr.block(Instr.emptyBlockType, [Instr.localGet(0), Instr.brTable([0, 1], 1)]),
              Instr.i32Const(100),
              Instr.br(1),
            ]),
            Instr.loop(Instr.emptyBlockType, [
              Instr.localGet(0),
              Instr.i32Const(100),
              Instr.op('i32.gt_s'),
              Instr.brIf(0),
            ]),
            Instr.localGet(0),
            Instr.ifElse(
              Instr.valueBlockType(ValType.i32),
              [Instr.i32Const(1)],
              [Instr.i32Const(2)],
            ),
          ]),
        ],
      })
      yield* Export.func(builder, 'classify', classify)
      const even = yield* Func.declare(builder, type, { name: 'even' })
      const odd = yield* Func.declare(builder, type, { name: 'odd' })
      yield* Func.define(builder, even, {
        body: [
          Instr.localGet(0),
          Instr.op('i32.eqz'),
          Instr.ifElse(
            Instr.valueBlockType(ValType.i32),
            [Instr.i32Const(1)],
            [Instr.localGet(0), Instr.i32Const(1), Instr.op('i32.sub'), Instr.returnCall(odd)],
          ),
        ],
      })
      yield* Func.define(builder, odd, {
        body: [
          Instr.localGet(0),
          Instr.op('i32.eqz'),
          Instr.ifElse(
            Instr.valueBlockType(ValType.i32),
            [Instr.i32Const(0)],
            [Instr.localGet(0), Instr.i32Const(1), Instr.op('i32.sub'), Instr.returnCall(even)],
          ),
        ],
      })
      yield* Export.func(builder, 'even', even)
      const pick = yield* Func.declare(
        builder,
        yield* Type.func(builder, [ValType.i32], [ValType.f64]),
        { name: 'pick' },
      )
      yield* Func.define(builder, pick, {
        body: [
          Instr.f64Const(1.5),
          Instr.f64Const(-0),
          Instr.localGet(0),
          Instr.op('select'),
          Instr.op('unreachable'),
        ],
      })
      return builder
    }),
  ),

  build(
    'declarations',
    Effect.gen(function* () {
      const builder = yield* Builder.make({ moduleName: 'declarations' })
      const logType = yield* Type.func(builder, [ValType.i32], [])
      const log = yield* Import.func(builder, 'env', 'log', logType, { name: 'log' })
      const base = yield* Import.global(builder, 'env', 'base', ValType.i32, false, {
        name: 'base',
      })
      const shared = yield* Import.memory(
        builder,
        'env',
        'shared',
        { min: 1, max: 8 },
        {
          name: 'shared',
        },
      )
      const indirect = yield* Import.table(
        builder,
        'env',
        'indirect',
        ValType.funcref,
        { min: 4 },
        { name: 'indirect' },
      )
      const scratch = yield* Memory.make(builder, { min: 2 }, { name: 'scratch' })
      const offset = yield* Global.make(
        builder,
        ValType.i32,
        false,
        [Instr.globalGet(base), Instr.i32Const(64), Instr.op('i32.mul')],
        { name: 'offset' },
      )
      const cursor = yield* Global.make(builder, ValType.i64, true, [Instr.i64Const(0n)], {
        name: 'cursor',
      })
      const externSlot = yield* Global.make(
        builder,
        ValType.externref,
        true,
        [Instr.refNull(ValType.externref)],
        { name: 'externSlot' },
      )
      const mainType = yield* Type.func(builder, [], [])
      const main = yield* Func.declare(builder, mainType, { name: 'main' })
      yield* Func.define(builder, main, {
        body: [
          Instr.globalGet(offset),
          Instr.call(log),
          Instr.globalGet(cursor),
          Instr.i64Const(1n),
          Instr.op('i64.add'),
          Instr.globalSet(cursor),
          // Cross-memory traffic between an imported and a defined memory.
          Instr.i32Const(0),
          Instr.i32Const(0),
          Instr.i32Const(64),
          Instr.memoryCopy(scratch, shared),
          Instr.memorySize(shared),
          Instr.memoryGrow(scratch),
          Instr.op('drop'),
        ],
      })
      yield* Func.start(builder, main)
      yield* Export.func(builder, 'main', main)
      yield* Export.memory(builder, 'scratch', scratch)
      yield* Export.table(builder, 'indirect', indirect)
      yield* Export.global(builder, 'cursor', cursor)
      yield* Export.global(builder, 'externSlot', externSlot)
      return builder
    }),
  ),

  build(
    'segments',
    Effect.gen(function* () {
      const builder = yield* Builder.make({ moduleName: 'segments' })
      const type = yield* Type.func(builder, [], [ValType.i32])
      const first = yield* Func.declare(builder, type, { name: 'first' })
      yield* Func.define(builder, first, { body: [Instr.i32Const(1)] })
      const second = yield* Func.declare(builder, type, { name: 'second' })
      yield* Func.define(builder, second, { body: [Instr.i32Const(2)] })
      const funcs = yield* Table.make(
        builder,
        ValType.funcref,
        { min: 4, max: 8 },
        {
          name: 'funcs',
        },
      )
      const externs = yield* Table.make(
        builder,
        ValType.externref,
        { min: 2 },
        {
          name: 'externs',
        },
      )
      yield* Elem.active(builder, funcs, [Instr.i32Const(0)], [first, second])
      const spare = yield* Elem.passive(builder, ValType.funcref, [
        second,
        [Instr.refNull(ValType.funcref)],
      ])
      yield* Elem.declarative(builder, ValType.funcref, [first])
      const memory = yield* Memory.make(builder, { min: 1 }, { name: 'memory' })
      yield* Data.active(builder, memory, [Instr.i32Const(16)], new Uint8Array([1, 2, 3, 4]))
      const greeting = yield* Data.passive(
        builder,
        new Uint8Array([0x68, 0x69, 0x00, 0xff, 0x22, 0x5c]),
        { name: 'greeting' },
      )
      const setup = yield* Func.declare(builder, yield* Type.func(builder, [], []), {
        name: 'setup',
      })
      yield* Func.define(builder, setup, {
        body: [
          Instr.i32Const(0),
          Instr.i32Const(0),
          Instr.i32Const(2),
          Instr.tableInit(spare, funcs),
          Instr.elemDrop(spare),
          Instr.i32Const(32),
          Instr.i32Const(0),
          Instr.i32Const(6),
          Instr.memoryInit(greeting, memory),
          Instr.dataDrop(greeting),
          Instr.i32Const(0),
          Instr.i32Const(16),
          Instr.i32Const(4),
          Instr.memoryCopy(memory, memory),
          Instr.i32Const(48),
          Instr.i32Const(0),
          Instr.i32Const(8),
          Instr.memoryFill(memory),
          Instr.i32Const(0),
          Instr.refNull(ValType.funcref),
          Instr.i32Const(1),
          Instr.tableFill(funcs),
          Instr.i32Const(0),
          Instr.i32Const(0),
          Instr.i32Const(1),
          Instr.tableCopy(funcs, funcs),
          Instr.refNull(ValType.externref),
          Instr.i32Const(1),
          Instr.tableGrow(externs),
          Instr.op('drop'),
          Instr.i32Const(0),
          Instr.i32Const(0),
          Instr.tableGet(funcs),
          Instr.tableSet(funcs),
          Instr.tableSize(externs),
          Instr.op('drop'),
        ],
      })
      yield* Export.func(builder, 'setup', setup)
      const chooser = yield* Func.declare(
        builder,
        yield* Type.func(builder, [ValType.i32], [ValType.i32]),
        { name: 'chooser' },
      )
      yield* Func.define(builder, chooser, {
        body: [Instr.localGet(0), Instr.callIndirect(funcs, type)],
      })
      yield* Export.func(builder, 'chooser', chooser)
      return builder
    }),
  ),
]
