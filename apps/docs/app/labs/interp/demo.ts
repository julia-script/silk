/**
 * The demo module the interpreter lab debugs.
 *
 * Three exported functions chosen to exercise the three things a debugger must show well:
 * `fib` grows a call stack, `sum_to` mutates locals inside a loop, and `store_series` writes
 * and reads linear memory. A mutable global counts `fib` entries so the globals pane has
 * something to watch.
 */

import * as Builder from '@silk-effect/wasm/Builder'
import * as ExportActor from '@silk-effect/wasm/Export'
import * as Func from '@silk-effect/wasm/Func'
import * as Global from '@silk-effect/wasm/Global'
import * as Instr from '@silk-effect/wasm/Instr'
import * as Interp from '@silk-effect/wasm/Interp'
import * as Memory from '@silk-effect/wasm/Memory'
import * as Type from '@silk-effect/wasm/Type'
import * as ValType from '@silk-effect/wasm/ValType'
import * as Effect from 'effect/Effect'

export interface DemoParam {
  readonly name: string
  readonly sample: number
}

export interface DemoFunction {
  readonly name: string
  readonly signature: string
  readonly params: ReadonlyArray<DemoParam>
  readonly body: ReadonlyArray<Instr.Instr>
}

export interface Demo {
  readonly functions: ReadonlyArray<DemoFunction>
  /** Display names for opaque handles referenced from instructions. */
  readonly names: ReadonlyMap<object, string>
  /** A fresh instance: reset globals, reset memory. */
  readonly instantiate: () => Interp.Instance
}

export const makeDemo = (): Demo =>
  Effect.runSync(
    Effect.gen(function* () {
      const builder = yield* Builder.make({ moduleName: 'interp-lab' })
      const names = new Map<object, string>()

      const memory = yield* Memory.make(builder, { min: 1 }, { name: 'mem' })
      names.set(memory, 'mem')
      const calls = yield* Global.make(builder, ValType.i32, true, [Instr.i32Const(0)], {
        name: 'fib_calls',
      })
      names.set(calls, 'fib_calls')

      const unary = yield* Type.func(builder, [ValType.i32], [ValType.i32])

      // fib: recursion — the call-stack demo.
      const fib = yield* Func.declare(builder, unary, { name: 'fib' })
      names.set(fib, 'fib')
      const fibBody: ReadonlyArray<Instr.Instr> = [
        Instr.globalGet(calls),
        Instr.i32Const(1),
        Instr.op('i32.add'),
        Instr.globalSet(calls),
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
      ]
      yield* Func.define(builder, fib, { body: fibBody })

      // sum_to: a counted loop — the locals demo.
      const sumTo = yield* Func.declare(builder, unary, { name: 'sum_to' })
      names.set(sumTo, 'sum_to')
      const sumToBody: ReadonlyArray<Instr.Instr> = [
        Instr.i32Const(1),
        Instr.localSet(1),
        Instr.block(Instr.emptyBlockType, [
          Instr.loop(Instr.emptyBlockType, [
            Instr.localGet(1),
            Instr.localGet(0),
            Instr.op('i32.gt_s'),
            Instr.brIf(1),
            Instr.localGet(2),
            Instr.localGet(1),
            Instr.op('i32.add'),
            Instr.localSet(2),
            Instr.localGet(1),
            Instr.i32Const(1),
            Instr.op('i32.add'),
            Instr.localSet(1),
            Instr.br(0),
          ]),
        ]),
        Instr.localGet(2),
      ]
      yield* Func.define(builder, sumTo, {
        locals: [
          { type: ValType.i32, name: 'i' },
          { type: ValType.i32, name: 'acc' },
        ],
        body: sumToBody,
      })

      // store_series: write bytes 1..n into memory, then sum them back — the memory demo.
      const storeSeries = yield* Func.declare(builder, unary, { name: 'store_series' })
      names.set(storeSeries, 'store_series')
      const storeSeriesBody: ReadonlyArray<Instr.Instr> = [
        Instr.i32Const(1),
        Instr.localSet(1),
        Instr.block(Instr.emptyBlockType, [
          Instr.loop(Instr.emptyBlockType, [
            Instr.localGet(1),
            Instr.localGet(0),
            Instr.op('i32.gt_s'),
            Instr.brIf(1),
            Instr.localGet(1),
            Instr.localGet(1),
            Instr.memoryAccess('i32.store8', memory),
            Instr.localGet(1),
            Instr.i32Const(1),
            Instr.op('i32.add'),
            Instr.localSet(1),
            Instr.br(0),
          ]),
        ]),
        Instr.i32Const(1),
        Instr.localSet(1),
        Instr.block(Instr.emptyBlockType, [
          Instr.loop(Instr.emptyBlockType, [
            Instr.localGet(1),
            Instr.localGet(0),
            Instr.op('i32.gt_s'),
            Instr.brIf(1),
            Instr.localGet(2),
            Instr.localGet(1),
            Instr.memoryAccess('i32.load8_u', memory),
            Instr.op('i32.add'),
            Instr.localSet(2),
            Instr.localGet(1),
            Instr.i32Const(1),
            Instr.op('i32.add'),
            Instr.localSet(1),
            Instr.br(0),
          ]),
        ]),
        Instr.localGet(2),
      ]
      yield* Func.define(builder, storeSeries, {
        locals: [
          { type: ValType.i32, name: 'i' },
          { type: ValType.i32, name: 'acc' },
        ],
        body: storeSeriesBody,
      })

      yield* ExportActor.func(builder, 'fib', fib)
      yield* ExportActor.func(builder, 'sum_to', sumTo)
      yield* ExportActor.func(builder, 'store_series', storeSeries)

      const functions: ReadonlyArray<DemoFunction> = [
        {
          name: 'fib',
          signature: '(n: i32) -> i32',
          params: [{ name: 'n', sample: 8 }],
          body: fibBody,
        },
        {
          name: 'sum_to',
          signature: '(n: i32) -> i32',
          params: [{ name: 'n', sample: 10 }],
          body: sumToBody,
        },
        {
          name: 'store_series',
          signature: '(n: i32) -> i32',
          params: [{ name: 'n', sample: 12 }],
          body: storeSeriesBody,
        },
      ]

      return {
        functions,
        names,
        instantiate: () => Effect.runSync(Interp.instantiate(builder)),
      }
    }),
  )
