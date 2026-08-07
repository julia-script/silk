/**
 * Differential test: every scalar operator, interpreter vs native WebAssembly.
 *
 * One module exports one function per operator (`(params) -> result` wrapping the bare
 * instruction). The same module is executed by `Interp` and by the host's WebAssembly engine
 * over a cross product of edge-case operands, and every outcome — value or trap — must agree.
 * `Object.is` does the comparing, so NaN agrees with NaN and `-0` disagrees with `0`.
 */
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Binary from '../src/Binary.js'
import * as Builder from '../src/Builder.js'
import * as ExportActor from '../src/Export.js'
import * as Func from '../src/Func.js'
import * as Instr from '../src/Instr.js'
import * as Interp from '../src/Interp.js'
import * as Type from '../src/Type.js'
import * as ValType from '../src/ValType.js'

type Shape = 'i' | 'l' | 'f' | 'd'

interface Group {
  readonly params: ReadonlyArray<Shape>
  readonly result: Shape
  readonly ops: ReadonlyArray<Instr.PlainMnemonic>
}

// prettier-ignore
const groups: ReadonlyArray<Group> = [
  {
    params: ['i', 'i'],
    result: 'i',
    ops: [
      'i32.add',
      'i32.sub',
      'i32.mul',
      'i32.div_s',
      'i32.div_u',
      'i32.rem_s',
      'i32.rem_u',
      'i32.and',
      'i32.or',
      'i32.xor',
      'i32.shl',
      'i32.shr_s',
      'i32.shr_u',
      'i32.rotl',
      'i32.rotr',
      'i32.eq',
      'i32.ne',
      'i32.lt_s',
      'i32.lt_u',
      'i32.gt_s',
      'i32.gt_u',
      'i32.le_s',
      'i32.le_u',
      'i32.ge_s',
      'i32.ge_u',
    ],
  },
  {
    params: ['i'],
    result: 'i',
    ops: ['i32.clz', 'i32.ctz', 'i32.popcnt', 'i32.eqz', 'i32.extend8_s', 'i32.extend16_s'],
  },
  {
    params: ['l', 'l'],
    result: 'l',
    ops: [
      'i64.add',
      'i64.sub',
      'i64.mul',
      'i64.div_s',
      'i64.div_u',
      'i64.rem_s',
      'i64.rem_u',
      'i64.and',
      'i64.or',
      'i64.xor',
      'i64.shl',
      'i64.shr_s',
      'i64.shr_u',
      'i64.rotl',
      'i64.rotr',
    ],
  },
  {
    params: ['l', 'l'],
    result: 'i',
    ops: [
      'i64.eq',
      'i64.ne',
      'i64.lt_s',
      'i64.lt_u',
      'i64.gt_s',
      'i64.gt_u',
      'i64.le_s',
      'i64.le_u',
      'i64.ge_s',
      'i64.ge_u',
    ],
  },
  {
    params: ['l'],
    result: 'l',
    ops: ['i64.clz', 'i64.ctz', 'i64.popcnt', 'i64.extend8_s', 'i64.extend16_s', 'i64.extend32_s'],
  },
  { params: ['l'], result: 'i', ops: ['i64.eqz'] },
  {
    params: ['d', 'd'],
    result: 'd',
    ops: ['f64.add', 'f64.sub', 'f64.mul', 'f64.div', 'f64.min', 'f64.max', 'f64.copysign'],
  },
  {
    params: ['d'],
    result: 'd',
    ops: ['f64.abs', 'f64.neg', 'f64.ceil', 'f64.floor', 'f64.trunc', 'f64.nearest', 'f64.sqrt'],
  },
  {
    params: ['d', 'd'],
    result: 'i',
    ops: ['f64.eq', 'f64.ne', 'f64.lt', 'f64.gt', 'f64.le', 'f64.ge'],
  },
  {
    params: ['f', 'f'],
    result: 'f',
    ops: ['f32.add', 'f32.sub', 'f32.mul', 'f32.div', 'f32.min', 'f32.max', 'f32.copysign'],
  },
  {
    params: ['f'],
    result: 'f',
    ops: ['f32.abs', 'f32.neg', 'f32.ceil', 'f32.floor', 'f32.trunc', 'f32.nearest', 'f32.sqrt'],
  },
  {
    params: ['f', 'f'],
    result: 'i',
    ops: ['f32.eq', 'f32.ne', 'f32.lt', 'f32.gt', 'f32.le', 'f32.ge'],
  },
  // conversions
  { params: ['l'], result: 'i', ops: ['i32.wrap_i64'] },
  {
    params: ['f'],
    result: 'i',
    ops: [
      'i32.trunc_f32_s',
      'i32.trunc_f32_u',
      'i32.trunc_sat_f32_s',
      'i32.trunc_sat_f32_u',
      'i32.reinterpret_f32',
    ],
  },
  {
    params: ['d'],
    result: 'i',
    ops: ['i32.trunc_f64_s', 'i32.trunc_f64_u', 'i32.trunc_sat_f64_s', 'i32.trunc_sat_f64_u'],
  },
  { params: ['i'], result: 'l', ops: ['i64.extend_i32_s', 'i64.extend_i32_u'] },
  {
    params: ['f'],
    result: 'l',
    ops: ['i64.trunc_f32_s', 'i64.trunc_f32_u', 'i64.trunc_sat_f32_s', 'i64.trunc_sat_f32_u'],
  },
  {
    params: ['d'],
    result: 'l',
    ops: [
      'i64.trunc_f64_s',
      'i64.trunc_f64_u',
      'i64.trunc_sat_f64_s',
      'i64.trunc_sat_f64_u',
      'i64.reinterpret_f64',
    ],
  },
  {
    params: ['i'],
    result: 'f',
    ops: ['f32.convert_i32_s', 'f32.convert_i32_u', 'f32.reinterpret_i32'],
  },
  { params: ['l'], result: 'f', ops: ['f32.convert_i64_s', 'f32.convert_i64_u'] },
  { params: ['d'], result: 'f', ops: ['f32.demote_f64'] },
  { params: ['i'], result: 'd', ops: ['f64.convert_i32_s', 'f64.convert_i32_u'] },
  {
    params: ['l'],
    result: 'd',
    ops: ['f64.convert_i64_s', 'f64.convert_i64_u', 'f64.reinterpret_i64'],
  },
  { params: ['f'], result: 'd', ops: ['f64.promote_f32'] },
]

const valTypeFor = (shape: Shape): ValType.ValType =>
  shape === 'i'
    ? ValType.i32
    : shape === 'l'
      ? ValType.i64
      : shape === 'f'
        ? ValType.f32
        : ValType.f64

const i32Operands: ReadonlyArray<number> = [
  0, 1, -1, 2, -2, 5, 31, 32, 33, 63, 64, -64, 2147483647, -2147483648, 1000000007, -1000000007,
]

const i64Operands: ReadonlyArray<bigint> = [
  0n,
  1n,
  -1n,
  2n,
  63n,
  64n,
  -64n,
  9223372036854775807n,
  -9223372036854775808n,
  81985529216486895n,
  -81985529216486895n,
  4294967296n,
]

const f64Operands: ReadonlyArray<number> = [
  0,
  -0,
  1,
  -1,
  0.5,
  -0.5,
  1.5,
  2.5,
  -2.5,
  3.5,
  Math.PI,
  Number.NaN,
  Number.POSITIVE_INFINITY,
  Number.NEGATIVE_INFINITY,
  1e308,
  5e-324,
  4503599627370495.5,
  2 ** 31,
  2 ** 63,
  -(2 ** 31) - 0.7,
]

const operandsFor = (shape: Shape): ReadonlyArray<number | bigint> => {
  switch (shape) {
    case 'i':
      return i32Operands
    case 'l':
      return i64Operands
    case 'f':
      return f64Operands.map(Math.fround)
    case 'd':
      return f64Operands
  }
}

type Outcome = { readonly _tag: 'Value'; readonly value: unknown } | { readonly _tag: 'Trap' }

const sameOutcome = (a: Outcome, b: Outcome): boolean =>
  a._tag === 'Trap' ? b._tag === 'Trap' : b._tag === 'Value' && Object.is(a.value, b.value)

const show = (outcome: Outcome): string =>
  outcome._tag === 'Trap' ? 'trap' : String(outcome.value)

it.effect('agrees with native WebAssembly on every scalar operator', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    for (const group of groups) {
      const signature = yield* Type.func(builder, group.params.map(valTypeFor), [
        valTypeFor(group.result),
      ])
      for (const op of group.ops) {
        const fn = yield* Func.declare(builder, signature)
        yield* Func.define(builder, fn, {
          body: [...group.params.map((_, index) => Instr.localGet(index)), Instr.op(op)],
        })
        yield* ExportActor.func(builder, op, fn)
      }
    }

    const bytes = yield* Binary.encode(builder)
    const native = yield* Effect.promise(() =>
      WebAssembly.instantiate(bytes.slice().buffer as ArrayBuffer),
    )
    const exports = native.instance.exports as Record<string, CallableFunction>
    const instance = yield* Interp.instantiate(builder)

    const mismatches: Array<string> = []
    let cases = 0
    for (const group of groups) {
      const argLists: ReadonlyArray<ReadonlyArray<number | bigint>> =
        group.params.length === 1
          ? operandsFor(group.params[0] as Shape).map((value) => [value])
          : operandsFor(group.params[0] as Shape).flatMap((a) =>
              operandsFor(group.params[1] as Shape).map((b) => [a, b]),
            )
      for (const op of group.ops) {
        const nativeFn = exports[op]
        assert.isDefined(nativeFn, op)
        for (const args of argLists) {
          cases += 1
          let expected: Outcome
          try {
            expected = { _tag: 'Value', value: nativeFn?.(...args) }
          } catch {
            expected = { _tag: 'Trap' }
          }
          const result = yield* Effect.result(Interp.invoke(instance, op, args))
          const actual: Outcome =
            result._tag === 'Success'
              ? { _tag: 'Value', value: result.success[0] }
              : { _tag: 'Trap' }
          if (!sameOutcome(actual, expected)) {
            mismatches.push(
              `${op}(${args.join(', ')}) → interp ${show(actual)}, native ${show(expected)}`,
            )
          }
        }
      }
    }

    assert.isAbove(cases, 10_000)
    assert.deepStrictEqual(mismatches.slice(0, 20), [])
    assert.strictEqual(mismatches.length, 0)
  }),
)
