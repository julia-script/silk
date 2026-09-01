import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Binary from '../src/Binary.js'
import * as Builder from '../src/Builder.js'
import * as Export from '../src/Export.js'
import * as Func from '../src/Func.js'
import * as Instr from '../src/Instr.js'
import * as Utf8 from '../src/internal/Utf8.js'
import * as Type from '../src/Type.js'
import * as Validate from '../src/Validate.js'
import * as ValType from '../src/ValType.js'

const header = [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]
/** One type `() -> i32`, one function of that type, and a code section holding one body. */
const sections = (body: ReadonlyArray<number>): ReadonlyArray<number> => [
  0x01,
  0x05,
  0x01,
  0x60,
  0x00,
  0x01,
  0x7f,
  0x03,
  0x02,
  0x01,
  0x00,
  0x0a,
  body.length + 2,
  0x01,
  body.length,
  ...body,
]

/** A `name` custom section naming function 0, the way an encoder carries declared names. */
const named = (name: string): ReadonlyArray<number> => {
  const bytes = Array.from(Utf8.encode(name))
  const subsection = [0x01, 0x00, bytes.length, ...bytes]
  const payload = [0x04, 0x6e, 0x61, 0x6d, 0x65, 0x01, subsection.length, ...subsection]
  return [0x00, payload.length, ...payload]
}

/** Locals count, then `i32.const 1`, then `end`. */
const returnsI32 = [0x00, 0x41, 0x01, 0x0b]
/** The same body returning an `i64`, which the declared result type does not accept. */
const returnsI64 = [0x00, 0x42, 0x01, 0x0b]

it.effect('accepts a module the host compiles', () =>
  Effect.gen(function* () {
    const bytes = Uint8Array.from([...header, ...sections(returnsI32)])
    assert.deepEqual(yield* Validate.validate(bytes), [])
  }),
)

it.effect('accepts a module the builder encoded', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const type = yield* Type.func(builder, [], [ValType.i32])
    const fn = yield* Func.declare(builder, type, { name: 'answer' })
    yield* Func.define(builder, fn, { body: [Instr.i32Const(42)] })
    yield* Export.func(builder, 'answer', fn)
    assert.deepEqual(yield* Validate.validate(yield* Binary.encode(builder)), [])
  }),
)

it.effect('reports the host diagnostic for a body the host rejects', () =>
  Effect.gen(function* () {
    const bytes = Uint8Array.from([...header, ...sections(returnsI64)])
    const violations = yield* Validate.validate(bytes)
    assert.strictEqual(violations.length, 1)
    assert.strictEqual(violations.at(0)?.function, '#0')
    assert.include(violations.at(0)?.message ?? '', 'i64')
    assert.include(Validate.format(violations), '(in #0)')
  }),
)

it.effect('names the offending function when the module names it', () =>
  Effect.gen(function* () {
    const bytes = Uint8Array.from([...header, ...sections(returnsI64), ...named('answer')])
    const violations = yield* Validate.validate(bytes)
    assert.strictEqual(violations.length, 1)
    assert.strictEqual(violations.at(0)?.function, 'answer')
  }),
)

it.effect('reports bytes that are not a module at all', () =>
  Effect.gen(function* () {
    const violations = yield* Validate.validate(Uint8Array.from([0x00, 0x61, 0x73, 0x6d]))
    assert.strictEqual(violations.length, 1)
    assert.strictEqual(violations.at(0)?.function, 'module')
  }),
)
