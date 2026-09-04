import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** The example from #100, verbatim: a plain `fn` calls through a function-typed parameter. */
const concrete = `fn double(value: i32) -> i32 { return value * 2 }

fn apply(transform: once fn(i32) -> i32, value: i32) -> i32 {
  return transform(value)
}

pub fn main() -> i32 { return apply(double, 21) }`

const callableTargetName = (type: Mir.Type | undefined): string | undefined =>
  type?._tag === 'CallableValue' && type.target._tag === 'DeclarationCallableTarget'
    ? type.target.declaration.name
    : undefined

it.effect('keeps the callable parameter in the lowered contract', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'indirect-call/contract',
      ascii(concrete),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const apply = Analysis.loweredMir(snapshot).functions.find((fn) => fn.id.name === 'apply')
    assert.isDefined(apply)
    if (apply === undefined) return
    // The regression in #100 was a contract whose parameter count outran its typed locals: the
    // callable slot was dropped and `value: i32` shifted into position 0.
    assert.strictEqual(apply.parameterCount, 2)
    assert.isAtLeast(apply.localTypes.length, apply.parameterCount)
    assert.strictEqual(callableTargetName(apply.localTypes.at(0)), 'double')
    assert.strictEqual(apply.localTypes.at(1)?._tag, 'i32')
  }),
)

it.effect('still holds a `once` callable to a single call', () =>
  Effect.gen(function* () {
    const source = `fn double(value: i32) -> i32 { return value * 2 }

fn twice(transform: once fn(i32) -> i32, value: i32) -> i32 {
  return transform(transform(value))
}

pub fn main() -> i32 { return twice(double, 10) + 2 }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'indirect-call/affine',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    // Lowering an indirect call must not weaken the affinity of the callable it goes through.
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['OWN0001'],
    )
  }),
)

it.effect('still requires an explicit move to forward a `once` callable', () =>
  Effect.gen(function* () {
    const source = `fn double(value: i32) -> i32 { return value * 2 }

fn inner(transform: once fn(i32) -> i32, value: i32) -> i32 { return transform(value) }

fn outer(transform: once fn(i32) -> i32, value: i32) -> i32 {
  return inner(transform, value)
}

pub fn main() -> i32 { return outer(double, 21) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'indirect-call/forward-move',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['OWN0003'],
    )
  }),
)
