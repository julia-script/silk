import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Type from '../src/Type.js'
import * as TypeCompatibility from '../src/TypeCompatibility.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct A {}
struct B { value: i32 }
struct C { left: i32 right: i32 }
fn accept(value: A | B | C) -> i32 { return 42 }
fn widen(value: A | B) -> i32 { return accept(move value) }
pub fn main() -> i32 { return widen(A {}) }`

it('computes canonical total member mappings', () => {
  const a = Type.nominal('unions/main', 'A')
  const b = Type.nominal('unions/main', 'B')
  const c = Type.nominal('unions/main', 'C')
  const narrow = Type.union([b, a])
  const wide = Type.union([c, a, b])
  assert.strictEqual(narrow._tag, 'Normalized')
  assert.strictEqual(wide._tag, 'Normalized')
  if (
    narrow._tag !== 'Normalized' ||
    wide._tag !== 'Normalized' ||
    !Type.isUnion(narrow.type) ||
    !Type.isUnion(wide.type)
  ) {
    return
  }
  const injection = TypeCompatibility.check(a, narrow.type)
  const widening = TypeCompatibility.check(narrow.type, wide.type)
  const narrowing = TypeCompatibility.check(wide.type, narrow.type)
  assert.strictEqual(injection._tag, 'Inject')
  assert.strictEqual(widening._tag, 'Widen')
  assert.strictEqual(narrowing._tag, 'Incompatible')
  if (widening._tag === 'Widen') {
    assert.deepEqual(
      widening.mappings.map((mapping) => [mapping.sourceOrdinal, mapping.targetOrdinal]),
      [
        [0, 0],
        [1, 1],
      ],
    )
  }
})

it.effect('emits deterministic native union conversion artifacts', () =>
  Effect.gen(function* () {
    const first = yield* Analysis.ofSourceRealized(
      'unions/main',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const second = yield* Analysis.ofSourceRealized(
      'unions/main',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const left = yield* Analysis.codegen(first, { mode: 'release' })
    const right = yield* Analysis.codegen(second, { mode: 'release' })
    assert.deepEqual(left.bitcode, right.bitcode)
    assert.strictEqual(left.ir, right.ir)
    assert.include(left.ir, 'union')
  }),
)

it.effect('diagnoses narrowing and non-containing union targets deterministically', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-invalid/main',
      ascii(`struct A {}
struct B {}
struct C {}
fn narrow(value: A | B) -> A { return move value }
fn accept(value: A | B) -> i32 { return 0 }
pub fn main() -> i32 { return accept(C {}) }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0040', 'SEM0040'],
    )
  }),
)
