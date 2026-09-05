import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as LayoutEncode from '../src/LayoutEncode.js'
import * as LayoutVerify from '../src/LayoutVerify.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const storedLayout = Effect.fnUntraced(function* (
  name: string,
  source: string,
  target: Target.Target,
) {
  const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), target.id)
  assert.deepEqual(Analysis.diagnostics(snapshot), [])
  const catalog = Layout.catalog(target, snapshot.index, snapshot.instances)
  return Layout.plan(catalog, snapshot.instances, snapshot.index)
})

const representedEntry = (plan: Layout.Plan): Layout.Entry =>
  plan.entries.find((entry) => Type.isRepresented(entry.type)) ??
  unreachable('expected one represented callable layout')

it.effect('lays out a represented callable only through its complete nominal', () =>
  Effect.gen(function* () {
    const source = `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  return parser.parse(1)
}`
    const plan = yield* storedLayout(
      'stored-callable-layout/named',
      source,
      Target.wasm32UnknownUnknown,
    )
    const represented = representedEntry(plan)
    const parser =
      plan.entries.find((entry) => Type.isNominal(entry.type) && entry.type.name === 'Parser') ??
      unreachable('expected Parser layout')

    assert.strictEqual(represented.representation._tag, 'CallableEnvironment')
    if (represented.representation._tag !== 'CallableEnvironment') return
    assert.deepEqual(represented.representation.fields, [])
    assert.strictEqual(represented.size, 0)
    assert.strictEqual(represented.alignment, 1)
    assert.strictEqual(parser.representation._tag, 'Aggregate')
    if (parser.representation._tag !== 'Aggregate') return
    assert.strictEqual(parser.representation.fields.at(0)?.size, 0)
    assert.deepEqual(LayoutVerify.verify(plan), [])
  }),
)

it.effect('places Copy and owned captures inline in declaration order', () =>
  Effect.gen(function* () {
    const source = `struct Token { value: i32 }
struct Parser<F: once fn(i32) -> i32> { parse: F }
fn decode(value: i32, token: Token) -> i32 { drop move token return value }
pub fn main() -> i32 {
  let token = Token { value: 7 }
  let parser = Parser { parse: decode(move token) }
  return 0
}`
    for (const target of [Target.wasm32UnknownUnknown, Target.aarch64AppleDarwin]) {
      const plan = yield* storedLayout('stored-callable-layout/owned', source, target)
      const represented = representedEntry(plan)
      assert.strictEqual(represented.representation._tag, 'CallableEnvironment')
      if (represented.representation._tag !== 'CallableEnvironment') continue
      assert.deepEqual(
        represented.representation.fields.map((field) => ({
          ordinal: field.ordinal,
          access: field.access,
          representation: field.representation,
          offset: field.offset,
          size: field.size,
        })),
        [{ ordinal: 0, access: 'Take', representation: 'Value', offset: 0, size: 4 }],
      )
      assert.strictEqual(represented.size, 4)
      assert.strictEqual(represented.alignment, 4)
      assert.deepEqual(LayoutVerify.verify(plan), [])
    }
  }),
)

it.effect('keeps nested nominal layouts deterministic', () =>
  Effect.gen(function* () {
    const source = `struct Parser<'env, F: fn<'env>(i32) -> i32> { parse: F }
struct Pair<'env, F: fn<'env>(i32) -> i32> { first: Parser<'env, F> second: Parser<'env, F> }
fn decode(value: i32) -> i32 { return value }
fn pair<'env, F: fn<'env>(i32) -> i32>(first: Parser<'env, F>, second: Parser<'env, F>) -> Pair<'env, F> {
  return Pair<'env, F> { first: move first, second: move second }
}
pub fn main() -> i32 {
  let first = Parser { parse: decode }
  let second = Parser { parse: decode }
  let paired = pair(move first, move second)
  return 0
}`
    const first = yield* storedLayout(
      'stored-callable-layout/nested',
      source,
      Target.wasm32UnknownUnknown,
    )
    const second = yield* storedLayout(
      'stored-callable-layout/nested',
      source,
      Target.wasm32UnknownUnknown,
    )
    const represented = representedEntry(first)
    assert.strictEqual(represented.representation._tag, 'CallableEnvironment')
    if (represented.representation._tag !== 'CallableEnvironment') return
    assert.deepEqual(represented.representation.fields, [])
    assert.strictEqual(LayoutEncode.encode(first), LayoutEncode.encode(second))
    assert.deepEqual(LayoutVerify.verify(first), [])
  }),
)

it.effect('stores borrowed captures as target-sized inline addresses', () =>
  Effect.gen(function* () {
    const source = `struct Holder<F: mut fn(i32) -> i32> { step: F }
fn read(value: i32, values: &mut [i32]) -> i32 { return value }
pub fn main() -> i32 {
  let mut values = [1]
  let holder = Holder { step: read(&mut values) }
  return 0
}`
    for (const target of [Target.wasm32UnknownUnknown, Target.aarch64AppleDarwin]) {
      const plan = yield* storedLayout('stored-callable-layout/borrowed', source, target)
      const represented = representedEntry(plan)
      assert.strictEqual(represented.representation._tag, 'CallableEnvironment')
      if (represented.representation._tag !== 'CallableEnvironment') continue
      // A slice capture is already a stable borrow descriptor and is stored inline.
      assert.strictEqual(represented.representation.fields.at(0)?.representation, 'Value')
      assert.strictEqual(represented.size, target.pointerSize * 2)
      assert.strictEqual(represented.alignment, target.pointerAlignment)
      assert.deepEqual(LayoutVerify.verify(plan), [])
    }
  }),
)
