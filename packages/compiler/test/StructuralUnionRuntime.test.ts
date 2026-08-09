import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Layout from '../src/Layout.js'
import * as Mir from '../src/Mir.js'
import * as Type from '../src/Type.js'
import * as TypeCompatibility from '../src/TypeCompatibility.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct A {}
struct B { value: I32 }
struct C { left: I32 right: I32 }
fn accept(value: A | B | C) -> I32 { return 42 }
fn widen(value: A | B) -> I32 { return accept(move value) }
pub fn main() -> I32 { return widen(A {}) }`

const expressions = (expression: Hir.Expression): ReadonlyArray<Hir.Expression> => {
  if (expression._tag === 'UnionConvert') {
    return Object.freeze([expression, ...expressions(expression.source)])
  }
  if (expression._tag === 'Move' || expression._tag === 'Project') {
    return Object.freeze([expression, ...expressions(expression.subject)])
  }
  if (expression._tag === 'IndexPlace') {
    return Object.freeze([
      expression,
      ...expressions(expression.subject),
      ...expressions(expression.index),
    ])
  }
  if (expression._tag === 'Construct') {
    return Object.freeze([
      expression,
      ...expression.fields.flatMap((field) => expressions(field.value)),
    ])
  }
  if (expression._tag === 'ArrayConstruct') {
    return Object.freeze([expression, ...expression.elements.flatMap(expressions)])
  }
  if (expression._tag === 'Call' || expression._tag === 'BuiltinCall') {
    return Object.freeze([expression, ...expression.arguments.flatMap(expressions)])
  }
  return Object.freeze([expression])
}

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

it.effect('lowers injection and widening through shared sum layouts and evaluates them', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource('unions/main', ascii(source), 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])

    const hir = Analysis.hirOf(self, 'unions/main')
    const conversions =
      hir?.functions.flatMap((fn) =>
        fn.statements.flatMap((statement) =>
          Hir.statementExpressions(statement).flatMap(expressions),
        ),
      ) ?? []
    assert.deepEqual(
      conversions
        .filter((expression) => expression._tag === 'UnionConvert')
        .map((expression) => expression.conversion),
      ['Widen', 'Inject'],
    )

    const layout = Analysis.layoutOf(self)
    assert.strictEqual(layout._tag, 'Available')
    if (layout._tag !== 'Available') return
    const wide = layout.value.entries.find(
      (entry) => Type.isUnion(entry.type) && entry.type.members.length === 3,
    )
    assert.strictEqual(wide?.representation._tag, 'Union')
    assert.deepEqual(wide === undefined ? undefined : [wide.size, wide.alignment], [12, 4])
    const shape = wide === undefined ? undefined : Layout.callingShape(layout.value, wide.type)
    assert.strictEqual(shape?.tree._tag, 'SumShape')
    assert.strictEqual(shape?.laneCount, 3)

    const mir = Analysis.loweredMir(self)
    assert.deepEqual(Mir.verify(mir), [])
    assert.deepEqual(
      mir.functions
        .flatMap(Mir.operations)
        .filter((operation) => operation._tag === 'ConvertUnion')
        .map((operation) => operation.conversion),
      ['Inject', 'Widen'],
    )

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42)
    assert.deepEqual(
      outcome.trace
        .filter((event) => event._tag === 'UnionConversion')
        .map((event) => event.conversion),
      ['Inject', 'Widen'],
    )

    const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') throw new RangeError('union program lost silk_main')
    assert.strictEqual(main(), 42)
  }),
)

it.effect('emits deterministic native union conversion artifacts', () =>
  Effect.gen(function* () {
    const first = yield* Analysis.ofSource('unions/main', ascii(source), 'aarch64-apple-darwin')
    const second = yield* Analysis.ofSource('unions/main', ascii(source), 'aarch64-apple-darwin')
    const left = yield* Analysis.codegen(first, { mode: 'release' })
    const right = yield* Analysis.codegen(second, { mode: 'release' })
    assert.deepEqual(left.bitcode, right.bitcode)
    assert.strictEqual(left.ir, right.ir)
    assert.include(left.ir, 'union')
  }),
)

it.effect('transports unions through returns, arrays, structs, and replacement', () =>
  Effect.gen(function* () {
    const aggregateSource = `struct A {}
struct B { value: I32 }
struct Box { value: A | B }
fn make() -> A | B { return A {} }
fn accept(values: [A | B; 2]) -> I32 { return 42 }
pub fn main() -> I32 {
  let mut box = Box { value: make() }
  box.value = B { value: 7 }
  return accept([A {}, B { value: 42 }])
}`
    const self = yield* Analysis.ofSource(
      'union-aggregate/main',
      ascii(aggregateSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 42)
      assert.deepEqual(
        outcome.trace
          .flatMap((event) => (event._tag === 'ReplacementCleanup' ? (event.members ?? []) : []))
          .map(Type.encode),
        ['union-aggregate/main.A'],
      )
      assert.deepEqual(
        outcome.trace
          .flatMap((event) => (event._tag === 'Cleanup' ? (event.members ?? []) : []))
          .map(Type.encode),
        ['union-aggregate/main.A', 'union-aggregate/main.B', 'union-aggregate/main.B'],
      )
    }
    const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') throw new RangeError('aggregate union program lost silk_main')
    assert.strictEqual(main(), 42)
  }),
)

it.effect('diagnoses narrowing and non-containing union targets deterministically', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'union-invalid/main',
      ascii(`struct A {}
struct B {}
struct C {}
fn narrow(value: A | B) -> A { return move value }
fn accept(value: A | B) -> I32 { return 0 }
pub fn main() -> I32 { return accept(C {}) }`),
    )
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0040', 'SEM0040'],
    )
  }),
)
