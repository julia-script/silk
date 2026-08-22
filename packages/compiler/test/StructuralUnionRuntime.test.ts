import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Layout from '../src/Layout.js'
import * as Mir from '../src/Mir.js'
import * as Type from '../src/Type.js'
import * as TypeCompatibility from '../src/TypeCompatibility.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct A {}
struct B { value: i32 }
struct C { left: i32 right: i32 }
fn accept(value: A | B | C) -> i32 { return 42 }
fn widen(value: A | B) -> i32 { return accept(move value) }
pub fn main() -> i32 { return widen(A {}) }`

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
    const self = yield* Analysis.ofSourceRealized(
      'unions/main',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])

    const hir = Projections.hirOf(self, 'unions/main')
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
    assert.strictEqual(outcome.result.value, 42n)
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

it.effect('transports unions through returns, arrays, structs, and replacement', () =>
  Effect.gen(function* () {
    const aggregateSource = `struct A {}
struct B { value: i32 }
struct Box { value: A | B }
fn make() -> A | B { return A {} }
fn accept(values: [A | B; 2]) -> i32 { return 42 }
pub fn main() -> i32 {
  let mut box = Box { value: make() }
  box.value = B { value: 7 }
  return accept([A {}, B { value: 42 }])
}`
    const self = yield* Analysis.ofSourceRealized(
      'union-aggregate/main',
      ascii(aggregateSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 42n)
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

it.effect('transports finite executable representations as ordinary union members', () =>
  Effect.gen(function* () {
    const programs = Object.freeze([
      Object.freeze({
        name: 'callable',
        source: `fn add(left: i32, right: i32) -> i32 { return left + right }
fn selected() -> typeof(add) | i32 { return add }
pub fn main() -> i32 { drop selected() return 42 }`,
      }),
      Object.freeze({
        name: 'effect',
        source: `fn selected() -> some<F: Effect<i32>> F | i32 {
  return effect { return 42 }
}
pub fn main() -> i32 { drop selected() return 42 }`,
      }),
    ])
    for (const program of programs) {
      const self = yield* Analysis.ofSourceRealized(
        `union-executable/${program.name}`,
        ascii(program.source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
        [],
        program.name,
      )
      const executableConversion = Projections.hirOf(self, `union-executable/${program.name}`)
        ?.functions.flatMap((fn) => fn.statements.flatMap(Hir.statementExpressions))
        .flatMap(expressions)
        .find(
          (expression) =>
            expression._tag === 'UnionConvert' &&
            expression.mappings.some(
              (mapping) => Type.isRepresented(mapping.source) && Type.isRepresented(mapping.target),
            ),
        )
      assert.strictEqual(executableConversion?._tag, 'UnionConvert', program.name)
      const layout = Analysis.layoutOf(self)
      assert.strictEqual(layout._tag, 'Available', program.name)
      if (layout._tag === 'Available') {
        const executableUnion = layout.value.entries.find(
          (entry) => Type.isUnion(entry.type) && entry.type.members.some(Type.isRepresented),
        )
        assert.isDefined(executableUnion, program.name)
        assert.strictEqual(
          executableUnion === undefined
            ? undefined
            : Layout.callingShape(layout.value, executableUnion.type)?.tree._tag,
          'SumShape',
          program.name,
        )
      }
      assert.deepEqual(Mir.verify(Analysis.loweredMir(self)), [], program.name)
      const outcome = Analysis.evaluate(self)
      assert.strictEqual(outcome._tag, 'Completed', program.name)
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n, program.name)
      const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42, program.name)
    }
  }),
)

it.effect('derives scalar and droppable-array behavior from the normalized member plan', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-array/main',
      ascii(`struct Token { value: i32 }
impl Drop for Token { fn drop(self: &mut Token) -> () { return () } }
fn accept(value: i32 | [Token; 2]) -> i32 { drop value return 42 }
pub fn main() -> i32 {
  return accept([Token { value: 1 }, Token { value: 2 }])
}`),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(Mir.verify(mir), [])
    const unionCleanup = mir.functions
      .flatMap(Mir.operations)
      .flatMap((operation) =>
        operation._tag === 'Drop' && operation.cleanup._tag === 'UnionCleanup'
          ? [operation.cleanup]
          : [],
      )
      .at(0)
    const arrayCase = unionCleanup?.cases.find((entry) => Type.isFixedArray(entry.member))
    assert.strictEqual(arrayCase?.cleanup._tag, 'ArrayCleanup')
    if (arrayCase?.cleanup._tag === 'ArrayCleanup')
      assert.strictEqual(arrayCase.cleanup.element._tag, 'HookCleanup')

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
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

/**
 * A union's payload slot is as wide as its widest member, so a narrower member and the slot holding
 * it are different wasm value types. The Wasm backend used to move one into the other unchanged,
 * which is not a valid instruction sequence, so every such program failed to emit while the
 * evaluator and the native backend ran it correctly — `Result<u64, E>` for any `E` narrower than
 * `u64` is that shape on a 32-bit target. The bits are the same bits either way, so the transfer
 * bridges the containers and every engine now agrees.
 */
const widened = `import silk.i32 as i32
import silk.u64 as u64
import silk.u8 as u8
struct Wide { value: u64 }
struct Narrow { code: u8 }
struct Holder { value: Wide | Narrow }

fn hold(selector: i32) -> Holder {
  if selector == 0 { return Holder { value: Wide { value: u64.MAX } } }
  return Holder { value: Narrow { code: i32.toU8(7) } }
}

fn read(selector: i32) -> i32 {
  return match move hold(selector) {
    Holder { value: outcome } => match move outcome {
      Wide { value } => u64.toI32(u64.remainder(u64.shiftRight(value, i32.toU64(32)), i32.toU64(1000)))
      Narrow { code } => u8.toI32(code)
    }
  }
}

pub fn main() -> i32 { return read(0) * 100 + read(1) }`

it.effect('carries a member narrower than its union slot through the Wasm backend', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-widened/main',
      ascii(widened),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 29507n)

    const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 29507)
  }),
)
