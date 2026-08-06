import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Layout from '../src/Layout.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct Pair { left: I32 right: I32 }
fn make() -> Pair { return Pair { right: 2, left: 1 } }
fn pass(value: Pair) -> Pair { return move value }
pub fn main() -> I32 { let pair = pass(make()) return pair.left + pair.right }`

const snapshot = (target?: string) => Analysis.ofSource('struct-values/main', ascii(source), target)

const multiSnapshot = (rootModule: string, sources: ReadonlyMap<string, Uint8Array>) => {
  const root = sources.get(rootModule)
  if (root === undefined) throw new RangeError(`Fixture has no root source ${rootModule}`)
  return Analysis.make({ root: SourceFile.make(rootModule, root) }).pipe(
    Effect.provide(
      SourceResolver.memory(new Map([...sources].filter(([module]) => module !== rootModule))),
    ),
  )
}

it.effect(
  'retains source field order while elaborating canonical construction and projection',
  () =>
    Effect.gen(function* () {
      const self = yield* snapshot()
      const make = Analysis.rootAnalysis(self).functions.at(0)
      const main = Analysis.rootAnalysis(self).functions.at(2)
      const literal = make?.returnedExpression

      assert.strictEqual(literal?._tag, 'StructLiteral')
      if (literal?._tag !== 'StructLiteral') return
      assert.deepEqual(
        literal.initializers.map((initializer) => initializer.name),
        ['right', 'left'],
      )
      assert.deepEqual(
        literal.fields.map(({ field }) =>
          field.name._tag === 'Present' ? field.name.spelling : 'unavailable',
        ),
        ['left', 'right'],
      )
      assert.strictEqual(main?.returnedExpression._tag, 'Operator')
      assert.deepEqual(Analysis.diagnostics(self), [])

      const makeHir = Analysis.rootAnalysis(self).hir.functions.at(0)
      assert.strictEqual(
        makeHir === undefined ? undefined : Hir.returned(makeHir)._tag,
        'Construct',
      )
      const mainHir = Analysis.rootAnalysis(self).hir.functions.at(2)
      const returned = mainHir === undefined ? undefined : Hir.returned(mainHir)
      assert.strictEqual(returned?._tag, 'BuiltinCall')
      if (returned?._tag === 'BuiltinCall') {
        for (const argument of returned.arguments) {
          assert.strictEqual(argument._tag, 'Project')
          if (argument._tag === 'Project') assert.strictEqual(argument.access, 'CopyRead')
        }
      }
    }),
)

it.effect('evaluates initializers in source order before constructing in declaration order', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'struct-values/evaluation-order',
      ascii(`struct Pair { left: I32 right: I32 }
fn left() -> I32 { return 1 }
fn right() -> I32 { return 2 }
fn make() -> Pair { return Pair { right: right(), left: left() } }
pub fn main() -> I32 { let pair = make() return pair.left }`),
      'wasm32-unknown-unknown',
    )
    const make = Analysis.loweredMir(self).functions.find((fn) => fn.id.name === 'make')
    const calls =
      make === undefined
        ? []
        : Mir.operations(make).flatMap((operation) =>
            operation._tag === 'Call' ? [operation.target.name] : [],
          )

    assert.deepEqual(calls, ['right', 'left'])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(self)), [])
  }),
)

it.effect('plans canonical aggregate lanes and evaluates whole-value calls and projections', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('wasm32-unknown-unknown')
    const layout = Analysis.layoutOf(self)
    assert.strictEqual(layout._tag, 'Available')
    if (layout._tag !== 'Available') return
    const pair = layout.value.entries.find(
      (entry) => Layout.encode(layout.value).includes('Pair') && entry.type !== 'I32',
    )
    assert.notStrictEqual(pair, undefined)
    const shape = pair === undefined ? undefined : Layout.callingShape(layout.value, pair.type)
    assert.deepEqual(
      shape?.lanes.map((lane) =>
        lane.path.map((selector) =>
          selector._tag === 'ElementSelector'
            ? `[${selector.index}]`
            : selector._tag === 'FieldId'
              ? selector.ordinal
              : selector._tag === 'UnionTagSelector'
                ? 'tag'
                : `payload:${selector.slot}`,
        ),
      ),
      [[0], [1]],
    )

    const mir = Analysis.loweredMir(self)
    assert.deepEqual(Mir.verify(mir), [])
    assert.include(Mir.encode(mir), 'construct struct-values/main.Pair')
    assert.include(Mir.encode(mir), 'read-place')

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 3)
    assert.include(
      outcome.trace.map((event) => event._tag),
      'Construct',
    )
    assert.include(
      outcome.trace.map((event) => event._tag),
      'PlaceRead',
    )
  }),
)

it.effect('keeps LLVM, WebAssembly, and evaluation in aggregate parity', () =>
  Effect.gen(function* () {
    const native = yield* snapshot('aarch64-apple-darwin')
    const wasm = yield* snapshot('wasm32-unknown-unknown')
    const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
    const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(
      new WebAssembly.Module(wasmArtifact.bitcode.slice()),
      {},
    )
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(Analysis.evaluate(native)._tag, 'Completed')
    assert.strictEqual(main(), 3)
    assert.include(nativeArtifact.ir, 'define i32 @silk_main')
    assert.include(nativeArtifact.ir, 'extractvalue')
    assert.include(wasmArtifact.ir, '(result i32 i32)')
  }),
)

it.effect('preserves empty nominal contracts with zero runtime lanes', () =>
  Effect.gen(function* () {
    const text = `struct End {}
fn end() -> End { return End {} }
fn consume(value: End) -> I32 { return 7 }
pub fn main() -> I32 { return consume(end()) }`
    const self = yield* Analysis.ofSource(
      'struct-values/empty',
      ascii(text),
      'wasm32-unknown-unknown',
    )
    const layout = Analysis.layoutOf(self)
    assert.strictEqual(layout._tag, 'Available')
    if (layout._tag !== 'Available') return
    const empty = layout.value.entries.find((entry) => typeof entry.type !== 'string')
    assert.strictEqual(
      empty === undefined ? undefined : Layout.callingShape(layout.value, empty.type)?.lanes.length,
      0,
    )
    assert.deepEqual(Mir.verify(Analysis.loweredMir(self)), [])
    assert.strictEqual(Analysis.evaluate(self)._tag, 'Completed')

    const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bitcode.slice()), {})
    const main = instance.exports.silk_main as () => number
    assert.strictEqual(main(), 7)
  }),
)

it.effect('keeps invalid construction, projection, and partial moves phase-owned', () =>
  Effect.gen(function* () {
    const invalid = yield* Analysis.ofSource(
      'struct-values/invalid',
      ascii(`struct Pair { left: I32 right: I32 }
struct Outer { pair: Pair }
fn missing() -> Pair { return Pair { left: 1 } }
fn duplicate() -> Pair { return Pair { left: 1, left: 2, right: 3 } }
fn unknown() -> Pair { return Pair { left: 1, right: 2, extra: 3 } }
fn mistyped() -> Pair { return Pair { left: true, right: 2 } }
fn partial(value: Outer) -> Pair { return move value.pair }
pub fn main() -> I32 { return 0 }`),
    )

    assert.deepEqual(
      Analysis.diagnostics(invalid).map((diagnostic) => diagnostic.code),
      ['SEM0024', 'SEM0023', 'SEM0022', 'SEM0025', 'OWN0002'],
    )
    const functions = Analysis.rootAnalysis(invalid).functions
    for (const ordinal of [0, 1, 2, 3]) {
      assert.strictEqual(functions.at(ordinal)?.returnedExpression.type._tag, 'Unavailable')
    }
    const partial = Analysis.rootAnalysis(invalid).hir.functions.at(4)
    const partialExpression = partial === undefined ? undefined : Hir.returned(partial)
    assert.strictEqual(partialExpression?._tag, 'Move')
    if (partialExpression?._tag === 'Move') {
      assert.strictEqual(partialExpression.subject._tag, 'Project')
      if (partialExpression.subject._tag === 'Project') {
        assert.strictEqual(partialExpression.subject.access, 'ConsumeRequested')
      }
    }
  }),
)

it.effect('allows external factory construction while refusing external raw literals', () =>
  Effect.gen(function* () {
    const sources = new Map([
      [
        'model/Token',
        ascii(`pub struct Token { pub kind: I32 }
pub fn make(kind: I32) -> Token { return Token { kind: kind } }`),
      ],
      [
        'app/Main',
        ascii(`import model.Token as Model { Token, make }
pub fn main() -> I32 { let token = Model.make(1) return token.kind }`),
      ],
    ])
    const valid = yield* multiSnapshot('app/Main', sources)
    assert.deepEqual(Analysis.diagnostics(valid), [])
    const outcome = Analysis.evaluate(valid)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 1)

    const raw = yield* multiSnapshot(
      'app/Main',
      new Map([
        ...sources,
        [
          'app/Main',
          ascii(`import model.Token as Model { Token }
pub fn main() -> I32 { let token = Model.Token { kind: 1 } return token.kind }`),
        ],
      ]),
    )
    assert.include(
      Analysis.diagnostics(raw).map((diagnostic) => diagnostic.code),
      'SEM0021',
    )
  }),
)
