import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Layout from '../src/Layout.js'
import * as LayoutEncode from '../src/LayoutEncode.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct Pair { left: i32 right: i32 }
fn make() -> Pair { return Pair { right: 2, left: 1 } }
fn pass(value: Pair) -> Pair { return move value }
pub fn main() -> i32 { let pair = pass(make()) return pair.left + pair.right }`

const snapshot = (target?: string) =>
  Analysis.ofSourceRealized('struct-values/main', ascii(source), target)

const multiSnapshot = (rootModule: string, sources: ReadonlyMap<string, Uint8Array>) => {
  const root = sources.get(rootModule)
  if (root === undefined) throw new RangeError(`Fixture has no root source ${rootModule}`)
  return Analysis.makeRealized({ root: SourceFile.make(rootModule, root) }).pipe(
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

it.effect('elaborates nominal union variants as precise parent values', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'union-values/construction',
      ascii(`union Option<T> { Some { value: T }, None }
union Result<A, E> { Success { value: A }, Failure { error: E } }
union State { Ready, Waiting { count: i32 } }
fn some() -> Option<i32> { return Option.Some { value: 42 } }
fn none() -> Option<i32> { return Option<i32>.None }
fn failed() -> Result<i32, bool> { return Result<i32>.Failure { error: true } }
fn ready() -> State { return State.Ready }`),
    )
    const functions = Analysis.rootAnalysis(self).functions
    const some = functions.at(0)?.returnedExpression
    const none = functions.at(1)?.returnedExpression
    const failed = functions.at(2)?.returnedExpression
    const ready = functions.at(3)?.returnedExpression

    for (const [name, expression] of [
      ['some', some],
      ['none', none],
      ['failed', failed],
      ['ready', ready],
    ] as const) {
      assert.strictEqual(expression?._tag, 'UnionVariant', name)
      if (expression?._tag === 'UnionVariant') assert.strictEqual(expression.type._tag, 'Available')
    }
    assert.strictEqual(
      some?.type._tag === 'Available' ? Type.encode(some.type.type) : undefined,
      'union-values/construction.Option<i32>',
    )
    assert.strictEqual(
      none?.type._tag === 'Available' ? Type.encode(none.type.type) : undefined,
      'union-values/construction.Option<i32>',
    )
    assert.strictEqual(
      failed?.type._tag === 'Available' ? Type.encode(failed.type.type) : undefined,
      'union-values/construction.Result<i32, bool>',
    )
    assert.strictEqual(
      ready?.type._tag === 'Available' ? Type.encode(ready.type.type) : undefined,
      'union-values/construction.State',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
  }),
)

it.effect('infers union arguments only from the selected variant fields', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'union-values/inference',
      ascii(`union Option<T> { Some { value: T }, None }
union Result<A, E> { Success { value: A }, Failure { error: E } }
fn missingError() -> Result<i32, bool> { return Result.Success { value: 42 } }
fn missingItem() -> Option<i32> { return Option.None }
fn unknown() -> Option<i32> { return Option<i32>.Missing }
fn conflict() -> Result<i32, bool> {
  return Result<i32>.Success { value: true }
}`),
    )

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0099', 'SEM0099', 'SEM0099', 'SEM0167', 'SEM0100'],
    )
    const functions = Analysis.rootAnalysis(self).functions
    assert.strictEqual(functions.at(0)?.returnedExpression._tag, 'UnionVariant')
    assert.strictEqual(functions.at(1)?.returnedExpression._tag, 'UnionVariant')
    assert.strictEqual(functions.at(2)?.returnedExpression._tag, 'UnionVariant')
    assert.ok(
      functions.every((fn) => fn.returnedExpression.type._tag === 'Unavailable'),
      'expected-type context must not complete a constructor application',
    )
  }),
)

it.effect('uses union variant field visibility as the external construction boundary', () =>
  Effect.gen(function* () {
    const self = yield* multiSnapshot(
      'app/Main',
      new Map([
        [
          'model/Secret',
          ascii(`pub union Secret { Open { pub value: i32, key: i32 }, Closed }
pub fn make(value: i32) -> Secret { return Secret.Open { value: value, key: 7 } }`),
        ],
        [
          'app/Main',
          ascii(`import model.Secret { Secret }
pub fn main() -> i32 { let secret = Secret.Open { value: 1, key: 2 } return 0 }`),
        ],
      ]),
    )

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0021'],
    )
    assert.notInclude(Analysis.diagnostics(self).at(0)?.message ?? '', 'key')
  }),
)

it.effect('evaluates initializers in source order before constructing in declaration order', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'struct-values/evaluation-order',
      ascii(`struct Pair { left: i32 right: i32 }
fn left() -> i32 { return 1 }
fn right() -> i32 { return 2 }
fn make() -> Pair { return Pair { right: right(), left: left() } }
pub fn main() -> i32 { let pair = make() return pair.left }`),
      'wasm32-unknown-unknown',
    )
    const make = Analysis.loweredMir(self).functions.find((fn) => fn.id.name === 'make')
    const calls =
      make === undefined
        ? []
        : MirVerification.operations(make).flatMap((operation) => {
            if (operation._tag === 'Call') return [operation.target.name]
            return []
          })

    assert.deepEqual(calls, ['right', 'left'])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
  }),
)

it.effect('plans canonical aggregate lanes and evaluates whole-value calls and projections', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('wasm32-unknown-unknown')
    const layout = Analysis.layoutOf(self)
    assert.strictEqual(layout._tag, 'Available')
    if (layout._tag !== 'Available') return
    const pair = layout.value.entries.find(
      (entry) => LayoutEncode.encode(layout.value).includes('Pair') && entry.type !== 'i32',
    )
    assert.notStrictEqual(pair, undefined)
    const shape = pair === undefined ? undefined : Layout.callingShape(layout.value, pair.type)
    assert.deepEqual(
      shape?.lanes.map((lane) =>
        lane.path.map((selector) => {
          switch (selector._tag) {
            case 'ElementSelector':
              return `[${selector.index}]`
            case 'FieldId':
              return selector.ordinal
            case 'UnionTagSelector':
              return 'tag'
            case 'UnionPayloadSelector':
              return `payload:${selector.slot}`
            case 'SliceAddressSelector':
              return 'address'
            case 'SliceLengthSelector':
              return 'length'
            default:
              return assert.fail('unexpected layout selector')
          }
        }),
      ),
      [[0], [1]],
    )

    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [])
    assert.include(MirEncoding.encode(mir), 'construct struct-values/main.Pair')
    assert.include(MirEncoding.encode(mir), 'read-place')

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 3n)
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
      new WebAssembly.Module(wasmArtifact.bytes.slice()),
      {},
    )
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(Analysis.evaluate(native)._tag, 'Completed')
    assert.strictEqual(main(), 3)
    assert.include(nativeArtifact.ir, 'define i32 @silk_main')
    assert.include(nativeArtifact.ir, 'extractvalue')
    assert.include(wasmArtifact.wat, '(result i32 i32)')
  }),
)

it.effect('preserves empty nominal contracts with zero runtime lanes', () =>
  Effect.gen(function* () {
    const text = `struct End {}
fn end() -> End { return End {} }
fn consume(value: End) -> i32 { return 7 }
pub fn main() -> i32 { return consume(end()) }`
    const self = yield* Analysis.ofSourceRealized(
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
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    assert.strictEqual(Analysis.evaluate(self)._tag, 'Completed')

    const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main as () => number
    assert.strictEqual(main(), 7)
  }),
)

it.effect('keeps invalid construction, projection, and partial moves phase-owned', () =>
  Effect.gen(function* () {
    const invalid = yield* Analysis.ofSourceRealized(
      'struct-values/invalid',
      ascii(`struct Pair { left: i32 right: i32 }
struct Outer { pair: Pair }
fn missing() -> Pair { return Pair { left: 1 } }
fn duplicate() -> Pair { return Pair { left: 1, left: 2, right: 3 } }
fn unknown() -> Pair { return Pair { left: 1, right: 2, extra: 3 } }
fn mistyped() -> Pair { return Pair { left: true, right: 2 } }
fn partial(value: Outer) -> Pair { return move value.pair }
pub fn main() -> i32 { return 0 }`),
    )

    assert.deepEqual(
      Analysis.diagnostics(invalid).map((diagnostic) => diagnostic.code),
      ['SEM0024', 'SEM0023', 'SEM0022', 'SEM0025', 'OWN0002'],
    )
    const functions = Analysis.rootAnalysis(invalid).functions
    for (const ordinal of [0, 1, 2, 3]) {
      assert.strictEqual(functions.at(ordinal)?.returnedExpression.type._tag, 'Unavailable')
    }
    const partial = Analysis.rootAnalysis(invalid).hir.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'partial',
    )
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

it.effect('uses field visibility as the external construction boundary', () =>
  Effect.gen(function* () {
    const sources = new Map([
      [
        'model/Token',
        ascii(`pub struct Token { pub kind: i32 }
pub fn make(kind: i32) -> Token { return Token { kind: kind } }`),
      ],
      [
        'app/Main',
        ascii(`import model.Token as Model { Token, make }
pub fn main() -> i32 { let token = Model.make(1) return token.kind }`),
      ],
    ])
    const valid = yield* multiSnapshot('app/Main', sources)
    assert.deepEqual(Analysis.diagnostics(valid), [])
    const outcome = Analysis.evaluate(valid)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 1n)

    const raw = yield* multiSnapshot(
      'app/Main',
      new Map([
        ...sources,
        [
          'app/Main',
          ascii(`import model.Token as Model { Token }
pub fn main() -> i32 { let token = Model.Token { kind: 1 } return token.kind }`),
        ],
      ]),
    )
    assert.deepEqual(Analysis.diagnostics(raw), [])
    const rawOutcome = Analysis.evaluate(raw)
    assert.strictEqual(rawOutcome._tag, 'Completed')
    if (rawOutcome._tag === 'Completed') assert.strictEqual(rawOutcome.result.value, 1n)

    const privateField = yield* multiSnapshot(
      'app/Main',
      new Map([
        [
          'model/Secret',
          ascii(`pub struct Secret { pub value: i32 key: i32 }
pub fn make(value: i32) -> Secret { return Secret { value: value, key: 7 } }`),
        ],
        [
          'app/Main',
          ascii(`import model.Secret as Model { Secret, make }
pub fn main() -> i32 { let secret = Model.Secret { value: 1 } return secret.value }`),
        ],
      ]),
    )
    assert.deepEqual(
      Analysis.diagnostics(privateField).map((diagnostic) => diagnostic.code),
      ['SEM0021'],
    )
    assert.notInclude(Analysis.diagnostics(privateField).at(0)?.message ?? '', 'key')
  }),
)

it.effect('infers omitted ordinary struct parameters from every supplied field', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'struct-values/inference',
      ascii(`struct Same<T> { first: T second: T }
struct Pair<A, B> { first: A second: B }
pub fn main() -> i32 {
  let same = Same { second: 2, first: 1 }
  let pair = Pair<i32> { second: true, first: 3 }
  return same.first + pair.first
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const bindings = Analysis.rootAnalysis(self)
      .functions.at(0)
      ?.statements.flatMap((statement) =>
        statement._tag === 'BindStatement' ? [statement.binding.initializer] : [],
      )
    const types = bindings?.flatMap((initializer) =>
      initializer._tag === 'StructLiteral' && initializer.type._tag === 'Available'
        ? [initializer.type.type]
        : [],
    )
    assert.deepEqual(
      types?.map((type) => Type.encode(type)),
      ['struct-values/inference.Same<i32>', 'struct-values/inference.Pair<i32, bool>'],
    )
    const pair = bindings?.at(1)
    assert.strictEqual(pair?._tag, 'StructLiteral')
    if (pair?._tag !== 'StructLiteral') return
    assert.deepEqual(
      pair.typeArguments.map((argument) => argument.source),
      ['Explicit', 'Inferred'],
    )
    assert.strictEqual(pair.typeArguments.at(1)?.origins.length, 1)
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 4n)
  }),
)

it.effect('rejects conflicting and absent ordinary struct inference evidence', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'struct-values/inference-errors',
      ascii(`struct Same<T> { first: T second: T }
struct Phantom<T> { value: i32 }
fn conflict() -> i32 { let value = Same { first: 1, second: true } return 0 }
fn absent() -> i32 { let value = Phantom { value: 1 } return 0 }
pub fn main() -> i32 { return 0 }`),
    )

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0099', 'SEM0100'],
    )
    const conflict = Analysis.diagnostics(self).find((diagnostic) => diagnostic.code === 'SEM0100')
    assert.strictEqual(conflict?.reason._tag, 'TypeArgumentConflict')
    assert.strictEqual(conflict?.relatedSpans?.at(0)?.label, 'type argument first constrained here')
  }),
)
