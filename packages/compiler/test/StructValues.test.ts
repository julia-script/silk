import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as MirLinearization from '../src/MirLinearization.js'
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

it.effect('lets a value-scope callable shadow a named tuple constructor', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'tuple-values/shadow',
      ascii(`tuple Point(i32)
fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { let Point = identity return Point(42) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const returned = Analysis.rootAnalysis(self).functions.at(1)?.returnedExpression
    assert.strictEqual(returned?._tag, 'CallableApply')
  }),
)

it.effect('reports aggregate-specific construction diagnostics', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'tuple-values/diagnostics',
      ascii(`tuple Point(i32, i32)
struct Person { age: i32 }
fn short() -> Point { return Point(1) }
fn named() -> Point { return Point { _0: 1, _1: 2 } }
fn wrongRecord() -> Point { return .{ age: 1 } }
fn wrongTuple() -> Person { return (1,) }
fn duplicateAnonymous() -> i32 { let value = .{ age: 1, age: 2 } return 0 }`),
    )
    const diagnostics = Analysis.diagnostics(self).filter((diagnostic) =>
      ['SEM0172', 'SEM0173', 'SEM0175'].includes(diagnostic.code),
    )
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0172', 'SEM0175', 'SEM0173', 'SEM0173'],
    )
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.reason._tag),
      [
        'TupleArityMismatch',
        'PositionalFieldConstruction',
        'ContextualAggregateKindMismatch',
        'ContextualAggregateKindMismatch',
      ],
    )
    assert.ok(diagnostics.every((diagnostic) => diagnostic.span.end > diagnostic.span.start))
    assert.strictEqual(
      Analysis.diagnostics(self).filter((diagnostic) => diagnostic.code === 'SEM0023').length,
      1,
    )
  }),
)

it.effect('keeps separate same-shaped anonymous record occurrences nominally incompatible', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'record-values/assignment',
      ascii(`pub fn main() -> i32 {
  let mut person = .{ name: "Julia", age: 32 }
  person = .{ name: "Julia", age: 32 }
  return 0
}`),
    )

    assert.include(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      'SEM0037',
    )
    const generated = Analysis.rootAnalysis(self).generatedAggregates
    assert.strictEqual(generated.length, 2)
    const first = generated.at(0)?.canonical
    const second = generated.at(1)?.canonical
    assert.notStrictEqual(
      first?._tag === 'Canonical' ? first.id.name : undefined,
      second?._tag === 'Canonical' ? second.id.name : undefined,
    )

    const equality = yield* Analysis.ofSource(
      'record-values/equality',
      ascii(`pub fn main() -> i32 {
  let first = .{ age: 32 }
  let second = .{ age: 32 }
  if first == second { return 1 }
  return 0
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(equality).map((diagnostic) => diagnostic.code),
      ['SEM0135'],
    )
    const equalityDiagnostic = Analysis.diagnostics(equality).at(0)
    assert.strictEqual(equalityDiagnostic?.reason._tag, 'OperatorNotApplicable')
    assert.notInclude(equalityDiagnostic?.message ?? '', '@Anonymous')
    assert.strictEqual(Analysis.rootAnalysis(equality).generatedAggregates.length, 2)
  }),
)

it.effect('keeps an all-Copy anonymous record affine as a whole value', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'record-values/ownership',
      ascii(`pub fn main() -> i32 {
  let original = .{ value: 42 }
  let moved = move original
  return original.value
}`),
    )

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0001'],
    )
  }),
)

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
    const hir = Analysis.rootAnalysis(self).hir.functions.map(Hir.returned)
    assert.deepEqual(
      hir.map((expression) => expression._tag),
      [
        'ConstructUnionVariant',
        'ConstructUnionVariant',
        'ConstructUnionVariant',
        'ConstructUnionVariant',
      ],
    )
    const someHir = hir.at(0)
    assert.strictEqual(
      someHir?._tag === 'ConstructUnionVariant' ? someHir.variant.name : undefined,
      'Some',
    )
    assert.deepEqual(
      someHir?._tag === 'ConstructUnionVariant'
        ? someHir.fields.map((field) => field.field.ordinal)
        : undefined,
      [0],
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

it.effect('does not synthesize fields on the nominal union parent', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'union-values/projection',
      ascii(`union Result<A, E> { Success { value: A }, Failure { value: E } }
fn project(result: Result<i32, bool>) -> i32 { return result.value }`),
    )

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0027'],
    )
    assert.strictEqual(
      Analysis.rootAnalysis(self).functions.at(0)?.returnedExpression.type._tag,
      'Unavailable',
    )
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

it.effect(
  'keeps construction and borrowed extraction errors phase-owned while admitting owned field moves',
  () =>
    Effect.gen(function* () {
      const source = `struct Pair { left: i32 right: i32 }
struct Outer { pair: Pair }
fn missing() -> Pair { return Pair { left: 1 } }
fn duplicate() -> Pair { return Pair { left: 1, left: 2, right: 3 } }
fn unknown() -> Pair { return Pair { left: 1, right: 2, extra: 3 } }
fn mistyped() -> Pair { return Pair { left: true, right: 2 } }
fn partial(value: Outer) -> Pair { return move value.pair }
fn borrowed(value: &Outer) -> Pair { return move value.pair }
pub fn main() -> i32 { return 0 }`
      const invalid = yield* Analysis.ofSource('struct-values/invalid', ascii(source))

      assert.deepEqual(
        Analysis.diagnostics(invalid).map((diagnostic) => ({
          code: diagnostic.code,
          span: source.slice(diagnostic.span.start, diagnostic.span.end).trim(),
        })),
        [
          { code: 'SEM0024', span: 'Pair { left: 1 }' },
          { code: 'SEM0023', span: 'left' },
          { code: 'SEM0022', span: 'extra' },
          { code: 'SEM0025', span: 'true' },
          { code: 'OWN0002', span: 'move value.pair' },
        ],
      )
      assert.strictEqual(
        Analysis.diagnostics(invalid).find((diagnostic) => diagnostic.code === 'OWN0002')?.span
          .start,
        source.lastIndexOf(' move value.pair'),
      )
      const functions = Analysis.rootAnalysis(invalid).functions
      for (const ordinal of [0, 1, 2, 3]) {
        assert.strictEqual(functions.at(ordinal)?.returnedExpression.type._tag, 'Unavailable')
      }
      const partial = Analysis.rootAnalysis(invalid).hir.functions.find(
        (fn) =>
          fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'partial',
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

it.effect('plans cleanup for omitted fields of the selected nominal-union variant', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-values/omitted-cleanup',
      ascii(`struct Bomb {}
impl Drop for Bomb { fn drop(self: &mut Bomb) -> () { return () } }
union State { Empty, Ready { value: i32, bomb: Bomb } }
fn consume(state: State) -> i32 {
  return match move state {
    State.Empty => 0
    State.Ready { value, .. } => value
  }
}
fn ignore(state: State) -> i32 { return match move state { _ => 42 } }
pub fn main() -> i32 {
  if ignore(State.Empty) != 42 { return 0 }
  return consume(State.Ready { value: 42, bomb: Bomb {} })
}`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const consume = Analysis.loweredMir(self).functions.find((fn) => fn.id.name === 'consume')
    const ignore = Analysis.loweredMir(self).functions.find((fn) => fn.id.name === 'ignore')
    assert.isTrue(
      consume !== undefined &&
        MirLinearization.linearize(consume).some((block) =>
          block.operations.some(
            (operation) => operation._tag === 'Drop' && operation.cleanup._tag === 'HookCleanup',
          ),
        ),
    )
    assert.isTrue(
      ignore !== undefined &&
        MirLinearization.linearize(ignore).some((block) =>
          block.operations.some(
            (operation) =>
              operation._tag === 'Drop' && operation.cleanup._tag === 'NominalUnionCleanup',
          ),
        ),
    )
  }),
)
