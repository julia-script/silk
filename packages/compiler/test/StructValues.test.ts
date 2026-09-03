import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Layout from '../src/Layout.js'
import * as LayoutEncode from '../src/LayoutEncode.js'
import * as Match from '../src/Match.js'
import * as MirEncoding from '../src/MirEncoding.js'
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

it.effect('erases contextual tuple values and ordinal projections through nominal struct HIR', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'tuple-values/main',
      ascii(`tuple Point(i32, i32)
fn make() -> Point { return (1, 2) }
fn direct() -> Point { return Point(3, 4) }
fn typed() -> i32 { let point: Point = (5, 6) return point.0 + point.1 }
pub fn main() -> i32 { let point = direct() return point.0 + point.1 + typed() }`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const make = Analysis.rootAnalysis(self).functions.at(0)?.returnedExpression
    assert.strictEqual(make?._tag, 'StructLiteral')
    if (make?._tag === 'StructLiteral' && make.target._tag === 'Resolved') {
      assert.strictEqual(make.target.struct.aggregateKind, 'Positional')
      assert.deepEqual(
        make.target.struct.fields.map((field) => field.member),
        [
          { _tag: 'OrdinalAggregateMember', ordinal: 0 },
          { _tag: 'OrdinalAggregateMember', ordinal: 1 },
        ],
      )
    }
    const hir = Analysis.rootAnalysis(self).hir.functions.at(0)
    assert.strictEqual(hir === undefined ? undefined : Hir.returned(hir)._tag, 'Construct')
    const directHir = Analysis.rootAnalysis(self).hir.functions.at(1)
    assert.strictEqual(
      directHir === undefined ? undefined : Hir.returned(directHir)._tag,
      'Construct',
    )
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 18n)
  }),
)

it.effect('finalizes one uncontextualized record literal as an anonymous nominal struct', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'record-values/main',
      ascii(`pub fn main() -> i32 {
  let person = .{ name: "Julia", age: 32 }
  return person.age
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const generated = Analysis.rootAnalysis(self).generatedAggregates
    assert.strictEqual(generated.length, 1)
    assert.strictEqual(generated.at(0)?.aggregateKind, 'AnonymousNamed')
    assert.strictEqual(generated.at(0)?.name._tag, 'Unavailable')
    const anonymous = generated.at(0)
    const anonymousCanonical = anonymous?.canonical
    const anonymousType =
      anonymousCanonical?._tag === 'Canonical'
        ? Type.nominal(anonymousCanonical.id.module, anonymousCanonical.id.name)
        : undefined
    assert.strictEqual(
      anonymousType === undefined ? undefined : Type.anonymousAggregateDisplay(anonymousType),
      'anonymous record',
    )
    assert.include(
      anonymousType === undefined ? '' : Type.encode(anonymousType),
      '@AnonymousNamed:',
    )
    assert.strictEqual(
      anonymousCanonical?._tag !== 'Canonical'
        ? undefined
        : Analysis.lookupName(self, 'record-values/main', anonymousCanonical.id.name)._tag,
      'Missing',
    )
    assert.strictEqual(
      anonymous === undefined
        ? undefined
        : Analysis.declarationIndex(self).modules.at(0)?.structs.includes(anonymous),
      false,
    )
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 32n)
  }),
)

it.effect('finalizes one uncontextualized tuple and projects it by ordinal', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'tuple-values/anonymous',
      ascii(`pub fn main() -> i32 {
  let args = ("Julia", 32)
  return args.1
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const generated = Analysis.rootAnalysis(self).generatedAggregates
    assert.strictEqual(generated.length, 1)
    assert.strictEqual(generated.at(0)?.aggregateKind, 'AnonymousPositional')
    const anonymous = generated.at(0)
    const canonical = anonymous?.canonical
    const type =
      canonical?._tag === 'Canonical'
        ? Type.nominal(canonical.id.module, canonical.id.name)
        : undefined
    assert.strictEqual(
      type === undefined ? undefined : Type.anonymousAggregateDisplay(type),
      'anonymous tuple',
    )
    assert.include(type === undefined ? '' : Type.encode(type), '@AnonymousPositional:')
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 32n)
  }),
)

it.effect('infers named tuple type arguments from positional construction', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'tuple-values/generic',
      ascii(`tuple Box<T>(T)
fn boxed() -> Box<i32> { return Box(42) }
pub fn main() -> i32 { return boxed().0 }`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const returned = Analysis.rootAnalysis(self).functions.at(0)?.returnedExpression
    assert.strictEqual(
      returned?.type._tag === 'Available' ? Type.encode(returned.type.type) : undefined,
      'tuple-values/generic.Box<i32>',
    )
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('infers an open generic tuple argument from a contextual literal', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'tuple-values/generic-context',
      ascii(`tuple Box<T>(T)
fn accept<T>(box: Box<T>) -> i32 { drop box return 42 }
pub fn main() -> i32 { return accept((32,)) }`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('lets value-scope callables shadow named tuple constructors', () =>
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
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('uses known call parameters as contextual record authority', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'record-values/context',
      ascii(`struct Person { name: string age: i32 }
fn age(person: Person) -> i32 { return person.age }
pub fn main() -> i32 { return age(.{ name: "Julia", age: 32 }) }`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.rootAnalysis(self).generatedAggregates.length, 0)
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 32n)
  }),
)

it.effect('infers generic named struct arguments through a contextual record call', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'record-values/generic-context',
      ascii(`struct Box<T> { value: T }
fn accept<T>(box: Box<T>) -> i32 { drop box return 42 }
pub fn main() -> i32 { return accept(.{ value: 32 }) }`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.rootAnalysis(self).generatedAggregates.length, 0)
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('preserves an anonymous record identity through a generic call', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'record-values/generic',
      ascii(`fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let args = .{ name: "Julia", age: 32 }
  let same = identity(move args)
  return same.age
}`),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.rootAnalysis(self).generatedAggregates.length, 1)
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 32n)
  }),
)

it.effect('rejects anonymous branch unification but accepts an explicit named context', () =>
  Effect.gen(function* () {
    const invalid = yield* Analysis.ofSource(
      'record-values/branch-invalid',
      ascii(`enum Choice { First, Second }
fn choose(value: Choice) -> i32 {
  let person = match value {
    Choice.First => .{ name: "Julia", age: 32 }
    Choice.Second => .{ name: "Maria", age: 28 }
  }
  return 0
}`),
    )
    assert.include(
      Analysis.diagnostics(invalid).map((diagnostic) => diagnostic.code),
      'SEM0174',
    )

    const accepted = yield* Analysis.ofSourceRealized(
      'record-values/branch-context',
      ascii(`enum Choice { First, Second }
struct Person { name: string age: i32 }
fn choose(value: Choice) -> Person {
  let person: Person = match value {
    Choice.First => .{ name: "Julia", age: 32 }
    Choice.Second => .{ name: "Maria", age: 28 }
  }
  return move person
}
pub fn main() -> i32 { return choose(Choice.First).age }`),
    )
    assert.deepEqual(Analysis.diagnostics(accepted), [])
    const evaluated = Analysis.evaluate(accepted)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 32n)
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

it.effect('uses union variant field visibility as the external pattern boundary', () =>
  Effect.gen(function* () {
    const model = ascii(`pub union Secret { Open { pub value: i32, key: i32 }, Closed }
pub fn make(value: i32) -> Secret { return Secret.Open { value: value, key: 7 } }`)
    const valid = yield* multiSnapshot(
      'app/Main',
      new Map([
        ['model/Secret', model],
        [
          'app/Main',
          ascii(`import model.Secret { Secret, make }
fn reveal(secret: Secret) -> i32 {
  return match move secret {
    Secret.Open { value, .. } => value
    Secret.Closed => 0
  }
}
pub fn main() -> i32 { return reveal(make(42)) }`),
        ],
      ]),
    )
    assert.deepEqual(Analysis.diagnostics(valid), [])
    const outcome = Analysis.evaluate(valid)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)

    const explicitPrivate = yield* multiSnapshot(
      'app/Main',
      new Map([
        ['model/Secret', model],
        [
          'app/Main',
          ascii(`import model.Secret { Secret, make }
pub fn main() -> i32 {
  return match move make(42) {
    Secret.Open { value, key } => value
    Secret.Closed => 0
  }
}`),
        ],
      ]),
    )
    assert.deepEqual(
      Analysis.diagnostics(explicitPrivate).map((diagnostic) => diagnostic.code),
      ['SEM0028'],
    )

    const undisclosedPrivate = yield* multiSnapshot(
      'app/Main',
      new Map([
        ['model/Secret', model],
        [
          'app/Main',
          ascii(`import model.Secret { Secret, make }
pub fn main() -> i32 {
  return match move make(42) {
    Secret.Open { value } => value
    Secret.Closed => 0
  }
}`),
        ],
      ]),
    )
    assert.deepEqual(
      Analysis.diagnostics(undisclosedPrivate).map((diagnostic) => diagnostic.code),
      ['SEM0046'],
    )
    assert.notInclude(Analysis.diagnostics(undisclosedPrivate).at(0)?.message ?? '', 'key')
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

it.effect('lowers and evaluates nominal union construction as a whole value', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-values/runtime',
      ascii(`union State { Ready, Waiting { count: i32 } }
fn make() -> State { return State.Waiting { count: 2 } }
fn keep(state: State) -> State { return move state }
pub fn main() -> i32 { let state = keep(make()) return 42 }`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(Analysis.loweredMir(self)._tag, 'MirModule')
    const make = Analysis.loweredMir(self).functions.find((fn) => fn.id.name === 'make')
    assert.strictEqual(
      make === undefined
        ? undefined
        : MirVerification.operations(make).find(
            (operation) => operation._tag === 'ConstructUnionVariant',
          )?._tag,
      'ConstructUnionVariant',
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('evaluates exhaustive nominal union variant patterns with payload bindings', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-values/patterns',
      ascii(`union Option<T> { Some { value: T }, None }
fn unwrap(option: Option<i32>) -> i32 {
  return match move option {
    Option<i32>.Some { value } => value
    Option<i32>.None => 0
  }
}
pub fn main() -> i32 { return unwrap(Option<i32>.Some { value: 42 }) }`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const returned = Analysis.rootAnalysis(self).functions.at(0)?.returnedExpression
    assert.strictEqual(returned?._tag, 'Match')
    if (returned?._tag !== 'Match') return
    assert.strictEqual(returned.exhaustive, true)
    assert.deepEqual(returned.members.map(Match.encodeIdentity), [
      'union-values/patterns.Option<i32>::union-values/patterns.Option<i32>.Some',
      'union-values/patterns.Option<i32>::union-values/patterns.Option<i32>.None',
    ])
    assert.deepEqual(
      returned.arms.map((arm) => [arm.pattern._tag, arm.bindings.length, arm.reachable]),
      [
        ['UnionVariantPattern', 1, true],
        ['UnionVariantPattern', 0, true],
      ],
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('cleans omitted fields of the selected nominal union variant', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-values/omitted-cleanup',
      ascii(`struct Bomb {}
impl Drop for Bomb {
  fn drop(self: &mut Bomb) -> () { let boom = 1 / 0 return () }
}
union State { Empty, Ready { value: i32, bomb: Bomb } }
fn consume(state: State) -> i32 {
  return match move state {
    State.Empty => 0
    State.Ready { value, .. } => value
  }
}
fn ignore(state: State) -> i32 {
  return match move state { _ => 42 }
}
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
    assert.strictEqual(Analysis.evaluate(self)._tag, 'Trap')
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.throws(() => (instance.exports.silk_main as () => number)(), WebAssembly.RuntimeError)
  }),
)

it.effect('keeps nominal variants nested beneath structural union roots', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-values/hierarchical-match',
      ascii(`union HttpError { Timeout, DNS { code: i32 } }
struct OutOfMemoryError {}

fn inspect(error: HttpError | OutOfMemoryError) -> i32 {
  return match move error {
    HttpError.DNS { code } => code
    HttpError.Timeout => 1
    OutOfMemoryError other => 0
  }
}

fn classify(error: HttpError | OutOfMemoryError) -> i32 {
  return match move error {
    HttpError whole => 7
    OutOfMemoryError other => 0
  }
}

pub fn main() -> i32 {
  return inspect(HttpError.DNS { code: 42 }) + classify(HttpError.Timeout)
}`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const returned = Analysis.rootAnalysis(self).functions.at(0)?.returnedExpression
    assert.strictEqual(returned?._tag, 'Match')
    if (returned?._tag !== 'Match') return
    assert.deepEqual(returned.members.map(Match.encodeIdentity), [
      'union-values/hierarchical-match.HttpError::union-values/hierarchical-match.HttpError.Timeout',
      'union-values/hierarchical-match.HttpError::union-values/hierarchical-match.HttpError.DNS',
      'union-values/hierarchical-match.OutOfMemoryError',
    ])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 49n)
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 49)
  }),
)

it.effect('keeps generic applications and never-payload variants as distinct coverage leaves', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-values/generic-coverage',
      ascii(`union Option<T> { Some { value: T }, None }
union Result<T, E> { Success { value: T }, Failure { error: E } }

fn read(option: Option<i32> | Option<bool>) -> i32 {
  return match move option {
    Option<i32>.Some { value } => value
    Option<i32>.None => 0
    Option<bool>.Some { value } => 1
    Option<bool>.None => 0
  }
}

fn required(result: Result<i32, never>) -> i32 {
  return match move result {
    Result<i32, never>.Success { value } => value
    Result<i32, never>.Failure { error } => 0
  }
}

pub fn main() -> i32 { return read(Option<i32>.Some { value: 42 }) }`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const functions = Analysis.rootAnalysis(self).functions
    const read = functions.at(0)?.returnedExpression
    const required = functions.at(1)?.returnedExpression
    assert.strictEqual(read?._tag, 'Match')
    assert.strictEqual(required?._tag, 'Match')
    if (read?._tag !== 'Match' || required?._tag !== 'Match') return
    assert.strictEqual(read.members.length, 4)
    assert.deepEqual(
      read.members.map((member) => Type.encode(Match.sourceType(member))),
      [
        'union-values/generic-coverage.Option<bool>',
        'union-values/generic-coverage.Option<bool>',
        'union-values/generic-coverage.Option<i32>',
        'union-values/generic-coverage.Option<i32>',
      ],
    )
    assert.deepEqual(required.members.map(Match.encodeIdentity), [
      'union-values/generic-coverage.Result<i32, never>::union-values/generic-coverage.Result<i32, never>.Success',
      'union-values/generic-coverage.Result<i32, never>::union-values/generic-coverage.Result<i32, never>.Failure',
    ])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('keeps affine variant payloads available after a false guard', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-values/guarded-affine',
      ascii(`struct Token { value: i32 }
union Option<T> { Some { value: T }, None }

fn select(option: Option<Token>, guard: bool) -> i32 {
  return match move option {
    Option<Token>.Some { value } if guard => value.value
    Option<Token>.Some { value } => value.value + 1
    Option<Token>.None => 0
  }
}

pub fn main() -> i32 {
  return select(Option<Token>.Some { value: Token { value: 41 } }, false)
}`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('realizes callable and Effect fields only in their active nominal variant', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'union-values/represented-fields',
      ascii(`union Parser<F: once fn(i32) -> i32> { Empty, Ready { parse: F } }
union Deferred<F: once Effect<i32>> { Empty, Ready { operation: F } }

fn increment(value: i32) -> i32 { return value + 1 }

fn parse<F: once fn(i32) -> i32>(parser: Parser<F>) -> i32 {
  return match move parser {
    Parser<F>.Empty => 0
    Parser<F>.Ready { parse } => parse(20)
  }
}

fn force<F: once Effect<i32>>(deferred: Deferred<F>) -> i32 {
  return match move deferred {
    Deferred<F>.Empty => 0
    Deferred<F>.Ready { operation } => run operation
  }
}

pub fn main() -> i32 {
  let parser = Parser.Ready { parse: increment }
  let deferred = Deferred.Ready { operation: effect { return 21 } }
  return parse(move parser) + force(move deferred)
}`),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    const constructions = Analysis.loweredMir(self)
      .functions.flatMap(MirVerification.operations)
      .filter((operation) => operation._tag === 'ConstructUnionVariant')
    assert.deepEqual(
      constructions.flatMap((operation) =>
        operation._tag === 'ConstructUnionVariant'
          ? operation.fields.flatMap((field) =>
              field.stored === undefined ? [] : [field.stored._tag],
            )
          : [],
      ),
      ['StoredCallableField', 'StoredEffectField'],
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
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
    assert.include(nativeArtifact.ir, 'define hidden i32 @silk_main')
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

    const contextualPrivateField = yield* multiSnapshot(
      'app/Main',
      new Map([
        [
          'model/Secret',
          ascii(`pub struct Secret { pub value: i32 key: i32 }
pub fn inspect(secret: Secret) -> i32 { return secret.value }`),
        ],
        [
          'app/Main',
          ascii(`import model.Secret { inspect }
pub fn main() -> i32 { return inspect(.{ value: 1, key: 7 }) }`),
        ],
      ]),
    )
    assert.deepEqual(
      Analysis.diagnostics(contextualPrivateField).map((diagnostic) => diagnostic.code),
      ['SEM0021'],
    )
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
