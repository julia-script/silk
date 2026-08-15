import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'
import { elaborate } from './support/elaborate.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (id: string, source: string): Elaboration.Result =>
  elaborate(Parser.parse(Lexer.lex(SourceFile.make(id, ascii(source)))))

const bindingInitializer = (
  result: Elaboration.Result,
  functionOrdinal = 2,
): Elaboration.ExpressionFact | undefined => {
  const statement = result.functions.at(functionOrdinal)?.statements.at(0)
  return statement?._tag === 'BindStatement' ? statement.binding.initializer : undefined
}

it('infers one exact callable representation through repeated fields', () => {
  const result = analyze(
    'representation-inference/same',
    `struct Mapper<A, B, F: fn(A) -> B> { first: F second: F }
fn decode(value: i32) -> i32 { return value }
fn unused(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let mapper = Mapper<i32, i32> { first: decode, second: decode }
  return 0
}`,
  )
  const literal = bindingInitializer(result)

  assert.strictEqual(literal?._tag, 'StructLiteral')
  if (literal?._tag !== 'StructLiteral' || literal.type._tag !== 'Available') return
  assert.strictEqual(Type.isNominal(literal.type.type), true)
  if (!Type.isNominal(literal.type.type)) return
  const representation = literal.type.type.arguments.at(2)
  assert.strictEqual(Type.isExactRepresentationArgument(representation ?? 'never'), true)
  if (representation === undefined || !Type.isExactRepresentationArgument(representation)) return
  assert.strictEqual(
    representation.identity.identity,
    'declaration:representation-inference/same:decode',
  )
  assert.deepEqual(result.diagnostics, [])
})

it('diagnoses the first initializer whose identity conflicts with an inferred binder', () => {
  const result = analyze(
    'representation-inference/conflict',
    `struct Mapper<A, B, F: fn(A) -> B> { first: F second: F }
fn decode(value: i32) -> i32 { return value }
fn other(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let mapper = Mapper<i32, i32> { first: decode, second: other }
  return 0
}`,
  )
  const diagnostic = result.diagnostics.find((candidate) => candidate.code === 'SEM0104')

  assert.strictEqual(diagnostic?.reason._tag, 'ConflictingInitializerRepresentation')
  assert.include(diagnostic?.message ?? '', 'typeof(representation-inference/conflict.decode)')
  assert.include(diagnostic?.message ?? '', 'typeof(representation-inference/conflict.other)')
  assert.strictEqual(diagnostic?.relatedSpans?.at(0)?.label, 'representation first inferred here')
  assert.strictEqual(bindingInitializer(result)?.type._tag, 'Unavailable')
})

it('infers deterministic callable-section and Effect-site identities', () => {
  const result = analyze(
    'representation-inference/sites',
    `struct Transform<F: fn(i32) -> i32> { value: F }
struct Deferred<F: Effect<i32>> { value: F }
fn add(value: i32, amount: i32) -> i32 { return value + amount }
pub fn main() -> i32 {
  let transform = Transform { value: add(1) }
  let deferred = Deferred { value: effect { return 1 } }
  return 0
}`,
  )
  const statements = result.functions.at(1)?.statements ?? []
  const types = statements.flatMap((statement) =>
    statement._tag === 'BindStatement' && statement.binding.inferredType._tag === 'Available'
      ? [statement.binding.inferredType.type]
      : [],
  )
  const identities = types.flatMap((type) =>
    Type.isNominal(type)
      ? type.arguments.flatMap((argument) =>
          Type.isExactRepresentationArgument(argument) ? [argument.identity.identity] : [],
        )
      : [],
  )

  assert.deepEqual(identities, [
    'declaration:representation-inference/sites.main:site:0:owner=representation-inference/sites.main<>',
    'effect:effect\u0000declaration:representation-inference/sites:main\u0000site:1',
  ])
  assert.deepEqual(result.diagnostics, [])
})

it('rejects the first divergent nominal representation at a join with consumption guidance', () => {
  const result = analyze(
    'representation-inference/join',
    `struct First {}
struct Second {}
struct Parser<F: fn(i32) -> i32> { parse: F }
fn decimal(value: i32) -> i32 { return value }
fn hexadecimal(value: i32) -> i32 { return value }
fn choose(input: First | Second) -> i32 {
  let parser = match move input {
    First {} => Parser { parse: decimal }
    Second {} => Parser { parse: hexadecimal }
  }
  return 0
}`,
  )
  const diagnostic = result.diagnostics.find((candidate) => candidate.code === 'SEM0105')

  assert.strictEqual(diagnostic?.reason._tag, 'DivergentRepresentationJoin')
  assert.include(diagnostic?.message ?? '', 'consume each represented value inside its branch')
  assert.deepEqual(
    diagnostic?.relatedSpans?.map((related) => related.label),
    ['first representation originates here', 'divergent representation originates here'],
  )
  assert.notInclude(
    result.diagnostics.map((candidate) => candidate.code),
    'SEM0049',
  )
})

it('allows distinct represented callables to converge after branch-local consumption', () => {
  const result = analyze(
    'representation-inference/consumed-join',
    `struct First {}
struct Second {}
struct Parser<F: fn(i32) -> i32> { parse: F }
fn decimal(value: i32) -> i32 { return value }
fn hexadecimal(value: i32) -> i32 { return value }
fn choose(input: First | Second) -> i32 {
  return match move input {
    First {} => Parser { parse: decimal }.parse(10)
    Second {} => Parser { parse: hexadecimal }.parse(16)
  }
}`,
  )

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(result.functions.at(2)?.returnedExpression.type._tag, 'Available')
})

it('reports divergent representations at assignment and aggregate joins', () => {
  const assignment = analyze(
    'representation-inference/assignment-join',
    `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decimal(value: i32) -> i32 { return value }
fn hexadecimal(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let mut parser = Parser { parse: decimal }
  parser = Parser { parse: hexadecimal }
  return 0
}`,
  )
  const aggregate = analyze(
    'representation-inference/aggregate-join',
    `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decimal(value: i32) -> i32 { return value }
fn hexadecimal(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parsers = [Parser { parse: decimal }, Parser { parse: hexadecimal }]
  return 0
}`,
  )

  assert.deepEqual(
    assignment.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0105'],
  )
  assert.deepEqual(
    aggregate.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0105'],
  )
})

it('forwards open and exact representations through nesting, signatures, borrows, and projection', () => {
  const result = analyze(
    'representation-inference/propagation',
    `struct Parser<A, F: fn(A) -> A> { parse: F }
struct Wrapper<A, F: fn(A) -> A> { parser: Parser<A, F> }
fn decode(value: i32) -> i32 { return value }
fn forward<A, F: fn(A) -> A>(value: Wrapper<A, F>) -> Wrapper<A, F> {
  return move value
}
fn apply<A, F: fn(A) -> A>(parser: &Parser<A, F>, value: A) -> A {
  return parser.parse(move value)
}
pub fn main() -> i32 {
  let wrapper = Wrapper<i32> { parser: Parser<i32> { parse: decode } }
  return 0
}`,
  )
  const forward = result.functions.at(1)?.declaration
  const parameterType = forward?.parameters.at(0)?.declaredType
  const returnType = forward?.returnType
  const literal = bindingInitializer(result, 3)

  assert.strictEqual(parameterType?._tag, 'Resolved')
  assert.strictEqual(returnType?._tag, 'Resolved')
  if (parameterType?._tag === 'Resolved' && returnType?._tag === 'Resolved')
    assert.strictEqual(Type.equals(parameterType.type, returnType.type), true)
  assert.strictEqual(literal?._tag, 'StructLiteral')
  if (literal?._tag !== 'StructLiteral' || literal.type._tag !== 'Available') return
  assert.strictEqual(Type.isNominal(literal.type.type), true)
  if (!Type.isNominal(literal.type.type)) return
  assert.strictEqual(
    Type.isExactRepresentationArgument(literal.type.type.arguments.at(1) ?? 'never'),
    true,
  )
  assert.deepEqual(result.diagnostics, [])
})

it('forwards an open represented parameter directly into a represented field', () => {
  const result = analyze(
    'representation-inference/direct-forwarding',
    `struct Parser<A, F: fn(A) -> A> { parse: F }
fn make<A, F: fn(A) -> A>(parse: F) -> Parser<A, F> {
  return Parser<A, F> { parse: move parse }
}`,
  )
  const make = result.functions.at(0)
  const returned = make?.returnedExpression

  assert.strictEqual(returned?._tag, 'StructLiteral')
  assert.deepEqual(result.diagnostics, [])
  if (returned?._tag !== 'StructLiteral' || returned.type._tag !== 'Available') return
  assert.strictEqual(Type.isNominal(returned.type.type), true)
  if (!Type.isNominal(returned.type.type)) return
  assert.strictEqual(
    Type.isRepresentationParameterArgument(returned.type.type.arguments.at(1) ?? 'never'),
    true,
  )
})

it('infers direct generic callable and Effect representations, including callable sections', () => {
  const result = analyze(
    'representation-inference/generic-calls',
    `struct Parser<F: fn(i32) -> i32> { parse: F }
struct Deferred<F: Effect<i32>> { operation: F }
fn decimal(value: i32) -> i32 { return value }
fn add(value: i32, amount: i32) -> i32 { return value + amount }
fn wrap<F: fn(i32) -> i32>(parse: F) -> Parser<F> {
  return Parser<F> { parse: move parse }
}
fn defer<F: Effect<i32>>(operation: F) -> Deferred<F> {
  return Deferred<F> { operation: move operation }
}
pub fn main() -> i32 {
  let named = wrap(decimal)
  let section = wrap(add(1))
  let deferred = defer(effect { return 1 })
  return 0
}`,
  )

  assert.deepEqual(result.diagnostics, [])
  const main = result.hir.functions.find(
    (fact) => fact.declaration.name._tag === 'Present' && fact.declaration.name.spelling === 'main',
  )
  const arguments_ = main?.statements.flatMap((statement) =>
    Hir.statementExpressions(statement)
      .flatMap(Hir.expressionTree)
      .flatMap((expression) => (expression._tag === 'Call' ? expression.typeArguments : [])),
  )
  assert.strictEqual(arguments_?.length, 3)
  for (const argument of arguments_ ?? [])
    assert.strictEqual(Type.isExactRepresentationArgument(argument), true)
})

it('reports divergent representations at an explicit return boundary', () => {
  const result = analyze(
    'representation-inference/return-join',
    `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decimal(value: i32) -> i32 { return value }
fn hexadecimal(value: i32) -> i32 { return value }
fn wrong<F: fn(i32) -> i32>(parser: Parser<F>) -> Parser<F> {
  return Parser { parse: hexadecimal }
}`,
  )

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0105'],
  )
  assert.strictEqual(result.diagnostics.at(0)?.reason._tag, 'DivergentRepresentationJoin')
})

it('infers an omitted representation by declared kind across interleaved rows', () => {
  const result = analyze(
    'representation-inference/interleaved-kinds',
    `struct Deferred<A, !E, ?R, F: once Effect<A ! E ? R>> { operation: F }
fn make<A, !E, ?R, F: once Effect<A ! E ? R>>(
  operation: F
) -> Deferred<A, E, R, F> {
  return Deferred<A, E, R> { operation: move operation }
}`,
  )
  const returned = result.functions.at(0)?.returnedExpression

  assert.strictEqual(returned?._tag, 'StructLiteral')
  assert.deepEqual(result.diagnostics, [])
  if (returned?._tag !== 'StructLiteral' || returned.type._tag !== 'Available') return
  assert.strictEqual(Type.isNominal(returned.type.type), true)
  if (!Type.isNominal(returned.type.type)) return
  assert.strictEqual(Type.isTypeArgument(returned.type.type.arguments.at(0) ?? 'never'), true)
  assert.strictEqual(Type.isFailureRowArgument(returned.type.type.arguments.at(1) ?? 'never'), true)
  assert.strictEqual(
    Type.isRequirementRowArgument(returned.type.type.arguments.at(2) ?? 'never'),
    true,
  )
  assert.strictEqual(
    Type.isRepresentationParameterArgument(returned.type.type.arguments.at(3) ?? 'never'),
    true,
  )
})

it('rejects an inferred open representation whose source bound cannot satisfy the field', () => {
  const result = analyze(
    'representation-inference/incompatible-bound',
    `struct SharedParser<A, F: fn(A) -> A> { parse: F }
fn invalid<A, F: once fn(A) -> A>(parse: F) -> i32 {
  let parser = SharedParser<A> { parse: move parse }
  return 0
}`,
  )
  const diagnostic = result.diagnostics.find((candidate) => candidate.code === 'SEM0106')

  assert.strictEqual(diagnostic?.reason._tag, 'IncompatibleRepresentationBound')
  assert.include(diagnostic?.message ?? '', 'requires fn(A) -> A')
  assert.include(diagnostic?.message ?? '', 'supplied bound once fn(A) -> A')
  assert.deepEqual(
    diagnostic?.relatedSpans?.map((related) => related.label),
    ['required representation bound declared here', 'supplied representation bound declared here'],
  )
  assert.strictEqual(bindingInitializer(result, 0)?.type._tag, 'Unavailable')
})

it.effect('preserves open generic HIR and concrete representation instance keys', () =>
  Effect.gen(function* () {
    const source = `struct Parser<A, F: fn(A) -> A> { parse: F }
fn decode(value: i32) -> i32 { return value }
fn consume<A, F: fn(A) -> A>(parser: Parser<A, F>) -> i32 { return 0 }
pub fn main() -> i32 {
  let parser = Parser<i32> { parse: decode }
  return consume<i32>(move parser)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'representation-inference/instances',
      ascii(source),
    )
    const consume = Analysis.rootAnalysis(snapshot).hir.functions.at(1)
    const parameterType = consume?.declaration.parameters.at(0)?.declaredType
    const main = Analysis.rootAnalysis(snapshot).hir.functions.at(2)
    const returned = main === undefined ? undefined : Hir.returned(main)
    const instance = Analysis.instancesOf(snapshot).instances.find(
      (candidate) => candidate.key.declaration.name === 'consume',
    )

    assert.strictEqual(parameterType?._tag, 'Resolved')
    if (parameterType?._tag === 'Resolved' && Type.isNominal(parameterType.type))
      assert.strictEqual(
        Type.isRepresentationParameterArgument(parameterType.type.arguments.at(1) ?? 'never'),
        true,
      )
    assert.strictEqual(returned?._tag, 'Call')
    assert.strictEqual(
      returned?._tag === 'Call'
        ? Type.isExactRepresentationArgument(returned.typeArguments.at(1) ?? 'never')
        : false,
      true,
    )
    assert.strictEqual(
      Type.isExactRepresentationArgument(instance?.key.typeArguments.at(1) ?? 'never'),
      true,
    )
  }),
)

it.effect('exposes complete representation-dependent types and origin navigation to tooling', () =>
  Effect.gen(function* () {
    const source = `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  return 0
}`
    const module = 'representation-inference/tooling'
    const snapshot = yield* Analysis.ofSource(module, ascii(source))
    const bindingOffset = source.indexOf('parser =')
    const originOffset = source.lastIndexOf('decode')
    const hover = Analysis.hoverSubjectAt(snapshot, module, bindingOffset)
    const origin = Analysis.semanticOccurrenceAt(snapshot, module, originOffset)

    assert.strictEqual(
      hover?.presentation.text,
      'let parser: Parser<typeof(representation-inference/tooling.decode)>',
    )
    assert.strictEqual(origin?.resolution._tag, 'Available')
  }),
)
