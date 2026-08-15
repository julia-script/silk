import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import type * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Formatter from '../src/Formatter.js'
import * as Lexer from '../src/Lexer.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as NameResolution from '../src/NameResolution.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const parse = (id: string, source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Element> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Element> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [child],
  )

const formatted = Effect.fnUntraced(function* (source: string) {
  const document = yield* Formatter.format(parse('opaque-result/format', source))
  return decoder.decode(FormattedDocument.toUint8Array(document))
})

const index = (id: string, source: string) =>
  Effect.map(
    ModuleClosure.load({ root: SourceFile.make(id, encoder.encode(source)) }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    ),
    (closure) => NameResolution.analyze(closure).index,
  )

const declaration = (self: DeclarationIndex.Index, module: string, name: string) =>
  self.modules
    .find((candidate) => candidate.module === module)
    ?.declarations.find(
      (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === name,
    )

it('parses a contextual some binder only in result position', () => {
  const syntax = parse(
    'opaque-result/parse',
    `pub struct Parser<F: fn(i32) -> i32> { parse: F }
pub fn make() -> some<F: fn(i32) -> i32> Parser<F> { return 0 }`,
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
  const binders = descendants(syntax.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'OpaqueResultType',
  )
  assert.strictEqual(binders.length, 1)
  assert.strictEqual(
    SyntaxTree.directNode(binders[0] ?? assert.fail('binder'), 'TypeParameterList') !== undefined,
    true,
  )
})

it('keeps some usable as an ordinary type name outside result position', () => {
  const syntax = parse(
    'opaque-result/contextual',
    `pub struct some<T> { value: T }
pub fn hold(value: some<i32>) -> i32 { return 0 }`,
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
  assert.strictEqual(
    descendants(syntax.root).some(
      (element) => SyntaxTree.isNode(element) && element.kind === 'OpaqueResultType',
    ),
    false,
  )
})

it.effect('formats a contextual some binder over its complete result', () =>
  Effect.gen(function* () {
    const text = yield* formatted(
      `pub struct Parser<F:fn(i32)->i32>{parse:F}
pub fn make()->some<F:fn(i32)->i32>Parser<F>{return 0}`,
    )
    assert.include(text, '-> some<F: fn(i32) -> i32> Parser<F>')
    assert.strictEqual(yield* formatted(text), text)
  }),
)

it('contains a damaged some binder and still parses the next declaration', () => {
  const syntax = parse(
    'opaque-result/recovery',
    `pub fn broken() -> some<F: fn(i32) -> > Parser<F> { return 0 }
pub fn next() -> i32 { return 0 }`,
  )
  assert.strictEqual(
    syntax.root.children.filter(
      (child) => SyntaxTree.isNode(child) && child.kind === 'FunctionDeclaration',
    ).length,
    2,
  )
})

it('rejects additional opaque binders without consuming the result', () => {
  const syntax = parse(
    'opaque-result/one-binder',
    `pub fn broken() -> some<F: fn(i32) -> i32, G: fn(i32) -> i32> F { return 0 }
pub fn next() -> i32 { return 0 }`,
  )
  assert.isAbove(syntax.parserDiagnostics.length, 0)
  assert.strictEqual(
    syntax.root.children.filter(
      (child) => SyntaxTree.isNode(child) && child.kind === 'FunctionDeclaration',
    ).length,
    2,
  )
})

it('rejects value and row parameters in opaque binder position', () => {
  for (const binder of ['T', '!E', '?R']) {
    const syntax = parse(
      `opaque-result/invalid-binder/${binder}`,
      `pub fn broken() -> some<${binder}> i32 { return 0 }`,
    )
    assert.isAbove(syntax.parserDiagnostics.length, 0, binder)
  }
})

it('recovers a missing binder close before the complete result type', () => {
  const syntax = parse(
    'opaque-result/missing-close',
    `pub fn broken() -> some<F: fn(i32) -> i32 F { return 0 }
pub fn next() -> i32 { return 0 }`,
  )
  assert.isAbove(syntax.parserDiagnostics.length, 0)
  assert.strictEqual(
    syntax.root.children.filter(
      (child) => SyntaxTree.isNode(child) && child.kind === 'FunctionDeclaration',
    ).length,
    2,
  )
})

it.effect('resolves one Effect representation binder over the result', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'opaque-result/effect-bound',
      'pub fn make() -> some<F: Effect<i32>> F { return 0 }',
    )
    const found = declaration(self, 'opaque-result/effect-bound', 'make')
    assert.strictEqual(found?.opaqueResult?.binder.type.kind, 'EffectRepresentation')
    assert.strictEqual(found?.returnType._tag, 'Resolved')
    if (found?.returnType._tag !== 'Resolved') return
    assert.strictEqual(Type.isRepresented(found.returnType.type), true)
    if (!Type.isRepresented(found.returnType.type)) return
    assert.strictEqual(Type.isEffect(found.returnType.type.contract), true)
  }),
)

it.effect('resolves one binder used more than once in the complete result', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'opaque-result/repeated',
      'pub fn make() -> some<F: fn(i32) -> i32> fn(F) -> F { return 0 }',
    )
    const found = declaration(self, 'opaque-result/repeated', 'make')
    assert.strictEqual(found?.returnType._tag, 'Resolved')
    if (found?.returnType._tag !== 'Resolved' || !Type.isCallable(found.returnType.type)) return
    const parameter = found.returnType.type.parameters.at(0)
    assert.isDefined(parameter)
    assert.strictEqual(Type.key(parameter ?? 'never'), Type.key(found.returnType.type.result))
    assert.strictEqual(found.opaqueResult?.binder.type.ordinal, found.typeParameters.length)
  }),
)
