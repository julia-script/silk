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

const codes = (self: DeclarationIndex.Index): ReadonlyArray<string> =>
  self.diagnostics.map((diagnostic) => diagnostic.code)

const formatted = Effect.fnUntraced(function* (source: string) {
  const document = yield* Formatter.format(parse('exact-representation/format', source))
  return decoder.decode(FormattedDocument.toUint8Array(document))
})

it('parses an exact representation result into one ExactRepresentationType node', () => {
  const syntax = parse(
    'exact-representation/parse',
    `fn decode(value: i32) -> i32 { return value }
pub fn selected() -> typeof(decode) { return decode }`,
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
  const nodes = descendants(syntax.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ExactRepresentationType',
  )
  assert.strictEqual(nodes.length, 1)
  assert.strictEqual(
    SyntaxTree.directNode(nodes[0] ?? assert.fail('node'), 'TypePath') !== undefined,
    true,
  )
})

it('keeps typeof usable as an ordinary identifier outside item position', () => {
  const syntax = parse(
    'exact-representation/contextual',
    'pub fn typeof(value: i32) -> i32 { return value }',
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
  assert.strictEqual(
    descendants(syntax.root).some(
      (element) => SyntaxTree.isNode(element) && element.kind === 'ExactRepresentationType',
    ),
    false,
  )
})

it.effect('formats an exact representation result without inner spacing', () =>
  Effect.gen(function* () {
    const text = yield* formatted(
      `fn decode(value:i32)->i32{return value}
pub fn selected()->typeof( decode ){return decode}`,
    )
    assert.include(text, '-> typeof(decode)')
    assert.strictEqual(yield* formatted(text), text)
  }),
)

it.effect('formats a specialized exact representation item', () =>
  Effect.gen(function* () {
    const text = yield* formatted(
      `fn identity<T>(value:T)->T{return value}
pub fn selected()->typeof(identity<i32>){return 0}`,
    )
    assert.include(text, '-> typeof(identity<i32>)')
    assert.strictEqual(yield* formatted(text), text)
  }),
)

it.effect('records the exact named-item representation of a specialized public function', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/positive',
      `pub fn identity<T>(value: T) -> T { return value }
pub fn selected() -> typeof(identity<i32>) { return 0 }`,
    )
    assert.notInclude(codes(self), 'SEM0112')
    const found = declaration(self, 'exact-representation/positive', 'selected')
    const returnType = found?.returnType
    assert.strictEqual(returnType?._tag, 'Resolved')
    if (returnType?._tag !== 'Resolved') return
    assert.strictEqual(Type.isRepresented(returnType.type), true)
    if (!Type.isRepresented(returnType.type)) return
    const argument = returnType.type.representation.argument
    assert.strictEqual(Type.isExactRepresentationArgument(argument), true)
    if (!Type.isExactRepresentationArgument(argument)) return
    assert.strictEqual(Type.isCallableIdentityArgument(argument.identity), true)
    if (!Type.isCallableIdentityArgument(argument.identity)) return
    assert.deepEqual(argument.identity.target, {
      _tag: 'Declaration',
      module: 'exact-representation/positive',
      name: 'identity',
    })
    assert.strictEqual(Type.encode(argument.contract), 'fn(i32) -> i32')
  }),
)

it.effect('gives one exact representation the same identity from two declarations', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/stability',
      `pub fn identity<T>(value: T) -> T { return value }
pub fn left() -> typeof(identity<i32>) { return 0 }

// Moving source trivia must not change a canonical representation identity.
pub fn right() -> typeof(identity<i32>) { return 0 }`,
    )
    const keyOf = (name: string) => {
      const returnType = declaration(self, 'exact-representation/stability', name)?.returnType
      return returnType?._tag === 'Resolved' ? Type.key(returnType.type) : undefined
    }
    const left = keyOf('left')
    assert.isDefined(left)
    assert.strictEqual(keyOf('right'), left)
  }),
)

it.effect('distinguishes exact representations of different specializations', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/specializations',
      `pub fn identity<T>(value: T) -> T { return value }
pub fn narrow() -> typeof(identity<i32>) { return 0 }
pub fn wide() -> typeof(identity<i64>) { return 0 }`,
    )
    const keyOf = (name: string) => {
      const returnType = declaration(self, 'exact-representation/specializations', name)?.returnType
      return returnType?._tag === 'Resolved' ? Type.key(returnType.type) : undefined
    }
    assert.isDefined(keyOf('narrow'))
    assert.notStrictEqual(keyOf('narrow'), keyOf('wide'))
  }),
)

it.effect('rejects a private exact identity leak through a public result', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/private-leak',
      `fn hidden(value: i32) -> i32 { return value }
pub fn leaked() -> typeof(hidden) { return hidden }`,
    )
    assert.include(codes(self), 'SEM0112')
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0112')
    assert.strictEqual(diagnostic?.reason._tag, 'PrivateExactRepresentationLeak')
    assert.include(
      diagnostic?.notes?.join(' ') ?? '',
      'Return an opaque representation result instead',
    )
  }),
)

it.effect('admits a private exact identity inside a private contract', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/private-contract',
      `fn hidden(value: i32) -> i32 { return value }
fn kept() -> typeof(hidden) { return hidden }`,
    )
    assert.notInclude(codes(self), 'SEM0112')
  }),
)

it.effect('rejects a partially specialized generic item', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/partial',
      `pub fn identity<T>(value: T) -> T { return value }
pub fn selected() -> typeof(identity) { return 0 }`,
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0111')
    assert.strictEqual(diagnostic?.reason._tag, 'OpenExactRepresentationItem')
  }),
)

it.effect('rejects an item that is still generic in an enclosing parameter', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/open',
      `pub fn identity<T>(value: T) -> T { return value }
pub fn selected<T>() -> typeof(identity<T>) { return 0 }`,
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0111')
    assert.strictEqual(diagnostic?.reason._tag, 'OpenExactRepresentationItem')
  }),
)

it.effect('rejects an unresolved exact representation item', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/missing',
      'pub fn selected() -> typeof(absent) { return absent }',
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0108')
    assert.strictEqual(diagnostic?.reason._tag, 'UnresolvedExactRepresentationItem')
  }),
)

it.effect('rejects an Effect construction site named through typeof', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/effect-item',
      `effect fn produce() -> i32 { return 1 }
pub fn selected() -> typeof(produce) { return produce }`,
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0110')
    assert.strictEqual(diagnostic?.reason._tag, 'UncallableExactRepresentationItem')
  }),
)

it('contains the damaged exact representation and still parses the next declaration', () => {
  const syntax = parse(
    'exact-representation/recovery',
    `fn decode(value: i32) -> i32 { return value }
pub fn broken() -> typeof( { return decode }
pub fn next() -> i32 { return 0 }`,
  )
  const missing = descendants(syntax.root)
    .flatMap((element) => (SyntaxTree.isNode(element) ? element.children : []))
    .filter(SyntaxTree.isMissingToken)
  assert.isAbove(missing.length, 0)
  assert.strictEqual(
    syntax.root.children.filter(
      (child) => SyntaxTree.isNode(child) && child.kind === 'FunctionDeclaration',
    ).length,
    3,
  )
})
