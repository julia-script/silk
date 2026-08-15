import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'
import * as DeclaredTypeSyntax from './support/DeclaredTypeSyntax.js'
import { raise } from './support/raise.js'

const parse = DeclaredTypeSyntax.parse
const descendants = DeclaredTypeSyntax.descendants
const formatted = (source: string) => DeclaredTypeSyntax.format('opaque-result/format', source)
const index = DeclaredTypeSyntax.index
const declaration = DeclaredTypeSyntax.declaration

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
    SyntaxTree.directNode(
      binders[0] ?? raise('expected opaque result binder'),
      'TypeParameterList',
    ) !== undefined,
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

it.effect('diagnoses an ordinary type bound in opaque binder position', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'opaque-result/ordinary-bound',
      'pub fn broken() -> some<T: i32> T { loop {} }',
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0116')
    assert.strictEqual(diagnostic?.reason._tag, 'InvalidOpaqueResultBinder')
  }),
)

it.effect('rejects opaque results on bodyless service and interface operations', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'opaque-result/bodyless-operation',
      `pub service Factory { fn make() -> some<F: fn(i32) -> i32> F }
pub interface FactoryShape { fn make() -> some<G: fn(i32) -> i32> G }`,
    )
    const diagnostics = self.diagnostics.filter((candidate) => candidate.code === 'SEM0118')
    assert.strictEqual(diagnostics.length, 2)
    assert.deepEqual(
      diagnostics.map((diagnostic) =>
        diagnostic.reason._tag === 'BodylessOpaqueResult' ? diagnostic.reason.context : undefined,
      ),
      ['ServiceOperation', 'InterfaceOperation'],
    )
  }),
)

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
