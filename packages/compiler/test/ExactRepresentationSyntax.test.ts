import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as RepresentationField from '../src/RepresentationField.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'
import * as DeclaredTypeSyntax from './support/DeclaredTypeSyntax.js'
import { raise } from './support/raise.js'

const encoder = new TextEncoder()
const parse = DeclaredTypeSyntax.parse
const descendants = DeclaredTypeSyntax.descendants
const index = DeclaredTypeSyntax.index
const indexWithImports = DeclaredTypeSyntax.indexWithImports
const declaration = DeclaredTypeSyntax.declaration
const codes = DeclaredTypeSyntax.codes
const formatted = (source: string) =>
  DeclaredTypeSyntax.format('exact-representation/format', source)

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
    SyntaxTree.directNode(nodes[0] ?? raise('expected exact representation node'), 'TypePath') !==
      undefined,
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

it.effect('resolves a specialized callable through a module namespace', () =>
  Effect.gen(function* () {
    const self = yield* indexWithImports('app/Main', {
      'app/Main': `import library.Callables as Lib
pub fn selected() -> typeof(Lib.identity<i32>) { return 0 }`,
      'library/Callables': 'pub fn identity<T>(value: T) -> T { return value }',
    })
    const found = declaration(self, 'app/Main', 'selected')
    assert.strictEqual(found?.returnType._tag, 'Resolved')
    if (found?.returnType._tag !== 'Resolved' || !Type.isRepresented(found.returnType.type)) return
    const argument = found.returnType.type.representation.argument
    assert.strictEqual(Type.isExactRepresentationArgument(argument), true)
    if (!Type.isExactRepresentationArgument(argument)) return
    assert.strictEqual(Type.isCallableIdentityArgument(argument.identity), true)
    if (!Type.isCallableIdentityArgument(argument.identity)) return
    assert.deepEqual(argument.identity.target, {
      _tag: 'Declaration',
      module: 'library/Callables',
      name: 'identity',
    })
  }),
)

it.effect('resolves a specialized callable through a selected import alias', () =>
  Effect.gen(function* () {
    const self = yield* indexWithImports('app/Main', {
      'app/Main': `import library.Callables { identity as id }
pub fn selected() -> typeof(id<i32>) { return 0 }`,
      'library/Callables': 'pub fn identity<T>(value: T) -> T { return value }',
    })
    const found = declaration(self, 'app/Main', 'selected')
    assert.strictEqual(found?.returnType._tag, 'Resolved')
    assert.notInclude(codes(self), 'SEM0108')
  }),
)

const identitySource = `struct Mappers<F: fn(i32) -> i32, G: fn(i32) -> i32> { first: F second: G }
struct Deferred<F: Effect<i32>, G: Effect<i32>> { first: F second: G }
pub fn main() -> i32 {
  let mappers = Mappers { first: i32.add(1), second: i32.add(1) }
  let deferred = Deferred { first: effect { return 1 }, second: effect { return 1 } }
  return 0
}`
const shiftedIdentitySource = `// Moving source trivia must not rename executable sites.

struct Mappers<F: fn(i32) -> i32, G: fn(i32) -> i32> { first: F second: G }
struct Deferred<F: Effect<i32>, G: Effect<i32>> { first: F second: G }
pub fn main() -> i32 {
  // Same-shaped sites remain distinct while retaining their structural ordinals.
  let mappers = Mappers { first: i32.add(1), second: i32.add(1) }
  let deferred = Deferred { first: effect { return 1 }, second: effect { return 1 } }
  return 0
}`

const identityFacts = (snapshot: Analysis.FrontendSnapshot) => {
  const main = Analysis.rootAnalysis(snapshot).functions.find(
    (fact) => fact.declaration.name._tag === 'Present' && fact.declaration.name.spelling === 'main',
  )
  const instances = (main?.statements ?? []).flatMap((statement) =>
    statement._tag === 'BindStatement' &&
    statement.binding.inferredType._tag === 'Available' &&
    Type.isNominal(statement.binding.inferredType.type)
      ? [statement.binding.inferredType.type]
      : [],
  )
  return instances.flatMap((instance) => {
    const resolutions = RepresentationField.resolveFields(snapshot.index, [instance])
    return RepresentationField.plansOf(snapshot.index, instance).map((plan) => {
      const resolution = RepresentationField.lookup(resolutions, instance, plan.id)
      return {
        nominal: Type.key(instance),
        field: RepresentationField.key(instance, plan.id),
        argument:
          resolution?._tag === 'ResolvedRepresentationField'
            ? Type.genericArgumentKey(resolution.argument)
            : '',
      }
    })
  })
}

it.effect('keeps executable identity keys stable while source trivia shifts', () =>
  Effect.gen(function* () {
    const module = 'exact-representation/identity-stability'
    const baseline = yield* Analysis.ofSource(module, encoder.encode(identitySource))
    const shifted = yield* Analysis.ofSource(module, encoder.encode(shiftedIdentitySource))
    const facts = identityFacts(baseline)
    assert.strictEqual(facts.length, 4)
    assert.strictEqual(new Set(facts.map((fact) => fact.argument)).size, 4)
    assert.deepEqual(identityFacts(shifted), facts)
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

it.effect('rejects fully supplied exact arguments that remain open in every generic kind', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/open-kinded',
      `pub fn target<A, E, ?R, F: fn(A) -> A, G: Effect<A>>(value: A) -> A {
  return move value
}
pub fn selected<A, E, ?R, F: fn(A) -> A, G: Effect<A>>() -> typeof(target<A, E, R, F, G>) {
  loop {}
}`,
    )
    const found = declaration(self, 'exact-representation/open-kinded', 'selected')
    assert.notStrictEqual(found?.returnType._tag, 'Resolved')
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0111')
    assert.strictEqual(diagnostic?.reason._tag, 'OpenExactRepresentationItem')
  }),
)

const isolatedOpenKinds = Object.freeze([
  Object.freeze({
    name: 'value',
    parameter: 'A',
    target: 'A',
    selected: 'A',
  }),
  Object.freeze({
    name: 'failure row',
    parameter: '!E',
    target: 'E',
    selected: 'E',
  }),
  Object.freeze({
    name: 'requirement row',
    parameter: '?R',
    target: 'R',
    selected: 'R',
  }),
  Object.freeze({
    name: 'callable representation',
    parameter: 'F: fn(i32) -> i32',
    target: 'F',
    selected: 'F',
  }),
  Object.freeze({
    name: 'Effect representation',
    parameter: 'G: Effect<i32>',
    target: 'G',
    selected: 'G',
  }),
])

for (const fixture of isolatedOpenKinds)
  it.effect(`rejects an isolated open ${fixture.name} exact argument`, () =>
    Effect.gen(function* () {
      const module = `exact-representation/open-${fixture.name.replaceAll(' ', '-')}`
      const self = yield* index(
        module,
        `pub fn target<${fixture.parameter}>() -> i32 { return 0 }
pub fn selected<${fixture.parameter}>() -> typeof(target<${fixture.target}>) { loop {} }`,
      )
      const found = declaration(self, module, 'selected')
      assert.notStrictEqual(found?.returnType._tag, 'Resolved', fixture.name)
      assert.strictEqual(
        self.diagnostics.filter((diagnostic) => diagnostic.code === 'SEM0111').length,
        1,
        fixture.name,
      )
    }),
  )

it.effect(
  'preserves every source-nameable concrete kind in a specialized exact item identity',
  () =>
    Effect.gen(function* () {
      const self = yield* index(
        'exact-representation/concrete-kinded',
        `pub struct Failure {}
pub service Capability {}
pub fn callable(value: i32) -> i32 { return value }
pub fn target<A, E, ?R, F: fn(A) -> A>(value: A) -> A {
  return move value
}
pub fn selected() -> typeof(target<i32, Failure, Capability, typeof(callable)>) {
  loop {}
}`,
      )
      assert.deepEqual(
        self.diagnostics.map((diagnostic) => ({
          code: diagnostic.code,
          reason: diagnostic.reason,
        })),
        [],
      )
      const found = declaration(self, 'exact-representation/concrete-kinded', 'selected')
      assert.strictEqual(found?.returnType._tag, 'Resolved')
      if (found?.returnType._tag !== 'Resolved' || !Type.isRepresented(found.returnType.type))
        return
      const exact = found.returnType.type.representation.argument
      assert.strictEqual(Type.isExactRepresentationArgument(exact), true)
      if (!Type.isExactRepresentationArgument(exact)) return
      assert.strictEqual(Type.isCallableIdentityArgument(exact.identity), true)
      if (!Type.isCallableIdentityArgument(exact.identity)) return
      const [value, failures, requirements, callable] = exact.identity.typeArguments
      assert.strictEqual(value !== undefined && Type.isTypeArgument(value), true)
      assert.strictEqual(failures !== undefined && Type.isTypeArgument(failures), true)
      assert.strictEqual(
        requirements !== undefined && Type.isRequirementRowArgument(requirements),
        true,
      )
      assert.strictEqual(
        callable !== undefined && Type.isExactRepresentationArgument(callable),
        true,
      )
      assert.notInclude(codes(self), 'SEM0111')
    }),
)

it.effect('rejects private exact identities nested in every non-value generic position', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/nested-private',
      `fn hidden(value: i32) -> i32 { return value }
pub struct Failure<F: fn(i32) -> i32> { value: i32 }
pub struct Capability<F: fn(i32) -> i32> { value: i32 }
pub fn generic<F: fn(i32) -> i32>(value: i32) -> i32 { return value }
pub fn rows<E, ?R>(value: i32) -> i32 { return value }
pub fn nominalLeak() -> Failure<typeof(hidden)> { loop {} }
pub fn identityLeak() -> typeof(generic<typeof(hidden)>) { loop {} }
pub fn rowLeak() -> typeof(rows<Failure<typeof(hidden)>, Capability<typeof(hidden)>>) { loop {} }`,
    )
    const leaks = self.diagnostics.filter((diagnostic) => diagnostic.code === 'SEM0112')
    assert.strictEqual(leaks.length, 3)
    assert.deepEqual(
      leaks.map((diagnostic) => diagnostic.reason._tag),
      [
        'PrivateExactRepresentationLeak',
        'PrivateExactRepresentationLeak',
        'PrivateExactRepresentationLeak',
      ],
    )
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

it.effect('rejects an ambiguous exact representation item', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/ambiguous',
      `pub fn duplicated(value: i32) -> i32 { return value }
pub fn duplicated(value: i64) -> i64 { return value }
pub fn selected() -> typeof(duplicated) { return 0 }`,
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0109')
    assert.strictEqual(diagnostic?.reason._tag, 'AmbiguousExactRepresentationItem')
  }),
)

it.effect('rejects an inaccessible exact item from another module', () =>
  Effect.gen(function* () {
    const self = yield* indexWithImports('app/Main', {
      'app/Main': `import library.Callables as Lib
pub fn selected() -> typeof(Lib.hidden) { return 0 }`,
      'library/Callables': 'fn hidden(value: i32) -> i32 { return value }',
    })
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

it.effect('rejects a non-callable module declaration through the callable diagnostic', () =>
  Effect.gen(function* () {
    const self = yield* index(
      'exact-representation/non-callable',
      `pub struct Value {}
pub fn selected() -> typeof(Value) { return 0 }`,
    )
    const diagnostic = self.diagnostics.find((candidate) => candidate.code === 'SEM0110')
    assert.strictEqual(diagnostic?.reason._tag, 'UncallableExactRepresentationItem')
  }),
)

it.effect('navigates valid and invalid exact item tokens to their declaration', () => {
  const source = `pub fn identity<T>(value: T) -> T { return value }
pub fn valid() -> typeof(identity<i32>) { return 0 }
pub fn open() -> typeof(identity) { return 0 }`
  return Analysis.ofSourceRealized('main', encoder.encode(source)).pipe(
    Effect.map((snapshot) => {
      const validOffset = source.indexOf('identity<i32>')
      const openOffset = source.lastIndexOf('identity)')
      const valid = Analysis.semanticOccurrenceAt(snapshot, 'main', validOffset)
      const open = Analysis.semanticOccurrenceAt(snapshot, 'main', openOffset)
      assert.strictEqual(valid?.role, 'Value')
      assert.strictEqual(valid?.resolution._tag, 'Available')
      assert.strictEqual(valid?.declaration?.module, 'main')
      assert.strictEqual(open?.role, 'Value')
      assert.strictEqual(open?.resolution._tag, 'Unavailable')
      assert.strictEqual(open?.declaration?.module, 'main')
      return undefined
    }),
  )
})

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
