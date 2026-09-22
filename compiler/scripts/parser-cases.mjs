// Behavioral cases adapted from packages/compiler/test/Parser.test.ts and its fixtures.
// Each witness names valid syntax that must survive; error-tree shape and diagnostic wording
// deliberately remain implementation choices. Generated nesting inputs keep fixtures readable.
const later = 'fn later() -> i32 { return 7 }'
const recovered = 'let recovered = 2'
const witness = (kind, text) => ({ kind, text })
const recovery = (name, source, witnesses = []) => ({
  name,
  source: `${source}\n${later}`,
  invalid: true,
  witnesses: [...witnesses, witness('FunctionDeclaration', later)],
})

export const cases = [
  {
    name: 'nested-call-siblings',
    source: 'fn main() -> i32 { return combine(identity(1), identity(2)) }',
    witnesses: [witness('CallExpression', 'identity(1)'), witness('CallExpression', 'identity(2)')],
  },
  {
    name: 'precedence',
    source: 'fn main() -> i32 { return 1 + 2 * 3 }',
    witnesses: [witness('InfixExpression', '2 * 3')],
  },
  {
    name: 'lifetime-callable',
    source:
      "fn apply<'a>(f: for<'b> fn(&'b i32) -> &'b i32, value: &'a i32) -> &'a i32 { return f(value) }",
    witnesses: [witness('ReferenceType', "&'a i32")],
  },
  {
    name: 'prefix-precedence',
    source: 'fn main() -> i32 { return -value.field * 2 + 1 }',
    witnesses: [
      witness('PrefixExpression', '-value.field'),
      witness('InfixExpression', '-value.field * 2'),
    ],
  },
  {
    name: 'anonymous-callables',
    source: `fn make() -> () {
      let ordinary = fn(value: i32) -> i32 { return value }
      let effectful = effect fn(error: Failure) -> i32 ! Failure ? &Logger { return 42 }
      let pending = effect { return 1 }
    }`,
    witnesses: [
      witness('AnonymousCallableExpression', 'fn(value: i32) -> i32 { return value }'),
      witness(
        'AnonymousCallableExpression',
        'effect fn(error: Failure) -> i32 ! Failure ? &Logger { return 42 }',
      ),
      witness('EffectExpression', 'effect { return 1 }'),
    ],
  },
  {
    name: 'effect-environment-intersection',
    source: "effect<'call & 'env> fn work(value: Effect<'call & 'env; i32>) -> i32 { return 1 }",
    witnesses: [
      witness(
        'FunctionDeclaration',
        "effect<'call & 'env> fn work(value: Effect<'call & 'env; i32>) -> i32 { return 1 }",
      ),
    ],
  },
  {
    name: 'contextual-test-qualifiers',
    source: `test fn plain() {}
pub test effect fn effectful() -> () ! i32 { fail 1 }
static if true { test fn selected() {} } else { test fn inactive() {} }
fn test() {}
fn use(test: i32) -> i32 { return test }`,
    witnesses: [
      witness('FunctionDeclaration', 'test fn plain() {}'),
      witness('FunctionDeclaration', 'pub test effect fn effectful() -> () ! i32 { fail 1 }'),
      witness('FunctionDeclaration', 'fn test() {}'),
      witness('FunctionDeclaration', 'fn use(test: i32) -> i32 { return test }'),
    ],
  },
  {
    name: 'module-call-argument',
    source:
      'fn write(module: i32, name: i32, ok: bool) -> i32 { return module } fn main(module: i32) -> i32 { return write(module, 1, true) }',
    witnesses: [witness('CallExpression', 'write(module, 1, true)')],
  },
  {
    name: 'applied-provider-constraint',
    source:
      'fn use<P, A>() -> () where &mut P provides &Service<A> from &mut Service<A> { return () }',
    witnesses: [witness('ProviderConstraint', '&mut P provides &Service<A> from &mut Service<A>')],
  },
  {
    name: 'service-operation-property',
    source:
      'service Logger { effect fn log() -> () with Intrinsic.nonParking() } fn after() -> i32 { return 1 }',
    witnesses: [
      witness('FunctionPropertyClause', 'with Intrinsic.nonParking()'),
      witness('FunctionDeclaration', 'fn after() -> i32 { return 1 }'),
    ],
  },
  recovery(
    'unsupported-anonymous-environment',
    "fn damaged() -> () { let use = effect<'call> fn() -> () {} }",
  ),
  {
    ...recovery('missing-parameter-type', 'fn damaged(value:, next: i32) -> () {}', [
      witness('ParameterDeclaration', 'next: i32'),
    ]),
    missing: { token: 'Identifier', before: ', next' },
  },
  {
    ...recovery('missing-parameter-comma', 'fn damaged(value: i32 next: i32) -> () {}', [
      witness('ParameterDeclaration', 'next: i32'),
    ]),
    missing: { token: 'Comma', before: 'next:' },
  },
  {
    ...recovery('missing-call-close', 'fn damaged() -> i32 { return identity(42 }'),
    missing: { token: 'RightParenthesis', before: '}' },
  },
  recovery(
    'damaged-argument-sibling',
    'fn damaged() -> i32 { return combine(identity(:), identity(2)) }',
    [witness('CallExpression', 'identity(2)')],
  ),
  recovery('missing-function-brace', 'fn damaged() -> i32 { return identity(42'),
  recovery('missing-lifetime-referent', "fn damaged<'a>(value: &'a, next: i32) -> () {}", [
    witness('ParameterDeclaration', 'next: i32'),
  ]),
  recovery('empty-effect-environment', 'fn damaged(value: Effect<; i32>) -> () {}'),
  recovery('empty-callable-binder', 'fn damaged(value: fn<>(i32) -> i32) -> () {}'),
  recovery('noncallable-lifetime-binder', "fn damaged(value: for<'a> i32) -> () {}"),
  recovery(
    'missing-lifetime-binder-close',
    "fn damaged(value: for<'a fn(&'a i32) -> &'a i32) -> () {}",
  ),
  recovery('lifetime-value-borrow', "fn damaged(value: i32) -> () { drop &'a value }"),
  recovery(
    'service-operation-close',
    `service Logger {
  effect fn broken(message: &[u8] -> ()
  fn enabled() -> bool
}`,
    [witness('ServiceOperation', 'fn enabled() -> bool')],
  ),
  recovery(
    'invalid-service-storage',
    `service Broken {
  state: i32
  fn enabled() -> bool
}`,
    [witness('ServiceOperation', 'fn enabled() -> bool')],
  ),
  recovery('missing-failure-member', 'effect fn damaged() -> i32 ! { return 1 }', [
    witness('ReturnStatement', 'return 1'),
  ]),
  recovery(
    'missing-impl-parameter-close',
    'impl<T Drop for Vector<T> { fn drop(self: &mut Vector<T>) -> () { return () } }',
  ),
  recovery(
    'missing-bounded-impl-close',
    'impl<S: Decoder<S> Decoder<MappedSchema<S>> for MappedSchema<S> { decode: MappedSchema.mappedDecode }',
  ),
  recovery('missing-impl-body', 'impl Allocator for Broken'),
  recovery(
    'damaged-effect-block',
    'fn make() -> i32 { let pending = effect { return broken( } return 0 }',
    [witness('ReturnStatement', 'return 0')],
  ),
  recovery(
    'damaged-anonymous-parameter',
    'fn make() -> i32 { return accept(fn(value:) -> i32 { return 1 }, 42) }',
    [witness('IntegerLiteralExpression', '42')],
  ),
  {
    ...recovery(
      'unsafe-call-statement-boundary',
      'fn damaged() -> i32 { unsafe { let value = Slot.take( return 42 } }',
      [witness('ReturnStatement', 'return 42')],
    ),
    // The bootstrap preserves the following function but consumes this return into recovery.
    bootstrapWitnesses: [witness('FunctionDeclaration', later)],
  },
  ...[
    { name: 'group', expression: (depth) => `${'('.repeat(depth)}1${')'.repeat(depth)}` },
    { name: 'array', expression: (depth) => `${'['.repeat(depth)}1${']'.repeat(depth)}` },
    { name: 'call', expression: (depth) => `${'f('.repeat(depth)}1${')'.repeat(depth)}` },
    { name: 'prefix', expression: (depth) => `${'!'.repeat(depth)}true` },
  ].flatMap(({ name, expression }) => [
    {
      name: `nested-${name}`,
      source: `fn main() -> i32 { return ${expression(16)} }`,
      witnesses: [witness('ReturnStatement', `return ${expression(16)}`)],
    },
    {
      ...recovery(
        `overdeep-${name}`,
        `fn damaged() -> i32 { return ${expression(2000)} ${recovered} return recovered }`,
        [witness('BindingStatement', recovered)],
      ),
      diagnostic: 'NestingLimit',
    },
  ]),
  {
    name: 'budget-sibling-reset',
    source: `fn main() -> i32 { let left = ${'('.repeat(30)}1${')'.repeat(30)} let right = ${'('.repeat(30)}2${')'.repeat(30)} return right }`,
    witnesses: [witness('ReturnStatement', 'return right')],
  },
  {
    ...recovery(
      'budget-boundary',
      `fn damaged() -> i32 { return ${'('.repeat(31)}1${')'.repeat(31)} }`,
    ),
    // Resource budgets need not match the bootstrap's 256-expression-depth policy.
    bootstrapInvalid: false,
    diagnostic: 'NestingLimit',
  },
  {
    ...recovery('overdeep-missing-delimiters', `fn damaged() -> i32 { return ${'('.repeat(2000)}1`),
    // The bootstrap currently swallows the following declaration in this unclosed region.
    // Keep the stronger native recovery contract rather than reproducing that behavior.
    bootstrapWitnesses: [],
    diagnostic: 'NestingLimit',
  },
  ...[
    { name: 'type', source: `fn damaged(value: ${'('.repeat(80)}i32${')'.repeat(80)}) -> () {}` },
    {
      name: 'pattern',
      source: `fn damaged(value: Pair) -> () { let ${'Pair { first: '.repeat(80)}leaf${' }'.repeat(80)} = value }`,
    },
    {
      name: 'block',
      source: `fn damaged() -> () { ${'if true { '.repeat(80)}return ()${' }'.repeat(80)} }`,
    },
    {
      name: 'static-declaration',
      source: `${'static if true { '.repeat(80)}const VALUE: i32 = 1${' }'.repeat(80)}`,
    },
    {
      name: 'record',
      source: `fn damaged() -> Pair { return ${'Pair { first: '.repeat(80)}1${' }'.repeat(80)} }`,
    },
  ].map(({ name, source }) => ({
    ...recovery(`overdeep-${name}`, source),
    bootstrapInvalid: false,
    diagnostic: 'NestingLimit',
  })),
]
