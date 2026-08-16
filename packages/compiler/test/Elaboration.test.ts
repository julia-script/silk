import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import { pipe } from 'effect/Function'
import * as Analysis from '../src/Analysis.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import type * as SyntaxFile from '../src/SyntaxFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import type * as Token from '../src/Token.js'
import * as Type from '../src/Type.js'
import {
  acceptedSource,
  ambiguousCallSource,
  beyondSafeIntegerSource,
  crossFunctionParameterSource,
  damagedIdentifierSource,
  damagedNestedCallSource,
  damagedTargetBodyCallSource,
  damagedTypeSource,
  duplicateNameSource,
  duplicateParameterSource,
  forwardCallSource,
  i32BoundarySource,
  identityCallSource,
  incompatibleNestedCallSource,
  missingCallCalleeSource,
  missingCallRightParenthesisSource,
  missingIntegerSource,
  missingNameSource,
  missingParameterNameSource,
  missingParameterTypeSource,
  missingSecondNameSource,
  mixedFunctionDamageSource,
  mixedResolutionDamageSource,
  nestedCallSource,
  nestedSiblingCallsSource,
  overflowSource,
  parserAndSemanticDamageSource,
  recoveredArgumentSource,
  sameParameterNamesSource,
  selfCallSource,
  threeFunctionSource,
  tooFewArgumentsSource,
  tooManyArgumentsSource,
  tripleDuplicateNameSource,
  tripleDuplicateParameterSource,
  twoArgumentCallSource,
  twoFunctionSource,
  twoParameterSource,
  unavailableArgumentContractSource,
  unavailableParameterContractSource,
  unknownCallSource,
  unknownParameterReferenceSource,
  unknownParameterTypeSource,
  unknownTypeSource,
  unresolvedNestedCallSource,
  unresolvedTargetTypeCallSource,
  validCallSource,
} from './fixtures/BootstrapSemanticFixture.js'
import { elaborate } from './support/elaborate.js'
import { raise } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parseText = (id: string, source: string): SyntaxFile.SyntaxFile =>
  Parser.parse(Lexer.lex(SourceFile.make(id, ascii(source))))

const analyzeText = (id: string, source: string): Elaboration.Result =>
  elaborate(parseText(id, source))

const analyzeWithStdlib = Effect.fnUntraced(function* (id: string, source: string) {
  const module = id.replace('://', '/').replace(/\.silk$/, '')
  return Analysis.rootAnalysis(yield* Analysis.ofSource(module, ascii(source)))
})

it('diagnoses invalid effect origins, runs, mutability, and escape explicitly', () => {
  const ordinaryFail = analyzeText(
    'effect://ordinary-fail',
    'struct Problem {}\nfn bad() -> i32 { fail move Problem {} }',
  )
  const undeclared = analyzeText(
    'effect://undeclared',
    'struct Problem {}\neffect fn bad() -> i32 { fail move Problem {} }',
  )
  const nonEffect = analyzeText('effect://non-effect', 'fn bad() -> i32 { return run 1 }')
  const unhandled = analyzeText(
    'effect://unhandled',
    `struct Problem {}
effect fn risky() -> i32 ! Problem { fail move Problem {} }
fn bad() -> i32 { return run risky() }`,
  )
  const recipeRules = analyzeText(
    'effect://recipe-rules',
    `effect fn work() -> i32 { return 1 }
effect fn bad() -> i32 {
  let mut recipe = work()
  return recipe
}`,
  )

  assert.include(
    ordinaryFail.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0063',
  )
  assert.include(
    undeclared.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0064',
  )
  assert.include(
    nonEffect.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0065',
  )
  assert.include(
    unhandled.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0066',
  )
  assert.deepEqual(
    recipeRules.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0068'],
  )
})

it.effect('replaces the caught failure row with the handler row canonically', () =>
  Effect.gen(function* () {
    const result = yield* analyzeWithStdlib(
      'effect://catch-algebra',
      `struct First {}
struct HandlerProblem {}
effect fn risky() -> i32 ! First { fail move First {} }
effect fn recover(problem: First) -> i32 ! HandlerProblem { fail move HandlerProblem {} }
effect fn outer() -> i32 ! HandlerProblem {
  let recipe = risky() |> Effect.catch(recover)
  return run recipe
}`,
    )

    assert.deepEqual(result.diagnostics, [])
    const outer = result.functions.at(2)
    const recipe = outer?.bindings.at(0)?.inferredType
    assert.strictEqual(recipe?._tag, 'Available')
    if (recipe?._tag !== 'Available' || !Type.isEffect(recipe.type)) return
    assert.deepEqual(recipe.type.failures.map(Type.encode), ['effect/catch-algebra.HandlerProblem'])
  }),
)

it('infers lazy effect-block results failures and exclusive captures', () => {
  const result = analyzeText(
    'effect://block-captures',
    `struct Problem { code: i32 }
fn build(value: i32) -> i32 {
  let mut counter = value
  let pending = effect {
    counter = counter + 1
    if counter == 0 { fail Problem { code: 41 } }
    return counter
  }
  return 0
}`,
  )

  assert.deepEqual(result.diagnostics, [])
  const pending = result.functions.at(0)?.bindings.at(1)?.initializer
  assert.strictEqual(pending?._tag, 'EffectBlock')
  if (pending?._tag !== 'EffectBlock' || pending.type._tag !== 'Available') return
  assert.strictEqual(
    Type.encode(pending.type.type),
    'mut Effect<i32 ! effect://block-captures.Problem>',
  )
  assert.deepEqual(
    pending.captures.map((capture) => [capture.reference.id.ordinal, capture.access]),
    [[0, 'Exclusive']],
  )
})

it('allows an ordinary function to return a hidden effect instance', () => {
  const result = analyzeText(
    'effect://returned-block',
    `fn delayed(value: i32) -> Effect<i32> {
  return effect { return value }
}
fn main() -> i32 {
  let pending = delayed(41)
  return run pending
}`,
  )

  assert.deepEqual(result.diagnostics, [])
  const delayed = result.functions.at(0)
  assert.deepEqual(delayed?.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(delayed?.returnedExpression._tag, 'EffectBlock')
  if (delayed?.returnedExpression._tag !== 'EffectBlock') return
  assert.strictEqual(delayed.returnedExpression.site.function.ordinal, 0)
  assert.deepEqual(
    delayed.returnedExpression.captures.map((capture) => capture.access),
    ['Copy'],
  )
})

it('rejects implicit erasure when a join merges distinct Effect construction sites', () => {
  const result = analyzeText(
    'effect://identity-erasure',
    `struct First {}
struct Second {}
fn choose(input: First | Second) -> Effect<i32> {
  return match move input {
    First {} => effect { return 1 }
    Second {} => effect { return 2 }
  }
}`,
  )

  assert.include(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0069',
  )
})

it('rejects heterogeneous callable joins and unknown-sized owned callable returns', () => {
  const joined = analyzeText(
    'callable://identity-erasure',
    `struct First {}
struct Second {}
fn choose(input: First | Second) -> fn(i32) -> i32 {
  return match move input {
    First {} => Intrinsic.i32Add(1)
    Second {} => Intrinsic.i32Add(2)
  }
}`,
  )
  const erased = analyzeText(
    'callable://unknown-owned-return',
    `fn passthrough(callback: once fn(i32) -> i32) -> once fn(i32) -> i32 {
  return move callback
}
fn main() -> i32 { return 0 }`,
  )
  const concrete = analyzeText(
    'callable://concrete-owned-return',
    `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value }
fn make(token: Token) -> once fn(i32) -> i32 { return consume(move token) }
fn main() -> i32 { return 0 }`,
  )

  assert.include(
    joined.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0080',
  )
  assert.include(
    erased.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0081',
  )
  assert.deepEqual(concrete.diagnostics, [])
})

it.effect('rejects retry for a take-once Effect', () =>
  Effect.gen(function* () {
    const result = yield* analyzeWithStdlib(
      'effect://take-retry',
      `struct Payload {}
effect fn consume(value: Payload) -> i32 { return 1 }
fn main() -> i32 {
  let payload = Payload {}
  let retried = consume(move payload) |> Effect.retry(2)
  return run retried
}`,
    )
    assert.isAbove(result.diagnostics.length, 0)
  }),
)

it.effect('derives take-once mapped Effect access and rejects retrying it', () =>
  Effect.gen(function* () {
    const result = yield* analyzeWithStdlib(
      'effect://take-callback-retry',
      `struct Payload { value: i32 }
effect fn succeed(value: i32) -> i32 { return value }
fn consume(value: i32, payload: Payload) -> i32 { return value + payload.value }
fn main() -> i32 {
  let payload = Payload { value: 2 }
  let mapped = succeed(40) |> Effect.map(consume(move payload))
  let retried = mapped |> Effect.retry(2)
  return 0
}`,
    )
    assert.isAbove(result.diagnostics.length, 0)
    const main = functionAt(result, 2)
    const mapped = main.statements.at(1)
    const mappedType =
      mapped?._tag === 'BindStatement' && mapped.binding.initializer.type._tag === 'Available'
        ? mapped.binding.initializer.type.type
        : undefined
    assert.strictEqual(
      mappedType !== undefined && Type.isEffect(mappedType) ? mappedType.access : undefined,
      'Take',
    )
  }),
)

it.effect(
  'propagates source-service requirements through effectful tap without eager dispatch',
  () =>
    Effect.gen(function* () {
      const result = yield* analyzeWithStdlib(
        'effect://logger-tap',
        `struct TapLogger {}
effect fn succeed(value: i32) -> i32 { return value }
effect fn log(value: i32) -> i32 ? &TapLogger { return value }
fn main() -> i32 {
  let logged = succeed(42) |> Effect.tap(log)
  return 0
}`,
      )
      assert.deepEqual(result.diagnostics, [])
      const main = functionAt(result, 2)
      const logged = main.statements.at(0)
      const type =
        logged?._tag === 'BindStatement' && logged.binding.initializer.type._tag === 'Available'
          ? logged.binding.initializer.type.type
          : undefined
      assert.strictEqual(
        type !== undefined && Type.isEffect(type) ? type.requirements.length : 0,
        1,
      )
      assert.strictEqual(
        type !== undefined && Type.isEffect(type)
          ? Type.encode(type.requirements.at(0)?.capability ?? 'never')
          : undefined,
        'effect/logger-tap.TapLogger',
      )
    }),
)

it('rejects failure payloads that retain lexical borrows', () => {
  const result = analyzeText(
    'effect://borrowed-failure',
    `struct BorrowedError { message: &[i32] }
effect fn risky(error: BorrowedError) -> i32 ! BorrowedError {
  fail move error
}`,
  )
  assert.include(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0073',
  )
})

it('accepts detached owned failure payloads', () => {
  const result = analyzeText(
    'effect://owned-failure',
    `struct Detail { code: i32 }
struct OwnedError { detail: Detail }
effect fn risky(error: OwnedError) -> i32 ! OwnedError {
  fail move error
}`,
  )
  assert.notInclude(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0073',
  )
})

it('subtracts a provided capability role from an Effect contract', () => {
  const result = analyzeText(
    'effect://provide-role',
    `struct Clock {}
effect fn work() -> i32 ? &Clock@Left | &Clock@Right { return 42 }
fn main() -> i32 {
  let left = Clock {}
  let right = Clock {}
  let recipe = work() |> Intrinsic.bindRequirement(&left, @Left) |> Intrinsic.bindRequirement(&right, @Right)
  return run recipe
}`,
  )
  assert.deepEqual(result.diagnostics, [])
})

it.effect('requires an explicit role when the same capability has multiple requirements', () =>
  Effect.gen(function* () {
    const result = yield* analyzeWithStdlib(
      'effect://ambiguous-provide-role',
      `struct Clock {}
effect fn work() -> i32 ? &Clock@Left | &Clock@Right { return 42 }
fn main() -> i32 {
  let clock = Clock {}
  let recipe = work() |> Effect.provide(&clock)
  return 0
}`,
    )

    assert.include(
      result.diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0089',
    )
  }),
)

it('requires an exclusive provider for an exclusive capability requirement', () => {
  const result = analyzeText(
    'effect://exclusive-provider',
    `struct Allocator {}
effect fn allocate() -> i32 ? &mut Allocator { return 42 }
fn main() -> i32 {
  let allocator = Allocator {}
  let recipe = allocate() |> Intrinsic.bindRequirement(&allocator)
  return 0
}`,
  )

  assert.include(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0074',
  )
})

it.effect('rejects a concrete provideMut provider without the required capability', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect/provide-mut-conformance',
      new TextEncoder().encode(`struct Clock {}
struct Wrong {}
effect fn read() -> i32 ? &mut Clock { return 42 }
pub fn main() -> i32 {
  let mut wrong = Wrong {}
  let recipe = read() |> Effect.provideMut(&mut wrong)
  return run recipe
}`),
    )

    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0074',
    )
    assert.strictEqual(Analysis.mirOf(snapshot)._tag, 'Unavailable')
  }),
)

it.effect('type-checks provideMut for a custom service', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect/provide-mut-custom-capability',
      new TextEncoder().encode(`struct Clock {}
effect fn read() -> i32 ? &mut Clock { return 42 }
pub fn main() -> i32 {
  let mut clock = Clock {}
  let recipe = Effect.provideMut(read(), &mut clock)
  return run recipe
}`),
    )

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42)
  }),
)

it('owns a moved provider in a take-once Effect wrapper', () => {
  const result = analyzeText(
    'effect://moved-provider',
    `struct Clock {}
effect fn read() -> i32 ? &mut Clock { return 42 }
fn main() -> i32 {
  let clock = Clock {}
  let recipe = read() |> Intrinsic.bindRequirement(move clock)
  return 0
}`,
  )
  const main = result.functions.at(1)
  const recipe = main?.bindings.at(1)?.initializer

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(recipe?.type._tag, 'Available')
  if (recipe?.type._tag !== 'Available' || !Type.isEffect(recipe.type.type)) return
  assert.strictEqual(recipe.type.type.access, 'Take')
})

it.effect('keeps per-run provider acquisition distinct and composes its contract', () =>
  Effect.gen(function* () {
    const result = yield* analyzeWithStdlib(
      'effect://provide-with',
      `struct Clock {}
struct OpenError {}
effect fn read() -> i32 ? &mut Clock { return 42 }
effect fn acquire() -> Clock ! OpenError { return Clock {} }
fn main() -> i32 {
  let recipe = read() |> Effect.provideWith(acquire())
  return 0
}`,
    )
    const main = result.functions.at(2)
    const recipe = main?.bindings.at(0)?.initializer
    const hirRecipe = result.hir.functions.at(2)?.statements.at(0)

    assert.deepEqual(result.diagnostics, [])
    assert.strictEqual(recipe?._tag, 'CallableApply')
    assert.strictEqual(
      hirRecipe?._tag === 'Bind' ? hirRecipe.initializer._tag : undefined,
      'CallableApply',
    )
    assert.strictEqual(recipe?.type._tag, 'Available')
    if (recipe?.type._tag !== 'Available' || !Type.isEffect(recipe.type.type)) return
    assert.deepEqual(recipe.type.type.failures.map(Type.encode), ['effect/provide-with.OpenError'])
    assert.deepEqual(recipe.type.type.requirements, [])
  }),
)

it.effect('defines allocation as an exclusive service Effect with an affine result', () =>
  Effect.gen(function* () {
    const result = yield* analyzeWithStdlib(
      'allocation://capability-contract',
      `fn allocate(layout: Layout) -> Effect<Allocation ! OutOfMemory ? &mut Allocator> {
  return Allocator.allocate(move layout)
}
fn main() -> i32 {
  let allocator = SystemAllocator.make()
  return 42
}`,
    )
    const allocation = result.functions.at(0)?.declaration.returnType
    const operation = result.functions.at(0)?.returnedExpression.type

    assert.deepEqual(result.diagnostics, [])
    assert.strictEqual(allocation?._tag, 'Resolved')
    if (allocation?._tag !== 'Resolved' || !Type.isEffect(allocation.type)) return
    assert.ok(Type.equals(allocation.type.success, Type.allocation))
    assert.deepEqual(allocation.type.failures.map(Type.encode), ['silk/core.OutOfMemory'])
    assert.deepEqual(
      allocation.type.requirements.map((requirement) => ({
        capability: Type.encode(requirement.capability),
        role: requirement.role,
        access: requirement.access,
      })),
      [{ capability: 'silk/core.Allocator', role: 'DefaultRole', access: 'Exclusive' }],
    )
    assert.strictEqual(operation?._tag, 'Available')
    if (operation?._tag === 'Available' && Type.isEffect(operation.type))
      assert.strictEqual(operation.type.access, 'Take')
  }),
)

it.effect('provides Allocator through nominal system and user-authored witnesses', () =>
  Effect.gen(function* () {
    const system = yield* analyzeWithStdlib(
      'allocation://nominal-system-provider',
      `effect fn use(layout: Layout) -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  drop allocation
  return 42
}
pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(system.diagnostics, [])
    const providerType = system.functions.at(0)?.bindings.at(0)?.inferredType
    assert.strictEqual(providerType?._tag, 'Available')
    if (providerType?._tag === 'Available')
      assert.isTrue(Type.equals(providerType.type, Type.systemAllocator))

    const custom = yield* analyzeWithStdlib(
      'allocation://custom-provider',
      `struct TestAllocator { remaining: i32 }
effect fn allocate(self: &mut TestAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  return run Intrinsic.systemAllocationAcquire(move layout)
}
impl Allocator for TestAllocator { allocate: TestAllocator.allocate }
effect fn use(layout: Layout) -> i32 ! OutOfMemory {
  let mut allocator = TestAllocator { remaining: 1 }
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  drop allocation
  return 42
}
pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(custom.diagnostics, [])
  }),
)

it.effect('rejects dynamically shaped or type-incompatible catch handlers', () =>
  Effect.gen(function* () {
    const result = yield* analyzeWithStdlib(
      'effect://bad-handler',
      `struct Problem {}
struct Other {}
effect fn risky() -> i32 ! Problem { fail move Problem {} }
effect fn wrong(problem: Other) -> bool { return true }
fn main() -> i32 {
  let recipe = Effect.catch(risky(), wrong)
  return 0
}`,
    )

    assert.isAbove(result.diagnostics.length, 0)
  }),
)

const functionAt = (result: Elaboration.Result, index: number): Elaboration.FunctionFact =>
  result.functions.at(index) ?? raise(`expected function fact at index ${index}`)

const integerFact = (fact: Elaboration.FunctionFact): Elaboration.IntegerExpressionFact =>
  fact.returnedExpression._tag === 'Integer'
    ? fact.returnedExpression.integer
    : raise('expected an integer returned expression')

const callFact = (
  fact: Elaboration.FunctionFact,
): Extract<Elaboration.ExpressionFact, { readonly _tag: 'Call' }> =>
  fact.returnedExpression._tag === 'Call'
    ? fact.returnedExpression
    : raise('expected a call returned expression')

const identifierFact = (
  fact: Elaboration.FunctionFact,
): Extract<Elaboration.ExpressionFact, { readonly _tag: 'Identifier' }> =>
  fact.returnedExpression._tag === 'Identifier'
    ? fact.returnedExpression
    : raise('expected an identifier returned expression')

const diagnosticView = (result: Elaboration.Result) =>
  result.diagnostics.map((diagnostic) => ({
    code: diagnostic.code,
    start: diagnostic.span.start,
    end: diagnostic.span.end,
    reason: diagnostic.reason,
  }))

const directToken = (node: SyntaxTree.Node, kind: Token.TokenKind): Token.Token | undefined =>
  node.children.find(
    (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === kind,
  )

it('publishes one immutable function fact with exact accepted provenance', () => {
  const parse = parseText('fixture://semantic-accepted.silk', acceptedSource)
  const result = elaborate(parse)
  const fact = functionAt(result, 0)
  const declaration = fact.declaration
  const name = declaration.name
  const returnType = declaration.returnType
  const integer = integerFact(fact)

  assert.strictEqual(result.syntax, parse)
  assert.strictEqual(result.functions.length, 1)
  assert.deepEqual(declaration.id, {
    _tag: 'DeclarationId',
    sourceId: 'fixture://semantic-accepted.silk',
    ordinal: 0,
  })
  assert.strictEqual(declaration.visibility, 'Public')
  assert.strictEqual(declaration.parameterCount, 0)
  assert.strictEqual(name._tag, 'Present')
  if (name._tag !== 'Present') return
  assert.strictEqual(name.spelling, 'main')
  assert.strictEqual(name.token, directToken(declaration.syntax, 'Identifier'))
  assert.strictEqual(returnType._tag, 'Resolved')
  if (returnType._tag !== 'Resolved') return
  assert.strictEqual(returnType.type, 'i32')
  assert.strictEqual(returnType.spelling, 'i32')
  assert.strictEqual(integer._tag, 'Available')
  if (integer._tag !== 'Available') return
  assert.strictEqual(integer.type, 'i32')
  assert.strictEqual(integer.value, 42n)
  assert.deepEqual(fact.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(result.diagnostics, [])

  const directLookup = Elaboration.declarationByName(result, 'main')
  const pipedLookup = pipe(result, Elaboration.declarationByName('main'))
  assert.strictEqual(directLookup._tag, 'Resolved')
  assert.strictEqual(pipedLookup._tag, 'Resolved')
  if (directLookup._tag !== 'Resolved' || pipedLookup._tag !== 'Resolved') return
  assert.strictEqual(directLookup.declaration, declaration)
  assert.strictEqual(pipedLookup.declaration, declaration)
  assert.deepEqual(Elaboration.declarationByName(result, 'other'), {
    _tag: 'Missing',
    spelling: 'other',
  })
  assert.strictEqual(Object.isFrozen(result), true)
  assert.strictEqual(Object.isFrozen(result.functions), true)
  assert.strictEqual(Object.isFrozen(fact), true)
  assert.strictEqual(Object.isFrozen(declaration), true)
  assert.strictEqual(Object.isFrozen(declaration.id), true)
  assert.strictEqual(Object.isFrozen(directLookup), true)
  assert.strictEqual(Object.isFrozen(result.diagnostics), true)
})

it('collects two and three declarations with deterministic source-order identities', () => {
  const two = analyzeText('fixture://two-functions.silk', twoFunctionSource)
  const three = analyzeText('fixture://three-functions.silk', threeFunctionSource)

  assert.deepEqual(
    two.functions.map((fact) => fact.declaration.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    three.functions.map((fact) => fact.declaration.id.ordinal),
    [0, 1, 2],
  )
  assert.deepEqual(
    three.functions.map((fact) =>
      fact.declaration.name._tag === 'Present' ? fact.declaration.name.spelling : 'Unavailable',
    ),
    ['one', 'two', 'three'],
  )
  assert.deepEqual(
    three.functions.map((fact) => {
      const integer = integerFact(fact)
      return integer._tag === 'Available' ? integer.value : undefined
    }),
    [1n, 2n, 3n],
  )
  assert.strictEqual(
    functionAt(two, 0).declaration.syntax.span.start <
      functionAt(two, 1).declaration.syntax.span.start,
    true,
  )
})

it('collects typed parameters and resolves returned identifiers', () => {
  const identity = analyzeText('fixture://identity-parameters.silk', identityCallSource)
  const multiple = analyzeText('fixture://two-parameters.silk', twoParameterSource)
  const identityFunction = functionAt(identity, 0)
  const main = functionAt(identity, 1)
  const returnedIdentifier = identifierFact(identityFunction)
  const returnedCall = callFact(main)

  assert.strictEqual(identityFunction.declaration.parameterCount, 1)
  assert.strictEqual(functionAt(multiple, 0).declaration.parameterCount, 2)
  assert.strictEqual(returnedIdentifier.reference._tag, 'Resolved')
  assert.deepEqual(returnedIdentifier.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(identityFunction.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(returnedCall.reference._tag, 'Resolved')
  assert.deepEqual(returnedCall.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(main.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(identity.diagnostics, [])
  assert.deepEqual(identity.syntax.parserDiagnostics, [])
})

it('publishes ordered function-local parameter identities, types, and lookup', () => {
  const result = analyzeText('fixture://same-parameter-names.silk', sameParameterNamesSource)
  const first = functionAt(result, 0).declaration
  const second = functionAt(result, 1).declaration
  const firstParameter = first.parameters.at(0) ?? raise('expected first parameter')
  const secondParameter = second.parameters.at(0) ?? raise('expected second parameter')
  const directLookup = Elaboration.parameterByName(first, 'value')
  const pipedLookup = pipe(second, Elaboration.parameterByName('value'))

  assert.deepEqual(firstParameter.id, {
    _tag: 'ParameterId',
    function: first.id,
    ordinal: 0,
  })
  assert.deepEqual(secondParameter.id, {
    _tag: 'ParameterId',
    function: second.id,
    ordinal: 0,
  })
  assert.notDeepEqual(firstParameter.id.function, secondParameter.id.function)
  assert.strictEqual(firstParameter.name._tag, 'Present')
  assert.strictEqual(firstParameter.declaredType._tag, 'Resolved')
  assert.strictEqual(firstParameter.syntax.kind, 'ParameterDeclaration')
  assert.strictEqual(directLookup._tag, 'Resolved')
  assert.strictEqual(pipedLookup._tag, 'Resolved')
  if (directLookup._tag !== 'Resolved' || pipedLookup._tag !== 'Resolved') return
  assert.strictEqual(directLookup.parameter, firstParameter)
  assert.strictEqual(pipedLookup.parameter, secondParameter)
  assert.deepEqual(Elaboration.parameterByName(first, 'missing'), {
    _tag: 'Missing',
    spelling: 'missing',
  })
  assert.strictEqual(Object.isFrozen(first.parameters), true)
  assert.strictEqual(Object.isFrozen(firstParameter), true)
  assert.strictEqual(Object.isFrozen(firstParameter.id), true)
})

it('resolves identifier arguments against the caller parameter collection', () => {
  const result = analyzeText(
    'fixture://identifier-argument.silk',
    `pub fn identity(value: i32) -> i32 { return value }
pub fn forward(value: i32) -> i32 { return identity(value) }`,
  )
  const forward = functionAt(result, 1)
  const call = callFact(forward)
  const argument = call.arguments.at(0) ?? raise('expected argument')
  const expression = argument.expression

  assert.strictEqual(expression._tag, 'Identifier')
  if (expression._tag !== 'Identifier') return
  assert.strictEqual(expression.reference._tag, 'Resolved')
  if (expression.reference._tag !== 'Resolved') return
  assert.strictEqual(expression.reference.parameter, forward.declaration.parameters.at(0))
  assert.deepEqual(expression.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(result.diagnostics, [])
})

it('publishes ordered argument identities, expressions, mappings, and compatible contracts', () => {
  const one = analyzeText('fixture://one-argument-contract.silk', identityCallSource)
  const two = analyzeText('fixture://two-argument-contract.silk', twoArgumentCallSource)
  const oneCall = callFact(functionAt(one, 1))
  const twoCall = callFact(functionAt(two, 1))
  const firstArgument = oneCall.arguments.at(0) ?? raise('expected first argument')

  assert.deepEqual(firstArgument.id, {
    _tag: 'ArgumentId',
    function: functionAt(one, 1).declaration.id,
    callSpan: oneCall.syntax.span,
    ordinal: 0,
  })
  assert.strictEqual(firstArgument.expression._tag, 'Integer')
  if (firstArgument.expression._tag !== 'Integer') return
  assert.strictEqual(firstArgument.expression.integer._tag, 'Available')
  if (firstArgument.expression.integer._tag !== 'Available') return
  assert.strictEqual(firstArgument.expression.integer.value, 42n)
  assert.deepEqual(firstArgument.type, { _tag: 'Available', type: 'i32' })
  assert.strictEqual(firstArgument.syntax, firstArgument.expression.syntax)
  assert.strictEqual(oneCall.mappings.at(0)?.argument, firstArgument)
  assert.strictEqual(
    oneCall.mappings.at(0)?.parameter,
    functionAt(one, 0).declaration.parameters.at(0),
  )
  assert.deepEqual(oneCall.contract, {
    _tag: 'Compatible',
    expectedCount: 1,
    actualCount: 1,
    typeArguments: [],
    substitution: new Map(),
  })
  assert.deepEqual(
    twoCall.arguments.map((argument) => argument.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    twoCall.mappings.map((mapping) => [mapping.argument.id.ordinal, mapping.parameter.id.ordinal]),
    [
      [0, 0],
      [1, 1],
    ],
  )
  assert.deepEqual(twoCall.contract, {
    _tag: 'Compatible',
    expectedCount: 2,
    actualCount: 2,
    typeArguments: [],
    substitution: new Map(),
  })
  assert.strictEqual(Object.isFrozen(oneCall.arguments), true)
  assert.strictEqual(Object.isFrozen(firstArgument), true)
  assert.strictEqual(Object.isFrozen(oneCall.mappings), true)
  assert.strictEqual(Object.isFrozen(oneCall.contract), true)
})

it('analyzes nested call arguments recursively from their leaves outward', () => {
  const result = analyzeText('fixture://nested-call-semantic.silk', nestedCallSource)
  const main = functionAt(result, 1)
  const call = callFact(main)
  const argument = call.arguments.at(0) ?? raise('expected nested argument')

  assert.strictEqual(argument.expression._tag, 'Call')
  if (argument.expression._tag !== 'Call') return
  const inner = argument.expression
  const innerArgument = inner.arguments.at(0) ?? raise('expected inner argument')
  assert.strictEqual(argument.syntax, inner.syntax)
  assert.strictEqual(innerArgument.expression._tag, 'Integer')
  assert.deepEqual(innerArgument.type, { _tag: 'Available', type: 'i32' })
  assert.strictEqual(inner.reference._tag, 'Resolved')
  assert.strictEqual(inner.contract._tag, 'Compatible')
  assert.strictEqual(inner.mappings.at(0)?.argument, innerArgument)
  assert.strictEqual(inner.mappings.at(0)?.parameter.id.ordinal, 0)
  assert.deepEqual(inner.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(argument.type, { _tag: 'Available', type: 'i32' })
  assert.strictEqual(call.contract._tag, 'Compatible')
  assert.deepEqual(call.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(main.returnCompatibility, { _tag: 'Compatible' })
  assert.notDeepEqual(inner.syntax.span, call.syntax.span)
  assert.deepEqual(argument.id.callSpan, call.syntax.span)
  assert.deepEqual(innerArgument.id.callSpan, inner.syntax.span)
  assert.deepEqual(result.syntax.parserDiagnostics, [])
  assert.deepEqual(result.diagnostics, [])
})

it('preserves nested sibling order and call-local argument identities', () => {
  const result = analyzeText('fixture://nested-siblings.silk', nestedSiblingCallsSource)
  const outer = callFact(functionAt(result, 2))
  const expressions = outer.arguments.map((argument) => argument.expression)

  assert.deepEqual(
    outer.arguments.map((argument) => argument.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    expressions.map((expression) => expression._tag),
    ['Call', 'Call'],
  )
  const left = expressions.at(0) ?? raise('expected left nested call')
  const right = expressions.at(1) ?? raise('expected right nested call')
  if (left._tag !== 'Call' || right._tag !== 'Call') return
  assert.ok(left.syntax.span.start < right.syntax.span.start)
  assert.deepEqual(
    left.arguments.map((argument) => argument.id.ordinal),
    [0],
  )
  assert.deepEqual(
    right.arguments.map((argument) => argument.id.ordinal),
    [0],
  )
  assert.deepEqual(left.arguments.at(0)?.id.callSpan, left.syntax.span)
  assert.deepEqual(right.arguments.at(0)?.id.callSpan, right.syntax.span)
  assert.strictEqual(left.contract._tag, 'Compatible')
  assert.strictEqual(right.contract._tag, 'Compatible')
  assert.strictEqual(outer.contract._tag, 'Compatible')
})

it('propagates an unresolved inner target only to dependent outer facts', () => {
  const result = analyzeText('fixture://unresolved-nested.silk', unresolvedNestedCallSource)
  const outer = callFact(functionAt(result, 1))
  const inner = outer.arguments.at(0)?.expression ?? raise('expected nested expression')

  assert.strictEqual(inner._tag, 'Call')
  if (inner._tag !== 'Call') return
  assert.strictEqual(inner.reference._tag, 'Missing')
  assert.deepEqual(inner.type, { _tag: 'Unavailable' })
  assert.strictEqual(inner.contract._tag, 'Unavailable')
  if (inner.contract._tag !== 'Unavailable') return
  assert.strictEqual(inner.contract.reason._tag, 'UnavailableCallTarget')
  assert.strictEqual(outer.mappings.length, 1)
  assert.strictEqual(outer.contract._tag, 'Unavailable')
  if (outer.contract._tag !== 'Unavailable') return
  assert.strictEqual(outer.contract.reason._tag, 'UnavailableMappedType')
  assert.deepEqual(outer.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0004'],
  )
})

it('diagnoses an incompatible inner call exactly once without inventing an outer mismatch', () => {
  const result = analyzeText('fixture://incompatible-nested.silk', incompatibleNestedCallSource)
  const outer = callFact(functionAt(result, 1))
  const inner = outer.arguments.at(0)?.expression ?? raise('expected nested expression')

  assert.strictEqual(inner._tag, 'Call')
  if (inner._tag !== 'Call') return
  assert.strictEqual(inner.contract._tag, 'ArityMismatch')
  assert.deepEqual(inner.type, { _tag: 'Available', type: 'i32' })
  assert.strictEqual(outer.contract._tag, 'Compatible')
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0078'],
  )
})

it('analyzes representative deep nesting deterministically', () => {
  const depth = 64
  const expression = `${'identity('.repeat(depth)}42${')'.repeat(depth)}`
  const source = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return ${expression} }`
  const first = analyzeText('fixture://deep-nested-semantic.silk', source)
  const second = analyzeText('fixture://deep-nested-semantic.silk', source)
  let current: Elaboration.ExpressionFact = functionAt(first, 1).returnedExpression
  let calls = 0

  while (current._tag === 'Call') {
    calls += 1
    current = current.arguments.at(0)?.expression ?? raise('expected nested argument')
  }

  assert.strictEqual(calls, depth)
  assert.strictEqual(current._tag, 'Integer')
  assert.deepEqual(first, second)
  assert.deepEqual(first.diagnostics, [])
})

it('keeps damaged nested syntax parser-owned while retaining recursive facts', () => {
  const result = analyzeText('fixture://damaged-nested-call.silk', damagedNestedCallSource)
  const main = functionAt(result, 1)
  const call = callFact(main)
  const argument = call.arguments.at(0) ?? raise('expected damaged nested argument')

  assert.strictEqual(argument.expression._tag, 'Call')
  if (argument.expression._tag !== 'Call') return
  assert.strictEqual(argument.expression.contract._tag, 'Unavailable')
  if (argument.expression.contract._tag !== 'Unavailable') return
  assert.strictEqual(argument.expression.contract.reason._tag, 'UnavailableCallSyntax')
  assert.deepEqual(argument.type, { _tag: 'Unavailable' })
  assert.strictEqual(call.mappings.length, 1)
  assert.strictEqual(call.contract._tag, 'Unavailable')
  if (call.contract._tag !== 'Unavailable') return
  assert.strictEqual(call.contract.reason._tag, 'UnavailableMappedType')
  assert.deepEqual(
    result.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('keeps flat argument semantics unchanged beside recursive nested facts', () => {
  const flat = analyzeText('fixture://flat-call-regression.silk', identityCallSource)
  const nested = analyzeText('fixture://nested-call-regression.silk', nestedCallSource)
  const flatCall = callFact(functionAt(flat, 1))
  const nestedCall = callFact(functionAt(nested, 1))

  assert.strictEqual(flatCall.arguments.at(0)?.expression._tag, 'Integer')
  assert.strictEqual(flatCall.contract._tag, 'Compatible')
  assert.deepEqual(flatCall.type, { _tag: 'Available', type: 'i32' })
  assert.strictEqual(nestedCall.arguments.at(0)?.expression._tag, 'Call')
  assert.strictEqual(nestedCall.contract._tag, 'Compatible')
  assert.deepEqual(nested.diagnostics, [])
})

it('checks zero-, one-, and two-argument compatible calls', () => {
  const zero = callFact(functionAt(analyzeText('fixture://zero-contract.silk', validCallSource), 1))
  const one = callFact(
    functionAt(analyzeText('fixture://one-contract.silk', identityCallSource), 1),
  )
  const two = callFact(
    functionAt(analyzeText('fixture://two-contract.silk', twoArgumentCallSource), 1),
  )

  assert.strictEqual(zero.contract._tag, 'Compatible')
  assert.strictEqual(one.contract._tag, 'Compatible')
  assert.strictEqual(two.contract._tag, 'Compatible')
})

it('forms the one leading section and still diagnoses deeper arity mismatches', () => {
  const tooFew = analyzeText('fixture://too-few.silk', tooFewArgumentsSource)
  const tooMany = analyzeText('fixture://too-many.silk', tooManyArgumentsSource)
  const fewSection = functionAt(tooFew, 1).returnedExpression
  const manyFunction = functionAt(tooMany, 1)
  const manyCall = callFact(manyFunction)

  assert.strictEqual(fewSection._tag, 'CallableSection')
  if (fewSection._tag !== 'CallableSection') return
  assert.strictEqual(fewSection.omittedParameter, 0)
  assert.strictEqual(fewSection.captures.length, 1)
  assert.strictEqual(
    fewSection.type._tag === 'Available' ? Type.encode(fewSection.type.type) : undefined,
    'fn(i32) -> i32',
  )
  assert.deepEqual(manyCall.contract, {
    _tag: 'ArityMismatch',
    expectedCount: 1,
    actualCount: 2,
  })
  assert.strictEqual(manyCall.mappings.length, 1)
  assert.deepEqual(manyFunction.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(manyCall.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(tooFew.diagnostics, [])
  assert.deepEqual(
    tooMany.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0007'],
  )
  assert.strictEqual(tooMany.diagnostics.at(0)?.span, manyCall.syntax.span)
})

it('withholds contracts without cascading when a prerequisite is unavailable', () => {
  const parameterType = analyzeText(
    'fixture://unavailable-parameter-contract.silk',
    unavailableParameterContractSource,
  )
  const argumentType = analyzeText(
    'fixture://unavailable-argument-contract.silk',
    unavailableArgumentContractSource,
  )
  const missingTarget = analyzeText('fixture://missing-contract-target.silk', unknownCallSource)
  const ambiguousTarget = analyzeText(
    'fixture://ambiguous-contract-target.silk',
    ambiguousCallSource,
  )
  const recovered = analyzeText('fixture://recovered-argument.silk', recoveredArgumentSource)
  const parameterCall = callFact(functionAt(parameterType, 1))
  const argumentCall = callFact(functionAt(argumentType, 1))
  const missingCall = callFact(functionAt(missingTarget, 0))
  const ambiguousCall = callFact(functionAt(ambiguousTarget, 2))
  const recoveredCall = callFact(functionAt(recovered, 1))

  assert.strictEqual(parameterCall.contract._tag, 'Unavailable')
  assert.strictEqual(parameterCall.mappings.length, 1)
  assert.strictEqual(argumentCall.contract._tag, 'Unavailable')
  assert.strictEqual(argumentCall.mappings.length, 1)
  assert.strictEqual(missingCall.contract._tag, 'Unavailable')
  assert.strictEqual(missingCall.mappings.length, 0)
  assert.strictEqual(ambiguousCall.contract._tag, 'Unavailable')
  assert.strictEqual(ambiguousCall.mappings.length, 0)
  assert.strictEqual(recoveredCall.contract._tag, 'Unavailable')
  assert.strictEqual(recoveredCall.arguments.length, 0)
  assert.strictEqual(recoveredCall.mappings.length, 0)
  assert.deepEqual(
    parameterType.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001'],
  )
  assert.deepEqual(
    argumentType.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0006'],
  )
  assert.deepEqual(
    missingTarget.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0004'],
  )
  assert.deepEqual(
    recovered.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
  assert.deepEqual(recovered.diagnostics, [])
})

it('keeps call contracts deterministic across fresh analyses', () => {
  const first = analyzeText('fixture://contract-determinism.silk', tooManyArgumentsSource)
  const second = analyzeText('fixture://contract-determinism.silk', tooManyArgumentsSource)

  assert.deepEqual(first.functions, second.functions)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
})

it('keeps parameter lookup isolated to its owning function', () => {
  const result = analyzeText(
    'fixture://cross-function-parameter.silk',
    crossFunctionParameterSource,
  )
  const owner = functionAt(result, 0)
  const other = functionAt(result, 1)
  const ownerReference = identifierFact(owner)
  const otherReference = identifierFact(other)

  assert.strictEqual(ownerReference.reference._tag, 'Resolved')
  assert.strictEqual(otherReference.reference._tag, 'Missing')
  assert.deepEqual(other.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0006'],
  )
})

it('diagnoses unknown local names at their exact reference spans', () => {
  const result = analyzeText(
    'fixture://unknown-parameter-reference.silk',
    unknownParameterReferenceSource,
  )
  const reference = identifierFact(functionAt(result, 0)).reference

  assert.strictEqual(reference._tag, 'Missing')
  if (reference._tag !== 'Missing') return
  assert.deepEqual(diagnosticView(result), [
    {
      code: 'SEM0006',
      start: reference.token.span.start,
      end: reference.token.span.end,
      reason: { _tag: 'UnknownValueReference', spelling: 'missing' },
    },
  ])
})

it('does not reinterpret an unknown final value as an invalid assignment place', () => {
  const result = analyzeText(
    'fixture://unknown-recovered-return.silk',
    'pub fn main() -> i32 { missing }',
  )

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      reason: diagnostic.reason,
    })),
    [
      {
        code: 'SEM0006',
        reason: { _tag: 'UnknownValueReference', spelling: 'missing' },
      },
    ],
  )
})

it('suppresses invalid-place cascades for unknown assignment roots', () => {
  const result = analyzeText(
    'fixture://unknown-assignment-root.silk',
    'pub fn main() -> i32 { missing = 1 return 0 }',
  )

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0006'],
  )
})

it('preserves duplicate parameters and reports declaration-owned ambiguity', () => {
  const duplicate = analyzeText('fixture://duplicate-parameter.silk', duplicateParameterSource)
  const triple = analyzeText(
    'fixture://triple-duplicate-parameter.silk',
    tripleDuplicateParameterSource,
  )
  const declaration = functionAt(duplicate, 0).declaration
  const lookup = Elaboration.parameterByName(declaration, 'value')
  const reference = identifierFact(functionAt(duplicate, 0)).reference

  assert.strictEqual(lookup._tag, 'Ambiguous')
  assert.strictEqual(reference._tag, 'Ambiguous')
  if (lookup._tag !== 'Ambiguous' || reference._tag !== 'Ambiguous') return
  assert.deepEqual(
    lookup.parameters.map((parameter) => parameter.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    reference.parameters.map((parameter) => parameter.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    duplicate.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0005'],
  )
  assert.deepEqual(
    triple.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0005', 'SEM0005'],
  )
})

it('resolves parameter types and preserves parser-owned damaged states', () => {
  const unknown = analyzeText('fixture://unknown-parameter-type.silk', unknownParameterTypeSource)
  const missingName = analyzeText(
    'fixture://missing-parameter-name.silk',
    missingParameterNameSource,
  )
  const missingType = analyzeText(
    'fixture://missing-parameter-type.silk',
    missingParameterTypeSource,
  )
  const damagedReference = analyzeText('fixture://damaged-identifier.silk', damagedIdentifierSource)
  const unknownParameter = functionAt(unknown, 0).declaration.parameters.at(0)
  const missingNameParameter = functionAt(missingName, 0).declaration.parameters.at(0)
  const missingTypeParameter = functionAt(missingType, 0).declaration.parameters.at(0)

  assert.strictEqual(unknownParameter?.declaredType._tag, 'Unresolved')
  assert.deepEqual(
    unknown.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001'],
  )
  assert.strictEqual(missingNameParameter?.name._tag, 'Unavailable')
  assert.strictEqual(missingTypeParameter?.declaredType._tag, 'Unavailable')
  assert.deepEqual(missingName.diagnostics, [])
  assert.deepEqual(missingType.diagnostics, [])
  assert.strictEqual(identifierFact(functionAt(damagedReference, 0)).reference._tag, 'Unavailable')
  assert.deepEqual(damagedReference.diagnostics, [])
  assert.deepEqual(
    damagedReference.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
})

it('keeps parameter facts and diagnostics deterministic across fresh analyses', () => {
  const source = `${duplicateParameterSource}\n${unknownParameterReferenceSource}`
  const first = analyzeText('fixture://parameter-determinism.silk', source)
  const second = analyzeText('fixture://parameter-determinism.silk', source)

  assert.deepEqual(first.functions, second.functions)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
})

it('resolves a call to an earlier declaration and propagates its type', () => {
  const result = analyzeText('fixture://valid-call.silk', validCallSource)
  const answer = functionAt(result, 0)
  const main = functionAt(result, 1)
  const returned = callFact(main)

  assert.strictEqual(answer.returnedExpression._tag, 'Integer')
  assert.strictEqual(integerFact(answer)._tag, 'Available')
  assert.deepEqual(answer.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(returned.syntax.kind, 'CallExpression')
  assert.strictEqual(returned.reference._tag, 'Resolved')
  if (returned.reference._tag !== 'Resolved') return
  assert.strictEqual(returned.reference.spelling, 'answer')
  const callee = SyntaxTree.directNode(returned.syntax, 'IdentifierExpression')
  assert.strictEqual(
    returned.reference.token,
    callee === undefined ? undefined : directToken(callee, 'Identifier'),
  )
  assert.strictEqual(returned.reference.declaration, answer.declaration)
  assert.deepEqual(returned.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(main.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(result.syntax.parserDiagnostics, [])
  assert.strictEqual(Object.isFrozen(returned), true)
  assert.strictEqual(Object.isFrozen(returned.reference), true)
})

it('resolves forward and self calls without evaluating them', () => {
  const forward = analyzeText('fixture://forward-call.silk', forwardCallSource)
  const self = analyzeText('fixture://self-call.silk', selfCallSource)
  const forwardCall = callFact(functionAt(forward, 0))
  const selfCall = callFact(functionAt(self, 0))

  assert.strictEqual(forwardCall.reference._tag, 'Resolved')
  assert.strictEqual(selfCall.reference._tag, 'Resolved')
  if (forwardCall.reference._tag !== 'Resolved' || selfCall.reference._tag !== 'Resolved') return
  assert.strictEqual(forwardCall.reference.declaration, functionAt(forward, 1).declaration)
  assert.strictEqual(selfCall.reference.declaration, functionAt(self, 0).declaration)
  assert.deepEqual(forwardCall.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(selfCall.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(functionAt(forward, 0).returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(functionAt(self, 0).returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(forward.diagnostics, [])
  assert.deepEqual(self.diagnostics, [])
})

it('diagnoses an unknown call target at the exact callee span', () => {
  const result = analyzeText('fixture://unknown-call.silk', unknownCallSource)
  const fact = functionAt(result, 0)
  const returned = callFact(fact)

  assert.strictEqual(returned.reference._tag, 'Missing')
  if (returned.reference._tag !== 'Missing') return
  assert.strictEqual(returned.reference.spelling, 'missing')
  assert.deepEqual(returned.type, { _tag: 'Unavailable' })
  assert.deepEqual(fact.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(diagnosticView(result), [
    {
      code: 'SEM0004',
      start: returned.reference.token.span.start,
      end: returned.reference.token.span.end,
      reason: { _tag: 'UnknownFunction', spelling: 'missing' },
    },
  ])
})

it('preserves every ambiguous target without adding a call-site diagnostic', () => {
  const result = analyzeText('fixture://ambiguous-call.silk', ambiguousCallSource)
  const fact = functionAt(result, 2)
  const returned = callFact(fact)

  assert.strictEqual(returned.reference._tag, 'Ambiguous')
  if (returned.reference._tag !== 'Ambiguous') return
  assert.deepEqual(
    returned.reference.declarations.map((declaration) => declaration.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(returned.type, { _tag: 'Unavailable' })
  assert.deepEqual(fact.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0003'],
  )
  assert.strictEqual(Object.isFrozen(returned.reference.declarations), true)
})

it('keeps target type resolution separate from target body compatibility', () => {
  const unresolvedType = analyzeText(
    'fixture://unresolved-target-type-call.silk',
    unresolvedTargetTypeCallSource,
  )
  const damagedBody = analyzeText(
    'fixture://damaged-target-body-call.silk',
    damagedTargetBodyCallSource,
  )
  const unresolvedCall = callFact(functionAt(unresolvedType, 1))
  const damagedBodyCall = callFact(functionAt(damagedBody, 1))

  assert.strictEqual(unresolvedCall.reference._tag, 'Resolved')
  assert.deepEqual(unresolvedCall.type, { _tag: 'Unavailable' })
  assert.deepEqual(functionAt(unresolvedType, 1).returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    unresolvedType.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001'],
  )

  assert.strictEqual(damagedBodyCall.reference._tag, 'Resolved')
  assert.deepEqual(damagedBodyCall.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(functionAt(damagedBody, 0).returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(functionAt(damagedBody, 1).returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(
    damagedBody.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0002'],
  )
})

it('types empty parentheses as unit', () => {
  const result = analyzeText('fixture://missing-call-callee.silk', missingCallCalleeSource)
  const fact = functionAt(result, 0)
  const returned = fact.returnedExpression

  assert.strictEqual(returned._tag, 'Unit')
  assert.deepEqual(fact.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(result.syntax.parserDiagnostics, [])
  assert.deepEqual(result.diagnostics, [])
})

it('resolves present callees, accepts unchecked arguments, and withholds damaged calls', () => {
  const missingParenthesis = analyzeText(
    'fixture://missing-call-right-parenthesis.silk',
    missingCallRightParenthesisSource,
  )
  const uncheckedArgument = analyzeText(
    'fixture://unchecked-call-argument.silk',
    identityCallSource,
  )

  const damagedFact = functionAt(missingParenthesis, 1)
  const damagedCall = callFact(damagedFact)
  assert.strictEqual(damagedCall.reference._tag, 'Resolved')
  assert.deepEqual(damagedCall.type, { _tag: 'Unavailable' })
  assert.deepEqual(damagedFact.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(missingParenthesis.diagnostics, [])

  const uncheckedFact = functionAt(uncheckedArgument, 1)
  const uncheckedCall = callFact(uncheckedFact)
  assert.strictEqual(uncheckedCall.reference._tag, 'Resolved')
  assert.deepEqual(uncheckedCall.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(uncheckedFact.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(uncheckedArgument.syntax.parserDiagnostics, [])
  assert.deepEqual(uncheckedArgument.diagnostics, [])

  assert.deepEqual(
    missingParenthesis.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
})

it('publishes deterministic resolved and damaged call facts across fresh analyses', () => {
  const first = analyzeText('fixture://deterministic-call.silk', ambiguousCallSource)
  const second = analyzeText('fixture://deterministic-call.silk', ambiguousCallSource)

  assert.deepEqual(first.functions, second.functions)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
})

it('orders type, integer, duplicate, and unknown-call diagnostics by source span', () => {
  const result = analyzeText('fixture://mixed-resolution-damage.silk', mixedResolutionDamageSource)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001', 'SEM0002', 'SEM0003', 'SEM0004'],
  )
  assert.deepEqual(result.syntax.parserDiagnostics, [])
})

it('uses deterministic source-local identities across fresh results', () => {
  const first = analyzeText('fixture://same.silk', twoFunctionSource)
  const second = analyzeText('fixture://same.silk', twoFunctionSource)
  const other = analyzeText('fixture://other.silk', twoFunctionSource)

  assert.deepEqual(
    first.functions.map((fact) => fact.declaration.id),
    second.functions.map((fact) => fact.declaration.id),
  )
  assert.notDeepEqual(
    first.functions.map((fact) => fact.declaration.id),
    other.functions.map((fact) => fact.declaration.id),
  )
})

it('keeps missing declaration names unavailable and out of lookup', () => {
  const single = analyzeText('fixture://missing-name.silk', missingNameSource)
  const multiple = analyzeText('fixture://missing-second-name.silk', missingSecondNameSource)

  assert.strictEqual(functionAt(single, 0).declaration.name._tag, 'Unavailable')
  assert.strictEqual(functionAt(multiple, 1).declaration.name._tag, 'Unavailable')
  assert.deepEqual(Elaboration.declarationByName(multiple, ''), {
    _tag: 'Missing',
    spelling: '',
  })
  assert.deepEqual(single.diagnostics, [])
  assert.deepEqual(multiple.diagnostics, [])
  assert.deepEqual(
    multiple.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
})

it('resolves unique names and reports every duplicate as ambiguous', () => {
  const unique = analyzeText('fixture://unique.silk', twoFunctionSource)
  const duplicate = analyzeText('fixture://duplicate.silk', duplicateNameSource)
  const answer = Elaboration.declarationByName(unique, 'answer')
  const main = Elaboration.declarationByName(unique, 'main')
  const same = Elaboration.declarationByName(duplicate, 'same')

  assert.strictEqual(answer._tag, 'Resolved')
  assert.strictEqual(main._tag, 'Resolved')
  assert.strictEqual(same._tag, 'Ambiguous')
  if (answer._tag !== 'Resolved' || main._tag !== 'Resolved' || same._tag !== 'Ambiguous') return
  assert.strictEqual(answer.declaration, functionAt(unique, 0).declaration)
  assert.strictEqual(main.declaration, functionAt(unique, 1).declaration)
  assert.deepEqual(
    same.declarations,
    duplicate.functions.map((fact) => fact.declaration),
  )
  assert.strictEqual(Object.isFrozen(same), true)
  assert.strictEqual(Object.isFrozen(same.declarations), true)
})

it('diagnoses later duplicate names at their exact spans with original provenance', () => {
  const result = analyzeText('fixture://duplicate.silk', duplicateNameSource)
  const firstName = functionAt(result, 0).declaration.name
  const secondName = functionAt(result, 1).declaration.name

  assert.strictEqual(firstName._tag, 'Present')
  assert.strictEqual(secondName._tag, 'Present')
  if (firstName._tag !== 'Present' || secondName._tag !== 'Present') return
  assert.strictEqual(result.diagnostics.length, 1)
  const diagnostic = result.diagnostics.at(0) ?? raise('expected duplicate diagnostic')
  assert.strictEqual(diagnostic.code, 'SEM0003')
  assert.strictEqual(diagnostic.span, secondName.token.span)
  assert.deepEqual(diagnostic.reason, {
    _tag: 'DuplicateDeclarationName',
    spelling: 'same',
    originalSpan: firstName.token.span,
  })
  assert.strictEqual(Object.isFrozen(diagnostic), true)
  assert.strictEqual(Object.isFrozen(diagnostic.reason), true)
})

it('diagnoses the second and third occurrence of one name', () => {
  const result = analyzeText('fixture://triple-duplicate.silk', tripleDuplicateNameSource)
  const lookup = Elaboration.declarationByName(result, 'same')

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0003', 'SEM0003'],
  )
  assert.strictEqual(lookup._tag, 'Ambiguous')
  if (lookup._tag !== 'Ambiguous') return
  assert.strictEqual(lookup.declarations.length, 3)
  assert.deepEqual(
    lookup.declarations.map((declaration) => declaration.id.ordinal),
    [0, 1, 2],
  )
})

it('analyzes return types, integers, and compatibility independently', () => {
  const result = analyzeText('fixture://mixed-function-damage.silk', mixedFunctionDamageSource)
  const valid = functionAt(result, 0)
  const damaged = functionAt(result, 1)

  assert.strictEqual(valid.declaration.returnType._tag, 'Resolved')
  assert.strictEqual(integerFact(valid)._tag, 'Available')
  assert.deepEqual(valid.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(damaged.declaration.returnType._tag, 'Unresolved')
  assert.strictEqual(integerFact(damaged)._tag, 'OutOfRange')
  assert.deepEqual(damaged.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001', 'SEM0002'],
  )
})

it('preserves existing type and integer edge behavior per function', () => {
  const unknown = functionAt(analyzeText('fixture://unknown-type.silk', unknownTypeSource), 0)
  const damaged = analyzeText('fixture://damaged-type.silk', damagedTypeSource)
  const boundary = functionAt(analyzeText('fixture://i32-boundary.silk', i32BoundarySource), 0)
  const overflow = analyzeText('fixture://i32-overflow.silk', overflowSource)
  const beyondSafe = analyzeText('fixture://beyond-safe.silk', beyondSafeIntegerSource)
  const missingInteger = analyzeText('fixture://missing-integer.silk', missingIntegerSource)

  assert.strictEqual(unknown.declaration.returnType._tag, 'Unresolved')
  assert.deepEqual(unknown.returnCompatibility, { _tag: 'Unavailable' })
  assert.strictEqual(functionAt(damaged, 0).declaration.returnType._tag, 'Unavailable')
  assert.deepEqual(damaged.diagnostics, [])
  assert.deepEqual(
    damaged.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
  const boundaryInteger = integerFact(boundary)
  assert.strictEqual(boundaryInteger._tag, 'Available')
  if (boundaryInteger._tag !== 'Available') return
  assert.strictEqual(boundaryInteger.value, 2147483647n)
  assert.deepEqual(boundary.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(integerFact(functionAt(overflow, 0))._tag, 'OutOfRange')
  assert.strictEqual(integerFact(functionAt(beyondSafe, 0))._tag, 'OutOfRange')
  assert.deepEqual(
    diagnosticView(beyondSafe).map((diagnostic) => diagnostic.reason),
    [
      {
        _tag: 'IntegerOutOfRange',
        spelling: '90071992547409931234567890',
        maximum: 2147483647,
        minimum: -2147483648,
      },
    ],
  )
  assert.strictEqual(functionAt(missingInteger, 0).returnedExpression._tag, 'Unit')
  assert.deepEqual(missingInteger.diagnostics, [])
})

it('keeps parser and semantic diagnostics in their owning ordered collections', () => {
  const result = analyzeText(
    'fixture://parser-and-semantic-damage.silk',
    parserAndSemanticDamageSource,
  )

  assert.deepEqual(
    result.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001', 'SEM0003', 'SEM0002'],
  )
  assert.strictEqual(functionAt(result, 1).declaration.name._tag, 'Unavailable')
  assert.strictEqual(functionAt(result, 1).declaration.returnType._tag, 'Unresolved')
  assert.strictEqual(integerFact(functionAt(result, 2))._tag, 'OutOfRange')
})

it('is deterministic across repeated fresh multi-function results', () => {
  const first = analyzeText('fixture://semantic-deterministic.silk', parserAndSemanticDamageSource)
  const second = analyzeText('fixture://semantic-deterministic.silk', parserAndSemanticDamageSource)

  assert.deepEqual(first.functions, second.functions)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
  assert.deepEqual(
    Elaboration.declarationByName(first, 'same'),
    Elaboration.declarationByName(second, 'same'),
  )
})

it('surfaces duplicate originals as labeled related spans', () => {
  const declarations = analyzeText('fixture://duplicate-related.silk', duplicateNameSource)
  const parameters = analyzeText(
    'fixture://duplicate-parameter-related.silk',
    duplicateParameterSource,
  )
  const firstName = functionAt(declarations, 0).declaration.name
  const firstParameterName = functionAt(parameters, 0).declaration.parameters.at(0)?.name

  assert.strictEqual(firstName._tag, 'Present')
  assert.strictEqual(firstParameterName?._tag, 'Present')
  if (firstName._tag !== 'Present' || firstParameterName?._tag !== 'Present') return
  assert.deepEqual(declarations.diagnostics.at(0)?.relatedSpans, [
    { label: 'first declared here', span: firstName.token.span },
  ])
  assert.deepEqual(parameters.diagnostics.at(0)?.relatedSpans, [
    { label: 'first declared here', span: firstParameterName.token.span },
  ])
})

it('links unavailable call contracts to the unresolved-target diagnostic without duplication', () => {
  const result = analyzeText('fixture://caused-contract.silk', unknownCallSource)
  const call = callFact(functionAt(result, 0))
  const diagnostic = result.diagnostics.at(0) ?? raise('expected an unknown-function diagnostic')

  assert.strictEqual(result.diagnostics.length, 1)
  assert.strictEqual(diagnostic.code, 'SEM0004')
  assert.strictEqual(call.reference._tag, 'Missing')
  if (call.reference._tag !== 'Missing') return
  const referenceCause = call.reference.cause ?? raise('expected a caused missing reference')
  assert.strictEqual(
    Diagnostic.identityEquals(referenceCause, Diagnostic.identity(diagnostic)),
    true,
  )
  assert.strictEqual(call.contract._tag, 'Unavailable')
  if (call.contract._tag !== 'Unavailable') return
  const contractCause = call.contract.cause ?? raise('expected a caused unavailable contract')
  assert.strictEqual(
    Diagnostic.identityEquals(contractCause, Diagnostic.identity(diagnostic)),
    true,
  )
})

it('links unresolved types and missing parameter references to their diagnostics', () => {
  const types = analyzeText('fixture://caused-type.silk', unknownTypeSource)
  const references = analyzeText('fixture://caused-reference.silk', unknownParameterReferenceSource)
  const returnType = functionAt(types, 0).declaration.returnType
  const reference = identifierFact(functionAt(references, 0)).reference

  assert.strictEqual(returnType._tag, 'Unresolved')
  if (returnType._tag !== 'Unresolved') return
  const typeDiagnostic = types.diagnostics.at(0) ?? raise('expected an unknown-type diagnostic')
  const typeCause = returnType.cause ?? raise('expected a caused unresolved type')
  assert.strictEqual(
    Diagnostic.identityEquals(typeCause, Diagnostic.identity(typeDiagnostic)),
    true,
  )

  assert.strictEqual(reference._tag, 'Missing')
  if (reference._tag !== 'Missing') return
  const referenceDiagnostic =
    references.diagnostics.at(0) ?? raise('expected an unknown-parameter diagnostic')
  const referenceCause = reference.cause ?? raise('expected a caused missing reference')
  assert.strictEqual(
    Diagnostic.identityEquals(referenceCause, Diagnostic.identity(referenceDiagnostic)),
    true,
  )
})

it('canonicalizes operator facts with typed builtin operand mappings', () => {
  const result = analyzeText(
    'fixture://operators.silk',
    'pub fn main() -> bool { return !(2 + 3 * 4 == 14) }',
  )
  const returned = functionAt(result, 0).returnedExpression

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(returned._tag, 'Operator')
  if (returned._tag !== 'Operator') return
  assert.strictEqual(returned.operator, 'Not')
  assert.deepEqual(returned.type, { _tag: 'Available', type: 'bool' })
  assert.deepEqual(
    returned.mappings.map((mapping) => [mapping.ordinal, mapping.expected]),
    [[0, 'bool']],
  )
  const grouped = returned.arguments.at(0)?.expression
  assert.strictEqual(grouped?._tag, 'Grouped')
  if (grouped?._tag !== 'Grouped') return
  assert.strictEqual(grouped.expression._tag, 'Operator')
  if (grouped.expression._tag !== 'Operator') return
  assert.strictEqual(grouped.expression.operator, 'Equals')
})

it('applies pipeline callables left-to-right without inserted call arguments', () => {
  const result = analyzeText(
    'fixture://pipeline.silk',
    'pub fn main() -> i32 { return 2 |> Intrinsic.i32Add(3) |> Intrinsic.i32Multiply(4) }',
  )
  const returned = functionAt(result, 0).returnedExpression

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(returned._tag, 'CallableApply')
  if (returned._tag !== 'CallableApply') return
  assert.strictEqual(returned.arguments.length, 1)
  const left = returned.arguments.at(0)?.expression
  assert.strictEqual(left?._tag, 'CallableApply')
  if (left === undefined) return
  assert.strictEqual(returned.callee._tag, 'CallableSection')
  assert.deepEqual(returned.provenance, {
    _tag: 'PipelineCallableApplication',
    left,
    callable: returned.callee,
    evaluation: 'LeftThenCallable',
  })
  assert.deepEqual(returned.type, { _tag: 'Available', type: 'i32' })
})

it('resolves named function values, stored sections, and callable bindings', () => {
  const result = analyzeText(
    'fixture://stored-callables.silk',
    `fn identity(value: i32) -> i32 { return value }
fn main() -> i32 {
  let named = identity
  let plusTwo = Intrinsic.i32Add(2)
  return named(plusTwo(40))
}`,
  )
  const main = functionAt(result, 1)
  const named = main.statements.at(0)
  const plusTwo = main.statements.at(1)
  const returned = main.returnedExpression

  assert.strictEqual(named?._tag, 'BindStatement')
  assert.strictEqual(
    named?._tag === 'BindStatement' ? named.binding.initializer._tag : undefined,
    'FunctionItem',
  )
  assert.strictEqual(plusTwo?._tag, 'BindStatement')
  assert.strictEqual(
    plusTwo?._tag === 'BindStatement' ? plusTwo.binding.initializer._tag : undefined,
    'CallableSection',
  )
  assert.strictEqual(returned._tag, 'CallableApply')
  if (returned._tag !== 'CallableApply') return
  assert.strictEqual(returned.callee._tag, 'Identifier')
  assert.strictEqual(returned.arguments.at(0)?.expression._tag, 'CallableApply')
  assert.deepEqual(returned.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(result.diagnostics, [])
})

it('infers shared, exclusive, and consuming section modes from captures', () => {
  const result = analyzeText(
    'fixture://callable-capture-modes.silk',
    `struct Token { value: i32 }
fn read(value: i32, values: &[i32]) -> i32 { return value }
fn write(value: i32, values: &mut [i32]) -> i32 { return value }
fn consume(value: i32, token: Token) -> i32 { return value }
fn main() -> i32 {
  let values = [1, 2]
  let mut output = [3, 4]
  let token = Token { value: 5 }
  let shared = read(&values)
  let exclusive = write(&mut output)
  let consuming = consume(move token)
  return 0
}`,
  )
  const main = functionAt(result, 3)
  const sections = main.statements.flatMap((statement) =>
    statement._tag === 'BindStatement' && statement.binding.initializer._tag === 'CallableSection'
      ? [statement.binding.initializer]
      : [],
  )

  assert.deepEqual(
    sections.map((section) => ({ mode: section.mode, access: section.captures.at(0)?.access })),
    [
      { mode: 'Shared', access: 'Shared' },
      { mode: 'Exclusive', access: 'Exclusive' },
      { mode: 'Take', access: 'Take' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('defers generic evidence owned by the omitted leading argument until application', () => {
  const result = analyzeText(
    'fixture://generic-callable-section.silk',
    `fn select<T>(value: T, enabled: bool) -> T { return value }
fn main() -> i32 {
  let whenEnabled = select(true)
  return whenEnabled(42)
}`,
  )
  const main = functionAt(result, 1)
  const binding = main.statements.at(0)
  const section =
    binding?._tag === 'BindStatement' && binding.binding.initializer._tag === 'CallableSection'
      ? binding.binding.initializer
      : undefined

  assert.notStrictEqual(section, undefined)
  assert.strictEqual(section?.site._tag, 'CallableSiteId')
  assert.deepEqual(section?.typeArguments, [])
  assert.strictEqual(
    section?.type._tag === 'Available' ? Type.encode(section.type.type) : undefined,
    'fn(T) -> T',
  )
  assert.deepEqual(main.returnedExpression.type, { _tag: 'Available', type: 'i32' })
  assert.deepEqual(result.diagnostics, [])
})

it('rejects generic evidence that exists only in a section result', () => {
  const result = analyzeText(
    'fixture://return-only-section-generic.silk',
    `fn make<T>(value: i32, enabled: bool) -> T { return value }
fn main() -> i32 { let maker = make(true) return 0 }`,
  )

  assert.include(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0052',
  )
})

it('accepts a partial explicit generic argument list on a callable section', () => {
  const result = analyzeText(
    'fixture://partial-explicit-section-generics.silk',
    `fn choose<T, U>(value: T, left: U, right: U) -> T { return value }
fn main() -> i32 { let callback = choose<i32>(1, 2) return 0 }`,
  )

  assert.deepEqual(result.diagnostics, [])
})

it('rejects more explicit generic arguments than a callable section declares', () => {
  const result = analyzeText(
    'fixture://excess-explicit-section-generics.silk',
    `fn choose<T, U>(value: T, left: U, right: U) -> T { return value }
fn main() -> i32 { let callback = choose<i32, i32, bool>(1, 2) return 0 }`,
  )

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0051'],
  )
})

it('retains a section identity when one captured dependency is unavailable', () => {
  const result = analyzeText(
    'fixture://unavailable-callable-capture.silk',
    `fn choose(value: i32, fallback: i32) -> i32 { return value }
fn main() -> i32 { let callback = choose(missing) return 0 }`,
  )
  const binding = functionAt(result, 1).statements.at(0)
  const section = binding?._tag === 'BindStatement' ? binding.binding.initializer : undefined

  assert.strictEqual(section?._tag, 'CallableSection')
  assert.strictEqual(section?.type._tag, 'Unavailable')
  assert.include(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0006',
  )
})

it.effect('runs the complete composed operand and preserves grouped one-layer execution', () =>
  Effect.gen(function* () {
    const ungrouped = yield* analyzeWithStdlib(
      'fixture://ungrouped-run-callable.silk',
      `effect fn work() -> i32 { return 1 }
fn main() -> i32 { return run work() |> Effect.retry(2) }`,
    )
    const grouped = analyzeText(
      'fixture://grouped-run-callable.silk',
      `effect fn work() -> i32 { return 1 }
fn main() -> i32 { return (run work()) |> Intrinsic.i32Add(1) }`,
    )
    const nested = analyzeText(
      'fixture://nested-run-callable.silk',
      `effect fn inner() -> i32 { return 1 }
effect fn outer() -> Effect<i32> { return inner() }
fn main() -> i32 { return run run outer() }`,
    )
    const ungroupedRun = functionAt(ungrouped, 1).returnedExpression
    const groupedApply = functionAt(grouped, 1).returnedExpression
    const outerRun = functionAt(nested, 2).returnedExpression

    assert.strictEqual(ungroupedRun._tag, 'Run')
    assert.strictEqual(
      ungroupedRun._tag === 'Run' ? ungroupedRun.subject._tag : undefined,
      'CallableApply',
    )
    assert.strictEqual(groupedApply._tag, 'CallableApply')
    assert.strictEqual(outerRun._tag, 'Run')
    assert.strictEqual(outerRun._tag === 'Run' ? outerRun.subject._tag : undefined, 'Run')
    assert.deepEqual(ungrouped.diagnostics, [])
    assert.deepEqual(grouped.diagnostics, [])
    assert.deepEqual(nested.diagnostics, [])
  }),
)

it('diagnoses each invalid callable application at the callable boundary', () => {
  const nonCallable = analyzeText(
    'fixture://non-callable-application.silk',
    'fn main() -> i32 { let value = 1 return value(2) }',
  )
  const incompatible = analyzeText(
    'fixture://incompatible-callable.silk',
    `struct Token {}
fn consume(value: i32, token: Token) -> i32 { return value }
fn accept(callback: fn(i32) -> i32) -> i32 { return 0 }
fn main() -> i32 { let token = Token {} let callback = consume(move token) return accept(callback) }`,
  )
  const exclusive = analyzeText(
    'fixture://exclusive-callable-access.silk',
    `fn write(value: i32, values: &mut [i32]) -> i32 { return value }
fn main() -> i32 { let mut values = [1] let callback = write(&mut values) return callback(2) }`,
  )
  const redundant = analyzeText(
    'fixture://redundant-unary-call.silk',
    'fn identity(value: i32) -> i32 { return value } fn main() -> i32 { return identity() }',
  )
  const deeper = analyzeText(
    'fixture://deeper-under-application.silk',
    'fn combine(a: i32, b: i32, c: i32) -> i32 { return a } fn main() -> i32 { return combine(1) }',
  )

  assert.include(
    nonCallable.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0075',
  )
  assert.include(
    incompatible.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0076',
  )
  assert.include(
    exclusive.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0077',
  )
  assert.include(
    redundant.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0078',
  )
  assert.include(
    deeper.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0079',
  )
})

it('withholds mistyped operator results after one local diagnostic', () => {
  const result = analyzeText(
    'fixture://operator-mismatch.silk',
    'pub fn main() -> i32 { return true + 1 }',
  )
  const returned = functionAt(result, 0).returnedExpression

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0012'],
  )
  assert.strictEqual(returned._tag, 'Operator')
  assert.deepEqual(returned.type, { _tag: 'Unavailable' })
})

it('specializes raw storage operations and requires lexical unsafe authority', () => {
  const accepted = analyzeText(
    'fixture://raw-storage-accepted.silk',
    `fn build(allocation: Allocation, count: usize) -> RawBuffer<i32> {
  unsafe { return Intrinsic.rawBufferFrom<i32>(move allocation, count) }
}
fn destroy(buffer: RawBuffer<i32>) -> () {
  let mut owner = move buffer
  unsafe { return Intrinsic.slotDrop(Intrinsic.rawBufferSlot(&mut owner, 0)) }
}`,
  )
  const rejected = analyzeText(
    'fixture://raw-storage-safe.silk',
    `fn build(allocation: Allocation, count: usize) -> RawBuffer<i32> {
  return Intrinsic.rawBufferFrom<i32>(move allocation, count)
}`,
  )

  assert.deepEqual(accepted.diagnostics, [])
  assert.include(
    rejected.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0082',
  )
  assert.strictEqual(functionAt(rejected, 0).returnedExpression.type._tag, 'Unavailable')
})

it('allows a shared value reborrow from a shared pattern field', () => {
  const result = analyzeText(
    'fixture://shared-pattern-field.silk',
    `struct Box { buffer: RawBuffer<i32> }
fn read(buffer: &RawBuffer<i32>) -> i32 {
  unsafe { return Intrinsic.rawBufferRead<i32>(buffer, 0) }
}
fn inspect(input: Box) -> i32 {
  return match &input { Box { buffer } => read(&buffer) }
}`,
  )

  assert.deepEqual(result.diagnostics, [])
  const inspect = functionAt(result, 1).returnedExpression
  assert.strictEqual(inspect._tag, 'Match')
})

it('accepts only unit and never expression statements without duplicating unavailable errors', () => {
  const unit = analyzeText(
    'fixture://unit-expression-statement.silk',
    `effect fn pulse() -> () { return () }
effect fn main() -> () { run pulse() return () }`,
  )
  const never = analyzeText(
    'fixture://never-expression-statement.silk',
    `struct Problem {}
effect fn stop() -> never ! Problem { fail move Problem {} }
effect fn main() -> () ! Problem { run stop() return () }`,
  )
  const scalar = analyzeText(
    'fixture://scalar-expression-statement.silk',
    `fn value() -> i32 { return 1 }
fn main() -> () { value() return () }`,
  )
  const owned = analyzeText(
    'fixture://owned-expression-statement.silk',
    `struct Token {}
fn make() -> Token { return Token {} }
fn main() -> () { make() return () }`,
  )
  const unavailable = analyzeText(
    'fixture://unavailable-expression-statement.silk',
    'fn main() -> () { missing() return () }',
  )

  assert.deepEqual(unit.diagnostics, [])
  assert.deepEqual(never.diagnostics, [])
  assert.strictEqual(functionAt(unit, 1).statements.at(0)?._tag, 'ExpressionStatement')
  assert.strictEqual(functionAt(never, 1).statements.at(0)?._tag, 'ExpressionStatement')
  assert.deepEqual(
    scalar.diagnostics.map((diagnostic) => ({ code: diagnostic.code, reason: diagnostic.reason })),
    [
      {
        code: 'SEM0087',
        reason: { _tag: 'ExpressionStatementResult', actual: 'i32' },
      },
    ],
  )
  assert.deepEqual(
    owned.diagnostics.map((diagnostic) => ({ code: diagnostic.code, reason: diagnostic.reason })),
    [
      {
        code: 'SEM0087',
        reason: { _tag: 'ExpressionStatementResult', actual: 'Token' },
      },
    ],
  )
  assert.deepEqual(
    unavailable.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0004'],
  )
})

it('elaborates source service operations as declaration-shaped effect requirements', () => {
  const result = analyzeText(
    'service://call',
    `service Logger {
  effect fn value() -> i32 ? &Logger
}
effect fn main() -> i32 ? &Logger { return run Logger.value() }`,
  )

  assert.deepEqual(result.diagnostics, [])
  const returned = functionAt(result, 0).returnedExpression
  assert.strictEqual(returned._tag, 'Run')
  if (returned._tag !== 'Run') return
  assert.strictEqual(returned.subject._tag, 'Call')
  if (returned.subject._tag !== 'Call') return
  assert.strictEqual(returned.subject.reference._tag, 'ResolvedServiceOperation')
  assert.include(
    Hir.encode(result.hir),
    'service-call service://call.Logger.value@DefaultRole:shared',
  )
})
