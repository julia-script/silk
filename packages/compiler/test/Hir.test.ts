import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'
import { elaborate as elaborateSyntax } from './support/elaborate.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const acceptedSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`
const damagedSource = `pub fn puzzle(value: Mystery) -> i32 { return value }
pub fn main() -> i32 { return missing(2147483648) }`

const elaborate = (id: string, text: string): Elaboration.Result =>
  elaborateSyntax(Parser.parse(Lexer.lex(SourceFile.make(id, ascii(text)))))

const elaborateWithStdlib = Effect.fnUntraced(function* (id: string, text: string) {
  const module = id.replace('://', '/').replace(/\.silk$/, '')
  return Analysis.rootAnalysis(yield* Analysis.ofSource(module, ascii(text)))
})

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it('owns complete callable target conversion and intrinsic-aware equality', () => {
  const declaration: Type.CallableIdentityArgument['target'] = Object.freeze({
    _tag: 'Declaration',
    module: 'targets',
    name: 'decode',
  })
  const identity: Type.CallableIdentityArgument['target'] = Object.freeze({
    _tag: 'Builtin',
    actor: 'Intrinsic',
    operation: 'Add',
    intrinsic: Object.freeze({ actor: 'i32', name: 'add' }),
  })
  const target = Hir.callableTargetFromIdentity(identity)

  assert.deepEqual(
    Hir.callableTargetIdentity(Hir.callableTargetFromIdentity(declaration)),
    declaration,
  )
  assert.deepEqual(Hir.callableTargetIdentity(target), identity)
  assert.strictEqual(Hir.matchesCallableTargetIdentity(target, identity), true)
  assert.strictEqual(
    Hir.matchesCallableTargetIdentity(target, {
      ...identity,
      intrinsic: Object.freeze({ actor: 'u32', name: 'add' }),
    }),
    false,
  )
})

it('constructs typed HIR with canonical call targets and normalized contracts', () => {
  const result = elaborate('golden://accepted.silk', acceptedSource)
  const main = result.hir.functions.at(1)

  assert.deepEqual(main?.contract, {
    _tag: 'Contract',
    unsafe: false,
    parameters: [],
    result: 'i32',
    constraints: [],
  })
  const body = main === undefined ? undefined : Hir.returned(main)
  assert.strictEqual(body?._tag, 'Call')
  if (body?._tag !== 'Call') return
  assert.deepEqual(body.target, {
    _tag: 'CanonicalDeclarationId',
    module: 'golden://accepted.silk',
    name: 'identity',
  })
  assert.strictEqual(body.type, 'i32')
  const inner = body.arguments.at(0)
  assert.strictEqual(inner?._tag, 'Call')
  if (inner?._tag !== 'Call') return
  assert.strictEqual(inner.arguments.at(0)?._tag, 'IntegerLiteral')
})

it('retains canonical scalar enum member, value, and equality identities in typed HIR', () => {
  const result = elaborate(
    'hir://enum-values.silk',
    `enum(i8) Status { Unknown = -1, Ready = 1 }
fn raw(value: Status) -> i8 { return Status.value(value) }
fn same(left: Status, right: Status) -> bool { return left == right }
fn ready() -> Status { return Status.Ready }`,
  )
  assert.deepEqual(result.diagnostics, [])

  const raw = result.hir.functions.at(0)
  const conversion = raw === undefined ? undefined : Hir.returned(raw)
  assert.strictEqual(conversion?._tag, 'EnumValue')
  if (conversion?._tag === 'EnumValue') {
    assert.deepEqual(conversion.enum, {
      _tag: 'CanonicalDeclarationId',
      module: 'hir://enum-values.silk',
      name: 'Status',
    })
    assert.strictEqual(conversion.intrinsic.name, 'enumValue')
    assert.strictEqual(conversion.type, 'i8')
  }

  const same = result.hir.functions.at(1)
  const equality = same === undefined ? undefined : Hir.returned(same)
  assert.strictEqual(equality?._tag, 'EnumEquality')
  if (equality?._tag === 'EnumEquality') assert.strictEqual(equality.type, 'bool')

  const ready = result.hir.functions.at(2)
  const member = ready === undefined ? undefined : Hir.returned(ready)
  assert.strictEqual(member?._tag, 'EnumMember')
  if (member?._tag === 'EnumMember') {
    assert.strictEqual(member.member.name, 'Ready')
    assert.strictEqual(member.discriminant, 1n)
    assert.strictEqual(Type.encode(member.type), 'hir://enum-values.silk.Status')
  }
  const encoded = Hir.encode(result.hir)
  assert.include(encoded, 'enum-value hir://enum-values.silk.Status via Intrinsic.enumValue : i8')
  assert.include(encoded, 'enum-equals hir://enum-values.silk.Status : bool')
  assert.include(encoded, 'enum-member hir://enum-values.silk.Status.Ready discriminant=1')
})

it('retains scalar enum pattern identities and nominal scrutinee type in typed HIR', () => {
  const result = elaborate(
    'hir://enum-match.silk',
    `enum Status { Unknown, Ready }
fn inspect(value: Status) -> i32 {
  return match value { Status.Unknown => 0 Status.Ready => 1 }
}`,
  )
  const inspect = result.hir.functions.at(0)
  const match = inspect === undefined ? undefined : Hir.returned(inspect)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(match?._tag, 'Match')
  if (match?._tag !== 'Match') return
  assert.notStrictEqual(match.scrutinee._tag, 'Unavailable')
  if (match.scrutinee._tag === 'Unavailable') return
  assert.strictEqual(Type.encode(match.scrutinee.type), 'hir://enum-match.silk.Status')
  assert.deepEqual(
    match.members.map((member) => ({
      tag: member._tag,
      name: member._tag === 'EnumMember' ? member.member.name : undefined,
      type: Type.encode(member.type),
    })),
    [
      { tag: 'EnumMember', name: 'Unknown', type: 'hir://enum-match.silk.Status' },
      { tag: 'EnumMember', name: 'Ready', type: 'hir://enum-match.silk.Status' },
    ],
  )
  assert.deepEqual(
    match.arms.map((arm) => ({
      member: arm.member?._tag === 'EnumMember' ? arm.member.member.name : undefined,
      before: arm.before.map((member) =>
        member._tag === 'EnumMember' ? member.member.name : Type.encode(member.type),
      ),
      after: arm.after.map((member) =>
        member._tag === 'EnumMember' ? member.member.name : Type.encode(member.type),
      ),
    })),
    [
      { member: 'Unknown', before: ['Unknown', 'Ready'], after: ['Ready'] },
      { member: 'Ready', before: ['Ready'], after: [] },
    ],
  )
  assert.deepEqual(Hir.verify(result.hir), [])
})

it('preserves unsafe declaration and section contracts in typed HIR', () => {
  const result = elaborate(
    'hir://unsafe-callable.silk',
    `unsafe fn combine(left: i32, right: i32) -> i32 { return left + right }
fn staged() -> unsafe fn(i32) -> i32 { return combine(2) }
pub fn main() -> i32 { let callback = staged() return unsafe callback(40) }`,
  )
  const combine = result.hir.functions.at(0)
  const staged = result.hir.functions.at(1)
  const section = staged === undefined ? undefined : Hir.returned(staged)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(combine?.contract._tag, 'Contract')
  assert.strictEqual(combine?.contract._tag === 'Contract' ? combine.contract.unsafe : false, true)
  assert.strictEqual(section?._tag, 'CallableSection')
  assert.strictEqual(section?._tag === 'CallableSection' ? section.type.unsafe : false, true)
})

it('keeps unknown facts explicit with causes instead of typed operations', () => {
  const result = elaborate('golden://damaged.silk', damagedSource)
  const puzzle = result.hir.functions.at(0)
  const main = result.hir.functions.at(1)

  assert.strictEqual(puzzle?.contract._tag, 'Unavailable')
  if (puzzle?.contract._tag !== 'Unavailable') return
  assert.strictEqual(puzzle.contract.cause?.code, 'SEM0001')
  assert.strictEqual(Hir.returned(puzzle)._tag, 'Unavailable')
  const mainBody = main === undefined ? undefined : Hir.returned(main)
  assert.strictEqual(mainBody?._tag, 'Unavailable')
  if (mainBody?._tag !== 'Unavailable') return
  assert.strictEqual(mainBody.cause?.code, 'SEM0004')
})

it('elaborates binding statements into typed locals with moves', () => {
  const result = elaborate(
    'golden://bindings.silk',
    `pub fn main() -> i32 { let value = 42 let copy = value return move copy }`,
  )
  const main = result.hir.functions.at(0)

  assert.strictEqual(main?.statements.length, 3)
  const first = main?.statements.at(0)
  assert.strictEqual(first?._tag, 'Bind')
  if (first?._tag !== 'Bind') return
  assert.strictEqual(first.name, 'value')
  assert.strictEqual(first.initializer._tag, 'IntegerLiteral')
  const second = main?.statements.at(1)
  assert.strictEqual(second?._tag, 'Bind')
  if (second?._tag !== 'Bind') return
  assert.strictEqual(second.initializer._tag, 'BindingReference')
  const returned = main === undefined ? undefined : Hir.returned(main)
  assert.strictEqual(returned?._tag, 'Move')
  if (returned?._tag !== 'Move') return
  assert.strictEqual(returned.subject._tag, 'BindingReference')
  if (returned.subject._tag !== 'BindingReference') return
  assert.strictEqual(returned.subject.binding.ordinal, 1)
  assert.strictEqual(result.diagnostics.length, 0)
})

it('keeps expression statements as Evaluate HIR with unavailable causes intact', () => {
  const accepted = elaborate(
    'golden://evaluate.silk',
    `effect fn pulse() -> () { return () }
effect fn main() -> () { run pulse() return () }`,
  )
  const damaged = elaborate(
    'golden://evaluate-damaged.silk',
    'fn main() -> () { missing() return () }',
  )
  const effectBlock = accepted.hir.functions
    .flatMap((fn) => fn.statements)
    .flatMap(Hir.statementExpressions)
    .flatMap(Hir.expressionTree)
    .find(
      (expression) =>
        expression._tag === 'EffectBlock' &&
        expression.statements.some((statement) => statement._tag === 'Evaluate'),
    )
  const evaluated =
    effectBlock?._tag === 'EffectBlock'
      ? effectBlock.statements.find((statement) => statement._tag === 'Evaluate')
      : undefined
  const unavailable = damaged.hir.functions
    .flatMap((fn) => fn.statements)
    .find((statement) => statement._tag === 'Evaluate')

  assert.strictEqual(evaluated?._tag, 'Evaluate')
  if (evaluated?._tag !== 'Evaluate') return
  assert.strictEqual(evaluated.expression._tag, 'Run')
  assert.include(Hir.encode(accepted.hir), 'evaluate r0')

  assert.strictEqual(unavailable?._tag, 'Evaluate')
  if (unavailable?._tag !== 'Evaluate') return
  assert.strictEqual(unavailable.expression._tag, 'Unavailable')
  if (unavailable.expression._tag !== 'Unavailable') return
  assert.strictEqual(unavailable.expression.cause?.code, 'SEM0004')
})

it('rejects rebinding a name while references keep resolving to the original', () => {
  const result = elaborate(
    'golden://rebind.silk',
    `pub fn main() -> i32 { let value = 1 let value = 2 return value }`,
  )

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0008'],
  )
  const main = result.hir.functions.at(0)
  const returned = main === undefined ? undefined : Hir.returned(main)
  assert.strictEqual(returned?._tag, 'BindingReference')
  if (returned?._tag !== 'BindingReference') return
  assert.strictEqual(returned.binding.ordinal, 0)
})

it('resolves a nested lexical shadow to the nearest local binding', () => {
  const result = elaborate(
    'golden://shadow.silk',
    `pub fn main() -> i32 {
      let value = 1
      if true {
        let value = 2
        return value
      }
      return value
    }`,
  )

  assert.deepEqual(result.diagnostics, [])
  const main = result.hir.functions.at(0)
  const conditional = main?.statements.at(1)
  assert.strictEqual(conditional?._tag, 'If')
  if (conditional?._tag !== 'If') return
  const returned = conditional.taken.at(-1)
  assert.strictEqual(returned?._tag, 'Return')
  if (returned?._tag !== 'Return') return
  assert.strictEqual(returned.expression._tag, 'BindingReference')
  if (returned.expression._tag !== 'BindingReference') return
  assert.strictEqual(returned.expression.binding.ordinal, 1)
})

it('reports an unknown name and a use before its binding as missing references', () => {
  const result = elaborate(
    'golden://forward.silk',
    `pub fn main() -> i32 { let early = late let late = 2 return early }`,
  )

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0006'],
  )
  const main = result.hir.functions.at(0)
  const first = main?.statements.at(0)
  assert.strictEqual(first?._tag, 'Bind')
  if (first?._tag !== 'Bind') return
  assert.strictEqual(first.initializer._tag, 'Unavailable')
})

it('matches the accepted HIR golden encoding byte-for-byte', () => {
  const result = elaborate('golden://accepted.silk', acceptedSource)

  assert.strictEqual(Hir.encode(result.hir), golden('accepted.hir.txt'))
})

it('matches the damaged HIR golden encoding and names unavailable states', () => {
  const result = elaborate('golden://damaged.silk', damagedSource)
  const encoded = Hir.encode(result.hir)

  assert.strictEqual(encoded, golden('damaged.hir.txt'))
  assert.include(encoded, 'contract-unavailable')
  assert.include(encoded, 'unavailable [')
})

it('elaborates and encodes byte-identically across repeated fresh runs', () => {
  const first = elaborate('golden://repeat.silk', damagedSource)
  const second = elaborate('golden://repeat.silk', damagedSource)

  assert.deepEqual(first, second)
  assert.strictEqual(Hir.encode(first.hir), Hir.encode(second.hir))
})

it('elaborates built-in arithmetic calls with signed literals', () => {
  const result = elaborate(
    'golden://arith.silk',
    'pub fn main() -> i32 { return Intrinsic.i32Add(-8, 50) }',
  )
  const main = result.hir.functions.at(0)
  const returned = main === undefined ? undefined : Hir.returned(main)

  assert.strictEqual(result.diagnostics.length, 0)
  assert.strictEqual(returned?._tag, 'BuiltinCall')
  if (returned?._tag !== 'BuiltinCall') return
  assert.strictEqual(returned.operation, 'Add')
  const first = returned.arguments.at(0)
  assert.strictEqual(first?._tag, 'IntegerLiteral')
  if (first?._tag !== 'IntegerLiteral') return
  assert.strictEqual(first.value, -8n)
  assert.include(Hir.encode(result.hir), 'builtin i32.Add : i32')
})

it('accepts the signed minimum and rejects one below it', () => {
  const minimum = elaborate('golden://min.silk', 'pub fn main() -> i32 { return -2147483648 }')
  const below = elaborate('golden://below.silk', 'pub fn main() -> i32 { return -2147483649 }')

  assert.deepEqual(minimum.diagnostics, [])
  const fn = minimum.hir.functions.at(0)
  const returned = fn === undefined ? undefined : Hir.returned(fn)
  assert.strictEqual(returned?._tag, 'IntegerLiteral')
  if (returned?._tag !== 'IntegerLiteral') return
  assert.strictEqual(returned.value, -2147483648n)
  assert.deepEqual(
    below.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0002'],
  )
})

it('diagnoses unknown actors and unknown operations distinctly', () => {
  const actor = elaborate('golden://actor.silk', 'pub fn main() -> i32 { return Math.add(1, 2) }')
  const operation = elaborate(
    'golden://operation.silk',
    'pub fn main() -> i32 { return Intrinsic.i32Frobnicate(1, 2) }',
  )
  const arity = elaborate(
    'golden://arity.silk',
    'pub fn main() -> i32 { return Intrinsic.i32Add() }',
  )

  assert.deepEqual(
    actor.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0009'],
  )
  assert.deepEqual(
    operation.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0010'],
  )
  assert.deepEqual(
    arity.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0007'],
  )
  for (const result of [actor, operation, arity]) {
    const fn = result.hir.functions.at(0)
    const returned = fn === undefined ? undefined : Hir.returned(fn)
    assert.strictEqual(returned?._tag, 'Unavailable')
  }
})

it('keeps bare built-in operation names unresolved', () => {
  const result = elaborate('golden://bare.silk', 'pub fn main() -> i32 { return add(1, 2) }')

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0004'],
  )
})

it('elaborates conditionals with typed bool conditions and arm scopes', () => {
  const result = elaborate(
    'golden://branch.silk',
    'pub fn main() -> i32 { let base = 40 if base == 40 { let bonus = 2 return base + bonus } return 0 }',
  )
  const main = result.hir.functions.at(0)

  assert.deepEqual(result.diagnostics, [])
  const conditional = main?.statements.at(1)
  assert.strictEqual(conditional?._tag, 'If')
  if (conditional?._tag !== 'If') return
  assert.strictEqual(conditional.condition._tag, 'BuiltinCall')
  assert.strictEqual(
    conditional.condition._tag === 'BuiltinCall' ? conditional.condition.type : undefined,
    'bool',
  )
  const armBind = conditional.taken.at(0)
  assert.strictEqual(armBind?._tag, 'Bind')
  if (armBind?._tag !== 'Bind') return
  assert.strictEqual(armBind.binding.ordinal, 1)
  assert.strictEqual(conditional.taken.at(1)?._tag, 'Return')
  const encoded = Hir.encode(result.hir)
  assert.include(encoded, 'if r')
  assert.include(encoded, 'then')
})

it('types booleans through declarations and literals', () => {
  const result = elaborate(
    'golden://bool.silk',
    `pub fn check(flag: bool) -> bool { return flag }
pub fn main() -> i32 { if check(true) { return 1 } return 0 }`,
  )

  assert.deepEqual(result.diagnostics, [])
  const check = result.hir.functions.at(0)
  assert.deepEqual(check?.contract, {
    _tag: 'Contract',
    unsafe: false,
    parameters: ['bool'],
    result: 'bool',
    constraints: [],
  })
  const returned = check === undefined ? undefined : Hir.returned(check)
  assert.strictEqual(returned?._tag, 'ParameterReference')
  if (returned?._tag !== 'ParameterReference') return
  assert.strictEqual(returned.type, 'bool')
})

it('rejects non-bool conditions and mistyped arguments', () => {
  const condition = elaborate(
    'golden://condition.silk',
    'pub fn main() -> i32 { if 1 { return 1 } return 0 }',
  )
  const builtinArg = elaborate(
    'golden://builtin-arg.silk',
    'pub fn main() -> i32 { return Intrinsic.i32Add(true, 1) }',
  )
  const userArg = elaborate(
    'golden://user-arg.silk',
    `pub fn pick(flag: bool) -> i32 { return 1 }
pub fn main() -> i32 { return pick(42) }`,
  )

  assert.deepEqual(
    condition.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0011'],
  )
  assert.deepEqual(
    builtinArg.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0012'],
  )
  assert.deepEqual(
    userArg.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0012'],
  )
  const mainFn = builtinArg.hir.functions.at(0)
  const returned = mainFn === undefined ? undefined : Hir.returned(mainFn)
  assert.strictEqual(returned?._tag, 'Unavailable')
})

it('erases grouping and operators into canonical builtin HIR calls', () => {
  const result = elaborate(
    'golden://operators.silk',
    'pub fn main() -> i32 { return -(2 + 3 * 4) }',
  )
  const fn = result.hir.functions.at(0)
  const returned = fn === undefined ? undefined : Hir.returned(fn)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(returned?._tag, 'BuiltinCall')
  if (returned?._tag !== 'BuiltinCall') return
  assert.strictEqual(returned.operation, 'Negate')
  const addition = returned.arguments.at(0)
  assert.strictEqual(addition?._tag, 'BuiltinCall')
  if (addition?._tag !== 'BuiltinCall') return
  assert.strictEqual(addition.operation, 'Add')
  const multiplication = addition.arguments.at(1)
  assert.strictEqual(multiplication?._tag, 'BuiltinCall')
  if (multiplication?._tag !== 'BuiltinCall') return
  assert.strictEqual(multiplication.operation, 'Multiply')
})

it('lowers builtin pipelines into left-first callable application with an erasable section', () => {
  const result = elaborate(
    'golden://pipeline.silk',
    'pub fn main() -> i32 { return 2 |> Intrinsic.i32Add(3) }',
  )
  const fn = result.hir.functions.at(0)
  const returned = fn === undefined ? undefined : Hir.returned(fn)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(returned?._tag, 'CallableApply')
  if (returned?._tag !== 'CallableApply') return
  assert.strictEqual(returned.evaluation, 'LeftThenCallable')
  assert.strictEqual(returned.realization, 'DirectErasedSection')
  assert.strictEqual(returned.arguments.at(0)?._tag, 'IntegerLiteral')
  assert.strictEqual(returned.callee._tag, 'CallableSection')
  if (returned.callee._tag !== 'CallableSection') return
  assert.strictEqual(returned.callee.target._tag, 'BuiltinCallableTarget')
  assert.strictEqual(returned.callee.captures.at(0)?.value._tag, 'IntegerLiteral')
})

it('preserves stored and cross-call owned callable environments', () => {
  const result = elaborate(
    'hir://owned-callable-return.silk',
    `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value }
fn make(token: Token) -> once fn(i32) -> i32 { return consume(move token) }
pub fn main() -> i32 {
  let token = Token { value: 42 }
  let callback = make(move token)
  return callback(1)
}`,
  )
  const make = result.hir.functions.at(1)
  const main = result.hir.functions.at(2)
  const returnedEnvironment = make === undefined ? undefined : Hir.returned(make)
  const applied = main === undefined ? undefined : Hir.returned(main)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(returnedEnvironment?._tag, 'CallableSection')
  assert.strictEqual(
    returnedEnvironment?._tag === 'CallableSection' ? returnedEnvironment.mode : undefined,
    'Take',
  )
  assert.strictEqual(applied?._tag, 'CallableApply')
  assert.strictEqual(
    applied?._tag === 'CallableApply' ? applied.realization : undefined,
    'Environment',
  )
})

it.effect('desugars effect functions and source-defined catch calls to hidden effect values', () =>
  Effect.gen(function* () {
    const result = yield* elaborateWithStdlib(
      'hir://effect.silk',
      `import silk.effect as Effect
struct Problem { code: i32 }
effect fn risky() -> i32 ! Problem { fail move Problem { code: 41 } }
effect fn recover(problem: Problem) -> i32 { return problem.code + 1 }
pub fn main() -> i32 {
  let recipe = Effect.catchAll(risky(), recover)
  return run recipe
}`,
    )
    const risky = result.hir.functions.at(0)
    const main = result.hir.functions.at(2)

    assert.deepEqual(result.diagnostics, [])
    assert.strictEqual(risky?.contract._tag, 'Contract')
    if (risky?.contract._tag === 'Contract') {
      assert.isUndefined(risky.contract.functionKind)
      assert.strictEqual(Type.encode(risky.contract.result), 'Effect<i32 ! hir/effect.Problem>')
    }
    assert.strictEqual(risky?.statements.at(0)?._tag, 'Return')
    const riskyBody = risky?.statements.at(0)
    assert.strictEqual(
      riskyBody?._tag === 'Return' ? riskyBody.expression._tag : undefined,
      'EffectBlock',
    )
    if (riskyBody?._tag === 'Return' && riskyBody.expression._tag === 'EffectBlock')
      assert.strictEqual(riskyBody.expression.statements.at(0)?._tag, 'Fail')
    assert.strictEqual(main?.statements.at(0)?._tag, 'Bind')
    const binding = main?.statements.at(0)
    assert.strictEqual(
      binding?._tag === 'Bind' ? binding.initializer._tag : undefined,
      'EffectConstruct',
    )
    if (binding?._tag === 'Bind' && binding.initializer._tag === 'EffectConstruct') {
      assert.strictEqual(binding.initializer.target.module, 'silk/effect')
      assert.strictEqual(binding.initializer.target.name, 'catchAll')
      assert.strictEqual(binding.initializer.arguments.at(0)?._tag, 'EffectConstruct')
    }
    assert.strictEqual(main === undefined ? undefined : Hir.returned(main)._tag, 'Run')
    assert.deepEqual(Hir.verify(result.hir), [])
  }),
)

it('retains effect blocks as lazy statement regions with canonical captures', () => {
  const result = elaborate(
    'hir://effect-block.silk',
    `fn main(value: i32) -> i32 {
  let mut counter = value
  let pending = effect { counter = counter + 1 return counter }
  return 0
}`,
  )
  const binding = result.hir.functions.at(0)?.statements.at(1)
  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(binding?._tag, 'Bind')
  if (binding?._tag !== 'Bind' || binding.initializer._tag !== 'EffectBlock') return
  assert.strictEqual(binding.initializer.type.access, 'Exclusive')
  assert.deepEqual(
    binding.initializer.site.owner === undefined
      ? undefined
      : Hir.effectRunnerId(binding.initializer.site.owner, binding.initializer.site),
    {
      _tag: 'CanonicalDeclarationId',
      module: 'hir://effect-block.silk',
      name: 'main$effect$0',
    },
  )
  assert.deepEqual(
    binding.initializer.captures.map((capture) => [capture.binding?.ordinal, capture.access]),
    [[0, 'Exclusive']],
  )
  assert.include(
    Hir.encode(result.hir),
    'effect-block site=effect:declaration:hir://effect-block.silk:main:site:',
  )
  assert.include(Hir.encode(result.hir), 'access=exclusive')
})

it('retains explicit unsafe boundaries as typed HIR regions', () => {
  const result = elaborate(
    'hir://unsafe.silk',
    'struct Token { value: i32 } pub fn main() -> i32 { unsafe { let token = Token { value: 1 } drop token } return 42 }',
  )
  const statement = result.hir.functions.at(0)?.statements.at(0)
  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(statement?._tag, 'Unsafe')
  if (statement?._tag !== 'Unsafe') return
  assert.deepEqual(
    statement.statements.map((nested) => nested._tag),
    ['Bind', 'Drop'],
  )
  assert.include(Hir.encode(result.hir), 'unsafe r')
  assert.deepEqual(Hir.verify(result.hir), [])
})

it('retains generic raw-buffer operations and whole-value borrows', () => {
  const result = elaborate(
    'hir://raw-storage.silk',
    `fn destroy(buffer: RawBuffer<i32>) -> () {
  let mut owner = move buffer
  unsafe { return Intrinsic.slotDrop(Intrinsic.rawBufferSlot(&mut owner, 0)) }
}`,
  )
  const unsafe = result.hir.functions.at(0)?.statements.at(1)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(unsafe?._tag, 'Unsafe')
  if (unsafe?._tag !== 'Unsafe') return
  const returned = unsafe.statements.at(0)
  assert.strictEqual(returned?._tag, 'Return')
  if (returned?._tag !== 'Return' || returned.expression._tag !== 'BuiltinCall') return
  assert.strictEqual(returned.expression.operation, 'SlotDrop')
  const slot = returned.expression.arguments.at(0)
  assert.strictEqual(slot?._tag, 'BuiltinCall')
  if (slot?._tag !== 'BuiltinCall') return
  assert.strictEqual(slot.operation, 'RawBufferSlot')
  assert.strictEqual(slot.arguments.at(0)?._tag, 'ValueBorrow')
  assert.deepEqual(Hir.verify(result.hir), [])
})

it('retains shared pattern-field reborrows and raw-buffer reads', () => {
  const result = elaborate(
    'hir://shared-pattern-read.silk',
    `struct Box { buffer: RawBuffer<i32> }
fn read(buffer: &RawBuffer<i32>) -> i32 {
  unsafe { return Intrinsic.rawBufferRead<i32>(buffer, 0) }
}
fn inspect(input: Box) -> i32 {
  return match &input { Box { buffer } => read(&buffer) }
}`,
  )

  assert.deepEqual(result.diagnostics, [])
  assert.include(Hir.encode(result.hir), 'borrow-value')
  assert.include(Hir.encode(result.hir), 'a0.b0')
  assert.include(Hir.encode(result.hir), 'RawBufferRead')
  assert.deepEqual(Hir.verify(result.hir), [])
})
