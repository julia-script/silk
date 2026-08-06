import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import type * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import { elaborate as elaborateSyntax } from './support/elaborate.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const acceptedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`
const damagedSource = `pub fn puzzle(value: Mystery) -> I32 { return value }
pub fn main() -> I32 { return missing(2147483648) }`

const elaborate = (id: string, text: string): Elaboration.Result =>
  elaborateSyntax(Parser.parse(Lexer.lex(SourceFile.make(id, ascii(text)))))

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it('constructs typed HIR with canonical call targets and normalized contracts', () => {
  const result = elaborate('golden://accepted.silk', acceptedSource)
  const main = result.hir.functions.at(1)

  assert.deepEqual(main?.contract, { _tag: 'Contract', parameters: [], result: 'I32' })
  const body = main === undefined ? undefined : Hir.returned(main)
  assert.strictEqual(body?._tag, 'Call')
  if (body?._tag !== 'Call') return
  assert.deepEqual(body.target, {
    _tag: 'CanonicalDeclarationId',
    module: 'golden://accepted.silk',
    name: 'identity',
  })
  assert.strictEqual(body.type, 'I32')
  const inner = body.arguments.at(0)
  assert.strictEqual(inner?._tag, 'Call')
  if (inner?._tag !== 'Call') return
  assert.strictEqual(inner.arguments.at(0)?._tag, 'IntegerLiteral')
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
    `pub fn main() -> I32 { let value = 42 let copy = value return move copy }`,
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

it('rejects rebinding a name while references keep resolving to the original', () => {
  const result = elaborate(
    'golden://rebind.silk',
    `pub fn main() -> I32 { let value = 1 let value = 2 return value }`,
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

it('reports an unknown name and a use before its binding as missing references', () => {
  const result = elaborate(
    'golden://forward.silk',
    `pub fn main() -> I32 { let early = late let late = 2 return early }`,
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
  const result = elaborate('golden://arith.silk', 'pub fn main() -> I32 { return I32.add(-8, 50) }')
  const main = result.hir.functions.at(0)
  const returned = main === undefined ? undefined : Hir.returned(main)

  assert.strictEqual(result.diagnostics.length, 0)
  assert.strictEqual(returned?._tag, 'BuiltinCall')
  if (returned?._tag !== 'BuiltinCall') return
  assert.strictEqual(returned.operation, 'Add')
  const first = returned.arguments.at(0)
  assert.strictEqual(first?._tag, 'IntegerLiteral')
  if (first?._tag !== 'IntegerLiteral') return
  assert.strictEqual(first.value, -8)
  assert.include(Hir.encode(result.hir), 'builtin I32.Add : I32')
})

it('accepts the signed minimum and rejects one below it', () => {
  const minimum = elaborate('golden://min.silk', 'pub fn main() -> I32 { return -2147483648 }')
  const below = elaborate('golden://below.silk', 'pub fn main() -> I32 { return -2147483649 }')

  assert.deepEqual(minimum.diagnostics, [])
  const fn = minimum.hir.functions.at(0)
  const returned = fn === undefined ? undefined : Hir.returned(fn)
  assert.strictEqual(returned?._tag, 'IntegerLiteral')
  if (returned?._tag !== 'IntegerLiteral') return
  assert.strictEqual(returned.value, -2147483648)
  assert.deepEqual(
    below.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0002'],
  )
})

it('diagnoses unknown actors and unknown operations distinctly', () => {
  const actor = elaborate('golden://actor.silk', 'pub fn main() -> I32 { return Math.add(1, 2) }')
  const operation = elaborate(
    'golden://operation.silk',
    'pub fn main() -> I32 { return I32.frobnicate(1, 2) }',
  )
  const arity = elaborate('golden://arity.silk', 'pub fn main() -> I32 { return I32.add(1) }')

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
  const result = elaborate('golden://bare.silk', 'pub fn main() -> I32 { return add(1, 2) }')

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0004'],
  )
})

it('elaborates conditionals with typed Bool conditions and arm scopes', () => {
  const result = elaborate(
    'golden://branch.silk',
    'pub fn main() -> I32 { let base = 40 if I32.equals(base, 40) { let bonus = 2 return I32.add(base, bonus) } return 0 }',
  )
  const main = result.hir.functions.at(0)

  assert.deepEqual(result.diagnostics, [])
  const conditional = main?.statements.at(1)
  assert.strictEqual(conditional?._tag, 'If')
  if (conditional?._tag !== 'If') return
  assert.strictEqual(conditional.condition._tag, 'BuiltinCall')
  assert.strictEqual(
    conditional.condition._tag === 'BuiltinCall' ? conditional.condition.type : undefined,
    'Bool',
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
    `pub fn check(flag: Bool) -> Bool { return flag }
pub fn main() -> I32 { if check(true) { return 1 } return 0 }`,
  )

  assert.deepEqual(result.diagnostics, [])
  const check = result.hir.functions.at(0)
  assert.deepEqual(check?.contract, { _tag: 'Contract', parameters: ['Bool'], result: 'Bool' })
  const returned = check === undefined ? undefined : Hir.returned(check)
  assert.strictEqual(returned?._tag, 'ParameterReference')
  if (returned?._tag !== 'ParameterReference') return
  assert.strictEqual(returned.type, 'Bool')
})

it('rejects non-Bool conditions and mistyped arguments', () => {
  const condition = elaborate(
    'golden://condition.silk',
    'pub fn main() -> I32 { if 1 { return 1 } return 0 }',
  )
  const builtinArg = elaborate(
    'golden://builtin-arg.silk',
    'pub fn main() -> I32 { return I32.add(true, 1) }',
  )
  const userArg = elaborate(
    'golden://user-arg.silk',
    `pub fn pick(flag: Bool) -> I32 { return 1 }
pub fn main() -> I32 { return pick(42) }`,
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
    'pub fn main() -> I32 { return -(2 + 3 * 4) }',
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

it('erases builtin pipelines into ordinary builtin HIR calls', () => {
  const result = elaborate(
    'golden://pipeline.silk',
    'pub fn main() -> I32 { return 2 |> I32.add(3) }',
  )
  const fn = result.hir.functions.at(0)
  const returned = fn === undefined ? undefined : Hir.returned(fn)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(returned?._tag, 'BuiltinCall')
  if (returned?._tag !== 'BuiltinCall') return
  assert.strictEqual(returned.operation, 'Add')
  assert.strictEqual(returned.arguments.length, 2)
})
