import { assert, it } from '@effect/vitest'
import type * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Ownership from '../src/Ownership.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'
import { elaborate, ownership } from './support/elaborate.js'
import { raise } from './support/raise.js'

const analyze = (id: string, source: string): Elaboration.Result =>
  elaborate(Parser.parse(Lexer.lex(SourceFile.make(id, new TextEncoder().encode(source)))))

const returnedMatch = (
  result: Elaboration.Result,
): Extract<Elaboration.ExpressionFact, { readonly _tag: 'Match' }> => {
  const returned = result.functions.at(0)?.returnedExpression
  return returned?._tag === 'Match' ? returned : raise('expected returned match fact')
}

it('publishes guarded source-order coverage, narrowed bindings, and acyclic HIR', () => {
  const result = analyze(
    'main',
    `pub struct Token { kind: i32 }
pub struct End {}
pub fn inspect(event: Token | End) -> i32 {
  return match &event {
    Token { kind } if false => kind
    Token { kind: fallback } => fallback
    End {} => 0
  }
}`,
  )
  const match = returnedMatch(result)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(match.access, 'Shared')
  assert.deepEqual(match.members.map(Type.encode), ['main.End', 'main.Token'])
  assert.deepEqual(
    match.arms.map((arm) => ({
      member:
        arm.pattern._tag === 'NominalPattern' && arm.pattern.member !== undefined
          ? Type.encode(arm.pattern.member)
          : '_',
      before: arm.before.map(Type.encode),
      after: arm.after.map(Type.encode),
      reachable: arm.reachable,
    })),
    [
      {
        member: 'main.Token',
        before: ['main.End', 'main.Token'],
        after: ['main.End', 'main.Token'],
        reachable: true,
      },
      {
        member: 'main.Token',
        before: ['main.End', 'main.Token'],
        after: ['main.End'],
        reachable: true,
      },
      { member: 'main.End', before: ['main.End'], after: [], reachable: true },
    ],
  )
  assert.strictEqual(match.type._tag, 'Available')
  assert.strictEqual(match.arms[0]?.bindings[0]?.name._tag, 'Present')
  assert.strictEqual(match.arms[1]?.result._tag, 'Identifier')
  if (match.arms[1]?.result._tag === 'Identifier') {
    assert.strictEqual(match.arms[1].result.reference._tag, 'ResolvedPattern')
  }
  const hir = result.hir.functions.at(0)?.statements.at(-1)
  assert.strictEqual(hir?._tag, 'Return')
  if (hir?._tag !== 'Return') return
  assert.strictEqual(hir.expression._tag, 'Match')
  assert.strictEqual(
    Hir.expressionTree(hir.expression).filter((item) => item._tag === 'Match').length,
    1,
  )
  assert.include(Hir.encode(result.hir), 'match shared members=main.End,main.Token : i32')
})

it('diagnoses incomplete, unreachable, foreign-member, guard, field, and result failures', () => {
  const incomplete = analyze(
    'incomplete',
    `pub struct Token { kind: i32 other: i32 }
pub struct End {}
pub struct Other {}
pub fn inspect(event: Token | End) -> i32 {
  return match event {
    Token { kind } if 1 => kind
    Other {} => 0
  }
}`,
  )
  assert.deepEqual(
    incomplete.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0044', 'SEM0046', 'SEM0045', 'SEM0042'],
  )
  assert.strictEqual(returnedMatch(incomplete).type._tag, 'Unavailable')

  const unreachable = analyze(
    'unreachable',
    `pub struct Token {}
pub fn inspect(event: Token) -> i32 {
  return match event { _ => 0 Token {} => 1 }
}`,
  )
  assert.deepEqual(
    unreachable.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0043'],
  )
  assert.strictEqual(returnedMatch(unreachable).arms[1]?.reachable, false)

  const incompatible = analyze(
    'incompatible',
    `pub struct Token {}
pub struct End {}
pub fn inspect(event: Token | End) -> i32 {
  return match event { Token {} => 0 End {} => false }
}`,
  )
  assert.deepEqual(
    incompatible.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0040'],
  )
  const joined = returnedMatch(incompatible).type
  assert.strictEqual(joined._tag, 'Available')
  if (joined._tag === 'Available') assert.strictEqual(Type.encode(joined.type), 'bool | i32')
})

it('retains nested canonical field paths and rejects pattern binding conflicts', () => {
  const result = analyze(
    'nested',
    `pub struct Span { start: i32 end: i32 }
pub struct Token { span: Span }
pub fn inspect(event: Token, offset: i32) -> i32 {
  return match event { Token { span: Span { start: offset, .. } } => offset }
}`,
  )
  const match = returnedMatch(result)
  const binding = match.arms[0]?.bindings[0]

  assert.strictEqual(binding?.path.length, 2)
  assert.deepEqual(
    binding?.path.map((field) => field.ordinal),
    [0, 0],
  )
  assert.include(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0048',
  )
  assert.strictEqual(match.type._tag, 'Unavailable')
})

it('joins nominal arm results and records explicit MatchArm widening in HIR', () => {
  const result = analyze(
    'joining',
    `pub struct Left {}
pub struct Right {}
pub struct HasLeft { value: Left }
pub struct HasRight { value: Right }
pub fn select(input: HasLeft | HasRight) -> Left | Right {
  return match move input {
    HasLeft { value } => value
    HasRight { value } => value
  }
}`,
  )
  const match = returnedMatch(result)
  const returned = result.hir.functions.at(0)?.statements.at(-1)

  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(match.type._tag, 'Available')
  if (match.type._tag === 'Available') {
    assert.strictEqual(Type.encode(match.type.type), 'joining.Left | joining.Right')
  }
  assert.strictEqual(returned?._tag, 'Return')
  if (returned?._tag !== 'Return' || returned.expression._tag !== 'Match') return
  assert.deepEqual(
    returned.expression.arms.map((arm) =>
      arm.result._tag === 'UnionConvert'
        ? { context: arm.result.context, target: Type.encode(arm.result.target) }
        : arm.result._tag,
    ),
    [
      { context: 'MatchArm', target: 'joining.Left | joining.Right' },
      { context: 'MatchArm', target: 'joining.Left | joining.Right' },
    ],
  )
  assert.deepEqual(Hir.verify(result.hir), [])
  const fn = result.hir.functions.at(0) ?? raise('expected joining HIR function')
  const reversedMatch = Object.freeze({
    ...returned.expression,
    arms: Object.freeze([...returned.expression.arms].reverse()),
  })
  const invalidModule: Hir.Module = Object.freeze({
    ...result.hir,
    functions: Object.freeze([
      Object.freeze({
        ...fn,
        statements: Object.freeze([Object.freeze({ ...returned, expression: reversedMatch })]),
      }),
    ]),
  })
  assert.include(
    Hir.verify(invalidModule).map((issue) => issue._tag),
    'InvalidMatchArmOrder',
  )
})

it('selects exact non-nominal members with whole-value bindings', () => {
  const result = analyze(
    'ordinary-members',
    `pub fn inspect(value: i32 | string) -> i32 {
  return match value {
    i32 number => number
    string text => 0
  }
}`,
  )
  const match = returnedMatch(result)

  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(match.members.map(Type.encode), ['i32', 'string'])
  assert.deepEqual(
    match.arms.map((arm) =>
      arm.pattern._tag === 'TypePattern' && arm.pattern.member !== undefined
        ? Type.encode(arm.pattern.member)
        : '_',
    ),
    ['i32', 'string'],
  )
  assert.deepEqual(
    match.arms.map((arm) =>
      arm.bindings[0]?.type._tag === 'Available'
        ? Type.encode(arm.bindings[0].type.type)
        : 'unavailable',
    ),
    ['i32', 'string'],
  )
})

it('keeps borrowed owners live, consumes move matches, and requires mutable exclusive roots', () => {
  const shared = analyze(
    'shared-owner',
    `pub struct Token { kind: i32 }
pub struct End {}
fn finish(event: Token | End) -> i32 { return 0 }
pub fn inspect(event: Token | End) -> i32 {
  let code = match &event { Token { kind } => kind End {} => 0 }
  return finish(move event)
}`,
  )
  assert.deepEqual(ownership(shared).diagnostics, [])

  const consumed = analyze(
    'consumed-owner',
    `pub struct Token { kind: i32 }
pub struct End {}
fn finish(event: Token | End) -> i32 { return 0 }
pub fn inspect(event: Token | End) -> i32 {
  let code = match move event { Token { kind } => kind End {} => 0 }
  return finish(move event)
}`,
  )
  assert.include(
    ownership(consumed).diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0001',
  )

  const bare = analyze(
    'bare-owner',
    `pub struct Token { kind: i32 }
pub fn inspect(event: Token) -> i32 { return match event { Token { kind } => kind } }`,
  )
  assert.include(
    ownership(bare).diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0003',
  )

  const exclusive = analyze(
    'exclusive-owner',
    `pub struct Token { kind: i32 }
pub fn inspect(event: Token) -> i32 { return match &mut event { Token { kind } => kind } }`,
  )
  assert.include(
    ownership(exclusive).diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0007',
  )
})

it('keeps guard bindings provisional and rejects borrowed payload escape', () => {
  const guarded = analyze(
    'guard-owner',
    `pub struct Payload {}
pub struct Box { value: Payload }
fn accept(value: Payload) -> bool { return true }
pub fn inspect(input: Box) -> i32 {
  return match move input {
    Box { value } if accept(move value) => 1
    Box { value: fallback } => 0
  }
}`,
  )
  assert.include(
    ownership(guarded).diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0008',
  )

  const borrowed = analyze(
    'borrowed-owner',
    `pub struct Payload {}
pub struct Box { value: Payload }
pub fn inspect(input: Box) -> Payload {
  return match &input { Box { value } => value }
}`,
  )
  assert.include(
    ownership(borrowed).diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0006',
  )
})

it('plans selected-arm cleanup for omitted and unreturned moved fields', () => {
  const result = analyze(
    'cleanup-owner',
    `pub struct Payload {}
pub struct Box { payload: Payload code: i32 }
pub fn inspect(input: Box) -> i32 {
  return match move input { Box { code, .. } => code }
}`,
  )
  const ownershipFacts = ownership(result)
  const match = ownershipFacts.functions.at(0)?.matches.at(0)

  assert.deepEqual(ownershipFacts.diagnostics, [])
  assert.strictEqual(match?.access, 'Move')
  assert.deepEqual(
    match?.arms.at(0)?.cleanup.map((entry) => ({
      path: entry.path.map((field) => field.ordinal),
      cleanup: entry.cleanup._tag,
    })),
    [{ path: [0], cleanup: 'StructCleanup' }],
  )
  assert.include(
    Ownership.encode(ownershipFacts),
    'cleanup=#0(struct:cleanup-owner.Payload fields=)',
  )
})
