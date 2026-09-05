import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as MirLinearization from '../src/MirLinearization.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Match from '../src/Match.js'
import * as OwnershipEncoding from '../src/OwnershipEncoding.js'
import * as Parser from '../src/Parser.js'
import * as StatementAnalysis from '../src/StatementAnalysis.js'
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
  assert.deepEqual(match.members.map(Match.encodeIdentity), ['main.End', 'main.Token'])
  assert.deepEqual(
    match.arms.map((arm) => ({
      member:
        arm.pattern._tag === 'NominalPattern' && arm.pattern.member !== undefined
          ? Type.encode(arm.pattern.member)
          : '_',
      before: arm.before.map(Match.encodeIdentity),
      after: arm.after.map(Match.encodeIdentity),
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
  const body = match.arms[1]?.body
  assert.strictEqual(body?._tag, 'Expression')
  if (body?._tag === 'Expression') {
    assert.strictEqual(body.expression._tag, 'Identifier')
    if (body.expression._tag === 'Identifier')
      assert.strictEqual(body.expression.reference._tag, 'ResolvedPattern')
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

it('covers scalar enums by canonical member identity without payload bindings', () => {
  const result = analyze(
    'enum-coverage',
    `enum Status { Unknown, Ready }
fn inspect(value: Status) -> i32 {
  return match value {
    Status.Unknown if false => 9
    Status.Unknown => 0
    Status.Ready => 1
  }
}`,
  )
  const match = returnedMatch(result)

  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(match.members.map(Match.encodeIdentity), [
    'enum-coverage.Status.Unknown',
    'enum-coverage.Status.Ready',
  ])
  assert.deepEqual(
    match.arms.map((arm) => ({
      pattern: arm.pattern._tag,
      bindings: arm.bindings.length,
      before: arm.before.map(Match.encodeIdentity),
      after: arm.after.map(Match.encodeIdentity),
      reachable: arm.reachable,
    })),
    [
      {
        pattern: 'EnumMemberPattern',
        bindings: 0,
        before: ['enum-coverage.Status.Unknown', 'enum-coverage.Status.Ready'],
        after: ['enum-coverage.Status.Unknown', 'enum-coverage.Status.Ready'],
        reachable: true,
      },
      {
        pattern: 'EnumMemberPattern',
        bindings: 0,
        before: ['enum-coverage.Status.Unknown', 'enum-coverage.Status.Ready'],
        after: ['enum-coverage.Status.Ready'],
        reachable: true,
      },
      {
        pattern: 'EnumMemberPattern',
        bindings: 0,
        before: ['enum-coverage.Status.Ready'],
        after: [],
        reachable: true,
      },
    ],
  )
  assert.strictEqual(match.type._tag, 'Available')
  assert.strictEqual(match.scrutinee.type._tag, 'Available')
  if (match.scrutinee.type._tag === 'Available')
    assert.strictEqual(Type.encode(match.scrutinee.type.type), 'enum-coverage.Status')
  const returned = result.hir.functions.at(0)?.statements.at(-1)
  assert.strictEqual(returned?._tag, 'Return')
  if (returned?._tag !== 'Return' || returned.expression._tag !== 'Match') return
  assert.strictEqual(returned.expression.arms[0]?.member?._tag, 'EnumMember')
  assert.deepEqual(Hir.verify(result.hir), [])
  assert.include(
    Hir.encode(result.hir),
    'match copy members=enum-coverage.Status.Unknown,enum-coverage.Status.Ready : i32',
  )
})

it('reports scalar enum coverage diagnostics with stable codes and exact spans', () => {
  const missingSource = `enum Status { Unknown, Ready }
fn inspect(value: Status) -> i32 {
  return match value { Status.Unknown if false => 0 Status.Ready => 1 }
}`
  const missing = analyze('enum-missing', missingSource)
  assert.deepEqual(
    missing.diagnostics.map((diagnostic) => ({ code: diagnostic.code, reason: diagnostic.reason })),
    [
      {
        code: 'SEM0158',
        reason: {
          _tag: 'IncompleteEnumMatch',
          enum: 'enum-missing.Status',
          missing: ['enum-missing.Status.Unknown'],
        },
      },
    ],
  )
  const missingMatchStart = missingSource.indexOf('match value')
  const functionEnd = missingSource.lastIndexOf('}')
  const missingMatchEnd = missingSource.lastIndexOf('}', functionEnd - 1) + 1
  assert.deepEqual(
    missing.diagnostics.map((diagnostic) => [diagnostic.span.start, diagnostic.span.end]),
    [[missingMatchStart, missingMatchEnd]],
  )

  const duplicateSource = `enum Status { Unknown, Ready }
fn inspect(value: Status) -> i32 {
  return match value { Status.Unknown => 0 Status.Unknown => 1 Status.Ready => 2 }
}`
  const duplicate = analyze('enum-duplicate', duplicateSource)
  const firstMember = duplicateSource.indexOf('Status.Unknown')
  const secondMember = duplicateSource.indexOf('Status.Unknown', firstMember + 1)
  assert.deepEqual(
    duplicate.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      span: [diagnostic.span.start, diagnostic.span.end],
      related: diagnostic.relatedSpans?.map((related) => [related.span.start, related.span.end]),
    })),
    [
      {
        code: 'SEM0159',
        span: [secondMember, secondMember + 'Status.Unknown'.length],
        related: [[firstMember, firstMember + 'Status.Unknown'.length]],
      },
    ],
  )

  const wildcardSource = `enum Status { Unknown, Ready }
fn inspect(value: Status) -> i32 {
  return match value { _ => 0 Status.Ready => 1 }
}`
  const wildcard = analyze('enum-wildcard', wildcardSource)
  const wildcardStart = wildcardSource.indexOf('_ => 0')
  const laterArm = wildcardSource.indexOf('Status.Ready')
  assert.deepEqual(
    wildcard.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      span: [diagnostic.span.start, diagnostic.span.end],
      related: diagnostic.relatedSpans?.map((related) => [related.span.start, related.span.end]),
    })),
    [
      {
        code: 'SEM0160',
        span: [laterArm, laterArm + 'Status.Ready => 1'.length],
        related: [[wildcardStart, wildcardStart + '_ => 0'.length]],
      },
    ],
  )

  const invalidSource = `enum Status { Ready }
enum Other { Ready }
fn foreign(value: Status) -> i32 { return match value { Other.Ready => 0 _ => 1 } }
fn integer(value: Status) -> i32 { return match value { 0 => 0 _ => 1 } }`
  const invalid = analyze('enum-invalid-patterns', invalidSource)
  const foreignStart = invalidSource.indexOf('Other.Ready')
  const integerStart = invalidSource.indexOf('0 => 0')
  assert.deepEqual(
    invalid.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      span: [diagnostic.span.start, diagnostic.span.end],
    })),
    [
      { code: 'SEM0161', span: [foreignStart, foreignStart + 'Other.Ready'.length] },
      { code: 'SEM0162', span: [integerStart, integerStart + 1] },
    ],
  )
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
      arm.body._tag === 'Expression' && arm.body.expression._tag === 'UnionConvert'
        ? { context: arm.body.expression.context, target: Type.encode(arm.body.expression.target) }
        : arm.body._tag,
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
  assert.deepEqual(match.members.map(Match.encodeIdentity), ['i32', "string<'life0>"])
  assert.deepEqual(
    match.arms.map((arm) =>
      arm.pattern._tag === 'TypePattern' && arm.pattern.member !== undefined
        ? Type.encode(arm.pattern.member)
        : '_',
    ),
    ['i32', "string<'life0>"],
  )
  assert.deepEqual(
    match.arms.map((arm) =>
      arm.bindings[0]?.type._tag === 'Available'
        ? Type.encode(arm.bindings[0].type.type)
        : 'unavailable',
    ),
    ['i32', "string<'life0>"],
  )
})

it('rejects refutable let and wildcard discard while accepting scoped if-let bindings', () => {
  const result = analyze(
    'statement-pattern-diagnostics',
    `pub struct Point { x: i32 y: i32 }
pub fn inspect(value: Point | i32) -> i32 {
  let Point { x, .. } = move value
  let _ = x
  if let i32 number = value { return number } else { return 0 }
}`,
  )

  assert.include(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0133',
  )
  assert.include(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    'SEM0087',
  )
  const statement = result.functions
    .at(0)
    ?.statements.find((candidate) => candidate._tag === 'IfLetStatement')
  assert.strictEqual(statement?._tag, 'IfLetStatement')
  if (statement?._tag !== 'IfLetStatement') return
  assert.strictEqual(statement.selection.bindings[0]?.name._tag, 'Present')
  assert.strictEqual(statement.taken[0]?._tag, 'ReturnStatement')
})

it('keeps statement-pattern loans scoped and move selection consumed on both outcomes', () => {
  const shared = analyze(
    'shared-pattern-loan',
    `pub struct Point { x: i32 y: i32 }
pub fn inspect(point: Point) -> i32 {
  let Point { x, .. } = &point
  let y = point.y
  return x + y
}`,
  )
  assert.deepEqual(shared.diagnostics, [])
  assert.deepEqual(ownership(shared).diagnostics, [])

  const exclusive = analyze(
    'exclusive-pattern-loan',
    `pub struct Point { x: i32 y: i32 }
pub fn inspect(point: Point) -> i32 {
  let mut owned = move point
  if let Point { x, .. } = &mut owned { let invalid = owned.y } else {}
  return owned.y
}`,
  )
  assert.include(
    ownership(exclusive).diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0011',
  )

  const moved = analyze(
    'moved-pattern-selection',
    `pub struct Point { x: i32 }
fn consume(value: Point | i32) -> i32 { return 0 }
pub fn inspect(value: Point | i32) -> i32 {
  if let Point { x } = move value { let selected = x } else {}
  return consume(move value)
}`,
  )
  assert.include(
    ownership(moved).diagnostics.map((diagnostic) => diagnostic.code),
    'OWN0001',
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
    OwnershipEncoding.encode(ownershipFacts),
    'cleanup=#0(struct:cleanup-owner.Payload fields=)',
  )
})

it('retains ordinary arm bodies and computes unit and never from structured statement flow', () => {
  const result = analyze(
    'ordinary-completion',
    `enum Choice { First, Last }
fn unit(value: Choice) { match value { Choice.First => {} Choice.Last => () } }
fn mixed(value: Choice) -> i32 { return match value { Choice.First => { return 7 } Choice.Last => 9 } }
fn total(value: Choice, flag: bool) -> i32 {
  match value { Choice.First => { if flag { return 1 } else { return 2 } } Choice.Last => { return 3 } }
}
fn inner(value: Choice) {
  match value { Choice.First => { while true { break } drop 42 } Choice.Last => {} }
}`,
  )
  assert.deepEqual(result.diagnostics, [])
  const matches: Array<Elaboration.MatchExpressionFact> = []
  for (const fn of result.functions)
    Elaboration.visitStatementFacts(fn.statements, {
      expression: (expression) => {
        if (expression._tag === 'Match') matches.push(expression)
      },
    })
  assert.deepEqual(
    matches.map((match) =>
      match.type._tag === 'Available' ? Type.encode(match.type.type) : 'unavailable',
    ),
    ['()', 'i32', 'never', '()'],
  )
  assert.deepEqual(
    matches.map((match) =>
      match.arms.map((arm) =>
        arm.body._tag === 'Block' ? arm.body.completion.fallsThrough : 'expression',
      ),
    ),
    [
      [true, 'expression'],
      [false, 'expression'],
      [false, false],
      [true, true],
    ],
  )
  assert.deepEqual(Hir.verify(result.hir), [])
  for (const fn of result.hir.functions)
    for (const root of fn.statements.flatMap(Hir.statementExpressions)) {
      for (const match of Hir.expressionTree(root).filter(
        (expression) => expression._tag === 'Match',
      )) {
        assert.isTrue(
          match.arms.every(
            (arm) => arm.body.span.start >= arm.span.start && arm.body.span.end <= arm.span.end,
          ),
        )
      }
    }
})

it('checks ordinary arm statements and partial completion without contextual unit coercion', () => {
  const source = `enum Choice { First, Last }
fn scalar(value: Choice) -> i32 { return match value { Choice.First => {} Choice.Last => 1 } }
fn partial(value: Choice, flag: bool) -> i32 { return match value { Choice.First => { if flag { return 2 } } Choice.Last => 1 } }
fn bare(value: Choice) { match value { Choice.First => { 42 } Choice.Last => {} } }
fn outside(value: Choice) { match value { Choice.First => { break } Choice.Last => { continue } } }
fn scope(value: Choice) { match value { Choice.First => { let inner = 1 drop inner } Choice.Last => {} } drop inner }
`
  const result = analyze('ordinary-errors', source)
  assert.deepEqual([...result.syntax.lexicalDiagnostics, ...result.syntax.parserDiagnostics], [])
  const selected = result.diagnostics.filter((diagnostic) =>
    ['SEM0049', 'SEM0087', 'SEM0038'].includes(diagnostic.code),
  )
  const matches: Array<Elaboration.MatchExpressionFact> = []
  for (const fn of result.functions)
    Elaboration.visitStatementFacts(fn.statements, {
      expression: (expression) => {
        if (expression._tag === 'Match') matches.push(expression)
      },
    })
  assert.deepEqual(
    selected.map((diagnostic) => [diagnostic.code, diagnostic.span.start, diagnostic.span.end]),
    [
      ...matches
        .slice(0, 2)
        .map((match) => ['SEM0049', match.syntax.span.start, match.syntax.span.end]),
      ['SEM0087', source.indexOf('42') - 1, source.indexOf('42') + 2],
      ['SEM0038', source.indexOf('break') - 1, source.indexOf('break') + 5],
      ['SEM0038', source.indexOf('continue') - 1, source.indexOf('continue') + 8],
    ],
  )
  assert.isTrue(
    result.diagnostics.some((diagnostic) => diagnostic.span.start === source.lastIndexOf('inner')),
  )
})

it('discovers enclosing returns through arguments and guards and suppresses an abandoned outer return', () => {
  const source = `enum Choice { First, Last }
fn use(value: i32) -> i32 { return value }
fn argument(value: Choice) -> i32 { return use(match value { Choice.First => { return true } Choice.Last => 1 }) }
fn operand(value: Choice) -> i32 { return match value { Choice.First => { return 2 } Choice.Last => { return 3 } } }
fn guarded(value: Choice) -> i32 { return match value { Choice.First if match value { Choice.First => { return 4 } Choice.Last => { return 5 } } => 8 Choice.First => 1 Choice.Last => 2 } }
fn borrowed(values: &[i32], value: Choice) -> &[i32] { return match value { Choice.First => { return values } Choice.Last => values } }
fn invalidBorrow(values: &[i32], value: Choice) -> &[i32] { return match value { Choice.First => { let local = [1] return &local } Choice.Last => values } }
`
  const result = analyze('ordinary-returns', source)
  assert.deepEqual([...result.syntax.lexicalDiagnostics, ...result.syntax.parserDiagnostics], [])
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => [
      diagnostic.code,
      diagnostic.span.start,
      diagnostic.span.end,
    ]),
    [
      ['SEM0129', source.indexOf('true') - 1, source.indexOf('true') + 4],
      ['OWN0019', source.lastIndexOf('match value') - 1, source.lastIndexOf('} }') + 1],
      ['OWN0019', source.indexOf('&local') - 1, source.indexOf('&local') + 6],
      ['SEM0212', source.indexOf('&local') - 1, source.indexOf('&local') + 6],
      ['OWN0019', source.lastIndexOf('values') - 1, source.lastIndexOf('values') + 6],
    ],
  )
  const operand =
    result.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'operand',
    ) ?? raise('expected operand function')
  const flow = StatementAnalysis.returnFlowOf(operand.statements)
  assert.strictEqual(flow.fallsThrough, false)
  assert.deepEqual(
    flow.returns.map((returned) =>
      source.slice(returned.expression.syntax.span.start, returned.expression.syntax.span.end),
    ),
    [' 2', ' 3'],
  )
  const guarded =
    result.hir.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'guarded',
    ) ?? raise('expected guarded function')
  assert.deepEqual(Hir.verify({ ...result.hir, functions: [guarded] }), [])
})

it('shares lexical loop destinations and enclosing failure contracts without crossing explicit boundaries', () => {
  const source = `enum Choice { First, Last }
struct Problem {}
effect fn step() -> () ! Problem { return () }
effect fn eager(value: Choice) -> () ! Problem {
  match value { Choice.First => { run step() run step() } Choice.Last => { fail Problem {} } }
}
fn loops(value: Choice) {
  while true {
    match value { Choice.First => { while true { break } continue } Choice.Last => { break } }
  }
}
fn boundary(value: Choice) {
  while true {
    match value { Choice.First => { let pending = effect { break } drop pending } Choice.Last => { break } }
  }
}
fn illegal(value: Choice) { match value { Choice.First => { fail Problem {} } Choice.Last => {} } }
struct Holder { item: i32 }
fn conflict(value: Holder) { match value { Holder { item } => { let item = 1 drop item } } }
`
  const result = analyze('ordinary-boundaries', source)
  assert.deepEqual([...result.syntax.lexicalDiagnostics, ...result.syntax.parserDiagnostics], [])
  const loops =
    result.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'loops',
    ) ?? raise('expected loops')
  const transfers: Array<
    Extract<Elaboration.StatementFact, { readonly _tag: 'BreakStatement' | 'ContinueStatement' }>
  > = []
  Elaboration.visitStatementFacts(loops.statements, {
    statement: (statement) => {
      if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement')
        transfers.push(statement)
    },
  })
  assert.deepEqual(
    transfers.map((statement) => [statement._tag, statement.target?.ordinal]),
    [
      ['BreakStatement', 1],
      ['ContinueStatement', 0],
      ['BreakStatement', 0],
    ],
  )
  const boundaryBreak = source.indexOf('break', source.indexOf('effect {'))
  assert.deepEqual(
    result.diagnostics
      .filter((diagnostic) => diagnostic.code === 'SEM0038')
      .map((diagnostic) => [diagnostic.span.start, diagnostic.span.end]),
    [[boundaryBreak - 1, boundaryBreak + 5]],
  )
  const eager =
    result.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'eager',
    ) ?? raise('expected eager')
  const eagerSpan = eager.declaration.syntax.span
  assert.deepEqual(
    result.diagnostics.filter(
      (diagnostic) =>
        diagnostic.span.start >= eagerSpan.start && diagnostic.span.end <= eagerSpan.end,
    ),
    [],
  )
  assert.isTrue(
    result.diagnostics.some((diagnostic) => diagnostic.span.start >= source.indexOf('fn illegal')),
  )
})

it.effect(
  'lowers an all-transferring argument without its outer call or a match result local',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'ordinary-mir-argument',
        new TextEncoder().encode(`enum Choice { First, Last }
fn first(value: i32) -> i32 { return value }
fn later(value: i32) -> i32 { return value }
fn consume(left: i32, right: i32, last: i32) -> i32 { return left + right + last }
fn argument(value: Choice) -> i32 {
  return consume(first(1), match value { Choice.First => { return 7 } Choice.Last => { return 8 } }, later(99))
}
struct DeferredValue { value: i32 }
struct DeferredEmpty {}
fn deferred(input: DeferredValue | DeferredEmpty) -> Effect<'static; i32> {
  match &input {
    DeferredValue { value } => { return effect { return run effect { return value } } }
    DeferredEmpty {} => { return effect { match move (DeferredValue { value: 8 }) { DeferredValue { value } => { return value } } } }
  }
}
pub fn main() -> i32 { let deferredValue = run deferred(DeferredValue { value: 7 }) return argument(Choice.First) + deferredValue }`),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const program = Analysis.loweredMir(snapshot)
      assert.deepEqual(MirVerification.verify(program), [])
      const fn =
        program.functions.find((candidate) => candidate.id.name === 'argument') ??
        raise('expected argument MIR')
      const operations = MirVerification.operations(fn)
      const match =
        operations.find((operation) => operation._tag === 'Match') ?? raise('expected match MIR')
      assert.isUndefined(match.destination)
      assert.isTrue(match.arms.every((arm) => arm.selected.execution.result === undefined))
      assert.isFalse(fn.localTypes.some((type) => type._tag === 'Bottom'))
      assert.deepEqual(
        operations
          .filter((operation) => operation._tag === 'Call')
          .map((operation) => operation.target.name),
        ['first'],
      )
      const blocks = MirLinearization.linearize(fn)
      assert.isTrue(blocks.some((block) => block.terminator._tag === 'Return'))
      assert.isFalse(
        blocks.some((block) =>
          block.operations.some(
            (operation) => operation._tag === 'Call' && operation.target.name !== 'first',
          ),
        ),
      )
      const deferredFact =
        Analysis.rootAnalysis(snapshot).functions.find(
          (candidate) =>
            candidate.declaration.name._tag === 'Present' &&
            candidate.declaration.name.spelling === 'deferred',
        ) ?? raise('expected deferred function fact')
      const captureReferences: Array<ReadonlyArray<string>> = []
      Elaboration.visitStatementFacts(deferredFact.statements, {
        expression: (expression) => {
          if (expression._tag === 'EffectBlock')
            captureReferences.push(expression.captures.map((capture) => capture.reference._tag))
        },
      })
      assert.deepEqual(captureReferences, [['PatternBinding'], ['PatternBinding'], []])
      const deferred =
        program.functions.find((candidate) => candidate.id.name === 'deferred') ??
        raise('expected deferred MIR')
      assert.strictEqual(deferred.result._tag, 'EffectComposite')
      assert.deepEqual(
        MirVerification.operations(deferred)
          .filter((operation) => operation._tag === 'PackEffectComposite')
          .map((operation) => operation.alternative)
          .sort((left, right) => left - right),
        [0, 1],
      )
      for (const region of Mir.regionsTree(deferred.regions)) {
        if (!('outcome' in region) || region.outcome._tag !== 'Return') continue
        assert.deepEqual(deferred.localTypes.at(region.outcome.value.ordinal), deferred.result)
      }
    }),
)

it.effect(
  'keeps unit completion and a partial enclosing return on distinct match region paths',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'ordinary-mir-partial',
        new TextEncoder().encode(`enum Choice { First, Last }
fn partial(value: Choice, stop: bool) -> i32 {
  let mut total = 0
  let completed = match value { Choice.First => { if stop { return 7 } total = 3 } Choice.Last => { total = 4 } }
  drop completed
  return total
}
pub fn main() -> i32 { return partial(Choice.First, false) }`),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const program = Analysis.loweredMir(snapshot)
      assert.deepEqual(MirVerification.verify(program), [])
      const fn =
        program.functions.find((candidate) => candidate.id.name === 'partial') ??
        raise('expected partial MIR')
      const match =
        MirVerification.operations(fn).find((operation) => operation._tag === 'Match') ??
        raise('expected match MIR')
      const destination = match.destination ?? raise('expected normal unit destination')
      const selected =
        match.arms.at(0)?.selected.execution ?? raise('expected selected region graph')
      assert.isDefined(selected.result)
      assert.isTrue(
        selected.regions.some((region) => 'outcome' in region && region.outcome._tag === 'Return'),
      )
      assert.isTrue(
        selected.regions.some(
          (region) => 'outcome' in region && region.outcome._tag === 'Complete',
        ),
      )
      const blocks = MirLinearization.linearize(fn)
      assert.isTrue(
        blocks.some((block) =>
          block.operations.some(
            (operation) =>
              operation._tag === 'Move' && operation.destination.ordinal === destination.ordinal,
          ),
        ),
      )
      for (const block of blocks.filter((block) => block.terminator._tag === 'Return'))
        assert.isFalse(
          block.operations.some(
            (operation) =>
              operation._tag === 'Move' && operation.destination.ordinal === destination.ordinal,
          ),
        )
    }),
)

it.effect('lowers transferring guards and preserves inner and enclosing loop destinations', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'ordinary-mir-guard-loop',
      new TextEncoder().encode(`enum Choice { First, Last }
fn guarded(value: Choice) -> i32 {
  return match value { Choice.First if match value { Choice.First => { return 4 } Choice.Last => { return 5 } } => 999 Choice.First => 1 Choice.Last => 2 }
}
fn stoppedGuard() -> i32 { match 0 { _ if match 0 { _ => { return 42 } } => {} _ => {} } }
fn loops(value: Choice) -> i32 {
  let mut total = 0
  while total < 2 {
    total = total + 1
    match value { Choice.First => { while true { break } continue } Choice.Last => { break } }
  }
  return total
}
pub fn main() -> i32 { return guarded(Choice.First) + loops(Choice.Last) + stoppedGuard() }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(program), [])
    const stoppedGuardFact =
      snapshot.results
        .get('ordinary-mir-guard-loop')
        ?.functions.find(
          (fn) =>
            fn.declaration.name._tag === 'Present' &&
            fn.declaration.name.spelling === 'stoppedGuard',
        ) ?? raise('expected stopped guard function fact')
    const stoppedMatches: Array<Elaboration.MatchExpressionFact> = []
    Elaboration.visitStatementFacts(stoppedGuardFact.statements, {
      expression: (expression) => {
        if (expression._tag === 'Match') stoppedMatches.push(expression)
      },
    })
    assert.deepEqual(
      stoppedMatches.map((match) =>
        match.type._tag === 'Available' ? Type.encode(match.type.type) : 'unavailable',
      ),
      ['never', 'never'],
    )
    const stoppedGuard =
      program.functions.find((fn) => fn.id.name === 'stoppedGuard') ??
      raise('expected stopped guard MIR')
    const stoppedOperations = MirVerification.operations(stoppedGuard)
    for (const match of stoppedOperations.filter((operation) => operation._tag === 'Match'))
      assert.isUndefined(match.destination)
    assert.isFalse(
      stoppedGuard.localTypes.some(
        (type) => type._tag === 'Bottom' || Type.equals(Mir.semanticType(type), Type.unit),
      ),
    )
    assert.doesNotThrow(() => MirLinearization.linearize(stoppedGuard))
    const guarded =
      program.functions.find((candidate) => candidate.id.name === 'guarded') ??
      raise('expected guarded MIR')
    const match =
      MirVerification.operations(guarded).find((operation) => operation._tag === 'Match') ??
      raise('expected guarded selection')
    assert.isUndefined(match.arms.at(0)?.guard?.execution.result)
    assert.isFalse(
      MirVerification.operations(guarded).some(
        (operation) => operation._tag === 'Literal' && operation.value === 999,
      ),
    )
    const loops =
      program.functions.find((candidate) => candidate.id.name === 'loops') ??
      raise('expected loop MIR')
    const outcomes = Mir.regionsTree(loops.regions).flatMap((region) =>
      'outcome' in region ? [region.outcome] : [],
    )
    assert.isTrue(outcomes.some((outcome) => outcome._tag === 'Exit' && outcome.loop.ordinal === 1))
    assert.isTrue(
      outcomes.some((outcome) => outcome._tag === 'Repeat' && outcome.loop.ordinal === 0),
    )
    assert.isTrue(outcomes.some((outcome) => outcome._tag === 'Exit' && outcome.loop.ordinal === 0))
    assert.doesNotThrow(() => MirLinearization.linearize(guarded))
    assert.doesNotThrow(() => MirLinearization.linearize(loops))
  }),
)
