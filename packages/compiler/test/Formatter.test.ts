import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Formatter from '../src/Formatter.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import type * as SyntaxFile from '../src/SyntaxFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const parse = (id: string, source: string): SyntaxFile.SyntaxFile =>
  Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(source))))

const formattedText = (document: FormattedDocument.FormattedDocument): string =>
  decoder.decode(FormattedDocument.toUint8Array(document))

interface NormalizedNode {
  readonly kind: SyntaxTree.NodeKind
  readonly children: ReadonlyArray<NormalizedNode | string>
}

const normalized = (syntax: SyntaxFile.SyntaxFile, node: SyntaxTree.Node): NormalizedNode => ({
  kind: node.kind,
  children: node.children.flatMap((child): ReadonlyArray<NormalizedNode | string> => {
    if (SyntaxTree.isNode(child)) return [normalized(syntax, child)]
    if (SyntaxTree.isMissingToken(child)) return [`missing:${child.expected}`]
    if (
      child.kind === 'Whitespace' ||
      child.kind === 'LineComment' ||
      child.kind === 'DocComment' ||
      child.kind === 'Comma' ||
      child.kind === 'EndOfFile'
    ) {
      return []
    }
    return [
      `${child.kind}:${decoder.decode(Option.getOrThrow(SourceFile.slice(syntax.source, child.span)))}`,
    ]
  }),
})

const comments = (syntax: SyntaxFile.SyntaxFile): ReadonlyArray<string> =>
  syntax.tokens
    .filter((token) => token.kind === 'LineComment' || token.kind === 'DocComment')
    .map((token) => decoder.decode(Option.getOrThrow(SourceFile.slice(syntax.source, token.span))))

const nodeKinds = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.NodeKind> => [
  node.kind,
  ...node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.NodeKind> =>
      SyntaxTree.isNode(child) ? nodeKinds(child) : [],
  ),
]

const completeNodeKinds: ReadonlyArray<SyntaxTree.NodeKind> = Object.freeze([
  'ArgumentList',
  'ArrayLiteralExpression',
  'AssignmentStatement',
  'BindingStatement',
  'Block',
  'BooleanLiteralExpression',
  'BorrowExpression',
  'BreakStatement',
  'CallExpression',
  'ConditionalStatement',
  'ContinueStatement',
  'DropStatement',
  'EffectExpression',
  'FieldProjectionExpression',
  'FailStatement',
  'FailureRow',
  'FixedArrayType',
  'FunctionDeclaration',
  'GroupedExpression',
  'IdentifierExpression',
  'ImportAlias',
  'ImportDeclaration',
  'ImportMember',
  'ImportMemberList',
  'ImportPath',
  'IndexProjectionExpression',
  'InfixExpression',
  'IntegerLiteralExpression',
  'MatchAccess',
  'MatchArm',
  'MatchExpression',
  'MoveExpression',
  'NominalPattern',
  'ParameterDeclaration',
  'ParameterList',
  'ParenthesizedType',
  'PipelineExpression',
  'CallableType',
  'PrefixExpression',
  'Requirement',
  'RequirementRow',
  'PatternField',
  'ReturnStatement',
  'RunExpression',
  'ReturnType',
  'RoleExpression',
  'RestPattern',
  'SliceType',
  'SourceFile',
  'StructDeclaration',
  'StructField',
  'StructFieldInitializer',
  'StructLiteralExpression',
  'TypePath',
  'UnionType',
  'UniversalPattern',
  'WhileStatement',
])

it.effect('formats a generic effect catch pipeline canonically and idempotently', () =>
  Effect.gen(function* () {
    const source =
      'pub fn main()->I32 { let recipe=risky()|>Core.prepare()|>Effect.catch<Problem>(recover) return 0 }'
    const first = yield* Formatter.format(parse('memory://effect-catch-pipeline.silk', source))
    const text = formattedText(first)
    assert.strictEqual(
      text,
      `pub fn main() -> I32 {
  let recipe = risky()
    |> Core.prepare()
    |> Effect.catch<Problem>(recover)
  return 0
}
`,
    )
    const second = yield* Formatter.format(parse('memory://effect-catch-pipeline.silk', text))
    assert.strictEqual(formattedText(second), text)
  }),
)

it.effect(
  'formats callable contracts, postfix calls, pipelines, and run precedence idempotently',
  () =>
    Effect.gen(function* () {
      const source =
        'fn apply(callback:mut fn(I32)->I32,value:I32)->I32{return (callback)(value)} fn main(attempt:Effect<I32>)->I32{return run attempt|>Effect.retry(2)}'
      const first = yield* Formatter.format(parse('memory://callable-format.silk', source))
      const text = formattedText(first)
      assert.strictEqual(
        text,
        `fn apply(callback: mut fn(I32) -> I32, value: I32) -> I32 {
  return (callback)(value)
}

fn main(attempt: Effect<I32>) -> I32 {
  return run attempt
    |> Effect.retry(2)
}
`,
      )
      const second = yield* Formatter.format(parse('memory://callable-format.silk', text))
      assert.deepEqual(second.bytes, first.bytes)
      assert.strictEqual(second.changed, false)
    }),
)

it.effect('formats effect contracts, run, and fail canonically and idempotently', () =>
  Effect.gen(function* () {
    const source = `effect   fn work(problem:Problem)->I32 ! Problem|Other { if true { fail   move problem } return 42 }
fn main()->I32 { let pending=work(Problem { code:1 }) return run   pending }`
    const first = yield* Formatter.format(parse('memory://effect-format.silk', source))
    const text = formattedText(first)
    assert.strictEqual(
      text,
      `effect fn work(problem: Problem) -> I32
! Problem | Other {
  if true {
    fail move problem
  }
  return 42
}

fn main() -> I32 {
  let pending = work(Problem {code: 1})
  return run pending
}
`,
    )
    const second = yield* Formatter.format(parse('memory://effect-format.silk', text))
    assert.strictEqual(formattedText(second), text)
  }),
)

it.effect('formats explicit drop canonically and idempotently', () =>
  Effect.gen(function* () {
    const first = yield* Formatter.format(
      parse(
        'memory://drop-format.silk',
        'struct Token { value:I32 } fn main()->I32 { let token=Token { value:1 } drop   token return 42 }',
      ),
    )
    const text = formattedText(first)
    assert.strictEqual(
      text,
      `struct Token {
  value: I32
}

fn main() -> I32 {
  let token = Token {value: 1}
  drop token
  return 42
}
`,
    )
    const second = yield* Formatter.format(parse('memory://drop-format.silk', text))
    assert.strictEqual(formattedText(second), text)
  }),
)

it.effect('formats unsafe blocks and conformance declarations canonically and idempotently', () =>
  Effect.gen(function* () {
    const source =
      'impl Allocator for SystemAllocator{allocate:SystemAllocator.allocate} impl Drop for Guard<Token>{fn drop(self:&mut Guard<Token>)->Unit{unsafe{drop self.value} return Unit.make()}}'
    const first = yield* Formatter.format(parse('memory://unsafe-format.silk', source))
    const text = formattedText(first)
    assert.strictEqual(
      text,
      `impl Allocator for SystemAllocator {
  allocate: SystemAllocator.allocate
}

impl Drop for Guard<Token> {
  fn drop(self: &mut Guard<Token>) -> Unit {
    unsafe {
      drop self.value
    }
    return Unit.make()
  }
}
`,
    )
    const second = yield* Formatter.format(parse('memory://unsafe-format.silk', text))
    assert.strictEqual(formattedText(second), text)
  }),
)

it.effect('formats whole-member binding patterns canonically and idempotently', () =>
  Effect.gen(function* () {
    const source =
      'fn take(state: Empty | Full) -> I32 { return match move state { Empty   nothing => 0 Full   full => 1 } }'
    const first = yield* Formatter.format(parse('memory://binding-pattern-format.silk', source))
    const text = formattedText(first)
    assert.strictEqual(
      text,
      `fn take(state: Empty | Full) -> I32 {
  return match move state {
    Empty nothing => 0
    Full full => 1
  }
}
`,
    )
    const second = yield* Formatter.format(parse('memory://binding-pattern-format.silk', text))
    assert.strictEqual(formattedText(second), text)
  }),
)

it.effect('formats parametric conformances canonically and idempotently', () =>
  Effect.gen(function* () {
    const source =
      'impl < T >Drop for Vector<T>{fn drop(self:&mut Vector<T>)->Unit{return Unit.make()}}'
    const first = yield* Formatter.format(parse('memory://parametric-format.silk', source))
    const text = formattedText(first)
    assert.strictEqual(
      text,
      `impl<T> Drop for Vector<T> {
  fn drop(self: &mut Vector<T>) -> Unit {
    return Unit.make()
  }
}
`,
    )
    const second = yield* Formatter.format(parse('memory://parametric-format.silk', text))
    assert.strictEqual(formattedText(second), text)
  }),
)

it.effect('formats explicit Effect and declaration requirement rows', () =>
  Effect.gen(function* () {
    const source = `fn later()->Effect<I32!Problem?&FileSystem|&mut Allocator@Scratch>{return effect{return 1}}
effect fn work()->I32!Problem?&FileSystem|&mut Allocator@Scratch{return 1}`
    const first = yield* Formatter.format(parse('memory://effect-requirement-format.silk', source))
    const text = formattedText(first)
    assert.strictEqual(
      text,
      `fn later() -> Effect<I32 ! Problem ? &FileSystem | &mut Allocator@Scratch> {
  return effect {
    return 1
  }
}

effect fn work() -> I32
! Problem
? &FileSystem | &mut Allocator@Scratch {
  return 1
}
`,
    )
    const second = yield* Formatter.format(parse('memory://effect-requirement-format.silk', text))
    assert.strictEqual(formattedText(second), text)
  }),
)

it.effect('formats effect blocks and Copy failure transfer canonically', () =>
  Effect.gen(function* () {
    const source = 'fn later()->I32 { let pending=effect{fail Problem{code:1}} return 0 }'
    const first = yield* Formatter.format(parse('memory://effect-expression-format.silk', source))
    const text = formattedText(first)
    assert.include(text, 'let pending = effect {')
    assert.include(text, 'fail Problem {code: 1}')
    const second = yield* Formatter.format(parse('memory://effect-expression-format.silk', text))
    assert.strictEqual(formattedText(second), text)
  }),
)

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it.effect('normalizes physical whitespace and detects canonical source', () =>
  Effect.gen(function* () {
    const source = 'pub\tfn main ( ) -> Mystery {\r\n\treturn  value   \r\n}\r\n\r\n'
    const first = yield* Formatter.format(parse('memory://physical.silk', source))
    const canonical = 'pub fn main() -> Mystery {\n  return value\n}\n'

    assert.strictEqual(formattedText(first), canonical)
    assert.strictEqual(first.changed, true)

    const second = yield* Formatter.format(parse('memory://physical.silk', canonical))
    assert.strictEqual(formattedText(second), canonical)
    assert.strictEqual(second.changed, false)
  }),
)

it.effect('rejects lexical and parser damage without producing formatted bytes', () =>
  Effect.gen(function* () {
    const lexical = yield* Effect.result(Formatter.format(parse('memory://lexical.silk', '@@@')))
    const parser = yield* Effect.result(
      Formatter.format(parse('memory://parser.silk', 'pub fn main() -> I32 { return 42')),
    )

    assert.strictEqual(Result.isFailure(lexical), true)
    assert.strictEqual(Result.isFailure(parser), true)
    if (Result.isFailure(lexical)) {
      assert.strictEqual(lexical.failure._tag, 'FormatterError')
      assert.strictEqual(lexical.failure.reason._tag, 'DamagedSyntax')
      assert.isAbove(lexical.failure.diagnostics.length, 0)
    }
  }),
)

it.effect('formats fixed-array source types with canonical bracketed layout', () =>
  Effect.gen(function* () {
    const source = 'struct Arrays { values: [ [ I32 ;4 ] ;3 ] }'
    const first = yield* Formatter.format(parse('memory://fixed-array-format.silk', source))
    const canonical = 'struct Arrays {\n  values: [[I32; 4]; 3]\n}\n'

    assert.strictEqual(formattedText(first), canonical)
    const second = yield* Formatter.format(
      parse('memory://fixed-array-format.silk', formattedText(first)),
    )
    assert.deepEqual(second.bytes, first.bytes)
    assert.strictEqual(second.changed, false)
  }),
)

it.effect('refuses to repair a missing fixed-array semicolon', () =>
  Effect.gen(function* () {
    const attempted = yield* Effect.result(
      Formatter.format(
        parse('memory://fixed-array-damage.silk', 'struct Broken { value: [I32 4] }'),
      ),
    )

    assert.strictEqual(Result.isFailure(attempted), true)
  }),
)

it.effect('formats syntactically complete source without semantic analysis', () =>
  Effect.gen(function* () {
    const document = yield* Formatter.format(
      parse(
        'memory://semantic.silk',
        'pub fn identity(value: Missing) -> Missing { return unknown }',
      ),
    )

    assert.strictEqual(
      formattedText(document),
      'pub fn identity(value: Missing) -> Missing {\n  return unknown\n}\n',
    )
  }),
)

it.effect('breaks over-width lists one item per line with a trailing comma', () =>
  Effect.gen(function* () {
    const source =
      'pub fn combine(firstParameterWithALongName: ExtremelyLongTypeName, ' +
      'secondParameterWithALongName: AnotherExtremelyLongTypeName) -> Result { return firstParameterWithALongName }'
    const document = yield* Formatter.format(parse('memory://width.silk', source))

    assert.strictEqual(
      formattedText(document),
      [
        'pub fn combine(',
        '  firstParameterWithALongName: ExtremelyLongTypeName,',
        '  secondParameterWithALongName: AnotherExtremelyLongTypeName,',
        ') -> Result {',
        '  return firstParameterWithALongName',
        '}',
        '',
      ].join('\n'),
    )
  }),
)

it.effect('preserves comment text and bounded blank-line attachment', () =>
  Effect.gen(function* () {
    const source = `/// first line
/// second line
pub fn first() -> I32 {
  let value = 1


  // grouped value
  let next = value // retained trailing comment
  return next
}


/// unattached

pub fn second() -> I32 { return 2 }
`
    const document = yield* Formatter.format(parse('memory://comments.silk', source))

    assert.strictEqual(
      formattedText(document),
      `/// first line
/// second line
pub fn first() -> I32 {
  let value = 1

  // grouped value
  let next = value // retained trailing comment
  return next
}

/// unattached

pub fn second() -> I32 {
  return 2
}
`,
    )
    assert.include(formattedText(document), '}\n\n/// unattached')
  }),
)

it.effect('keeps nested, delimiter, field-documentation, and end-of-file comments in order', () =>
  Effect.gen(function* () {
    const source = `pub struct Documented {
  /// field documentation
  value: I32
}
pub fn main(
  /// parameter documentation
  value: I32
) -> I32 {
  return helper(
    value, // trailing argument
    2
  ) // trailing call
}
// end of file
`
    const original = parse('memory://comment-boundaries.silk', source)
    const document = yield* Formatter.format(original)
    const reparsed = parse('memory://comment-boundaries.silk', formattedText(document))

    assert.deepEqual(comments(reparsed), comments(original))
    assert.strictEqual(
      formattedText(document),
      `pub struct Documented {
  /// field documentation
  value: I32
}

pub fn main(
  /// parameter documentation
  value: I32,
) -> I32 {
  return helper(
    value, // trailing argument
    2,
  ) // trailing call
}
// end of file
`,
    )
  }),
)

it.effect('removes only terminal horizontal whitespace from comment spellings', () =>
  Effect.gen(function* () {
    const source = '/// documentation  \t\npub fn main() -> I32 { return 1 // value  \t\n}\n'
    const document = yield* Formatter.format(parse('memory://comment-whitespace.silk', source))

    assert.strictEqual(
      formattedText(document),
      '/// documentation\npub fn main() -> I32 {\n  return 1 // value\n}\n',
    )
  }),
)

it.effect('formats match arms, guards, access modes, and nested patterns idempotently', () =>
  Effect.gen(function* () {
    const source = `pub struct Span { start: I32 end: I32 }
pub struct Token { span: Span }
pub fn inspect(event: Token) -> I32 { return match   & mut event { Token { span: Span { start: offset , .. }, .. } if true=>offset _=>0 } }`
    const first = yield* Formatter.format(parse('memory://match-format.silk', source))
    const expected = `pub struct Span {
  start: I32
  end: I32
}

pub struct Token {
  span: Span
}

pub fn inspect(event: Token) -> I32 {
  return match &mut event {
    Token {span: Span {start: offset, ..}, ..} if true => offset
    _ => 0
  }
}
`

    assert.strictEqual(formattedText(first), expected)
    const second = yield* Formatter.format(parse('memory://match-format.silk', expected))
    assert.deepEqual(second.bytes, first.bytes)
    assert.strictEqual(second.changed, false)
  }),
)

it.effect('prints and reparses the complete current grammar surface', () =>
  Effect.gen(function* () {
    const source = `import Core.Math as Math { add as plus, subtract }
pub struct Pair {
  pub left: [I32; 2]
  right: Bool
  choice: Alpha | (Beta | Alpha)
}
pub struct Span { start: I32 end: I32 }
pub struct Token { span: Span }
pub struct End {}
fn helper(value: I32, other: I32) -> I32 {
  let mut moved = move value
  while moved < other {
    if false { break } else { continue }
  }
  moved = moved + 1
  if !false { return (moved + other) } else { return Pair { left: [1, 2], right: true }.left[0] }
  return moved
}
fn inspect(event: Token | End) -> I32 {
  return match &mut event { Token { span: Span { start: offset, .. }, .. } if true => offset _ => 0 }
}
fn scan(values: &[I32], output: &mut [I32]) -> I32 {
  return helper(values.length, output[0])
}
fn callbacks(shared: fn(I32, Bool) -> I32, exclusive: mut fn(I32) -> Bool, consuming: once fn() -> I32) -> I32 {
  return shared(1, true)
}
effect fn delayed(problem: Token) -> I32 ! Token {
  if false { fail move problem }
  return 1
}
effect fn timed() -> I32 ? &End@Clock { return 1 }
fn execute(problem: Token) -> I32 {
  let local = effect { return 2 }
  drop local
  let pending = delayed(move problem)
  let timed = timed() |> End.provide(&local, @Clock)
  return run pending
}
fn borrow(values: [I32; 2], output: [I32; 2]) -> I32 {
  let mut target = move output
  return scan(&values, &mut target)
}
pub fn main() -> I32 { return helper(-1, 2) |> Core.finish() }
`
    const original = parse('memory://grammar.silk', source)
    assert.deepEqual(original.lexicalDiagnostics, [])
    assert.deepEqual(original.parserDiagnostics, [])

    const first = yield* Formatter.format(original)
    const reparsed = parse('memory://grammar.silk', formattedText(first))
    assert.deepEqual(reparsed.lexicalDiagnostics, [])
    assert.deepEqual(reparsed.parserDiagnostics, [])
    assert.strictEqual(SyntaxTree.isAvailableSyntax(reparsed.root), true)
    assert.deepEqual(normalized(reparsed, reparsed.root), normalized(original, original.root))
    assert.deepEqual(comments(reparsed), comments(original))
    assert.deepEqual(
      Array.from(new Set(nodeKinds(original.root))).sort(),
      Array.from(completeNodeKinds).sort(),
    )
    assert.strictEqual(formattedText(first), golden('canonical.silk'))

    const second = yield* Formatter.format(reparsed)
    assert.deepEqual(second.bytes, first.bytes)
    assert.strictEqual(second.changed, false)
  }),
)
