import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import type * as SyntaxFile from '../src/SyntaxFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import type * as Token from '../src/Token.js'
import {
  acceptedShape,
  acceptedSource,
  damagedCallBeforeNextFunctionSource,
  damagedNestedBeforeNextFunctionSource,
  damagedNestedSiblingSource,
  damagedStructSource,
  denseTriviaSource,
  type ExpectedNodeShape,
  emptySource,
  identifierCallArgumentSource,
  identitySource,
  interFunctionPunctuationSource,
  invalidUtf8Source,
  malformedArgumentSource,
  missingCallCalleeSource,
  missingCallRightParenthesisSource,
  missingFirstRightBraceSource,
  missingNameSource,
  missingNestedRightParenthesisSource,
  missingParameterCommaSource,
  missingParameterTypeSource,
  missingRightBraceSource,
  nestedCallSource,
  nestedSiblingCallSource,
  threeFunctionSource,
  trailingTriviaSource,
  triviaCallSource,
  twoFunctionSource,
  twoParameterSource,
  unexpectedPunctuationSource,
  validCallSource,
  validStructSource,
  valueCallArgumentSource,
  whollyUnrelatedSource,
} from './fixtures/BootstrapParserFixture.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parseBytes = (id: string, bytes: Uint8Array): SyntaxFile.SyntaxFile =>
  Parser.parse(Lexer.lex(SourceFile.make(id, bytes)))

const parseText = (id: string, source: string): SyntaxFile.SyntaxFile =>
  parseBytes(id, ascii(source))

const nodeShape = (node: SyntaxTree.Node): ExpectedNodeShape => ({
  kind: node.kind,
  children: node.children.map((child): string | ExpectedNodeShape => {
    if (SyntaxTree.isNode(child)) return nodeShape(child)
    if (SyntaxTree.isToken(child)) return child.kind
    return `Missing(${child.expected})`
  }),
})

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Element> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Element> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [child],
  )

const missingLeaves = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.MissingToken> =>
  descendants(node).filter(SyntaxTree.isMissingToken)

const errorNodes = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  descendants(node).filter(
    (element): element is SyntaxTree.Node => SyntaxTree.isNode(element) && element.kind === 'Error',
  )

const directFunctionDeclarations = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'FunctionDeclaration',
  )

const directServiceDeclarations = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ServiceDeclaration',
  )

const directTokenText = (
  result: SyntaxFile.SyntaxFile,
  node: SyntaxTree.Node,
  kind: Token.TokenKind,
): string | undefined => {
  const token = node.children.find(
    (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === kind,
  )
  if (token === undefined) return undefined
  return Array.from(Option.getOrThrow(SourceFile.slice(result.source, token.span)), (byte) =>
    String.fromCharCode(byte),
  ).join('')
}

const assertOriginalTokenTraversal = (result: SyntaxFile.SyntaxFile): void => {
  const flattened = SyntaxTree.tokens(result.root)
  assert.strictEqual(flattened.length, result.tokens.length)
  for (const [index, token] of flattened.entries()) {
    assert.strictEqual(token, result.tokens.at(index))
  }
}

it('preserves signed exponent literals and malformed exponent recovery losslessly', () => {
  for (const source of [
    'fn value() -> f64 { return -1.25e-3 }',
    'fn damaged() -> f64 { return 1.25e- }',
  ]) {
    const result = parseText('memory://float-parser.silk', source)
    const literal = descendants(result.root).find(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'FloatingLiteralExpression',
    )
    assert.isDefined(literal)
    assert.strictEqual(
      literal === undefined ? undefined : directTokenText(result, literal, 'DecimalFloat'),
      source.includes('-1.25') ? '1.25e-3' : '1.25e-',
    )
    assert.deepEqual(reconstructedBytes(result), ascii(source))
  }
})

const reconstructedBytes = (result: SyntaxFile.SyntaxFile): Uint8Array => {
  const bytes = SyntaxTree.tokens(result.root)
    .filter((token) => token.kind !== 'EndOfFile')
    .flatMap((token) => Array.from(Option.getOrThrow(SourceFile.slice(result.source, token.span))))
  return Uint8Array.from(bytes)
}

const diagnosticView = (result: SyntaxFile.SyntaxFile) =>
  result.parserDiagnostics.map((diagnostic) => ({
    code: diagnostic.code,
    start: diagnostic.span.start,
    end: diagnostic.span.end,
    reason: diagnostic.reason,
  }))

it('parses effect declarations, failure rows, delayed run, and consuming fail losslessly', () => {
  const result = parseText(
    'memory://effect.silk',
    `struct Problem { code: i32 }
effect fn work(problem: Problem) -> i32 ! Problem | Problem {
  if true { fail move problem }
  return 42
}
fn main() -> i32 { let pending = work(Problem { code: 1 }) return run pending }`,
  )
  const effect = directFunctionDeclarations(result.root).at(0)
  assert.strictEqual(
    SyntaxTree.directToken(effect ?? result.root, 'EffectKeyword')?.kind,
    'EffectKeyword',
  )
  assert.strictEqual(SyntaxTree.directNode(effect ?? result.root, 'FailureRow')?.kind, 'FailureRow')
  assert.include(
    descendants(result.root)
      .filter(SyntaxTree.isNode)
      .map((node) => node.kind),
    'FailStatement',
  )
  assert.include(
    descendants(result.root)
      .filter(SyntaxTree.isNode)
      .map((node) => node.kind),
    'RunExpression',
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assert.deepEqual(Array.from(reconstructedBytes(result)), result.source.bytes)
})

it('parses source service contracts with complete operation rows losslessly', () => {
  const source = `/// A portable logging contract.
pub service Logger<T> {
  effect fn log(message: &[u8], value: T) -> () ! WriteFailure ? &mut Logger<T>
  fn enabled() -> bool
}
fn after() -> i32 { return 1 }`
  const result = parseText('memory://service.silk', source)
  const service = directServiceDeclarations(result.root).at(0)

  assert.strictEqual(service?.kind, 'ServiceDeclaration')
  assert.strictEqual(
    SyntaxTree.directToken(service ?? result.root, 'PubKeyword')?.kind,
    'PubKeyword',
  )
  assert.strictEqual(
    SyntaxTree.directToken(service ?? result.root, 'ServiceKeyword')?.kind,
    'ServiceKeyword',
  )
  const operations =
    service === undefined ? [] : SyntaxTree.directNodes(service, 'ServiceOperation')
  assert.strictEqual(operations.length, 2)
  assert.strictEqual(
    SyntaxTree.directNode(operations[0] ?? result.root, 'FailureRow')?.kind,
    'FailureRow',
  )
  assert.strictEqual(
    SyntaxTree.directNode(operations[0] ?? result.root, 'RequirementRow')?.kind,
    'RequirementRow',
  )
  assert.strictEqual(directFunctionDeclarations(result.root).length, 1)
  assert.deepEqual(result.parserDiagnostics, [])
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses ordinary static interfaces separately from services', () => {
  const source = `pub interface Integer<T> {
  fn add(left: T, right: T) -> T
}
fn after() -> i32 { return 1 }`
  const result = parseText('memory://interface.silk', source)
  const interface_ = SyntaxTree.directNodes(result.root, 'InterfaceDeclaration').at(0)

  assert.strictEqual(interface_?.kind, 'InterfaceDeclaration')
  assert.strictEqual(
    SyntaxTree.directToken(interface_ ?? result.root, 'InterfaceKeyword')?.kind,
    'InterfaceKeyword',
  )
  assert.strictEqual(
    interface_ === undefined ? 0 : SyntaxTree.directNodes(interface_, 'ServiceOperation').length,
    1,
  )
  assert.strictEqual(directFunctionDeclarations(result.root).length, 1)
  assert.deepEqual(result.parserDiagnostics, [])
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('recovers a damaged service operation before the following operation and declaration', () => {
  const source = `service Logger {
  effect fn broken(message: &[u8] -> ()
  fn enabled() -> bool
}
fn after() -> i32 { return 1 }`
  const result = parseText('memory://damaged-service.silk', source)
  const service = directServiceDeclarations(result.root).at(0)
  const operations =
    service === undefined ? [] : SyntaxTree.directNodes(service, 'ServiceOperation')

  assert.strictEqual(operations.length, 2)
  assert.strictEqual(directFunctionDeclarations(result.root).length, 1)
  assert.include(
    missingLeaves(result.root).map((token) => token.expected),
    'RightParenthesis',
  )
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('contains invalid service storage without consuming a valid following operation', () => {
  const source = `service Broken {
  state: i32
  fn enabled() -> bool
}`
  const result = parseText('memory://service-storage.silk', source)
  const service = directServiceDeclarations(result.root).at(0)

  assert.strictEqual(
    service === undefined ? 0 : SyntaxTree.directNodes(service, 'ServiceInvalidMember').length,
    1,
  )
  assert.strictEqual(
    service === undefined ? 0 : SyntaxTree.directNodes(service, 'ServiceOperation').length,
    1,
  )
  assert.isAbove(result.parserDiagnostics.length, 0)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('recovers a missing failure-row member without consuming the effect body or next function', () => {
  const result = parseText(
    'memory://damaged-effect.silk',
    'effect fn work() -> i32 ! { return 1 } fn after() -> i32 { return 2 }',
  )
  assert.strictEqual(directFunctionDeclarations(result.root).length, 2)
  const first = directFunctionDeclarations(result.root).at(0)
  const row = first === undefined ? undefined : SyntaxTree.directNode(first, 'FailureRow')
  assert.strictEqual(row?.kind, 'FailureRow')
  assert.include(
    missingLeaves(result.root).map((token) => token.expected),
    'Identifier',
  )
})

it('parses an effect block as a primary lazy expression and retains Copy fail syntax', () => {
  const result = parseText(
    'memory://effect-expression.silk',
    'fn later() -> i32 { let pending = effect { fail Problem { code: 1 } } return 0 }',
  )
  const kinds = descendants(result.root)
    .filter(SyntaxTree.isNode)
    .map((node) => node.kind)
  assert.include(kinds, 'EffectExpression')
  const failed = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'FailStatement',
  )
  assert.strictEqual(
    failed === undefined ? undefined : SyntaxTree.directToken(failed, 'MoveKeyword'),
    undefined,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assert.deepEqual(Array.from(reconstructedBytes(result)), result.source.bytes)
})

it('bounds damaged effect-block recovery before the following declaration', () => {
  const result = parseText(
    'memory://damaged-effect-expression.silk',
    'fn make() -> i32 { let pending = effect { return broken( } return 0 } fn after() -> i32 { return 2 }',
  )
  assert.strictEqual(directFunctionDeclarations(result.root).length, 2)
  assert.include(
    descendants(result.root)
      .filter(SyntaxTree.isNode)
      .map((node) => node.kind),
    'EffectExpression',
  )
})

it('parses Effect.retry in direct and pipeline insertion forms', () => {
  const result = parseText(
    'memory://effect-retry.silk',
    `fn main() -> i32 {
  let direct = Effect.retry(work(), policy)
  let piped = work() |> Effect.retry(policy)
  return 0
}`,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  const identifiers = descendants(result.root)
    .filter(SyntaxTree.isToken)
    .filter((token) => token.kind === 'Identifier')
    .map((token) => directTokenText(result, result.root, token.kind))
  assert.isAtLeast(identifiers.length, 1)
  assert.include(
    descendants(result.root)
      .filter(SyntaxTree.isNode)
      .map((node) => node.kind),
    'PipelineExpression',
  )
})

it('parses explicit drop as a statement without making the block terminal', () => {
  const result = parseText(
    'memory://drop-statement.silk',
    'struct Token { value: i32 } fn main() -> i32 { let token = Token { value: 1 } drop token return 42 }',
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assert.include(
    descendants(result.root)
      .filter(SyntaxTree.isNode)
      .map((node) => node.kind),
    'DropStatement',
  )
})

it('parses unsafe blocks and allocator and Drop conformances losslessly', () => {
  const source = `struct Guard<T> { value: T }
impl Allocator for SystemAllocator { allocate: SystemAllocator.allocate }
impl Drop for Guard<Token> {
  fn drop(self: &mut Guard<Token>) -> () { unsafe { drop self.value } return () }
}
fn main() -> i32 { unsafe { let allocation = Allocator.allocate(Layout.make(4, 4)) drop allocation } return 42 }`
  const result = parseText('memory://unsafe-conformance.silk', source)
  assert.deepEqual(result.parserDiagnostics, [])
  const kinds = descendants(result.root)
    .filter(SyntaxTree.isNode)
    .map((node) => node.kind)
  assert.strictEqual(kinds.filter((kind) => kind === 'ImplDeclaration').length, 2)
  assert.include(kinds, 'ImplOperation')
  assert.strictEqual(kinds.filter((kind) => kind === 'UnsafeStatement').length, 2)
  assert.deepEqual(Array.from(reconstructedBytes(result)), result.source.bytes)
})

it('parses a parametric conformance losslessly', () => {
  const source = `struct Vector<T> { count: usize }
impl<T> Drop for Vector<T> {
  fn drop(self: &mut Vector<T>) -> () { return () }
}
fn main() -> i32 { return 42 }`
  const result = parseText('memory://parametric-conformance.silk', source)
  assert.deepEqual(result.parserDiagnostics, [])
  const impl = descendants(result.root)
    .filter(SyntaxTree.isNode)
    .find((node) => node.kind === 'ImplDeclaration')
  assert.isDefined(impl)
  const implKinds = descendants(impl ?? result.root)
    .filter(SyntaxTree.isNode)
    .map((node) => node.kind)
  assert.include(implKinds, 'TypeParameterList')
  assert.include(implKinds, 'TypeParameter')
  assert.deepEqual(Array.from(reconstructedBytes(result)), result.source.bytes)
})

it('parses whole-member binding patterns losslessly', () => {
  const source = `struct Empty {}
struct Full { value: i32 }
fn take(state: Empty | Full) -> i32 {
  return match move state {
    Empty nothing => 0
    Full full => 1
  }
}`
  const result = parseText('memory://binding-pattern.silk', source)
  assert.deepEqual(result.parserDiagnostics, [])
  const kinds = descendants(result.root)
    .filter(SyntaxTree.isNode)
    .map((node) => node.kind)
  assert.strictEqual(kinds.filter((kind) => kind === 'BindingPattern').length, 2)
  assert.deepEqual(Array.from(reconstructedBytes(result)), result.source.bytes)
})

it('recovers from a malformed impl type-parameter list inside the declaration', () => {
  const source =
    'impl<T Drop for Vector<T> { fn drop(self: &mut Vector<T>) -> () { return () } } fn after() -> i32 { return 7 }'
  const result = parseText('memory://damaged-parametric-conformance.silk', source)
  assert.isAbove(result.parserDiagnostics.length, 0)
  assert.strictEqual(directFunctionDeclarations(result.root).length, 1)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('bounds damaged conformance recovery before the following declaration', () => {
  const result = parseText(
    'memory://damaged-conformance.silk',
    'impl Allocator for Broken fn after() -> i32 { return 42 }',
  )
  assert.strictEqual(directFunctionDeclarations(result.root).length, 1)
  assert.include(
    missingLeaves(result.root).map((token) => token.expected),
    'LeftBrace',
  )
  assert.include(
    missingLeaves(result.root).map((token) => token.expected),
    'RightBrace',
  )
})

it('bounds a damaged unsafe call before the following statement and declaration', () => {
  const source =
    'fn main() -> i32 { unsafe { let value = Slot.take( return 42 } } fn after() -> i32 { return 7 }'
  const result = parseText('memory://damaged-unsafe-call.silk', source)
  const functions = directFunctionDeclarations(result.root)

  assert.strictEqual(functions.length, 2)
  assert.include(
    missingLeaves(functions.at(0) ?? result.root).map((token) => token.expected),
    'RightParenthesis',
  )
  assert.deepEqual(missingLeaves(functions.at(1) ?? result.root), [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses an explicit provider role in a provision pipeline', () => {
  const result = parseText(
    'memory://provider-role.silk',
    'fn main() -> i32 { let recipe = work() |> Effect.provide(&clock, @Scratch) return 0 }',
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assert.include(
    descendants(result.root)
      .filter(SyntaxTree.isNode)
      .map((node) => node.kind),
    'RoleExpression',
  )
})

it('parses explicit Effect contracts and declaration requirement rows', () => {
  const result = parseText(
    'memory://effect-contract-rows.silk',
    `fn later() -> Effect<i32 ! Problem ? &FileSystem | &mut Allocator@Scratch> {
  return effect { return 1 }
}
effect fn work() -> i32 ! Problem ? &FileSystem | &mut Allocator@Scratch { return 1 }`,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  const kinds = descendants(result.root)
    .filter(SyntaxTree.isNode)
    .map((node) => node.kind)
  assert.strictEqual(kinds.filter((kind) => kind === 'FailureRow').length, 2)
  assert.strictEqual(kinds.filter((kind) => kind === 'RequirementRow').length, 2)
  assert.strictEqual(kinds.filter((kind) => kind === 'Requirement').length, 4)
})

it('parses the accepted function into the exact first concrete node shape', () => {
  const lexical = Lexer.lex(SourceFile.make('fixture://accepted.silk', ascii(acceptedSource)))
  const result = Parser.parse(lexical)

  assert.deepEqual(nodeShape(result.root), acceptedShape)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(acceptedSource))
  assert.strictEqual(result.source, lexical.source)
  assert.strictEqual(result.tokens, lexical.tokens)
})

it('parses dense whitespace and line-comment trivia without changing the grammar nodes', () => {
  const result = parseText('fixture://dense-trivia.silk', denseTriviaSource)
  const kinds = descendants(result.root)
    .filter(SyntaxTree.isNode)
    .map((node) => node.kind)

  assert.deepEqual(kinds, [
    'FunctionDeclaration',
    'ParameterList',
    'ReturnType',
    'TypePath',
    'Block',
    'ReturnStatement',
    'IntegerLiteralExpression',
  ])
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(denseTriviaSource))
})

it('parses two declarations as separate direct branches in source order', () => {
  const result = parseText('fixture://two-functions.silk', twoFunctionSource)
  const declarations = directFunctionDeclarations(result.root)

  assert.strictEqual(declarations.length, 2)
  assert.deepEqual(
    declarations.map((declaration) => directTokenText(result, declaration, 'Identifier')),
    ['answer', 'main'],
  )
  const secondLeading = declarations.at(1)?.children.at(0)
  assert.strictEqual(
    secondLeading === undefined ? undefined : SyntaxTree.isToken(secondLeading),
    true,
  )
  if (secondLeading === undefined || !SyntaxTree.isToken(secondLeading)) return
  assert.strictEqual(secondLeading.kind, 'Whitespace')
  assert.strictEqual(Object.isFrozen(result.root), true)
  assert.strictEqual(Object.isFrozen(result.root.children), true)
  assert.strictEqual(Object.isFrozen(declarations.at(0)), true)
  assert.strictEqual(Object.isFrozen(declarations.at(1)), true)
  assert.strictEqual(Object.isFrozen(result.parserDiagnostics), true)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(twoFunctionSource))
})

it('parses three declarations without imposing a temporary source-file limit', () => {
  const result = parseText('fixture://three-functions.silk', threeFunctionSource)

  assert.deepEqual(
    directFunctionDeclarations(result.root).map((declaration) =>
      directTokenText(result, declaration, 'Identifier'),
    ),
    ['one', 'two', 'three'],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(threeFunctionSource))
})

it('parses a zero-argument call as one lossless concrete expression', () => {
  const result = parseText('fixture://valid-call.silk', validCallSource)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const call = calls.at(0)

  assert.strictEqual(calls.length, 1)
  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  assert.deepEqual(nodeShape(call), {
    kind: 'CallExpression',
    children: [
      {
        kind: 'IdentifierExpression',
        children: ['Whitespace', 'Identifier'],
      },
      { kind: 'ArgumentList', children: ['LeftParenthesis', 'RightParenthesis'] },
    ],
  })
  const callee = SyntaxTree.directNode(call, 'IdentifierExpression')
  assert.strictEqual(
    callee === undefined ? undefined : directTokenText(result, callee, 'Identifier'),
    'answer',
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(validCallSource))
})

it('retains trivia between every concrete call element', () => {
  const result = parseText('fixture://trivia-call.silk', triviaCallSource)
  const call = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )

  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  const argumentsList = call.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
  )
  assert.notStrictEqual(argumentsList, undefined)
  if (argumentsList === undefined) return
  assert.deepEqual(
    argumentsList.children.map((element) =>
      SyntaxTree.isNode(element)
        ? element.kind
        : SyntaxTree.isToken(element)
          ? element.kind
          : `Missing(${element.expected})`,
    ),
    [
      'Whitespace',
      'LineComment',
      'Whitespace',
      'LeftParenthesis',
      'Whitespace',
      'LineComment',
      'Whitespace',
      'LineComment',
      'Whitespace',
      'RightParenthesis',
    ],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(triviaCallSource))
})

it('parses empty parentheses as the unit value', () => {
  const result = parseText('fixture://missing-call-callee.silk', missingCallCalleeSource)
  const unit = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'UnitExpression',
  )

  assert.notStrictEqual(unit, undefined)
  assert.deepEqual(
    SyntaxTree.tokens(unit ?? result.root).map((token) => token.kind),
    ['Whitespace', 'LeftParenthesis', 'RightParenthesis'],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('inserts a missing call parenthesis without consuming the block brace', () => {
  const result = parseText(
    'fixture://missing-call-right-parenthesis.silk',
    missingCallRightParenthesisSource,
  )
  const call = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const block = descendants(result.root).find(
    (element): element is SyntaxTree.Node => SyntaxTree.isNode(element) && element.kind === 'Block',
  )

  assert.notStrictEqual(call, undefined)
  assert.notStrictEqual(block, undefined)
  if (call === undefined || block === undefined) return
  assert.deepEqual(
    missingLeaves(call).map((leaf) => leaf.expected),
    ['RightParenthesis'],
  )
  assert.strictEqual(
    block.children.some((element) => SyntaxTree.isToken(element) && element.kind === 'RightBrace'),
    true,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('parses decimal and identifier call arguments as ordered concrete expressions', () => {
  const literal = parseText('fixture://value-call-argument.silk', valueCallArgumentSource)
  const identifier = parseText(
    'fixture://identifier-call-argument.silk',
    identifierCallArgumentSource,
  )
  const literalArguments = descendants(literal.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
  )
  const identifierArguments = descendants(identifier.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
  )

  assert.deepEqual(nodeShape(literalArguments.at(0) ?? literal.root), {
    kind: 'ArgumentList',
    children: [
      'LeftParenthesis',
      { kind: 'IntegerLiteralExpression', children: ['DecimalInteger'] },
      'RightParenthesis',
    ],
  })
  assert.deepEqual(nodeShape(identifierArguments.at(0) ?? identifier.root), {
    kind: 'ArgumentList',
    children: [
      'LeftParenthesis',
      { kind: 'IdentifierExpression', children: ['Identifier'] },
      'RightParenthesis',
    ],
  })
  assert.deepEqual(literal.parserDiagnostics, [])
  assert.deepEqual(identifier.parserDiagnostics, [])
  assertOriginalTokenTraversal(literal)
  assertOriginalTokenTraversal(identifier)
})

it('parses nested calls as lossless argument expressions', () => {
  const result = parseText('fixture://nested-call.silk', nestedCallSource)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const outer = calls.at(0)
  const inner = calls.at(1)

  assert.strictEqual(calls.length, 2)
  assert.notStrictEqual(outer, undefined)
  assert.notStrictEqual(inner, undefined)
  if (outer === undefined || inner === undefined) return
  assert.deepEqual(nodeShape(outer), {
    kind: 'CallExpression',
    children: [
      {
        kind: 'IdentifierExpression',
        children: ['Whitespace', 'Identifier'],
      },
      {
        kind: 'ArgumentList',
        children: [
          'LeftParenthesis',
          {
            kind: 'CallExpression',
            children: [
              { kind: 'IdentifierExpression', children: ['Identifier'] },
              {
                kind: 'ArgumentList',
                children: [
                  'LeftParenthesis',
                  { kind: 'IntegerLiteralExpression', children: ['DecimalInteger'] },
                  'RightParenthesis',
                ],
              },
            ],
          },
          'RightParenthesis',
        ],
      },
    ],
  })
  assert.strictEqual(inner.span.start, nestedCallSource.lastIndexOf('identity(42)'))
  assert.strictEqual(inner.span.end, nestedCallSource.lastIndexOf('identity(42)') + 12)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(nestedCallSource))
})

it('preserves sibling nested calls and their outer comma', () => {
  const result = parseText('fixture://nested-siblings.silk', nestedSiblingCallSource)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const outerArguments = calls
    .at(0)
    ?.children.find(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
    )

  assert.strictEqual(calls.length, 3)
  assert.notStrictEqual(outerArguments, undefined)
  if (outerArguments === undefined) return
  assert.deepEqual(
    outerArguments.children.map((element) =>
      SyntaxTree.isNode(element)
        ? element.kind
        : SyntaxTree.isToken(element)
          ? element.kind
          : `Missing(${element.expected})`,
    ),
    ['LeftParenthesis', 'CallExpression', 'Comma', 'CallExpression', 'RightParenthesis'],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(nestedSiblingCallSource))
})

it('reserves the outer closing parenthesis when the inner call is damaged', () => {
  const result = parseText(
    'fixture://missing-nested-right-parenthesis.silk',
    missingNestedRightParenthesisSource,
  )
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const outer = calls.at(0)
  const inner = calls.at(1)

  assert.notStrictEqual(outer, undefined)
  assert.notStrictEqual(inner, undefined)
  if (outer === undefined || inner === undefined) return
  assert.deepEqual(
    missingLeaves(inner).map((leaf) => leaf.expected),
    ['RightParenthesis'],
  )
  assert.deepEqual(
    missingLeaves(outer).map((leaf) => leaf.expected),
    ['RightParenthesis'],
  )
  assert.strictEqual(
    outer.children
      .flatMap((element) => (SyntaxTree.isNode(element) ? element.children : []))
      .some((element) => SyntaxTree.isToken(element) && element.kind === 'RightParenthesis'),
    true,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(missingNestedRightParenthesisSource))
})

it('keeps a sibling argument after damaged nested syntax', () => {
  const result = parseText('fixture://damaged-nested-sibling.silk', damagedNestedSiblingSource)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const outerArguments = calls
    .at(0)
    ?.children.find(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
    )

  assert.strictEqual(calls.length, 3)
  assert.notStrictEqual(outerArguments, undefined)
  if (outerArguments === undefined) return
  assert.strictEqual(
    outerArguments.children.filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'CallExpression',
    ).length,
    2,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      message: diagnostic.message,
      reason: diagnostic.reason,
    })),
    [
      {
        code: 'PAR0002',
        message: 'Unexpected `:`; expected identifier',
        reason: {
          _tag: 'UnexpectedTokens',
          unexpected: ['Colon'],
          context: 'syntax',
          expected: ['identifier'],
        },
      },
    ],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(damagedNestedSiblingSource))
})

it('bounds nested recovery before the following declaration', () => {
  const result = parseText(
    'fixture://damaged-nested-before-next-function.silk',
    damagedNestedBeforeNextFunctionSource,
  )
  const declarations = directFunctionDeclarations(result.root)
  const after = declarations.at(2)

  assert.strictEqual(declarations.length, 3)
  assert.notStrictEqual(after, undefined)
  if (after === undefined) return
  assert.deepEqual(missingLeaves(after), [])
  assert.strictEqual(directTokenText(result, after, 'Identifier'), 'after')
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(damagedNestedBeforeNextFunctionSource))
})

it('parses representative deep nested calls deterministically', () => {
  const depth = 64
  const expression = `${'identity('.repeat(depth)}42${')'.repeat(depth)}`
  const source = `pub fn identity(value: i32) -> i32 { return value }\npub fn main() -> i32 { return ${expression} }`
  const first = parseText('fixture://deep-nested-call.silk', source)
  const second = parseText('fixture://deep-nested-call.silk', source)

  assert.strictEqual(
    descendants(first.root).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'CallExpression',
    ).length,
    depth,
  )
  assert.deepEqual(nodeShape(first.root), nodeShape(second.root))
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
  assertOriginalTokenTraversal(first)
  assert.deepEqual(reconstructedBytes(first), ascii(source))
})

it('parses typed parameters and bare identifier return expressions', () => {
  const identity = parseText('fixture://identity.silk', identitySource)
  const multiple = parseText('fixture://two-parameters.silk', twoParameterSource)
  const identityParameters = descendants(identity.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ParameterDeclaration',
  )
  const multipleParameters = descendants(multiple.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ParameterDeclaration',
  )

  assert.strictEqual(identityParameters.length, 1)
  assert.strictEqual(multipleParameters.length, 2)
  assert.deepEqual(nodeShape(identityParameters.at(0) ?? identity.root), {
    kind: 'ParameterDeclaration',
    children: ['Identifier', 'Colon', { kind: 'TypePath', children: ['Whitespace', 'Identifier'] }],
  })
  assert.strictEqual(
    descendants(identity.root).some(
      (element) => SyntaxTree.isNode(element) && element.kind === 'IdentifierExpression',
    ),
    true,
  )
  assert.deepEqual(identity.parserDiagnostics, [])
  assert.deepEqual(multiple.parserDiagnostics, [])
  assertOriginalTokenTraversal(identity)
  assertOriginalTokenTraversal(multiple)
})

it('recovers missing parameter types and commas without losing later syntax', () => {
  const missingType = parseText('fixture://missing-parameter-type.silk', missingParameterTypeSource)
  const missingComma = parseText(
    'fixture://missing-parameter-comma.silk',
    missingParameterCommaSource,
  )

  assert.deepEqual(
    missingLeaves(missingType.root).map((leaf) => leaf.expected),
    ['Identifier'],
  )
  assert.deepEqual(
    missingLeaves(missingComma.root).map((leaf) => leaf.expected),
    ['Comma'],
  )
  assert.strictEqual(
    descendants(missingComma.root).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'ParameterDeclaration',
    ).length,
    2,
  )
  assertOriginalTokenTraversal(missingType)
  assertOriginalTokenTraversal(missingComma)
})

it('keeps malformed arguments explicit and resumes at the next comma', () => {
  const result = parseText('fixture://malformed-call-argument.silk', malformedArgumentSource)
  const call = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )

  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  assert.deepEqual(
    errorNodes(call)
      .flatMap((node) => SyntaxTree.tokens(node))
      .map((token) => token.kind),
    ['Colon'],
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
  assert.strictEqual(
    descendants(call).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'IdentifierExpression',
    ).length,
    3,
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(malformedArgumentSource))
})

it('bounds damaged call recovery before the following function', () => {
  const result = parseText(
    'fixture://damaged-call-before-next-function.silk',
    damagedCallBeforeNextFunctionSource,
  )
  const declarations = directFunctionDeclarations(result.root)
  const first = declarations.at(0)
  const second = declarations.at(1)

  assert.notStrictEqual(first, undefined)
  assert.notStrictEqual(second, undefined)
  if (first === undefined || second === undefined) return
  assert.deepEqual(
    missingLeaves(first).map((leaf) => leaf.expected),
    ['RightParenthesis', 'RightBrace'],
  )
  assert.deepEqual(missingLeaves(second), [])
  assert.strictEqual(directTokenText(result, second, 'Identifier'), 'after')
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(damagedCallBeforeNextFunctionSource))
})

it('keeps trailing trivia with the end-of-file expectation', () => {
  const result = parseText('fixture://trailing-trivia.silk', trailingTriviaSource)
  const directTokens = result.root.children.filter(SyntaxTree.isToken)

  assert.deepEqual(
    directTokens.map((token) => token.kind),
    ['Whitespace', 'LineComment', 'Whitespace', 'EndOfFile'],
  )
  assert.strictEqual(directFunctionDeclarations(result.root).length, 1)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(trailingTriviaSource))
})

it('inserts a missing first brace without consuming the second declaration', () => {
  const result = parseText('fixture://missing-first-brace.silk', missingFirstRightBraceSource)
  const declarations = directFunctionDeclarations(result.root)
  const first = declarations.at(0)
  const second = declarations.at(1)

  assert.notStrictEqual(first, undefined)
  assert.notStrictEqual(second, undefined)
  if (first === undefined || second === undefined) return
  assert.deepEqual(
    missingLeaves(first).map((leaf) => ({
      expected: leaf.expected,
      start: leaf.span.start,
      end: leaf.span.end,
    })),
    [
      {
        expected: 'RightBrace',
        start: missingFirstRightBraceSource.indexOf('pub fn main'),
        end: missingFirstRightBraceSource.indexOf('pub fn main'),
      },
    ],
  )
  assert.deepEqual(missingLeaves(second), [])
  assert.strictEqual(directTokenText(result, second, 'Identifier'), 'main')
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(missingFirstRightBraceSource))
})

it('retains unexpected punctuation at a function boundary and parses the next declaration', () => {
  const result = parseText(
    'fixture://inter-function-punctuation.silk',
    interFunctionPunctuationSource,
  )
  const declarations = directFunctionDeclarations(result.root)
  const boundaryErrors = errorNodes(result.root)
  const second = declarations.at(1)

  assert.strictEqual(declarations.length, 2)
  assert.notStrictEqual(second, undefined)
  if (second === undefined) return
  assert.strictEqual(directTokenText(result, second, 'Identifier'), 'main')
  assert.deepEqual(missingLeaves(second), [])
  assert.strictEqual(boundaryErrors.length, 1)
  assert.deepEqual(
    boundaryErrors.flatMap((node) => SyntaxTree.tokens(node)).map((token) => token.kind),
    ['Invalid', 'Whitespace'],
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(interFunctionPunctuationSource))
})

it('inserts a missing function name before the opening parenthesis', () => {
  const result = parseText('fixture://missing-name.silk', missingNameSource)
  const missing = missingLeaves(result.root)

  assert.deepEqual(
    missing.map((leaf) => ({
      expected: leaf.expected,
      start: leaf.span.start,
      end: leaf.span.end,
    })),
    [{ expected: 'Identifier', start: 7, end: 7 }],
  )
  assert.deepEqual(diagnosticView(result), [
    {
      code: 'PAR0001',
      start: 7,
      end: 7,
      reason: { _tag: 'MissingToken', expected: 'Identifier' },
    },
  ])
  assertOriginalTokenTraversal(result)
})

it('inserts a missing right brace at end-of-file', () => {
  const result = parseText('fixture://missing-brace.silk', missingRightBraceSource)
  const missing = missingLeaves(result.root)

  assert.deepEqual(
    missing.map((leaf) => ({
      expected: leaf.expected,
      start: leaf.span.start,
      end: leaf.span.end,
    })),
    [
      {
        expected: 'RightBrace',
        start: missingRightBraceSource.length,
        end: missingRightBraceSource.length,
      },
    ],
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('groups unexpected punctuation and following trivia before the function name', () => {
  const result = parseText('fixture://unexpected.silk', unexpectedPunctuationSource)
  const errors = errorNodes(result.root)

  assert.strictEqual(errors.length, 1)
  const error = errors.at(0)
  assert.notStrictEqual(error, undefined)
  if (error === undefined) return
  assert.deepEqual(
    SyntaxTree.tokens(error).map((token) => token.kind),
    ['Invalid', 'Whitespace'],
  )
  assert.deepEqual(diagnosticView(result), [
    {
      code: 'PAR0002',
      start: 7,
      end: 9,
      reason: {
        _tag: 'UnexpectedTokens',
        unexpected: ['Invalid', 'Whitespace'],
        context: 'syntax',
        expected: ['identifier'],
      },
    },
  ])
  assert.deepEqual(
    result.lexicalDiagnostics.map((diagnostic) => diagnostic.code),
    ['LEX0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('parses empty input as a module containing only end-of-file', () => {
  const result = parseBytes('fixture://empty.silk', emptySource)

  assert.deepEqual(
    result.root.children.map((element) => element._tag),
    ['Token'],
  )
  assert.strictEqual(SyntaxTree.directNodes(result.root, 'FunctionDeclaration').length, 0)
  assert.deepEqual(missingLeaves(result.root), [])
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('reports one diagnostic for an incomplete declaration prefix at end-of-file', () => {
  const result = parseText('fixture://incomplete-pub.silk', 'pub')

  assert.deepEqual(
    missingLeaves(result.root).map((leaf) => leaf.expected),
    ['FnKeyword', 'Identifier', 'LeftParenthesis', 'RightParenthesis', 'LeftBrace', 'RightBrace'],
  )
  assert.deepEqual(
    result.parserDiagnostics.map(({ code, message }) => ({ code, message })),
    [{ code: 'PAR0001', message: 'Expected `fn`' }],
  )
  assertOriginalTokenTraversal(result)
})

it('reports a later independent mistake after consuming a recovery anchor', () => {
  const result = parseText('fixture://synchronized-recovery.silk', 'pub () -> i32 { return }')

  assert.deepEqual(
    missingLeaves(result.root).map((leaf) => leaf.expected),
    ['FnKeyword', 'Identifier'],
  )
  assert.deepEqual(
    result.parserDiagnostics.map(({ code, message }) => ({ code, message })),
    [{ code: 'PAR0001', message: 'Expected `fn`' }],
  )
  assertOriginalTokenTraversal(result)
})

it('terminates on wholly unrelated input and retains it in one error region', () => {
  const result = parseText('fixture://unrelated.silk', whollyUnrelatedSource)
  const errors = errorNodes(result.root)

  assert.strictEqual(errors.length, 1)
  assert.strictEqual(errors.at(0)?.span.start, 0)
  assert.strictEqual(errors.at(0)?.span.end, whollyUnrelatedSource.length)
  assert.strictEqual(missingLeaves(result.root).length, 6)
  assert.strictEqual(result.parserDiagnostics.at(0)?.code, 'PAR0002')
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(whollyUnrelatedSource))
})

it('retains invalid UTF-8 bytes and lexical diagnostics inside concrete recovery', () => {
  const result = parseBytes('fixture://invalid-utf8.silk', invalidUtf8Source)
  const errors = errorNodes(result.root)

  assert.deepEqual(
    result.lexicalDiagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    [{ code: 'LEX0001', start: 7, end: 9 }],
  )
  assert.deepEqual(
    errors.flatMap((node) => SyntaxTree.tokens(node)).map((token: Token.Token) => token.kind),
    ['Invalid', 'Whitespace'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), invalidUtf8Source)
})

it('is deterministic across repeated fresh lexical results', () => {
  const first = parseText('fixture://deterministic.silk', interFunctionPunctuationSource)
  const second = parseText('fixture://deterministic.silk', interFunctionPunctuationSource)

  assert.deepEqual(nodeShape(first.root), nodeShape(second.root))
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
  assert.deepEqual(first.lexicalDiagnostics, second.lexicalDiagnostics)
  assert.deepEqual(reconstructedBytes(first), reconstructedBytes(second))
})

it('parses import declarations before functions as separate lossless branches', () => {
  const result = parseText(
    'fixture://imports.silk',
    'import math\nimport io\npub fn main() -> i32 { return 42 }',
  )
  const kinds = result.root.children.flatMap((element) =>
    SyntaxTree.isNode(element) ? [element.kind] : [],
  )

  assert.deepEqual(kinds, ['ImportDeclaration', 'ImportDeclaration', 'FunctionDeclaration'])
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(
    reconstructedBytes(result),
    ascii('import math\nimport io\npub fn main() -> i32 { return 42 }'),
  )
})

it('recovers a missing import name and keeps the following function parseable', () => {
  const result = parseText(
    'fixture://missing-import-name.silk',
    'import\npub fn main() -> i32 { return 42 }',
  )
  const importNode = result.root.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ImportDeclaration',
  )
  const functionNode = result.root.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'FunctionDeclaration',
  )

  assert.notStrictEqual(importNode, undefined)
  assert.notStrictEqual(functionNode, undefined)
  assert.strictEqual(
    SyntaxTree.directNode(importNode ?? result.root, 'ImportPath')?.children.some(
      (element) => SyntaxTree.isMissingToken(element) && element.expected === 'Identifier',
    ),
    true,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('keeps import as a keyword only when spelled completely', () => {
  const lexical = Lexer.lex(
    SourceFile.make('fixture://import-keyword.silk', ascii('import importer')),
  )

  assert.deepEqual(
    lexical.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['ImportKeyword', 'Identifier', 'EndOfFile'],
  )
})

it('parses namespace, selective, member-alias, and hybrid imports losslessly', () => {
  const source = `import compiler.Syntax
import compiler.Tree as Ast
import compiler.Parse { Node, parse, encode as encodeSyntax }
import compiler.Hir as Ir { lower, inspect as show }
pub fn main() -> i32 { return 42 }`
  const result = parseText('fixture://full-imports.silk', source)
  const imports = SyntaxTree.directNodes(result.root, 'ImportDeclaration')
  assert.strictEqual(imports.length, 4)
  assert.deepEqual(
    imports.map((node) => {
      const list = SyntaxTree.directNode(node, 'ImportMemberList')
      return list === undefined ? 0 : SyntaxTree.directNodes(list, 'ImportMember').length
    }),
    [0, 0, 3, 2],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('parses private functions without fabricating a public modifier', () => {
  const result = parseText('fixture://private.silk', 'fn helper() -> i32 { return 42 }')
  const declaration = SyntaxTree.directNode(result.root, 'FunctionDeclaration')
  assert.notStrictEqual(declaration, undefined)
  assert.strictEqual(
    declaration === undefined ? undefined : SyntaxTree.directToken(declaration, 'PubKeyword'),
    undefined,
  )
  assert.deepEqual(result.parserDiagnostics, [])
})

it('bounds import recovery and preserves the following declaration', () => {
  const cases = [
    'import compiler. as Tree\nfn helper() -> i32 { return 1 }',
    'import compiler.Syntax as\nfn helper() -> i32 { return 1 }',
    'import compiler.Syntax { Node, , parse }\nfn helper() -> i32 { return 1 }',
    'import compiler.Syntax { Node parse }\nfn helper() -> i32 { return 1 }',
    'import compiler.Syntax { Node\nfn helper() -> i32 { return 1 }',
  ]
  for (const [ordinal, source] of cases.entries()) {
    const result = parseText(`fixture://damaged-import-${ordinal}.silk`, source)
    assert.strictEqual(SyntaxTree.directNodes(result.root, 'ImportDeclaration').length, 1)
    assert.strictEqual(SyntaxTree.directNodes(result.root, 'FunctionDeclaration').length, 1)
    assert.isAtLeast(result.parserDiagnostics.length, 1)
    assertOriginalTokenTraversal(result)
  }
})

it('parses a binding sequence as ordered statement branches', () => {
  const result = parseText(
    'fixture://bindings.silk',
    'pub fn main() -> i32 { let value = 42 return value }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  assert.notStrictEqual(block, undefined)
  if (block === undefined) return

  const statements = block.children.filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['BindingStatement', 'ReturnStatement'])
  const binding = SyntaxTree.directNode(block, 'BindingStatement')
  assert.notStrictEqual(binding, undefined)
  if (binding === undefined) return
  assert.strictEqual(directTokenText(result, binding, 'Identifier'), 'value')
  assert.notStrictEqual(SyntaxTree.directToken(binding, 'Equals'), undefined)
  assert.notStrictEqual(SyntaxTree.directNode(binding, 'IntegerLiteralExpression'), undefined)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(
    reconstructedBytes(result),
    ascii('pub fn main() -> i32 { let value = 42 return value }'),
  )
})

it('parses standalone expressions as ordered statements without phantom declarations', () => {
  const source = `pub effect fn first() -> () { return () }
pub effect fn second() -> () { return () }
pub effect fn main() -> () {
  run first()
  run second()
  return ()
}`
  const result = parseText('memory/expression-statements', source)
  const declarations = directFunctionDeclarations(result.root)
  const main = declarations.at(2)
  const block = main === undefined ? undefined : SyntaxTree.directNode(main, 'Block')

  assert.strictEqual(declarations.length, 3)
  assert.notStrictEqual(block, undefined)
  assert.deepEqual(
    (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind),
    ['ExpressionStatement', 'ExpressionStatement', 'ReturnStatement'],
  )
  assert.strictEqual(
    (block === undefined ? [] : SyntaxTree.directNodes(block, 'ExpressionStatement')).every(
      (statement) => SyntaxTree.directNode(statement, 'RunExpression') !== undefined,
    ),
    true,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('keeps assignment dispatch ahead of identifier-led expression statements', () => {
  const source = `fn observe(value: i32) -> () { return () }
fn main() -> () {
  let mut value = 0
  value = 1
  observe(value)
}`
  const result = parseText('memory/expression-statement-assignment', source)
  const main = directFunctionDeclarations(result.root).at(1)
  const block = main === undefined ? undefined : SyntaxTree.directNode(main, 'Block')

  assert.deepEqual(
    (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind),
    ['BindingStatement', 'AssignmentStatement', 'ExpressionStatement', 'ReturnStatement'],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('keeps a valid return inside its block after a damaged run expression', () => {
  const source = `pub effect fn main() -> () {
  run )
  return ()
}`
  const result = parseText('memory/damaged-expression-statement', source)
  const declarations = directFunctionDeclarations(result.root)
  const block = SyntaxTree.directNode(declarations[0] ?? result.root, 'Block')

  assert.strictEqual(declarations.length, 1)
  assert.deepEqual(
    (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind),
    ['ExpressionStatement', 'ErrorStatement', 'ReturnStatement'],
  )
  assert.strictEqual(result.parserDiagnostics.length, 1)
  assertOriginalTokenTraversal(result)
})

it('recovers unexpected statement punctuation at the next statement and owning brace', () => {
  const source = `pub fn main() -> () {
  ;
  return ()
}`
  const result = parseText('memory/unexpected-statement-punctuation', source)
  const declarations = directFunctionDeclarations(result.root)
  const block = SyntaxTree.directNode(declarations[0] ?? result.root, 'Block')

  assert.strictEqual(declarations.length, 1)
  assert.deepEqual(
    (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind),
    ['ErrorStatement', 'ReturnStatement'],
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
  assertOriginalTokenTraversal(result)
})

it('parses a move operand with its keyword and name', () => {
  const result = parseText(
    'fixture://move.silk',
    'pub fn main() -> i32 { let value = 42 return move value }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const returnStatement =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ReturnStatement')
  const move =
    returnStatement === undefined
      ? undefined
      : SyntaxTree.directNode(returnStatement, 'MoveExpression')

  assert.notStrictEqual(move, undefined)
  if (move === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(move, 'MoveKeyword'), undefined)
  const subject = SyntaxTree.directNode(move, 'IdentifierExpression')
  assert.notStrictEqual(subject, undefined)
  if (subject === undefined) return
  assert.strictEqual(directTokenText(result, subject, 'Identifier'), 'value')
  assert.deepEqual(result.parserDiagnostics, [])
})

it('recovers a missing initializer at the return boundary', () => {
  const result = parseText(
    'fixture://missing-initializer.silk',
    'pub fn main() -> i32 { let value = return 42 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const binding = block === undefined ? undefined : SyntaxTree.directNode(block, 'BindingStatement')

  assert.notStrictEqual(binding, undefined)
  if (binding === undefined) return
  assert.strictEqual(missingLeaves(binding).length, 1)
  const returnStatement =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ReturnStatement')
  assert.notStrictEqual(returnStatement, undefined)
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('recovers a missing binding name before the equals token', () => {
  const result = parseText(
    'fixture://missing-binding-name.silk',
    'pub fn main() -> i32 { let = 42 return 0 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const binding = block === undefined ? undefined : SyntaxTree.directNode(block, 'BindingStatement')

  assert.notStrictEqual(binding, undefined)
  if (binding === undefined) return
  assert.strictEqual(
    binding.children.some(
      (element) => SyntaxTree.isMissingToken(element) && element.expected === 'Identifier',
    ),
    true,
  )
  const statements = (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['BindingStatement', 'ReturnStatement'])
  assertOriginalTokenTraversal(result)
})

it('recovers a block with only bindings by inserting the missing return', () => {
  const result = parseText(
    'fixture://missing-return.silk',
    'pub fn main() -> i32 { let value = 42 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')

  assert.notStrictEqual(block, undefined)
  if (block === undefined) return
  const statements = block.children.filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['BindingStatement', 'ReturnStatement'])
  const returnStatement = SyntaxTree.directNode(block, 'ReturnStatement')
  assert.strictEqual(
    returnStatement?.children.some(
      (element) => SyntaxTree.isMissingToken(element) && element.expected === 'ReturnKeyword',
    ),
    true,
  )
  assert.deepEqual(
    missingLeaves(returnStatement ?? block).map((element) => element.expected),
    ['ReturnKeyword', 'DecimalInteger'],
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      message: diagnostic.message,
    })),
    [{ code: 'PAR0004', message: 'Expected return statement' }],
  )
  assertOriginalTokenTraversal(result)
})

it('keeps a final identifier as an expression statement before the missing return', () => {
  const result = parseText('fixture://missing-return-keyword.silk', 'pub fn main() -> i32 { foo }')
  const declaration = directFunctionDeclarations(result.root).at(0)
  const block = declaration === undefined ? undefined : SyntaxTree.directNode(declaration, 'Block')
  const returned = block === undefined ? undefined : SyntaxTree.directNode(block, 'ReturnStatement')
  const expression =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ExpressionStatement')

  assert.notStrictEqual(returned, undefined)
  assert.notStrictEqual(expression, undefined)
  if (returned === undefined || expression === undefined) return
  assert.strictEqual(SyntaxTree.directNode(block ?? result.root, 'AssignmentStatement'), undefined)
  assert.notStrictEqual(SyntaxTree.directNode(expression, 'IdentifierExpression'), undefined)
  assert.deepEqual(
    missingLeaves(returned).map((element) => element.expected),
    ['ReturnKeyword', 'DecimalInteger'],
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      message: diagnostic.message,
    })),
    [{ code: 'PAR0004', message: 'Expected return statement' }],
  )
  assertOriginalTokenTraversal(result)
})

it('recovers a bare move with a missing identifier', () => {
  const result = parseText(
    'fixture://bare-move.silk',
    'pub fn main() -> i32 { let value = move return 0 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const binding = block === undefined ? undefined : SyntaxTree.directNode(block, 'BindingStatement')
  const move = binding === undefined ? undefined : SyntaxTree.directNode(binding, 'MoveExpression')

  assert.notStrictEqual(move, undefined)
  if (move === undefined) return
  assert.strictEqual(
    missingLeaves(move).some((element) => element.expected === 'Identifier'),
    true,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('keeps statements after the return statement as concrete branches', () => {
  const result = parseText(
    'fixture://trailing-statement.silk',
    'pub fn main() -> i32 { return 0 let late = 1 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')

  assert.notStrictEqual(block, undefined)
  if (block === undefined) return
  const statements = block.children.filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['ReturnStatement', 'BindingStatement'])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(
    reconstructedBytes(result),
    ascii('pub fn main() -> i32 { return 0 let late = 1 }'),
  )
})

it('parses signed literals and qualified callees', () => {
  const result = parseText(
    'fixture://arith.silk',
    'pub fn main() -> i32 { return i32.add(-8, 50) }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const returnStatement =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ReturnStatement')
  const call =
    returnStatement === undefined
      ? undefined
      : SyntaxTree.directNode(returnStatement, 'CallExpression')

  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  const callee = SyntaxTree.directNode(call, 'FieldProjectionExpression')
  assert.notStrictEqual(callee, undefined)
  if (callee === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(callee, 'Dot'), undefined)
  assert.strictEqual(
    SyntaxTree.tokens(callee).filter((token) => token.kind === 'Identifier').length,
    2,
  )
  const argumentList = SyntaxTree.directNode(call, 'ArgumentList')
  const firstArgument =
    argumentList === undefined
      ? undefined
      : SyntaxTree.directNode(argumentList, 'IntegerLiteralExpression')
  assert.notStrictEqual(firstArgument, undefined)
  if (firstArgument === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(firstArgument, 'Minus'), undefined)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('recovers a missing operation name after the dot', () => {
  const result = parseText(
    'fixture://missing-operation.silk',
    'pub fn main() -> i32 { return i32.(1, 2) }',
  )

  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('recovers a dangling minus before the closing brace', () => {
  const result = parseText('fixture://dangling-minus.silk', 'pub fn main() -> i32 { return - }')

  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('parses conditionals with both arms and boolean literals', () => {
  const result = parseText(
    'fixture://conditional.silk',
    'pub fn main() -> i32 { if flag { return 1 } else { return 2 } return 0 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const conditional =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ConditionalStatement')

  assert.notStrictEqual(conditional, undefined)
  if (conditional === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(conditional, 'IfKeyword'), undefined)
  assert.notStrictEqual(SyntaxTree.directToken(conditional, 'ElseKeyword'), undefined)
  assert.strictEqual(SyntaxTree.directNodes(conditional, 'Block').length, 2)
  assert.notStrictEqual(SyntaxTree.directNode(conditional, 'IdentifierExpression'), undefined)
  const statements = (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['ConditionalStatement', 'ReturnStatement'])
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)

  const booleans = parseText(
    'fixture://booleans.silk',
    'pub fn main() -> i32 { let flag = false return true }',
  )
  const boolFn = directFunctionDeclarations(booleans.root).at(0)
  const boolBlock = boolFn === undefined ? undefined : SyntaxTree.directNode(boolFn, 'Block')
  const binding =
    boolBlock === undefined ? undefined : SyntaxTree.directNode(boolBlock, 'BindingStatement')
  assert.notStrictEqual(
    binding === undefined ? undefined : SyntaxTree.directNode(binding, 'BooleanLiteralExpression'),
    undefined,
  )
  assert.deepEqual(booleans.parserDiagnostics, [])
})

it('parses a chain of three conditions as nested conditional statements', () => {
  const result = parseText(
    'fixture://else-if-chain.silk',
    'pub fn main() -> i32 { if first { return 1 } else if second { return 2 } else if third { return 3 } else { return 4 } return 0 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const outer =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ConditionalStatement')

  assert.notStrictEqual(outer, undefined)
  if (outer === undefined) return
  const statements = (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['ConditionalStatement', 'ReturnStatement'])

  // Each chained arm is the same node kind nested one level deeper, so no new
  // node kind carries the chain and the tree stays lossless.
  const middle = SyntaxTree.directNode(outer, 'ConditionalStatement')
  assert.notStrictEqual(middle, undefined)
  if (middle === undefined) return
  const inner = SyntaxTree.directNode(middle, 'ConditionalStatement')
  assert.notStrictEqual(inner, undefined)
  if (inner === undefined) return
  assert.strictEqual(SyntaxTree.directNode(inner, 'ConditionalStatement'), undefined)

  for (const conditional of [outer, middle, inner]) {
    assert.notStrictEqual(SyntaxTree.directToken(conditional, 'IfKeyword'), undefined)
    assert.notStrictEqual(SyntaxTree.directToken(conditional, 'ElseKeyword'), undefined)
    assert.notStrictEqual(SyntaxTree.directNode(conditional, 'IdentifierExpression'), undefined)
  }

  // A chained arm keeps only its taken block; the final arm keeps both blocks.
  assert.strictEqual(SyntaxTree.directNodes(outer, 'Block').length, 1)
  assert.strictEqual(SyntaxTree.directNodes(middle, 'Block').length, 1)
  assert.strictEqual(SyntaxTree.directNodes(inner, 'Block').length, 2)

  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('keeps an else arm that is not a chained if anchored to its brace', () => {
  const result = parseText(
    'fixture://else-not-if.silk',
    'pub fn main() -> i32 { if first { return 1 } else while second { } return 0 }',
  )

  assert.include(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    'PAR0001',
  )
  assertOriginalTokenTraversal(result)
})

it('recovers a missing condition before the arm brace', () => {
  const result = parseText(
    'fixture://missing-condition.silk',
    'pub fn main() -> i32 { if { return 1 } return 0 }',
  )

  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('recovers an arm missing its closing brace before the trailing return', () => {
  const result = parseText(
    'fixture://missing-arm-brace.silk',
    'pub fn main() -> i32 { if flag { return 1 return 0 }',
  )

  assert.strictEqual(
    result.parserDiagnostics.every(
      (diagnostic) => diagnostic.code === 'PAR0001' || diagnostic.code === 'PAR0004',
    ),
    true,
  )
  assertOriginalTokenTraversal(result)
})

it('parses arithmetic and equality by the closed precedence table', () => {
  const source = 'pub fn main() -> bool { return 1 + 2 * 3 == 7 }'
  const result = parseText('memory/operators-precedence', source)
  const expressions = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'InfixExpression',
  )
  const [equality, addition, multiplication] = expressions

  assert.strictEqual(expressions.length, 3)
  assert.notStrictEqual(equality, undefined)
  assert.notStrictEqual(addition, undefined)
  assert.notStrictEqual(multiplication, undefined)
  if (equality === undefined || addition === undefined || multiplication === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(equality, 'EqualEqual'), undefined)
  assert.notStrictEqual(SyntaxTree.directToken(addition, 'Plus'), undefined)
  assert.notStrictEqual(SyntaxTree.directToken(multiplication, 'Star'), undefined)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('reserves template element and fragment starts without consuming following syntax', () => {
  const source =
    'pub fn element() -> i32 { return <Button /> return 1 }\n' +
    'pub fn fragment() -> i32 { return <> return 2 }\n' +
    'pub fn after() -> i32 { return 3 }'
  const result = parseText('memory/reserved-template-starts', source)
  const declarations = directFunctionDeclarations(result.root)

  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      reason: diagnostic.reason,
    })),
    [
      { code: 'PAR0003', reason: { _tag: 'ReservedTemplateSyntax' } },
      { code: 'PAR0003', reason: { _tag: 'ReservedTemplateSyntax' } },
    ],
  )
  assert.strictEqual(declarations.length, 3)
  assert.deepEqual(
    declarations.map((declaration) => directTokenText(result, declaration, 'Identifier')),
    ['element', 'fragment', 'after'],
  )
  assert.deepEqual(missingLeaves(declarations.at(-1) ?? result.root), [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('preserves every relational operator after an existing left operand', () => {
  const source =
    'fn less() -> bool { return 1 < 2 }\n' +
    'fn lessEqual() -> bool { return 1 <= 2 }\n' +
    'fn greater() -> bool { return 2 > 1 }\n' +
    'fn greaterEqual() -> bool { return 2 >= 1 }'
  const result = parseText('memory/relational-reservation-boundary', source)
  const expressions = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'InfixExpression',
  )
  const relationalKinds: ReadonlyArray<Token.TokenKind> = [
    'Less',
    'LessEqual',
    'Greater',
    'GreaterEqual',
  ]

  assert.deepEqual(
    expressions.flatMap((expression) =>
      relationalKinds.filter((kind) => SyntaxTree.directToken(expression, kind) !== undefined),
    ),
    ['Less', 'LessEqual', 'Greater', 'GreaterEqual'],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('parses grouping and right-associative prefix expressions losslessly', () => {
  const source = 'pub fn main(value: i32) -> i32 { return -(-(value + 1)) }'
  const result = parseText('memory/operator-prefix', source)
  const prefixes = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'PrefixExpression',
  )
  const groups = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'GroupedExpression',
  )

  assert.strictEqual(prefixes.length, 2)
  assert.strictEqual(groups.length, 2)
  assert.strictEqual(
    prefixes.every((node) => SyntaxTree.directToken(node, 'Minus') !== undefined),
    true,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses complete callable pipelines left-to-right', () => {
  const source =
    'pub fn main() -> i32 { return 2 |> i32.add(3) |> i32.multiply(4) }\n' +
    'pub fn flag() -> bool { return true |> bool.not }\n' +
    'pub fn recover() -> i32 { return risky() |> Effect.catch(handler) }'
  const result = parseText('memory/operator-pipelines', source)
  const pipelines = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'PipelineExpression',
  )
  const targets = pipelines.map((pipeline) =>
    pipeline.children
      .filter((element): element is SyntaxTree.Node => SyntaxTree.isNode(element))
      .at(1),
  )

  assert.strictEqual(pipelines.length, 4)
  assert.strictEqual(targets.length, 4)
  assert.deepEqual(
    targets.map((target) =>
      target === undefined ? 0 : SyntaxTree.directNodes(target, 'ArgumentList').length,
    ),
    [1, 1, 0, 1],
  )
  assert.deepEqual(
    targets.map((target) =>
      target === undefined ? 0 : SyntaxTree.directNodes(target, 'CallTypeArgumentList').length,
    ),
    [0, 0, 0, 0],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses callable contracts with ordered parameters and all invocation modes', () => {
  const source =
    'fn use(shared: fn(i32, bool) -> i32, exclusive: mut fn(i32) -> bool, consuming: once fn() -> i32) -> i32 { return 0 }'
  const result = parseText('memory/callable-types', source)
  const callableTypes = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallableType',
  )

  assert.strictEqual(callableTypes.length, 3)
  assert.deepEqual(
    callableTypes.map((type) => ({
      mut: SyntaxTree.directToken(type, 'MutKeyword') !== undefined,
      once: SyntaxTree.directToken(type, 'OnceKeyword') !== undefined,
      parameters: type.children.filter((element) => SyntaxTree.isNode(element)).length - 1,
    })),
    [
      { mut: false, once: false, parameters: 2 },
      { mut: true, once: false, parameters: 1 },
      { mut: false, once: true, parameters: 0 },
    ],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('parses reusable, exclusive, and take-capable Effect access bounds', () => {
  const source =
    'fn use(shared: Effect<i32>, exclusive: mut Effect<i32>, any: once Effect<i32>) -> i32 { return 0 }'
  const result = parseText('memory/effect-access-bounds', source)
  const effects = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'AppliedType',
  )

  assert.deepEqual(
    effects.map((effect) => ({
      mut: SyntaxTree.directToken(effect, 'MutKeyword') !== undefined,
      once: SyntaxTree.directToken(effect, 'OnceKeyword') !== undefined,
    })),
    [
      { mut: false, once: false },
      { mut: true, once: false },
      { mut: false, once: true },
    ],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('recovers every callable type boundary without crossing the next declaration', () => {
  const cases = [
    ['mut (i32) -> i32', 'FnKeyword'],
    ['once fn i32) -> i32', 'LeftParenthesis'],
    ['fn(i32 -> i32', 'RightParenthesis'],
    ['fn(i32) i32', 'Arrow'],
    ['fn(i32) ->', 'Identifier'],
  ] as const

  for (const [damaged, expected] of cases) {
    const source = `fn damaged(callback: ${damaged}) -> i32 { return 0 } fn after() -> i32 { return 1 }`
    const result = parseText(`memory/callable-recovery-${expected}`, source)
    const first = directFunctionDeclarations(result.root).at(0)
    const second = directFunctionDeclarations(result.root).at(1)
    assert.notStrictEqual(first, undefined)
    assert.notStrictEqual(second, undefined)
    assert.strictEqual(
      first === undefined ? false : missingLeaves(first).some((leaf) => leaf.expected === expected),
      true,
    )
    assert.strictEqual(
      second === undefined ? undefined : SyntaxTree.isAvailableSyntax(second),
      true,
    )
    assertOriginalTokenTraversal(result)
  }
})

it('parses repeated postfix application over every callable-producing expression', () => {
  const source = `fn use(operation: fn(i32) -> i32, value: i32) -> i32 {
  let bound = i32.add(2)
  let named = operation(value)
  let qualified = i32.add(2)(value)
  let grouped = (operation)(value)
  return choose(true)(value)
}`
  const result = parseText('memory/postfix-callables', source)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )

  assert.strictEqual(calls.length, 7)
  assert.strictEqual(
    calls.some((call) => SyntaxTree.directNode(call, 'CallExpression') !== undefined),
    true,
  )
  assert.strictEqual(
    calls.some((call) => SyntaxTree.directNode(call, 'GroupedExpression') !== undefined),
    true,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('gives pipelines a complete callable expression and run the complete following expression', () => {
  const source = `fn use(attempt: Effect<i32>, operation: fn(i32) -> i32) -> i32 {
  let a = 1 |> operation
  let b = 2 |> (operation)
  let c = 3 |> choose(true)
  let d = run attempt |> Effect.retry(2)
  let e = (run attempt) |> operation
  return run run nested
}`
  const result = parseText('memory/callable-pipe-run', source)
  const bindings = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'BindingStatement',
  )
  const d = bindings.at(3)
  const e = bindings.at(4)
  const dRun = d === undefined ? undefined : SyntaxTree.directNode(d, 'RunExpression')
  const ePipeline = e === undefined ? undefined : SyntaxTree.directNode(e, 'PipelineExpression')

  assert.notStrictEqual(dRun, undefined)
  assert.notStrictEqual(
    dRun === undefined ? undefined : SyntaxTree.directNode(dRun, 'PipelineExpression'),
    undefined,
  )
  assert.notStrictEqual(ePipeline, undefined)
  assert.notStrictEqual(
    ePipeline === undefined ? undefined : SyntaxTree.directNode(ePipeline, 'GroupedExpression'),
    undefined,
  )
  assert.strictEqual(
    descendants(result.root).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'RunExpression',
    ).length,
    4,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('bounds run at commas, delimiters, blocks, and following statements', () => {
  const source = `fn comma(first: Effect<i32>, second: i32) -> i32 { return pair(run first, second) }
fn delimiter(first: Effect<i32>) -> i32 { return (run first) }
fn block(first: Effect<i32>) -> i32 { if true { return run first } return 0 }
fn statement(first: Effect<i32>) -> i32 { let value = run first return value }`
  const result = parseText('memory/run-boundaries', source)

  assert.strictEqual(directFunctionDeclarations(result.root).length, 4)
  assert.strictEqual(
    descendants(result.root).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'RunExpression',
    ).length,
    4,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('bounds operator recovery at expression and declaration boundaries', () => {
  const source =
    'pub fn missingOperand() -> i32 { return 1 + }\n' +
    'pub fn missingGroup() -> i32 { return (1 + 2 }\n' +
    'pub fn chained() -> bool { return 1 < 2 < 3 }\n' +
    'pub fn after() -> i32 { return 4 }'
  const result = parseText('memory/operator-recovery', source)
  const declarations = directFunctionDeclarations(result.root)
  const after = declarations.at(-1)

  assert.strictEqual(declarations.length, 4)
  assert.strictEqual(
    missingLeaves(result.root).some((leaf) => leaf.expected === 'DecimalInteger'),
    true,
  )
  assert.strictEqual(
    missingLeaves(result.root).some((leaf) => leaf.expected === 'RightParenthesis'),
    true,
  )
  assert.strictEqual(errorNodes(result.root).length > 0, true)
  assert.notStrictEqual(after, undefined)
  if (after === undefined) return
  assert.strictEqual(directTokenText(result, after, 'Identifier'), 'after')
  assert.deepEqual(missingLeaves(after), [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses nominal struct declarations and qualified field types losslessly', () => {
  const source = validStructSource
  const result = parseText('memory/structs', source)
  const structs = SyntaxTree.directNodes(result.root, 'StructDeclaration')
  const fields = structs.flatMap((struct) => SyntaxTree.directNodes(struct, 'StructField'))

  assert.strictEqual(structs.length, 2)
  assert.strictEqual(fields.length, 2)
  assert.deepEqual(
    fields.map((field) =>
      SyntaxTree.directNode(field, 'TypePath')
        ?.children.filter(SyntaxTree.isToken)
        .map((token) => token.kind),
    ),
    [
      ['Whitespace', 'Identifier'],
      ['Whitespace', 'Identifier', 'Dot', 'Identifier'],
    ],
  )
  assert.notStrictEqual(
    SyntaxTree.directToken(structs.at(0) ?? result.root, 'PubKeyword'),
    undefined,
  )
  assert.strictEqual(SyntaxTree.directNodes(structs.at(1) ?? result.root, 'StructField').length, 0)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('keeps damaged struct fields and following declarations separate', () => {
  const source = damagedStructSource
  const result = parseText('memory/damaged-structs', source)
  const structs = SyntaxTree.directNodes(result.root, 'StructDeclaration')
  const after = SyntaxTree.directNode(result.root, 'FunctionDeclaration')

  assert.strictEqual(structs.length, 2)
  assert.notStrictEqual(after, undefined)
  assert.deepEqual(
    missingLeaves(structs.at(0) ?? result.root).map((leaf) => leaf.expected),
    ['Identifier', 'Identifier'],
  )
  assert.deepEqual(
    missingLeaves(structs.at(1) ?? result.root).map((leaf) => leaf.expected),
    ['RightBrace'],
  )
  assert.deepEqual(after === undefined ? [] : missingLeaves(after), [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses mode-aware match expressions and nested patterns losslessly', () => {
  const source = `pub struct Span { start: i32 end: i32 }
pub struct Token { kind: i32 span: Span }
pub struct End {}
pub fn inspect(event: Token | End) -> i32 {
  let code = match move event {
    Token { kind, span: Span { start: offset, .. } } if true => offset
    End {} => 0
  }
  return code
}`
  const result = parseText('memory/match-expression', source)
  const match = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'MatchExpression',
  )

  assert.notStrictEqual(match, undefined)
  if (match === undefined) return
  assert.strictEqual(SyntaxTree.directNodes(match, 'MatchAccess').length, 1)
  assert.strictEqual(SyntaxTree.directNodes(match, 'MatchArm').length, 2)
  assert.strictEqual(
    descendants(match).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'NominalPattern',
    ).length,
    3,
  )
  assert.strictEqual(
    descendants(match).some(
      (element) => SyntaxTree.isNode(element) && element.kind === 'RestPattern',
    ),
    true,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses bare, shared, and exclusive matches in expression positions', () => {
  const source = `pub struct Token { kind: i32 }
pub fn bare(event: Token) -> i32 { return match event { Token { kind } => kind } }
pub fn shared(event: Token) -> i32 { return i32.add(match &event { Token { kind } => kind }, 1) }
pub fn exclusive(event: Token) -> i32 { let value = match &mut event { _ => 0 } return value }`
  const result = parseText('memory/match-modes', source)
  const matches = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'MatchExpression',
  )

  assert.strictEqual(matches.length, 3)
  assert.deepEqual(
    matches.map((match) =>
      SyntaxTree.directNode(match, 'MatchAccess')
        ?.children.filter(SyntaxTree.isToken)
        .map((token) => token.kind),
    ),
    [[], ['Whitespace', 'Ampersand'], ['Whitespace', 'Ampersand', 'MutKeyword']],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('keeps a missing match arm arrow local to its arm', () => {
  const source = `pub struct Token { kind: i32 }
pub struct End {}
pub fn inspect(event: Token | End) -> i32 {
  return match event {
    Token { kind } kind
    End {} => 0
  }
}`
  const result = parseText('memory/damaged-match-arm', source)
  const match = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'MatchExpression',
  )

  assert.notStrictEqual(match, undefined)
  if (match === undefined) return
  assert.strictEqual(SyntaxTree.directNodes(match, 'MatchArm').length, 2)
  assert.deepEqual(
    missingLeaves(match).map((leaf) => leaf.expected),
    ['FatArrow'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('bounds damaged pattern fields, nesting, braces, and guards at their arm', () => {
  const cases: ReadonlyArray<readonly [string, string, Token.TokenKind]> = [
    ['missing-pattern-name', '{ kind } => 1', 'Identifier'],
    ['missing-binding-name', 'Token { kind: , .. } => 1', 'Identifier'],
    ['missing-field-comma', 'Token { kind other } => 1', 'Comma'],
    ['missing-nested-colon', 'Token { child Inner {}, .. } => 1', 'Colon'],
    ['missing-pattern-brace', 'Token { kind, .. if true => 1', 'RightBrace'],
    ['missing-guard-expression', 'Token { kind, .. } if => 1', 'Identifier'],
  ]

  for (const [name, damagedArm, expected] of cases) {
    const source = `pub struct Inner {}
pub struct Token { kind: i32 other: i32 child: Inner }
pub struct End {}
pub fn inspect(event: Token | End) -> i32 {
  return match event {
    ${damagedArm}
    End {} => 0
  }
}
pub fn after() -> i32 { return 2 }`
    const result = parseText(`memory/${name}`, source)
    const match = descendants(result.root).find(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'MatchExpression',
    )
    const after = directFunctionDeclarations(result.root).at(-1)

    assert.notStrictEqual(match, undefined, name)
    assert.notStrictEqual(after, undefined, name)
    if (match === undefined || after === undefined) continue
    assert.strictEqual(
      missingLeaves(match).some((leaf) => leaf.expected === expected),
      true,
      name,
    )
    assert.strictEqual(SyntaxTree.directNodes(match, 'MatchArm').length, 2, name)
    assert.deepEqual(missingLeaves(after), [], name)
    assertOriginalTokenTraversal(result)
    assert.deepEqual(reconstructedBytes(result), ascii(source), name)
  }
})
