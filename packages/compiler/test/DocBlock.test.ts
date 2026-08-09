import { assert, it } from '@effect/vitest'
import { unreachable } from '../../../test/support/raise.js'
import * as DocBlock from '../src/DocBlock.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'

const encoder = new TextEncoder()

const parse = (source: string) =>
  Parser.parse(Lexer.lex(SourceFile.make('memory://documentation.silk', encoder.encode(source))))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> => [
  node,
  ...node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Node> => (SyntaxTree.isNode(child) ? descendants(child) : []),
  ),
]

const node = (syntax: ReturnType<typeof parse>, kind: SyntaxTree.NodeKind, occurrence = 0) =>
  descendants(syntax.root)
    .filter((candidate) => candidate.kind === kind)
    .at(occurrence)

const requiredNode = (
  syntax: ReturnType<typeof parse>,
  kind: SyntaxTree.NodeKind,
  occurrence = 0,
): SyntaxTree.Node => node(syntax, kind, occurrence) ?? unreachable(`expected ${kind}`)

const spellings = (block: DocBlock.DocBlock | undefined, source: string) =>
  block?.comments.map((comment) => source.slice(comment.span.start, comment.span.end))

it('attaches raw module and declaration documentation at every declaration level', () => {
  const source = `//! Recovery operations.
//! Shared by the complete module.
/// A problem.
pub struct Problem {
  /// Numeric code.
  pub code: i32
}
/// Recovers a problem.
pub effect fn recover(
  /// Problem to inspect.
  problem: Problem,
) -> i32 {
  return problem.code
}
impl Allocator for SystemAllocator {
  /// Allocation operation.
  allocate: SystemAllocator.allocate
}
`
  const syntax = parse(source)

  assert.deepEqual(spellings(DocBlock.ofModule(syntax), source), [
    '//! Recovery operations.',
    '//! Shared by the complete module.',
  ])
  assert.deepEqual(
    spellings(DocBlock.ofNode(syntax, requiredNode(syntax, 'StructDeclaration')), source),
    ['/// A problem.'],
  )
  assert.deepEqual(
    spellings(DocBlock.ofNode(syntax, requiredNode(syntax, 'StructField')), source),
    ['/// Numeric code.'],
  )
  assert.deepEqual(
    spellings(DocBlock.ofNode(syntax, requiredNode(syntax, 'FunctionDeclaration')), source),
    ['/// Recovers a problem.'],
  )
  assert.deepEqual(
    spellings(DocBlock.ofNode(syntax, requiredNode(syntax, 'ParameterDeclaration')), source),
    ['/// Problem to inspect.'],
  )
  assert.deepEqual(
    spellings(DocBlock.ofNode(syntax, requiredNode(syntax, 'ImplOperation')), source),
    ['/// Allocation operation.'],
  )
})

it('does not attach across blank lines or ordinary comments', () => {
  const blank = parse('/// separated\n\nfn answer() -> i32 { return 42 }')
  const ordinary = parse('/// separated\n// note\nfn answer() -> i32 { return 42 }')

  assert.isUndefined(DocBlock.ofNode(blank, requiredNode(blank, 'FunctionDeclaration')))
  assert.isUndefined(DocBlock.ofNode(ordinary, requiredNode(ordinary, 'FunctionDeclaration')))
})
