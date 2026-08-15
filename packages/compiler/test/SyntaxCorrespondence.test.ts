import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxCorrespondence from '../src/SyntaxCorrespondence.js'
import * as SyntaxFile from '../src/SyntaxFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parse = (id: string, source: string): SyntaxFile.SyntaxFile =>
  Parser.parse(Lexer.lex(SourceFile.make(id, ascii(source))))

const functions = (syntax: SyntaxFile.SyntaxFile): ReadonlyArray<SyntaxTree.Node> =>
  SyntaxTree.directNodes(syntax.root, 'FunctionDeclaration')

const functionAt = (syntax: SyntaxFile.SyntaxFile, index: number): SyntaxTree.Node =>
  functions(syntax).at(index) ?? assert.fail(`expected function ${index}`)

const source = (name: string, value: number): string =>
  `pub fn ${name}() -> i32 { return ${value} }`

it('relates distinct shifted and reordered declarations through canonical identity pairs', () => {
  const previous = parse('app/Main', `${source('first', 1)}\n${source('second', 2)}`)
  const inserted = parse(
    'app/Main',
    `${source('first', 1)}\n${source('zero', 0)}\n${source('second', 2)}`,
  )
  const insertion = Option.getOrThrow(SyntaxCorrespondence.between(previous, inserted))
  const previousFirst = functionAt(previous, 1)
  const insertedFirst = functionAt(inserted, 2)
  assert.strictEqual(
    Option.getOrThrow(SyntaxCorrespondence.currentOf(insertion, previousFirst)),
    insertedFirst,
  )
  assert.strictEqual(
    Option.getOrThrow(SyntaxCorrespondence.previousOf(insertion, insertedFirst)),
    previousFirst,
  )
  const previousId = Option.getOrThrow(SyntaxFile.idOf(previous, previousFirst))
  const currentId = Option.getOrThrow(SyntaxFile.idOf(inserted, insertedFirst))
  assert.notStrictEqual(previousId.ordinal, currentId.ordinal)
  assert.deepInclude(insertion.identities, { previous: previousId, current: currentId })
  assert.isAbove(insertion.counts.correspondingElements, 0)
  assert.isBelow(insertion.counts.correspondingElements, insertion.counts.currentElements)

  const compactPrevious = parse('app/Main', `${source('first', 1)}${source('second', 2)}`)
  const reordered = parse('app/Main', `${source('second', 2)}${source('first', 1)}`)
  const reorder = Option.getOrThrow(SyntaxCorrespondence.between(compactPrevious, reordered))
  assert.strictEqual(
    Option.getOrThrow(SyntaxCorrespondence.currentOf(reorder, functionAt(compactPrevious, 0))),
    functionAt(reordered, 1),
  )
  assert.strictEqual(
    Option.getOrThrow(SyntaxCorrespondence.currentOf(reorder, functionAt(compactPrevious, 1))),
    functionAt(reordered, 0),
  )
})

it('leaves edited and ambiguous duplicate declaration subtrees unmatched', () => {
  const previous = parse('app/Main', `${source('first', 1)}\n${source('second', 2)}`)
  const edited = parse('app/Main', `${source('first', 9)}\n${source('second', 2)}`)
  const correspondence = Option.getOrThrow(SyntaxCorrespondence.between(previous, edited))
  assert.isTrue(
    Option.isNone(SyntaxCorrespondence.currentOf(correspondence, functionAt(previous, 0))),
  )
  assert.strictEqual(
    Option.getOrThrow(SyntaxCorrespondence.currentOf(correspondence, functionAt(previous, 1))),
    functionAt(edited, 1),
  )

  const duplicate = source('same', 1)
  const duplicatePrevious = parse('app/Main', `${duplicate}${duplicate}`)
  const duplicateCurrent = parse('app/Main', `${duplicate}${duplicate}${duplicate}`)
  const ambiguous = Option.getOrThrow(
    SyntaxCorrespondence.between(duplicatePrevious, duplicateCurrent),
  )
  for (const declaration of functions(duplicatePrevious)) {
    assert.isTrue(Option.isNone(SyntaxCorrespondence.currentOf(ambiguous, declaration)))
  }
})

it('handles exact recovery subtrees conservatively and rejects foreign sources', () => {
  const previous = parse('app/Main', `pub fn damaged() -> i32 { return }\n${source('stable', 2)}`)
  const current = parse('app/Main', `pub fn damaged() -> i32 { return 1 }\n${source('stable', 2)}`)
  const correspondence = Option.getOrThrow(SyntaxCorrespondence.between(previous, current))
  assert.isTrue(
    Option.isNone(SyntaxCorrespondence.currentOf(correspondence, functionAt(previous, 0))),
  )
  assert.strictEqual(
    Option.getOrThrow(SyntaxCorrespondence.currentOf(correspondence, functionAt(previous, 1))),
    functionAt(current, 1),
  )
  assert.isTrue(
    Option.isNone(
      SyntaxCorrespondence.between(previous, parse('app/Other', SyntaxFile.encode(current))),
    ),
  )
})

it('produces deterministic ordered evidence across repeated constructions', () => {
  const build = () => {
    const previous = parse('app/Main', `${source('first', 1)}\n${source('second', 2)}`)
    const current = parse(
      'app/Main',
      `${source('first', 1)}\n${source('zero', 0)}\n${source('second', 2)}`,
    )
    const correspondence = Option.getOrThrow(SyntaxCorrespondence.between(previous, current))
    return { identities: correspondence.identities, counts: correspondence.counts }
  }
  assert.deepEqual(build(), build())
})

it('corresponds an unchanged opaque result subtree after a preceding declaration is inserted', () => {
  const opaque = 'pub fn make() -> some<F: fn(i32) -> i32> fn(F) -> F { return 0 }'
  const previous = parse('app/Main', opaque)
  const current = parse('app/Main', `${source('before', 0)}${opaque}`)
  const correspondence = Option.getOrThrow(SyntaxCorrespondence.between(previous, current))
  const previousOpaque = SyntaxTree.directNode(
    SyntaxTree.directNode(functionAt(previous, 0), 'ReturnType') ?? assert.fail('return type'),
    'OpaqueResultType',
  )
  const currentOpaque = SyntaxTree.directNode(
    SyntaxTree.directNode(functionAt(current, 1), 'ReturnType') ?? assert.fail('return type'),
    'OpaqueResultType',
  )
  assert.isDefined(previousOpaque)
  assert.strictEqual(
    Option.getOrThrow(
      SyntaxCorrespondence.currentOf(
        correspondence,
        previousOpaque ?? assert.fail('previous opaque result'),
      ),
    ),
    currentOpaque,
  )
})
