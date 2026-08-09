import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxFile from '../src/SyntaxFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const acceptedSource = '/// answers\npub fn main() -> i32 { return 42 }'
const malformedBytes = Uint8Array.of(0x40, 0xff, ...ascii(' pub fn main( -> Mystery { return 42 }'))

const parseBytes = (id: string, bytes: Uint8Array): SyntaxFile.SyntaxFile =>
  Parser.parse(Lexer.lex(SourceFile.make(id, bytes)))

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it('bundles one lossless artifact with source, tokens, tree, and diagnostics', () => {
  const syntax = parseBytes('golden://accepted.silk', ascii(acceptedSource))

  assert.strictEqual(Object.isFrozen(syntax), true)
  assert.strictEqual(syntax.source.id, 'golden://accepted.silk')
  assert.deepEqual(syntax.lexicalDiagnostics, [])
  assert.deepEqual(syntax.parserDiagnostics, [])
  const reconstructed = syntax.tokens
    .filter((token) => token.kind !== 'EndOfFile')
    .flatMap((token) => Array.from(Option.getOrThrow(SourceFile.slice(syntax.source, token.span))))
  assert.deepEqual(Uint8Array.from(reconstructed), ascii(acceptedSource))
})

it('assigns stable element identities qualified by source identity', () => {
  const first = parseBytes('golden://identity.silk', ascii(acceptedSource))
  const second = parseBytes('golden://identity.silk', ascii(acceptedSource))
  const other = parseBytes('golden://other.silk', ascii(acceptedSource))

  const identities = (syntax: SyntaxFile.SyntaxFile) =>
    SyntaxTree.tokens(syntax.root).map((token) => Option.getOrThrow(SyntaxFile.idOf(syntax, token)))
  assert.deepEqual(identities(first), identities(second))
  assert.deepEqual(Option.getOrThrow(SyntaxFile.idOf(first, first.root)), {
    _tag: 'SyntaxId',
    sourceId: 'golden://identity.silk',
    ordinal: 0,
  })
  assert.notDeepEqual(identities(first), identities(other))
})

it('rejects identity lookups for foreign elements', () => {
  const first = parseBytes('golden://foreign-a.silk', ascii(acceptedSource))
  const second = parseBytes('golden://foreign-b.silk', ascii(acceptedSource))

  assert.strictEqual(Option.isNone(SyntaxFile.idOf(first, second.root)), true)
  const foreignToken = second.tokens.at(0)
  assert.notStrictEqual(foreignToken, undefined)
  if (foreignToken === undefined) return
  assert.strictEqual(Option.isNone(SyntaxFile.idOf(first, foreignToken)), true)
})

it('matches the accepted golden encoding byte-for-byte', () => {
  const syntax = parseBytes('golden://accepted.silk', ascii(acceptedSource))

  assert.strictEqual(SyntaxFile.encode(syntax), golden('accepted.syntax.txt'))
})

it('matches the malformed golden encoding and names recovered structure', () => {
  const syntax = parseBytes('golden://malformed.silk', malformedBytes)
  const encoded = SyntaxFile.encode(syntax)

  assert.strictEqual(encoded, golden('malformed.syntax.txt'))
  assert.include(encoded, 'missing ')
  assert.include(encoded, 'Error ')
  assert.include(encoded, '\\xFF')
})

it('encodes byte-identically across repeated fresh constructions', () => {
  const first = SyntaxFile.encode(parseBytes('golden://repeat.silk', malformedBytes))
  const second = SyntaxFile.encode(parseBytes('golden://repeat.silk', malformedBytes))

  assert.strictEqual(first, second)
})
