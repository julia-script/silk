import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Elaboration from '../src/Elaboration.js'
import * as Hir from '../src/Hir.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const acceptedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`
const damagedSource = `pub fn puzzle(value: Mystery) -> I32 { return value }
pub fn main() -> I32 { return missing(2147483648) }`

const elaborate = (id: string, text: string): Elaboration.Result =>
  Elaboration.elaborateModule(Parser.parse(Lexer.lex(SourceFile.make(id, ascii(text)))))

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

it('constructs typed HIR with canonical call targets and normalized contracts', () => {
  const result = elaborate('golden://accepted.silk', acceptedSource)
  const main = result.hir.functions.at(1)

  assert.deepEqual(main?.contract, { _tag: 'Contract', parameters: [], result: 'I32' })
  const body = main?.body
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
  assert.strictEqual(puzzle.body._tag, 'Unavailable')
  assert.strictEqual(main?.body._tag, 'Unavailable')
  if (main?.body._tag !== 'Unavailable') return
  assert.strictEqual(main.body.cause?.code, 'SEM0004')
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
