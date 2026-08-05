import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Diagnostic from '../src/Diagnostic.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceSpan from '../src/SourceSpan.js'
import { raise } from './support/raise.js'

const source = SourceFile.make('memory://diagnostic.silk', new Uint8Array(64))

const spanAt = (start: number, end: number): SourceSpan.SourceSpan =>
  Option.getOrThrow(SourceSpan.make(source, start, end))

const build = () => {
  const lexical = [Diagnostic.unsupportedBytes(spanAt(10, 12))]
  const parser = [
    Diagnostic.missingToken('RightParenthesis', spanAt(4, 4)),
    Diagnostic.unexpectedTokens(spanAt(10, 12)),
  ]
  const semantic = [
    Diagnostic.unknownType('Bogus', spanAt(2, 7)),
    Diagnostic.unknownFunction('missing', spanAt(10, 12)),
  ]
  return { lexical, parser, semantic }
}

it('merges phase collections into one deterministic driver order', () => {
  const { lexical, parser, semantic } = build()
  const merged = Diagnostic.merge(lexical, parser, semantic)

  assert.strictEqual(Object.isFrozen(merged), true)
  assert.deepEqual(
    merged.map((diagnostic) => ({
      phase: diagnostic.phase,
      code: diagnostic.code,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    [
      { phase: 'semantic', code: 'SEM0001', start: 2, end: 7 },
      { phase: 'parser', code: 'PAR0001', start: 4, end: 4 },
      { phase: 'lexical', code: 'LEX0001', start: 10, end: 12 },
      { phase: 'parser', code: 'PAR0002', start: 10, end: 12 },
      { phase: 'semantic', code: 'SEM0004', start: 10, end: 12 },
    ],
  )
})

it('produces an identical merged sequence across repeated fresh constructions', () => {
  const first = build()
  const second = build()

  assert.deepEqual(
    Diagnostic.merge(first.lexical, first.parser, first.semantic),
    Diagnostic.merge(second.lexical, second.parser, second.semantic),
  )
})

it('orders same-span same-phase diagnostics by stable code', () => {
  const span = spanAt(3, 5)
  const merged = Diagnostic.merge([
    Diagnostic.unexpectedTokens(span),
    Diagnostic.missingToken('Identifier', span),
  ])

  assert.deepEqual(
    merged.map((diagnostic) => diagnostic.code),
    ['PAR0001', 'PAR0002'],
  )
})

it('assigns identity ordinals among equal phase, code, and span', () => {
  const span = spanAt(0, 1)
  const equal = Diagnostic.unknownFunction('twice', span)
  const other = Diagnostic.unknownType('Other', spanAt(2, 3))
  const identities = Diagnostic.identify([equal, other, equal])
  const first = identities.at(0) ?? raise('expected first diagnostic identity')
  const repeated = identities.at(2) ?? raise('expected repeated diagnostic identity')

  assert.deepEqual(
    identities.map((identity) => identity.ordinal),
    [0, 0, 1],
  )
  assert.strictEqual(Diagnostic.identityEquals(first, Diagnostic.identity(equal)), true)
  assert.strictEqual(Diagnostic.identityEquals(first, repeated), false)
  assert.isBelow(Diagnostic.compareIdentity(first, repeated), 0)
})

it('merges ownership diagnostics after the semantic phase at the same span', () => {
  const span = spanAt(10, 12)
  const merged = Diagnostic.merge(
    [Diagnostic.useAfterMove('value', spanAt(2, 7), span)],
    [Diagnostic.unknownFunction('missing', span)],
  )

  assert.deepEqual(
    merged.map((diagnostic) => ({ phase: diagnostic.phase, code: diagnostic.code })),
    [
      { phase: 'ownership', code: 'OWN0001' },
      { phase: 'semantic', code: 'SEM0004' },
    ],
  )
  const ownership = merged.at(0)
  assert.strictEqual(ownership?.relatedSpans?.at(0)?.label, 'moved here')
  assert.strictEqual(ownership?.reason._tag, 'UseAfterMove')
})

it('carries the original declaration span on a rebinding diagnostic', () => {
  const diagnostic = Diagnostic.rebindingName('value', spanAt(2, 7), spanAt(10, 15))

  assert.strictEqual(diagnostic.phase, 'semantic')
  assert.strictEqual(diagnostic.code, 'SEM0008')
  assert.strictEqual(diagnostic.relatedSpans?.at(0)?.label, 'first declared here')
  assert.strictEqual(diagnostic.relatedSpans?.at(0)?.span.start, 2)
})
