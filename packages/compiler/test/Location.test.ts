import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as AuthoredIdentity from '../src/AuthoredIdentity.js'
import * as AuthoredLowering from '../src/AuthoredLowering.js'
import * as Lexer from '../src/Lexer.js'
import * as Location from '../src/Location.js'
import * as Parser from '../src/Parser.js'
import * as Provenance from '../src/Provenance.js'
import * as SemanticContext from '../src/SemanticContext.js'
import * as SourceFile from '../src/SourceFile.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

/** Lowers named sources and answers their registry plus every text literal anchor, in order. */
const present = (sources: Readonly<Record<string, string>>) =>
  Effect.gen(function* () {
    const contexts: Array<SemanticContext.SemanticContext> = []
    const literals = new Map<string, ReadonlyArray<AuthoredIdentity.Anchor>>()
    for (const [name, text] of Object.entries(sources)) {
      const lowered = yield* AuthoredLowering.lower(
        Parser.parse(Lexer.lex(SourceFile.make(name, encoder.encode(text)))),
        AuthoredIdentity.module('memory', name),
      )
      contexts.push(SemanticContext.make(lowered))
      literals.set(
        name,
        lowered.presentation.entries
          .filter((entry) => entry.spelling?.startsWith('"') === true)
          .map((entry) => entry.anchor),
      )
    }
    const registry = SemanticContext.registry(contexts)
    const literal = (module: string, ordinal: number): AuthoredIdentity.Anchor => {
      const anchor = literals.get(module)?.at(ordinal)
      if (anchor === undefined) throw new RangeError(`no literal ${ordinal} in ${module}`)
      return anchor
    }
    const text = (span: { sourceId: string; start: number; end: number }): string =>
      decoder.decode(encoder.encode(sources[span.sourceId] ?? '').slice(span.start, span.end))
    return { registry, literal, text }
  })

it.effect('maps one decoded byte to the source bytes of the escape that produced it', () =>
  Effect.gen(function* () {
    const { registry, literal, text } = yield* present({
      'loc/main': 'pub fn f() -> i32 { return g("a\\x41\\u{e9}z") }',
    })
    const written = literal('loc/main', 0)
    const fallback = written
    // 61 41 c3 a9 7a: five decoded bytes behind thirteen spelling bytes.
    const value = Provenance.literal(written, 5)
    const rejected = Location.within(
      Provenance.sourcesOf(Provenance.slice(value, 1, 2), 0, 1),
      fallback,
    )
    assert.strictEqual(text(Location.resolve(rejected, registry).span), '\\x41')
    const accent = Location.within(Provenance.sourcesOf(value, 2, 4), fallback)
    assert.strictEqual(text(Location.resolve(accent, registry).span), '\\u{e9}')
  }),
)

it.effect('carries a substring through helpers and reports each call at its own spelling', () =>
  Effect.gen(function* () {
    const { registry, literal, text } = yield* present({
      'loc/main': 'pub fn main() -> i32 { let first = reject("aéz") return reject("a\\u{e9}z") }',
    })
    const fallback = literal('loc/main', 0)
    // inner slices 1..4 of its parameter, outer slices 0..2 of that: no helper sees a position.
    const inner = Provenance.slice(Provenance.parameter(0, 4), 1, 4)
    const outer = Provenance.slice(Provenance.substitute(inner, [Provenance.parameter(0, 4)]), 0, 2)
    assert.deepEqual(Provenance.sourcesOf(outer, 0, 2), [
      { _tag: 'Parameter', ordinal: 0, range: { start: 1, end: 3 } },
    ])
    const shared = Location.within(Provenance.sourcesOf(outer, 0, 2), fallback)
    assert.isTrue(Location.isShared(shared))
    // One shared location, two call sites, two spellings of the same value.
    const reports = [0, 1].map((ordinal) => {
      const argument = literal('loc/main', ordinal)
      const local = Location.substitute(shared, [Provenance.literal(argument, 4)])
      assert.isFalse(Location.isShared(local))
      return text(Location.resolve(local, registry).span)
    })
    assert.deepEqual(reports, ['é', '\\u{e9}'])
  }),
)

it.effect('keeps a callee-owned literal in a shared result and substitutes only parameters', () =>
  Effect.gen(function* () {
    const { registry, literal, text } = yield* present({
      'loc/helper': 'pub static fn helper(value: string) -> string { return "prefix:" + value }',
      'loc/main': 'pub fn g() -> i32 { return reject(helper("aéz")) }',
    })
    const owned = literal('loc/helper', 0)
    const argument = literal('loc/main', 0)
    const fallback = argument
    // "prefix:" + value, as the shared outcome of helper records it.
    const outcome = Provenance.concat(Provenance.literal(owned, 7), 7, Provenance.parameter(0, 4))
    assert.deepEqual(
      outcome.map((segment) => segment.from._tag),
      ['Literal', 'Parameter'],
    )
    const composed = Provenance.substitute(outcome, [Provenance.literal(argument, 4)])
    const rejected = Location.within(Provenance.sourcesOf(composed, 5, 10), fallback)
    const resolved = Location.resolve(rejected, registry)
    // The span lies in the helper's module; the rest of the range is related, in the caller's.
    assert.strictEqual(resolved.span.sourceId, 'loc/helper')
    assert.strictEqual(text(resolved.span), 'x:')
    assert.deepEqual(resolved.related.map(text), ['aé'])
    // A returned literal alone needs no caller at all.
    const returned = Location.within(
      Provenance.sourcesOf(Provenance.literal(owned, 7), 0, 3),
      fallback,
    )
    assert.isFalse(Location.isShared(returned))
    assert.strictEqual(text(Location.resolve(returned, registry).span), 'pre')
  }),
)

it.effect('falls back to the call when an argument was computed rather than written', () =>
  Effect.gen(function* () {
    const { registry, literal, text } = yield* present({
      'loc/main': 'pub fn main() -> i32 { return reject("call site") }',
    })
    const call = literal('loc/main', 0)
    const fallback = call
    const shared = Location.within(Provenance.sourcesOf(Provenance.parameter(0, 4), 1, 3), fallback)
    const local = Location.substitute(shared, [undefined])
    assert.strictEqual(text(Location.resolve(local, registry).span), '"call site"')
  }),
)
