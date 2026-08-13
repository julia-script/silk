import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Stdlib from '../src/Stdlib.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (text: string) =>
  Analysis.makeRealized({ root: SourceFile.make('root', ascii(text)) }).pipe(
    Effect.provide(SourceResolver.memory(new Map())),
  )

const codes = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)

const messages = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.message)

/** A two-member failure row plus a handler for each member on its own. */
const preamble = `struct A { code: i32 }
struct B { code: i32 }
effect fn risky(flag: bool) -> i32 ! A | B {
  if flag { fail A { code: 10 } }
  fail B { code: 20 }
}
effect fn recoverA(problem: A) -> i32 { return problem.code + 1 }
effect fn recoverB(problem: B) -> i32 { return problem.code + 2 }
effect fn recoverRow(problem: Row<!A | B>) -> i32 { return 99 }
`

it.effect('recovers one member and leaves the other in the result failure row', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch<A>(risky(flag), recoverA)
}
pub fn main() -> i32 { return 0 }`)
    // Declaring the residual as the result row is accepted: A is gone, B survives.
    assert.deepEqual(codes(self), [])
  }),
)

it.effect('reports the unrecovered member as still present in the result row', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
pub fn main() -> i32 {
  return run Effect.catch<A>(risky(true), recoverA)
}`)
    // The residual is not silently discarded; B reaches the caller and must be handled there.
    assert.deepEqual(codes(self), ['SEM0066'])
    assert.deepEqual(messages(self), ['Run leaves unhandled failures: root.B'])
  }),
)

it.effect('catch and catchAll produce different result types on the same input', () =>
  Effect.gen(function* () {
    // Same protected Effect, same recovery site, only the operation differs. catchAll erases the
    // whole row, so running it where no failure may escape is accepted.
    const wholeRow = yield* analyze(`${preamble}
pub fn main() -> i32 { return run Effect.catchAll(risky(true), recoverRow) }`)
    assert.deepEqual(codes(wholeRow), [])

    // The selective form keeps B, so the identical run site now has a failure left to handle.
    const selective = yield* analyze(`${preamble}
pub fn main() -> i32 { return run Effect.catch<A>(risky(true), recoverA) }`)
    assert.deepEqual(codes(selective), ['SEM0066'])
    assert.deepEqual(messages(selective), ['Run leaves unhandled failures: root.B'])
  }),
)

it.effect('records the protected, selected, handler, and residual rows as facts', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch<A>(risky(flag), recoverA)
}
pub fn main() -> i32 { return 0 }`)
    const hir = Analysis.hirOf(self, 'root')
    const catches = (hir?.functions ?? []).flatMap((fn) =>
      fn.statements
        .flatMap(Hir.statementExpressions)
        .flatMap((root) => [...Hir.expressionTree(root)])
        .filter((expression) => expression._tag === 'EffectCatch'),
    )
    assert.strictEqual(catches.length, 1)
    const fact = catches.at(0)
    if (fact?._tag !== 'EffectCatch') throw new Error('missing EffectCatch fact')
    assert.deepEqual(
      {
        selected: Type.encode(fact.selected),
        protectedRow: fact.protectedRow.map(Type.encode),
        handlerRow: fact.handlerRow.map(Type.encode),
        residualRow: fact.residualRow.map(Type.encode),
      },
      {
        selected: 'root.A',
        protectedRow: ['root.A', 'root.B'],
        handlerRow: [],
        residualRow: ['root.B'],
      },
    )
  }),
)

it.effect('rejects a selector the protected Effect cannot fail with', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
struct C { code: i32 }
effect fn recoverC(problem: C) -> i32 { return 0 }
effect fn selective(flag: bool) -> i32 ! A | B {
  return run Effect.catch<C>(risky(flag), recoverC)
}
pub fn main() -> i32 { return 0 }`)
    assert.include(messages(self).join('\n'), 'does not fail with root.C')
  }),
)

it.effect('rejects a handler that does not accept the selected member', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch<A>(risky(flag), recoverB)
}
pub fn main() -> i32 { return 0 }`)
    assert.include(messages(self).join('\n'), 'must accept root.A')
  }),
)

/**
 * The gap this operation closes was user-visible as a contract mismatch: a helper narrowed to one
 * member could not accept a two-member Effect, and there was no way to spell the narrowing.
 */
it.effect('regression: narrowing a two-member row by contract still reports the mismatch', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
effect fn handleA(self: once Effect<i32 ! A>) -> i32 {
  return run Effect.catchAll(move self, recoverA)
}
pub fn main() -> i32 { return run handleA(risky(true)) }`)
    assert.deepEqual(codes(self), ['SEM0012'])
    assert.deepEqual(messages(self), [
      'Expected once Effect<i32 ! root.A> but received Effect<i32 ! root.A | root.B>',
    ])
  }),
)

it.effect('catch and catchAll no longer share a doc comment', () =>
  Effect.gen(function* () {
    const bytes = Stdlib.sources.get('silk/effects')
    if (bytes === undefined) throw new Error('silk/effects source is missing')
    const effects = new TextDecoder().decode(bytes)
    const docOf = (name: string): string => {
      const declaration = effects.indexOf(`pub effect fn ${name}<`)
      if (declaration < 0) throw new Error(`${name} is missing from silk/effects`)
      const before = effects.slice(0, declaration).split('\n')
      const lines: Array<string> = []
      for (let index = before.length - 2; index >= 0; index -= 1) {
        const line = before[index] ?? ''
        if (!line.startsWith('///')) break
        lines.unshift(line)
      }
      return lines.join('\n')
    }
    const catchDoc = docOf('catch')
    const catchAllDoc = docOf('catchAll')
    assert.notStrictEqual(catchDoc, '')
    assert.notStrictEqual(catchAllDoc, '')
    assert.notStrictEqual(catchDoc, catchAllDoc)
  }),
)
