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

/**
 * Every program below that writes the selector form also carries `SEM0097`, because the operation
 * is analysis-only: it types, but no engine lowers it. That code appearing beside a clean analysis
 * is the point — the two statements are independent, and each assertion here still pins exactly
 * what the analysis concluded.
 */
const analysisOnly = 'SEM0097'
const notExecutable =
  'Member-selective Effect.catch is analysis-only: it type-checks, but no engine lowers it yet, so a program that uses it cannot be built'

it.effect('recovers one member and leaves the other in the result failure row', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch<A>(risky(flag), recoverA)
}
pub fn main() -> i32 { return 0 }`)
    // Declaring the residual as the result row is accepted: A is gone, B survives. Nothing about
    // the recovery itself is rejected — the only code is the analysis-only notice.
    assert.deepEqual(codes(self), [analysisOnly])
  }),
)

it.effect('reports the unrecovered member as still present in the result row', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
pub fn main() -> i32 {
  return run Effect.catch<A>(risky(true), recoverA)
}`)
    // The residual is not silently discarded; B reaches the caller and must be handled there.
    assert.deepEqual(codes(self), ['SEM0066', analysisOnly])
    assert.deepEqual(messages(self), ['Run leaves unhandled failures: root.B', notExecutable])
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
    assert.deepEqual(codes(selective), ['SEM0066', analysisOnly])
    assert.deepEqual(messages(selective), ['Run leaves unhandled failures: root.B', notExecutable])
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

/**
 * The operation types but does not execute. These three tests pin the only thing standing between
 * a user and a build that dies inside the backend with no source span: writing the selector form
 * says so, at the call site, under a stable code.
 */
it.effect('reports the analysis-only diagnostic at the call that writes the selector form', () =>
  Effect.gen(function* () {
    const call = 'Effect.catch<A>(risky(flag), recoverA)'
    const text = `${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run ${call}
}
pub fn main() -> i32 { return 0 }`
    const self = yield* analyze(text)
    const reported = Analysis.diagnostics(self).filter(
      (diagnostic) => diagnostic.code === analysisOnly,
    )
    assert.strictEqual(reported.length, 1)
    const diagnostic = reported.at(0)
    assert.strictEqual(diagnostic?.severity, 'error')
    assert.strictEqual(diagnostic?.message, notExecutable)
    // The span is the call itself: not the enclosing `run`, not the whole statement, and not a
    // stdlib function the user never wrote.
    const start = text.indexOf(call)
    assert.deepEqual(
      {
        sourceId: diagnostic?.span.sourceId,
        start: diagnostic?.span.start,
        end: diagnostic?.span.end,
      },
      { sourceId: 'root', start, end: start + call.length },
    )
  }),
)

it.effect('reports it at the pipelined selector form too, spanning the operation', () =>
  Effect.gen(function* () {
    const call = 'Effect.catch<A>(recoverA)'
    const text = `${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run risky(flag) |> ${call}
}
pub fn main() -> i32 { return 0 }`
    const self = yield* analyze(text)
    const diagnostic = Analysis.diagnostics(self).find(
      (candidate) => candidate.code === analysisOnly,
    )
    const start = text.indexOf(call)
    assert.deepEqual(
      { start: diagnostic?.span.start, end: diagnostic?.span.end },
      { start, end: start + call.length },
    )
  }),
)

/**
 * Before this diagnostic the same program reached the backend and was rejected as an
 * `InvalidEffectOperation` MIR violation inside `silk/effects`, naming a generated stdlib function
 * and no user span. Emission must now stop on the source diagnostic instead.
 */
it.effect('stops emission on the diagnostic rather than on a MIR violation', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch<A>(risky(flag), recoverA)
}
effect fn recoverRest(problem: B) -> i32 { return problem.code + 3 }
pub fn main() -> i32 { return run Effect.catchAll(selective(true), recoverRest) }`)
    const failure = yield* Effect.flip(Analysis.codegen(self, { mode: 'release' }))
    assert.strictEqual(failure._tag, 'CodegenUnavailable')
    if (failure._tag !== 'CodegenUnavailable') return
    assert.include(
      failure.diagnostics.map((diagnostic) => diagnostic.code),
      analysisOnly,
    )
    assert.deepEqual(
      failure.diagnostics
        .filter((diagnostic) => diagnostic.code === analysisOnly)
        .map((diagnostic) => diagnostic.message),
      [notExecutable],
    )
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
