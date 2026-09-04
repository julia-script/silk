import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'
import * as MirNormalization from '../src/MirNormalization.js'
import * as MirVerification from '../src/MirVerification.js'
import * as RowAlgebra from '../src/RowAlgebra.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Stdlib from '../src/Stdlib.js'
import * as Type from '../src/Type.js'
import * as Projections from './support/projections.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (text: string, target?: string) =>
  Analysis.makeRealized({
    root: SourceFile.make('root', ascii(text)),
    ...(target === undefined ? {} : { target }),
  }).pipe(Effect.provide(SourceResolver.memory(new Map())))

const codes = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)

const messages = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.message)

const failureMembers = (row: Type.FailureRow): ReadonlyArray<string> => {
  const concrete = RowAlgebra.concretize(Type.failureRowPolicy(), row)
  return concrete._tag === 'Concrete' ? concrete.row.members.map(Type.encode) : []
}

/** A two-member failure row plus a handler for each member on its own. */
const preamble = `pub struct A { code: i32 }
pub struct B { code: i32 }
effect fn risky(flag: bool) -> i32 ! A | B {
  if flag { fail A { code: 10 } }
  fail B { code: 20 }
}
effect fn recoverA(problem: A) -> i32 { return problem.code + 1 }
effect fn recoverB(problem: B) -> i32 { return problem.code + 2 }
effect fn recoverRow(problem: A | B) -> i32 { return 99 }
`

const borrowedMatchSource = (catch_: string): string => `import silk.effect { Effect }
struct Selected { code: i32 }
struct Token { value: i32 }
struct Left {}
struct Right {}
effect fn risky() -> i32 ! Selected { fail Selected { code: 1 } }
effect fn recover(problem: Selected, token: &Token) -> i32 {
  return problem.code + token.value
}
fn choose(choice: Left | Right) -> i32 {
  let token = Token { value: 41 }
  let caught = ${catch_}(risky(), recover(&token))
  let result = match move choice {
    Left {} => run move caught
    Right {} => run move caught
  }
  return result
}
pub fn main() -> i32 { return choose(Left {}) + choose(Right {}) }`

const infallibleRunLoanSource = `import silk.effect { Effect }
import silk.result { Result }
struct Token { value: i32 }
effect fn succeed(value: i32) -> i32 { return value }
fn add(value: i32, token: &Token) -> i32 { return value + token.value }
effect fn userMap(self: once Effect<i32>, onSuccess: once fn(i32) -> i32) -> i32 {
  let completed = run Effect.result(move self)
  return match move completed {
      Result<i32, never>.Success { value } => onSuccess(move value)
      Result<i32, never>.Failure { error } => move error
  }
}
pub fn main() -> i32 {
  let token = Token { value: 22 }
  return run succeed(20) |> userMap(add(&token))
}`

it.effect('applies ordinary take-once ownership to the direct intrinsic operands', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct A {}
struct Payload { value: i32 }
effect fn risky(payload: Payload) -> i32 ! A { fail A {} }
effect fn recoverA(problem: A) -> i32 { return 1 }
effect fn misuse() -> i32 ! A {
  let payload = Payload { value: 1 }
  let candidate = risky(move payload)
  let handled = Intrinsic.catchFailure<A>(move candidate, recoverA)
  let second = run candidate
  drop second
  return run handled
}
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(self), ['OWN0001'])
  }),
)

it.effect('reports the unrecovered member as still present in the result row', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect { Effect }
${preamble}
pub fn main() -> i32 {
  return run Effect.catch<A>(risky(true), recoverA)
}`)
    // The residual is not silently discarded; B reaches the caller and must be handled there.
    assert.deepEqual(codes(self), ['SEM0066'])
  }),
)

it.effect('catch and catchAll produce different result types on the same input', () =>
  Effect.gen(function* () {
    // Same protected Effect, same recovery site, only the operation differs. catchAll erases the
    // whole row, so running it where no failure may escape is accepted.
    const wholeRow = yield* analyze(`import silk.effect { Effect }
${preamble}
pub fn main() -> i32 { return run Effect.catchAll(risky(true), recoverRow) }`)
    assert.deepEqual(codes(wholeRow), [])

    // The selective form keeps B, so the identical run site now has a failure left to handle.
    const selective = yield* analyze(`import silk.effect { Effect }
${preamble}
pub fn main() -> i32 { return run Effect.catch<A>(risky(true), recoverA) }`)
    assert.deepEqual(codes(selective), ['SEM0066'])
  }),
)

it.effect('records the protected, selected, handler, and residual rows as facts', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect { Effect }
${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch<A>(risky(flag), recoverA)
}
pub fn main() -> i32 { return run Effect.catchAll(selective(true), recoverB) }`)
    const catches = self.instances.instances.flatMap((instance) =>
      instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap((root) => [...Hir.expressionTree(root)])
        .filter((expression) => expression._tag === 'EffectCatch')
        .map((expression) => Object.freeze({ expression, substitution: instance.substitution }))
        .filter(
          ({ expression, substitution }) =>
            Type.encode(Type.substitute(expression.selected, substitution)) === 'root.A',
        ),
    )
    assert.strictEqual(catches.length, 1)
    const found = catches.at(0)
    const fact = found?.expression
    if (fact?._tag !== 'EffectCatch' || found === undefined) {
      return unreachable('missing EffectCatch fact')
    }
    assert.deepEqual(
      {
        selected: Type.encode(Type.substitute(fact.selected, found.substitution)),
        protectedRow: failureMembers(
          Type.substituteFailureRow(fact.protectedRow, found.substitution),
        ),
        handlerRow: failureMembers(Type.substituteFailureRow(fact.handlerRow, found.substitution)),
        residualRow: failureMembers(
          Type.substituteFailureRow(fact.residualRow, found.substitution),
        ),
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
    const self = yield* analyze(`import silk.effect { Effect }
${preamble}
struct C { code: i32 }
effect fn recoverC(problem: C) -> i32 { return 0 }
effect fn selective(flag: bool) -> i32 ! A | B {
  return run Effect.catch<C>(risky(flag), recoverC)
}
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(self), ['SEM0067'])
  }),
)

it.effect('rejects a handler that does not accept the selected member', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect { Effect }
${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch<A>(risky(flag), recoverB)
}
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(self), ['SEM0100'])
  }),
)

for (const [name, selected, handler] of [
  ['empty selector', 'never', 'recoverNever'],
  ['absent scalar selector', 'i32', 'recoverInteger'],
] as const) {
  it.effect(`rejects a ${name} before availability`, () =>
    Effect.gen(function* () {
      const self = yield* analyze(`import silk.effect { Effect }
${preamble}
effect fn recoverNever(problem: never) -> i32 { return 0 }
effect fn recoverUnion(problem: A | B) -> i32 { return 0 }
effect fn recoverInteger(problem: i32) -> i32 { return problem }
effect fn invalid(flag: bool) -> i32 ! A | B {
  return run Effect.catch<${selected}>(risky(flag), ${handler})
}
pub fn main() -> i32 { return 0 }`)
      assert.include(codes(self), 'SEM0067')
      assert.notInclude(codes(self), 'SEM0098')
    }),
  )
}

it.effect('does not let nominal membership evidence discharge a failure by itself', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${preamble}
effect fn lie<S, A, E>(self: once Effect<A ! E>) -> A ! Without<E, S>
where S in E {
  return run self
}
pub fn main() -> i32 { return 0 }`)
    assert.include(codes(self), 'SEM0066')
  }),
)

it.effect('does not let subset evidence discharge a requirement by itself', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`struct Clock {}
effect fn lie<?S, A, ?R>(self: once Effect<A ? R>) -> A ? Without<R, S>
where S in R {
  return run self
}
pub fn main() -> i32 { return 0 }`)
    assert.include(codes(self), 'SEM0071')
  }),
)

/**
 * The gap this operation closes was user-visible as a contract mismatch: a helper narrowed to one
 * member could not accept a two-member Effect, and there was no way to spell the narrowing.
 */
it.effect('regression: narrowing a two-member row by contract still reports the mismatch', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect { Effect }
${preamble}
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

it.effect('rejects failure-only loan metadata on every infallible MIR run form', () =>
  Effect.gen(function* () {
    const raw = yield* Analysis.ofSourceRealized(
      'test/infallible-run-failure-loans',
      ascii(infallibleRunLoanSource),
      'wasm32-unknown-unknown',
      { normalizeMir: false },
    )
    assert.deepEqual(Analysis.diagnostics(raw), [])
    const rawModule = Analysis.loweredMir(raw)
    const provisional = Projections.provisionalMirOf(raw)
    const normalizedModule = MirNormalization.normalize(
      rawModule,
      provisional._tag === 'Unavailable'
        ? unreachable(`expected provisional MIR: ${provisional.error.message}`)
        : provisional.value,
    )
    assert.deepEqual(MirVerification.verify(rawModule), [])
    assert.deepEqual(MirVerification.verify(normalizedModule), [])

    const withFailureMetadata = (
      module: Mir.Module,
      form: 'RunEffect' | 'RunEffectValue' | 'RunStaticEffect',
      includeMetadata: boolean,
    ): Mir.Module => {
      const forged = structuredClone(module)
      const sourceTag = form === 'RunStaticEffect' ? 'RunStaticEffect' : 'RunEffectValue'
      const owner =
        forged.functions.find(
          (fn) =>
            MirVerification.operations(fn).some((operation) => operation._tag === sourceTag) &&
            MirVerification.operations(fn).some((operation) => operation._tag === 'EndLoan'),
        ) ?? unreachable(`expected ${sourceTag} with a caller loan for ${form}`)
      const candidate = MirVerification.operations(owner).find(
        (
          operation,
        ): operation is Extract<
          Mir.Operation,
          { readonly _tag: 'RunEffectValue' | 'RunStaticEffect' }
        > => operation._tag === sourceTag,
      )
      if (candidate === undefined)
        return unreachable(`expected infallible ${sourceTag} for ${form}`)
      const ending = MirVerification.operations(owner).find(
        (operation): operation is Mir.EndLoanOperation => operation._tag === 'EndLoan',
      )
      if (ending === undefined) return unreachable(`expected ${form} caller loan ending`)
      if (form === 'RunEffect' && candidate._tag === 'RunEffectValue') {
        Reflect.set(candidate, '_tag', 'RunEffect')
        Reflect.set(candidate, 'target', candidate.runner)
        Reflect.set(candidate, 'typeArguments', candidate.runnerTypeArguments)
        Reflect.set(
          candidate,
          'arguments',
          Object.freeze([candidate.effect, ...candidate.arguments]),
        )
      }
      if (includeMetadata) Reflect.set(candidate, 'failureLoanEnds', Object.freeze([ending]))
      return forged
    }

    for (const [module, form, rule] of [
      [rawModule, 'RunEffect', 'InvalidEffectOperation'],
      [rawModule, 'RunEffectValue', 'InvalidEffectOperation'],
      [normalizedModule, 'RunStaticEffect', 'InvalidNormalization'],
    ] as const) {
      assert.notInclude(
        MirVerification.verify(withFailureMetadata(module, form, false)).map(
          (violation) => violation.rule,
        ),
        rule,
        `expected a valid infallible ${form} template`,
      )
      assert.include(
        MirVerification.verify(withFailureMetadata(module, form, true)).map(
          (violation) => violation.rule,
        ),
        rule,
        `${form} must reject unreachable failure-only loan metadata`,
      )
    }
  }),
)

it.effect('rejects uncovered match endings and path-exclusive endings replayed by a loop', () =>
  Effect.gen(function* () {
    const self = yield* analyze(
      borrowedMatchSource('Effect.catch<Selected>'),
      'wasm32-unknown-unknown',
    )
    const mir = Analysis.loweredMir(self)
    const chooseIndex = mir.functions.findIndex((fn) => fn.id.name === 'choose')
    const choose = mir.functions.at(chooseIndex) ?? unreachable('expected choose MIR')
    const replaceChoose = (fn: Mir.MirFunction): Mir.Module =>
      Object.freeze({
        ...mir,
        functions: Object.freeze([
          ...mir.functions.slice(0, chooseIndex),
          fn,
          ...mir.functions.slice(chooseIndex + 1),
        ]),
      })

    let removed = false
    const uncovered: Mir.MirFunction = Object.freeze({
      ...choose,
      regions: Object.freeze(
        choose.regions.map((region) => {
          if (region._tag !== 'OperationRegion') return region
          return Object.freeze({
            ...region,
            operations: Object.freeze(
              region.operations.map((operation) => {
                if (operation._tag !== 'Match') return operation
                return Object.freeze({
                  ...operation,
                  arms: Object.freeze(
                    operation.arms.map((arm, ordinal) => {
                      if (ordinal !== 0) return arm
                      return Object.freeze({
                        ...arm,
                        selected: Object.freeze({
                          ...arm.selected,
                          operations: Object.freeze(
                            arm.selected.operations.filter((nested) => {
                              if (removed || nested._tag !== 'EndLoan') return true
                              removed = true
                              return false
                            }),
                          ),
                        }),
                      })
                    }),
                  ),
                })
              }),
            ),
          })
        }),
      ),
    })
    assert.strictEqual(removed, true)
    assert.include(
      MirVerification.verify(replaceChoose(uncovered)).map((violation) => violation.rule),
      'InvalidLoan',
    )

    const operations = MirVerification.operations(choose)
    const beginningCandidate = operations.find((operation) => operation._tag === 'BeginLoan')
    const beginning =
      beginningCandidate?._tag === 'BeginLoan'
        ? beginningCandidate
        : unreachable('expected one loan beginning')
    const endings = operations.filter(
      (operation): operation is Extract<Mir.Operation, { readonly _tag: 'EndLoan' }> =>
        operation._tag === 'EndLoan',
    )
    const firstEnding = endings.at(0) ?? unreachable('expected first match-arm ending')
    const secondEnding = endings.at(1) ?? unreachable('expected second match-arm ending')
    const initialCandidate = choose.regions.find((region) => region.id.ordinal === 0)
    const initial =
      initialCandidate?._tag === 'OperationRegion'
        ? initialCandidate
        : unreachable('expected choose initial region')
    const loop: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: 0 })
    const region = (ordinal: number): Mir.RegionId => Object.freeze({ _tag: 'Region', ordinal })
    const provenance = initial.outcome.provenance
    const alternating: Mir.MirFunction = Object.freeze({
      ...choose,
      entry: region(0),
      regions: Object.freeze([
        Object.freeze({
          ...initial,
          operations: Object.freeze([...initial.operations, beginning]),
          outcome: Object.freeze({ _tag: 'Forward', target: region(1), provenance }),
        }),
        Object.freeze({
          _tag: 'LoopRegion',
          id: region(1),
          loop,
          condition: region(2),
          conditionValue: Object.freeze({ _tag: 'Local', ordinal: 0 }),
          body: region(3),
          following: region(7),
          provenance,
        }),
        Object.freeze({
          _tag: 'OperationRegion',
          id: region(2),
          ownerLoop: loop,
          operations: Object.freeze([]),
          outcome: Object.freeze({ _tag: 'Yield', provenance }),
        }),
        Object.freeze({
          _tag: 'OperationRegion',
          id: region(3),
          ownerLoop: loop,
          operations: Object.freeze([]),
          outcome: Object.freeze({ _tag: 'Forward', target: region(4), provenance }),
        }),
        Object.freeze({
          _tag: 'ConditionalRegion',
          id: region(4),
          ownerLoop: loop,
          condition: Object.freeze({ _tag: 'Local', ordinal: 0 }),
          taken: region(5),
          otherwise: region(6),
          provenance,
        }),
        Object.freeze({
          _tag: 'CleanupRegion',
          id: region(5),
          ownerLoop: loop,
          releases: Object.freeze([firstEnding]),
          outcome: Object.freeze({ _tag: 'Repeat', loop, provenance }),
        }),
        Object.freeze({
          _tag: 'CleanupRegion',
          id: region(6),
          ownerLoop: loop,
          releases: Object.freeze([secondEnding]),
          outcome: Object.freeze({ _tag: 'Repeat', loop, provenance }),
        }),
        Object.freeze({
          _tag: 'OperationRegion',
          id: region(7),
          operations: Object.freeze([]),
          outcome: Object.freeze({ _tag: 'Trap', reason: 'loop finished', provenance }),
        }),
      ]),
    })
    assert.include(
      MirVerification.verify(replaceChoose(alternating)).map((violation) => violation.rule),
      'InvalidLoan',
    )
  }),
)

it.effect('does not let an unreachable loan ending follow failure propagation', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      `import silk.effect { Effect }
struct Selected { code: i32 }
struct Residual { code: i32 }
struct Token { value: i32 }
struct Left {}
struct Right {}
effect fn risky() -> i32 ! Selected | Residual { fail Residual { code: 20 } }
effect fn recover(problem: Selected, token: &Token) -> i32 {
  return problem.code + token.value
}
effect fn recoverResidual(problem: Residual) -> i32 { return problem.code + 2 }
effect fn choose(choice: Left | Right) -> i32 ! Residual {
  let token = Token { value: 41 }
  let caught = Effect.catch<Selected>(risky(), recover(&token))
  let result = match move choice {
    Left {} => run move caught
    Right {} => run move caught
  }
  return result
}
pub fn main() -> i32 {
  return run Effect.catchAll(choose(Left {}), recoverResidual)
}`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const module = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(module), [])
    const chooseIndex = module.functions.findIndex((fn) => fn.id.name === 'choose$effect$-1')
    const choose =
      module.functions.at(chooseIndex) ?? unreachable('expected effectful choose runner')
    const propagationCandidate = module.functions
      .flatMap(MirVerification.operations)
      .find((operation) => operation._tag === 'PropagateEffectFailure')
    const propagation =
      propagationCandidate?._tag === 'PropagateEffectFailure'
        ? propagationCandidate
        : unreachable('expected selective failure propagation')
    let injected = false
    const injectBeforeEnding = (
      nested: ReadonlyArray<Mir.Operation>,
    ): ReadonlyArray<Mir.Operation> =>
      Object.freeze(
        nested.flatMap((operation): ReadonlyArray<Mir.Operation> => {
          if (!injected && operation._tag === 'EndLoan') {
            injected = true
            return [propagation, operation]
          }
          if (operation._tag === 'ShortCircuit')
            return [
              Object.freeze({
                ...operation,
                right: Object.freeze({
                  ...operation.right,
                  operations: injectBeforeEnding(operation.right.operations),
                }),
              }),
            ]
          if (operation._tag === 'Match')
            return [
              Object.freeze({
                ...operation,
                arms: Object.freeze(
                  operation.arms.map((arm) =>
                    Object.freeze({
                      ...arm,
                      ...(arm.guard === undefined
                        ? {}
                        : {
                            guard: Object.freeze({
                              ...arm.guard,
                              operations: injectBeforeEnding(arm.guard.operations),
                            }),
                          }),
                      selected: Object.freeze({
                        ...arm.selected,
                        operations: injectBeforeEnding(arm.selected.operations),
                      }),
                    }),
                  ),
                ),
              }),
            ]
          return [operation]
        }),
      )
    const forgedChoose: Mir.MirFunction = Object.freeze({
      ...choose,
      regions: Object.freeze(
        choose.regions.map((region): Mir.Region =>
          region._tag === 'OperationRegion'
            ? Object.freeze({
                ...region,
                operations: injectBeforeEnding(region.operations),
              })
            : region,
        ),
      ),
    })
    assert.strictEqual(injected, true)
    const replaceChoose = (replacement: Mir.MirFunction): Mir.Module =>
      Object.freeze({
        ...module,
        functions: Object.freeze([
          ...module.functions.slice(0, chooseIndex),
          replacement,
          ...module.functions.slice(chooseIndex + 1),
        ]),
      })
    assert.include(
      MirVerification.verify(replaceChoose(forgedChoose)).map((violation) => violation.rule),
      'InvalidLoan',
    )
  }),
)

it('catch and catchAll no longer share a doc comment', () => {
  const bytes = Stdlib.sources.get('silk/effect')
  if (bytes === undefined) unreachable('silk/effect source is missing')
  const effects = new TextDecoder().decode(bytes)
  const docOf = (name: string): string => {
    const declaration = effects.indexOf(`pub effect fn ${name}<`)
    if (declaration < 0) unreachable(`${name} is missing from silk/effect`)
    const before = effects.slice(0, declaration).split('\n')
    const lines: Array<string> = []
    for (let index = before.length - 2; index >= 0; index -= 1) {
      const line = before[index] ?? ''
      if (!line.trimStart().startsWith('///')) break
      lines.unshift(line)
    }
    return lines.join('\n')
  }
  const catchDoc = docOf('catch')
  const catchAllDoc = docOf('catchAll')
  assert.notStrictEqual(catchDoc, '')
  assert.notStrictEqual(catchAllDoc, '')
  assert.notStrictEqual(catchDoc, catchAllDoc)
})
