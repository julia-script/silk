import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as Hir from '../src/Hir.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import * as LlvmBackend from '../src/LlvmBackend.js'
import * as Mir from '../src/Mir.js'
import * as MirNormalization from '../src/MirNormalization.js'
import * as MirVerification from '../src/MirVerification.js'
import * as RowAlgebra from '../src/RowAlgebra.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as Stdlib from '../src/Stdlib.js'
import * as Type from '../src/Type.js'
import * as WasmBackend from '../src/WasmBackend.js'
import {
  heterogeneousFailurePayload,
  heterogeneousOwnedFailurePayload,
  heterogeneousOwnedFailureResultDrop,
} from './support/corpus.js'
import * as Json from './support/Json.js'
import * as Projections from './support/projections.js'
import { unreachable } from './support/raise.js'
import * as WasmMain from './support/WasmMain.js'

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

const replaceMirOperation = (
  module: Mir.Module,
  target: Mir.Operation,
  replacement: Mir.Operation,
): Mir.Module => {
  const rewrite = (operation: Mir.Operation): Mir.Operation => {
    if (operation === target) return replacement
    if (operation._tag === 'ShortCircuit')
      return Object.freeze({
        ...operation,
        right: Object.freeze({
          ...operation.right,
          operations: Object.freeze(operation.right.operations.map(rewrite)),
        }),
      })
    if (operation._tag === 'Conditional')
      return Object.freeze({
        ...operation,
        taken: Object.freeze({
          ...operation.taken,
          operations: Object.freeze(operation.taken.operations.map(rewrite)),
        }),
        otherwise: Object.freeze({
          ...operation.otherwise,
          operations: Object.freeze(operation.otherwise.operations.map(rewrite)),
        }),
      })
    if (operation._tag !== 'Match') return operation
    return Object.freeze({
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
                    operations: Object.freeze(arm.guard.operations.map(rewrite)),
                  }),
                }),
            selected: Object.freeze({
              ...arm.selected,
              operations: Object.freeze(arm.selected.operations.map(rewrite)),
            }),
          }),
        ),
      ),
    })
  }
  return Object.freeze({
    ...module,
    functions: Object.freeze(
      module.functions.map((fn) =>
        Object.freeze({
          ...fn,
          regions: Object.freeze(
            fn.regions.map((region): Mir.Region =>
              region._tag === 'OperationRegion'
                ? Object.freeze({
                    ...region,
                    operations: Object.freeze(region.operations.map(rewrite)),
                  })
                : region,
            ),
          ),
        }),
      ),
    ),
  })
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

const runtimeMatrixSource = `import silk.effect as Effect
struct Selected { code: i32 }
struct Residual { code: i32 }
effect fn risky(mode: i32) -> i32 ! Selected | Residual {
  if mode == 0 { return 10 }
  if mode == 1 { fail Selected { code: 10 } }
  fail Residual { code: 16 }
}
effect fn recoverSelected(problem: Selected) -> i32 { return problem.code + 4 }
effect fn recoverResidual(problem: Residual) -> i32 { return problem.code + 2 }
effect fn selective(mode: i32) -> i32 ! Residual {
  return run Effect.catch<Selected>(risky(mode), recoverSelected)
}
effect fn completed(mode: i32) -> i32 {
  return run Effect.catchAll(selective(mode), recoverResidual)
}
pub fn main() -> i32 {
  return (run completed(0)) + (run completed(1)) + (run completed(2))
}`

const cleanupPreamble = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.layout { Layout }
import silk.effect as Effect
struct Selected { code: i32 }
struct Residual { code: i32 }
struct HandlerProblem { code: i32 }
struct Guard { value: i32 storage: Allocation }
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    if self.value == 0 { let boom = 1 / 0 return () }
    self.value = 0
    return ()
  }
}
effect fn makeGuard(value: i32) -> Guard ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let storage = run recipe
  return Guard { value: value, storage: move storage }
}
effect fn succeed() -> i32 ! Selected | Residual { return 10 }
effect fn failSelected() -> i32 ! Selected | Residual { fail Selected { code: 10 } }
effect fn failResidual() -> i32 ! Selected | Residual { fail Residual { code: 20 } }
effect fn recoverSelected(problem: Selected) -> i32 { return problem.code + 1 }
effect fn recoverSelectedWithGuard(problem: Selected, guard: Guard) -> i32 {
  return problem.code + guard.value
}
effect fn failFromHandler(problem: Selected) -> i32 ! HandlerProblem {
  fail HandlerProblem { code: problem.code + 20 }
}
effect fn recoverResidual(problem: Residual) -> i32 { return problem.code + 2 }
effect fn recoverAny(problem: Residual | HandlerProblem) -> i32 { return 30 }
`

const borrowedMatchSource = (catch_: string): string => `import silk.effect as Effect
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

const borrowedResidualMatchSource = (catch_: string): string => `import silk.effect as Effect
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
  let caught = ${catch_}(risky(), recover(&token))
  let result = match move choice {
    Left {} => run move caught
    Right {} => run move caught
  }
  return result
}
pub fn main() -> i32 {
  let left = Effect.catchAll(choose(Left {}), recoverResidual)
  let right = Effect.catchAll(choose(Right {}), recoverResidual)
  return (run move left) + (run move right)
}`

const runFailureLoanSource = `import silk.effect as Effect
struct Residual { code: i32 }
struct Token { value: i32 }
effect fn risky(value: i32) -> i32 ! Residual {
  if value == 0 { fail Residual { code: 20 } }
  return value
}
fn finish(value: i32, token: &Token) -> i32 { return value + token.value }
effect fn direct(value: i32) -> i32 ! Residual {
  let token = Token { value: 1 }
  let complete = finish(&token)
  let result = run risky(value)
  return complete(result)
}
effect fn stored(value: i32) -> i32 ! Residual {
  let token = Token { value: 2 }
  let complete = finish(&token)
  let pending = risky(value)
  let result = run move pending
  return complete(result)
}
effect fn recover(problem: Residual) -> i32 { return problem.code }
pub fn main() -> i32 {
  let first = Effect.catchAll(direct(1), recover)
  let second = Effect.catchAll(stored(1), recover)
  return (run move first) + (run move second)
}`

const infallibleRunLoanSource = `import silk.effect as Effect
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

it.effect('recovers one member and leaves the other in the result failure row', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch<A>(risky(flag), recoverA)
}
pub fn main() -> i32 { return run Effect.catchAll(selective(true), recoverB) }`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 11n)
  }),
)

it.effect('infers the singleton selector from the handler parameter', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
effect fn selective(flag: bool) -> i32 ! B {
  return run Effect.catch(risky(flag), recoverA)
}
pub fn main() -> i32 { return run Effect.catchAll(selective(true), recoverB) }`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 11n)
  }),
)

it.effect('forwards nominal-member evidence through an open generic wrapper', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
effect fn select<S, A, E>(
  self: once Effect<A ! E>,
  handler: once fn(S) -> Effect<A>
) -> A ! Without<E, S>
where S in E {
  return run Effect.catch(move self, move handler)
}
effect fn selective(flag: bool) -> i32 ! B {
  return run select<A>(risky(flag), recoverA)
}
    pub fn main() -> i32 { return run Effect.catchAll(selective(true), recoverB) }`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 11n)
  }),
)

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

it.effect('executes a reachable direct intrinsic through the ordinary target inventory', () =>
  Effect.gen(function* () {
    const source = `import silk.effect as Effect
${preamble}
effect fn recoverRest(problem: B) -> i32 { return problem.code + 3 }
pub fn main() -> i32 {
  let selected = Intrinsic.catchFailure<A>(risky(true), recoverA)
  return run Effect.catchAll(move selected, recoverRest)
}
`
    const self = yield* analyze(source, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const calls = self.instances.intrinsics.filter(
      (call) =>
        call.span.sourceId === 'root' &&
        Intrinsic.operationText(call.operation) === 'Intrinsic.catchFailure',
    )
    assert.strictEqual(calls.length, 1)
    for (const target of Intrinsic.executionTargets) {
      assert.strictEqual(IntrinsicAvailability.select(calls, target)._tag, 'Available', target)
    }
    const representedCatch = self.instances.effects.find(
      (effect) => effect.site.ordinal >= 0x40000000,
    )
    assert.isDefined(representedCatch)
    assert.strictEqual(self.layout._tag, 'Available')
    if (representedCatch !== undefined && self.layout._tag === 'Available')
      assert.isDefined(
        self.layout.value.effectEnvironments.find(
          (environment) =>
            environment._tag === 'EffectEnvironment' &&
            Hir.sameExecutableSite(environment.site, representedCatch.site),
        ),
      )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 11n)

    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    assert.strictEqual(
      yield* WasmMain.invoke(wasm.bytes, 'SelectiveCatch.invokeDirectIntrinsicWasm'),
      11,
    )
  }),
)

it.effect('releases an owned handler captured by a stored direct intrinsic', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
${cleanupPreamble}
effect fn direct() -> i32 ! OutOfMemoryError {
  let guard = run makeGuard(1)
  let selected = Intrinsic.catchFailure<Selected>(
    succeed(),
    recoverSelectedWithGuard(move guard),
  )
  return run Effect.catchAll(move selected, recoverResidual)
}
effect fn recoverAllocation(problem: OutOfMemoryError) -> i32 { return 99 }
pub fn main() -> i32 { return run Effect.catchAll(direct(), recoverAllocation) }
`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', evaluated._tag)
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 10n)
    assert.strictEqual(
      evaluated.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
      ).length,
      1,
    )
  }),
)

it.effect('is independent of the authored failure-row member order', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
effect fn left() -> i32 ! A | B { fail A { code: 10 } }
effect fn right() -> i32 ! B | A { fail A { code: 10 } }
effect fn recoverRest(problem: B) -> i32 { return problem.code + 3 }
effect fn completedLeft() -> i32 {
  return run Effect.catchAll(Effect.catch<A>(left(), recoverA), recoverRest)
}
effect fn completedRight() -> i32 {
  return run Effect.catchAll(Effect.catch<A>(right(), recoverA), recoverRest)
}
pub fn main() -> i32 { return (run completedLeft()) + (run completedRight()) }
`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 22n)
  }),
)

it.effect('deduplicates nested generic wrapper runners reached from multiple source calls', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
effect fn select<S, A, E>(
  self: once Effect<A ! E>,
  handler: once fn(S) -> Effect<A>
) -> A ! Without<E, S>
where S in E {
  return run Intrinsic.catchFailure<S>(move self, move handler)
}
effect fn nested(flag: bool) -> i32 ! B {
  return run select<A>(risky(flag), recoverA)
}
effect fn recoverRest(problem: B) -> i32 { return problem.code + 3 }
effect fn completed(flag: bool) -> i32 {
  return run Effect.catchAll(nested(flag), recoverRest)
}
pub fn main() -> i32 { return (run completed(true)) + (run completed(false)) }
`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const calls = self.instances.intrinsics.filter(
      (call) =>
        call.span.sourceId === 'root' &&
        Intrinsic.operationText(call.operation) === 'Intrinsic.catchFailure',
    )
    assert.strictEqual(calls.length, 1)
    for (const target of Intrinsic.executionTargets)
      assert.strictEqual(IntrinsicAvailability.select(calls, target)._tag, 'Available', target)
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [])
    const runnerKeys = mir.functions
      .filter((fn) => fn.effectRunner !== undefined)
      .map((fn) => `${fn.id.module}.${fn.id.name}`)
    assert.strictEqual(new Set(runnerKeys).size, runnerKeys.length)
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 34n)
  }),
)

it.effect('unions handler failures with the nonselected remainder', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
pub struct C { code: i32 }
effect fn recoverAWithFailure(problem: A) -> i32 ! C { fail C { code: problem.code } }
effect fn selective(flag: bool) -> i32 ! B | C {
  return run Effect.catch<A>(risky(flag), recoverAWithFailure)
}
effect fn recoverRest(problem: B | C) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(selective(true), recoverRest) }`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 0n)
  }),
)

it.effect('reports the unrecovered member as still present in the result row', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
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
    const wholeRow = yield* analyze(`import silk.effect as Effect
${preamble}
pub fn main() -> i32 { return run Effect.catchAll(risky(true), recoverRow) }`)
    assert.deepEqual(codes(wholeRow), [])

    // The selective form keeps B, so the identical run site now has a failure left to handle.
    const selective = yield* analyze(`import silk.effect as Effect
${preamble}
pub fn main() -> i32 { return run Effect.catch<A>(risky(true), recoverA) }`)
    assert.deepEqual(codes(selective), ['SEM0066'])
  }),
)

it.effect('records the protected, selected, handler, and residual rows as facts', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
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
    const self = yield* analyze(`import silk.effect as Effect
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
    const self = yield* analyze(`import silk.effect as Effect
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
      const self = yield* analyze(`import silk.effect as Effect
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

it.effect('accepts a nonempty selected failure union', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
effect fn recoverUnion(problem: A | B) -> i32 { return 0 }
effect fn recovered(flag: bool) -> i32 {
  return run Effect.catch<A | B>(risky(flag), recoverUnion)
}
pub fn main() -> i32 { return run recovered(true) }`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 0n)
  }),
)

it.effect('joins different protected and handler success types as an ordinary union', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
struct ProblemError {}
effect fn risky() -> i32 ! ProblemError { fail ProblemError {} }
effect fn recover(problem: ProblemError) -> string { return "fallback" }
pub fn main() -> i32 {
  let recovered = run Effect.catchAll(risky(), recover)
  drop recovered
  return 0
}`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 0n)
  }),
)

it.effect('carries an ordinary string directly through the failure channel', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
effect fn failText() -> i32 ! string { fail "oops" }
effect fn recoverText(error: string) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(failText(), recoverText) }`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

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
    const self = yield* analyze(`import silk.effect as Effect
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

it.effect('executes the direct selector form', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
effect fn recoverRest(problem: B) -> i32 { return problem.code + 3 }
pub fn main() -> i32 {
  let selected = Effect.catch<A>(risky(true), recoverA)
  return run Effect.catchAll(move selected, recoverRest)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 11n)
  }),
)

for (const [label, catch_] of [
  ['direct intrinsic', 'Intrinsic.catchFailure<Selected>'],
  ['ordinary wrapper', 'Effect.catch<Selected>'],
] as const)
  it.effect(
    `retains borrowed handler captures through inline, stored, and branched ${label} execution`,
    () =>
      Effect.gen(function* () {
        const source = `import silk.effect as Effect
struct Selected { code: i32 }
struct Token { value: i32 }
effect fn risky(shouldFail: bool) -> i32 ! Selected {
  if shouldFail { fail Selected { code: 1 } }
  return 10
}
effect fn recover(problem: Selected, token: &Token) -> i32 {
  return problem.code + token.value
}
effect fn inlineCatch(shouldFail: bool) -> i32 {
  let token = Token { value: 41 }
  return run ${catch_}(risky(shouldFail), recover(&token))
}
fn branch(flag: bool, shouldFail: bool) -> i32 {
  let token = Token { value: 41 }
  let caught = ${catch_}(risky(shouldFail), recover(&token))
  if flag {
    let alias = move caught
    return run move alias
  }
  return run move caught
}
fn swapped(flag: bool, shouldFail: bool) -> i32 {
  let token = Token { value: 41 }
  let caught = ${catch_}(risky(shouldFail), recover(&token))
  if flag { return run move caught }
  let first = move caught
  let second = move first
  return run move second
}
pub fn main() -> i32 {
  return (run inlineCatch(false)) + (run inlineCatch(true))
    + branch(true, true) + branch(false, true)
    + swapped(true, false) + swapped(false, false)
}`
        const self = yield* analyze(source, 'wasm32-unknown-unknown')
        assert.deepEqual(Analysis.diagnostics(self), [])
        assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
        const evaluated = Analysis.evaluate(self)
        assert.strictEqual(
          evaluated._tag,
          'Completed',
          JSON.stringify(evaluated, Json.bigIntReplacer),
        )
        if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 156n)

        const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
        assert.strictEqual(
          yield* WasmMain.invoke(artifact.bytes, 'SelectiveCatch.invokeBorrowedHandlerWasm'),
          156,
        )
      }),
    180_000,
  )

it.effect('requires failure-path loan endings on every MIR run form', () =>
  Effect.gen(function* () {
    const raw = yield* Analysis.ofSourceRealized(
      'test/run-failure-loans',
      ascii(runFailureLoanSource),
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

    type FailureRun = Extract<
      Mir.Operation,
      { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'RunStaticEffect' }
    >
    const findRun = (module: Mir.Module, tag: FailureRun['_tag']): FailureRun => {
      const candidate = module.functions
        .flatMap(MirVerification.operations)
        .find(
          (operation): operation is FailureRun =>
            operation._tag === tag &&
            Type.failureMembers(operation.outcomeType.type).length > 0 &&
            (operation.failureLoanEnds?.length ?? 0) > 0,
        )
      return (
        candidate ??
        unreachable(
          `expected ${tag} with failure-path loan cleanup; saw ${module.functions
            .flatMap(MirVerification.operations)
            .flatMap((operation) =>
              operation._tag === 'RunEffect' ||
              operation._tag === 'RunEffectValue' ||
              operation._tag === 'RunStaticEffect'
                ? [`${operation._tag}:${operation.failureLoanEnds?.length ?? 0}`]
                : [],
            )
            .join(',')}`,
        )
      )
    }
    const storedCandidate = findRun(rawModule, 'RunEffectValue')
    const stored =
      storedCandidate._tag === 'RunEffectValue'
        ? storedCandidate
        : unreachable('expected stored Effect run')
    const staticCandidate = findRun(normalizedModule, 'RunStaticEffect')
    const static_ =
      staticCandidate._tag === 'RunStaticEffect'
        ? staticCandidate
        : unreachable('expected normalized static Effect run')

    const replace = (
      module: Mir.Module,
      target: FailureRun,
      replacement: FailureRun,
    ): Mir.Module => {
      const rewrite = (operation: Mir.Operation): Mir.Operation => {
        if (operation === target) return replacement
        if (operation._tag === 'ShortCircuit')
          return Object.freeze({
            ...operation,
            right: Object.freeze({
              ...operation.right,
              operations: Object.freeze(operation.right.operations.map(rewrite)),
            }),
          })
        if (operation._tag === 'Match')
          return Object.freeze({
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
                          operations: Object.freeze(arm.guard.operations.map(rewrite)),
                        }),
                      }),
                  selected: Object.freeze({
                    ...arm.selected,
                    operations: Object.freeze(arm.selected.operations.map(rewrite)),
                  }),
                }),
              ),
            ),
          })
        return operation
      }
      return Object.freeze({
        ...module,
        functions: Object.freeze(
          module.functions.map((fn) =>
            Object.freeze({
              ...fn,
              regions: Object.freeze(
                fn.regions.map((region): Mir.Region =>
                  region._tag === 'OperationRegion'
                    ? Object.freeze({
                        ...region,
                        operations: Object.freeze(region.operations.map(rewrite)),
                      })
                    : region,
                ),
              ),
            }),
          ),
        ),
      })
    }
    const withCrossRegionFailureEndings = (
      module: Mir.Module,
      target: FailureRun,
      parentFirst: boolean,
    ): Mir.Module => {
      let rewritten = false
      const rewrittenModule: Mir.Module = Object.freeze({
        ...module,
        functions: Object.freeze(
          module.functions.map((fn) => {
            if (!MirVerification.operations(fn).includes(target)) return fn
            const unavailable = new Set(
              MirVerification.operationLocals(target).map((local) => local.ordinal),
            )
            const rootOrdinal = fn.localTypes.findIndex(
              (type, ordinal) => type._tag === 'Nominal' && !unavailable.has(ordinal),
            )
            const rootType = fn.localTypes.at(rootOrdinal)
            if (rootType?._tag !== 'Nominal')
              return unreachable(`expected an independent owner for ${target._tag}`)
            const seed =
              target.failureLoanEnds?.at(0) ?? unreachable(`expected ${target._tag} failure ending`)
            const parent = Object.freeze({
              ...seed.borrow,
              callSpan: target.provenance.span,
              ordinal: seed.borrow.ordinal + 0x4000_0000,
            })
            const child = Object.freeze({ ...parent, ordinal: parent.ordinal + 1 })
            const parentSlice = Object.freeze({
              _tag: 'Local' as const,
              ordinal: fn.localTypes.length,
            })
            const childSlice = Object.freeze({
              _tag: 'Local' as const,
              ordinal: fn.localTypes.length + 1,
            })
            const referenceType: Extract<Mir.Type, { readonly _tag: 'Reference' }> = Object.freeze({
              _tag: 'Reference',
              type: Type.reference('Shared', rootType.type),
            })
            const parentBeginning: Extract<Mir.Operation, { readonly _tag: 'BeginLoan' }> =
              Object.freeze({
                _tag: 'BeginLoan',
                borrow: parent,
                destination: parentSlice,
                root: Object.freeze({ _tag: 'Local', ordinal: rootOrdinal }),
                selectors: Object.freeze([]),
                sourceType: rootType,
                type: referenceType,
                access: 'Shared',
                reborrow: false,
                suspendsParent: false,
                provenance: target.provenance,
              })
            const childBeginning: Extract<Mir.Operation, { readonly _tag: 'BeginLoan' }> =
              Object.freeze({
                _tag: 'BeginLoan',
                borrow: child,
                destination: childSlice,
                root: parentSlice,
                selectors: Object.freeze([]),
                sourceType: referenceType,
                type: referenceType,
                access: 'Shared',
                reborrow: true,
                suspendsParent: false,
                provenance: target.provenance,
              })
            const parentEnding: Mir.EndLoanOperation = Object.freeze({
              _tag: 'EndLoan',
              borrow: parent,
              slice: parentSlice,
              provenance: target.provenance,
            })
            const childEnding: Mir.EndLoanOperation = Object.freeze({
              _tag: 'EndLoan',
              borrow: child,
              slice: childSlice,
              provenance: target.provenance,
            })
            const malformed = Object.freeze({
              ...target,
              failureLoanEnds: Object.freeze([
                ...(parentFirst ? [parentEnding, childEnding] : [childEnding, parentEnding]),
                ...(target.failureLoanEnds ?? []),
              ]),
            })
            const added: Array<Mir.Region> = []
            const followingId = Object.freeze({
              _tag: 'Region' as const,
              ordinal: Math.max(...fn.regions.map((region) => region.id.ordinal)) + 1,
            })
            const regions = fn.regions.map((region): Mir.Region => {
              if (region._tag !== 'OperationRegion') return region
              const targetIndex = region.operations.indexOf(target)
              if (targetIndex < 0) return region
              rewritten = true
              added.push(
                Object.freeze({
                  ...region,
                  id: followingId,
                  operations: Object.freeze([
                    malformed,
                    childEnding,
                    parentEnding,
                    ...region.operations.slice(targetIndex + 1),
                  ]),
                }),
              )
              return Object.freeze({
                ...region,
                operations: Object.freeze([
                  ...region.operations.slice(0, targetIndex),
                  parentBeginning,
                  childBeginning,
                ]),
                outcome: Object.freeze({
                  _tag: 'Forward' as const,
                  target: followingId,
                  provenance: target.provenance,
                }),
              })
            })
            return Object.freeze({
              ...fn,
              localTypes: Object.freeze([...fn.localTypes, referenceType, referenceType]),
              regions: Object.freeze([...regions, ...added]),
            })
          }),
        ),
      })
      return rewritten ? rewrittenModule : unreachable(`expected to split ${target._tag} region`)
    }
    const propagationType =
      stored.propagationType ?? unreachable('expected stored failure propagation type')
    const direct: Extract<Mir.Operation, { readonly _tag: 'RunEffect' }> = Object.freeze({
      _tag: 'RunEffect',
      destination: stored.destination,
      outcome: stored.outcome,
      target: stored.runner,
      typeArguments: stored.runnerTypeArguments,
      arguments: Object.freeze([stored.effect, ...stored.arguments]),
      outcomeType: stored.outcomeType,
      propagationType,
      tagMappings: stored.tagMappings,
      failureLoanEnds:
        stored.failureLoanEnds ?? unreachable('expected stored failure-path loan cleanup'),
      ...(stored.releases === undefined ? {} : { releases: stored.releases }),
      propagationLaneCount: stored.propagationLaneCount,
      type: stored.type,
      provenance: stored.provenance,
    })
    const directModule = replace(rawModule, stored, direct)
    assert.notInclude(
      MirVerification.verify(directModule).map((violation) => violation.rule),
      'InvalidLoan',
    )
    for (const [module, operation] of [
      [directModule, direct],
      [rawModule, stored],
      [normalizedModule, static_],
    ] as const)
      assert.include(
        MirVerification.verify(
          replace(
            module,
            operation,
            Object.freeze({ ...operation, failureLoanEnds: Object.freeze([]) }),
          ),
        ).map((violation) => violation.rule),
        'InvalidLoan',
      )

    for (const [module, operation, propagationRule] of [
      [directModule, direct, 'InvalidEffectOperation'],
      [rawModule, stored, 'InvalidEffectOperation'],
      [normalizedModule, static_, 'InvalidNormalization'],
    ] as const) {
      const missingPropagation = structuredClone(module)
      const cloned = MirVerification.operations(
        missingPropagation.functions.find((fn) =>
          MirVerification.operations(fn).some(
            (candidate) =>
              candidate._tag === operation._tag &&
              candidate.provenance.span.sourceId === operation.provenance.span.sourceId &&
              candidate.provenance.span.start === operation.provenance.span.start &&
              candidate.provenance.span.end === operation.provenance.span.end,
          ),
        ) ?? unreachable(`expected cloned ${operation._tag} owner`),
      ).find(
        (candidate) =>
          candidate._tag === operation._tag &&
          candidate.provenance.span.sourceId === operation.provenance.span.sourceId &&
          candidate.provenance.span.start === operation.provenance.span.start &&
          candidate.provenance.span.end === operation.provenance.span.end,
      )
      const clonedRun =
        cloned?._tag === 'RunEffect' ||
        cloned?._tag === 'RunEffectValue' ||
        cloned?._tag === 'RunStaticEffect'
          ? cloned
          : unreachable(`expected cloned ${operation._tag}`)
      Reflect.deleteProperty(clonedRun, 'propagationType')
      Reflect.set(clonedRun, 'tagMappings', Object.freeze([]))
      assert.include(
        MirVerification.verify(missingPropagation).map((violation) => violation.rule),
        propagationRule,
        `${operation._tag} must not erase a fallible outcome boundary`,
      )

      const firstMapping =
        operation.tagMappings.at(0) ??
        unreachable(`expected ${operation._tag} failure propagation mapping`)
      for (const [field, tag] of [
        ['source', 0],
        ['source', -1],
        ['source', Number.MAX_SAFE_INTEGER + 1],
        ['target', 0],
        ['target', -1],
        ['target', Number.MAX_SAFE_INTEGER + 1],
      ] as const) {
        const forgedMapping = Object.freeze({ ...firstMapping, [field]: tag })
        assert.include(
          MirVerification.verify(
            replace(
              module,
              operation,
              Object.freeze({
                ...operation,
                tagMappings: Object.freeze([forgedMapping, ...operation.tagMappings.slice(1)]),
              }),
            ),
          ).map((violation) => violation.rule),
          propagationRule,
          `${operation._tag} must reject non-canonical ${field} tag ${tag}`,
        )
      }

      const firstEnding =
        operation.failureLoanEnds?.at(0) ?? unreachable(`expected ${operation._tag} failure ending`)
      const wrongSlice = Object.freeze({ ...firstEnding, slice: operation.destination })
      assert.include(
        MirVerification.verify(
          replace(
            module,
            operation,
            Object.freeze({
              ...operation,
              failureLoanEnds: Object.freeze([
                wrongSlice,
                ...(operation.failureLoanEnds?.slice(1) ?? []),
              ]),
            }),
          ),
        ).map((violation) => violation.rule),
        'InvalidLoan',
        `${operation._tag} must validate its failure-only slice against the beginning`,
      )

      const owner =
        module.functions.find((fn) => MirVerification.operations(fn).includes(operation)) ??
        unreachable(`expected ${operation._tag} owner`)
      const undeclared = Object.freeze({
        ...firstEnding,
        slice: Object.freeze({ _tag: 'Local' as const, ordinal: owner.localTypes.length }),
      })
      const undeclaredViolations = MirVerification.verify(
        replace(
          module,
          operation,
          Object.freeze({
            ...operation,
            failureLoanEnds: Object.freeze([
              undeclared,
              ...(operation.failureLoanEnds?.slice(1) ?? []),
            ]),
          }),
        ),
      ).map((violation) => violation.rule)
      assert.include(undeclaredViolations, 'UndeclaredLocal')
      assert.include(undeclaredViolations, 'InvalidLoan')
      const validAncestry = withCrossRegionFailureEndings(module, operation, false)
      assert.notInclude(
        MirVerification.verify(validAncestry).map((violation) => violation.rule),
        'InvalidLoan',
        `${operation._tag} must accept child-before-parent failure cleanup across regions`,
      )
      assert.include(
        MirVerification.verify(withCrossRegionFailureEndings(module, operation, true)).map(
          (violation) => violation.rule,
        ),
        'InvalidLoan',
        `${operation._tag} must carry parent/child state across regions on its failure path`,
      )
    }

    const evaluated = Analysis.evaluate(raw)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 5n)
    const wasm = yield* Analysis.codegenWasm(raw, { mode: 'release' })
    assert.strictEqual(
      yield* WasmMain.invoke(wasm.bytes, 'SelectiveCatch.invokeFailureLoanRunFormsWasm'),
      5,
    )
    const llvm = yield* Backend.emit(LlvmBackend.LlvmBackend, normalizedModule, { mode: 'release' })
    assert.strictEqual(llvm._tag, 'LlvmBitcodeArtifact')
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

for (const [label, catch_] of [
  ['direct intrinsic', 'Intrinsic.catchFailure<Selected>'],
  ['ordinary wrapper', 'Effect.catch<Selected>'],
] as const)
  it.effect(
    `ends borrowed ${label} handler loans before residual failure leaves a match arm`,
    () =>
      Effect.gen(function* () {
        const snapshot = yield* analyze(
          borrowedResidualMatchSource(catch_),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [])
        const module = Analysis.loweredMir(snapshot)
        assert.deepEqual(MirVerification.verify(module), [])
        const choose =
          module.functions.find((fn) => fn.id.name === 'choose$effect$-1') ??
          unreachable('expected effectful choose runner')
        const failureRuns = MirVerification.operations(choose).filter(
          (
            operation,
          ): operation is Extract<
            Mir.Operation,
            { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'RunStaticEffect' }
          > =>
            (operation._tag === 'RunEffect' ||
              operation._tag === 'RunEffectValue' ||
              operation._tag === 'RunStaticEffect') &&
            Type.failureMembers(operation.outcomeType.type).length > 0,
        )
        assert.isAtLeast(failureRuns.length, 2)
        assert.isTrue(
          failureRuns.every((operation) => (operation.failureLoanEnds?.length ?? 0) > 0),
        )
        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(
          evaluated._tag,
          'Completed',
          JSON.stringify(evaluated, Json.bigIntReplacer),
        )
        if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 44n)
        const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        assert.strictEqual(
          yield* WasmMain.invoke(artifact.bytes, 'SelectiveCatch.invokeResidualMatchWasm'),
          44,
        )
      }),
    180_000,
  )

for (const [label, catch_] of [
  ['direct intrinsic', 'Intrinsic.catchFailure<Selected>'],
  ['ordinary wrapper', 'Effect.catch<Selected>'],
] as const)
  it.effect(
    `ends a borrowed ${label} handler on every exhaustive match arm`,
    () =>
      Effect.gen(function* () {
        const source = borrowedMatchSource(catch_)
        const self = yield* analyze(source, 'wasm32-unknown-unknown')
        assert.deepEqual(Analysis.diagnostics(self), [])
        assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])

        const evaluated = Analysis.evaluate(self)
        assert.strictEqual(
          evaluated._tag,
          'Completed',
          JSON.stringify(evaluated, Json.bigIntReplacer),
        )
        if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 84n)

        const artifact = yield* Analysis.codegenWasm(self, { mode: 'release' })
        assert.strictEqual(
          yield* WasmMain.invoke(artifact.bytes, 'SelectiveCatch.invokeBorrowedHandlerMatchWasm'),
          84,
        )
      }),
    180_000,
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
      `import silk.effect as Effect
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

it.effect('executes the pipelined form and propagates the nonselected member', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`import silk.effect as Effect
${preamble}
effect fn recoverRest(problem: B) -> i32 { return problem.code + 3 }
pub fn main() -> i32 {
  let selected = risky(false) |> Effect.catch<A>(recoverA)
  return run Effect.catchAll(move selected, recoverRest)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 23n)
  }),
)

it.effect(
  'keeps success, selected recovery, and residual propagation aligned on evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze(runtimeMatrixSource, 'wasm32-unknown-unknown')
      const mir = Analysis.loweredMir(snapshot)
      assert.deepEqual(MirVerification.verify(mir), [])
      assert.isAtLeast(
        mir.functions
          .flatMap(MirVerification.operations)
          .filter((operation) => operation._tag === 'PropagateEffectFailure').length,
        1,
      )
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, Json.bigIntReplacer),
      )
      if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

      const propagation =
        Analysis.loweredMir(snapshot)
          .functions.flatMap((fn) =>
            fn.regions.flatMap((region) => {
              if (region._tag !== 'OperationRegion') return []
              return region.operations.flatMap(Mir.operationTree).flatMap((operation) => {
                if (operation._tag === 'PropagateEffectFailure') {
                  return [Object.freeze({ fn, region, operation })]
                }
                return []
              })
            }),
          )
          .at(0) ?? unreachable('expected selective residual propagation in MIR')
      const wasmArtifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const propagationReturns = wasmArtifact.control.filter(
        (entry) =>
          entry.construct === 'WasmReturn' &&
          entry.function.module === propagation.fn.id.module &&
          entry.function.name === propagation.fn.id.name &&
          entry.region.ordinal === propagation.region.id.ordinal &&
          entry.span.sourceId === propagation.operation.provenance.span.sourceId &&
          entry.span.start === propagation.operation.provenance.span.start &&
          entry.span.end === propagation.operation.provenance.span.end,
      )
      assert.lengthOf(propagationReturns, 1)
      assert.strictEqual(
        yield* WasmMain.invoke(wasmArtifact.bytes, 'SelectiveCatch.invokeRuntimeMatrixWasm'),
        42,
      )
    }),
)

it.effect('repacks heterogeneous failure payload carriers without changing member bits', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(heterogeneousFailurePayload, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const module = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(module), [])
    const recoverIndex = module.functions.findIndex((fn) => fn.id.name.startsWith('recoverAny'))
    const recover =
      module.functions.at(recoverIndex) ?? unreachable('expected the whole-row recovery function')
    const unionSource =
      recover.localTypes
        .map((type, ordinal) =>
          Object.freeze({ type, local: Object.freeze({ _tag: 'Local' as const, ordinal }) }),
        )
        .find(
          (
            candidate,
          ): candidate is {
            readonly type: Extract<Mir.Type, { readonly _tag: 'Union' }>
            readonly local: Mir.LocalId
          } => candidate.type._tag === 'Union',
        ) ?? unreachable('expected the recovery union parameter')
    const targetType =
      module.functions
        .flatMap((fn) => fn.localTypes)
        .find(
          (type): type is Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> =>
            type._tag === 'EffectOutcome' &&
            Type.failureMembers(type.type).length === unionSource.type.type.members.length &&
            unionSource.type.type.members.every((member) =>
              Type.failureMembers(type.type).some((failure) => Type.equals(failure, member)),
            ),
        ) ?? unreachable('expected the widened failure outcome type')
    const mappings = Object.freeze(
      unionSource.type.type.members.map((member, source) => {
        const target = Type.failureMembers(targetType.type).findIndex((failure) =>
          Type.equals(failure, member),
        )
        return target < 0
          ? unreachable('expected the union member in the widened failure row')
          : Object.freeze({ source, target: target + 1 })
      }),
    )
    const destination: Mir.LocalId = Object.freeze({
      _tag: 'Local',
      ordinal: recover.localTypes.length,
    })
    const packProvenance =
      MirVerification.operations(recover).at(0)?.provenance ??
      unreachable('expected recovery operation provenance')
    const pack: Extract<Mir.Operation, { readonly _tag: 'PackEffectFailureUnion' }> = Object.freeze(
      {
        _tag: 'PackEffectFailureUnion',
        destination,
        source: unionSource.local,
        sourceType: unionSource.type,
        mappings,
        type: targetType,
        provenance: packProvenance,
      },
    )
    let inserted = false
    const packedRecover: Mir.MirFunction = Object.freeze({
      ...recover,
      localTypes: Object.freeze([...recover.localTypes, targetType]),
      regions: Object.freeze(
        recover.regions.map((region): Mir.Region =>
          !inserted && region._tag === 'OperationRegion'
            ? (() => {
                inserted = true
                return Object.freeze({
                  ...region,
                  operations: Object.freeze([pack, ...region.operations]),
                })
              })()
            : region,
        ),
      ),
    })
    if (!inserted) unreachable('expected an operation region in the recovery function')
    const packedModule: Mir.Module = Object.freeze({
      ...module,
      functions: Object.freeze([
        ...module.functions.slice(0, recoverIndex),
        packedRecover,
        ...module.functions.slice(recoverIndex + 1),
      ]),
    })
    assert.deepEqual(MirVerification.verify(packedModule), [])
    const firstPackMapping = mappings.at(0) ?? unreachable('expected first pack mapping')
    const secondPackMapping = mappings.at(1) ?? unreachable('expected second pack mapping')
    for (const forgedMappings of [
      Object.freeze([firstPackMapping, firstPackMapping]),
      Object.freeze([secondPackMapping, firstPackMapping]),
    ]) {
      const forged = replaceMirOperation(
        packedModule,
        pack,
        Object.freeze({ ...pack, mappings: forgedMappings }),
      )
      assert.include(
        MirVerification.verify(forged).map((violation) => violation.rule),
        'InvalidEffectOperation',
      )
    }
    for (const [field, tag] of [
      ['source', -1],
      ['source', Number.MAX_SAFE_INTEGER + 1],
      ['target', 0],
      ['target', -1],
      ['target', Number.MAX_SAFE_INTEGER + 1],
    ] as const) {
      const forged = replaceMirOperation(
        packedModule,
        pack,
        Object.freeze({
          ...pack,
          mappings: Object.freeze([
            Object.freeze({ ...firstPackMapping, [field]: tag }),
            ...mappings.slice(1),
          ]),
        }),
      )
      assert.include(
        MirVerification.verify(forged).map((violation) => violation.rule),
        'InvalidEffectOperation',
      )
    }
    const operations = packedModule.functions.flatMap(MirVerification.operations)
    assert.isTrue(
      operations.some((operation) => operation._tag === 'PropagateEffectFailure'),
      'expected the selective residual propagation path',
    )
    const outcomePack =
      operations.find(
        (operation): operation is Extract<Mir.Operation, { readonly _tag: 'PackEffectOutcome' }> =>
          operation._tag === 'PackEffectOutcome' && operation.tag > 0,
      ) ?? unreachable('expected a packed failure outcome')
    for (const tag of [-1, Number.MAX_SAFE_INTEGER + 1, Number.NaN]) {
      assert.include(
        MirVerification.verify(
          replaceMirOperation(packedModule, outcomePack, Object.freeze({ ...outcomePack, tag })),
        ).map((violation) => violation.rule),
        'InvalidEffectOperation',
      )
    }
    assert.isTrue(
      operations.some(
        (operation) =>
          (operation._tag === 'RunEffect' ||
            operation._tag === 'RunEffectValue' ||
            operation._tag === 'RunStaticEffect') &&
          operation.tagMappings.length > 0,
      ),
      'expected a propagated run boundary',
    )
    const evaluated = BootstrapEvaluation.evaluate(snapshot.instances, packedModule)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
    const artifact = yield* Backend.emit(WasmBackend.WasmBackend, packedModule, {
      mode: 'release',
    })
    assert.strictEqual(artifact._tag, 'WebAssemblyModuleArtifact')
    if (artifact._tag !== 'WebAssemblyModuleArtifact') return
    assert.strictEqual(
      yield* WasmMain.invoke(artifact.bytes, 'SelectiveCatch.invokeHeterogeneousFailureWasm'),
      42,
    )
  }),
)

it.effect(
  'preserves and releases detached address payloads through a floating failure carrier',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze(heterogeneousOwnedFailurePayload, 'wasm32-unknown-unknown')
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, Json.bigIntReplacer),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)
      assert.deepEqual(
        evaluated.trace.flatMap((event) =>
          event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease'
            ? [event._tag]
            : [],
        ),
        ['AllocationAcquire', 'AllocationRelease'],
      )

      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      assert.strictEqual(
        yield* WasmMain.invoke(artifact.bytes, 'SelectiveCatch.invokeOwnedFailurePayloadWasm'),
        42,
      )
    }),
)

it.effect('releases a reified owned residual directly from a floating union carrier', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(heterogeneousOwnedFailureResultDrop, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      evaluated.trace.flatMap((event) =>
        event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease'
          ? [event._tag]
          : [],
      ),
      ['AllocationAcquire', 'AllocationRelease'],
    )

    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    assert.strictEqual(
      yield* WasmMain.invoke(artifact.bytes, 'SelectiveCatch.invokeOwnedFailureResultDropWasm'),
      42,
    )
  }),
)

it.effect('preserves structured prefixes while propagating zero-based union failure tags', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      `import silk.effect as Effect
struct A { code: i32 }
struct B { code: i32 }
struct C { code: i32 }
effect fn risky(mode: i32) -> i32 ! A | B | C {
  if mode == 1 { fail B { code: 20 } }
  if mode == 2 { fail C { code: 30 } }
  fail A { code: 10 }
}
effect fn recoverA(problem: A) -> i32 ! A { fail A { code: problem.code + 100 } }
effect fn recoverAll(problem: A | B | C) -> i32 {
  return match move problem {
    A { code } => code + 1
    B { code } => code + 2
    C { code } => code + 3
  }
}
effect fn selected(mode: i32) -> i32 ! A | B | C {
  return run Effect.catch<A>(risky(mode), recoverA)
}
pub fn main() -> i32 { return run Effect.catchAll(selected(1), recoverAll) }`,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const module = Analysis.loweredMir(snapshot)
    type UnionSource = {
      readonly local: Mir.LocalId
      readonly type: Extract<Mir.Type, { readonly _tag: 'Union' }>
    }
    let rewritten = 0
    const rewriteOperations = (
      operations: ReadonlyArray<Mir.Operation>,
      enclosing?: UnionSource,
    ): ReadonlyArray<Mir.Operation> =>
      Object.freeze(
        operations.map((operation): Mir.Operation => {
          if (operation._tag === 'ShortCircuit')
            return Object.freeze({
              ...operation,
              right: Object.freeze({
                ...operation.right,
                operations: rewriteOperations(operation.right.operations, enclosing),
              }),
            })
          if (operation._tag === 'Conditional')
            return Object.freeze({
              ...operation,
              taken: Object.freeze({
                ...operation.taken,
                operations: rewriteOperations(operation.taken.operations, enclosing),
              }),
              otherwise: Object.freeze({
                ...operation.otherwise,
                operations: rewriteOperations(operation.otherwise.operations, enclosing),
              }),
            })
          if (operation._tag === 'Match') {
            const source =
              operation.scrutineeType._tag === 'Union'
                ? Object.freeze({
                    local: operation.scrutinee,
                    type: operation.scrutineeType,
                  })
                : enclosing
            return Object.freeze({
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
                            operations: rewriteOperations(arm.guard.operations, source),
                          }),
                        }),
                    selected: Object.freeze({
                      ...arm.selected,
                      operations: rewriteOperations(arm.selected.operations, source),
                    }),
                  }),
                ),
              ),
            })
          }
          if (operation._tag !== 'PropagateEffectFailure' || enclosing === undefined)
            return operation
          const failures = Type.failureMembers(operation.propagationType.type)
          const tagMappings = Object.freeze(
            enclosing.type.type.members.map((member, source) => {
              const target = failures.findIndex((failure) => Type.equals(failure, member))
              return target < 0
                ? unreachable('expected propagated union member in the enclosing failure row')
                : Object.freeze({ source, target: target + 1 })
            }),
          )
          rewritten += 1
          return Object.freeze({
            ...operation,
            source: enclosing.local,
            sourceType: enclosing.type,
            tagMappings,
          })
        }),
      )
    const mutated: Mir.Module = Object.freeze({
      ...module,
      functions: Object.freeze(
        module.functions.map((fn) =>
          Object.freeze({
            ...fn,
            regions: Object.freeze(
              fn.regions.map((region): Mir.Region =>
                region._tag === 'OperationRegion'
                  ? Object.freeze({
                      ...region,
                      operations: rewriteOperations(region.operations),
                    })
                  : region,
              ),
            ),
          }),
        ),
      ),
    })
    assert.isAbove(rewritten, 0)
    assert.deepEqual(MirVerification.verify(mutated), [])
    const propagation =
      mutated.functions
        .flatMap(MirVerification.operations)
        .find(
          (
            operation,
          ): operation is Extract<Mir.Operation, { readonly _tag: 'PropagateEffectFailure' }> =>
            operation._tag === 'PropagateEffectFailure' && operation.tagMappings.length > 1,
        ) ?? unreachable('expected a multi-member failure propagation')
    const firstPropagationMapping =
      propagation.tagMappings.at(0) ?? unreachable('expected first propagation mapping')
    const secondPropagationMapping =
      propagation.tagMappings.at(1) ?? unreachable('expected second propagation mapping')
    for (const tagMappings of [
      Object.freeze([firstPropagationMapping, firstPropagationMapping, firstPropagationMapping]),
      Object.freeze([
        secondPropagationMapping,
        firstPropagationMapping,
        propagation.tagMappings.at(2) ?? firstPropagationMapping,
      ]),
    ]) {
      const forged = replaceMirOperation(
        mutated,
        propagation,
        Object.freeze({ ...propagation, tagMappings }),
      )
      assert.include(
        MirVerification.verify(forged).map((violation) => violation.rule),
        'InvalidEffectOperation',
      )
    }
    for (const [field, tag] of [
      ['source', -1],
      ['source', Number.MAX_SAFE_INTEGER + 1],
      ['target', 0],
      ['target', -1],
      ['target', Number.MAX_SAFE_INTEGER + 1],
    ] as const) {
      const forged = replaceMirOperation(
        mutated,
        propagation,
        Object.freeze({
          ...propagation,
          tagMappings: Object.freeze([
            Object.freeze({ ...firstPropagationMapping, [field]: tag }),
            ...propagation.tagMappings.slice(1),
          ]),
        }),
      )
      assert.include(
        MirVerification.verify(forged).map((violation) => violation.rule),
        'InvalidEffectOperation',
      )
    }
    const evaluated = BootstrapEvaluation.evaluate(snapshot.instances, mutated)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated, Json.bigIntReplacer))
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 22n)
    const artifact = yield* Backend.emit(WasmBackend.WasmBackend, mutated, { mode: 'release' })
    assert.strictEqual(artifact._tag, 'WebAssemblyModuleArtifact')
    if (artifact._tag !== 'WebAssemblyModuleArtifact') return
    assert.strictEqual(
      yield* WasmMain.invoke(artifact.bytes, 'SelectiveCatch.invokeUnionPropagationWasm'),
      22,
    )

    const mixedOrderSpan =
      SourceSpan.fromOffsets('test/selective-catch-mixed-order', 0, 0) ??
      unreachable('expected a valid mixed-order span')
    let mixedOrderRegion: Mir.RegionId | undefined
    let mixedOrderMatch: Extract<Mir.Operation, { readonly _tag: 'Match' }> | undefined
    let mixedOrderPropagation:
      | Extract<Mir.Operation, { readonly _tag: 'PropagateEffectFailure' }>
      | undefined
    const injectStructuredPrefix = (
      operations: ReadonlyArray<Mir.Operation>,
      region: Mir.RegionId,
    ): ReadonlyArray<Mir.Operation> =>
      Object.freeze(
        operations.map((operation): Mir.Operation => {
          if (operation._tag === 'ShortCircuit')
            return Object.freeze({
              ...operation,
              right: Object.freeze({
                ...operation.right,
                operations: injectStructuredPrefix(operation.right.operations, region),
              }),
            })
          if (operation._tag === 'Conditional')
            return Object.freeze({
              ...operation,
              taken: Object.freeze({
                ...operation.taken,
                operations: injectStructuredPrefix(operation.taken.operations, region),
              }),
              otherwise: Object.freeze({
                ...operation.otherwise,
                operations: injectStructuredPrefix(operation.otherwise.operations, region),
              }),
            })
          if (operation._tag !== 'Match') return operation
          return Object.freeze({
            ...operation,
            arms: Object.freeze(
              operation.arms.map((arm) => {
                const selected = injectStructuredPrefix(arm.selected.operations, region)
                const propagationIndex =
                  mixedOrderRegion === undefined
                    ? selected.findIndex((candidate) => candidate._tag === 'PropagateEffectFailure')
                    : -1
                if (propagationIndex < 0)
                  return Object.freeze({
                    ...arm,
                    ...(arm.guard === undefined
                      ? {}
                      : {
                          guard: Object.freeze({
                            ...arm.guard,
                            operations: injectStructuredPrefix(arm.guard.operations, region),
                          }),
                        }),
                    selected: Object.freeze({ ...arm.selected, operations: selected }),
                  })
                const propagation = selected.at(propagationIndex)
                if (propagation?._tag !== 'PropagateEffectFailure')
                  return unreachable('expected residual propagation at the selected index')
                const prefix: Extract<Mir.Operation, { readonly _tag: 'Match' }> = Object.freeze({
                  ...operation,
                  provenance: Object.freeze({ ...operation.provenance, span: mixedOrderSpan }),
                })
                mixedOrderRegion = region
                mixedOrderMatch = prefix
                mixedOrderPropagation = propagation
                return Object.freeze({
                  ...arm,
                  ...(arm.guard === undefined
                    ? {}
                    : {
                        guard: Object.freeze({
                          ...arm.guard,
                          operations: injectStructuredPrefix(arm.guard.operations, region),
                        }),
                      }),
                  selected: Object.freeze({
                    ...arm.selected,
                    operations: Object.freeze([
                      ...selected.slice(0, propagationIndex),
                      prefix,
                      ...selected.slice(propagationIndex),
                    ]),
                  }),
                })
              }),
            ),
          })
        }),
      )
    const mixedOrder: Mir.Module = Object.freeze({
      ...mutated,
      functions: Object.freeze(
        mutated.functions.map((fn) =>
          Object.freeze({
            ...fn,
            regions: Object.freeze(
              fn.regions.map((region): Mir.Region =>
                region._tag === 'OperationRegion'
                  ? Object.freeze({
                      ...region,
                      operations: injectStructuredPrefix(region.operations, region.id),
                    })
                  : region,
              ),
            ),
          }),
        ),
      ),
    })
    const selectedRegion =
      mixedOrderRegion ?? unreachable('expected a region with structured residual propagation')
    const selectedMatch = mixedOrderMatch ?? unreachable('expected a structured prefix')
    const selectedPropagation =
      mixedOrderPropagation ?? unreachable('expected residual propagation after the prefix')
    assert.deepEqual(MirVerification.verify(mixedOrder), [])
    const llvm = yield* Backend.emit(LlvmBackend.LlvmBackend, mixedOrder, { mode: 'release' })
    assert.isTrue(
      llvm.control.some(
        (entry) =>
          entry.construct === 'LlvmJump' &&
          entry.region.ordinal === selectedRegion.ordinal &&
          entry.span.sourceId === mixedOrderSpan.sourceId &&
          entry.span.start === selectedMatch.provenance.span.start &&
          entry.span.end === selectedMatch.provenance.span.end,
      ),
    )
    assert.isTrue(
      llvm.control.some(
        (entry) =>
          entry.construct === 'LlvmReturn' &&
          entry.region.ordinal === selectedRegion.ordinal &&
          entry.span.start === selectedPropagation.provenance.span.start &&
          entry.span.end === selectedPropagation.provenance.span.end,
      ),
    )
  }),
)

it.effect('releases catch-owned handlers and live owners once before recovery', () =>
  Effect.gen(function* () {
    for (const [name, body, expected, recovery] of [
      [
        'success-bypass',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
effect fn layer() -> i32 ! Residual | OutOfMemoryError {
  let guard = run makeGuard(1)
  let selected = Effect.catch<Selected>(succeed(), recoverSelectedWithGuard(move guard))
  return run move selected
}
effect fn recoverTop(problem: Residual | OutOfMemoryError) -> i32 { return 99 }
pub fn main() -> i32 { return run Effect.catchAll(layer(), recoverTop) }`,
        10,
        undefined,
      ],
      [
        'residual-propagation',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
effect fn layer() -> i32 ! Residual | OutOfMemoryError {
  let guard = run makeGuard(1)
  return run Effect.catch<Selected>(failResidual(), recoverSelected)
}
effect fn recoverTop(problem: Residual | OutOfMemoryError) -> i32 { return 22 }
pub fn main() -> i32 { return run Effect.catchAll(layer(), recoverTop) }`,
        22,
        'recoverTop',
      ],
      [
        'handler-failure',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
effect fn layer() -> i32 ! Residual | HandlerProblem | OutOfMemoryError {
  let guard = run makeGuard(1)
  return run Effect.catch<Selected>(failSelected(), failFromHandler)
}
effect fn recoverTop(problem: Residual | HandlerProblem | OutOfMemoryError) -> i32 { return 30 }
pub fn main() -> i32 { return run Effect.catchAll(layer(), recoverTop) }`,
        30,
        'recoverTop',
      ],
    ] as const) {
      const self = yield* analyze(`${cleanupPreamble}${body}`)
      assert.deepEqual(Analysis.diagnostics(self), [], name)
      const evaluated = Analysis.evaluate(self)
      assert.strictEqual(evaluated._tag, 'Completed', name)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, BigInt(expected), name)
      const hookIndices = evaluated.trace.flatMap((event, index) =>
        event._tag === 'Call' && event.target.name.startsWith('drop@impl') ? [index] : [],
      )
      assert.strictEqual(hookIndices.length, 1, `${name} drop count`)
      if (recovery === undefined) continue
      const recoveryIndex = evaluated.trace.findIndex(
        (event) => event._tag === 'Call' && event.target.name.startsWith(recovery),
      )
      assert.isAtLeast(recoveryIndex, 0, `${name} recovery call`)
      assert.isBelow(hookIndices[0] ?? recoveryIndex, recoveryIndex, `${name} cleanup order`)
    }
  }),
)

it.effect('catch and catchAll no longer share a doc comment', () =>
  Effect.gen(function* () {
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
