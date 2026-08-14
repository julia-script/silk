import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'

const encoder = new TextEncoder()

const inspect = (value: unknown): string =>
  JSON.stringify(value, (_key, member) => (typeof member === 'bigint' ? `${member}n` : member), 2)

const evaluate = (source: string, options: Parameters<typeof Analysis.evaluate>[1] = {}) =>
  Effect.map(
    Analysis.ofSourceRealized(
      'effect-suspension-evaluation/main',
      encoder.encode(source),
      'wasm32-unknown-unknown',
    ),
    (snapshot) => Analysis.evaluate(snapshot, options),
  )

const suspended = `effect fn delayed() -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return 2 })
}
effect fn program() -> i32 ! OutOfMemory ? &mut Allocator {
  let left = 40
  return left + run delayed()
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(program() |> Effect.provideMut(&mut allocator), recover)
}`

it.effect('executes suspension through the heap activation machine', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(suspended)
    assert.strictEqual(outcome._tag, 'Completed', inspect(outcome))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42)
    const continuation = outcome.trace.filter(
      (event): event is BootstrapEvaluation.ContinuationTraceEvent =>
        event._tag === 'SuspensionOrigin' ||
        event._tag === 'ContinuationRequest' ||
        event._tag === 'ContinuationAcquire' ||
        event._tag === 'ContinuationLoanEnd' ||
        event._tag === 'ContinuationInitialize' ||
        event._tag === 'ContinuationPublish' ||
        event._tag === 'ContinuationResume' ||
        event._tag === 'ContinuationRelease' ||
        event._tag === 'SuspensionChildStart' ||
        event._tag === 'SuspensionChildComplete',
    )
    assert.lengthOf(
      continuation.filter((event) => event._tag === 'SuspensionOrigin'),
      1,
    )
    const requests = continuation.filter((event) => event._tag === 'ContinuationRequest')
    const releases = continuation.filter((event) => event._tag === 'ContinuationRelease')
    assert.isAbove(requests.length, 0)
    assert.lengthOf(releases, requests.length)
    for (const request of requests) {
      const same = (event: BootstrapEvaluation.ContinuationTraceEvent): boolean =>
        event.ordinal === request.ordinal
      const requestIndex = continuation.indexOf(request)
      const loanEnd = continuation.findIndex(
        (event) => event._tag === 'ContinuationLoanEnd' && same(event),
      )
      const initialize = continuation.findIndex(
        (event) => event._tag === 'ContinuationInitialize' && same(event),
      )
      const publish = continuation.findIndex(
        (event) => event._tag === 'ContinuationPublish' && same(event),
      )
      assert.isBelow(requestIndex, loanEnd)
      assert.isBelow(loanEnd, initialize)
      assert.isBelow(initialize, publish)
    }
  }),
)

it.effect('allocates nothing when a suspendable runner completes synchronously', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(`effect fn child() -> i32 { return 1 }
effect fn maybe(shouldSuspend: bool) -> i32 ! OutOfMemory ? &mut Allocator {
  if shouldSuspend { return run Effect.suspend(child()) }
  return 42
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(maybe(false) |> Effect.provideMut(&mut allocator), recover)
}`)
    assert.strictEqual(outcome._tag, 'Completed', inspect(outcome))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42)
    assert.lengthOf(
      outcome.trace.filter(
        (event) => event._tag === 'SuspensionOrigin' || event._tag === 'ContinuationRequest',
      ),
      0,
    )
  }),
)

it.effect('resumes an unchanged typed failure through saved continuations', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(`effect fn failing() -> i32 ! OutOfMemory {
  fail OutOfMemory {}
}
effect fn delayed() -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(failing())
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(delayed() |> Effect.provideMut(&mut allocator), recover)
}`)
    assert.strictEqual(outcome._tag, 'Completed', inspect(outcome))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 7)
    const childCompletions = outcome.trace.filter(
      (event): event is BootstrapEvaluation.ContinuationTraceEvent =>
        event._tag === 'SuspensionChildComplete',
    )
    assert.isAbove(childCompletions.length, 0)
    assert.isTrue(
      childCompletions.some((event) => event.outcome === 'Failure'),
      inspect(childCompletions),
    )
    assert.lengthOf(
      outcome.trace.filter((event) => event._tag === 'ContinuationRelease'),
      outcome.trace.filter((event) => event._tag === 'ContinuationAcquire').length,
    )
  }),
)

const depth = 20
const chain = Array.from({ length: depth }, (_value, ordinal) => {
  const next = ordinal + 1
  return `effect fn level${ordinal}() -> i32 ! OutOfMemory ? &mut Allocator {
  return 1 + run Effect.suspend(level${next}())
}`
}).join('\n')

const recursive = `${chain}
effect fn level${depth}() -> i32 ! OutOfMemory ? &mut Allocator { return 0 }
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(level0() |> Effect.provideMut(&mut allocator), recover)
}`

const recursiveSelf = `effect fn count(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  if value == 0 { return 0 }
  let next = run Effect.suspend(effect { return value - 1 })
  let inner = run count(next)
  return inner + 1
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(count(1200) |> Effect.provideMut(&mut allocator), recover)
}`

it.effect('keeps suspended parents in deterministic logical CallDepth', () =>
  Effect.gen(function* () {
    const blocked = yield* evaluate(recursive, { maxCallDepth: 12, maxSteps: 100_000 })
    assert.strictEqual(blocked._tag, 'Blocked', inspect(blocked))
    if (blocked._tag !== 'Blocked') return
    assert.strictEqual(blocked.reason._tag, 'EvaluationLimit')
    if (blocked.reason._tag !== 'EvaluationLimit') return
    assert.strictEqual(blocked.reason.kind, 'CallDepth')
    assert.strictEqual(blocked.reason.count, 12)
    assert.lengthOf(blocked.reason.activeFrames, 12)

    const completed = yield* evaluate(recursive, { maxCallDepth: 256, maxSteps: 100_000 })
    assert.strictEqual(completed._tag, 'Completed', inspect(completed))
    if (completed._tag === 'Completed') assert.strictEqual(completed.result.value, 20)
  }),
)

it.effect('blocks self recursion at default CallDepth and completes at a raised limit', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect-suspension-evaluation/self-depth',
      encoder.encode(recursiveSelf),
      'wasm32-unknown-unknown',
    )
    const blocked = Analysis.evaluate(snapshot, { maxSteps: 5_000_000 })
    assert.strictEqual(blocked._tag, 'Blocked', inspect(blocked))
    if (blocked._tag !== 'Blocked' || blocked.reason._tag !== 'EvaluationLimit') return
    assert.strictEqual(blocked.reason.kind, 'CallDepth')
    assert.strictEqual(blocked.reason.limit, 1_024)
    assert.strictEqual(blocked.reason.count, 1_024)

    const completed = Analysis.evaluate(snapshot, {
      maxCallDepth: 2_048,
      maxSteps: 5_000_000,
    })
    assert.strictEqual(completed._tag, 'Completed', inspect(completed))
    if (completed._tag === 'Completed') assert.strictEqual(completed.result.value, 1_200)
  }),
)
