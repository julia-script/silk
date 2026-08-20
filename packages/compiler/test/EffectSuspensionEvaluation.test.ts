import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'

const encoder = new TextEncoder()

const inspect = (value: unknown): string =>
  JSON.stringify(value, (_key, member) => (typeof member === 'bigint' ? `${member}n` : member), 2)

const snapshot = (source: string) =>
  Analysis.ofSourceRealized(
    'effect-suspension-evaluation/main',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )

const evaluate = (source: string, options: Parameters<typeof Analysis.evaluate>[1] = {}) =>
  Effect.map(snapshot(source), (self) => Analysis.evaluate(self, options))

const suspended = `effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 2 })
}
effect fn program() -> i32 {
  let left = 40
  return left + run delayed()
}
pub fn main() -> i32 { return run program() }`

const frameEvents = (
  trace: ReadonlyArray<BootstrapEvaluation.TraceEvent>,
): ReadonlyArray<BootstrapEvaluation.CoroutineFrameTraceEvent> =>
  trace.filter(
    (event): event is BootstrapEvaluation.CoroutineFrameTraceEvent =>
      event._tag === 'SuspensionOrigin' ||
      event._tag === 'CoroutineFramePush' ||
      event._tag === 'CoroutineFrameStateTransition' ||
      event._tag === 'CoroutineFrameResume' ||
      event._tag === 'CoroutineFrameComplete' ||
      event._tag === 'SuspensionChildStart' ||
      event._tag === 'SuspensionChildComplete',
  )

it.effect('executes suspension through an iterative coroutine-frame machine', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(suspended)
    assert.strictEqual(outcome._tag, 'Completed', inspect(outcome))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    const events = frameEvents(outcome.trace)
    assert.lengthOf(
      events.filter((event) => event._tag === 'SuspensionOrigin'),
      1,
    )
    assert.isAbove(events.filter((event) => event._tag === 'CoroutineFramePush').length, 0)
    assert.isAbove(
      events.filter((event) => event._tag === 'CoroutineFrameStateTransition').length,
      0,
    )
    assert.lengthOf(
      events.filter((event) => event._tag === 'CoroutineFrameComplete'),
      events.filter((event) => event._tag === 'CoroutineFramePush').length,
    )
    assert.lengthOf(
      outcome.trace.filter((event) => event._tag === 'AllocationAcquire'),
      0,
    )
  }),
)

it.effect('creates no frame when a suspendable runner completes synchronously', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(`effect fn child() -> i32 { return 1 }
effect fn maybe(shouldSuspend: bool) -> i32 {
  if shouldSuspend { return run Effect.suspend(child()) }
  return 42
}
pub fn main() -> i32 { return run maybe(false) }`)
    assert.strictEqual(outcome._tag, 'Completed', inspect(outcome))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.deepEqual(frameEvents(outcome.trace), [])
  }),
)

it.effect('traps fatally when private execution-stack storage is exhausted', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(suspended, { maxExecutionStackBytes: 1 })
    assert.strictEqual(outcome._tag, 'Trap', inspect(outcome))
    if (outcome._tag !== 'Trap') return
    assert.strictEqual(outcome.reason, 'private execution stack exhausted')
    assert.lengthOf(
      outcome.trace.filter((event) => event._tag === 'CoroutineFrameComplete'),
      0,
    )
    assert.lengthOf(
      outcome.trace.filter((event) => event._tag === 'EffectFailure'),
      0,
    )
  }),
)

it.effect('resumes an unchanged typed failure through saved frame state', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(`struct BoomError {}
effect fn failing() -> i32 ! BoomError { fail BoomError {} }
effect fn delayed() -> i32 ! BoomError {
  return run Effect.suspend(failing())
}
effect fn recover(error: BoomError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(delayed(), recover) }`)
    assert.strictEqual(outcome._tag, 'Completed', inspect(outcome))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 7n)
    assert.isTrue(
      frameEvents(outcome.trace).some(
        (event) => event._tag === 'SuspensionChildComplete' && event.outcome === 'Failure',
      ),
    )
  }),
)

it.effect('preserves a source-level failure path across suspension', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(`struct BoomError {}
effect fn failing() -> () ! BoomError { fail BoomError {} }
effect fn delayed() -> () ! BoomError {
  return run Effect.suspend(failing())
}
pub effect fn main() -> () ! BoomError { return run delayed() }`)
    assert.strictEqual(outcome._tag, 'UnhandledFailure', inspect(outcome))
    if (outcome._tag !== 'UnhandledFailure') return
    assert.strictEqual(outcome.identity, 'effect-suspension-evaluation/main.BoomError')
    assert.isAbove(outcome.logicalPath.length, 1, inspect(outcome.logicalPath))
    assert.notInclude(
      outcome.logicalPath.map((frame) => frame.function.name),
      '$effect-entry',
    )
    assert.isTrue(
      frameEvents(outcome.trace).some((event) => event._tag === 'SuspensionChildComplete'),
    )
  }),
)

it.effect('reuses one invocation frame across repeated suspension states', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluate(`effect fn delayed(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
effect fn twice() -> i32 {
  let left = run delayed(20)
  let right = run delayed(22)
  return left + right
}
pub fn main() -> i32 { return run twice() }`)
    assert.strictEqual(outcome._tag, 'Completed', inspect(outcome))
    if (outcome._tag !== 'Completed') return
    const owned = frameEvents(outcome.trace).filter((event) =>
      event.function.name.startsWith('twice$effect$'),
    )
    const pushes = owned.filter((event) => event._tag === 'CoroutineFramePush')
    const transitions = owned.filter((event) => event._tag === 'CoroutineFrameStateTransition')
    const completions = owned.filter((event) => event._tag === 'CoroutineFrameComplete')
    assert.lengthOf(pushes, 1, inspect(owned))
    assert.isAtLeast(transitions.length, 2, inspect(owned))
    assert.lengthOf(completions, 1, inspect(owned))
    assert.strictEqual(pushes.at(0)?.ticket, completions.at(0)?.ticket)
    assert.isTrue(transitions.every((event) => event.ticket === pushes.at(0)?.ticket))
  }),
)

const depth = 20
const chain = Array.from({ length: depth }, (_value, ordinal) => {
  const next = ordinal + 1
  return `effect fn level${ordinal}() -> i32 {
  return 1 + run Effect.suspend(level${next}())
}`
}).join('\n')

const recursive = `${chain}
effect fn level${depth}() -> i32 { return 0 }
pub fn main() -> i32 { return run level0() }`

const recursiveSelf = `effect fn count(value: i32) -> i32 {
  if value == 0 { return 0 }
  let next = run Effect.suspend(effect { return value - 1 })
  let inner = run count(next)
  return inner + 1
}
pub fn main() -> i32 { return run count(1200) }`

it.effect('counts suspended parents in deterministic logical CallDepth', () =>
  Effect.gen(function* () {
    const blocked = yield* evaluate(recursive, { maxCallDepth: 12, maxSteps: 100_000 })
    assert.strictEqual(blocked._tag, 'Blocked', inspect(blocked))
    if (blocked._tag !== 'Blocked' || blocked.reason._tag !== 'EvaluationLimit') return
    assert.strictEqual(blocked.reason.kind, 'CallDepth')
    assert.strictEqual(blocked.reason.count, 12)
    assert.lengthOf(blocked.reason.activeFrames, 12)

    const completed = yield* evaluate(recursive, { maxCallDepth: 256, maxSteps: 100_000 })
    assert.strictEqual(completed._tag, 'Completed', inspect(completed))
    if (completed._tag === 'Completed') assert.strictEqual(completed.result.value, 20n)
  }),
)

it.effect(
  'blocks suspended self recursion at default CallDepth and completes at a raised limit',
  () =>
    Effect.gen(function* () {
      const self = yield* snapshot(recursiveSelf)
      assert.deepEqual(Analysis.diagnostics(self), [])
      const blocked = Analysis.evaluate(self, { maxSteps: 5_000_000 })
      assert.strictEqual(blocked._tag, 'Blocked', inspect(blocked))
      if (blocked._tag !== 'Blocked' || blocked.reason._tag !== 'EvaluationLimit') return
      assert.strictEqual(blocked.reason.kind, 'CallDepth')
      assert.strictEqual(blocked.reason.limit, 1_024)
      assert.strictEqual(blocked.reason.count, 1_024)

      const completed = Analysis.evaluate(self, {
        maxCallDepth: 2_048,
        maxSteps: 5_000_000,
      })
      assert.strictEqual(completed._tag, 'Completed', inspect(completed))
      if (completed._tag === 'Completed') assert.strictEqual(completed.result.value, 1_200n)
    }),
  30_000,
)

it.effect('keeps uncovered recursion valid and applies the ordinary runtime limit', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn recurse(value: i32) -> i32 {
  if value == 0 { return 0 }
  return 1 + recurse(value - 1)
}
pub fn main() -> i32 { return recurse(80) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const completed = Analysis.evaluate(self, { maxCallDepth: 128 })
    assert.strictEqual(completed._tag, 'Completed', inspect(completed))
    if (completed._tag === 'Completed') assert.strictEqual(completed.result.value, 80n)
  }),
)
