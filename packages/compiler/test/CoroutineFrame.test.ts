import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CoroutineFrame from '../src/CoroutineFrame.js'
import * as Instances from '../src/Instances.js'
import type * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'

const encoder = new TextEncoder()

const suspended = `import silk.effect as Effect
effect fn delayed(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
effect fn program() -> i32 {
  let left = run delayed(40)
  let right = run delayed(2)
  return left + right
}
pub fn main() -> i32 { return run program() }`

const analyze = (source = suspended) =>
  Analysis.ofSourceRealized(
    'coroutine-frame/main',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )

it.effect('plans one deterministic maximum frame per specialized invocation', () =>
  Effect.gen(function* () {
    const first = yield* analyze()
    const second = yield* analyze()
    const left = Analysis.loweredMir(first)
    const right = Analysis.loweredMir(second)
    assert.deepEqual(Analysis.diagnostics(first), [])
    assert.deepEqual(MirVerification.verify(left), [])
    assert.isDefined(left.coroutineFrames)
    assert.isDefined(right.coroutineFrames)
    if (left.coroutineFrames === undefined || right.coroutineFrames === undefined) return
    assert.strictEqual(
      CoroutineFrame.summary(left.coroutineFrames),
      CoroutineFrame.summary(right.coroutineFrames),
    )
    assert.strictEqual(MirEncoding.encode(left), MirEncoding.encode(right))

    const descriptors = left.functions.flatMap((fn) =>
      fn.suspension?.frame === undefined ? [] : [fn.suspension.frame],
    )
    assert.lengthOf(left.coroutineFrames.entries, descriptors.length)
    for (const descriptor of descriptors) {
      const entry: Mir.CoroutineFrameTargetLayout | undefined = left.coroutineFrames.entries.find(
        (candidate) =>
          Instances.keyText(candidate.function) === Instances.keyText(descriptor.function),
      )
      assert.isDefined(entry)
      if (entry === undefined) continue
      assert.deepEqual(
        entry.header.map((field) => field.role),
        ['Parent', 'State'],
      )
      assert.deepEqual(
        entry.header.map((field) => field.offset),
        [0, 4],
      )
      assert.lengthOf(entry.states, descriptor.states.length)
      assert.strictEqual(entry.size % entry.alignment, 0)
      assert.isAtLeast(entry.size, Math.max(...entry.states.map((state) => state.size)))
      for (const state of entry.states) {
        assert.strictEqual(state.size % state.alignment, 0)
        assert.deepEqual(
          state.payload.map((field) => field.slot),
          state.payload.map((_field, ordinal) => ordinal),
        )
      }
    }
    assert.notInclude(MirEncoding.encode(left), 'Allocator')
    assert.notInclude(CoroutineFrame.summary(left.coroutineFrames), 'allocator')
  }),
)

it.effect('uses distinct resume states in one reusable invocation frame', () =>
  Effect.gen(function* () {
    const self = yield* analyze()
    const program = Analysis.loweredMir(self)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(MirVerification.verify(program), [])
    const owner = program.functions.find((fn) => fn.id.name.startsWith('program$effect$'))
    const descriptor = owner?.suspension?.frame
    assert.isDefined(descriptor)
    if (descriptor === undefined) return
    assert.isAtLeast(descriptor.states.length, 2)
    const frame = program.coroutineFrames?.entries.find(
      (entry) => Instances.keyText(entry.function) === Instances.keyText(descriptor.function),
    )
    assert.isDefined(frame)
    assert.lengthOf(frame?.states ?? [], descriptor.states.length)
    assert.strictEqual(
      new Set(descriptor.states.map((state) => state.point.ordinal)).size,
      descriptor.states.length,
    )
  }),
)

it.effect('keeps synchronous MIR free of coroutine-frame storage', () =>
  Effect.gen(function* () {
    const self = yield* analyze(
      'effect fn seed(value: i32) -> i32 { return value } pub fn main() -> i32 { return run seed(42) }',
    )
    const program = Analysis.loweredMir(self)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(MirVerification.verify(program), [])
    assert.isUndefined(program.coroutineFrames)
    assert.notInclude(MirEncoding.encode(program), 'coroutine-frame ')
  }),
)

it.effect('rejects missing, stale, and physically incomplete frame plans', () =>
  Effect.gen(function* () {
    const self = yield* analyze()
    const program = Analysis.loweredMir(self)
    const plan = program.coroutineFrames
    assert.isDefined(plan)
    if (plan === undefined) return
    const { coroutineFrames: _coroutineFrames, ...logicalProgram } = program
    const withoutPlan: Mir.Module = Object.freeze(logicalProgram)
    assert.isTrue(
      MirVerification.verify(withoutPlan).some(
        (violation) => violation.rule === 'InvalidCoroutineFrame',
      ),
    )
    const first = plan.entries.at(0)
    const firstState = first?.states.at(0)
    assert.isDefined(first)
    assert.isDefined(firstState)
    if (first === undefined || firstState === undefined) return
    const malformed: Mir.Module = Object.freeze({
      ...program,
      coroutineFrames: Object.freeze({
        ...plan,
        entries: Object.freeze([
          Object.freeze({
            ...first,
            states: Object.freeze([
              Object.freeze({ ...firstState, size: firstState.size + firstState.alignment }),
              ...first.states.slice(1),
            ]),
          }),
          ...plan.entries.slice(1),
        ]),
      }),
    })
    assert.isTrue(
      MirVerification.verify(malformed).some(
        (violation) => violation.rule === 'InvalidCoroutineFrame',
      ),
    )
    const stale: Mir.Module = Object.freeze({
      ...program,
      coroutineFrames: Object.freeze({
        ...plan,
        entries: Object.freeze([...plan.entries, first]),
      }),
    })
    assert.isTrue(
      MirVerification.verify(stale).some((violation) => violation.rule === 'InvalidCoroutineFrame'),
    )
  }),
)
