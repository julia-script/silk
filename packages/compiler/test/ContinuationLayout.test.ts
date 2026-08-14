import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ContinuationLayout from '../src/ContinuationLayout.js'
import * as Mir from '../src/Mir.js'

const encoder = new TextEncoder()

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

const analyze = (source = suspended) =>
  Analysis.ofSourceRealized(
    'continuation-layout/main',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )

it.effect('plans one complete target allocation for every frame-producing relay', () =>
  Effect.gen(function* () {
    const first = yield* analyze()
    const second = yield* analyze()
    const left = Analysis.loweredMir(first)
    const right = Analysis.loweredMir(second)
    assert.deepEqual(Analysis.diagnostics(first), [])
    assert.deepEqual(Mir.verify(left), [])
    assert.isDefined(left.continuations)
    assert.isDefined(right.continuations)
    if (left.continuations === undefined || right.continuations === undefined) return
    assert.strictEqual(
      ContinuationLayout.summary(left.continuations),
      ContinuationLayout.summary(right.continuations),
    )
    assert.strictEqual(Mir.encode(left), Mir.encode(right))
    const descriptors = left.functions.flatMap((fn) =>
      (fn.suspension?.regions ?? []).flatMap((region) =>
        region._tag === 'RunSuspendableEffectRegion' && region.relay.continuation !== undefined
          ? [region.relay.continuation]
          : [],
      ),
    )
    assert.lengthOf(left.continuations.entries, descriptors.length)
    const origins = left.functions.flatMap((fn) =>
      (fn.suspension?.regions ?? []).flatMap((region) =>
        region._tag === 'SuspendEffectRegion' ? [Object.freeze({ fn, region })] : [],
      ),
    )
    assert.lengthOf(origins, 1)
    for (const { fn, region } of origins) {
      const providerType = fn.localTypes.at(region.transfer.allocator.argument.ordinal)
      assert.strictEqual(providerType?._tag, 'Reference')
      if (providerType?._tag === 'Reference')
        assert.strictEqual(providerType.type.access, 'Exclusive')
    }
    for (const entry of left.continuations.entries) {
      assert.deepEqual(
        entry.header.map((field) => field.role),
        ['Parent', 'Resume', 'Reclaim', 'ReclaimContext'],
      )
      assert.deepEqual(
        entry.header.map((field) => field.offset),
        [0, 4, 8, 12],
      )
      assert.isAtLeast(entry.size, 16)
      assert.strictEqual(entry.size % entry.alignment, 0)
      assert.strictEqual(entry.publication._tag, 'AfterCompleteInitialization')
      assert.strictEqual(entry.acquisition.allocator._tag, 'IncomingTransferAllocator')
      assert.strictEqual(entry.acquisition.allocator.capability.name, 'Allocator')
      assert.deepEqual(entry.acquisition.retainedAuthority, ['Reclaim', 'ReclaimContext'])
      assert.deepEqual(entry.acquisition.order, [
        'Request',
        'EndAllocatorLoan',
        'Initialize',
        'Publish',
        'ChildReborrow',
        'ChildStart',
      ])
      assert.deepEqual(
        entry.payload.map((field) => field.slot),
        entry.payload.map((_field, ordinal) => ordinal),
      )
      assert.deepEqual(
        entry.initialization.map((prefix) => prefix.initialized),
        entry.payload.map((_field, ordinal) => ordinal).concat(entry.payload.length),
      )
      for (const prefix of entry.initialization)
        assert.deepEqual(
          prefix.fields,
          entry.payload.slice(0, prefix.initialized).map((field) => field.slot),
        )
    }
  }),
)

it.effect(
  'closes each continuation allocation loan before a nested child reborrows Allocator',
  () =>
    Effect.gen(function* () {
      const self = yield* analyze(`effect fn inner() -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return 1 })
}
effect fn outer() -> i32 ! OutOfMemory ? &mut Allocator {
  let retained = 40
  return retained + run Effect.suspend(effect { return run inner() })
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(outer() |> Effect.provideMut(&mut allocator), recover)
}`)
      const program = Analysis.loweredMir(self)
      assert.deepEqual(Analysis.diagnostics(self), [])
      assert.deepEqual(Mir.verify(program), [])
      const origins = program.functions.flatMap((fn) =>
        (fn.suspension?.regions ?? []).filter(
          (region): region is Extract<Mir.SuspensionRegion, { _tag: 'SuspendEffectRegion' }> =>
            region._tag === 'SuspendEffectRegion',
        ),
      )
      assert.isAtLeast(origins.length, 1)
      assert.isDefined(program.continuations)
      assert.isAtLeast(program.continuations?.entries.length ?? 0, 2)
      for (const entry of program.continuations?.entries ?? []) {
        const close = entry.acquisition.order.indexOf('EndAllocatorLoan')
        const publish = entry.acquisition.order.indexOf('Publish')
        const reborrow = entry.acquisition.order.indexOf('ChildReborrow')
        const start = entry.acquisition.order.indexOf('ChildStart')
        assert.isBelow(close, publish)
        assert.isBelow(close, reborrow)
        assert.isBelow(reborrow, start)
        assert.deepEqual(entry.acquisition.retainedAuthority, ['Reclaim', 'ReclaimContext'])
      }
    }),
)

it.effect('does not attach physical continuation storage to synchronous MIR', () =>
  Effect.gen(function* () {
    const self = yield* analyze(
      'effect fn seed(value: i32) -> i32 { return value } pub fn main() -> i32 { return run seed(42) }',
    )
    const program = Analysis.loweredMir(self)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(Mir.verify(program), [])
    assert.isUndefined(program.continuations)
    assert.notInclude(Mir.encode(program), 'continuation-target')
  }),
)

it.effect('rejects missing, stale, and physically incomplete continuation plans', () =>
  Effect.gen(function* () {
    const self = yield* analyze()
    const program = Analysis.loweredMir(self)
    const plan = program.continuations
    assert.isDefined(plan)
    if (plan === undefined) return
    const { continuations: _continuations, ...logicalProgram } = program
    const withoutPlan: Mir.Module = Object.freeze(logicalProgram)
    assert.isTrue(
      Mir.verify(withoutPlan).some((violation) => violation.rule === 'InvalidContinuationLayout'),
    )
    const first = plan.entries.at(0)
    assert.isDefined(first)
    if (first === undefined) return
    const malformed: Mir.Module = Object.freeze({
      ...program,
      continuations: Object.freeze({
        ...plan,
        entries: Object.freeze([
          Object.freeze({
            ...first,
            size: first.size + first.alignment,
            initialization: Object.freeze(first.initialization.slice(0, -1)),
          }),
          ...plan.entries.slice(1),
        ]),
      }),
    })
    assert.isTrue(
      Mir.verify(malformed).some((violation) => violation.rule === 'InvalidContinuationLayout'),
    )
    const stale: Mir.Module = Object.freeze({
      ...program,
      continuations: Object.freeze({
        ...plan,
        entries: Object.freeze([...plan.entries, first]),
      }),
    })
    assert.isTrue(
      Mir.verify(stale).some((violation) => violation.rule === 'InvalidContinuationLayout'),
    )
  }),
)
