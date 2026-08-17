import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ContinuationTransaction from '../src/ContinuationTransaction.js'
import * as Mir from '../src/Mir.js'

const encoder = new TextEncoder()

const source = `effect fn inner(value: once Effect<i32>) -> i32 ! OutOfMemory ? &mut Allocator {
  let retained = move value
  let left = run Effect.suspend(effect { return 1 })
  return left + run retained
}
effect fn middle(value: once Effect<i32>) -> i32 ! OutOfMemory ? &mut Allocator {
  let retained = move value
  let left = run inner(effect { return 1 })
  return left + run retained
}
effect fn outer(value: once Effect<i32>) -> i32 ! OutOfMemory ? &mut Allocator {
  let retained = move value
  let left = run middle(effect { return 1 })
  return left + run retained
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(outer(effect { return 42 }) |> Effect.provideMut(&mut allocator), recover)
}`

const analyze = () =>
  Analysis.ofSourceRealized(
    'continuation-transaction/main',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )

const sourceName = (point: Mir.SuspensionPointId): string => point.owner.declaration.name

it.effect('sweeps every frame-producing relay refusal transactionally', () =>
  Effect.gen(function* () {
    const self = yield* analyze()
    const program = Analysis.loweredMir(self)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(Mir.verify(program), [])
    const relays = program.functions.flatMap((fn) =>
      (fn.suspension?.regions ?? []).flatMap((region) => {
        if (region._tag !== 'RunSuspendableEffectRegion') return []
        const descriptor = region.relay.continuation
        if (descriptor === undefined) return []
        const relay = ContinuationTransaction.relay(program, fn, descriptor)
        return relay === undefined ? [] : [relay]
      }),
    )
    const chain = ['inner$effect$', 'middle$effect$', 'outer$effect$'].flatMap((name) => {
      const found = relays.find((relay) => sourceName(relay.descriptor.point).startsWith(name))
      return found === undefined ? [] : [found]
    })
    assert.lengthOf(chain, 3, relays.map((relay) => sourceName(relay.descriptor.point)).join(','))

    const outOfMemory = Object.freeze({ _tag: 'OutOfMemory', identity: 73 })
    for (let refusal = 1; refusal <= chain.length; refusal += 1) {
      const result = ContinuationTransaction.prepare(chain, outOfMemory, refusal)
      assert.strictEqual(result._tag, 'Refused')
      if (result._tag !== 'Refused') continue
      assert.strictEqual(result.failure, outOfMemory)
      assert.strictEqual(result.ordinal, refusal)
      assert.lengthOf(
        result.events.filter((event) => event._tag === 'Request'),
        refusal,
      )
      assert.lengthOf(
        result.events.filter((event) => event._tag === 'Acquire'),
        refusal - 1,
      )
      assert.lengthOf(
        result.events.filter((event) => event._tag === 'Reject'),
        1,
      )
      assert.lengthOf(
        result.events.filter((event) => event._tag === 'Publish'),
        0,
      )
      assert.lengthOf(
        result.events.filter((event) => event._tag === 'ChildStart'),
        0,
      )
      assert.deepEqual(
        result.events.filter((event) => event._tag === 'Reclaim').map((event) => event.ordinal),
        Array.from({ length: refusal - 1 }, (_value, index) => refusal - index - 1),
      )
      const cleaned = result.events
        .filter(
          (
            event,
          ): event is Extract<
            ContinuationTransaction.Event,
            { readonly _tag: 'FrameDrop' | 'SourceCleanup' }
          > => event._tag === 'FrameDrop' || event._tag === 'SourceCleanup',
        )
        .map((event) => `${event.ordinal}:${event.local.ordinal}`)
      assert.strictEqual(new Set(cleaned).size, cleaned.length)
    }
  }),
)

it.effect('publishes only after the complete chain is initialized', () =>
  Effect.gen(function* () {
    const self = yield* analyze()
    const program = Analysis.loweredMir(self)
    const chain = program.functions.flatMap((fn) =>
      (fn.suspension?.regions ?? []).flatMap((region) => {
        if (region._tag !== 'RunSuspendableEffectRegion') return []
        const descriptor = region.relay.continuation
        if (descriptor === undefined) return []
        const relay = ContinuationTransaction.relay(program, fn, descriptor)
        return relay === undefined ? [] : [relay]
      }),
    )
    const prepared = ContinuationTransaction.prepare(chain, Object.freeze({ _tag: 'Unused' }))
    assert.strictEqual(prepared._tag, 'Prepared')
    if (prepared._tag !== 'Prepared') return
    const firstPublish = prepared.events.findIndex((event) => event._tag === 'Publish')
    const lastInitialize = prepared.events.findLastIndex((event) => event._tag === 'Initialize')
    const childStart = prepared.events.findIndex((event) => event._tag === 'ChildStart')
    assert.isAbove(firstPublish, lastInitialize)
    assert.isAbove(childStart, firstPublish)
    assert.lengthOf(
      prepared.events.filter((event) => event._tag === 'Request'),
      chain.length,
    )
    assert.lengthOf(
      prepared.events.filter((event) => event._tag === 'Publish'),
      chain.length,
    )
    assert.lengthOf(
      prepared.events.filter((event) => event._tag === 'ChildStart'),
      1,
    )
  }),
)
