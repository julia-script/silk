import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import * as TestExchange from '../src/TestExchange.js'

const nonce = Uint8Array.from({ length: 32 }, (_, index) => index)
const catalogDigest = Uint8Array.from({ length: 32 }, (_, index) => 255 - index)
const firstIdentity = '11'.repeat(32)
const secondIdentity = '22'.repeat(32)

const entries: ReadonlyArray<TestExchange.PlanEntry> = [
  {
    declarationIdentity: 'tests/Cases::cached',
    executionIdentity: firstIdentity,
    action: 'Cached',
  },
  {
    declarationIdentity: 'tests/Cases::ineligible',
    executionIdentity: undefined,
    action: 'Execute',
  },
  {
    declarationIdentity: 'tests/Cases::passing',
    executionIdentity: secondIdentity,
    action: 'Execute',
  },
]

const perTestPlan: TestExchange.PerTestPlan = { _tag: 'PerTest', nonce, entries }

const expectExchangeFailure = Effect.fnUntraced(function* <A, R>(
  effect: Effect.Effect<A, TestExchange.ExchangeError, R>,
) {
  const result = yield* Effect.result(effect)
  assert.isTrue(Result.isFailure(result))
  if (Result.isFailure(result)) assert.strictEqual(result.failure._tag, 'ExchangeError')
})

it.effect('round trips the per-test plan and admits one exact complete receipt', () =>
  Effect.gen(function* () {
    const planBytes = yield* TestExchange.encodePlan(perTestPlan)
    assert.deepEqual(yield* TestExchange.decodePlan(planBytes), perTestPlan)
    const digest = yield* TestExchange.planDigest(planBytes)
    const receipt: TestExchange.PerTestReceipt = {
      _tag: 'PerTest',
      nonce,
      planDigest: digest,
      entries: [
        {
          ordinal: 0n,
          declarationIdentity: entries[0]?.declarationIdentity ?? '',
          executionIdentity: firstIdentity,
          disposition: 'Cached',
        },
        {
          ordinal: 2n,
          declarationIdentity: entries[2]?.declarationIdentity ?? '',
          executionIdentity: secondIdentity,
          disposition: 'Passed',
        },
      ],
      counts: {
        discovered: 3n,
        selected: 2n,
        cached: 1n,
        executed: 1n,
        passed: 2n,
        failed: 0n,
      },
      status: 0,
    }
    const receiptBytes = yield* TestExchange.encodeReceipt(receipt)
    assert.deepEqual(
      yield* TestExchange.admitReceipt(perTestPlan, planBytes, receiptBytes, 0),
      receipt,
    )
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('round trips the exact compact layouts with unsigned 64-bit counts', () =>
  Effect.gen(function* () {
    const discovered = 9_007_199_254_740_993n
    const plan: TestExchange.UncachedPlan = {
      _tag: 'Uncached',
      nonce,
      catalogDigest,
      discovered,
    }
    const planBytes = yield* TestExchange.encodePlan(plan)
    assert.strictEqual(planBytes.length, 256)
    assert.deepEqual(yield* TestExchange.decodePlan(planBytes), plan)
    const receipt: TestExchange.UncachedReceipt = {
      _tag: 'Uncached',
      nonce,
      planDigest: yield* TestExchange.planDigest(planBytes),
      counts: {
        discovered,
        selected: 2n,
        cached: 0n,
        executed: 2n,
        passed: 1n,
        failed: 1n,
      },
      status: 1,
    }
    const receiptBytes = yield* TestExchange.encodeReceipt(receipt)
    assert.strictEqual(receiptBytes.length, 256)
    assert.deepEqual(yield* TestExchange.admitReceipt(plan, planBytes, receiptBytes, 1), receipt)
  }).pipe(Effect.provide(NodeServices.layer)),
)

it('preflights both directions before hits or allocation', () => {
  assert.strictEqual(TestExchange.selectMode(entries), 'PerTest')
  assert.strictEqual(
    TestExchange.selectMode([
      {
        declarationIdentity: 'x'.repeat(4300),
        executionIdentity: undefined,
        action: 'Execute',
      },
    ]),
    'Uncached',
  )
  assert.isFalse(
    TestExchange.admitsPerTestSizes(70_000n, BigInt(TestExchange.hardMaximumBytes + 1), 256n),
  )
  assert.isFalse(TestExchange.admitsPerTestSizes(1n, 4300n, 4384n))
  assert.isFalse(TestExchange.admitsPerTestSizes(0x1_0000_0000_0000_0000n, 1n, 1n))
})

it.effect('rejects duplicate, reordered, mismatched, truncated, and extra per-test data', () =>
  Effect.gen(function* () {
    const planBytes = yield* TestExchange.encodePlan(perTestPlan)
    const planDigest = yield* TestExchange.planDigest(planBytes)
    const base = (receiptEntries: ReadonlyArray<TestExchange.ReceiptEntry>) =>
      TestExchange.encodeReceipt({
        _tag: 'PerTest',
        nonce,
        planDigest,
        entries: receiptEntries,
        counts: {
          discovered: 3n,
          selected: BigInt(receiptEntries.length),
          cached: 0n,
          executed: BigInt(receiptEntries.length),
          passed: BigInt(receiptEntries.length),
          failed: 0n,
        },
        status: 0,
      })
    const receipt = (ordinal: bigint, declarationIdentity: string): TestExchange.ReceiptEntry => ({
      ordinal,
      declarationIdentity,
      executionIdentity: ordinal === 0n ? firstIdentity : secondIdentity,
      disposition: 'Passed',
    })
    for (const invalidEntries of [
      [receipt(0n, 'tests/Cases::cached'), receipt(0n, 'tests/Cases::cached')],
      [receipt(2n, 'tests/Cases::passing'), receipt(0n, 'tests/Cases::cached')],
      [receipt(0n, 'tests/Cases::wrong')],
    ]) {
      const bytes = yield* base(invalidEntries)
      yield* expectExchangeFailure(TestExchange.admitReceipt(perTestPlan, planBytes, bytes, 0))
    }
    const validBytes = yield* base([receipt(0n, 'tests/Cases::cached')])
    yield* expectExchangeFailure(
      TestExchange.admitReceipt(perTestPlan, planBytes, validBytes.slice(0, -1), 0),
    )
    const extra = new Uint8Array(validBytes.length + 1)
    extra.set(validBytes)
    yield* expectExchangeFailure(TestExchange.admitReceipt(perTestPlan, planBytes, extra, 0))
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('rejects wrong versions, modes, bindings, padding, counts, and statuses', () =>
  Effect.gen(function* () {
    const plan: TestExchange.UncachedPlan = {
      _tag: 'Uncached',
      nonce,
      catalogDigest,
      discovered: 2n,
    }
    const planBytes = yield* TestExchange.encodePlan(plan)
    for (const offset of [8, 12, 255]) {
      const damaged = Uint8Array.from(planBytes)
      damaged[offset] = (damaged.at(offset) ?? 0) + 1
      yield* expectExchangeFailure(TestExchange.decodePlan(damaged))
    }
    yield* expectExchangeFailure(TestExchange.decodePlan(planBytes.slice(0, -1)))
    yield* expectExchangeFailure(
      TestExchange.decodePlan(new Uint8Array(TestExchange.hardMaximumBytes + 1)),
    )

    const receipt: TestExchange.UncachedReceipt = {
      _tag: 'Uncached',
      nonce,
      planDigest: yield* TestExchange.planDigest(planBytes),
      counts: {
        discovered: 2n,
        selected: 2n,
        cached: 0n,
        executed: 2n,
        passed: 2n,
        failed: 0n,
      },
      status: 0,
    }
    const receiptBytes = yield* TestExchange.encodeReceipt(receipt)
    for (const offset of [8, 12, 16, 48, 96, 104, 112, 120, 128, 255]) {
      const damaged = Uint8Array.from(receiptBytes)
      damaged[offset] = (damaged.at(offset) ?? 0) + 1
      yield* expectExchangeFailure(TestExchange.admitReceipt(plan, planBytes, damaged, 0))
    }
    yield* expectExchangeFailure(TestExchange.admitReceipt(plan, planBytes, receiptBytes, 1))
    yield* expectExchangeFailure(
      TestExchange.admitReceipt(plan, planBytes, receiptBytes.slice(0, -1), 0),
    )
    yield* expectExchangeFailure(TestExchange.admitReceipt(plan, planBytes, new Uint8Array(257), 0))
  }).pipe(Effect.provide(NodeServices.layer)),
)
