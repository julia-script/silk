import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'

const encoder = new TextEncoder()
const source = `fn nativeWrapper() -> i32 { return Intrinsic.i32Add(20, 22) }
pub fn main() -> i32 { return 0 }`

const snapshot = (text: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('availability/main', encoder.encode(text), target)

const catalog = (): ReadonlyArray<Intrinsic.Operation> =>
  Intrinsic.all().flatMap((actor) => actor.operations)

it.effect('keeps loaded but unreachable intrinsic calls out of executable closure', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(source, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(self.instances.intrinsics, [])

    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    const llvm = yield* Analysis.codegen(self, { mode: 'release' })
    assert.notInclude(llvm.ir, 'silk_standard_stream_write_v1')
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    assert.deepEqual(wasm.hostImports, [])
  }),
)

it.effect('retains exact canonical identities and call provenance only when reachable', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(source.replace('return 0', 'return nativeWrapper()'))
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(
      self.instances.intrinsics.map((call) => ({
        operation: Intrinsic.operationText(call.operation),
        sourceId: call.span.sourceId,
      })),
      [{ operation: 'Intrinsic.i32Add', sourceId: 'availability/main' }],
    )
  }),
)

it.effect('does not grant compiler availability privilege to an ordinary declaration name', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn i32Add(left: i32, right: i32) -> i32 {
  return left
}
pub fn main() -> i32 { return i32Add(20, 22) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.isFalse(
      self.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.i32Add',
      ),
    )
  }),
)

it.effect(
  'diagnoses a reachable operation against a restricted sealed catalog deterministically',
  () =>
    Effect.gen(function* () {
      const self = yield* snapshot(source.replace('return 0', 'return nativeWrapper()'))
      const operation = Intrinsic.findOperation('Intrinsic', 'i32Add')
      if (operation === undefined) throw new Error('expected Intrinsic.i32Add')
      const restricted = Object.freeze({
        ...operation,
        targets: Object.freeze(['Evaluator', 'LLVM'] as const),
      })
      const fixtureCatalog = catalog().map((candidate) =>
        Intrinsic.operationText(candidate.id) === Intrinsic.operationText(operation.id)
          ? restricted
          : candidate,
      )
      const first = IntrinsicAvailability.select(self.instances.intrinsics, 'Wasm', fixtureCatalog)
      const second = IntrinsicAvailability.select(self.instances.intrinsics, 'Wasm', fixtureCatalog)
      const evaluator = IntrinsicAvailability.select(
        self.instances.intrinsics,
        'Evaluator',
        fixtureCatalog,
      )
      const llvm = IntrinsicAvailability.select(self.instances.intrinsics, 'LLVM', fixtureCatalog)
      assert.deepEqual(second, first)
      assert.strictEqual(evaluator._tag, 'Available')
      assert.strictEqual(llvm._tag, 'Available')
      assert.strictEqual(first._tag, 'Unavailable')
      if (first._tag === 'Unavailable') {
        assert.deepEqual(
          first.diagnostics.map((diagnostic) => ({
            code: diagnostic.code,
            message: diagnostic.message,
            sourceId: diagnostic.span.sourceId,
          })),
          [
            {
              code: 'SEM0093',
              message: 'Intrinsic.i32Add is unavailable for Wasm',
              sourceId: 'availability/main',
            },
          ],
        )
      }
    }),
)

it('requires normalized target metadata in every sealed inventory entry', () => {
  assert.deepEqual(Intrinsic.executionTargets, ['Evaluator', 'LLVM', 'Wasm'])
  assert.isTrue(
    Intrinsic.inventory().every(
      (entry) => JSON.stringify(entry.targets) === JSON.stringify(Intrinsic.executionTargets),
    ),
  )
})
