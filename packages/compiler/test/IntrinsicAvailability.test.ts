import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as ToolchainIntegrity from '../src/ToolchainIntegrity.js'

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

it.effect('does not grant enum projection privilege to an ordinary value declaration', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn value(input: i32) -> i32 { return input }
pub fn main() -> i32 { return value(42) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.isFalse(
      self.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.enumValue',
      ),
    )
  }),
)

it.effect('does not grant suspension privilege to a user-defined Effect.suspend', () =>
  Effect.gen(function* () {
    const root = `import user_effect as Effect
pub fn main() -> i32 {
  return run Effect.suspend(effect { return 42 })
}`
    const userEffect = `pub effect fn suspend(deferred: once Effect<i32>) -> i32 {
  return run deferred
}`
    const self = yield* Analysis.makeRealized({
      root: SourceFile.make('availability/user-main', encoder.encode(root)),
    }).pipe(
      Effect.provide(
        SourceResolver.memory(new Map([['user_effect', encoder.encode(userEffect)] as const])),
      ),
    )

    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.isTrue(
      self.instances.calls.some(
        (call) =>
          call.target.declaration.module === 'user_effect' &&
          call.target.declaration.name === 'suspend',
      ),
    )
    assert.isFalse(
      self.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.suspendEffect',
      ),
    )
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
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

it.effect('checks restricted intrinsics only after static arm selection', () =>
  Effect.gen(function* () {
    const program = (selected: boolean) => `fn choose(static selected: bool) -> i32 {
  static if selected { return Intrinsic.i32Add(20, 22) } else { return 0 }
}

pub fn main() -> i32 { return choose(${selected}) }`
    const inactive = yield* snapshot(program(false), 'wasm32-unknown-unknown')
    const active = yield* snapshot(program(true), 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(inactive), [])
    assert.deepEqual(Analysis.diagnostics(active), [])
    assert.isFalse(
      inactive.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.i32Add',
      ),
    )
    assert.isTrue(
      active.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.i32Add',
      ),
    )
    const operation = Intrinsic.findOperation('Intrinsic', 'i32Add')
    if (operation === undefined) throw new Error('expected Intrinsic.i32Add')
    const restrictedCatalog = catalog().map((candidate) =>
      Intrinsic.operationText(candidate.id) === Intrinsic.operationText(operation.id)
        ? Object.freeze({
            ...candidate,
            targets: Object.freeze(['Evaluator', 'LLVM'] as const),
          })
        : candidate,
    )
    assert.strictEqual(
      IntrinsicAvailability.select(inactive.instances.intrinsics, 'Wasm', restrictedCatalog)._tag,
      'Available',
    )
    assert.strictEqual(
      IntrinsicAvailability.select(active.instances.intrinsics, 'Wasm', restrictedCatalog)._tag,
      'Unavailable',
    )
  }),
)

it('requires normalized target metadata in every sealed inventory entry', () => {
  assert.deepEqual(Intrinsic.executionTargets, ['Evaluator', 'LLVM', 'Wasm'])
  assert.isTrue(
    Intrinsic.inventory().every((entry) => {
      let expected: ReadonlyArray<Intrinsic.ExecutionTarget> = Intrinsic.executionTargets
      if (entry.operation.startsWith('Intrinsic.os')) expected = ['Evaluator', 'LLVM']
      if (entry.phase === 'StaticOnly') expected = []
      return JSON.stringify(entry.targets) === JSON.stringify(expected)
    }),
  )
})

it('rejects static-only intrinsic leakage at runtime availability and integrity seams', () => {
  const operation = Intrinsic.findOperation('Intrinsic', 'targetProfile')
  const span = SourceSpan.fromOffsets('availability/static-only', 0, 0)
  assert.isDefined(operation)
  assert.isDefined(span)
  if (operation === undefined || span === undefined) return
  const calls = [{ _tag: 'ReachableIntrinsicCall' as const, operation: operation.id, span }]
  for (const target of Intrinsic.executionTargets) {
    const availability = IntrinsicAvailability.select(calls, target)
    assert.strictEqual(availability._tag, 'Unavailable')
    if (availability._tag === 'Unavailable')
      assert.deepEqual(availability.operations, ['Intrinsic.targetProfile'])
    const integrity = ToolchainIntegrity.validateTarget(
      ToolchainIntegrity.installed(),
      target,
      calls,
      [],
    )
    assert.strictEqual(integrity._tag, 'UnsupportedTarget')
    if (integrity._tag === 'UnsupportedTarget')
      assert.deepEqual(integrity.operations, ['Intrinsic.targetProfile'])
  }
  assert.isFalse(
    ToolchainIntegrity.installed().components.some(
      (component) =>
        component.kind === 'RuntimeSupport' && component.id.endsWith('/Intrinsic.targetProfile'),
    ),
  )
})

it.effect('does not admit targetProfile into runtime HIR', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      'pub fn main() -> u8 { return Intrinsic.targetProfile() }',
      'wasm32-unknown-unknown',
    )
    assert.isAbove(Analysis.diagnostics(self).length, 0)
    assert.isFalse(
      self.instances.intrinsics.some(
        (call) => Intrinsic.operationText(call.operation) === 'Intrinsic.targetProfile',
      ),
    )
  }),
)

it.effect('validates only reachable runtime support and selected provider identities', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(source.replace('return 0', 'return nativeWrapper()'))
    const installed = ToolchainIntegrity.installed()
    const withoutUnrelatedRuntime = ToolchainIntegrity.make(
      installed.components.filter(
        (component) => component.id !== 'runtime/LLVM/Intrinsic.boolEquals',
      ),
    )
    assert.strictEqual(
      ToolchainIntegrity.validateTarget(
        withoutUnrelatedRuntime,
        'LLVM',
        self.instances.intrinsics,
        [],
      )._tag,
      'Matched',
    )

    const withoutRequiredRuntime = ToolchainIntegrity.make(
      installed.components.filter((component) => component.id !== 'runtime/LLVM/Intrinsic.i32Add'),
    )
    const missingRuntime = ToolchainIntegrity.validateTarget(
      withoutRequiredRuntime,
      'LLVM',
      self.instances.intrinsics,
      [],
    )
    assert.strictEqual(missingRuntime._tag, 'Invalid')
    if (missingRuntime._tag === 'Invalid')
      assert.isTrue(
        missingRuntime.failures.some(
          (failure) =>
            failure.reason._tag === 'MissingComponent' &&
            failure.reason.id === 'runtime/LLVM/Intrinsic.i32Add',
        ),
      )

    const withoutProvider = ToolchainIntegrity.make(
      installed.components.filter((component) => component.id !== 'provider/silk/os_filesystem'),
    )
    const missingProvider = ToolchainIntegrity.validateTarget(
      withoutProvider,
      'LLVM',
      self.instances.intrinsics,
      ['silk/os_filesystem'],
    )
    assert.strictEqual(missingProvider._tag, 'Invalid')
  }),
)

it('distinguishes unsupported target inventory from a missing promised runtime', () => {
  const unavailable = Intrinsic.inventory().find(
    (entry) => entry.phase === 'Runtime' && !entry.targets.includes('Wasm'),
  )
  assert.isDefined(unavailable)
  if (unavailable === undefined) return
  const spelling = unavailable.operation.slice('Intrinsic.'.length)
  const operation = Intrinsic.findOperation('Intrinsic', spelling)
  const span = SourceSpan.fromOffsets('availability/target', 0, 0)
  assert.isDefined(operation)
  assert.isDefined(span)
  if (operation === undefined || span === undefined) return
  const result = ToolchainIntegrity.validateTarget(
    ToolchainIntegrity.installed(),
    'Wasm',
    [{ _tag: 'ReachableIntrinsicCall', operation: operation.id, span }],
    [],
  )
  assert.strictEqual(result._tag, 'UnsupportedTarget')
  if (result._tag === 'UnsupportedTarget')
    assert.deepEqual(result.operations, [unavailable.operation])
})
