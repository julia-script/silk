import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as LocalSharedPayloadCleanup from '../src/LocalSharedPayloadCleanup.js'
import type * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Ownership from '../src/Ownership.js'
import * as Type from '../src/Type.js'
import { ordinaryStorageSource } from './support/ordinaryStorageSource.js'
import * as Projections from './support/projections.js'
import { raise } from './support/raise.js'

const bytes = new Uint8Array(
  readFileSync(new URL('./fixtures/owned-allocation-guard.silk', import.meta.url)),
)
const moduleName = 'owned-allocation-acceptance/main'
const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const evaluationDescription = (evaluated: ReturnType<typeof Analysis.evaluate>): string => {
  if (evaluated._tag !== 'Blocked') return evaluated._tag
  if (evaluated.reason._tag === 'InvalidMir') {
    return evaluated.reason.violations.map((violation) => violation.detail).join('\n')
  }
  return evaluated.reason._tag
}

const rewriteOperations = (
  self: Mir.Module,
  rewrite: (operation: Mir.Operation) => Mir.Operation,
): Mir.Module =>
  Object.freeze({
    ...self,
    functions: Object.freeze(
      self.functions.map((fn) =>
        Object.freeze({
          ...fn,
          regions: Object.freeze(
            fn.regions.map((region) => {
              if (region._tag === 'OperationRegion') {
                return Object.freeze({
                  ...region,
                  operations: Object.freeze(region.operations.map(rewrite)),
                })
              }
              if (region._tag === 'CleanupRegion') {
                return Object.freeze({
                  ...region,
                  releases: Object.freeze(
                    region.releases.map((release) => {
                      const rewritten = rewrite(release)
                      if (rewritten._tag === 'Drop' || rewritten._tag === 'EndLoan')
                        return rewritten
                      return release
                    }),
                  ),
                })
              }
              return region
            }),
          ),
        }),
      ),
    ),
  })

const appendAfterOperation = (
  self: Mir.Module,
  append: (operation: Mir.Operation) => ReadonlyArray<Mir.Operation>,
): Mir.Module =>
  Object.freeze({
    ...self,
    functions: Object.freeze(
      self.functions.map((fn) =>
        Object.freeze({
          ...fn,
          regions: Object.freeze(
            fn.regions.map((region) =>
              region._tag !== 'OperationRegion'
                ? region
                : Object.freeze({
                    ...region,
                    operations: Object.freeze(
                      region.operations.flatMap((operation) => [operation, ...append(operation)]),
                    ),
                  }),
            ),
          ),
        }),
      ),
    ),
  })

/**
 * The engines only agree by construction if they agree on the substrate, so the guard program
 * runs on the evaluator and Wasm rather than on the evaluator alone. The logical trace is
 * asserted on the evaluator because it is the only engine that publishes one; Wasm is held to
 * the observable result the trace predicts. Native agreement on this program is proven by the
 * differential corpus (`support/corpus.ts`).
 */
it.effect('keeps one owned allocation in parity across the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSourceRealized(moduleName, bytes, 'aarch64-apple-darwin')
    const wasm = yield* Analysis.ofSourceRealized(moduleName, bytes, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(native), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])

    const evaluated = Analysis.evaluate(native)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      evaluated._tag === 'Blocked' ? JSON.stringify(evaluated.reason) : evaluated._tag,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    // Exactly one logical block is acquired and released, and every typed storage step in
    // between is ordered: no take precedes its write, and no release precedes the last take.
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      [
        'AllocationAcquire',
        'RawBufferForm',
        'SlotProject',
        'SlotWrite',
        'SlotProject',
        'SlotWrite',
        'SlotProject',
        'SlotTake',
        'SlotProject',
        'SlotTake',
        'AllocationRelease',
      ],
    )

    const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const wasmInstance = new WebAssembly.Instance(
      new WebAssembly.Module(wasmArtifact.bytes.slice()),
      {},
    )
    assert.strictEqual((wasmInstance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('computes a local-shared layout without allocator access or cleanup authority', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.layout { Layout }
pub fn main() -> i32 {
  let layout = Intrinsic.sharedLayout<i32>()
  if layout.bytes == 72 { return 42 }
  return 0
}`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-layout/native',
      source,
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', evaluationDescription(evaluated))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(Projections.allocationTraceEventsOf(evaluated), [])
  }),
)

it.effect('initializes one caller-funded local-shared core in evaluator and Wasm parity', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
effect fn construct() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const native = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/native',
      source,
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/wasm',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    const mir = Analysis.loweredMir(native)
    assert.deepEqual(MirVerification.verify(mir), [])
    assert.strictEqual(
      mir.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'SharedFromAllocation').length,
      1,
    )
    const malformed = rewriteOperations(mir, (operation) =>
      operation._tag !== 'SharedFromAllocation'
        ? operation
        : Object.freeze({
            ...operation,
            allocationBlock: Object.freeze({
              ...operation.allocationBlock,
              size: operation.allocationBlock.size + 1,
            }),
          }),
    )
    const invalidInitialization = MirVerification.verify(malformed).find(
      (violation) => violation.localSharedReason === 'InitializationContract',
    )
    assert.strictEqual(invalidInitialization?.rule, 'InvalidLocalSharedOperation')
    assert.strictEqual(
      invalidInitialization?.provenance?.span.sourceId,
      'local-shared-allocation/native',
    )
    const reusedInput = appendAfterOperation(mir, (operation) =>
      operation._tag !== 'SharedFromAllocation'
        ? Object.freeze([])
        : Object.freeze([
            Object.freeze({
              _tag: 'Drop' as const,
              local: operation.allocation,
              cleanup: Object.freeze({
                _tag: 'AllocationCleanup' as const,
                type: Type.allocation,
                ticket: 'ActiveReclaimTicket' as const,
              }),
              provenance: operation.provenance,
            }),
          ]),
    )
    const reusedViolation = MirVerification.verify(reusedInput).find(
      (violation) => violation.localSharedReason === 'InitializationContract',
    )
    assert.strictEqual(reusedViolation?.rule, 'InvalidLocalSharedOperation')
    assert.strictEqual(reusedViolation?.provenance?.span.sourceId, 'local-shared-allocation/native')
    const evaluated = Analysis.evaluate(native)
    assert.strictEqual(evaluated._tag, 'Completed', evaluationDescription(evaluated))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => ({
        tag: event._tag,
        strong: event.strong,
        access: event.access,
      })),
      [
        { tag: 'AllocationAcquire', strong: undefined, access: undefined },
        { tag: 'SharedInitialize', strong: 1n, access: 'Available' },
        { tag: 'SharedLastCleanup', strong: 0n, access: 'Available' },
        { tag: 'AllocationRelease', strong: undefined, access: undefined },
      ],
    )
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('clones, accesses sequentially, and cleans one local-shared payload exactly once', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
fn selected(value: &mut i32) -> i32 { return 21 }
fn conflict() -> i32 { return 0 }
effect fn construct() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    let clone = Intrinsic.sharedClone<i32>(&core)
    let first = Intrinsic.sharedWithMut<i32, i32>(&core, selected, conflict)
    let second = Intrinsic.sharedWithMut<i32, i32>(&clone, selected, conflict)
    drop clone
    drop core
    return first + second
  }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const native = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/native',
      source,
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/wasm',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    const mir = Analysis.loweredMir(native)
    assert.deepEqual(MirVerification.verify(mir), [])
    assert.deepEqual(
      mir.functions
        .flatMap(MirVerification.operations)
        .filter(
          (operation) => operation._tag === 'SharedClone' || operation._tag === 'SharedWithMut',
        )
        .map((operation) => operation._tag),
      ['SharedClone', 'SharedWithMut', 'SharedWithMut'],
    )
    const encodedShared = `${MirEncoding.encode(mir)
      .split('\n')
      .filter(
        (line) =>
          / = shared-(from-allocation|clone|with-mut) /.test(line) ||
          line.includes('cleanup=LocalSharedCoreCleanup'),
      )
      .join('\n')}\n`
    assert.strictEqual(encodedShared, golden('local-shared.mir.txt'))
    assert.notMatch(encodedShared, /silk\/shared|Deferred|Scheduler|address|offset=/)
    const repeated = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/native',
      source,
      'aarch64-apple-darwin',
    )
    assert.strictEqual(MirEncoding.encode(mir), MirEncoding.encode(Analysis.loweredMir(repeated)))

    const tamperedClone = rewriteOperations(mir, (operation) =>
      operation._tag !== 'SharedClone'
        ? operation
        : Object.freeze({
            ...operation,
            block: Object.freeze({
              ...operation.block,
              accessOffset: operation.block.accessOffset + 1,
            }),
          }),
    )
    const tamperedAccess = rewriteOperations(mir, (operation) =>
      operation._tag !== 'SharedWithMut'
        ? operation
        : Object.freeze({ ...operation, retainedLoans: Object.freeze([operation.loan]) }),
    )
    const tamperedDrop = rewriteOperations(mir, (operation) =>
      operation._tag !== 'Drop' || operation.cleanup._tag !== 'LocalSharedCoreCleanup'
        ? operation
        : Object.freeze({
            _tag: 'Drop' as const,
            local: operation.local,
            cleanup: Object.freeze({ _tag: 'NoCleanup' as const, type: operation.cleanup.type }),
            provenance: operation.provenance,
          }),
    )
    for (const [module, reason] of [
      [tamperedClone, 'CloneContract'],
      [tamperedAccess, 'AccessContract'],
      [tamperedDrop, 'CleanupContract'],
    ] as const) {
      const violation = MirVerification.verify(module).find(
        (candidate) => candidate.localSharedReason === reason,
      )
      assert.strictEqual(violation?.rule, 'InvalidLocalSharedOperation')
      assert.strictEqual(violation?.provenance?.span.sourceId, 'local-shared-lifecycle/native')
    }
    const evaluated = Analysis.evaluate(native)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      evaluated._tag === 'Blocked' && evaluated.reason._tag === 'InvalidMir'
        ? evaluated.reason.violations.map((violation) => violation.detail).join('\n')
        : evaluated._tag,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => ({
        tag: event._tag,
        strong: event.strong,
        access: event.access,
      })),
      [
        { tag: 'AllocationAcquire', strong: undefined, access: undefined },
        { tag: 'SharedInitialize', strong: 1n, access: 'Available' },
        { tag: 'SharedClone', strong: 2n, access: 'Available' },
        { tag: 'SharedAccessBegin', strong: 2n, access: 'Active' },
        { tag: 'SharedAccessEnd', strong: 2n, access: 'Available' },
        { tag: 'SharedAccessBegin', strong: 2n, access: 'Active' },
        { tag: 'SharedAccessEnd', strong: 2n, access: 'Available' },
        { tag: 'SharedDecrement', strong: 1n, access: 'Available' },
        { tag: 'SharedLastCleanup', strong: 0n, access: 'Available' },
        { tag: 'AllocationRelease', strong: undefined, access: undefined },
      ],
    )
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
    const overflow = nativeArtifact.ir.indexOf('_overflow = icmp eq')
    const accepted = nativeArtifact.ir.indexOf('_clone_accepted:', overflow)
    const incremented = nativeArtifact.ir.indexOf('_incremented = add', accepted)
    const countStore = nativeArtifact.ir.indexOf('store i64', incremented)
    assert.isAtLeast(overflow, 0)
    assert.isAbove(accepted, overflow)
    assert.isAbove(incremented, accepted)
    assert.isAbove(countStore, incremented)
    assert.notMatch(nativeArtifact.ir, /atomicrmw|cmpxchg|\bfence\b/)
    assert.notMatch(artifact.wat, /memory\.atomic|atomic\.|cmpxchg|\bfence\b/)
    assert.match(
      artifact.wat,
      /i32\.load(?: offset=\d+)?\s+i32\.const -1\s+i32\.eq\s+if\s+unreachable\s+end[\s\S]*?i32\.add\s+i32\.store/,
    )
    const nativeMemorySource = readFileSync(
      new URL('../src/NativeMemoryOperation.ts', import.meta.url),
      'utf8',
    )
    const nativeAccessSource = readFileSync(
      new URL('../src/NativeLocalSharedOperation.ts', import.meta.url),
      'utf8',
    )
    const wasmSource = readFileSync(new URL('../src/WasmBackend.ts', import.meta.url), 'utf8')
    const helperBody = (source: string, start: string, end: string): string => {
      const from = source.indexOf(start)
      const to = source.indexOf(end, from + start.length)
      assert.isAtLeast(from, 0)
      assert.isAbove(to, from)
      return source.slice(from, to)
    }
    const helperBodies = [
      helperBody(nativeMemorySource, "case 'SharedClone':", "case 'RawBufferCount':"),
      helperBody(nativeAccessSource, 'export const emit', '\n})'),
      helperBody(wasmSource, 'const emitSharedCloneOperation', 'const emitRawBufferCountOperation'),
    ]
    for (const body of helperBodies) {
      assert.notMatch(
        body,
        /\b(?:allocate|allocation|reallocate|reallocation|atomic|lock|scheduler|collector|background|gc)\b|silk\/shared/i,
      )
    }
  }),
)

it.effect('relates a callback-borrow escape to its local-shared access boundary', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
struct Pair { first: i32 second: i32 }
fn deferred(value: &mut Pair) -> Effect<i32> {
  return effect { return value.first }
}
fn fallback() -> Effect<i32> { return effect { return 0 } }
effect fn construct() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<Pair>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<Pair>(move allocation, Pair { first: 20, second: 22 })
    let escaped = Intrinsic.sharedWithMut<Pair, Effect<i32>>(&core, deferred, fallback)
    drop escaped
    drop core
  }
  return 0
}
pub fn main() -> i32 { return 0 }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/escape',
      source,
      'wasm32-unknown-unknown',
    )
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'OWN0016',
    )
    assert.exists(
      diagnostic,
      JSON.stringify(
        Analysis.diagnostics(snapshot).map((candidate) => ({
          code: candidate.code,
          reason: candidate.reason._tag,
          span: candidate.span,
        })),
      ),
    )
    assert.strictEqual(diagnostic?.reason._tag, 'LocalSharedAccessEscape')
    assert.strictEqual(diagnostic?.span.sourceId, 'local-shared-lifecycle/escape')
    assert.strictEqual(diagnostic?.relatedSpans?.length, 1)
    assert.strictEqual(
      diagnostic?.relatedSpans?.at(0)?.span.sourceId,
      'local-shared-lifecycle/escape',
    )
    assert.isBelow(diagnostic?.span.start ?? Number.MAX_SAFE_INTEGER, diagnostic?.span.end ?? -1)
    assert.isBelow(
      diagnostic?.relatedSpans?.at(0)?.span.start ?? Number.MAX_SAFE_INTEGER,
      diagnostic?.relatedSpans?.at(0)?.span.end ?? -1,
    )
  }),
)

it('classifies inexpressible local-shared escape containers at the ownership-fact tier', () => {
  const narrowed = Type.slice('Shared', 'i32')
  const genericAggregate = Type.nominal('test', 'Box', [narrowed])
  const failureValue = Type.effect('i32', [Type.nominal('test', 'Problem', [narrowed])])
  const storedCallable = Type.callable([], narrowed, 'Take')
  for (const resultType of [narrowed, genericAggregate, failureValue, storedCallable]) {
    assert.isTrue(
      Ownership.localSharedResultEscapes({
        resultType,
        capturesRestrictedParameter: false,
        referencesRestrictedParameter: true,
      }),
    )
  }
  assert.isFalse(
    Ownership.localSharedResultEscapes({
      resultType: 'i32',
      capturesRestrictedParameter: false,
      referencesRestrictedParameter: true,
    }),
  )
})

it.effect('rejects a direct local-shared callback borrow at its access boundary', () =>
  Effect.gen(function* () {
    const source = ascii(`struct Pair { first: i32 second: i32 }
fn direct(value: &mut Pair) -> &mut Pair { return value }
fn directConflict() -> &mut Pair { return directConflict() }
unsafe fn directProbe(core: &Intrinsic.SharedCore<Pair>) -> &mut Pair {
  return Intrinsic.sharedWithMut<Pair, &mut Pair>(core, direct, directConflict)
}
pub fn main() -> i32 { return 0 }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/direct-escape',
      source,
      'wasm32-unknown-unknown',
    )
    const diagnostics = Analysis.diagnostics(snapshot).filter(
      (candidate) => candidate.code === 'OWN0016',
    )
    assert.strictEqual(
      diagnostics.length,
      1,
      JSON.stringify(
        Analysis.diagnostics(snapshot).map((candidate) => ({
          code: candidate.code,
          reason: candidate.reason._tag,
          start: candidate.span.start,
          end: candidate.span.end,
        })),
      ),
    )
    for (const diagnostic of diagnostics) {
      assert.strictEqual(diagnostic.reason._tag, 'LocalSharedAccessEscape')
      assert.strictEqual(diagnostic.relatedSpans?.length, 1)
      assert.strictEqual(
        diagnostic.relatedSpans?.at(0)?.span.sourceId,
        'local-shared-lifecycle/direct-escape',
      )
    }
  }),
)

it.effect('rejects generic aggregate capture and suspension across local-shared access', () =>
  Effect.gen(function* () {
    const source = ascii(`struct Pair { first: i32 second: i32 }
struct Box<A> { value: A }
fn wrap<A>(value: A) -> Box<A> { return Box<A> { value: move value } }
fn deferred(value: &mut Pair) -> Box<Effect<i32>> {
  let escaped = effect { return value.first }
  return wrap<Effect<i32>>(move escaped)
}
fn deferredConflict() -> Box<Effect<i32>> {
  return wrap<Effect<i32>>(effect { return 0 })
}
effect fn read(value: &mut Pair) -> i32 { return value.second }
fn suspended(value: &mut Pair) -> i32 {
  let result = run read(value)
  return result
}
fn numberConflict() -> i32 { return 0 }
unsafe fn aggregateProbe(core: &Intrinsic.SharedCore<Pair>) -> Box<Effect<i32>> {
  let callback = deferred
  return Intrinsic.sharedWithMut<Pair, Box<Effect<i32>>>(
    core,
    move callback,
    deferredConflict,
  )
}
unsafe fn suspensionProbe(core: &Intrinsic.SharedCore<Pair>) -> i32 {
  return Intrinsic.sharedWithMut<Pair, i32>(core, suspended, numberConflict)
}
pub fn main() -> i32 { return 0 }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/aggregate-suspension-escape',
      source,
      'wasm32-unknown-unknown',
    )
    const diagnostics = Analysis.diagnostics(snapshot).filter(
      (candidate) => candidate.code === 'OWN0016',
    )
    assert.deepEqual(
      diagnostics.map((diagnostic) =>
        diagnostic.reason._tag === 'LocalSharedAccessEscape'
          ? diagnostic.reason.kind
          : diagnostic.reason._tag,
      ),
      ['Result', 'Suspension'],
    )
    for (const diagnostic of diagnostics) {
      assert.strictEqual(diagnostic.relatedSpans?.length, 1)
      assert.strictEqual(
        diagnostic.relatedSpans?.at(0)?.span.sourceId,
        'local-shared-lifecycle/aggregate-suspension-escape',
      )
    }
    assert.isUndefined(Analysis.loweredMir(snapshot).coroutineFrames)
  }),
)

it.effect('keeps an outer local-shared access active while nested access selects conflict', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
fn selected(value: &mut i32) -> i32 { return 0 }
fn conflict() -> i32 { return 21 }
fn unused(value: &mut i32, captured: Intrinsic.SharedCore<i32>) -> i32 {
  drop captured
  return 0
}
fn nested(value: &mut i32, core: Intrinsic.SharedCore<i32>) -> i32 {
  let cleanupCore = Intrinsic.sharedClone<i32>(&core)
  return Intrinsic.sharedWithMut<i32, i32>(&core, unused(move cleanupCore), conflict) + 21
}
effect fn construct() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    let nestedCore = Intrinsic.sharedClone<i32>(&core)
    let result = Intrinsic.sharedWithMut<i32, i32>(&core, nested(move nestedCore), conflict)
    drop core
    return result
  }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/nested',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', evaluationDescription(evaluated))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated)
        .filter(
          (event) => event._tag.startsWith('SharedAccess') || event._tag === 'SharedDecrement',
        )
        .map((event) => ({ tag: event._tag, strong: event.strong, access: event.access })),
      [
        { tag: 'SharedAccessBegin', strong: 2n, access: 'Active' },
        { tag: 'SharedAccessConflict', strong: 3n, access: 'Active' },
        { tag: 'SharedDecrement', strong: 2n, access: 'Active' },
        { tag: 'SharedDecrement', strong: 1n, access: 'Active' },
        { tag: 'SharedAccessEnd', strong: 1n, access: 'Available' },
      ],
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('rejects consuming the borrowed local-shared receiver in its selected callback', () =>
  Effect.gen(function* () {
    const source = ascii(`fn consume(value: &mut i32, core: Intrinsic.SharedCore<i32>) -> i32 {
  drop core
  return 0
}
fn conflict() -> i32 { return 0 }
unsafe fn probe(core: Intrinsic.SharedCore<i32>) -> i32 {
  return Intrinsic.sharedWithMut<i32, i32>(&core, consume(move core), conflict)
}
pub fn main() -> i32 { return 0 }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/consume-borrowed-receiver',
      source,
      'wasm32-unknown-unknown',
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'OWN0011',
    )
  }),
)

it.effect('cleans acyclic nested local-shared cores from payload to allocation', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
struct Outer { inner: Intrinsic.SharedCore<i32> }
effect fn construct() -> i32 ! OutOfMemoryError {
  let innerLayout = Intrinsic.sharedLayout<i32>()
  let innerAllocation = run Intrinsic.systemAllocationAcquire(move innerLayout)
  let outerLayout = Intrinsic.sharedLayout<Outer>()
  let outerAllocation = run Intrinsic.systemAllocationAcquire(move outerLayout)
  unsafe {
    let inner = Intrinsic.sharedFromAllocation<i32>(move innerAllocation, 42)
    let outer = Intrinsic.sharedFromAllocation<Outer>(
      move outerAllocation,
      Outer { inner: move inner },
    )
    drop outer
    return 42
  }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/acyclic-nested',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      evaluated._tag === 'Blocked' && evaluated.reason._tag === 'InvalidMir'
        ? evaluated.reason.violations.map((violation) => violation.detail).join('\n')
        : evaluated._tag,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      [
        'AllocationAcquire',
        'AllocationAcquire',
        'SharedInitialize',
        'SharedInitialize',
        'SharedLastCleanup',
        'AllocationRelease',
        'SharedLastCleanup',
        'AllocationRelease',
      ],
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('cleans every core in an acyclic recursive local-shared chain', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
struct Empty {}
struct Node { next: Intrinsic.SharedCore<Node> | Empty }
effect fn construct() -> i32 ! OutOfMemoryError {
  let firstAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  let secondAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  let thirdAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  unsafe {
    let third = Intrinsic.sharedFromAllocation<Node>(
      move thirdAllocation,
      Node { next: Empty {} },
    )
    let second = Intrinsic.sharedFromAllocation<Node>(
      move secondAllocation,
      Node { next: move third },
    )
    let first = Intrinsic.sharedFromAllocation<Node>(
      move firstAllocation,
      Node { next: move second },
    )
    drop first
    return 42
  }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/acyclic-recursive',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const helpers = Analysis.loweredMir(snapshot).functions.filter(
      (fn) =>
        fn.id.module === LocalSharedPayloadCleanup.declaration.module &&
        fn.id.name === LocalSharedPayloadCleanup.declaration.name,
    )
    assert.strictEqual(helpers.length, 1)
    assert.strictEqual(
      MirVerification.operations(
        helpers.at(0) ?? raise('expected a payload cleanup helper'),
      ).filter((operation) => operation._tag === 'Drop').length,
      1,
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const events = Projections.allocationTraceEventsOf(evaluated)
    assert.strictEqual(events.filter((event) => event._tag === 'SharedLastCleanup').length, 3)
    assert.strictEqual(events.filter((event) => event._tag === 'AllocationRelease').length, 3)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('leaks the specified strong cycle without manufacturing cleanup authority', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
struct Empty {}
struct Bomb {}
impl Drop for Bomb {
  fn drop(self: &mut Bomb) -> () { let boom = 1 / 0 return () }
}
struct Node { bomb: Bomb next: Intrinsic.SharedCore<Node> | Empty }
fn link(value: &mut Node, next: Intrinsic.SharedCore<Node>) -> i32 {
  value.next = move next
  return 0
}
fn conflict() -> i32 { return 0 }
effect fn construct() -> i32 ! OutOfMemoryError {
  let firstAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  let secondAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  unsafe {
    let first = Intrinsic.sharedFromAllocation<Node>(
      move firstAllocation,
      Node { bomb: Bomb {}, next: Empty {} },
    )
    let second = Intrinsic.sharedFromAllocation<Node>(
      move secondAllocation,
      Node { bomb: Bomb {}, next: Empty {} },
    )
    let secondEdge = Intrinsic.sharedClone<Node>(&second)
    let firstLink = Intrinsic.sharedWithMut<Node, i32>(&first, link(move secondEdge), conflict)
    let firstEdge = Intrinsic.sharedClone<Node>(&first)
    let secondLink = Intrinsic.sharedWithMut<Node, i32>(&second, link(move firstEdge), conflict)
    if firstLink == 999 { let bomb = Bomb {} drop bomb }
    drop first
    drop second
    return firstLink + secondLink + 42
  }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/strong-cycle',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const events = Projections.allocationTraceEventsOf(evaluated)
    assert.strictEqual(events.filter((event) => event._tag === 'SharedDecrement').length, 2)
    assert.strictEqual(events.filter((event) => event._tag === 'SharedLastCleanup').length, 0)
    assert.strictEqual(events.filter((event) => event._tag === 'AllocationRelease').length, 0)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('discharges local-shared obligations across two typed-failure frames', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
struct Problem {}
effect fn failInner(core: Intrinsic.SharedCore<i32>) -> i32 ! Problem {
  let inner = Intrinsic.sharedClone<i32>(&core)
  fail Problem {}
}
effect fn construct() -> i32 ! Problem | OutOfMemoryError {
  let allocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<i32>())
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    let transferred = Intrinsic.sharedClone<i32>(&core)
    return run failInner(move transferred)
  }
}
effect fn recover(error: Problem | OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/two-frame-failure',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated)
        .filter((event) => event._tag.startsWith('Shared') || event._tag === 'AllocationRelease')
        .map((event) => ({ tag: event._tag, strong: event.strong })),
      [
        { tag: 'SharedInitialize', strong: 1n },
        { tag: 'SharedClone', strong: 2n },
        { tag: 'SharedClone', strong: 3n },
        { tag: 'SharedDecrement', strong: 2n },
        { tag: 'SharedDecrement', strong: 1n },
        { tag: 'SharedLastCleanup', strong: 0n },
        { tag: 'AllocationRelease', strong: undefined },
      ],
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('leaves the payload cleanup obligation with source when allocation is exhausted', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
import silk.layout { Layout }
struct Token { storage: Allocation }
struct Exhausted {}
effect fn reject(self: &mut Exhausted, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}
impl Allocator for Exhausted { allocate: Exhausted.reject }
effect fn construct() -> i32 ! OutOfMemoryError {
  let payloadLayout = Layout.of<i32>()
  let storage = run Intrinsic.systemAllocationAcquire(move payloadLayout)
  let token = Token { storage: move storage }
  let mut allocator = Exhausted {}
  let recipe = Allocator.allocate(Intrinsic.sharedLayout<Token>())
    |> Effect.provideMut<Allocator>(&mut allocator)
  let allocation = run recipe
  unsafe {
    let core = Intrinsic.sharedFromAllocation<Token>(move allocation, move token)
    drop core
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/exhausted',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      evaluated._tag === 'Blocked' && evaluated.reason._tag === 'InvalidMir'
        ? evaluated.reason.violations.map((violation) => violation.detail).join('\n')
        : evaluated._tag,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      ['AllocationAcquire', 'AllocationRelease'],
    )
  }),
)

it.effect('transports exact local-shared allocation provenance through an ordinary helper', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
fn forward(allocation: Allocation) -> Allocation { return move allocation }
effect fn construct() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let acquired = run Intrinsic.systemAllocationAcquire(move layout)
  let allocation = forward(move acquired)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/helper',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

it.effect('invalidates inherited allocation provenance after a mutable parameter write', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
fn replace(mut allocation: Allocation, replacement: Allocation) -> Allocation {
  allocation = move replacement
  return move allocation
}
effect fn construct() -> i32 ! OutOfMemoryError {
  let firstLayout = Intrinsic.sharedLayout<i32>()
  let first = run Intrinsic.systemAllocationAcquire(move firstLayout)
  let secondLayout = Intrinsic.sharedLayout<i32>()
  let second = run Intrinsic.systemAllocationAcquire(move secondLayout)
  let allocation = replace(move first, move second)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/mutable-parameter',
      source,
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0138'],
    )
    const diagnostic = Analysis.diagnostics(snapshot).at(0)
    assert.strictEqual(
      diagnostic?.reason._tag === 'LocalSharedLayoutMismatch'
        ? diagnostic.reason.actual
        : undefined,
      'mutable parameter allocation provenance',
    )
  }),
)

it.effect('invalidates inherited allocation provenance after replacing a mutable parameter', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
fn replace(mut allocation: Allocation, replacement: Allocation) -> Allocation {
  let old = Intrinsic.replace(allocation, move replacement)
  drop old
  return move allocation
}
effect fn construct() -> i32 ! OutOfMemoryError {
  let firstLayout = Intrinsic.sharedLayout<i32>()
  let first = run Intrinsic.systemAllocationAcquire(move firstLayout)
  let secondLayout = Intrinsic.sharedLayout<i64>()
  let second = run Intrinsic.systemAllocationAcquire(move secondLayout)
  let allocation = replace(move first, move second)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/mutable-parameter-replace',
      source,
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0138'],
    )
    const diagnostic = Analysis.diagnostics(snapshot).at(0)
    assert.strictEqual(
      diagnostic?.reason._tag === 'LocalSharedLayoutMismatch'
        ? diagnostic.reason.actual
        : undefined,
      'mutable parameter allocation provenance',
    )
  }),
)

it.effect('proves exact local-shared provenance through the selected allocator provider', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
effect fn construct() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let recipe = Allocator.allocate(Intrinsic.sharedLayout<i32>())
    |> Effect.provideMut<Allocator>(&mut allocator)
  let allocation = run recipe
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/provider',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(ordinaryStorageSource(value), (character) => character.charCodeAt(0))

/** The accepted shape every negative below deviates from in exactly one way. */
const guarded = (body: string): string => `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
${body}
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

/**
 * The substrate is only sound if the frontend keeps rejecting the programs that would violate
 * it, so each prohibited shape is pinned to the code that rejects it. A regression here would
 * otherwise surface as a trap — or as undefined behaviour in a released backend — rather than
 * as a compile error.
 */
it.effect('rejects every prohibited allocation shape before lowering', () =>
  Effect.gen(function* () {
    const cases: ReadonlyArray<readonly [string, string, string]> = [
      [
        'local-shared-layout-provenance-mismatch',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i64>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'SEM0138',
      ],
      [
        'local-shared-ordinary-layout',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
import silk.layout { Layout }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Layout.of<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'SEM0138',
      ],
      [
        'local-shared-helper-provenance-mismatch',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
fn forward(allocation: Allocation) -> Allocation { return move allocation }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<u32>()
  let acquired = run Intrinsic.systemAllocationAcquire(move layout)
  let allocation = forward(move acquired)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'SEM0138',
      ],
      [
        'local-shared-conditional-helper-provenance-mismatch',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
fn choose(flag: bool, wrong: Allocation, right: Allocation) -> Allocation {
  if flag {
    drop right
    return move wrong
  }
  drop wrong
  return move right
}
effect fn store() -> i32 ! OutOfMemoryError {
  let wrongLayout = Intrinsic.sharedLayout<u32>()
  let wrong = run Intrinsic.systemAllocationAcquire(move wrongLayout)
  let rightLayout = Intrinsic.sharedLayout<i32>()
  let right = run Intrinsic.systemAllocationAcquire(move rightLayout)
  let allocation = choose(true, move wrong, move right)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'SEM0138',
      ],
      [
        'local-shared-provider-forges-provenance',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
import silk.layout { Layout }
service Forge {
  effect fn allocate(layout: Layout) -> Allocation ! OutOfMemoryError ? &mut Forge
}
struct BadForge {}
effect fn allocate(self: &mut BadForge, layout: Layout) -> Allocation ! OutOfMemoryError {
  drop layout
  return run Intrinsic.systemAllocationAcquire(Layout.of<i32>())
}
impl Forge for BadForge { allocate: BadForge.allocate }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut forge = BadForge {}
  let recipe = Forge.allocate(Intrinsic.sharedLayout<i32>())
    |> Effect.provideMut<Forge>(&mut forge)
  let allocation = run recipe
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'SEM0138',
      ],
      [
        'local-shared-same-spelling-layout',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
import silk.layout { Layout }
fn sharedLayout() -> Layout { return Layout.of<i32>() }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = sharedLayout()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'SEM0138',
      ],
      [
        'local-shared-outside-unsafe',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
  drop core
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'SEM0082',
      ],
      [
        'local-shared-reuses-consumed-allocation',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop allocation
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'OWN0001',
      ],
      [
        'local-shared-reuses-consumed-payload',
        `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
effect fn store() -> i32 ! OutOfMemoryError {
  let blockLayout = Intrinsic.sharedLayout<Allocation>()
  let block = run Intrinsic.systemAllocationAcquire(move blockLayout)
  let payloadLayout = Intrinsic.sharedLayout<i32>()
  let payload = run Intrinsic.systemAllocationAcquire(move payloadLayout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<Allocation>(move block, move payload)
    drop payload
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'OWN0001',
      ],
      [
        'raw-storage-outside-unsafe',
        `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let mut buffer = Intrinsic.rawBufferFrom<i32>(move allocation, 2)
  drop buffer
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
        'SEM0082',
      ],
      [
        'slot-escapes-its-buffer',
        guarded(`    let slot = RawBuffer.slot(&mut buffer, 0)
    drop buffer
    let value = Slot.take(move slot)
    return value`),
        'OWN0011',
      ],
      [
        'buffer-moves-under-a-live-slot',
        guarded(`    let slot = RawBuffer.slot(&mut buffer, 0)
    let moved = move buffer
    drop moved
    return 1`),
        'OWN0011',
      ],
      [
        'foreign-allocator-conformance',
        `import silk.allocator { Allocator }
struct TestAllocator { remaining: i32 }
impl Allocator for TestAllocator { allocate: Foreign.allocate }
pub fn main() -> i32 { return 0 }`,
        'SEM0083',
      ],
      [
        'drop-hook-on-a-copy-type',
        `struct CopyValue { value: i32 }
impl Copy for CopyValue {}
impl Drop for CopyValue { fn drop(self: &mut CopyValue) -> () { return () } }
pub fn main() -> i32 { return 0 }`,
        'SEM0083',
      ],
    ]

    for (const [name, source, code] of cases) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `owned-allocation-negative/${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.include(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        code,
        `${name}\n${Hir.encode(Analysis.rootAnalysis(snapshot).hir)}`,
      )
      if (code === 'SEM0138')
        assert.throws(() => Analysis.loweredMir(snapshot), /MIR is unavailable/)
    }
  }),
)

/**
 * A fresh process must reach the same artifacts: the substrate introduces ticket ordinals and
 * logical addresses, and either would reintroduce run-to-run variation if it leaked into a key.
 */
it.effect('produces byte-identical artifacts across repeated analyses', () =>
  Effect.gen(function* () {
    const first = yield* Analysis.ofSourceRealized(moduleName, bytes, 'wasm32-unknown-unknown')
    const second = yield* Analysis.ofSourceRealized(moduleName, bytes, 'wasm32-unknown-unknown')

    assert.strictEqual(
      MirEncoding.encode(Analysis.loweredMir(first)),
      MirEncoding.encode(Analysis.loweredMir(second)),
    )
    const firstWasm = yield* Analysis.codegenWasm(first, { mode: 'release' })
    const secondWasm = yield* Analysis.codegenWasm(second, { mode: 'release' })
    assert.deepEqual(Array.from(firstWasm.bytes), Array.from(secondWasm.bytes))
  }),
)
