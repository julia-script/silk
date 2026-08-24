import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ExecutionAffinity from '../src/ExecutionAffinity.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as LocalSharedOwnership from '../src/LocalSharedOwnership.js'
import * as Type from '../src/Type.js'
import { ordinaryStorageSource } from './support/ordinaryStorageSource.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(ordinaryStorageSource(value), (character) => character.charCodeAt(0))

const ordinaryUse = `import silk.core { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.shared as Shared
struct Counter { value: i32 }
fn increment(value: &mut Counter) -> i32 { value.value = value.value + 1 return value.value }
fn read(value: &Counter) -> i32 { return value.value }
effect fn useCell() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let creating = Shared.make<Counter>(Counter { value: 41 })
    |> Effect.provideMut<Allocator>(&mut allocator)
  let first = run creating
  let second = Shared.clone<Counter>(&first)
  let updated = Shared.withMut<Counter, i32>(&second, increment)
  let answer = Shared.with<Counter, i32>(&first, read)
  drop second
  drop first
  return answer
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(useCell(), recover) }`

const affineMovement = `import silk.core { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.shared as Shared
struct Empty {}
struct Token { storage: Allocation }
struct Mailbox { state: Empty | Token }
fn take(self: &mut Mailbox) -> Empty | Token {
  return Intrinsic.replace(self.state, Empty {})
}
fn consume(value: Empty | Token) -> i32 {
  return match move value {
    Empty {} => 0
    Token { storage } => release(move storage)
  }
}
fn release(storage: Allocation) -> i32 { drop storage return 42 }
effect fn useCell() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let storage = run (Allocator.allocate(Layout.of<i32>())
    |> Effect.provideMut<Allocator>(&mut allocator))
  let mailbox = run (Shared.make<Mailbox>(Mailbox {
    state: Token { storage: move storage }
  }) |> Effect.provideMut<Allocator>(&mut allocator))
  let token = Shared.withMut<Mailbox, Empty | Token>(&mailbox, take)
  drop mailbox
  return consume(move token)
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(useCell(), recover) }`

const exhaustedConstruction = `import silk.core { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.shared as Shared
struct Token { storage: Allocation }
struct Exhausted {}
effect fn reject(self: &mut Exhausted, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}
impl Allocator for Exhausted { allocate: Exhausted.reject }
effect fn construct() -> i32 ! OutOfMemoryError {
  let mut system = SystemAllocator.make()
  let payload = run (Allocator.allocate(Layout.of<i32>())
    |> Effect.provideMut<Allocator>(&mut system))
  let token = Token { storage: move payload }
  let mut exhausted = Exhausted {}
  let shared = run (Shared.make<Token>(move token)
    |> Effect.provideMut<Allocator>(&mut exhausted))
  drop shared
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`

const mixedAllocatorConstruction = `import silk.core { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.shared as Shared
struct BadAllocator {}
effect fn badAllocate(self: &mut BadAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  drop layout
  let wrong = Layout.of<u8>()
  return run Intrinsic.systemAllocationAcquire(move wrong)
}
impl Allocator for BadAllocator { allocate: BadAllocator.badAllocate }
effect fn good() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let shared = run (Shared.make<i32>(41)
    |> Effect.provideMut<Allocator>(&mut allocator))
  drop shared
  return 1
}
effect fn bad() -> i32 ! OutOfMemoryError {
  let mut allocator = BadAllocator {}
  let shared = run (Shared.make<i32>(42)
    |> Effect.provideMut<Allocator>(&mut allocator))
  drop shared
  return 1
}
effect fn both() -> i32 ! OutOfMemoryError {
  let first = run good()
  let second = run bad()
  return first + second
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(both(), recover) }`

const unrelatedCallbackShape = `import silk.shared as Shared
struct Box { value: i32 }
fn delayed(value: &Box) -> Effect<i32> { return effect { return value.value } }
fn unrelated(
  shared: &Shared.Shared<Box>,
  callback: once fn(&Box) -> Effect<i32>,
) -> Effect<i32> {
  return effect { return 0 }
}
fn probe(shared: &Shared.Shared<Box>) -> Effect<i32> {
  return unrelated(shared, delayed)
}
pub fn main() -> i32 { return 0 }`

const renamedMultiCallbackBoundary = `struct Other<T> { core: Intrinsic.SharedCore<T> }
struct Box { value: i32 }
fn ignored(value: &mut Box) -> Effect<i32> { return effect { return 0 } }
fn escaping(value: &mut Box) -> Effect<i32> { return effect { return value.value } }
fn conflict() -> Effect<i32> { return effect { return 0 } }
fn access(
  self: &Other<Box>,
  unused: once fn(&mut Box) -> Effect<i32>,
  use: once fn(&mut Box) -> Effect<i32>,
) -> Effect<i32> {
  drop unused
  return Intrinsic.sharedWithMut<Box, Effect<i32>>(&self.core, move use, conflict)
}
fn probe(self: &Other<Box>) -> Effect<i32> {
  return access(self, ignored, escaping)
}
pub fn main() -> i32 { return 0 }`

const renamedWrapper = `import silk.core { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
struct Other<T> { core: Intrinsic.SharedCore<T> }
struct Counter { value: i32 }
fn absurd<T>() -> T { let boom = 1 / 0 return absurd<T>() }
fn conflict() -> never { let boom = 1 / 0 return conflict() }
effect fn create<T>(value: T) -> Other<T> ! OutOfMemoryError ? &mut Allocator {
  let allocation = run Allocator.allocate(Intrinsic.sharedLayout<T>())
  unsafe {
    let core = Intrinsic.sharedFromAllocation<T>(move allocation, move value)
    return Other<T> { core: move core }
  }
  return absurd<Other<T>>()
}
fn retain<T>(self: &Other<T>) -> Other<T> {
  let core = Intrinsic.sharedClone<T>(&self.core)
  return Other<T> { core: move core }
}
fn access<T, A>(self: &Other<T>, use: once fn(&mut T) -> A) -> A {
  return Intrinsic.sharedWithMut<T, A>(&self.core, move use, conflict)
}
fn increment(value: &mut Counter) -> i32 {
  value.value = value.value + 1
  return value.value
}
effect fn runOther() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let first = run (create<Counter>(Counter { value: 41 })
    |> Effect.provideMut<Allocator>(&mut allocator))
  let second = retain<Counter>(&first)
  let answer = access<Counter, i32>(&second, increment)
  drop second
  drop first
  return answer
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(runOther(), recover) }`

const nestedAccess = (outer: 'with' | 'withMut', inner: 'with' | 'withMut'): string => {
  const outerReference = outer === 'with' ? '&Counter' : '&mut Counter'
  const innerCallback = inner === 'with' ? 'read' : 'increment'
  return `import silk.core { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.shared as Shared
struct Counter { value: i32 }
fn read(value: &Counter) -> i32 { return value.value }
fn increment(value: &mut Counter) -> i32 {
  value.value = value.value + 1
  return value.value
}
fn nested(value: ${outerReference}, alias: Shared.Shared<Counter>) -> i32 {
  return Shared.${inner}<Counter, i32>(&alias, ${innerCallback})
}
effect fn conflictCase() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let first = run (Shared.make<Counter>(Counter { value: 41 })
    |> Effect.provideMut<Allocator>(&mut allocator))
  let alias = Shared.clone<Counter>(&first)
  return Shared.${outer}<Counter, i32>(&first, nested(move alias))
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(conflictCase(), recover) }`
}

const publicEscapeMatrix = `import silk.result as Result
import silk.shared as Shared
struct Pair { first: i32 second: i32 }
struct Box<A> { value: A }
fn direct(value: &Pair) -> &Pair { return value }
fn directMut(value: &mut Pair) -> &mut Pair { return value }
fn narrowedMut(value: &mut Pair) -> &Pair { return move value }
fn generic<A>(value: A) -> A { return move value }
fn viaGeneric(value: &Pair) -> &Pair { return generic<&Pair>(move value) }
fn viaGenericMut(value: &mut Pair) -> &mut Pair { return generic<&mut Pair>(move value) }
fn aggregate(value: &Pair) -> Box<&Pair> { return Box<&Pair> { value: move value } }
fn aggregateMut(value: &mut Pair) -> Box<&mut Pair> {
  return Box<&mut Pair> { value: move value }
}
fn failed(value: &Pair) -> Result.Result<i32, &Pair> {
  return Result.failResult<i32, &Pair>(move value)
}
fn failedMut(value: &mut Pair) -> Result.Result<i32, &mut Pair> {
  return Result.failResult<i32, &mut Pair>(move value)
}
fn delayed(value: &Pair) -> Effect<i32> { return effect { return value.first } }
fn delayedMut(value: &mut Pair) -> Effect<i32> { return effect { return value.first } }
fn readStored(input: i32, value: &Pair) -> i32 { return input + value.first }
fn stored(value: &Pair) -> once fn(i32) -> i32 { return readStored(move value) }
fn storedMut(value: &mut Pair) -> once fn(i32) -> i32 { return readStored(move value) }
effect fn read(value: &Pair) -> i32 { return value.first }
effect fn readMut(value: &mut Pair) -> i32 { return value.first }
fn suspended(value: &Pair) -> i32 { return run read(value) }
fn suspendedMut(value: &mut Pair) -> i32 { return run readMut(value) }
unsafe fn sharedDirect(self: &Shared.Shared<Pair>) -> &Pair {
  return Shared.with(self, direct)
}
unsafe fn mutDirect(self: &Shared.Shared<Pair>) -> &mut Pair {
  return Shared.withMut(self, directMut)
}
unsafe fn mutNarrowed(self: &Shared.Shared<Pair>) -> &Pair {
  return Shared.withMut(self, narrowedMut)
}
unsafe fn sharedGeneric(self: &Shared.Shared<Pair>) -> &Pair {
  return Shared.with(self, viaGeneric)
}
unsafe fn mutGeneric(self: &Shared.Shared<Pair>) -> &mut Pair {
  return Shared.withMut(self, viaGenericMut)
}
unsafe fn sharedAggregate(self: &Shared.Shared<Pair>) -> Box<&Pair> {
  return Shared.with(self, aggregate)
}
unsafe fn mutAggregate(self: &Shared.Shared<Pair>) -> Box<&mut Pair> {
  return Shared.withMut(self, aggregateMut)
}
fn sharedFailure(self: &Shared.Shared<Pair>) -> Result.Result<i32, &Pair> {
  return Shared.with(self, failed)
}
fn mutFailure(self: &Shared.Shared<Pair>) -> Result.Result<i32, &mut Pair> {
  return Shared.withMut(self, failedMut)
}
fn sharedEffect(self: &Shared.Shared<Pair>) -> Effect<i32> {
  return Shared.with(self, delayed)
}
fn mutEffect(self: &Shared.Shared<Pair>) -> Effect<i32> {
  return Shared.withMut(self, delayedMut)
}
fn sharedCallable(self: &Shared.Shared<Pair>) -> once fn(i32) -> i32 {
  return Shared.with(self, stored)
}
fn mutCallable(self: &Shared.Shared<Pair>) -> once fn(i32) -> i32 {
  return Shared.withMut(self, storedMut)
}
fn sharedSuspension(self: &Shared.Shared<Pair>) -> i32 {
  return Shared.with(self, suspended)
}
fn mutSuspension(self: &Shared.Shared<Pair>) -> i32 {
  return Shared.withMut(self, suspendedMut)
}
pub fn main() -> i32 { return 0 }`

it.effect('imports the canonical Shared actor and executes allocation-free clone and access', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'shared-stdlib/ordinary',
      ascii(ordinaryUse),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      [
        'AllocationAcquire',
        'SharedInitialize',
        'SharedClone',
        'SharedAccessBegin',
        'SharedAccessEnd',
        'SharedAccessBegin',
        'SharedAccessEnd',
        'SharedDecrement',
        'SharedLastCleanup',
        'AllocationRelease',
      ],
    )
  }),
)

it.effect('derives affine local ownership through the ordinary Shared wrapper', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'shared-stdlib/facts',
      ascii(`import silk.shared { Shared }
struct Holder<T> { value: Shared<T> }
fn retain(value: Shared<i32>) -> i32 {
  let pending = effect { drop move value return 42 }
  drop pending
  return 42
}
pub fn main() -> i32 { return 42 }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const shared = Type.nominal('silk/shared', 'Shared', ['i32'])
    const holder = Type.nominal('shared-stdlib/facts', 'Holder', ['i32'])
    assert.strictEqual(ExecutionAffinity.ofType(snapshot.index, shared)._tag, 'LocalExecution')
    assert.strictEqual(ExecutionAffinity.ofType(snapshot.index, holder)._tag, 'LocalExecution')
    assert.strictEqual(
      LocalSharedOwnership.count(LocalSharedOwnership.ofType(snapshot.index, shared)),
      1,
    )
    assert.isFalse(Type.isSharedCore(shared))
    const ownership = snapshot.ownership.get('shared-stdlib/facts')
    const retain = ownership?.functions.find(
      (fn) => fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'retain',
    )
    const pending = retain?.bindings.find((binding) => binding.name === 'pending')
    assert.strictEqual(pending?.executionAffinity._tag, 'LocalExecution')
    assert.strictEqual(
      LocalSharedOwnership.count(pending?.localSharedObligations ?? LocalSharedOwnership.none),
      1,
    )
  }),
)

it.effect('moves an affine payload through public Shared access on evaluation and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'shared-stdlib/affine-movement',
      ascii(affineMovement),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const events = Projections.allocationTraceEventsOf(evaluated)
    assert.strictEqual(events.filter((event) => event._tag === 'AllocationAcquire').length, 2)
    assert.strictEqual(events.filter((event) => event._tag === 'AllocationRelease').length, 2)
    assert.deepEqual(
      events.filter((event) => event._tag.startsWith('SharedAccess')).map((event) => event._tag),
      ['SharedAccessBegin', 'SharedAccessEnd'],
    )
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('cleans the source payload once when the selected allocator rejects make', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'shared-stdlib/exhausted',
      ascii(exhaustedConstruction),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      ['AllocationAcquire', 'AllocationRelease'],
    )
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'rejects a forged allocator even when another caller proves the same make specialization',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'shared-stdlib/mixed-allocators',
        ascii(mixedAllocatorConstruction),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        ['SEM0138'],
      )
    }),
)

it.effect('does not infer access-boundary privilege from an unrelated wrapper shape', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'shared-stdlib/unrelated-shape',
      ascii(unrelatedCallbackShape),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('propagates the sealed access edge through a renamed multi-callback wrapper', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'shared-stdlib/renamed-multi-callback',
      ascii(renamedMultiCallbackBoundary),
      'wasm32-unknown-unknown',
    )
    const diagnostics = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'OWN0016',
    )
    assert.strictEqual(diagnostics.length, 1)
    assert.strictEqual(diagnostics.at(0)?.span.sourceId, 'shared-stdlib/renamed-multi-callback')
    assert.strictEqual(diagnostics.at(0)?.relatedSpans?.length, 1)
    assert.strictEqual(
      diagnostics.at(0)?.relatedSpans?.at(0)?.span.sourceId,
      'shared-stdlib/renamed-multi-callback',
    )
  }),
)

it.effect('gives a renamed ordinary wrapper the same lifecycle and access behavior', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'shared-stdlib/renamed',
      ascii(renamedWrapper),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      [
        'AllocationAcquire',
        'SharedInitialize',
        'SharedClone',
        'SharedAccessBegin',
        'SharedAccessEnd',
        'SharedDecrement',
        'SharedLastCleanup',
        'AllocationRelease',
      ],
    )
  }),
)

it.effect('traps all four nested public access combinations before the nested callback', () =>
  Effect.gen(function* () {
    for (const outer of ['with', 'withMut'] as const) {
      for (const inner of ['with', 'withMut'] as const) {
        const snapshot = yield* Analysis.ofSourceRealized(
          `shared-stdlib/conflict-${outer}-${inner}`,
          ascii(nestedAccess(outer, inner)),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [])
        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(evaluated._tag, 'Trap', `${outer} -> ${inner}`)
        if (evaluated._tag !== 'Trap') continue
        const access = Projections.allocationTraceEventsOf(evaluated)
          .filter((event) => event._tag.startsWith('SharedAccess'))
          .map((event) => event._tag)
        assert.deepEqual(access, ['SharedAccessBegin', 'SharedAccessConflict'])
        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.throws(() => (instance.exports.silk_main as () => number)())
      }
    }
  }),
)

it.effect(
  'rejects recursive and suspended borrow escape through both public access operations',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'shared-stdlib/public-escape',
        ascii(publicEscapeMatrix),
        'wasm32-unknown-unknown',
      )
      const diagnostics = Analysis.diagnostics(snapshot).filter(
        (diagnostic) => diagnostic.code === 'OWN0016',
      )
      assert.strictEqual(
        diagnostics.length,
        15,
        JSON.stringify(
          Analysis.diagnostics(snapshot).map((diagnostic) => ({
            code: diagnostic.code,
            reason: diagnostic.reason._tag,
            start: diagnostic.span.start,
          })),
        ),
      )
      assert.deepEqual(
        diagnostics.map((diagnostic) =>
          diagnostic.reason._tag === 'LocalSharedAccessEscape'
            ? diagnostic.reason.kind
            : diagnostic.reason._tag,
        ),
        [
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Result',
          'Suspension',
          'Suspension',
        ],
      )
      for (const diagnostic of diagnostics) {
        assert.strictEqual(diagnostic.span.sourceId, 'shared-stdlib/public-escape')
        assert.strictEqual(diagnostic.relatedSpans?.length, 1)
        assert.strictEqual(
          diagnostic.relatedSpans?.at(0)?.span.sourceId,
          'shared-stdlib/public-escape',
        )
      }
      assert.isUndefined(Analysis.loweredMir(snapshot).coroutineFrames)
    }),
)

it('maps every sealed local-shared primitive to the ordinary Shared consumer that actually calls it', () => {
  assert.deepEqual(
    Intrinsic.inventory()
      .filter((entry) => entry.operation.startsWith('Intrinsic.shared'))
      .map((entry) => [entry.operation, entry.consumer]),
    [
      ['Intrinsic.sharedLayout', 'silk/shared.make'],
      ['Intrinsic.sharedFromAllocation', 'silk/shared.make'],
      ['Intrinsic.sharedClone', 'silk/shared.clone'],
      ['Intrinsic.sharedWithMut', 'silk/shared.withMut'],
    ],
  )
})
