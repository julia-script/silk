import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as CallableContract from '../src/CallableContract.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as Scalar from '../src/Scalar.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const key = (actor: string, operation: string): string => `${actor}.${operation}`

const operationKeys = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  [...snapshot.semanticOccurrences.modules.values()].flatMap((module) =>
    module.occurrences.flatMap((occurrence) =>
      occurrence.resolution._tag === 'Available' &&
      occurrence.resolution.identity._tag === 'IntrinsicOperationIdentity'
        ? [key(occurrence.resolution.identity.id.actor, occurrence.resolution.identity.id.name)]
        : [],
    ),
  )

const acceptedSources = Object.freeze([
  ...Scalar.integers().map((scalar, scalarOrdinal) => {
    const calls = scalar.operations.map((operation, operationOrdinal) => {
      const arguments_ = operation.arity === 1 ? '1' : '1, 1'
      return `  let v${operationOrdinal} = ${scalar.spelling}.${operation.spelling}(${arguments_})`
    })
    return `import silk.${scalar.spelling} as ${scalar.spelling}\npub fn scalar${scalarOrdinal}() -> i32 {\n${calls.join('\n')}\n  return 0\n}`
  }),
  ...Scalar.floats().map((scalar, scalarOrdinal) => {
    const calls = scalar.operations.map((operation, operationOrdinal) => {
      const argument = operation.code === 'FromBits' ? '1' : '1.0'
      const arguments_ = operation.arity === 1 ? argument : `${argument}, ${argument}`
      return `  let v${operationOrdinal} = ${scalar.spelling}.${operation.spelling}(${arguments_})`
    })
    return `import silk.${scalar.spelling} as ${scalar.spelling}\npub fn floating${scalarOrdinal}() -> i32 {\n${calls.join('\n')}\n  return 0\n}`
  }),
  // Character operations use typed parameters so checked construction receives `u32` while
  // comparison and inspection receive `char`.
  ...Scalar.all()
    .filter((scalar) => scalar.category === 'Character')
    .map((scalar, scalarOrdinal) => {
      const calls = scalar.operations.map((operation, operationOrdinal) => {
        const parameters =
          operation.parameters ?? Array.from({ length: operation.arity }, () => scalar.spelling)
        const arguments_ = parameters.map((parameter, ordinal) => {
          if (parameter === 'u32') return 'number'
          return ordinal === 0 ? 'left' : 'right'
        })
        return `  let v${operationOrdinal} = ${scalar.spelling}.${operation.spelling}(${arguments_.join(', ')})`
      })
      return `import silk.${scalar.spelling} as ${scalar.spelling}\npub fn character${scalarOrdinal}(number: u32, left: ${scalar.spelling}, right: ${scalar.spelling}) -> i32 {\n${calls.join('\n')}\n  return 0\n}`
    }),
  `import silk.bool as bool
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.i32 as i32
import silk.layout { Layout }
import silk.usize as usize
pub fn main() -> i32 {
  let i00 = i32.negate(1)
  let i01 = i32.add(1, 2)
  let i02 = i32.subtract(2, 1)
  let i03 = i32.multiply(2, 3)
  let i04 = i32.divide(4, 2)
  let i05 = i32.remainder(5, 2)
  let i06 = i32.equals(1, 1)
  let i07 = i32.notEquals(1, 2)
  let i08 = i32.lessThan(1, 2)
  let i09 = i32.lessOrEqual(1, 2)
  let i10 = i32.greaterThan(2, 1)
  let i11 = i32.greaterOrEqual(2, 1)
  let u00 = usize.add(1, 2)
  let u01 = usize.subtract(2, 1)
  let u02 = usize.multiply(2, 3)
  let u03 = usize.divide(4, 2)
  let u04 = usize.remainder(5, 2)
  let u05 = usize.equals(1, 1)
  let u06 = usize.notEquals(1, 2)
  let u07 = usize.lessThan(1, 2)
  let u08 = usize.lessOrEqual(1, 2)
  let u09 = usize.greaterThan(2, 1)
  let u10 = usize.greaterOrEqual(2, 1)
  let b00 = bool.equals(true, false)
  let b01 = bool.notEquals(true, false)
  let b02 = bool.not(false)
  let layout = Layout.of<i32>()
  let repeated = Layout.repeat(move layout, 2)
  let made = Layout.make(4, 4)
  let allocator = Allocator.systemAllocatorProvider()
  let unit = ()
  return i00
}`,
  `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
fn useShared(value: &mut i32) -> i32 { return 42 }
fn conflictShared() -> i32 { return 0 }
effect fn storage() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Effect.provideMut(Allocator.allocate(move layout), &mut allocator)
  let allocation = run recipe
  let coreLayout = Layout.of<i32>()
  let coreRecipe = Allocator.allocate(move coreLayout) |> Intrinsic.bindRequirementMut(&mut allocator)
  let coreAllocation = run coreRecipe
  drop coreAllocation
  let sharedLayout = Intrinsic.sharedLayout<i32>()
  let sharedRecipe = Allocator.allocate(move sharedLayout) |> Intrinsic.bindRequirementMut(&mut allocator)
  let sharedAllocation = run sharedRecipe
  unsafe {
    let shared = Intrinsic.sharedFromAllocation<i32>(move sharedAllocation, 42)
    let cloned = Intrinsic.sharedClone<i32>(&shared)
    let selected = Intrinsic.sharedWithMut<i32, i32>(&cloned, useShared, conflictShared)
    drop shared
    drop cloned
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
    let count = RawBuffer.count(&buffer)
    let firstSlot = RawBuffer.slot(&mut buffer, 0)
    let firstWrite = Slot.write(move firstSlot, 21)
    let secondSlot = RawBuffer.slot(&mut buffer, 1)
    let secondWrite = Slot.write(move secondSlot, 21)
    let read = RawBuffer.read<i32>(&buffer, 0)
    let copySlot = RawBuffer.slot(&mut buffer, 0)
    let copied = Slot.copy(move copySlot)
    let takeSlot = RawBuffer.slot(&mut buffer, 0)
    let taken = Slot.take(move takeSlot)
    let dropSlot = RawBuffer.slot(&mut buffer, 1)
    let dropped = Slot.dropValue(move dropSlot)
    drop buffer
    return read + copied + taken + selected
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(storage(), recover) }`,
  `import silk.allocator { Allocator, OutOfMemoryError }
import silk.execution as Execution
fn ready(state: &()) -> () { return () }
fn complete(state: (), value: i32) -> () { return () }
fn suspend(state: (), execution: Intrinsic.Execution<i32>) -> () { drop execution return () }
effect fn packaged() -> () ! OutOfMemoryError ? &mut Allocator {
  let execution = run Execution.make(effect { return 42 }, (), ready)
  return run Execution.drive(move execution, (), complete, suspend)
}
pub fn main() -> i32 { return 42 }`,
  `import silk.execution as Execution
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard {
  Intrinsic.wake(move wake)
  return Guard {}
}
effect fn parking() -> () { return run Execution.park(register) }
pub fn main() -> i32 { return 42 }`,
  `import silk.effect as Effect
import silk.i32 as i32
struct Problem {}
service Clock {}
struct FixedClock {}
impl Clock for FixedClock {}
effect fn succeed(value: i32) -> i32 { return value }
effect fn double(value: i32) -> i32 { return value * 2 }
effect fn observe(value: i32) -> i32 { return value }
effect fn risky() -> i32 ! Problem { fail Problem {} }
effect fn recover(error: Problem) -> i32 { return 1 }
effect fn read() -> i32 ? &Clock { return 20 }
effect fn acquire() -> FixedClock { return FixedClock {} }
pub fn main() -> i32 {
  let mapped = succeed(1) |> Effect.map(i32.add(1))
  let chained = mapped |> Effect.flatMap(double)
  let tapped = chained |> Effect.tap(observe)
  let retried = tapped |> Effect.retry(1)
  let handled = risky() |> Effect.catchAll(recover)
  let clock = FixedClock {}
  let provided = read() |> Effect.provide(&clock)
  let acquired = read() |> Effect.provideEffect(acquire())
  let retriedValue = run retried
  let handledValue = run handled
  let providedValue = run provided
  let acquiredValue = run acquired
  return retriedValue + handledValue + providedValue + acquiredValue
}`,
  `struct Opaque {}
fn pointers(value: &mut i32, values: &mut [u8], shared: &i32, view: &[u8]) -> i32 {
  let empty = Intrinsic.pointerNull<Opaque>()
  let missing = Intrinsic.pointerIsNull<Opaque>(empty)
  let constant = Intrinsic.pointerFromRef<i32>(shared)
  let mutable = Intrinsic.pointerFromMutRef<i32>(value)
  let first = Intrinsic.pointerFromSlice<u8>(view)
  let firstMut = Intrinsic.pointerFromMutSlice<u8>(values)
  unsafe {
    let second = Intrinsic.pointerOffset<u8>(first, 1)
    let secondMut = Intrinsic.pointerOffsetMut<u8>(firstMut, 1)
    Intrinsic.pointerWrite<i32>(mutable, 7)
    return Intrinsic.pointerRead<i32>(constant)
  }
  return 0
}`,
  `struct Counter { value: i32 }
fn replace(self: &mut Counter) -> i32 { return Intrinsic.replace(self.value, 42) }
pub fn main() -> i32 {
  let mut counter = Counter { value: 1 }
  return replace(&mut counter)
}`,
  `fn inspect(bytes: &[u8]) -> bool {
  unsafe {
    let text = Intrinsic.stringFromUtf8Unchecked(bytes)
    let raw = Intrinsic.stringUtf8Bytes(text)
    let length = Intrinsic.stringByteLength(text)
    return Intrinsic.stringEqualsExact(text, text)
  }
  return false
}`,
  `import silk.effect as Effect
import silk.result { Result }
struct ResultProblem {}
effect fn succeed() -> i32 ! ResultProblem { return 42 }
pub effect fn main() -> i32 {
  let completed = run Effect.result(succeed())
  return match move completed {
      Result<i32, ResultProblem>.Success { value } => value
      Result<i32, ResultProblem>.Failure { error } => 0
  }
}`,
  `struct CatalogProblem {}
effect fn catalogRisky() -> i32 ! CatalogProblem { fail CatalogProblem {} }
effect fn catalogRecover(error: CatalogProblem) -> i32 { return 1 }
fn inspectCatch() -> once Effect<i32> {
  return Intrinsic.catchFailure<CatalogProblem>(catalogRisky(), catalogRecover)
}
pub fn main() -> i32 { return 42 }`,
  `import silk.allocator { Allocator, OutOfMemoryError }
struct SuspendProblem {}
service SuspendClock {}
effect fn suspendDirect(
  deferred: once Effect<i32 ! SuspendProblem ? &SuspendClock>
) -> i32 ! SuspendProblem | OutOfMemoryError ? &SuspendClock | &mut Allocator {
  return run Intrinsic.suspendEffect(move deferred)
}
pub fn main() -> i32 { return 42 }`,
  `import silk.writer as Streams
import silk.writer { Writer, WriterError }
import silk.effect as Effect
pub effect fn main() -> () ! WriterError {
  let mut native = Streams.stdoutWriterProvider()
  let first = run Effect.provideMut(Writer.writeAll(b"out"), &mut native)
  let second = run Effect.provideMut(Writer.writeAll(b"error"), &mut native)
  return ()
}`,
  `import silk.usize as usize
import silk.option { Option, none, some }
fn absurd<T>() -> T { let boom = 1 / 0 return absurd<T>() }
fn opened(handle: OsHandle) -> Option<OsHandle> { return some<OsHandle>(move handle) }
fn refused() -> Option<OsHandle> { return none<OsHandle>() }
effect fn systemClockNow(seconds: &mut i64, nanoseconds: &mut i64) -> bool {
  unsafe { return run Intrinsic.osSystemClockNow(seconds, nanoseconds) }
  return false
}
effect fn systemClockResolution(nanoseconds: &mut u64) -> bool {
  unsafe { return run Intrinsic.osSystemClockResolution(nanoseconds) }
  return false
}
effect fn monotonicClockNow(seconds: &mut i64, nanoseconds: &mut i64) -> bool {
  unsafe { return run Intrinsic.osMonotonicClockNow(seconds, nanoseconds) }
  return false
}
effect fn monotonicClockResolution(nanoseconds: &mut u64) -> bool {
  unsafe { return run Intrinsic.osMonotonicClockResolution(nanoseconds) }
  return false
}
effect fn monotonicClockWaitUntil(seconds: i64, nanoseconds: i64) -> bool {
  unsafe { return run Intrinsic.osMonotonicClockWaitUntil(seconds, nanoseconds) }
  return false
}
effect fn randomFill(output: &mut [u8]) -> bool {
  unsafe { return run Intrinsic.osRandomFill(move output) }
  return false
}
effect fn fileOpen(root: &[u8], path: &[u8], reason: &mut i32, code: &mut u32) -> Option<OsHandle> {
  unsafe { return run Intrinsic.osFileOpen<Option<OsHandle>>(root, path, 0, reason, code, opened, refused) }
  return none<OsHandle>()
}
effect fn fileRead(handle: &mut OsHandle, output: &mut [u8], count: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osFileRead(handle, output, count, reason, code) }
  return false
}
effect fn fileWrite(handle: &mut OsHandle, input: &[u8], count: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osFileWrite(handle, input, 0, count, reason, code) }
  return false
}
effect fn directoryOpen(root: &[u8], path: &[u8], reason: &mut i32, code: &mut u32) -> Option<OsHandle> {
  unsafe { return run Intrinsic.osDirectoryOpen<Option<OsHandle>>(root, path, reason, code, opened, refused) }
  return none<OsHandle>()
}
effect fn directoryNext(handle: &mut OsHandle, output: &mut [u8], count: &mut usize, kind: &mut i32, required: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osDirectoryNext(handle, output, count, kind, required, reason, code) }
  return false
}
effect fn inspect(root: &[u8], path: &[u8], kind: &mut i32, length: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osPathInspect(root, path, kind, length, reason, code) }
  return false
}
effect fn create(root: &[u8], path: &[u8], reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osDirectoryCreate(root, path, reason, code) }
  return false
}
effect fn createUnique(root: &[u8], parent: &[u8], prefix: &[u8], output: &mut [u8], count: &mut usize, required: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osDirectoryCreateUnique(root, parent, prefix, move output, count, required, reason, code) }
  return false
}
effect fn removeFile(root: &[u8], path: &[u8], reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osFileRemove(root, path, reason, code) }
  return false
}
effect fn removeDirectory(root: &[u8], path: &[u8], reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osDirectoryRemove(root, path, reason, code) }
  return false
}
effect fn close(handle: OsHandle, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osHandleClose(move handle, reason, code) }
  return false
}
effect fn standardInputRead(output: &mut [u8], count: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osStandardInputRead(move output, count, reason, code) }
  return false
}
effect fn processExecute(program: &[u8], arguments: &[u8], environment: &[u8], directory: &[u8], status: &mut i32, exit: &mut i32, outputLength: &mut usize, errorLength: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osProcessExecute(program, arguments, environment, directory, status, exit, outputLength, errorLength, reason, code) }
  return false
}
effect fn processCapture(output: &mut [u8], count: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osProcessCapture(0, usize.ZERO, move output, count, reason, code) }
  return false
}
effect fn hostArgumentCount(count: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osHostArgumentCount(count, reason, code) }
  return false
}
effect fn hostArgument(index: usize, output: &mut [u8], count: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osHostArgument(index, move output, count, reason, code) }
  return false
}
effect fn hostVariable(name: &[u8], output: &mut [u8], count: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osHostVariable(name, move output, count, reason, code) }
  return false
}
effect fn hostWorkingDirectory(output: &mut [u8], count: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osHostWorkingDirectory(move output, count, reason, code) }
  return false
}`,
])

it('models enumValue as a sealed declaration-dependent rule with no generic type hole', () => {
  const operation = Intrinsic.findOperation('Intrinsic', 'enumValue')
  assert.isDefined(operation)
  if (operation === undefined) return
  assert.strictEqual(operation.rule._tag, 'EnumValueRule')
  assert.deepEqual(operation.typeParameters, [])
  assert.deepEqual(operation.parameters, [{ name: 'value', type: '<owning enum>' }])
  assert.strictEqual(operation.result, '<owning enum representation>')
  assert.strictEqual(operation.consumer, 'language:scalar-enum-value')
})

it('uses one binding contract for inventory, admission, and the proof-only post hook', () => {
  for (const name of ['bindRequirement', 'bindRequirementMut', 'bindRequirementOwned']) {
    const operation = Intrinsic.findOperation('Intrinsic', name)
    const entry = Intrinsic.inventory().find(
      (candidate) => candidate.operation === `Intrinsic.${name}`,
    )
    assert.isDefined(operation)
    assert.isDefined(entry)
    if (operation === undefined || entry === undefined) continue
    assert.strictEqual(operation.rule._tag, 'ContractRule')
    if (operation.rule._tag !== 'ContractRule') continue
    assert.strictEqual(operation.rule.post, 'BindRequirement')
    assert.strictEqual(entry.signature, Intrinsic.signature(operation))
    assert.strictEqual(
      CallableContract.key(operation.rule.contract),
      CallableContract.key(operation.rule.contract),
    )
    assert.deepEqual(Object.keys(operation.rule).sort(), [
      '_tag',
      'contract',
      'post',
      'providerMode',
    ])
  }
})

it.effect(
  'pairs every intrinsic presentation with accepted semantic analysis',
  () =>
    Effect.gen(function* () {
      const observed = new Set<string>()
      for (const [ordinal, source] of acceptedSources.entries()) {
        const snapshot = yield* Analysis.ofSource(
          `intrinsic/accepted-${ordinal}`,
          encoder.encode(source),
        )
        assert.deepEqual(
          Analysis.diagnostics(snapshot),
          [],
          `accepted intrinsic fixture ${ordinal}`,
        )
        for (const operation of operationKeys(snapshot)) observed.add(operation)
      }
      const catalog = Intrinsic.all().flatMap((actor) =>
        actor.operations.flatMap((operation) =>
          operation.rule._tag === 'EnumValueRule' || operation.phase !== 'Runtime'
            ? []
            : [key(actor.spelling, operation.spelling)],
        ),
      )
      assert.deepEqual([...observed].sort(), [...catalog].sort())
    }),
  // Measured near the 60s floor while the full parallel gate saturates the host; the timeout
  // is headroom for contention, not a performance assertion.
  180_000,
)

it.effect('keeps every intrinsic identifiable and presentable in rejected calls', () =>
  Effect.gen(function* () {
    for (const actor of Intrinsic.all())
      for (const operation of actor.operations) {
        if (
          operation.rule._tag === 'EnumValueRule' ||
          operation.rule._tag === 'StaticOnlyRule' ||
          operation.rule._tag === 'MixedFieldProjectionRule'
        )
          continue
        const arguments_ = operation.parameters.length === 0 ? '0' : ''
        const source =
          operation.rule._tag === 'PlaceRule'
            ? `pub fn main() -> i32 { let mut value = 0 let rejected = Intrinsic.replace(value) return 0 }`
            : `pub fn main() -> i32 { let rejected = ${actor.spelling}.${operation.spelling}(${arguments_}) return 0 }`
        const snapshot = yield* Analysis.ofSource(
          `intrinsic/rejected-${actor.spelling}-${operation.spelling}`,
          encoder.encode(source),
        )
        assert.isAbove(Analysis.diagnostics(snapshot).length, 0)
        assert.include(operationKeys(snapshot), key(actor.spelling, operation.spelling))
        assert.include(Intrinsic.signature(operation), operation.spelling)
      }
  }),
)

it.effect('infers the suspension intrinsic exact Effect channels', () =>
  Effect.gen(function* () {
    const module = 'intrinsic/suspend-rows'
    const source = `struct Problem {}
service Clock {}
fn suspend(
  deferred: once Effect<i32 ! Problem ? &Clock>
) -> once Effect<i32 ! Problem ? &Clock> {
  return Intrinsic.suspendEffect(move deferred)
}
pub fn main() -> i32 { return 42 }`
    const snapshot = yield* Analysis.ofSourceRealized(module, encoder.encode(source))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const suspended = Analysis.expressionsOf(snapshot, module).find(
      (expression) =>
        expression._tag === 'Call' &&
        expression.reference._tag === 'ResolvedBuiltin' &&
        expression.reference.operation === 'EffectSuspend',
    )
    assert.strictEqual(
      suspended?.type._tag === 'Available' ? Type.encode(suspended.type.type) : undefined,
      `once Effect<i32 ! ${module}.Problem ? &${module}.Clock>`,
    )
  }),
)

it.effect('does not retain provideWith as a compatibility alias', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect/no-provide-with-alias',
      encoder.encode(`import silk.effect as Effect
service Clock {}
struct FixedClock {}
impl Clock for FixedClock {}
effect fn read() -> i32 ? &Clock { return 1 }
effect fn acquire() -> FixedClock { return FixedClock {} }
pub fn main() -> i32 { return run (read() |> Effect.provideWith(acquire())) }`),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0014',
    )
  }),
)

it('admits the Pointer actor with one invariant per unsafe primitive', () => {
  const pointer = Intrinsic.inventory().filter((entry) =>
    entry.operation.startsWith('Intrinsic.pointer'),
  )
  assert.deepEqual(
    pointer.map((entry) => [entry.operation, entry.unsafe, entry.invariant !== undefined]),
    [
      ['Intrinsic.pointerNull', false, false],
      ['Intrinsic.pointerIsNull', false, false],
      ['Intrinsic.pointerFromRef', false, false],
      ['Intrinsic.pointerFromMutRef', false, false],
      ['Intrinsic.pointerFromSlice', false, false],
      ['Intrinsic.pointerFromMutSlice', false, false],
      ['Intrinsic.pointerOffset', true, true],
      ['Intrinsic.pointerOffsetMut', true, true],
      ['Intrinsic.pointerRead', true, true],
      ['Intrinsic.pointerWrite', true, true],
    ],
  )
  assert.isTrue(pointer.every((entry) => entry.admission === 'Ownership'))
  assert.isTrue(pointer.every((entry) => entry.targets.length === 3))
  assert.strictEqual(
    pointer.find((entry) => entry.operation === 'Intrinsic.pointerFromMutSlice')?.signature,
    'fn Intrinsic.pointerFromMutSlice<T>(values: &mut [T]) -> *mut T',
  )
})

it('keeps catalog ordering stable across fresh reads', () => {
  const first = Intrinsic.all().map((actor) => ({
    actor: actor.spelling,
    operations: actor.operations.map((operation) => operation.spelling),
  }))
  const second = Intrinsic.all().map((actor) => ({
    actor: actor.spelling,
    operations: actor.operations.map((operation) => operation.spelling),
  }))
  assert.deepEqual(second, first)
})

it('indexes every actor and operation without changing catalog identity', () => {
  for (const actor of Intrinsic.all()) {
    assert.strictEqual(Intrinsic.findActor(actor.spelling), actor)
    for (const operation of actor.operations) {
      assert.strictEqual(Intrinsic.findOperation(actor.spelling, operation.spelling), operation)
      assert.strictEqual(Intrinsic.findOperationById(operation.id), operation)
    }
  }
  for (const scalar of Scalar.all())
    for (const operation of scalar.operations) {
      const resolved = Intrinsic.findOperation(scalar.spelling, operation.spelling)
      assert.notStrictEqual(resolved, undefined)
      if (resolved !== undefined)
        assert.strictEqual(resolved, Intrinsic.findOperation('Intrinsic', resolved.spelling))
    }
  assert.strictEqual(Intrinsic.findActor('Missing'), undefined)
  assert.strictEqual(Intrinsic.findOperation('Intrinsic', 'missing'), undefined)
  assert.strictEqual(Intrinsic.findOperation('missing', 'add'), undefined)
  assert.strictEqual(
    Intrinsic.findOperationById({
      _tag: 'IntrinsicOperationId',
      actor: 'Intrinsic',
      name: 'missing',
    }),
    undefined,
  )
})

it('matches the checked intrinsic inventory and records every unsafe invariant', () => {
  const fixture: unknown = JSON.parse(
    readFileSync(new URL('./fixtures/intrinsic-inventory.json', import.meta.url), 'utf8'),
  )
  const entries = Intrinsic.inventory().map((entry) => ({
    operation: entry.operation,
    signature: entry.signature,
    unsafe: entry.unsafe,
    admission: entry.admission,
    consumer: entry.consumer,
    ...(entry.hir === undefined ? {} : { identity: entry.hir }),
    ...(entry.invariant === undefined ? {} : { invariant: entry.invariant }),
    ...(entry.hostImport === undefined ? {} : { hostImport: entry.hostImport }),
  }))
  assert.deepEqual(fixture, { targets: ['Evaluator', 'LLVM', 'Wasm'], entries })
  assert.deepEqual(
    Intrinsic.all().map((actor) => actor.spelling),
    ['string', 'Intrinsic'],
  )
  assert.isTrue(entries.every((entry) => entry.consumer.length > 0))
  assert.isTrue(entries.filter((entry) => entry.unsafe).every((entry) => 'invariant' in entry))
  assert.deepEqual(
    Intrinsic.inventory()
      .filter((entry) => entry.operation.startsWith('Intrinsic.shared'))
      .map((entry) => entry.operation),
    [
      'Intrinsic.sharedLayout',
      'Intrinsic.sharedFromAllocation',
      'Intrinsic.sharedClone',
      'Intrinsic.sharedWithMut',
    ],
  )
  assert.deepEqual(
    Intrinsic.inventory()
      .filter((entry) => entry.operation.toLowerCase().includes('suspend'))
      .map((entry) => ({
        operation: entry.operation,
        signature: entry.signature,
        targets: entry.targets,
      })),
    [
      {
        operation: 'Intrinsic.suspendEffect',
        signature:
          'fn Intrinsic.suspendEffect<A, E, ?R>(deferred: once Effect<A ! E ? R>) -> Effect<A ! E ? R>',
        targets: ['Evaluator', 'LLVM', 'Wasm'],
      },
    ],
  )
  const externalParking = Intrinsic.inventory().filter(
    (entry) => entry.consumer === 'language:external-wake-parking',
  )
  assert.deepEqual(
    externalParking.map((entry) => ({
      operation: entry.operation,
      signature: entry.signature,
      unsafe: entry.unsafe,
      targets: entry.targets,
    })),
    [
      {
        operation: 'Intrinsic.wake',
        signature: 'fn Intrinsic.wake(wake: Wake) -> ()',
        unsafe: false,
        targets: ['Evaluator', 'LLVM', 'Wasm'],
      },
      {
        operation: 'Intrinsic.park',
        signature: 'fn Intrinsic.park<G, F>(register: F) -> Effect<()>',
        unsafe: false,
        targets: ['Evaluator', 'LLVM', 'Wasm'],
      },
    ],
  )
  assert.isFalse(
    externalParking.some((entry) =>
      /cancel|destroy|scheduler|timer|payload|allocator/i.test(
        `${entry.operation} ${entry.signature} ${entry.hir}`,
      ),
    ),
  )
})

it('classifies targetProfile as a static-only u8 intrinsic with no runtime identity', () => {
  const operation = Intrinsic.findOperation('Intrinsic', 'targetProfile')
  assert.isDefined(operation)
  if (operation === undefined) return
  assert.deepEqual(
    {
      signature: Intrinsic.signature(operation),
      phase: operation.phase,
      parameters: operation.parameters,
      result: operation.result,
      unsafe: operation.unsafe,
      targets: operation.targets,
      rule: operation.rule,
    },
    {
      signature: 'fn Intrinsic.targetProfile() -> u8',
      phase: 'StaticOnly',
      parameters: [],
      result: 'u8',
      unsafe: false,
      targets: [],
      rule: {
        _tag: 'StaticOnlyRule',
        contract: {
          functionKind: 'Function',
          unsafe: false,
          binders: [],
          parameters: [],
          result: 'u8',
          constraints: [],
          captures: [],
        },
      },
    },
  )
  const entry = Intrinsic.inventory().find(
    (candidate) => candidate.operation === 'Intrinsic.targetProfile',
  )
  assert.deepEqual(entry, {
    operation: 'Intrinsic.targetProfile',
    signature: 'fn Intrinsic.targetProfile() -> u8',
    unsafe: false,
    phase: 'StaticOnly',
    admission: 'Language',
    consumer: 'silk/target.profile',
    targets: [],
  })
})

it('keeps reflection metadata and static sequences sealed to static evaluation', () => {
  const operations = [
    ['reflectType', 1],
    ['reflectFields', 1],
    ['reflectTypeKind', 1],
    ['reflectFieldKind', 2],
    ['reflectFieldLabel', 2],
    ['reflectFieldOrdinal', 2],
    ['staticSequenceEmpty', 1],
    ['staticSequenceAppend', 1],
    ['staticSequenceConcat', 1],
    ['staticSequenceLength', 1],
    ['staticSequenceAt', 1],
  ] as const
  for (const [name, arity] of operations) {
    const operation = Intrinsic.findOperation('Intrinsic', name)
    assert.isDefined(operation)
    if (operation === undefined) continue
    assert.strictEqual(operation.phase, 'StaticOnly')
    assert.strictEqual(operation.rule._tag, 'StaticOnlyRule')
    assert.strictEqual(operation.typeParameters.length, arity)
    assert.deepEqual(operation.targets, [])
  }
  for (const [name, arity] of [
    ['Intrinsic.Type', 1],
    ['Intrinsic.Fields', 1],
    ['Intrinsic.Field', 2],
    ['Intrinsic.StaticSequence', 1],
  ] as const) {
    const nominal = Type.intrinsicNominals.get(name)
    assert.isDefined(nominal)
    if (nominal === undefined) continue
    assert.strictEqual(Type.intrinsicNominalArity(nominal), arity)
    assert.isFalse(Type.runtimeAvailable(nominal))
  }
})

it('models borrowField as one mixed shared lane plus one consumed static lane', () => {
  const operation = Intrinsic.findOperation('Intrinsic', 'borrowField')
  assert.isDefined(operation)
  if (operation === undefined) return
  assert.strictEqual(operation.phase, 'Mixed')
  assert.strictEqual(operation.rule._tag, 'MixedFieldProjectionRule')
  assert.deepEqual(operation.parameters, [
    { name: 'owner', type: '&Owner' },
    { name: 'field', type: 'Field<Owner, Value>', phase: 'Static' },
  ])
  assert.strictEqual(operation.returnedBorrowParameter, 0)
  assert.strictEqual(
    Intrinsic.signature(operation),
    'fn Intrinsic.borrowField<Owner, Value>(owner: &Owner, static field: Field<Owner, Value>) -> &Value',
  )
  const entry = Intrinsic.inventory().find(
    (candidate) => candidate.operation === 'Intrinsic.borrowField',
  )
  assert.deepEqual(entry, {
    operation: 'Intrinsic.borrowField',
    signature:
      'fn Intrinsic.borrowField<Owner, Value>(owner: &Owner, static field: Field<Owner, Value>) -> &Value',
    unsafe: false,
    phase: 'Mixed',
    admission: 'Language',
    consumer: 'silk/reflect.borrowField',
    targets: [],
  })
})

it.effect(
  'resolves former scalar actor spellings to source wrappers, not compiler identities',
  () =>
    Effect.gen(function* () {
      const source = 'import silk.i32 as i32\npub fn main() -> i32 { return i32.add(20, 22) }'
      const snapshot = yield* Analysis.ofSourceRealized(
        'intrinsic/source-wrapper',
        encoder.encode(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const occurrence = Analysis.semanticOccurrenceAt(
        snapshot,
        'intrinsic/source-wrapper',
        source.indexOf('add'),
      )
      assert.strictEqual(occurrence?.resolution._tag, 'Available')
      if (occurrence?.resolution._tag === 'Available')
        assert.strictEqual(occurrence.resolution.identity._tag, 'DeclarationIdentity')
      assert.strictEqual(occurrence?.declaration?.module, 'silk/i32')
    }),
)

it.effect('keeps same-spelled local-shared operations entirely ordinary outside Intrinsic', () =>
  Effect.gen(function* () {
    const source = `fn sharedLayout() -> i32 { return 20 }
fn sharedFromAllocation(value: i32) -> i32 { return value + 21 }
fn sharedClone(value: i32) -> i32 { return value + 1 }
fn sharedWithMut(value: i32, use: i32, onConflict: i32) -> i32 {
  return value + use + onConflict
}
pub fn main() -> i32 {
  return sharedWithMut(sharedClone(sharedFromAllocation(sharedLayout())), 0, 0)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'intrinsic/local-shared-same-spelling',
      encoder.encode(source),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(
      operationKeys(snapshot).filter((operation) => operation.includes('shared')),
      [],
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)
