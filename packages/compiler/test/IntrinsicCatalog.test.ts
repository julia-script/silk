import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as Scalar from '../src/Scalar.js'

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
    return `pub fn scalar${scalarOrdinal}() -> i32 {\n${calls.join('\n')}\n  return 0\n}`
  }),
  ...Scalar.floats().map((scalar, scalarOrdinal) => {
    const calls = scalar.operations.map((operation, operationOrdinal) => {
      const argument = operation.code === 'FromBits' ? '1' : '1.0'
      const arguments_ = operation.arity === 1 ? argument : `${argument}, ${argument}`
      return `  let v${operationOrdinal} = ${scalar.spelling}.${operation.spelling}(${arguments_})`
    })
    return `pub fn floating${scalarOrdinal}() -> i32 {\n${calls.join('\n')}\n  return 0\n}`
  }),
  // `char` has no literal yet, so its operands arrive as parameters rather than constants.
  ...Scalar.all()
    .filter((scalar) => scalar.category === 'Character')
    .map((scalar, scalarOrdinal) => {
      const calls = scalar.operations.map(
        (operation, operationOrdinal) =>
          `  let v${operationOrdinal} = ${scalar.spelling}.${operation.spelling}(left, right)`,
      )
      return `pub fn character${scalarOrdinal}(left: ${scalar.spelling}, right: ${scalar.spelling}) -> i32 {\n${calls.join('\n')}\n  return 0\n}`
    }),
  `pub fn main() -> i32 {
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
  let allocator = SystemAllocator.make()
  let unit = ()
  return i00
}`,
  `effect fn storage() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Effect.provideMut(Allocator.allocate(move layout), &mut allocator)
  let allocation = run recipe
  let coreLayout = Layout.of<i32>()
  let coreRecipe = Allocator.allocate(move coreLayout) |> Intrinsic.bindRequirement(&mut allocator)
  let coreAllocation = run coreRecipe
  drop coreAllocation
  unsafe {
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
    return read + copied + taken
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catch(storage(), recover) }`,
  `struct Problem {}
struct Clock {}
effect fn succeed(value: i32) -> i32 { return value }
effect fn double(value: i32) -> i32 { return value * 2 }
effect fn observe(value: i32) -> i32 { return value }
effect fn risky() -> i32 ! Problem { fail Problem {} }
effect fn recover(error: Problem) -> i32 { return 1 }
effect fn read() -> i32 ? &Clock { return 20 }
effect fn acquire() -> Clock { return Clock {} }
pub fn main() -> i32 {
  let mapped = succeed(1) |> Effect.map(i32.add(1))
  let chained = mapped |> Effect.flatMap(double)
  let tapped = chained |> Effect.tap(observe)
  let retried = tapped |> Effect.retry(1)
  let handled = risky() |> Effect.catch(recover)
  let clock = Clock {}
  let provided = read() |> Effect.provide(&clock)
  let acquired = read() |> Effect.provideWith(acquire())
  let retriedValue = run retried
  let handledValue = run handled
  let providedValue = run provided
  let acquiredValue = run acquired
  return retriedValue + handledValue + providedValue + acquiredValue
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
  `import silk.result { Result, Success, Failure }
struct ResultProblem {}
effect fn succeed() -> i32 ! ResultProblem { return 42 }
pub effect fn main() -> i32 {
  let completed = run Effect.result(succeed())
  return match move completed {
    Result<i32, ResultProblem> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<ResultProblem> { error } => 0
    }
  }
}`,
  `pub effect fn main() -> () ! StreamWriteFailure {
  let mut native = NativeStandardStreams.native()
  let stdout = StandardStream.stdout()
  let stderr = StandardStream.stderr()
  let first = run Effect.provideMut(StandardStream.send(stdout, b"out"), &mut native)
  let second = run Effect.provideMut(StandardStream.send(stderr, b"error"), &mut native)
  return ()
}`,
  `import silk.option { Option, none }
fn absurd<T>() -> T { let boom = 1 / 0 return absurd<T>() }
effect fn fileOpen(root: &[u8], path: &[u8], reason: &mut i32, code: &mut u32) -> Option<OsHandle> {
  unsafe { return run Intrinsic.osFileOpen(root, path, 0, reason, code) }
  return none<OsHandle>()
}
effect fn fileRead(handle: &mut OsHandle, output: &mut [u8], reason: &mut i32, code: &mut u32) -> Option<usize> {
  unsafe { return run Intrinsic.osFileRead(handle, output, reason, code) }
  return none<usize>()
}
effect fn fileWrite(handle: &mut OsHandle, input: &[u8], reason: &mut i32, code: &mut u32) -> Option<usize> {
  unsafe { return run Intrinsic.osFileWrite(handle, input, 0, reason, code) }
  return none<usize>()
}
effect fn directoryOpen(root: &[u8], path: &[u8], reason: &mut i32, code: &mut u32) -> Option<OsHandle> {
  unsafe { return run Intrinsic.osDirectoryOpen(root, path, reason, code) }
  return none<OsHandle>()
}
effect fn directoryNext(handle: &mut OsHandle, output: &mut [u8], kind: &mut i32, required: &mut usize, reason: &mut i32, code: &mut u32) -> Option<usize> {
  unsafe { return run Intrinsic.osDirectoryNext(handle, output, kind, required, reason, code) }
  return none<usize>()
}
effect fn inspect(root: &[u8], path: &[u8], kind: &mut i32, length: &mut usize, reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osPathInspect(root, path, kind, length, reason, code) }
  return false
}
effect fn create(root: &[u8], path: &[u8], reason: &mut i32, code: &mut u32) -> bool {
  unsafe { return run Intrinsic.osDirectoryCreate(root, path, reason, code) }
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
effect fn standardInputRead(output: &mut [u8], reason: &mut i32, code: &mut u32) -> Option<usize> {
  unsafe { return run Intrinsic.osStandardInputRead(move output, reason, code) }
  return none<usize>()
}`,
])

it.effect('pairs every intrinsic presentation with accepted semantic analysis', () =>
  Effect.gen(function* () {
    const observed = new Set<string>()
    for (const [ordinal, source] of acceptedSources.entries()) {
      const snapshot = yield* Analysis.ofSource(
        `intrinsic/accepted-${ordinal}`,
        encoder.encode(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], `accepted intrinsic fixture ${ordinal}`)
      for (const operation of operationKeys(snapshot)) observed.add(operation)
    }
    const catalog = Intrinsic.all().flatMap((actor) =>
      actor.operations.map((operation) => key(actor.spelling, operation.spelling)),
    )
    assert.deepEqual([...observed].sort(), [...catalog].sort())
  }),
)

it.effect('keeps every intrinsic identifiable and presentable in rejected calls', () =>
  Effect.gen(function* () {
    for (const actor of Intrinsic.all())
      for (const operation of actor.operations) {
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
    identity: entry.hir,
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
})

it.effect(
  'resolves former scalar actor spellings to source wrappers, not compiler identities',
  () =>
    Effect.gen(function* () {
      const source = 'pub fn main() -> i32 { return i32.add(20, 22) }'
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
