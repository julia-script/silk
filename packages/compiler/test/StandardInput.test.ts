import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import * as Mir from '../src/Mir.js'
import * as StandardInput from '../src/StandardInput.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('standard-input/main', encoder.encode(source), target)

/**
 * A pure in-source provider. It replays a scripted byte sequence in at most `chunk` byte commits,
 * so a caller observes both partial fills and the end of input without a host boundary.
 */
const scriptedProvider = `struct Scripted {
  bytes: [u8; 5]
  length: usize
  offset: usize
  chunk: usize
}

fn scripted(length: usize, chunk: usize) -> Scripted {
  return Scripted {
    bytes: [u8.toU8(10), u8.toU8(20), u8.toU8(30), u8.toU8(40), u8.toU8(50)],
    length: length,
    offset: usize.ZERO,
    chunk: chunk
  }
}

effect fn scriptedRead(self: &mut Scripted, buffer: &mut [u8]) -> ReadOutcome ! StreamReadFailure {
  if self.offset == self.length { return endOfInput() }
  let mut limit = self.chunk
  let remaining = self.length - self.offset
  if remaining < limit { limit = remaining }
  if buffer.length < limit { limit = buffer.length }
  let source = self.bytes
  let mut index = usize.ZERO
  while index < limit {
    buffer[index] = source[self.offset + index]
    index = index + usize.ONE
  }
  self.offset = self.offset + limit
  return filled(limit)
}

impl StandardInput for Scripted { read: Scripted.scriptedRead }

struct Broken {}

effect fn brokenRead(self: &mut Broken, buffer: &mut [u8]) -> ReadOutcome ! StreamReadFailure {
  fail readFailure()
}

impl StandardInput for Broken { read: Broken.brokenRead }
`

const scriptedSource = (body: string) => `import silk.standard_input {
  ReadOutcome,
  count,
  endOfInput,
  filled,
  isEndOfInput,
  readFailure,
  receive
}

${scriptedProvider}
${body}`

it.effect('reads a known byte sequence from a test provider and reports its count', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedSource(`effect fn program() -> i32 ! StreamReadFailure {
  let mut provider = scripted(usize.add(0, 5), usize.add(0, 5))
  let mut buffer = [u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0)]
  let outcome = run Effect.provideMut(receive(&mut buffer), &mut provider)
  if isEndOfInput(&outcome) { return 1 }
  if count(&outcome) != usize.add(0, 5) { return 2 }
  if buffer[usize.ZERO] != u8.toU8(10) { return 3 }
  if buffer[usize.add(0, 4)] != u8.toU8(50) { return 4 }
  return 42
}

effect fn recover(error: StreamReadFailure) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catch(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome._tag))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('reports the end of input as data rather than a typed failure', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedSource(`effect fn program() -> i32 ! StreamReadFailure {
  let mut provider = scripted(usize.ZERO, usize.add(0, 4))
  let mut buffer = [u8.toU8(1), u8.toU8(1), u8.toU8(1), u8.toU8(1)]
  let outcome = run Effect.provideMut(receive(&mut buffer), &mut provider)
  if isEndOfInput(&outcome) == false { return 1 }
  if count(&outcome) != usize.ZERO { return 2 }
  // The buffer is untouched, so the end of input never masquerades as committed data.
  if buffer[usize.ZERO] != u8.toU8(1) { return 3 }
  return 42
}

// A recovery branch that survives proves the drained read never entered the failure channel.
effect fn recover(error: StreamReadFailure) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catch(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('reports the true committed count of a partial read, not the buffer length', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedSource(`effect fn program() -> i32 ! StreamReadFailure {
  let mut provider = scripted(usize.add(0, 5), usize.add(0, 2))
  let mut buffer = [u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0)]
  let first = run Effect.provideMut(receive(&mut buffer), &mut provider)
  if count(&first) != usize.add(0, 2) { return 1 }
  // Bytes past the committed prefix stay untouched even though the buffer had room.
  if buffer[usize.add(0, 2)] != u8.toU8(0) { return 2 }
  let second = run Effect.provideMut(receive(&mut buffer), &mut provider)
  if count(&second) != usize.add(0, 2) { return 3 }
  if buffer[usize.ZERO] != u8.toU8(30) { return 4 }
  let third = run Effect.provideMut(receive(&mut buffer), &mut provider)
  if count(&third) != usize.ONE { return 5 }
  let fourth = run Effect.provideMut(receive(&mut buffer), &mut provider)
  if isEndOfInput(&fourth) == false { return 6 }
  return 42
}

effect fn recover(error: StreamReadFailure) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catch(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('routes a provider error into the typed failure channel', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedSource(`effect fn program() -> i32 ! StreamReadFailure {
  let mut provider = Broken {}
  let mut buffer = [u8.toU8(0), u8.toU8(0)]
  let outcome = run Effect.provideMut(receive(&mut buffer), &mut provider)
  return 1
}

effect fn recover(error: StreamReadFailure) -> i32 { return 42 }

pub fn main() -> i32 { return run Effect.catch(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

const nativeSource = `import silk.standard_input { count, isEndOfInput, receive }

effect fn program() -> i32 ! StreamReadFailure {
  let mut provider = OsStandardInput.make()
  let mut buffer = [u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0)]
  let first = run Effect.provideMut(receive(&mut buffer), &mut provider)
  if count(&first) != usize.add(0, 3) { return 1 }
  if buffer[usize.ZERO] != u8.toU8(115) { return 2 }
  let second = run Effect.provideMut(receive(&mut buffer), &mut provider)
  if isEndOfInput(&second) == false { return 3 }
  return 42
}

effect fn recover(error: StreamReadFailure) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catch(program(), recover) }`

it.effect('drains the native provider through the OS intrinsic host boundary', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(nativeSource)
    assert.deepEqual(Analysis.diagnostics(self), [])

    const blocked = Analysis.evaluate(self)
    assert.strictEqual(blocked._tag, 'Blocked')
    if (blocked._tag === 'Blocked') assert.strictEqual(blocked.reason._tag, 'MissingStandardInput')

    const host = StandardInput.memory([115, 105, 108], { chunk: 3 })
    const outcome = Analysis.evaluate(self, { standardInput: host.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
    assert.deepEqual(
      host.events().map((event) => [event.capacity, [...event.bytes]]),
      [
        [4, [115, 105, 108]],
        [4, []],
      ],
    )
  }),
)

it.effect('routes a native host error into the typed failure channel', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(nativeSource)
    const host = StandardInput.memory([115, 105, 108], { chunk: 3, failAt: 0 })
    const outcome = Analysis.evaluate(self, { standardInput: host.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 0)
  }),
)

it.effect('lowers the native read to one reachable native-only runtime symbol', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(nativeSource)
    const operations = Analysis.loweredMir(self)
      .functions.flatMap(Mir.operations)
      .filter((operation) => operation._tag === 'OsCall')
    assert.deepEqual(
      operations.map((operation) => operation.operation.name),
      ['osStandardInputRead'],
    )

    const llvm = yield* Analysis.codegen(self, { mode: 'release' })
    assert.include(llvm.ir, '@silk_os_standard_input_read_v1')
    assert.notInclude(llvm.ir, '@silk_os_file_read_v1')

    const wasm = yield* snapshot(nativeSource, 'wasm32-unknown-unknown')
    const availability = IntrinsicAvailability.select(wasm.instances.intrinsics, 'Wasm')
    assert.strictEqual(availability._tag, 'Unavailable')
    if (availability._tag === 'Unavailable')
      assert.deepEqual(
        availability.diagnostics.map((diagnostic) => diagnostic.code),
        ['SEM0093'],
      )
  }),
)

it('replays scripted bytes and reports host failures at the memory boundary', () => {
  const host = StandardInput.memory([1, 2, 3, 4], { chunk: 3 })
  assert.deepEqual(host.provider.read(2), { _tag: 'Read', bytes: [1, 2] })
  assert.deepEqual(host.provider.read(8), { _tag: 'Read', bytes: [3, 4] })
  assert.deepEqual(host.provider.read(8), { _tag: 'Read', bytes: [] })
  assert.deepEqual(
    host.events().map((event) => event.capacity),
    [2, 8, 8],
  )

  const failing = StandardInput.memory([1], { failAt: 1 })
  assert.deepEqual(failing.provider.read(4), { _tag: 'Read', bytes: [1] })
  assert.strictEqual(failing.provider.read(4)._tag, 'ReadFailure')
  assert.strictEqual(failing.events().length, 1)
  assert.throws(() => StandardInput.memory([1], { chunk: -1 }))
})
