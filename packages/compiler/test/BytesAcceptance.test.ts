import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Ownership from '../src/Ownership.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parity = `import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effects as Effect
import silk.u8 as u8
import silk.usize as usize
import silk.bytes { Bytes, copy, append, asMutSlice, asSlice, length }

fn octet(value: u8) -> u8 { return value }

fn checksum(values: &[u8]) -> i32 {
  let mut index = usize.add(0, 0)
  let mut total = 0
  while index < values.length {
    total = total + u8.toI32(values[index])
    index = index + usize.add(0, 1)
  }
  return total
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let source = [octet(0), octet(255), octet(128), octet(1)]
  let copying = copy(&source) |> Effect.provideMut(&mut allocator)
  let mut bytes = run copying
  let suffix = [octet(42), octet(7)]
  let appending = append(&mut bytes, &suffix) |> Effect.provideMut(&mut allocator)
  let appended = run appending
  let mut writable = asMutSlice(&mut bytes)
  writable[1] = octet(2)
  let readable = asSlice(&bytes)
  if length(&bytes) == 6 {} else { return 1 }
  return checksum(move readable)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'copies, appends, mutates, and releases arbitrary octets on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const wasmSnapshot = yield* Analysis.ofSourceRealized(
        'bytes-acceptance/parity',
        ascii(parity),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])

      const evaluated = Analysis.evaluate(wasmSnapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, (_, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 180n)
      const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
      const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.strictEqual(acquires.length, 2)
      assert.strictEqual(releases.length, 2)
      assert.isTrue(
        evaluated.trace.some(
          (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
        ),
      )

      const ownership = Analysis.ownershipOf(wasmSnapshot, 'bytes-acceptance/parity')
      assert.isDefined(ownership)
      if (ownership !== undefined) assert.include(Ownership.encode(ownership), 'returned-view')

      const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
      const module = new WebAssembly.Module(wasm.bytes.slice())
      assert.deepEqual(WebAssembly.Module.imports(module), [])
      const instance = new WebAssembly.Instance(module, {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 180)
    }),
  60_000,
)

const failedCopy = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effects as Effect
import silk.layout { Layout }
import silk.bytes { Bytes, copy }

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  return run pending
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

fn octet(value: u8) -> u8 { return value }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: 0 }
  let source = [octet(255)]
  let copying = copy(&source) |> Effect.provideMut(&mut allocator)
  let bytes = run copying
  return 1
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('preserves allocation failure and creates no owned storage when copy fails', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'bytes-acceptance/failed-copy',
      ascii(failedCopy),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire'),
      [],
    )
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'keeps Bytes move-only and rejects exclusive field projection through shared access',
  () =>
    Effect.gen(function* () {
      const moved = yield* Analysis.ofSourceRealized(
        'bytes-acceptance/moved',
        ascii(`import silk.usize as usize
import silk.bytes { make, length }
pub fn main() -> i32 {
  let first = make()
  let second = move first
  return usize.toI32(length(&first))
}`),
      )
      assert.include(
        Analysis.diagnostics(moved).map((diagnostic) => diagnostic.code),
        'OWN0001',
      )

      const shared = yield* Analysis.ofSourceRealized(
        'bytes-acceptance/shared-field',
        ascii(`struct Wrapper { values: [u8; 1] }
fn consume(values: &mut [u8]) -> () { return () }
fn invalid(self: &Wrapper) -> () { return consume(&mut self.values) }
pub fn main() -> i32 { return 0 }`),
      )
      assert.include(
        Analysis.diagnostics(shared).map((diagnostic) => diagnostic.code),
        'SEM0057',
      )
    }),
)
