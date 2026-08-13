import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const encoder = new TextEncoder()
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-string-acceptance-'))

afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

const assertRunsEverywhere = Effect.fnUntraced(function* (
  sourceId: string,
  source: string,
  expected: number,
) {
  const bytes = encoder.encode(source)
  const snapshot = yield* Analysis.ofSourceRealized(sourceId, bytes, 'wasm32-unknown-unknown')
  assert.deepEqual(Analysis.diagnostics(snapshot), [])

  const evaluated = Analysis.evaluate(snapshot)
  assert.strictEqual(
    evaluated._tag,
    'Completed',
    JSON.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
  )
  if (evaluated._tag === 'Completed') {
    assert.strictEqual(evaluated.result.value, expected)
  }

  const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
  assert.strictEqual((instance.exports.silk_main as () => number)(), expected)

  const compiled = yield* Driver.compile({
    compilation: { root: SourceFile.make(sourceId, bytes) },
    toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
    profile: 'release',
    destination: join(destinationRoot, sourceId.replaceAll('/', '-')),
  }).pipe(Effect.provide(SourceResolver.empty))
  assert.strictEqual(
    compiled._tag,
    'Compiled',
    JSON.stringify(compiled, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
  )
  if (compiled._tag === 'Compiled') {
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, expected, run.stderr)
  }
})

const ownedAndScalars = `import silk.string {
  ScalarCursor,
  ScalarStep,
  copy,
  append,
  view,
  scalarCursor,
  nextScalar,
  scalarValue,
  nextCursor
}
import silk.option { Some, None }

fn scalarSum(value: string, cursor: ScalarCursor) -> u32 {
  return match move nextScalar(value, move cursor) {
    Some<ScalarStep> { value: step } => continueSum(value, move step)
    None nothing => u32.toU32(0)
  }
}

fn continueSum(value: string, step: ScalarStep) -> u32 {
  let scalar = scalarValue(&step)
  let cursor = nextCursor(move step)
  return scalar + scalarSum(value, move cursor)
}

effect fn build() -> i32 ! OutOfMemory {
  let literal = "A\\u{a2}"
  if literal == "A\\u{a2}" {} else { return 1 }
  if literal != "A\\u{a3}" {} else { return 2 }

  let mut allocator = SystemAllocator.make()
  let copying = copy(literal) |> Effect.provideMut(&mut allocator)
  let mut owned = run copying
  let appending = append(&mut owned, "\\u{20ac}\\u{10348}")
    |> Effect.provideMut(&mut allocator)
  let appended = run appending
  let borrowed = view(&owned)
  if borrowed == "A\\u{a2}\\u{20ac}\\u{10348}" {} else { return 3 }
  if scalarSum(borrowed, scalarCursor()) == u32.toU32(74967) {} else { return 4 }
  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

it.effect(
  'runs literals, owned copy/view/append, exact equality, and scalar traversal everywhere',
  () => assertRunsEverywhere('string-acceptance/owned-scalars', ownedAndScalars, 42),
  60_000,
)

const validation = `import silk.string { InvalidUtf8, fromUtf8, byteLength }
import silk.result { Result, Success, Failure }

fn inspect(bytes: &[u8]) -> i32 {
  let result = fromUtf8(bytes)
  return match move result {
    Result<string, InvalidUtf8> { value: outcome } => match move outcome {
      Success<string> { value } => usize.toI32(byteLength(value))
      Failure<InvalidUtf8> { error } => usize.toI32(error.offset) + 40
    }
  }
}

pub fn main() -> i32 {
  return inspect(b"ok") + inspect(b"a\\x80") - 1
}`

it.effect(
  'validates UTF-8 safely and reports invalid input everywhere',
  () => assertRunsEverywhere('string-acceptance/validation', validation, 42),
  60_000,
)

const allocationFailure = `import silk.string { copy, append, view, ownedByteLength }

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  if self.remaining == 0 { fail OutOfMemory {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  return run pending
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn ignore(error: OutOfMemory) -> () { return () }
effect fn recover(error: OutOfMemory) -> i32 { return 0 }

effect fn build() -> i32 ! OutOfMemory {
  // One allocation, spent by the copy. Appending grows the copied storage in place rather than
  // copying it into fresh storage first, so starving the append takes one fewer than it used to.
  let mut allocator = QuotaAllocator { remaining: 1 }
  let copying = copy("ok") |> Effect.provideMut(&mut allocator)
  let mut value = run copying
  let recovered = run Effect.catch(
    append(&mut value, "\\u{1f642}") |> Effect.provideMut(&mut allocator),
    ignore
  )
  if view(&value) == "ok" {} else { return 1 }
  if ownedByteLength(&value) == 2 {} else { return 2 }
  return 42
}

pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

it.effect(
  'rolls owned append back after allocation failure everywhere',
  () => assertRunsEverywhere('string-acceptance/allocation-failure', allocationFailure, 42),
  60_000,
)

it.effect('requires an unsafe boundary for unchecked UTF-8 formation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'string-acceptance/unsafe-authorization',
      encoder.encode(
        'fn view(bytes: &[u8]) -> string { return Intrinsic.stringFromUtf8Unchecked(bytes) }',
      ),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        reason: diagnostic.reason._tag,
      })),
      [{ code: 'SEM0082', reason: 'MissingUnsafeBoundary' }],
    )
  }),
)
