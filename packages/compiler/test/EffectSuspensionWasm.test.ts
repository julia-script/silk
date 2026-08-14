import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const successSource = `effect fn delayed() -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return 2 })
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(delayed() |> Effect.provideMut(&mut allocator), recover)
}`

const retainedStateSource = `effect fn delayed() -> i32 ! OutOfMemory ? &mut Allocator {
  let left = 40
  return left + run Effect.suspend(effect { return 2 })
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(delayed() |> Effect.provideMut(&mut allocator), recover)
}`

const typedFailureSource = `struct Problem { code: i32 }
effect fn delayed() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let value = run Effect.suspend(effect { return 2 })
  if value == 2 { fail Problem { code: 35 } }
  return value
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let handled = delayed() |> Effect.catch(recover)
  return run (move handled |> Effect.provideMut(&mut allocator))
}`

const recursiveSource = (
  depth: number,
): string => `effect fn count(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  if value == 0 { return 0 }
  let next = run Effect.suspend(effect { return value - 1 })
  let inner = run count(next)
  return inner + 1
}
effect fn recover(error: OutOfMemory) -> i32 { return 1 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let answer = run Effect.catch(count(${depth}) |> Effect.provideMut(&mut allocator), recover)
  if answer == ${depth} { return 42 }
  return 2
}`

const run = Effect.fnUntraced(function* (name: string, source: string) {
  const analysis = yield* Analysis.ofSourceRealized(
    `suspension-wasm/${name}`,
    ascii(source),
    'wasm32-unknown-unknown',
  )
  assert.deepEqual(Analysis.diagnostics(analysis), [])
  const artifact = yield* Analysis.codegenWasm(analysis, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
  const main = instance.exports.silk_main
  assert.strictEqual(typeof main, 'function')
  if (typeof main !== 'function') return -1
  return main()
})

it.effect('resumes Wasm continuation frames from inner to outer', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('success', successSource), 2)
  }),
)

it.effect('restores Wasm scalar state retained after run', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('state', retainedStateSource), 42)
  }),
)

it.effect('propagates a resumed typed failure through Wasm handlers', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('failure', typedFailureSource), 42)
  }),
)

it.effect('runs deep suspended Wasm recursion with bounded host stack', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('deep', recursiveSource(100_000)), 42)
  }),
)

it.effect('uses a private iterative Wasm suspension protocol', () =>
  Effect.gen(function* () {
    const analysis = yield* Analysis.ofSourceRealized(
      'suspension-wasm/shape',
      ascii(successSource),
      'wasm32-unknown-unknown',
    )
    const artifact = yield* Analysis.codegenWasm(analysis, { mode: 'debug' })
    const binaryText = new TextDecoder().decode(artifact.bytes)

    assert.include(binaryText, '$suspend_step')
    assert.include(binaryText, 'silk_suspend_child_')
    assert.include(binaryText, 'silk_suspend_resume_')
    assert.notInclude(binaryText, 'return_call')
    assert.notInclude(binaryText, 'promise')
  }),
)
