import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as WasmBackend from '../src/WasmBackend.js'
import { storedCatchSuspension } from './support/storedCatchSuspension.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const successSource = `import silk.effects as Effect
effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 2 })
}
pub fn main() -> i32 { return run delayed() }`

const retainedStateSource = `import silk.effects as Effect
effect fn delayed() -> i32 {
  let left = 40
  return left + run Effect.suspend(effect { return 2 })
}
pub fn main() -> i32 { return run delayed() }`

const repeatedStateSource = `import silk.effects as Effect
effect fn twice() -> i32 {
  let left = run Effect.suspend(effect { return 40 })
  let right = run Effect.suspend(effect { return 2 })
  return left + right
}
pub fn main() -> i32 { return run twice() }`

const recursiveSource = (depth: number): string => `import silk.effects as Effect
struct Owner { value: i32 }
effect fn count(value: i32) -> i32 {
  if value == 0 { return 0 }
  let next = run Effect.suspend(effect { return value - 1 })
  let inner = run count(next)
  return inner + 1
}
effect fn retainOwner(owner: &mut Owner, value: i32) -> i32 {
  let answer = run count(value)
  return owner.value + answer - answer + 1
}
pub fn main() -> i32 {
  let mut owner = Owner { value: 41 }
  return run retainOwner(&mut owner, ${depth})
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

it.effect('resumes Wasm coroutine frames from inner to outer', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('success', successSource), 2)
  }),
)

it.effect('restores Wasm scalar state retained after run', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('state', retainedStateSource), 42)
  }),
)

it.effect('reuses one Wasm invocation frame across repeated suspension states', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('repeated-state', repeatedStateSource), 42)
  }),
)

it.effect('propagates a resumed typed failure through Wasm handlers', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('failure', storedCatchSuspension), 42)
  }),
)

it.effect('runs deep suspended Wasm recursion with bounded host stack', () =>
  Effect.gen(function* () {
    assert.strictEqual(yield* run('deep', recursiveSource(100_000)), 42)
  }),
)

it.effect('traps when bounded private Wasm execution-stack storage is exhausted', () =>
  Effect.gen(function* () {
    const analysis = yield* Analysis.ofSourceRealized(
      'suspension-wasm/exhaustion',
      ascii(recursiveSource(10_000)),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(analysis), [])
    const artifact = yield* Analysis.codegenWasm(analysis, {
      mode: 'release',
      privateExecutionStackPages: 1,
    })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    assert.strictEqual(typeof main, 'function')
    if (typeof main !== 'function') return
    let trapped = false
    try {
      main()
    } catch (error) {
      trapped = error instanceof WebAssembly.RuntimeError
    }
    assert.isTrue(trapped)
    const memory = instance.exports.__silk_memory_v1
    assert.instanceOf(memory, WebAssembly.Memory)
    if (memory instanceof WebAssembly.Memory) {
      assert.strictEqual(
        new DataView(memory.buffer).getInt32(WasmBackend.statusAddress, true),
        WasmBackend.statusStackOverflow,
      )
    }
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
