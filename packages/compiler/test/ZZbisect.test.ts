import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const llvm = (label: string, text: string) =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSource(`probe/${label}`, ascii(text), 'aarch64-apple-darwin')
    if (native.diagnostics.length > 0) return `diags ${native.diagnostics.length}`
    const out = yield* Effect.exit(Analysis.codegen(native, { mode: 'release' }))
    return out._tag === 'Success' ? 'ok' : String(out.cause).slice(0, 120)
  })

const prelude = `import silk.vector { Vector, make, append, get, length, capacity }
`

const makeOnly = `${prelude}
pub fn main() -> I32 {
  let values = make<I32>()
  if length<I32>(&values) == 0 { return 42 }
  return 0
}`

const appendOnce = `${prelude}
effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<I32>()
  let pending = append<I32>(&mut values, 41) |> Allocator.provide(&mut allocator)
  let appended = run pending
  return 42
}
effect fn recover(error: OutOfMemory) -> I32 { return 7 }
pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

const getOnce = `${prelude}
effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<I32>()
  let pending = append<I32>(&mut values, 42) |> Allocator.provide(&mut allocator)
  let appended = run pending
  return get<I32>(&mut values, 0)
}
effect fn recover(error: OutOfMemory) -> I32 { return 7 }
pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

it.effect('probe', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      {
        makeOnly: yield* llvm('make-only', makeOnly),
        appendOnce: yield* llvm('append-once', appendOnce),
        getOnce: yield* llvm('get-once', getOnce),
      },
      { sentinel: true },
    )
  }),
)
