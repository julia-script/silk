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

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-scanner-acceptance-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

export const scannerSource = `import silk.vector { Vector, make, append, get, length, capacity }

struct U8 { value: I32 }
struct Token { kind: I32 }

fn observe(kind: I32) -> I32 { return kind }

effect fn scan(source: &[U8]) -> Vector<Token> ! OutOfMemory ? &mut Allocator {
  let mut tokens = make<Token>()
  let mut index = 0
  while index < source.length {
    let byte = source[index].value
    let mut kind = 3
    if byte == 1 { kind = 1 }
    if byte == 2 { kind = 2 }
    let token = Token { kind: kind }
    let appended = run append<Token>(&mut tokens, move token)
    index = index + 1
  }
  return move tokens
}

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let source = [
    U8 { value: 1 }, U8 { value: 2 }, U8 { value: 3 }, U8 { value: 1 },
    U8 { value: 2 }, U8 { value: 3 }, U8 { value: 1 }, U8 { value: 2 },
    U8 { value: 3 }, U8 { value: 1 }
  ]
  let pending = scan(&source) |> Allocator.provide(&mut allocator)
  let mut tokens = run pending
  if length<Token>(&tokens) == 10 {} else { return 0 }
  if capacity<Token>(&tokens) == 16 {} else { return 1 }
  let token0 = get<Token>(&mut tokens, 0)
  let token1 = get<Token>(&mut tokens, 1)
  let token2 = get<Token>(&mut tokens, 2)
  let token3 = get<Token>(&mut tokens, 3)
  let token4 = get<Token>(&mut tokens, 4)
  let token5 = get<Token>(&mut tokens, 5)
  let token6 = get<Token>(&mut tokens, 6)
  let token7 = get<Token>(&mut tokens, 7)
  let token8 = get<Token>(&mut tokens, 8)
  let token9 = get<Token>(&mut tokens, 9)
  let kind0 = observe(token0.kind)
  let kind1 = observe(token1.kind)
  let kind2 = observe(token2.kind)
  let kind3 = observe(token3.kind)
  let kind4 = observe(token4.kind)
  let kind5 = observe(token5.kind)
  let kind6 = observe(token6.kind)
  let kind7 = observe(token7.kind)
  let kind8 = observe(token8.kind)
  let kind9 = observe(token9.kind)
  return kind0 + kind1 + kind2 + kind3 + kind4 + kind5 + kind6 + kind7 + kind8 + kind9 + 23
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

it.effect(
  'returns an owned token vector through two reallocations on all three engines',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSource(
        'scanner-acceptance/main',
        ascii(scannerSource),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)
      const tokenKinds = evaluated.trace.flatMap((event) =>
        event._tag === 'Binding' &&
        event.target.name === 'observe' &&
        event.value._tag === 'I32Value'
          ? [event.value.value]
          : [],
      )
      assert.deepEqual(tokenKinds, [1, 2, 3, 1, 2, 3, 1, 2, 3, 1])
      assert.strictEqual(
        evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length,
        3,
      )
      assert.strictEqual(
        evaluated.trace.filter((event) => event._tag === 'AllocationRelease').length,
        3,
      )

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('scanner-acceptance/main', ascii(scannerSource)),
        },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'scanner'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, run.stderr)
    }),
  15_000,
)
