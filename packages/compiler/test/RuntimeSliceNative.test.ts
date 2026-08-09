import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct Pair { left: I32 right: I32 }
fn replace(values: &mut [Pair], index: I32) -> I32 {
  values[index] = Pair { left: 40, right: 2 }
  return values.length
}
pub fn main() -> I32 {
  let mut values = [Pair { left: 1, right: 2 }, Pair { left: 3, right: 4 }]
  let length = replace(&mut values, 1)
  return values[1].left + length
}`

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-runtime-slice-native-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect(
  'emits typed pointer lanes, stride-aware storage, and deterministic native artifacts',
  () =>
    Effect.gen(function* () {
      const self = yield* Analysis.ofSource(
        'runtime-slice-native/main',
        ascii(source),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(self), [])
      const first = yield* Analysis.codegen(self, { mode: 'release' })
      const second = yield* Analysis.codegen(self, { mode: 'release' })

      assert.include(first.ir, 'ptr %')
      assert.include(first.ir, 'alloca i8')
      assert.include(first.ir, 'getelementptr i8')
      assert.include(first.ir, 'slice')
      assert.include(first.ir, 'store i32')
      assert.strictEqual(first.ir, second.ir)
      assert.deepEqual(first.bitcode, second.bitcode)
    }),
)

it.effect(
  'executes exclusive aggregate replacement and observes the callee write in the caller',
  () =>
    Effect.gen(function* () {
      const toolchain: NativeToolchain.Toolchain = Object.freeze({
        _tag: 'Toolchain',
        clang: '/usr/bin/clang',
      })
      const compiled = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('runtime-slice-native/main', ascii(source)),
        },
        toolchain,
        profile: 'release',
        destination: join(destinationRoot, 'exclusive-slice'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(
        compiled._tag,
        'Compiled',
        compiled._tag === 'BackendFailed' ? compiled.error.message : undefined,
      )
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, run.stderr)
    }),
)

it.effect('traps an equal-length native slice index before loading memory', () =>
  Effect.gen(function* () {
    const boundsSource = `fn choose(values: &[I32], index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { let values = [10, 20] return choose(&values, 2) }`
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('runtime-slice-native/bounds', ascii(boundsSource)),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'slice-bounds'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.notStrictEqual(run.signal, null)
  }),
)
