import * as AnalysisFixture from './support/AnalysisFixture.js'
import { base64AcceptanceSource } from './support/base64Acceptance.js'
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const encoder = new TextEncoder()
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-base64-test-'))

afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}

const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang: defaultClang(),
  llvmAr: 'llvm-ar',
})

it.effect('realizes one consolidated Base64 contract on 32-bit and 64-bit targets', () =>
  Effect.gen(function* () {
    for (const target of ['wasm32-unknown-unknown', 'x86_64-unknown-linux-gnu'] as const) {
      const snapshot = yield* AnalysisFixture.retainingMain(
        `base64/acceptance-${target}`,
        encoder.encode(base64AcceptanceSource),
        target,
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const artifact = yield* Analysis.codegen(snapshot, { mode: 'debug' })
      assert.include(artifact.ir, `target triple = "${target}"`)
      assert.isAbove(artifact.bitcode.length, 0)
    }
  }),
)

it.effect('rejects overlapping Base64 input and output borrows', () =>
  Effect.gen(function* () {
    const source = `import silk.base64 { Base64 }
pub fn main() -> i32 {
  let mut bytes: [u8; 4] = [90, 103, 61, 61]
  let encoded = Base64.encodeInto(&mut bytes, &bytes)
  let decoded = Base64.decodeInto(&mut bytes, &bytes)
  drop encoded
  drop decoded
  return 42
}`
    const snapshot = yield* AnalysisFixture.retainingMain(
      'base64/overlapping-borrows',
      encoder.encode(source),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['OWN0010', 'OWN0010'],
    )
  }),
)

it.effect(
  'executes strict allocation-free Base64 through LLVM-to-Wasm',
  () =>
    Effect.gen(function* () {
      const outcome = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('base64/wasm-acceptance', encoder.encode(base64AcceptanceSource)),
          target: 'wasm32-unknown-unknown',
        },
        toolchain,
        optimization: 'release',
        destination: join(destinationRoot, 'base64.wasm'),
        cache: false,
        artifactKind: 'WebAssemblyModule',
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(outcome._tag, 'Compiled')
      if (outcome._tag !== 'Compiled') return
      const module = new WebAssembly.Module(Uint8Array.from(readFileSync(outcome.path)))
      assert.deepEqual(WebAssembly.Module.imports(module), [])
      const instance = new WebAssembly.Instance(module)
      const main = instance.exports['main']
      assert.isFunction(main)
      if (typeof main === 'function') assert.strictEqual(main(), 42)
    }),
  300_000,
)
