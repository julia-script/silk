import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { tlsClientWasmSource } from './support/tlsClientAcceptance.js'
import * as Driver from './support/TestDriver.js'

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}

const clang = Effect.runSync(
  Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(defaultClang())),
)
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  llvmAr: 'llvm-ar',
  runtimeObjectCache: NativeToolchain.makeRuntimeObjectCache(),
})

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-driver-tls-client-wasm-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

// Keep this representative compile/execute leg in its own Vitest process. In full-suite order the
// compiler graphs retained by earlier Driver tests amplify this case from ~220 s past its 600 s
// timeout; a fresh process preserves the same assertion without turning heap history into a gate.
it.effect(
  'executes the authenticated TLS client through LLVM-to-Wasm',
  () =>
    Effect.gen(function* () {
      const outcome = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('memory/tls-client-wasm', ascii(tlsClientWasmSource)),
          target: 'wasm32-unknown-unknown',
        },
        toolchain,
        optimization: 'release',
        destination: join(destinationRoot, 'tls-client.wasm'),
        cache: false,
        artifactKind: 'WebAssemblyModule',
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(outcome._tag, 'Compiled')
      if (outcome._tag !== 'Compiled') return
      yield* Effect.sync(() => {
        const module = new WebAssembly.Module(Uint8Array.from(readFileSync(outcome.path)))
        assert.deepEqual(WebAssembly.Module.imports(module), [])
        const main = new WebAssembly.Instance(module).exports['main']
        assert.isFunction(main)
        if (typeof main === 'function') assert.strictEqual(main(), 42)
      })
    }),
  { timeout: 600_000 },
)
