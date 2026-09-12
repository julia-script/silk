import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { tlsConnectionWasmSource } from './support/tlsConnectionAcceptance.js'
import * as Driver from './support/TestDriver.js'
import * as Json from './support/Json.js'

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

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-driver-tls-connection-wasm-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

// This TLS witness needs a fresh process so compiler graphs from unrelated Driver cases do
// not remain in its heap. CI runs it in a separate lane with the measured TLS heap allowance.
it.effect(
  'executes the scoped authenticated TLS connection through LLVM-to-Wasm',
  () =>
    Effect.gen(function* () {
      const outcome = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('memory/tls-connection-wasm', ascii(tlsConnectionWasmSource)),
          target: 'wasm32-unknown-unknown',
        },
        toolchain,
        optimization: 'release',
        destination: join(destinationRoot, 'tls-connection.wasm'),
        cache: false,
        artifactKind: 'WebAssemblyModule',
      }).pipe(Effect.provide(SourceResolver.empty))
      let compilationMessage = 'scoped TLS connection'
      if (outcome._tag === 'BackendFailed') {
        compilationMessage = `${outcome.error.message}\n${Json.stringify(outcome.error.reason)}`
      } else if (outcome._tag === 'Rejected') {
        compilationMessage = outcome.diagnostics
          .map((diagnostic) => `${diagnostic.code}: ${diagnostic.message}`)
          .join('\n')
      }
      assert.strictEqual(outcome._tag, 'Compiled', compilationMessage)
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
