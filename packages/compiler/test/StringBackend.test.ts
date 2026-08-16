import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as Target from '../src/Target.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parity = `fn pass(value: string) -> string { return value }

pub fn main() -> i32 {
  let bytes = [u8.toU8(115), u8.toU8(105), u8.toU8(108), u8.toU8(107)]
  unsafe {
    let runtime = Intrinsic.stringFromUtf8Unchecked(&bytes)
    let returned = pass(runtime)
    let raw = Intrinsic.stringUtf8Bytes(returned)
    if Intrinsic.stringByteLength(returned) == 4 {} else { return 1 }
    if raw.length == 4 {} else { return 2 }
    if u8.toI32(raw[0]) == 115 {} else { return 3 }
    if returned == "silk" {} else { return 4 }
    if returned != "silk!" {} else { return 5 }
    return 42
  }
  return 0
}`

it.effect(
  'emits static/runtime strings, calls, byte views, lengths, and exact equality on Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'string-backend/parity',
        ascii(parity),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  60_000,
)

it.effect(
  'keeps UTF-8 string identity debug-only and emits deterministic native and Wasm artifacts',
  () =>
    Effect.gen(function* () {
      const source = `fn pass(value: string) -> string { return value }
fn byteCount(value: &[u8]) -> usize { return value.length }

pub fn main() -> i32 {
  let text = pass("caf\\u{e9}")
  let bytes = Intrinsic.stringUtf8Bytes(text)
  if byteCount(bytes) == 5 { return 42 }
  return 0
}`
      const host = yield* Target.host()
      const nativeFirst = yield* Analysis.ofSourceRealized(
        'string-backend/debug-native',
        ascii(source),
        host.id,
      )
      const nativeSecond = yield* Analysis.ofSourceRealized(
        'string-backend/debug-native',
        ascii(source),
        host.id,
      )
      const wasmFirst = yield* Analysis.ofSourceRealized(
        'string-backend/debug-wasm',
        ascii(source),
        Target.wasm32UnknownUnknown.id,
      )
      const wasmSecond = yield* Analysis.ofSourceRealized(
        'string-backend/debug-wasm',
        ascii(source),
        Target.wasm32UnknownUnknown.id,
      )
      assert.deepEqual(Analysis.diagnostics(nativeFirst), [])
      assert.deepEqual(Analysis.diagnostics(wasmFirst), [])

      const nativeDebugFirst = yield* Analysis.codegen(nativeFirst, {
        mode: 'debug',
        sources: new Map([['string-backend/debug-native', ascii(source)]]),
      })
      const nativeDebugSecond = yield* Analysis.codegen(nativeSecond, {
        mode: 'debug',
        sources: new Map([['string-backend/debug-native', ascii(source)]]),
      })
      const nativeRelease = yield* Analysis.codegen(nativeFirst, { mode: 'release' })
      assert.include(nativeDebugFirst.ir, '!DIStringType(name: "string"')
      assert.include(nativeDebugFirst.ir, 'encoding: DW_ATE_UTF')
      assert.include(nativeDebugFirst.ir, 'name: "&[u8]"')
      assert.notInclude(nativeRelease.ir, 'DIStringType')
      assert.notInclude(nativeRelease.ir, 'DW_ATE_UTF')
      assert.strictEqual(nativeDebugFirst.ir, nativeDebugSecond.ir)
      assert.deepEqual(nativeDebugFirst.bitcode, nativeDebugSecond.bitcode)

      const objectBytes = (artifact: typeof nativeDebugFirst, name: string): Uint8Array =>
        NativeToolchain.withBuildScope(name, (scope) => {
          const emitted = NativeToolchain.emitObject(
            Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
            scope,
            artifact,
            host,
            'debug',
          )
          assert.strictEqual(emitted._tag, 'ObjectArtifact')
          if (emitted._tag !== 'ObjectArtifact') return new Uint8Array()
          return readFileSync(emitted.artifact.path)
        })
      assert.deepEqual(
        objectBytes(nativeDebugFirst, 'string-debug-object-first'),
        objectBytes(nativeDebugSecond, 'string-debug-object-second'),
      )

      const wasmDebugFirst = yield* Analysis.codegenWasm(wasmFirst, { mode: 'debug' })
      const wasmDebugSecond = yield* Analysis.codegenWasm(wasmSecond, { mode: 'debug' })
      const wasmRelease = yield* Analysis.codegenWasm(wasmFirst, { mode: 'release' })
      assert.include(wasmDebugFirst.wat, '$string')
      assert.include(wasmDebugFirst.wat, '$bytes')
      assert.notInclude(wasmRelease.wat, '$string')
      assert.notInclude(wasmRelease.wat, '$bytes')
      assert.include(new TextDecoder().decode(wasmDebugFirst.bytes), 'string_utf8')
      assert.notInclude(new TextDecoder().decode(wasmRelease.bytes), 'string_utf8')
      assert.strictEqual(wasmDebugFirst.wat, wasmDebugSecond.wat)
      assert.deepEqual(wasmDebugFirst.bytes, wasmDebugSecond.bytes)
    }),
  60_000,
)
