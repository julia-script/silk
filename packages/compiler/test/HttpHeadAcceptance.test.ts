import { httpHeadAcceptanceSource } from './support/httpHeadAcceptance.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Driver from './support/TestDriver.js'

const encoder = new TextEncoder()
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-http-head-test-'))

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

it.effect(
  'compiles the HTTP head reference example',
  () =>
    Effect.gen(function* () {
      const document = readFileSync(
        new URL('../../../apps/docs/content/reference/http-head-parsing.md', import.meta.url),
        'utf8',
      )
      const source = document.match(/```silk\n([\s\S]*?)\n```/)?.[1]
      assert.isString(source)
      if (source === undefined) return
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-head/reference-example',
        encoder.encode(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
    }),
  15_000,
)

it.effect(
  'realizes one consolidated HTTP head contract on 32-bit and 64-bit targets',
  () =>
    Effect.gen(function* () {
      for (const target of ['wasm32-unknown-unknown', 'x86_64-unknown-linux-gnu'] as const) {
        const snapshot = yield* AnalysisFixture.retainingMain(
          `http-head/acceptance-${target}`,
          encoder.encode(httpHeadAcceptanceSource),
          target,
        )
        assert.deepEqual(Analysis.diagnostics(snapshot), [])
        if (snapshot.mir._tag === 'Available') {
          assert.deepEqual(MirVerification.verify(snapshot.mir.value), [])
        }
        const artifact = yield* Analysis.codegen(snapshot, { mode: 'debug' })
        assert.include(artifact.ir, `target triple = "${target}"`)
        assert.isAbove(artifact.bitcode.length, 0)
      }
    }),
  30_000,
)

it.effect(
  'rejects reset while a completed HTTP head view remains live',
  () =>
    Effect.gen(function* () {
      const source = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.http_head { Limits, ParseError, Progress, RequestHead, RequestParser }
import silk.http_headers { Limits as ValueLimits }
import silk.result { Result }

effect fn program() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let limits = Limits {
    maxHeadBytes: 64,
    maxStartLineBytes: 32,
    maxFieldLineBytes: 32,
    maxOwnedBytes: 96,
    values: ValueLimits {
      maxMethodBytes: 8,
      maxTargetBytes: 8,
      maxNameBytes: 8,
      maxValueBytes: 8,
      maxFields: 1,
      maxFieldBytes: 16,
      maxOwnedBytes: 64,
    },
  }
  let made = run RequestParser.make(limits)
  let mut parser = match move made {
    Result<RequestParser, ParseError>.Failure {error} => { return 1 }
    Result<RequestParser, ParseError>.Success {value} => move value
  }
  let fed = RequestParser.feed(&mut parser, b"GET / HTTP/1.0\\r\\n\\r\\n", true)
  drop fed
  let head = RequestParser.head(&parser)
  let reset = RequestParser.reset(&mut parser)
  drop reset
  drop head
  return 42
}

pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run program() |> Effect.provideMut<Allocator>(&mut allocator)
}`
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-head/reset-live-head',
        encoder.encode(source),
      )
      assert.include(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        'OWN0010',
      )
    }),
  15_000,
)

it.effect(
  'executes strict bounded HTTP head parsing through LLVM-to-Wasm',
  () =>
    Effect.gen(function* () {
      const outcome = yield* Driver.compile({
        compilation: {
          root: SourceFile.make(
            'http-head/wasm-acceptance',
            encoder.encode(httpHeadAcceptanceSource),
          ),
          target: 'wasm32-unknown-unknown',
        },
        toolchain,
        optimization: 'release',
        destination: join(destinationRoot, 'http-head.wasm'),
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
