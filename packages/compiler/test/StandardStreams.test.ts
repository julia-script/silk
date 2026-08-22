import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as StandardStreams from '../src/StandardStreams.js'
import * as Driver from './support/TestDriver.js'

const encoder = new TextEncoder()
const outputRoot = mkdtempSync(join(tmpdir(), 'silk-standard-streams-'))
afterAll(() => rmSync(outputRoot, { recursive: true, force: true }))

const source = `import silk.core as StandardStream
import silk.core { NativeStandardStreams }
import silk.core { StreamWriteError }
import silk.effect as Effect
pub effect fn main() -> () ! StreamWriteError {
  let mut native = NativeStandardStreams.native()
  let first = run Effect.provideMut(StandardStream.send(StandardStream.stdout(), Intrinsic.stringUtf8Bytes("heading\\n")), &mut native)
  let second = run Effect.provideMut(StandardStream.send(StandardStream.stderr(), b"warning\\n"), &mut native)
  let third = run Effect.provideMut(StandardStream.send(StandardStream.stdout(), Intrinsic.stringUtf8Bytes("row\\n")), &mut native)
  return ()
}`

const snapshot = (target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('standard-streams/main', encoder.encode(source), target)

it.effect('dispatches through the source service and reports a missing host boundary', () =>
  Effect.gen(function* () {
    const self = yield* snapshot()
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(self.instances.entry._tag, 'Resolved')
    if (self.instances.entry._tag === 'Resolved' && self.instances.entry.kind === 'Effect') {
      assert.deepEqual(self.instances.entry.requirements, [])
    }
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag === 'Blocked')
      assert.strictEqual(outcome.reason._tag, 'MissingStandardStreams')
  }),
)

it.effect('records complete ordered writes and typed provider failure deterministically', () =>
  Effect.gen(function* () {
    const self = yield* snapshot()
    const memory = StandardStreams.memory()
    const completed = Analysis.evaluate(self, { standardStreams: memory.provider })
    assert.strictEqual(completed._tag, 'Completed')
    assert.deepEqual(memory.events(), [
      {
        _tag: 'HostWrite',
        destination: 'Stdout',
        bytes: Array.from(encoder.encode('heading\n')),
      },
      {
        _tag: 'HostWrite',
        destination: 'Stderr',
        bytes: Array.from(encoder.encode('warning\n')),
      },
      {
        _tag: 'HostWrite',
        destination: 'Stdout',
        bytes: Array.from(encoder.encode('row\n')),
      },
    ])
    const writes = completed.trace.filter((event) => event._tag === 'HostWrite')
    assert.deepEqual(
      writes.map((event) => [event.destination, event.outcome]),
      [
        ['Stdout', 'Written'],
        ['Stderr', 'Written'],
        ['Stdout', 'Written'],
      ],
    )

    const failing = StandardStreams.memory({ failAt: 1 })
    const failed = Analysis.evaluate(self, { standardStreams: failing.provider })
    assert.strictEqual(failed._tag, 'UnhandledFailure')
    if (failed._tag === 'UnhandledFailure') assert.include(failed.identity, 'StreamWriteError')
    assert.strictEqual(failing.events().length, 1)
  }),
)

it.effect('preserves an arbitrary thrown stream-provider cause in the evaluation trace', () =>
  Effect.gen(function* () {
    const self = yield* snapshot()
    const cause = Object.freeze({ boundary: 'stream', code: 17 })
    const failed = Analysis.evaluate(self, {
      standardStreams: {
        writeAll: () => {
          throw cause
        },
      },
    })
    const write = failed.trace.find((event) => event._tag === 'HostWrite')
    assert.isDefined(write)
    assert.strictEqual(write?.cause, cause)
  }),
)

it.effect('lowers target-neutral writes through native and hosted Wasm boundaries', () =>
  Effect.gen(function* () {
    const native = yield* snapshot()
    const mir = Analysis.loweredMir(native)
    assert.deepEqual(MirVerification.verify(mir), [])
    const hostWrite = mir.functions
      .flatMap(MirVerification.operations)
      .find(
        (operation): operation is Extract<Mir.Operation, { readonly _tag: 'HostWrite' }> =>
          operation._tag === 'HostWrite',
      )
    assert.isDefined(hostWrite)
    for (const failureTag of [0, -1, Number.MAX_SAFE_INTEGER + 1]) {
      const forged = structuredClone(mir)
      const operation = forged.functions
        .flatMap(MirVerification.operations)
        .find((candidate) => candidate._tag === 'HostWrite')
      assert.isDefined(operation)
      if (operation === undefined) return
      Reflect.set(operation, 'failureTag', failureTag)
      assert.include(
        MirVerification.verify(forged).map((violation) => violation.rule),
        'InvalidStandardStreamOperation',
      )
    }
    assert.strictEqual(
      mir.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'HostWrite').length,
      1,
    )
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.include(llvm.ir, '@silk_standard_stream_write_v1')

    const wasm = yield* snapshot('wasm32-unknown-unknown')
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    assert.deepEqual(artifact.hostImports, [
      { module: StandardStreams.wasmModule, name: StandardStreams.wasmWriteAll },
    ])
    assert.include(artifact.wat, StandardStreams.wasmModule)
    const memory = StandardStreams.memory()
    let instance: WebAssembly.Instance | undefined
    const imports = StandardStreams.wasmImports(memory.provider, () => {
      const exported = instance?.exports[StandardStreams.wasmMemoryExport]
      return exported instanceof WebAssembly.Memory ? exported : undefined
    })
    instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), imports)
    const main = instance.exports.silk_main
    if (typeof main !== 'function') throw new Error('standard-stream Wasm lost silk_main')
    assert.strictEqual(main(), 0)
    assert.deepEqual(
      memory.events().map((event) => event.destination),
      ['Stdout', 'Stderr', 'Stdout'],
    )

    assert.throws(
      () => new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {}),
    )
    const failing = StandardStreams.memory({ failAt: 1 })
    let failingInstance: WebAssembly.Instance | undefined
    failingInstance = new WebAssembly.Instance(
      new WebAssembly.Module(artifact.bytes.slice()),
      StandardStreams.wasmImports(failing.provider, () => {
        const exported = failingInstance?.exports[StandardStreams.wasmMemoryExport]
        return exported instanceof WebAssembly.Memory ? exported : undefined
      }),
    )
    const failingMain = failingInstance.exports.silk_main
    if (typeof failingMain !== 'function') throw new Error('failing Wasm lost silk_main')
    assert.strictEqual(failingMain(), 1)
    assert.strictEqual(failing.events().length, 1)

    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('standard-streams/native', encoder.encode(source)),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(outputRoot, 'standard-streams'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 0, run.stderr)
    assert.strictEqual(run.stdout, 'heading\nrow\n')
    assert.strictEqual(run.stderr, 'warning\n')
  }),
)

it.effect('replaces the host provider with a pure source in-memory implementation', () =>
  Effect.gen(function* () {
    const replaced = yield* Analysis.ofSourceRealized(
      'standard-streams/memory',
      encoder.encode(`import silk.core as StandardStream
import silk.core { StandardStreams }
import silk.core { StreamWriteError }
import silk.effect as Effect
struct MemoryStreams { writes: i32 }
effect fn record(
  self: &mut MemoryStreams,
  destination: bool,
  bytes: &[u8]
) -> () {
  self.writes = self.writes + 1
  return ()
}
impl StandardStreams for MemoryStreams { writeAll: MemoryStreams.record }
pub effect fn main() -> () ! StreamWriteError {
  let mut memory = MemoryStreams { writes: 0 }
  let first = run Effect.provideMut(StandardStream.send(StandardStream.stdout(), Intrinsic.stringUtf8Bytes("one")), &mut memory)
  let second = run Effect.provideMut(StandardStream.send(StandardStream.stderr(), Intrinsic.stringUtf8Bytes("two")), &mut memory)
  if memory.writes != 2 { let boom = 1 / 0 }
  return ()
}`),
    )
    assert.deepEqual(Analysis.diagnostics(replaced), [])
    const outcome = Analysis.evaluate(replaced)
    assert.strictEqual(outcome._tag, 'Completed')
    assert.strictEqual(
      Analysis.loweredMir(replaced)
        .functions.flatMap(MirVerification.operations)
        .some((operation) => operation._tag === 'HostWrite'),
      false,
    )
  }),
)
