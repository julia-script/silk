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
import { recoveredDirectWrite } from './support/recoveredProvidedWrite.js'
import * as Driver from './support/TestDriver.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()
const outputRoot = mkdtempSync(join(tmpdir(), 'silk-standard-streams-'))
afterAll(() => rmSync(outputRoot, { recursive: true, force: true }))

const source = `import silk.writer { Writer, WriterError, StdoutWriter }
import silk.writer { Writer, WriterError }
import silk.effect { Effect }
pub effect fn main() -> () ! WriterError {
  let mut stdout = Writer.stdoutWriterProvider()
  let mut stderr = Writer.stderrWriterProvider()
  let first = run Effect.provideMut(Writer.writeAll(Intrinsic.stringUtf8Bytes("heading\\n")), &mut stdout)
  let second = run Effect.provideMut(Writer.writeAll(b"warning\\n"), &mut stderr)
  let third = run Effect.provideMut(Writer.writeAll(Intrinsic.stringUtf8Bytes("row\\n")), &mut stdout)
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
    if (failed._tag === 'UnhandledFailure') assert.include(failed.identity, 'WriterError')
    assert.strictEqual(failing.events().length, 1)
  }),
)

const formatterSource = `import silk.effect { Effect }
import silk.format { Format }
import silk.format { Alignment, Display, FormatOptions, Formatter, Sign }
import silk.option { Option }
import silk.usize as usize
import silk.writer { Writer }
import silk.writer { WriterError }

struct Badge {}

effect fn badgeDisplay(
  self: &Badge,
  formatter: &mut Formatter
) -> () ! WriterError ? &mut Writer {
  run Format.writeLeadingPadding(&mut formatter, usize.ONE)
  if Format.color(&formatter) { run Format.write(&mut formatter, b"\\x1b[31m") }
  run Format.write(&mut formatter, b"X")
  if Format.color(&formatter) { run Format.write(&mut formatter, b"\\x1b[0m") }
  return run Format.writeTrailingPadding(&mut formatter, usize.ONE)
}

impl Display for Badge { display: Badge.badgeDisplay }

fn options(color: bool) -> FormatOptions {
  return FormatOptions {
    width: Option.some<usize>(5),
    alignment: Alignment.Center,
    fill: '.',
    sign: Sign.NegativeOnly,
    alternate: false,
    zeroPad: false,
    precision: Option.none<usize>(),
    color: color,
  }
}

effect fn render(color: bool) -> () ! WriterError ? &mut Writer {
  let badge = Badge {}
  return run Format.displayWith(&badge, options(color))
}

pub effect fn main() -> () ! WriterError {
  let mut stdout = Writer.stdoutWriterProvider()
  run render(false) |> Effect.provideMut<Writer>(&mut stdout)
  let mut stderr = Writer.stderrWriterProvider()
  run render(false) |> Effect.provideMut<Writer>(&mut stderr)
  return run render(true) |> Effect.provideMut<Writer>(&mut stderr)
}`

it.effect('lets a nominal Display pad visible content across distinct Writer providers', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'standard-streams/formatter',
      encoder.encode(formatterSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(self), [])

    const memory = StandardStreams.memory()
    const completed = Analysis.evaluate(self, { standardStreams: memory.provider })
    assert.strictEqual(completed._tag, 'Completed')
    const events = memory.events()
    const stdoutRequests = events.slice(0, 3).map((event) => event.bytes)
    const stderrRequests = events.slice(3, 6).map((event) => event.bytes)
    assert.deepEqual(stdoutRequests, stderrRequests)
    assert.deepEqual(
      events.slice(0, 6).map((event) => event.destination),
      ['Stdout', 'Stdout', 'Stdout', 'Stderr', 'Stderr', 'Stderr'],
    )
    assert.strictEqual(decoder.decode(Uint8Array.from(stdoutRequests.flat())), '..X..')
    assert.strictEqual(
      decoder.decode(Uint8Array.from(events.slice(6).flatMap((event) => event.bytes))),
      '..\x1b[31mX\x1b[0m..',
    )

    const failing = StandardStreams.memory({ failAt: 4 })
    const failed = Analysis.evaluate(self, { standardStreams: failing.provider })
    assert.strictEqual(failed._tag, 'UnhandledFailure')
    if (failed._tag === 'UnhandledFailure') assert.include(failed.identity, 'WriterError')
    assert.deepEqual(
      failing
        .events()
        .map((event) => [event.destination, decoder.decode(Uint8Array.from(event.bytes))]),
      [
        ['Stdout', '..'],
        ['Stdout', 'X'],
        ['Stdout', '..'],
        ['Stderr', '..'],
      ],
    )
    const failedWrite = failed.trace.find(
      (event) => event._tag === 'HostWrite' && event.outcome === 'WriteFailure',
    )
    assert.isDefined(failedWrite)
    if (failedWrite?._tag === 'HostWrite') {
      assert.strictEqual(failedWrite.destination, 'Stderr')
      assert.strictEqual(decoder.decode(Uint8Array.from(failedWrite.bytes)), 'X')
    }
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
      2,
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

it.effect('lowers direct typed-failure recovery through LLVM and Wasm', () =>
  Effect.gen(function* () {
    const wasm = yield* Analysis.ofSourceRealized(
      'standard-streams/recovered-direct-write',
      encoder.encode(recoveredDirectWrite),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const memory = StandardStreams.memory()
    let instance: WebAssembly.Instance | undefined
    instance = new WebAssembly.Instance(
      new WebAssembly.Module(artifact.bytes.slice()),
      StandardStreams.wasmImports(memory.provider, () => {
        const exported = instance?.exports[StandardStreams.wasmMemoryExport]
        return exported instanceof WebAssembly.Memory ? exported : undefined
      }),
    )
    const wasmMain = instance.exports.silk_main
    if (typeof wasmMain !== 'function') throw new Error('recovered write Wasm lost silk_main')
    assert.strictEqual(wasmMain(), 0)
    assert.deepEqual(
      memory.events().map((event) => event.bytes),
      [Array.from(encoder.encode('Hello'))],
    )

    const native = yield* Analysis.ofSourceRealized(
      'standard-streams/recovered-direct-write-native',
      encoder.encode(recoveredDirectWrite),
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.include(llvm.ir, '@silk_standard_stream_write_v1')
  }),
)

it.effect('replaces the host provider with a pure source in-memory implementation', () =>
  Effect.gen(function* () {
    const replaced = yield* Analysis.ofSourceRealized(
      'standard-streams/memory',
      encoder.encode(`import silk.writer { Writer, WriterError, StdoutWriter }
import silk.writer { WriterError, Writer }
import silk.effect { Effect }
struct MemoryStreams { writes: i32 }
effect fn record(
  self: &mut MemoryStreams,
  bytes: &[u8]
) -> () {
  self.writes = self.writes + 1
  return ()
}
effect fn flush(self: &mut MemoryStreams) -> () {
  return ()
}
impl Writer for MemoryStreams {
  writeAll: MemoryStreams.record
  flush: MemoryStreams.flush
}
pub effect fn main() -> () ! WriterError {
  let mut memory = MemoryStreams { writes: 0 }
  let first = run Effect.provideMut(Writer.writeAll(Intrinsic.stringUtf8Bytes("one")), &mut memory)
  let second = run Effect.provideMut(Writer.writeAll(Intrinsic.stringUtf8Bytes("two")), &mut memory)
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
