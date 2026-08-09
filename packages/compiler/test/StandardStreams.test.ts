import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as StandardStreams from '../src/StandardStreams.js'

const encoder = new TextEncoder()
const outputRoot = mkdtempSync(join(tmpdir(), 'silk-standard-streams-'))
afterAll(() => rmSync(outputRoot, { recursive: true, force: true }))

const source = `pub effect fn main() -> () ! StreamWriteFailure ? &StandardStreams {
  let first = run StandardStreams.writeAll(StandardStreams.stdout(), "heading\\n")
  let second = run StandardStreams.writeAll(StandardStreams.stderr(), b"warning\\n")
  let third = run StandardStreams.writeAll(StandardStreams.stdout(), "row\\n")
  return ()
}`

const snapshot = (target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('standard-streams/main', encoder.encode(source), target)

it.effect('retains the explicit requirement and reports a missing evaluator provider', () =>
  Effect.gen(function* () {
    const self = yield* snapshot()
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(self.instances.entry._tag, 'Resolved')
    if (self.instances.entry._tag === 'Resolved' && self.instances.entry.kind === 'Effect') {
      assert.deepEqual(
        self.instances.entry.requirements.map((requirement) => requirement.capability.name),
        ['StandardStreams'],
      )
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
        _tag: 'StandardStreamWrite',
        destination: 'Stdout',
        bytes: Array.from(encoder.encode('heading\n')),
      },
      {
        _tag: 'StandardStreamWrite',
        destination: 'Stderr',
        bytes: Array.from(encoder.encode('warning\n')),
      },
      {
        _tag: 'StandardStreamWrite',
        destination: 'Stdout',
        bytes: Array.from(encoder.encode('row\n')),
      },
    ])
    const writes = completed.trace.filter((event) => event._tag === 'StandardStreamWrite')
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
    if (failed._tag === 'UnhandledFailure') assert.include(failed.report, 'StreamWriteFailure')
    assert.strictEqual(failing.events().length, 1)
  }),
)

it.effect('lowers target-neutral writes through native and hosted Wasm boundaries', () =>
  Effect.gen(function* () {
    const native = yield* snapshot()
    const mir = Analysis.loweredMir(native)
    assert.deepEqual(Mir.verify(mir), [])
    assert.strictEqual(
      mir.functions
        .flatMap(Mir.operations)
        .filter((operation) => operation._tag === 'StandardStreamWrite').length,
      3,
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
