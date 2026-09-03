import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as StandardStreams from '../src/StandardStreams.js'
import * as Driver from './support/TestDriver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const clang = existsSync('/opt/homebrew/opt/llvm/bin/clang')
  ? '/opt/homebrew/opt/llvm/bin/clang'
  : '/usr/bin/clang'
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  shimCache: NativeToolchain.makeShimCache(),
})
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-effect-lowering-stabilization-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const stdoutOf = (streams: StandardStreams.Memory): string =>
  new TextDecoder().decode(
    Uint8Array.from(
      streams
        .events()
        .filter((event) => event.destination === 'Stdout')
        .flatMap((event) => event.bytes),
    ),
  )

/** Runs one program on the evaluator, direct Wasm, and a native executable; asserts parity. */
const runEverywhere = Effect.fnUntraced(function* (
  name: string,
  source: string,
  expected: { readonly status: number; readonly stdout?: string },
) {
  const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')
  assert.deepEqual(Analysis.diagnostics(snapshot), [], name)

  const streams = StandardStreams.memory()
  const evaluated = Analysis.evaluate(snapshot, { standardStreams: streams.provider })
  assert.strictEqual(evaluated._tag, 'Completed', `${name}: ${evaluated._tag}`)
  if (evaluated._tag !== 'Completed') return
  assert.strictEqual(Number(evaluated.result.value), expected.status, name)

  const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
  const wasmStreams = StandardStreams.memory()
  let instance: WebAssembly.Instance | undefined
  instance = new WebAssembly.Instance(
    new WebAssembly.Module(wasm.bytes.slice()),
    StandardStreams.wasmImports(
      wasmStreams.provider,
      () =>
        instance?.exports[StandardStreams.wasmMemoryExport] as
          | { readonly buffer: ArrayBufferLike }
          | undefined,
    ),
  )
  assert.strictEqual((instance.exports.silk_main as () => number)(), expected.status, name)
  if (expected.stdout !== undefined) assert.strictEqual(stdoutOf(wasmStreams), expected.stdout)

  const compiled = yield* Driver.compile({
    compilation: { root: SourceFile.make(name, ascii(source)) },
    toolchain,
    profile: 'release',
    cache: false,
    destination: join(destinationRoot, name.replaceAll('/', '-')),
  }).pipe(Effect.provide(SourceResolver.empty))
  assert.strictEqual(
    compiled._tag,
    'Compiled',
    compiled._tag === 'BackendFailed' ? compiled.error.message : compiled._tag,
  )
  if (compiled._tag !== 'Compiled') return
  const run = spawnSync(compiled.path, [], { encoding: 'utf8', timeout: 60_000 })
  assert.strictEqual(run.signal, null, run.stderr)
  // POSIX exposes only the low unsigned byte of a process exit value.
  assert.strictEqual(run.status, expected.status & 0xff, name)
  if (expected.stdout !== undefined) assert.strictEqual(run.stdout, expected.stdout, name)
})

const loggedResources = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.shared { Shared }
struct UseError {}
struct Log { entries: i32 }
fn record(self: &mut Log, id: i32) -> () { self.entries = self.entries * 10 + id return () }
fn readLog(self: &mut Log) -> i32 { return self.entries }
struct Resource { id: i32 log: Shared<Log> }
impl Drop for Resource {
  fn drop(self: &mut Resource) -> () {
    let id = self.id
    Shared.withMut(&self.log, record(id))
    return ()
  }
}
fn make(id: i32, log: &Shared<Log>) -> Resource {
  let copy = Shared.clone(log)
  return Resource { id: id, log: move copy }
}
effect fn recover(error: UseError) -> () { return () }
effect fn program() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let log = run Shared.make<Log>(Log { entries: 0 }) |> Effect.provideMut<Allocator>(&mut allocator)
  let r3 = make(3, &log)
  let r4 = make(4, &log)
  run Effect.catchAll(retained(move r3, move r4), recover)
  Shared.withMut(&log, record(9))
  let entries = Shared.withMut(&log, readLog)
  drop log
  return entries
}
effect fn recoverOom(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recoverOom) }
`

// ISSUE-83: a value moved into an `effect { }` block and consumed there is cleaned once.
it.effect(
  'cleans an affine value dropped inside an effect block exactly once',
  () =>
    Effect.gen(function* () {
      for (const [name, body] of [
        ['block', 'run effect { drop other return () }\n  fail UseError {}'],
        [
          'suspended-block',
          'run Effect.suspend(effect { drop other return () })\n  fail UseError {}',
        ],
        ['suspended-nofail', 'run Effect.suspend(effect { drop other return () })\n  return ()'],
      ] as const) {
        yield* runEverywhere(
          `effect-lowering/drop-in-block-${name}`,
          `${loggedResources}effect fn retained(resource: Resource, other: Resource) -> () ! UseError {
  ${body}
}
`,
          { status: 439 },
        )
      }
    }),
  300_000,
)

// ISSUE-82: an ordinary fn that runs an effect fn whose body suspends completes on every engine.
it.effect(
  'drives a suspending effect fn run from an ordinary fn to completion',
  () =>
    Effect.gen(function* () {
      yield* runEverywhere(
        'effect-lowering/ordinary-runs-suspending-block',
        `import silk.effect { Effect }
effect fn inner(value: i32) -> i32 { return run Effect.suspend(effect { return value + 1 }) }
fn wrapper(value: i32) -> i32 { return run inner(value) }
pub fn main() -> i32 { return wrapper(1) }
`,
        { status: 2 },
      )
      yield* runEverywhere(
        'effect-lowering/ordinary-runs-suspending-fn',
        `import silk.effect { Effect }
effect fn leaf(value: i32) -> i32 { return value + 1 }
effect fn inner(value: i32) -> i32 { return run Effect.suspend(leaf(value)) }
fn wrapper(value: i32) -> i32 { return run inner(value) }
pub fn main() -> i32 { return wrapper(1) + wrapper(2) }
`,
        { status: 5 },
      )
    }),
  300_000,
)

// ISSUE-24: dispatching a `&Self` operation on an owned (`&mut`-bound) provider builds natively.
it.effect(
  'dispatches shared operations on an owned provider on every engine',
  () =>
    Effect.gen(function* () {
      yield* runEverywhere(
        'effect-lowering/owned-provider',
        `import silk.effect { Effect }
service Counter {
  effect fn get() -> i32 ? &Counter
  effect fn bump() -> () ? &mut Counter
}
struct Cell { n: i32 }
impl Cell {
  effect fn getImpl(self: &Self) -> i32 { return self.n }
  effect fn bumpImpl(self: &mut Self) -> () { self.n = self.n + 1 }
}
impl Counter for Cell {
  get: Cell.getImpl
  bump: Cell.bumpImpl
}
effect fn both() -> i32 ? &mut Counter {
  run Counter.bump()
  run Counter.bump()
  return run Counter.get()
}
pub fn main() -> i32 {
  let cell = Cell { n: 1 }
  return run Effect.bindRequirementOwned<Counter>(both(), move cell)
}
`,
        { status: 3 },
      )
      yield* runEverywhere(
        'effect-lowering/acquired-provider',
        `import silk.effect { Effect }
import silk.writer { Writer }
fn say(s: string) -> () {
  let mut w = Writer.stdoutWriterProvider()
  run Writer.ignoreError(Effect.provideMut(Writer.writeString(s), &mut w))
}
service Counter {
  effect fn get() -> i32 ? &Counter
}
struct Cell { n: i32 }
impl Drop for Cell { fn drop(self: &mut Cell) -> () { say("drop cell\\n") } }
impl Cell {
  effect fn getImpl(self: &Self) -> i32 { return self.n }
}
impl Counter for Cell { get: Cell.getImpl }
struct AcquireError {}
struct WorkError {}
effect fn acquire(flag: bool) -> Cell ! AcquireError {
  say("acquire\\n")
  if flag { fail AcquireError {} }
  return Cell { n: 5 }
}
effect fn work(flag: bool) -> i32 ! WorkError ? &Counter {
  say("work\\n")
  let v = run Counter.get()
  if flag { fail WorkError {} }
  return v
}
effect fn recA(e: AcquireError) -> i32 { say("recA\\n")
  return 100 }
effect fn recW(e: WorkError) -> i32 { say("recW\\n")
  return 200 }
pub fn main() -> i32 {
  let a = run Effect.catch<WorkError>(Effect.catch<AcquireError>(Effect.provideEffect<Counter>(work(false), acquire(false)), recA), recW)
  let b = run Effect.catch<WorkError>(Effect.catch<AcquireError>(Effect.provideEffect<Counter>(work(false), acquire(true)), recA), recW)
  let c = run Effect.catch<WorkError>(Effect.catch<AcquireError>(Effect.provideEffect<Counter>(work(true), acquire(false)), recA), recW)
  return a + b + c
}
`,
        {
          status: 305,
          stdout: 'acquire\nwork\ndrop cell\nacquire\nrecA\nacquire\nwork\ndrop cell\nrecW\n',
        },
      )
    }),
  300_000,
)

// ISSUE-92: a service operation provided as a value inside a combinator chain lowers.
it.effect(
  'lowers a provided service operation value inside a combinator chain',
  () =>
    Effect.gen(function* () {
      const prelude = `import silk.effect { Effect }
import silk.writer { Writer, WriterError }
effect fn recover(error: WriterError) -> () { return () }
`
      for (const [name, helper] of [
        [
          'catch-all-pipe-ordinary',
          `fn helper() -> i32 {
  let mut writer = Writer.stdoutWriterProvider()
  run Effect.catchAll(Writer.writeAll(b"x\\n") |> Effect.provideMut<Writer>(&mut writer), recover)
  return 0
}
pub fn main() -> i32 { return helper() }`,
        ],
        [
          'catch-all-call-ordinary',
          `fn helper() -> i32 {
  let mut writer = Writer.stdoutWriterProvider()
  run Effect.catchAll(Effect.provideMut<Writer>(Writer.writeAll(b"x\\n"), &mut writer), recover)
  return 0
}
pub fn main() -> i32 { return helper() }`,
        ],
        [
          'ignore-error-ordinary',
          `fn helper() -> i32 {
  let mut writer = Writer.stdoutWriterProvider()
  let ignored = run Writer.ignoreError(Writer.writeAll(b"x\\n") |> Effect.provideMut<Writer>(&mut writer))
  drop ignored
  return 0
}
pub fn main() -> i32 { return helper() }`,
        ],
        [
          'ignore-error-effect-fn',
          `effect fn helper() -> i32 {
  let mut writer = Writer.stdoutWriterProvider()
  let ignored = run Writer.ignoreError(Writer.writeAll(b"x\\n") |> Effect.provideMut<Writer>(&mut writer))
  drop ignored
  return 0
}
pub fn main() -> i32 { return run helper() }`,
        ],
      ] as const) {
        yield* runEverywhere(`effect-lowering/service-value-${name}`, `${prelude}${helper}\n`, {
          status: 0,
          stdout: 'x\n',
        })
      }
    }),
  300_000,
)
