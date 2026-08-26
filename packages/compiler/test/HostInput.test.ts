import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as HostInput from '../src/HostInput.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Driver from './support/TestDriver.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('host-input/main', encoder.encode(source), target)

/**
 * A pure in-source provider. It answers from a scripted command line, a one-name environment, and a
 * fixed working directory, so a caller reaches the complete service without a host boundary.
 */
const scriptedProvider = `import silk.bytes { Bytes }
import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.host_input { HostInput }
import silk.host_input { HostInputError }
import silk.option { Option }
import silk.u8 as u8
import silk.usize as usize
struct Scripted {
  count: usize
}

fn scripted(count: usize) -> Scripted {
  return Scripted { count: count }
}

effect fn scriptedCount(self: &mut Scripted) -> usize ! HostInputError {
  return self.count
}

effect fn scriptedArgument(
  self: &mut Scripted,
  index: usize
) -> Option<Bytes> ! HostInputError | OutOfMemoryError ? &mut Allocator {
  if index >= self.count { return none<Bytes>() }
  let source = [u8.toU8(97), u8.toU8(98), u8.toU8(99)]
  let one = [source[index]]
  let owned = run bytesCopy(&one)
  return some<Bytes>(move owned)
}

// The scripted environment holds exactly one four-byte name whose value is not valid UTF-8.
effect fn scriptedVariable(
  self: &mut Scripted,
  name: &[u8]
) -> Option<Bytes> ! HostInputError | OutOfMemoryError ? &mut Allocator {
  if name.length != usize.add(0, 4) { return none<Bytes>() }
  let value = [u8.toU8(200), u8.toU8(201)]
  let owned = run bytesCopy(&value)
  return some<Bytes>(move owned)
}

effect fn scriptedDirectory(
  self: &mut Scripted
) -> Bytes ! HostInputError | OutOfMemoryError ? &mut Allocator {
  let value = [u8.toU8(47), u8.toU8(119)]
  return run bytesCopy(&value)
}

impl HostInput for Scripted {
  argumentCount: Scripted.scriptedCount
  argument: Scripted.scriptedArgument
  variable: Scripted.scriptedVariable
  workingDirectory: Scripted.scriptedDirectory
}

struct Broken {}

effect fn brokenCount(self: &mut Broken) -> usize ! HostInputError {
  fail inputFailure()
}

effect fn brokenArgument(
  self: &mut Broken,
  index: usize
) -> Option<Bytes> ! HostInputError | OutOfMemoryError ? &mut Allocator {
  fail inputFailure()
}

effect fn brokenVariable(
  self: &mut Broken,
  name: &[u8]
) -> Option<Bytes> ! HostInputError | OutOfMemoryError ? &mut Allocator {
  fail inputFailure()
}

effect fn brokenDirectory(
  self: &mut Broken
) -> Bytes ! HostInputError | OutOfMemoryError ? &mut Allocator {
  fail inputFailure()
}

impl HostInput for Broken {
  argumentCount: Broken.brokenCount
  argument: Broken.brokenArgument
  variable: Broken.brokenVariable
  workingDirectory: Broken.brokenDirectory
}
`

/** Shared readers. Owned host bytes are read through a stable binding, never through an index. */
const support = `import silk.bytes { Bytes }
import silk.host_input { HostInputError }
import silk.option { None }
import silk.option { Option }
import silk.option { Some }
import silk.result { Result }
import silk.string { InvalidUtf8 }
import silk.usize as usize
fn firstOf(entry: &Bytes) -> u8 {
  return bytesSlice(entry)[usize.ZERO]
}

fn byteAt(entry: &Bytes, index: usize) -> u8 {
  return bytesSlice(entry)[index]
}

fn present(value: Option<Bytes>) -> bool {
  return match move value {
    Some<Bytes> { value: bytes } => true
    None {} => false
  }
}

fn decodes(entry: &Bytes) -> bool {
  return match move text(bytesSlice(entry)) {
    Result<string, InvalidUtf8> { value: outcome } => match move outcome {
      Success<string> { value: view } => true
      Failure<InvalidUtf8> { error: invalid } => false
    }
  }
}

effect fn raiseMissing() -> never ! HostInputError {
  fail inputFailure()
}

effect fn required(found: Option<Bytes>) -> Bytes ! HostInputError {
  return match move found {
    Some<Bytes> { value: bytes } => move bytes
    None {} => run raiseMissing()
  }
}
`

const preamble = `import silk.bytes { Bytes, asSlice as bytesSlice, copy as bytesCopy, length as bytesLength }
import silk.host_input {
  HostInput,
  HostInputError,
  argument,
  argumentCount,
  arguments as hostArguments,
  inputFailure,
  text,
  variable,
  variableNamed,
  workingDirectory
}
import silk.option { None, Option, Some, none, some }
import silk.result { Failure, Result, Success }
import silk.string { InvalidUtf8 }
import silk.vector { Vector, length as vectorLength, remove as vectorRemove }

`

const scriptedSource = (body: string) => `${preamble}
${support}
${scriptedProvider}
${body}`

const nativeSource = (body: string) => `${preamble}
${support}
${body}`

it.effect('returns the command-line arguments in the order the process received them', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedSource(`import silk.bytes { Bytes }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.host_input { HostInputError }
import silk.u8 as u8
import silk.usize as usize
effect fn program() -> i32 ! HostInputError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut provider = scripted(usize.add(0, 3))
  let total = run Effect.provideMut(argumentCount(), &mut provider)
  if total != usize.add(0, 3) { return 1 }
  let collected = run Effect.provideMut(
    Effect.provideMut(hostArguments(), &mut allocator),
    &mut provider
  )
  let mut owned = move collected
  if vectorLength<Bytes>(&owned) != usize.add(0, 3) { return 2 }
  let first = vectorRemove<Bytes>(&mut owned, usize.ZERO)
  if firstOf(&first) != u8.toU8(97) { return 3 }
  let second = vectorRemove<Bytes>(&mut owned, usize.ZERO)
  if firstOf(&second) != u8.toU8(98) { return 4 }
  let third = vectorRemove<Bytes>(&mut owned, usize.ZERO)
  if firstOf(&third) != u8.toU8(99) { return 5 }
  return 42
}

effect fn recover(error: HostInputError | OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('reports an argument past the end and an unset variable as absence, not failure', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedSource(`import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.host_input { HostInputError }
import silk.usize as usize
effect fn program() -> i32 ! HostInputError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut provider = scripted(usize.ONE)
  let past = run Effect.provideMut(
    Effect.provideMut(argument(usize.add(0, 9)), &mut allocator),
    &mut provider
  )
  if present(move past) { return 1 }
  let unset = run Effect.provideMut(
    Effect.provideMut(variableNamed("NO"), &mut allocator),
    &mut provider
  )
  if present(move unset) { return 2 }
  let set = run Effect.provideMut(
    Effect.provideMut(variableNamed("PATH"), &mut allocator),
    &mut provider
  )
  if present(move set) == false { return 3 }
  return 42
}

// A recovery branch that survives proves absence never entered the failure channel.
effect fn recover(error: HostInputError | OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('keeps a value that is not valid UTF-8 readable as its exact bytes', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedSource(`import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.host_input { HostInputError }
import silk.u8 as u8
import silk.usize as usize
effect fn program() -> i32 ! HostInputError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut provider = scripted(usize.ONE)
  let found = run Effect.provideMut(
    Effect.provideMut(variableNamed("PATH"), &mut allocator),
    &mut provider
  )
  let owned = run required(move found)
  if bytesLength(&owned) != usize.add(0, 2) { return 1 }
  // 0xC8 0xC9 is not a UTF-8 sequence, and both bytes survive the service exactly.
  if byteAt(&owned, usize.ZERO) != u8.toU8(200) { return 2 }
  if byteAt(&owned, usize.ONE) != u8.toU8(201) { return 3 }
  // The textual view is checked, so it refuses those bytes rather than replacing them.
  if decodes(&owned) { return 4 }
  let directory = run Effect.provideMut(
    Effect.provideMut(workingDirectory(), &mut allocator),
    &mut provider
  )
  if firstOf(&directory) != u8.toU8(47) { return 5 }
  if decodes(&directory) == false { return 6 }
  return 42
}

effect fn recover(error: HostInputError | OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('routes an in-source provider error into the typed failure channel', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedSource(`import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
import silk.host_input { HostInputError }
effect fn program() -> i32 ! HostInputError | OutOfMemoryError {
  let mut provider = Broken {}
  let total = run Effect.provideMut(argumentCount(), &mut provider)
  return 1
}

effect fn recover(error: HostInputError | OutOfMemoryError) -> i32 { return 42 }

pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

const nativeProgram = nativeSource(`import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.host_input { HostInputError }
import silk.os_host_input { OsHostInput }
import silk.u8 as u8
import silk.usize as usize
effect fn program() -> i32 ! HostInputError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut provider = OsHostInput.make()
  let total = run Effect.provideMut(argumentCount(), &mut provider)
  if total != usize.add(0, 3) { return 1 }
  let found = run Effect.provideMut(
    Effect.provideMut(argument(usize.ONE), &mut allocator),
    &mut provider
  )
  let owned = run required(move found)
  if bytesLength(&owned) != usize.add(0, 3) { return 2 }
  if byteAt(&owned, usize.ONE) != u8.toU8(255) { return 3 }
  let unset = run Effect.provideMut(
    Effect.provideMut(variableNamed("SILK_UNSET"), &mut allocator),
    &mut provider
  )
  if present(move unset) { return 4 }
  let set = run Effect.provideMut(
    Effect.provideMut(variableNamed("SILK_MODE"), &mut allocator),
    &mut provider
  )
  let value = run required(move set)
  if bytesLength(&value) != usize.add(0, 2) { return 5 }
  if byteAt(&value, usize.ZERO) != u8.toU8(200) { return 6 }
  let directory = run Effect.provideMut(
    Effect.provideMut(workingDirectory(), &mut allocator),
    &mut provider
  )
  if firstOf(&directory) != u8.toU8(47) { return 7 }
  return 42
}

effect fn recover(error: HostInputError | OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`)

const nativeHost = () =>
  HostInput.memory({
    arguments: [HostInput.ascii('/bin/tool'), [0x61, 0xff, 0x62], HostInput.ascii('third')],
    environment: [[HostInput.ascii('SILK_MODE'), [0xc8, 0xc9]]],
    workingDirectory: HostInput.ascii('/work/space'),
  })

it.effect('reaches the native provider through the injected OS host boundary', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(nativeProgram)
    assert.deepEqual(Analysis.diagnostics(self), [])

    const blocked = Analysis.evaluate(self)
    assert.strictEqual(blocked._tag, 'Blocked')
    if (blocked._tag === 'Blocked') assert.strictEqual(blocked.reason._tag, 'MissingHostInput')

    const host = nativeHost()
    const outcome = Analysis.evaluate(self, { hostInput: host.provider })
    assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    assert.deepEqual(host.events(), [
      { _tag: 'ArgumentCount' },
      { _tag: 'Argument', index: 1 },
      { _tag: 'Variable', name: HostInput.ascii('SILK_UNSET') },
      { _tag: 'Variable', name: HostInput.ascii('SILK_MODE') },
      { _tag: 'WorkingDirectory' },
    ])
  }),
)

it.effect('copies a value longer than the provider buffer completely', () =>
  Effect.gen(function* () {
    const long = Object.freeze(Array.from({ length: 300 }, (_, index) => (index % 251) + 1))
    const self = yield* snapshot(
      nativeSource(`import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.host_input { HostInputError }
import silk.os_host_input { OsHostInput }
import silk.u8 as u8
import silk.usize as usize
effect fn program() -> i32 ! HostInputError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut provider = OsHostInput.make()
  let found = run Effect.provideMut(
    Effect.provideMut(argument(usize.ZERO), &mut allocator),
    &mut provider
  )
  let owned = run required(move found)
  if bytesLength(&owned) != usize.add(0, 300) { return 1 }
  if byteAt(&owned, usize.ZERO) != u8.toU8(1) { return 2 }
  if byteAt(&owned, usize.add(0, 299)) != u8.toU8(49) { return 3 }
  return 42
}

effect fn recover(error: HostInputError | OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const host = HostInput.memory({ arguments: [long] })
    const outcome = Analysis.evaluate(self, { hostInput: host.provider })
    assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    // The first pass reports the complete length from a short buffer; the second copies it.
    assert.deepEqual(host.events(), [
      { _tag: 'Argument', index: 0 },
      { _tag: 'Argument', index: 0 },
    ])
  }),
)

it.effect('routes a native host error into the typed failure channel', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(nativeProgram)
    const host = HostInput.memory({
      arguments: [HostInput.ascii('/bin/tool')],
      failing: ['argumentCount'],
    })
    const outcome = Analysis.evaluate(self, { hostInput: host.provider })
    assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 0n)
  }),
)

it.effect('lowers the native reads to reachable-only native runtime symbols', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(nativeProgram)
    const operations = Analysis.loweredMir(self)
      .functions.flatMap(MirVerification.operations)
      .filter((operation) => operation._tag === 'OsCall')
    assert.deepEqual([...new Set(operations.map((operation) => operation.operation.name))].sort(), [
      'osHostArgument',
      'osHostArgumentCount',
      'osHostVariable',
      'osHostWorkingDirectory',
    ])

    const llvm = yield* Analysis.codegen(self, { mode: 'release' })
    assert.include(llvm.ir, '@silk_os_host_argument_count_v1')
    assert.include(llvm.ir, '@silk_os_host_argument_v1')
    assert.include(llvm.ir, '@silk_os_host_variable_v1')
    assert.include(llvm.ir, '@silk_os_host_working_directory_v1')
    assert.notInclude(llvm.ir, '@silk_os_file_read_v1')

    const wasm = yield* snapshot(nativeProgram, 'wasm32-unknown-unknown')
    const availability = IntrinsicAvailability.select(wasm.instances.intrinsics, 'Wasm')
    assert.strictEqual(availability._tag, 'Unavailable')
    if (availability._tag === 'Unavailable')
      assert.deepEqual(
        [...new Set(availability.diagnostics.map((diagnostic) => diagnostic.code))],
        ['SEM0093'],
      )
  }),
)

it('answers scripted input and reports host failures at the memory boundary', () => {
  const host = HostInput.memory({
    arguments: [[1, 2], [3]],
    environment: [[HostInput.ascii('A'), [0xff]]],
    workingDirectory: [47],
  })
  assert.deepEqual(host.provider.argumentCount(), { _tag: 'Count', count: 2 })
  assert.deepEqual(host.provider.argument(1), { _tag: 'Present', bytes: [3] })
  assert.deepEqual(host.provider.argument(2), { _tag: 'Absent' })
  assert.deepEqual(host.provider.variable(HostInput.ascii('A')), {
    _tag: 'Present',
    bytes: [0xff],
  })
  assert.deepEqual(host.provider.variable(HostInput.ascii('B')), { _tag: 'Absent' })
  assert.deepEqual(host.provider.workingDirectory(), { _tag: 'Present', bytes: [47] })

  const failing = HostInput.memory({ failing: ['variable', 'workingDirectory'] })
  assert.strictEqual(failing.provider.variable([])._tag, 'LookupFailure')
  assert.strictEqual(failing.provider.workingDirectory()._tag, 'LookupFailure')
  assert.deepEqual(failing.provider.argumentCount(), { _tag: 'Count', count: 0 })
})

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}

const clang = process.env.SILK_TEST_CLANG ?? defaultClang()

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-host-input-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

it.effect(
  'reads the real command line and environment of a compiled native program',
  () =>
    Effect.gen(function* () {
      // The exit status is derived from the arguments the process actually received, so a shim that
      // dropped `argc` and `argv` could not produce it.
      const source = nativeSource(`import silk.bytes { Bytes }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.host_input { HostInputError }
import silk.os_host_input { OsHostInput }
import silk.u8 as u8
import silk.usize as usize
effect fn program() -> i32 ! HostInputError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut provider = OsHostInput.make()
  let total = run Effect.provideMut(argumentCount(), &mut provider)
  if total != usize.add(0, 4) { return 1 }
  // The three passed arguments follow the program name, in the order the process received them.
  let collected = run Effect.provideMut(
    Effect.provideMut(hostArguments(), &mut allocator),
    &mut provider
  )
  let mut owned = move collected
  if vectorLength<Bytes>(&owned) != usize.add(0, 4) { return 2 }
  let program = vectorRemove<Bytes>(&mut owned, usize.ZERO)
  let first = vectorRemove<Bytes>(&mut owned, usize.ZERO)
  if firstOf(&first) != u8.toU8(97) { return 3 }
  let second = vectorRemove<Bytes>(&mut owned, usize.ZERO)
  if firstOf(&second) != u8.toU8(98) { return 8 }
  if bytesLength(&second) != usize.add(0, 4) { return 10 }
  let third = vectorRemove<Bytes>(&mut owned, usize.ZERO)
  if firstOf(&third) != u8.toU8(103) { return 11 }
  let unset = run Effect.provideMut(
    Effect.provideMut(variableNamed("SILK_ABSENT_NAME"), &mut allocator),
    &mut provider
  )
  if present(move unset) { return 4 }
  let set = run Effect.provideMut(
    Effect.provideMut(variableNamed("SILK_EXIT_MARK"), &mut allocator),
    &mut provider
  )
  let mark = run required(move set)
  if bytesLength(&mark) != usize.ONE { return 5 }
  let directory = run Effect.provideMut(
    Effect.provideMut(workingDirectory(), &mut allocator),
    &mut provider
  )
  if firstOf(&directory) != u8.toU8(47) { return 6 }
  // 'K' is 75, so the marked exit status is 75 - 33 = 42.
  return u8.toI32(firstOf(&mark)) - 33
}

effect fn recover(error: HostInputError | OutOfMemoryError) -> i32 { return 9 }

pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`)
      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make('host-input/native', encoder.encode(source)) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang }),
        profile: 'release',
        destination: join(destinationRoot, 'native-host-input'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(
        compiled._tag,
        'Compiled',
        JSON.stringify(compiled._tag === 'BackendFailed' ? compiled.error : compiled),
      )
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.path, ['alpha', 'beta', 'gamma'], {
        encoding: 'utf8',
        env: { ...process.env, SILK_EXIT_MARK: 'K' },
      })
      assert.strictEqual(
        run.status,
        42,
        JSON.stringify({ signal: run.signal, stderr: run.stderr, stdout: run.stdout }),
      )
    }),
  120_000,
)
