import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ChildProcess from '../src/ChildProcess.js'
import * as IntrinsicAvailability from '../src/IntrinsicAvailability.js'
import * as Mir from '../src/Mir.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target = 'aarch64-apple-darwin') =>
  Analysis.ofSourceRealized('child-process/main', encoder.encode(source), target)

const ascii = (text: string): ReadonlyArray<number> =>
  Array.from(text, (character) => character.charCodeAt(0))

/** Binds one Silk byte array, so a test can name bytes that are not well-formed text at all. */
const binding = (name: string, values: ReadonlyArray<number>): string =>
  `  let ${name} = [${values.map((value) => `u8.toU8(${value})`).join(', ')}]`

/** The shared byte vocabulary every program below borrows from. */
const vocabulary = [
  binding('toolPath', ascii('/bin/tool')),
  binding('absentPath', ascii('/bin/absent')),
  binding('workPath', ascii('/work')),
  binding('firstArgument', ascii('a')),
  binding('secondArgument', ascii('bc')),
  binding('variableName', ascii('K')),
  binding('variableValue', ascii('v')),
  // Neither of these is well-formed text; both must reach the child unchanged.
  binding('rawValue', [0xff, 0x41]),
  binding('rawArgument', [0xfe]),
].join('\n')

/**
 * A pure in-source provider. It answers from a scripted exit code and a scripted first output byte
 * without a host boundary, so the portable contract is observable on its own.
 */
const scriptedProvider = `struct Scripted {
  code: i32
  signal: bool
  first: u8
}

effect fn scriptedExecute(
  self: &mut Scripted,
  request: &ProcessRequest
) -> ProcessOutcome ! ProcessFailure | OutOfMemory ? &mut Allocator {
  let mut output = bytesMake()
  let one = [self.first]
  let appended = run bytesAppend(&mut output, &one)
  if self.signal { return signaled(self.code, move output, bytesMake()) }
  return exited(self.code, move output, bytesMake())
}

impl ChildProcess for Scripted { execute: Scripted.scriptedExecute }

struct Broken {}

effect fn brokenExecute(
  self: &mut Broken,
  request: &ProcessRequest
) -> ProcessOutcome ! ProcessFailure | OutOfMemory ? &mut Allocator {
  fail failure(spawnOperation(), notFound())
}

impl ChildProcess for Broken { execute: Broken.brokenExecute }
`

const imports = `import silk.bytes { append as bytesAppend, make as bytesMake }
import silk.child_process {
  ChildProcess,
  ProcessFailure,
  ProcessOutcome,
  ProcessRequest,
  addArgument,
  argumentCount,
  environmentCount,
  errorBytes,
  exitCode,
  exited,
  failure,
  hasWorkingDirectory,
  isSignaled,
  notFound,
  outputBytes,
  providerCode,
  request,
  requestWithin,
  setVariable,
  signaled,
  spawnOperation,
  submit,
  terminatingSignal
}
import silk.filesystem { FileError, fromBytes as pathFromBytes }
import silk.option { None, Option, Some }
import silk.result { Failure, Result, Success }

impl Report for OutOfMemory {}
`

/** Reports the program's own return value, or a distinct band for each typed failure it recovers. */
const recovery = `pub fn main() -> i32 {
  let attempted = run Intrinsic.effectResult(program())
  return match move attempted {
    Result<i32, ProcessFailure | OutOfMemory | FileError> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<ProcessFailure | OutOfMemory | FileError> { error } => match move error {
        ProcessFailure processFailure => match move providerCode(&processFailure) {
          Some<i32> { value } =>
            70 + processFailure.reason.code + 10 * processFailure.operation.code + value
          None {} => 70 + processFailure.reason.code + 10 * processFailure.operation.code
        }
        OutOfMemory exhausted => 98
        FileError invalid => 99
      }
    }
  }
}`

const scriptedRun = (provider: string, body: string) => `${imports}
${scriptedProvider}
effect fn program() -> i32 ! ProcessFailure | OutOfMemory | FileError {
  let mut allocator = SystemAllocator.make()
  let mut provider = ${provider}
${vocabulary}
${body}
}

${recovery}`

const nativeRun = (body: string) => `${imports}
import silk.os_child_process { make as osChildMake }

effect fn program() -> i32 ! ProcessFailure | OutOfMemory | FileError {
  let mut allocator = SystemAllocator.make()
  let mut provider = osChildMake()
${vocabulary}
${body}
}

${recovery}`

it.effect('loads the ordinary canonical native provider without compiler-known privilege', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.os_child_process { OsChildProcess, make }
pub fn construct() -> OsChildProcess {
  return make()
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
  }),
)

it.effect('carries an exit code, captured output, and captured errors as one owned outcome', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedRun(
        'Scripted { code: 0, signal: false, first: u8.toU8(115) }',
        `  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let built = run request(&path) |> Effect.provideMut(&mut allocator)
  let outcome = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&built), &mut provider),
    &mut allocator
  )
  if isSignaled(&outcome) { return 1 }
  if outputBytes(&outcome).length != usize.ONE { return 2 }
  if outputBytes(&outcome)[usize.ZERO] != u8.toU8(115) { return 3 }
  if errorBytes(&outcome).length != usize.ZERO { return 4 }
  return match move exitCode(&outcome) {
    Some<i32> { value } => 42 + value
    None {} => 5
  }`,
      ),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('reports a nonzero exit code as outcome data rather than as a typed failure', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedRun(
        'Scripted { code: 3, signal: false, first: u8.toU8(0) }',
        // A failing child stays on the success channel, so this run never reaches the recovery band.
        `  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let built = run request(&path) |> Effect.provideMut(&mut allocator)
  let outcome = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&built), &mut provider),
    &mut allocator
  )
  if isSignaled(&outcome) { return 1 }
  return match move exitCode(&outcome) {
    Some<i32> { value } => 39 + value
    None {} => 2
  }`,
      ),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('separates termination by a signal from an ordinary exit code', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedRun(
        'Scripted { code: 9, signal: true, first: u8.toU8(0) }',
        `  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let built = run request(&path) |> Effect.provideMut(&mut allocator)
  let outcome = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&built), &mut provider),
    &mut allocator
  )
  if isSignaled(&outcome) == false { return 1 }
  // A signal never presents itself as an exit code, so no caller reads 9 as a return value.
  let absent = match move exitCode(&outcome) {
    Some<i32> { value } => false
    None {} => true
  }
  if absent == false { return 2 }
  return match move terminatingSignal(&outcome) {
    Some<i32> { value } => 33 + value
    None {} => 3
  }`,
      ),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('routes a provider failure into the typed failure channel', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedRun(
        'Broken {}',
        `  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let built = run request(&path) |> Effect.provideMut(&mut allocator)
  let outcome = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&built), &mut provider),
    &mut allocator
  )
  return 1`,
      ),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    // 70 + notFound(0) + 10 * spawn(0): the start stage reached the typed failure channel.
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 70)
  }),
)

it.effect('retains ordered arguments, an exact environment, and an optional directory', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      scriptedRun(
        'Scripted { code: 0, signal: false, first: u8.toU8(0) }',
        `  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let mut plain = run request(&path) |> Effect.provideMut(&mut allocator)
  if argumentCount(&plain) != usize.ZERO { return 1 }
  if environmentCount(&plain) != usize.ZERO { return 2 }
  if hasWorkingDirectory(&plain) { return 3 }
  let first = run addArgument(&mut plain, &firstArgument) |> Effect.provideMut(&mut allocator)
  let second = run addArgument(&mut plain, &secondArgument) |> Effect.provideMut(&mut allocator)
  let variable = run setVariable(&mut plain, &variableName, &variableValue)
    |> Effect.provideMut(&mut allocator)
  if argumentCount(&plain) != usize.add(0, 2) { return 4 }
  if environmentCount(&plain) != usize.ONE { return 5 }
  let directory = run pathFromBytes(&workPath) |> Effect.provideMut(&mut allocator)
  let located = run requestWithin(&path, &directory) |> Effect.provideMut(&mut allocator)
  if hasWorkingDirectory(&located) == false { return 6 }
  return 42`,
      ),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

const nativeEcho =
  nativeRun(`  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let mut built = run request(&path) |> Effect.provideMut(&mut allocator)
  let added = run addArgument(&mut built, &firstArgument) |> Effect.provideMut(&mut allocator)
  let outcome = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&built), &mut provider),
    &mut allocator
  )
  if isSignaled(&outcome) { return 1 }
  if outputBytes(&outcome).length != usize.add(0, 3) { return 2 }
  if outputBytes(&outcome)[usize.ZERO] != u8.toU8(111) { return 3 }
  if outputBytes(&outcome)[usize.add(0, 2)] != u8.toU8(116) { return 4 }
  if errorBytes(&outcome).length != usize.add(0, 2) { return 5 }
  if errorBytes(&outcome)[usize.ZERO] != u8.toU8(101) { return 6 }
  return match move exitCode(&outcome) {
    Some<i32> { value } => 42 + value
    None {} => 7
  }`)

it.effect('runs a program that exits zero and owns everything it captured', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(nativeEcho)
    assert.deepEqual(Analysis.diagnostics(self), [])

    const blocked = Analysis.evaluate(self)
    assert.strictEqual(blocked._tag, 'Blocked')
    if (blocked._tag === 'Blocked') assert.strictEqual(blocked.reason._tag, 'MissingChildProcess')

    const host = ChildProcess.memory([
      { _tag: 'Exited', code: 0, output: ascii('out'), errors: ascii('er') },
    ])
    const outcome = Analysis.evaluate(self, { childProcess: host.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
    assert.deepEqual(host.requests(), [
      { program: ascii('/bin/tool'), arguments: [ascii('a')], environment: [] },
    ])
  }),
)

it.effect('keeps a nonzero exit code on the success channel through the native provider', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      nativeRun(`  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let built = run request(&path) |> Effect.provideMut(&mut allocator)
  let outcome = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&built), &mut provider),
    &mut allocator
  )
  if isSignaled(&outcome) { return 1 }
  if outputBytes(&outcome).length != usize.ONE { return 2 }
  if errorBytes(&outcome).length != usize.add(0, 4) { return 3 }
  return match move exitCode(&outcome) {
    Some<i32> { value } => 39 + value
    None {} => 4
  }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const host = ChildProcess.memory([
      { _tag: 'Exited', code: 3, output: ascii('x'), errors: ascii('boom') },
    ])
    const outcome = Analysis.evaluate(self, { childProcess: host.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('turns a missing executable into a typed start failure that retains its code', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      nativeRun(`  let path = run pathFromBytes(&absentPath) |> Effect.provideMut(&mut allocator)
  let built = run request(&path) |> Effect.provideMut(&mut allocator)
  let outcome = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&built), &mut provider),
    &mut allocator
  )
  return 1`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const host = ChildProcess.memory([
      { _tag: 'ExecuteFailure', reason: 'NotFound', nativeCode: 2 },
    ])
    const outcome = Analysis.evaluate(self, { childProcess: host.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    // 70 + notFound(0) + 10 * spawn(0) + the platform code 2 the failure retained.
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 72)
  }),
)

it.effect('never reads the caller environment and passes exact bytes for what it was given', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      nativeRun(`  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let inherited = run request(&path) |> Effect.provideMut(&mut allocator)
  let first = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&inherited), &mut provider),
    &mut allocator
  )
  let mut named = run request(&path) |> Effect.provideMut(&mut allocator)
  let variable = run setVariable(&mut named, &variableName, &rawValue)
    |> Effect.provideMut(&mut allocator)
  let argument = run addArgument(&mut named, &rawArgument) |> Effect.provideMut(&mut allocator)
  let second = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&named), &mut provider),
    &mut allocator
  )
  return 42`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const host = ChildProcess.memory([
      { _tag: 'Exited', code: 0, output: [], errors: [] },
      { _tag: 'Exited', code: 0, output: [], errors: [] },
    ])
    const outcome = Analysis.evaluate(self, { childProcess: host.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
    // The default request carries no variable at all rather than the caller's own environment, and
    // bytes that are not well-formed text survive both the name and the value unchanged.
    assert.deepEqual(host.requests(), [
      { program: ascii('/bin/tool'), arguments: [], environment: [] },
      {
        program: ascii('/bin/tool'),
        arguments: [[0xfe]],
        environment: [[...ascii('K='), 0xff, 0x41]],
      },
    ])
  }),
)

it.effect('presents the requested working directory to the host', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(
      nativeRun(`  let path = run pathFromBytes(&toolPath) |> Effect.provideMut(&mut allocator)
  let directory = run pathFromBytes(&workPath) |> Effect.provideMut(&mut allocator)
  let built = run requestWithin(&path, &directory) |> Effect.provideMut(&mut allocator)
  let outcome = run Intrinsic.bindRequirement(
    Intrinsic.bindRequirement(submit(&built), &mut provider),
    &mut allocator
  )
  return 42`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const host = ChildProcess.memory([{ _tag: 'Exited', code: 0, output: [], errors: [] }])
    const outcome = Analysis.evaluate(self, { childProcess: host.provider })
    assert.strictEqual(outcome._tag, 'Completed')
    assert.deepEqual(host.requests(), [
      {
        program: ascii('/bin/tool'),
        arguments: [],
        environment: [],
        workingDirectory: ascii('/work'),
      },
    ])
  }),
)

it.effect('lowers the native execution to reachable native-only runtime symbols', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(nativeEcho)
    const operations = Analysis.loweredMir(self)
      .functions.flatMap(Mir.operations)
      .filter((operation) => operation._tag === 'OsCall')
    assert.deepEqual([...new Set(operations.map((operation) => operation.operation.name))].sort(), [
      'osProcessCapture',
      'osProcessExecute',
    ])

    const llvm = yield* Analysis.codegen(self, { mode: 'release' })
    assert.include(llvm.ir, '@silk_os_process_execute_v1')
    assert.include(llvm.ir, '@silk_os_process_capture_v1')
    assert.notInclude(llvm.ir, '@silk_os_file_read_v1')

    const wasm = yield* snapshot(nativeEcho, 'wasm32-unknown-unknown')
    const availability = IntrinsicAvailability.select(wasm.instances.intrinsics, 'Wasm')
    assert.strictEqual(availability._tag, 'Unavailable')
    if (availability._tag === 'Unavailable')
      assert.deepEqual(
        [...new Set(availability.diagnostics.map((diagnostic) => diagnostic.code))],
        ['SEM0093'],
      )
  }),
)

it('records every request and reports a missing scripted response as a typed failure', () => {
  const host = ChildProcess.memory([{ _tag: 'Signaled', signal: 9, output: [1], errors: [] }])
  const request = Object.freeze({
    program: ascii('/bin/tool'),
    arguments: Object.freeze([ascii('one')]),
    environment: Object.freeze([]),
  })
  assert.deepEqual(host.provider.execute(request), {
    _tag: 'Signaled',
    signal: 9,
    output: [1],
    errors: [],
  })
  assert.strictEqual(host.provider.execute(request)._tag, 'ExecuteFailure')
  assert.deepEqual(host.requests(), [request, request])
  assert.deepEqual(ChildProcess.memory().provider.execute(request), {
    _tag: 'ExecuteFailure',
    reason: 'Other',
  })
})
