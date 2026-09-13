import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'
import { bufferedByteIoAcceptanceSource } from './support/bufferedByteIoAcceptance.js'
import * as Driver from './support/TestDriver.js'

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-buffered-byte-io-test-'))

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

const actorNames = [
  'buffered_input',
  'buffered_output',
  'buffered_duplex',
  'buffered_transfer',
] as const

const actorBytes = (name: (typeof actorNames)[number]): Uint8Array =>
  new Uint8Array(readFileSync(new URL(`../stdlib/silk/${name}.silk`, import.meta.url)))

const internalActorBytes = (name: (typeof actorNames)[number]): Uint8Array => {
  const actor = new TextDecoder()
    .decode(actorBytes(name))
    .replaceAll('silk.buffered_', 'ticket.buffered_')
  const vectorOverflowProbe = `
pub effect fn ticketVectorOverflowProbe() -> bool {
  let result = run Effect.result(checkedAggregateLength(usize.MAX, usize.ONE))
  return match move result {
    Result<usize, BufferError>.Success {value} => { drop value return false }
    Result<usize, BufferError>.Failure {error: failure} => match move failure {
      BufferError.LengthOverflow => true
      BufferError.InvalidCapacity {capacity} => { drop capacity return false }
      BufferError.BufferTooSmall {requested, capacity} => { drop requested drop capacity return false }
      BufferError.InvalidConsumption {requested, available} => { drop requested drop available return false }
      BufferError.InvalidReadCount {count, limit} => { drop count drop limit return false }
      BufferError.UnexpectedEnd {progress} => { drop progress return false }
      BufferError.ReadFailed {progress, error: cause} => { drop progress drop cause return false }
      BufferError.InputFailed {progress, error: inputCause} => { drop progress drop inputCause return false }
      BufferError.WriteFailed {accepted, drained, error: writeCause} => {
        drop accepted drop drained drop writeCause return false
      }
      BufferError.UnknownExternalTransfer {progress, error: writerCause} => { drop progress drop writerCause return false }
      BufferError.Terminal => false
    }
  }
}
`
  const duplexConstructionProbe = `
pub effect fn ticketMakeBuffered<'transport, P>(
  transport: &'transport mut P,
  inputCapacity: usize,
  outputCapacity: usize,
) -> BufferedDuplex<'transport, P>
! BufferError | OutOfMemoryError
? &mut Allocator {
  let input = run BufferedInput.make(inputCapacity)
  let output = run BufferedOutput.make(outputCapacity)
  return BufferedDuplex {
    inputState: move input,
    outputState: move output,
    transport: move transport,
    terminal: false,
  }
}
`
  if (name === 'buffered_output') return new TextEncoder().encode(actor + vectorOverflowProbe)
  if (name === 'buffered_duplex') return new TextEncoder().encode(actor + duplexConstructionProbe)
  return new TextEncoder().encode(actor)
}

const transferRuntimeProbe = `
fn transferFailureMatches(
  result: Result<TransferOutcome, BufferError>,
  accepted: usize,
  drained: usize,
) -> bool {
  return match move result {
    Result<TransferOutcome, BufferError>.Success {value} => { drop value return false }
    Result<TransferOutcome, BufferError>.Failure {error} => match move error {
      BufferError.WriteFailed {accepted: actualAccepted, drained: actualDrained, error: cause} => {
        drop cause
        return actualAccepted == accepted && actualDrained == drained
      }
      BufferError.ReadFailed {progress, error: readCause} => { drop progress drop readCause return false }
      BufferError.InputFailed {progress, error: inputCause} => { drop progress drop inputCause return false }
      BufferError.UnknownExternalTransfer {progress, error: writerCause} => { drop progress drop writerCause return false }
      BufferError.UnexpectedEnd {progress} => { drop progress return false }
      BufferError.InvalidCapacity {capacity} => { drop capacity return false }
      BufferError.BufferTooSmall {requested, capacity} => { drop requested drop capacity return false }
      BufferError.InvalidConsumption {requested, available} => { drop requested drop available return false }
      BufferError.InvalidReadCount {count, limit} => { drop count drop limit return false }
      BufferError.LengthOverflow => false
      BufferError.Terminal => false
    }
  }
}

fn retainsTransferSuffix<'transport, P>(source: &BufferedDuplex<'transport, P>) -> bool {
  let retained = BufferedDuplex.peek(source)
  return equal(retained, b"cdef")
}

effect fn ticketTransferProbe() -> bool
! BufferError | OutOfMemoryError
? &mut Allocator | &mut MonotonicClock {
  let bytes = run Bytes.copy(&b"abcdef")
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(0, 0),
    bytes: move bytes,
  })
  let mut sourceProvider = run MemoryByteDuplex.make(
    move reads,
    Vector.make<MemoryWriteEvent>(),
    1,
    8,
    Option.none<i32>(),
  )

  let mut writes = Vector.make<MemoryWriteEvent>()
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Accept {count: usize.ONE},
  })
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Failure {code: 23},
  })
  let mut destinationProvider = run MemoryByteDuplex.make(
    Vector.make<MemoryReadEvent>(),
    move writes,
    4,
    8,
    Option.none<i32>(),
  )

  let mut source = run ticketMakeBuffered(&mut sourceProvider, 8, 2)
  let mut destination = run ticketMakeBuffered(&mut destinationProvider, 2, 2)
  let zero = run BufferedTransfer.transferAtMost(
    &mut source,
    &mut destination,
    usize.ZERO,
    Option.none<Instant>(),
  )
  if zero.count != usize.ZERO || zero.endObserved { return false }
  if BufferedDuplex.unread(&source) != usize.ZERO { return false }
  if BufferedDuplex.pending(&destination) != usize.ZERO { return false }

  let transfer = BufferedTransfer.transferAtMost(
    &mut source,
    &mut destination,
    6,
    Option.none<Instant>(),
  )
  let attempted = run Effect.result(move transfer)
  if !transferFailureMatches(move attempted, 2, usize.ONE) { return false }
  if !retainsTransferSuffix(&source) { return false }
  drop source
  drop destination
  if MemoryByteDuplex.audit(&sourceProvider).length != usize.ONE { return false }
  if MemoryByteDuplex.audit(&destinationProvider).length != 2 { return false }
  let emitted = MemoryByteDuplex.outbound(&destinationProvider)
  if !equal(emitted, b"a") { drop emitted return false }
  drop emitted
  return true
}
`

const internalAcceptanceBytes = new TextEncoder().encode(
  bufferedByteIoAcceptanceSource
    .replaceAll('silk.buffered_', 'ticket.buffered_')
    .replace(
      'import ticket.buffered_output {BufferedOutput}',
      'import ticket.buffered_output {BufferedOutput, ticketVectorOverflowProbe}',
    )
    .replace(
      'import ticket.buffered_duplex {BufferedDuplex, withBufferedCapacity}',
      'import ticket.buffered_duplex {BufferedDuplex, ticketMakeBuffered, withBufferedCapacity}\nimport ticket.buffered_transfer {BufferedTransfer, TransferOutcome}',
    )
    .replace(
      'effect fn program() -> i32 ! BufferError | OutOfMemoryError {',
      `${transferRuntimeProbe}\neffect fn program() -> i32 ! BufferError | OutOfMemoryError {\n  if !(run ticketVectorOverflowProbe()) { return 59 }`,
    )
    .replace(
      'let mut clock = FixedClock {}',
      'let mut clock = FixedClock {}\n  let transferEvidence = ticketTransferProbe()\n    |> Effect.provideMut<MonotonicClock>(&mut clock)\n    |> Effect.provideMut<Allocator>(&mut allocator)\n  if !(run move transferEvidence) { return 60 }',
    ),
)

const peekConflictSource = `import silk.buffered_duplex {BufferedDuplex}
import silk.buffered_input {BufferError}
effect fn conflict<'session, P>(
  session: &'session mut BufferedDuplex<'session, P>,
) -> () ! BufferError {
  let peeked = BufferedDuplex.peek(&session.*)
  run BufferedDuplex.consume(&mut session.*, 1)
  drop peeked
  return ()
}
pub fn main() -> i32 { return 0 }
`

const scopedEscapeSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.buffered_duplex {BufferedDuplex, withBuffered}
import silk.buffered_input {BufferError}
import silk.byte_duplex {ByteDuplex}
import silk.monotonic_clock {MonotonicClock}
effect<'call> fn leakSession<'call, P>(
  session: &'call mut BufferedDuplex<'call, P>,
) -> &'call mut BufferedDuplex<'call, P> {
  return move session
}
effect fn escape<'env, P>(transport: &'env mut P) -> &'env mut BufferedDuplex<'env, P>
! BufferError | OutOfMemoryError
? &mut Allocator
where &'env mut P provides &ByteDuplex from &mut ByteDuplex,
  &'env mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  return run withBuffered<&'env mut BufferedDuplex<'env, P>, never>(
    move transport,
    leakSession,
  )
}
pub fn main() -> i32 { return 0 }
`

const scopedPeekEscapeSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.buffered_duplex {BufferedDuplex, withBuffered}
import silk.buffered_input {BufferError}
import silk.byte_duplex {ByteDuplex}
import silk.monotonic_clock {MonotonicClock}
effect<'call> fn leakPeek<'call, P>(
  session: &'call mut BufferedDuplex<'call, P>,
) -> &'call [u8] {
  return BufferedDuplex.peek(&session.*)
}
effect fn escape<'env, P>(transport: &'env mut P) -> &'env [u8]
! BufferError | OutOfMemoryError
? &mut Allocator
where &'env mut P provides &ByteDuplex from &mut ByteDuplex,
  &'env mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  return run withBuffered<&'env [u8], never>(move transport, leakPeek)
}
pub fn main() -> i32 { return 0 }
`

const ambientAliasSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.buffered_duplex {BufferedDuplex, withBuffered}
import silk.buffered_input {BufferError}
import silk.byte_duplex {ByteDuplex, ByteIoError}
import silk.monotonic_clock {MonotonicClock}
import silk.option {Option}
import silk.system_clock {Instant}
effect<'call> fn useAmbient<'call, P>(
  session: &'call mut BufferedDuplex<'call, P>,
) -> () ! ByteIoError ? &mut ByteDuplex | &mut MonotonicClock {
  drop session
  return run ByteDuplex.flush(Option.none<Instant>())
}
effect fn alias<'env, P>(transport: &'env mut P) -> ()
! BufferError | ByteIoError | OutOfMemoryError
? &mut Allocator | &mut MonotonicClock | &mut ByteDuplex
where &'env mut P provides &ByteDuplex from &mut ByteDuplex,
  &'env mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  return run withBuffered<(), ByteIoError>(move transport, useAmbient)
}
pub fn main() -> i32 { return 0 }
`

const publicSurfaceSource = `import silk.buffered_duplex {BufferedDuplex, DEFAULT_CAPACITY, withBuffered, withBufferedCapacity}
import silk.buffered_input {BufferError, BufferedInput, DiscardOutcome, FillOutcome, MAX_CAPACITY}
import silk.buffered_output {BufferedOutput}
import silk.buffered_transfer {BufferedTransfer, TransferOutcome}
import silk.byte_duplex {ByteDuplex}
import silk.bytes {Bytes}
import silk.monotonic_clock {MonotonicClock}
import silk.option {Option}
import silk.system_clock {Instant}
import silk.usize
effect fn writeVectors<'session, P>(
  session: &'session mut BufferedDuplex<'session, P>,
  values: &[Bytes],
) -> () ! BufferError ? &mut MonotonicClock
where &'session mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  return run BufferedDuplex.writeVecAll(
    &mut session.*,
    move values,
    Option.none<Instant>(),
  )
}
pub fn main() -> i32 {
  return usize.toI32(DEFAULT_CAPACITY + MAX_CAPACITY - 1056768)
}
`

it('keeps every buffered byte actor syntactically complete', () => {
  for (const name of actorNames) {
    const syntax = Parser.parse(Lexer.lex(SourceFile.make(`silk/${name}`, actorBytes(name))))
    assert.deepEqual(syntax.lexicalDiagnostics, [], name)
    assert.deepEqual(syntax.parserDiagnostics, [], name)
  }
})

it.effect('realizes the independently rooted BufferedInput actor', () =>
  Effect.gen(function* () {
    const snapshot = yield* AnalysisFixture.declarations(
      'buffered-byte-io/input',
      actorBytes('buffered_input'),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('resolves the generated public buffered namespaces and aliases', () =>
  Effect.gen(function* () {
    const snapshot = yield* AnalysisFixture.retainingMain(
      'buffered-byte-io/public-surface',
      new TextEncoder().encode(publicSurfaceSource),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('realizes the complete buffered actor graph before manifest integration', () =>
  Effect.gen(function* () {
    const sources = new Map(
      actorNames.map((name) => [
        `ticket/buffered_${name.slice('buffered_'.length)}`,
        internalActorBytes(name),
      ]),
    )
    const module = 'ticket/buffered_transfer'
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make(module, sources.get(module) ?? new Uint8Array()),
      configuration: AnalysisFixture.configuration(module, 'x86_64-unknown-linux-gnu', []),
    }).pipe(Effect.provide(SourceResolver.memory(sources)))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('realizes the consolidated portable buffering contract', () =>
  Effect.gen(function* () {
    const sources = new Map(
      actorNames.map((name) => [
        `ticket/buffered_${name.slice('buffered_'.length)}`,
        internalActorBytes(name),
      ]),
    )
    const module = 'buffered-byte-io/acceptance'
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make(module, internalAcceptanceBytes),
      configuration: AnalysisFixture.configuration(module, 'wasm32-unknown-unknown', []),
    }).pipe(Effect.provide(SourceResolver.memory(sources)))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('keeps the public buffered byte example executable', () =>
  Effect.gen(function* () {
    const reference = readFileSync(
      new URL('../../../apps/docs/content/reference/buffered-byte-io.md', import.meta.url),
      'utf8',
    )
    const example = /```silk\n([\s\S]*?)```/.exec(reference)?.[1]
    assert.isString(example)
    if (example === undefined) return
    const sources = new Map(
      actorNames.map((name) => [
        `ticket/buffered_${name.slice('buffered_'.length)}`,
        internalActorBytes(name),
      ]),
    )
    const module = 'buffered-byte-io/reference-example'
    const snapshot = yield* Analysis.makeRealized({
      root: SourceFile.make(
        module,
        new TextEncoder().encode(example.replaceAll('silk.buffered_', 'ticket.buffered_')),
      ),
      configuration: AnalysisFixture.configuration(module, 'wasm32-unknown-unknown', []),
    }).pipe(Effect.provide(SourceResolver.memory(sources)))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect(
  'rejects conflicting peeks, scoped session escape, and ambient duplex aliases',
  () =>
    Effect.gen(function* () {
      for (const [name, source, expected] of [
        ['peek-conflict', peekConflictSource, ['OWN0010']],
        ['session-escape', scopedEscapeSource, ['SEM0074', 'SEM0122']],
        ['peek-escape', scopedPeekEscapeSource, ['SEM0074', 'SEM0122']],
        ['ambient-alias', ambientAliasSource, ['SEM0074']],
      ] as const) {
        const snapshot = yield* AnalysisFixture.declarations(
          `buffered-byte-io/${name}`,
          new TextEncoder().encode(source),
        )
        assert.deepEqual(
          Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
          [...expected],
          name,
        )
      }
    }),
  30_000,
)

it.effect(
  'executes the consolidated portable buffering contract on Wasm',
  () =>
    Effect.gen(function* () {
      const sources = new Map(
        actorNames.map((name) => [
          `ticket/buffered_${name.slice('buffered_'.length)}`,
          internalActorBytes(name),
        ]),
      )
      const outcome = yield* Driver.compile({
        compilation: {
          root: SourceFile.make('buffered-byte-io/wasm-acceptance', internalAcceptanceBytes),
          target: 'wasm32-unknown-unknown',
        },
        toolchain,
        optimization: 'release',
        destination: join(destinationRoot, 'buffered-byte-io.wasm'),
        cache: false,
        artifactKind: 'WebAssemblyModule',
      }).pipe(Effect.provide(SourceResolver.memory(sources)))
      assert.strictEqual(
        outcome._tag,
        'Compiled',
        outcome._tag === 'Rejected'
          ? outcome.diagnostics
              .map((diagnostic) => `${diagnostic.code}: ${diagnostic.message}`)
              .join('\n')
          : outcome._tag,
      )
      if (outcome._tag !== 'Compiled') return
      const module = new WebAssembly.Module(Uint8Array.from(readFileSync(outcome.path)))
      assert.deepEqual(WebAssembly.Module.imports(module), [])
      const main = new WebAssembly.Instance(module).exports['main']
      assert.isFunction(main)
      if (typeof main === 'function') assert.strictEqual(main(), 0)
    }),
  300_000,
)
