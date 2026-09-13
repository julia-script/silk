/** Consolidated portable acceptance program for fixed buffering and scoped teardown. */
export const bufferedByteIoAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.buffered_duplex {BufferedDuplex, withBufferedCapacity}
import silk.buffered_input {BufferError, BufferedInput, FillOutcome}
import silk.buffered_output {BufferedOutput}
import silk.byte_duplex {ByteDuplex, ReadTransfer}
import silk.bytes {Bytes}
import silk.effect {Effect}
import silk.memory_byte_duplex {MemoryByteDuplex, MemoryByteDuplexPhase, MemoryReadEvent, MemoryWriteAction, MemoryWriteEvent}
import silk.monotonic_clock {MonotonicClock}
import silk.option {Option}
import silk.result {Result}
import silk.system_clock {Instant, SystemClock}
import silk.u64
import silk.usize
import silk.vector {Vector}
import silk.writer {Writer, WriterError}

struct FixedClock {}
impl MonotonicClock for FixedClock {
  effect fn now(self: &mut Self) -> Instant { return SystemClock.make(0, 0) }
  effect fn getResolution(self: &mut Self) -> u64 { return u64.toU64(1) }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () { drop when return () }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () { drop howLong return () }
}

struct FailingWriter { calls: usize }

effect fn rejectWrite(self: &mut FailingWriter, values: &[u8]) -> () ! WriterError {
  drop values
  self.calls = self.calls + usize.ONE
  fail Writer.failure()
}

effect fn rejectFlush(self: &mut FailingWriter) -> () ! WriterError {
  self.calls = self.calls + usize.ONE
  fail Writer.failure()
}

impl Writer for FailingWriter { writeAll: FailingWriter.rejectWrite flush: FailingWriter.rejectFlush }

fn equal(actual: &[u8], expected: &[u8]) -> bool {
  if actual.length != expected.length { return false }
  let mut index = usize.ZERO
  while index < actual.length {
    if actual[index] != expected[index] { return false }
    index = index + usize.ONE
  }
  return true
}

fn failureCode(failure: BufferError) -> usize {
  return match move failure {
    BufferError.ReadFailed {progress, error} => { drop error return 100 + progress }
    BufferError.WriteFailed {progress, error} => { drop error return 200 + progress }
    BufferError.InputFailed {progress, error} => { drop error return 300 + progress }
    BufferError.UnknownExternalTransfer {progress, error} => { drop error return 400 + progress }
    BufferError.UnexpectedEnd {progress} => 500 + progress
    BufferError.InvalidCapacity {capacity} => 600 + capacity
    BufferError.BufferTooSmall {requested, capacity} => 700 + requested + capacity
    BufferError.InvalidConsumption {requested, available} => 800 + requested + available
    BufferError.InvalidReadCount {count, limit} => 900 + count + limit
    BufferError.LengthOverflow => 1000
    BufferError.Terminal => 1100
  }
}

fn unitResultCode(result: Result<(), BufferError>) -> usize {
  return match move result {
    Result<(), BufferError>.Success {value} => { drop value return usize.ZERO }
    Result<(), BufferError>.Failure {error} => failureCode(move error)
  }
}

fn fillResultCode(result: Result<FillOutcome, BufferError>) -> usize {
  return match move result {
    Result<FillOutcome, BufferError>.Success {value} => { drop value return usize.ZERO }
    Result<FillOutcome, BufferError>.Failure {error} => failureCode(move error)
  }
}

effect<'session> fn exercise<'session, P>(
  session: &'session mut BufferedDuplex<'session, P>,
) -> i32 ! BufferError ? &mut MonotonicClock
where &'session mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  let oversized = run Effect.result(BufferedDuplex.fill(
    &mut session.*,
    9,
    Option.none<Instant>(),
  ))
  if fillResultCode(move oversized) != 717 { return 1 }
  let first = run BufferedDuplex.fill(&mut session.*, 6, Option.none<Instant>())
  let available = match move first {
    FillOutcome.Available {count} => count
    FillOutcome.End {available} => available
  }
  if available != 8 { return 2 }
  let headAndBody = BufferedDuplex.peek(&session.*)
  if !equal(headAndBody, b"head\\r\\nbo") { drop headAndBody return 3 }
  drop headAndBody
  let overrun = run Effect.result(BufferedDuplex.consume(&mut session.*, 9))
  if unitResultCode(move overrun) != 817 { return 4 }
  let preserved = BufferedDuplex.peek(&session.*)
  if !equal(preserved, b"head\\r\\nbo") { drop preserved return 5 }
  drop preserved
  run BufferedDuplex.consume(&mut session.*, 6)
  let compacted = run BufferedDuplex.fill(&mut session.*, 4, Option.none<Instant>())
  drop compacted
  let retainedBody = BufferedDuplex.peek(&session.*)
  if !equal(retainedBody, b"body") { drop retainedBody return 6 }
  drop retainedBody
  let mut body: [u8; 4] = [0, 0, 0, 0]
  run BufferedDuplex.readExact(&mut session.*, &mut body, Option.none<Instant>())
  if !equal(&body, b"body") { return 7 }
  let ended = run BufferedDuplex.fill(&mut session.*, usize.ONE, Option.none<Instant>())
  let firstEnd = match move ended {
    FillOutcome.Available {count} => count + usize.ONE
    FillOutcome.End {available: remainingAtEnd} => remainingAtEnd
  }
  if firstEnd != usize.ZERO { return 8 }
  let sticky = run BufferedDuplex.fill(&mut session.*, usize.ONE, Option.none<Instant>())
  let stickyEnd = match move sticky {
    FillOutcome.Available {count} => count + usize.ONE
    FillOutcome.End {available: remainingAtStickyEnd} => remainingAtStickyEnd
  }
  if stickyEnd != usize.ZERO { return 9 }
  run BufferedDuplex.writeAll(&mut session.*, b"abcd", Option.none<Instant>())
  run BufferedDuplex.finish(&mut session.*, Option.none<Instant>())
  return 42
}

effect<'session> fn abandon<'session, P>(
  session: &'session mut BufferedDuplex<'session, P>,
) -> i32 ! BufferError ? &mut MonotonicClock
where &'session mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  let zeroFill = run BufferedDuplex.fill(&mut session.*, usize.ZERO, Option.none<Instant>())
  drop zeroFill
  let mut empty: [u8; 0] = []
  let zeroRead = run BufferedDuplex.readSome(&mut session.*, &mut empty, Option.none<Instant>())
  drop zeroRead
  let zeroDiscard = run BufferedDuplex.discardAtMost(
    &mut session.*,
    usize.ZERO,
    Option.none<Instant>(),
  )
  if zeroDiscard.count != usize.ZERO || zeroDiscard.endObserved { return 5 }
  let zeroWrite = run BufferedDuplex.writeSome(
    &mut session.*,
    b"",
    Option.none<Instant>(),
  )
  if zeroWrite != usize.ZERO { return 6 }
  let pending = run BufferedDuplex.writeSome(
    &mut session.*,
    b"x",
    Option.none<Instant>(),
  )
  if pending != usize.ONE { return 7 }
  return 42
}

effect fn filledProvider() -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let bytes = run Bytes.copy(&b"head\\r\\nbody")
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(0, 0),
    bytes: move bytes,
  })
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.End {
    readyAt: SystemClock.make(0, 0),
  })
  let mut writes = Vector.make<MemoryWriteEvent>()
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Accept {count: usize.ONE},
  })
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Accept {count: 2},
  })
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Accept {count: usize.ONE},
  })
  return run MemoryByteDuplex.make(move reads, move writes, 8, 16, Option.none<i32>())
}

effect fn emptyProvider() -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  return run MemoryByteDuplex.make(
    Vector.make<MemoryReadEvent>(),
    Vector.make<MemoryWriteEvent>(),
    1,
    4,
    Option.none<i32>(),
  )
}

effect fn failingInputProvider() -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let bytes = run Bytes.copy(&b"ab")
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(0, 0),
    bytes: move bytes,
  })
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Failure {
    readyAt: SystemClock.make(0, 0),
    code: 17,
  })
  return run MemoryByteDuplex.make(
    move reads,
    Vector.make<MemoryWriteEvent>(),
    1,
    4,
    Option.none<i32>(),
  )
}

effect fn failingOutputProvider() -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let mut writes = Vector.make<MemoryWriteEvent>()
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Accept {count: usize.ONE},
  })
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Failure {code: 18},
  })
  return run MemoryByteDuplex.make(
    Vector.make<MemoryReadEvent>(),
    move writes,
    2,
    4,
    Option.none<i32>(),
  )
}

effect fn program() -> i32 ! BufferError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut clock = FixedClock {}
  let mut provider = run filledProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let result = run withBufferedCapacity<i32, BufferError>(
    &mut provider,
    8,
    3,
    exercise,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if result != 42 { return 10 + result }
  if MemoryByteDuplex.closeAttempts(&provider) != usize.ONE { return 20 }
  if MemoryByteDuplex.phase(&provider) != MemoryByteDuplexPhase.Closed { return 21 }
  let outbound = MemoryByteDuplex.outbound(&provider)
  if !equal(outbound, b"abcd") { drop outbound return 22 }
  drop outbound
  let completedAudit = MemoryByteDuplex.audit(&provider)
  if completedAudit.length != 8 { drop completedAudit return 23 }
  drop completedAudit

  let mut abandoned = run emptyProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let abandonedResult = run withBufferedCapacity<i32, BufferError>(
    &mut abandoned,
    4,
    4,
    abandon,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if abandonedResult != 42 { return 30 + abandonedResult }
  if MemoryByteDuplex.closeAttempts(&abandoned) != usize.ONE { return 40 }
  let dropped = MemoryByteDuplex.outbound(&abandoned)
  if dropped.length != usize.ZERO { drop dropped return 41 }
  drop dropped
  let audit = MemoryByteDuplex.audit(&abandoned)
  if audit.length != usize.ONE { drop audit return 42 }
  drop audit

  let mut failingInput = run failingInputProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut input = run BufferedInput.make(4)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut exact: [u8; 4] = [0, 0, 0, 0]
  let inputOperation = BufferedInput.readExact(
    &mut input,
    &mut exact,
    Option.none<Instant>(),
  )
    |> Effect.provideMut<ByteDuplex>(&mut failingInput)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
  let inputResult = run Effect.result(move inputOperation)
  if unitResultCode(move inputResult) != 102 { return 50 }
  if !BufferedInput.isTerminal(&input) { return 51 }
  if exact[usize.ZERO] != 97 || exact[usize.ONE] != 98 { return 52 }

  let mut failingOutput = run failingOutputProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut output = run BufferedOutput.make(2)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let accepted = BufferedOutput.writeAll(&mut output, b"ab", Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut failingOutput)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
  run accepted
  let flush = BufferedOutput.flush(&mut output, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut failingOutput)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
  let outputResult = run Effect.result(move flush)
  if unitResultCode(move outputResult) != 201 { return 53 }
  if !BufferedOutput.isTerminal(&output) { return 54 }
  let emitted = MemoryByteDuplex.outbound(&failingOutput)
  if !equal(emitted, b"a") { drop emitted return 55 }
  drop emitted

  let mut writer = FailingWriter {calls: usize.ZERO}
  let mut writerOutput = run BufferedOutput.make(2)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let writerOperation = BufferedOutput.writeAllWriter(&mut writerOutput, b"abcd")
    |> Effect.provideMut<Writer>(&mut writer)
  let writerResult = run Effect.result(move writerOperation)
  if unitResultCode(move writerResult) != 402 { return 56 }
  if !BufferedOutput.isTerminal(&writerOutput) { return 57 }
  if writer.calls != usize.ONE { return 58 }
  return 0
}

effect fn failed<E>(error: E) -> i32 { drop error return 99 }

pub fn main() -> i32 { return run Effect.catchAll(program(), failed) }
`
