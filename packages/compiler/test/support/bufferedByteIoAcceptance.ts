/** Consolidated portable acceptance program for fixed buffering and scoped teardown. */
export const bufferedByteIoAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.buffered_duplex {BufferedDuplex, withBufferedCapacity, withBufferedPairCapacity}
import silk.buffered_input {BufferError, BufferedInput, FillOutcome}
import silk.buffered_output {BufferedOutput}
import silk.buffered_transfer {BufferedTransfer, TransferOutcome}
import silk.byte_duplex {ByteDuplex, ByteIoError, ReadTransfer}
import silk.bytes {Bytes}
import silk.effect {Effect}
import silk.execution {Execution}
import silk.layout {Layout}
import silk.memory_byte_duplex {MemoryByteDuplex, MemoryByteDuplexPhase, MemoryReadEvent, MemoryWriteAction, MemoryWriteEvent}
import silk.monotonic_clock {MonotonicClock}
import silk.option {Option}
import silk.result {Result}
import silk.shared {Shared}
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

struct CountingAllocator { calls: usize }

struct FailingAllocator { calls: usize failAt: usize }

struct PairCancellationState { sourceCloses: usize destinationCloses: usize }

struct PairParkingDuplex { state: Shared<PairCancellationState> source: bool }

struct PairParkGuard { wake: Intrinsic.Wake }

effect fn allocate(self: &mut CountingAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  self.calls = self.calls + usize.ONE
  let mut system = Allocator.systemAllocatorProvider()
  return run Allocator.allocate(move layout) |> Effect.provideMut<Allocator>(&mut system)
}

impl Allocator for CountingAllocator { allocate: CountingAllocator.allocate }

effect fn allocateUntilFailure(
  self: &mut FailingAllocator,
  layout: Layout,
) -> Allocation ! OutOfMemoryError {
  self.calls = self.calls + usize.ONE
  if self.calls == self.failAt { fail OutOfMemoryError {} }
  let mut system = Allocator.systemAllocatorProvider()
  return run Allocator.allocate(move layout) |> Effect.provideMut<Allocator>(&mut system)
}

impl Allocator for FailingAllocator { allocate: FailingAllocator.allocateUntilFailure }

fn retainPairWake(wake: Intrinsic.Wake) -> PairParkGuard {
  return PairParkGuard {wake: move wake}
}

fn recordPairClose(state: &mut PairCancellationState, source: bool) -> () {
  if source {
    state.sourceCloses = state.sourceCloses + usize.ONE
  } else {
    state.destinationCloses = state.destinationCloses + usize.ONE
  }
  return ()
}

impl PairParkingDuplex {
  unsafe effect fn read(
    self: &mut Self,
    output: &mut [u8],
    deadline: Option<Instant>,
  ) -> ReadTransfer ! ByteIoError ? &mut MonotonicClock {
    drop output
    drop deadline
    return ReadTransfer.End
  }

  unsafe effect fn write(
    self: &mut Self,
    input: &[u8],
    deadline: Option<Instant>,
  ) -> usize ! ByteIoError ? &mut MonotonicClock {
    drop deadline
    return input.length
  }

  unsafe effect fn flush(
    self: &mut Self,
    deadline: Option<Instant>,
  ) -> () ! ByteIoError ? &mut MonotonicClock {
    drop deadline
    return ()
  }

  unsafe effect fn shutdown(
    self: &mut Self,
    deadline: Option<Instant>,
  ) -> () ! ByteIoError ? &mut MonotonicClock {
    drop deadline
    return ()
  }

  unsafe effect fn close(self: &mut Self) -> () ! ByteIoError {
    let source = self.source
    let update = fn(state: &mut PairCancellationState) -> () {
      return recordPairClose(move state, source)
    }
    Shared.withMut(&self.state, move update)
    return ()
  }
}

impl ByteDuplex for PairParkingDuplex {
  readSomeRaw: PairParkingDuplex.read
  writeSomeRaw: PairParkingDuplex.write
  flushRaw: PairParkingDuplex.flush
  shutdownWriteRaw: PairParkingDuplex.shutdown
  closeRaw: PairParkingDuplex.close
}

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
    BufferError.WriteFailed {accepted, drained, error} => {
      drop drained drop error return 200 + accepted
    }
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

fn writeFailureMatches(
  result: Result<(), BufferError>,
  accepted: usize,
  drained: usize,
) -> bool {
  return match move result {
    Result<(), BufferError>.Success {value} => { drop value return false }
    Result<(), BufferError>.Failure {error} => match move error {
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

fn transferFailureMatches(result: Result<TransferOutcome, BufferError>) -> bool {
  return match move result {
    Result<TransferOutcome, BufferError>.Success {value} => { drop value return false }
    Result<TransferOutcome, BufferError>.Failure {error} => match move error {
      BufferError.WriteFailed {accepted, drained, error: cause} => {
        drop cause
        return accepted == 2 && drained == usize.ONE
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

fn constructionFailureMatches(result: Result<i32, BufferError | OutOfMemoryError>) -> bool {
  return match move result {
    Result<i32, BufferError | OutOfMemoryError>.Success {value} => { drop value return false }
    Result<i32, BufferError | OutOfMemoryError>.Failure {error} => match move error {
      OutOfMemoryError {} => false
      BufferError.InvalidCapacity {capacity} => capacity == usize.ZERO
      BufferError.BufferTooSmall {requested, capacity} => { drop requested drop capacity return false }
      BufferError.InvalidConsumption {requested, available} => { drop requested drop available return false }
      BufferError.InvalidReadCount {count, limit} => { drop count drop limit return false }
      BufferError.UnexpectedEnd {progress} => { drop progress return false }
      BufferError.ReadFailed {progress, error: readCause} => { drop progress drop readCause return false }
      BufferError.InputFailed {progress, error: inputCause} => { drop progress drop inputCause return false }
      BufferError.WriteFailed {accepted, drained, error: writeCause} => {
        drop accepted drop drained drop writeCause return false
      }
      BufferError.UnknownExternalTransfer {progress, error: writerCause} => {
        drop progress drop writerCause return false
      }
      BufferError.LengthOverflow => false
      BufferError.Terminal => false
    }
  }
}

fn pairConstructionFailureMatches(result: Result<bool, BufferError | OutOfMemoryError>) -> bool {
  return match move result {
    Result<bool, BufferError | OutOfMemoryError>.Success {value} => { drop value return false }
    Result<bool, BufferError | OutOfMemoryError>.Failure {error} => match move error {
      BufferError.InvalidCapacity {capacity} => capacity == usize.ZERO
      _ => false
    }
  }
}

fn allocationFailureMatches(result: Result<bool, BufferError | OutOfMemoryError>) -> bool {
  return match move result {
    Result<bool, BufferError | OutOfMemoryError>.Success {value} => { drop value return false }
    Result<bool, BufferError | OutOfMemoryError>.Failure {error} => match move error {
      OutOfMemoryError {} => true
      _ => false
    }
  }
}

fn callbackFailureMatches(
  result: Result<bool, BufferError | OutOfMemoryError>,
) -> bool {
  return match move result {
    Result<bool, BufferError | OutOfMemoryError>.Success {value} => {
      drop value
      return false
    }
    Result<bool, BufferError | OutOfMemoryError>.Failure {error} => {
      return match move error {
        BufferError.InvalidConsumption {requested, available} => {
          return requested == 77 && available == 19
        }
        _ => false
      }
    }
  }
}

effect<'session> fn exercise<'session, P>(
  session: &'session mut BufferedDuplex<'session, P>,
) -> i32 ! BufferError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock
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
  let firstOutput = run Bytes.copy(&b"ab")
  let secondOutput = run Bytes.copy(&b"cd")
  let outputs: [Bytes; 2] = [move firstOutput, move secondOutput]
  run BufferedDuplex.writeVecAll(&mut session.*, &outputs, Option.none<Instant>())
  drop outputs
  run BufferedDuplex.finish(&mut session.*, Option.none<Instant>())
  return 42
}

effect<'session> fn readFailureStopsWrites<'session, P>(
  session: &'session mut BufferedDuplex<'session, P>,
) -> bool ! BufferError ? &mut MonotonicClock
where &'session mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  let mut output: [u8; 4] = [0, 0, 0, 0]
  let reading = run Effect.result(BufferedDuplex.readExact(
    &mut session.*,
    &mut output,
    Option.none<Instant>(),
  ))
  if unitResultCode(move reading) != 102 { return false }
  let writing = run Effect.result(BufferedDuplex.writeSome(
    &mut session.*,
    b"x",
    Option.none<Instant>(),
  ))
  return match move writing {
    Result<usize, BufferError>.Success {value} => { drop value return false }
    Result<usize, BufferError>.Failure {error} => failureCode(move error) == 1100
  }
}

effect<'session> fn writeFailureStopsReads<'session, P>(
  session: &'session mut BufferedDuplex<'session, P>,
) -> bool ! BufferError ? &mut MonotonicClock
where &'session mut P provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  let writing = run Effect.result(BufferedDuplex.writeAll(
    &mut session.*,
    b"abcd",
    Option.none<Instant>(),
  ))
  if !writeFailureMatches(move writing, 2, usize.ONE) { return false }
  let reading = run Effect.result(BufferedDuplex.fill(
    &mut session.*,
    usize.ONE,
    Option.none<Instant>(),
  ))
  return fillResultCode(move reading) == 1100
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

effect<'source & 'destination> fn abandonPair<'source, 'destination, SP, DP>(
  source: &'source mut BufferedDuplex<'source, SP>,
  destination: &'destination mut BufferedDuplex<'destination, DP>,
) -> bool ! BufferError ? &mut MonotonicClock
where &'source mut SP provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock,
  &'destination mut DP provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  let sourceFill = run BufferedDuplex.fill(
    &mut source.*,
    usize.ZERO,
    Option.none<Instant>(),
  )
  drop sourceFill
  let destinationFill = run BufferedDuplex.fill(
    &mut destination.*,
    usize.ZERO,
    Option.none<Instant>(),
  )
  drop destinationFill
  return true
}

effect<'source & 'destination> fn failPair<'source, 'destination, SP, DP>(
  source: &'source mut BufferedDuplex<'source, SP>,
  destination: &'destination mut BufferedDuplex<'destination, DP>,
) -> bool ! BufferError ? &mut MonotonicClock
where &'source mut SP provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock,
  &'destination mut DP provides &ByteDuplex from &mut ByteDuplex | &mut MonotonicClock {
  let sourceFill = run BufferedDuplex.fill(
    &mut source.*,
    usize.ZERO,
    Option.none<Instant>(),
  )
  drop sourceFill
  drop destination
  fail BufferError.InvalidConsumption {requested: 77, available: 19}
}

effect<'source & 'destination> fn parkPair<'source, 'destination>(
  source: &'source mut BufferedDuplex<'source, PairParkingDuplex>,
  destination: &'destination mut BufferedDuplex<'destination, PairParkingDuplex>,
) -> bool ! BufferError ? &mut MonotonicClock {
  drop source
  drop destination
  let now = run MonotonicClock.now()
  drop now
  run Execution.park(retainPairWake)
  return true
}

effect fn useMemoryPair<'env>(
  source: &'env mut MemoryByteDuplex,
  sourceInputCapacity: usize,
  sourceOutputCapacity: usize,
  destination: &'env mut MemoryByteDuplex,
  destinationInputCapacity: usize,
  destinationOutputCapacity: usize,
  callback: for<'sourceCall, 'destinationCall> once fn<'env>(
    &'sourceCall mut BufferedDuplex<'sourceCall, MemoryByteDuplex>,
    &'destinationCall mut BufferedDuplex<'destinationCall, MemoryByteDuplex>,
  ) -> once Effect<'sourceCall & 'destinationCall; bool ! BufferError ? &mut MonotonicClock>,
) -> bool
! BufferError | OutOfMemoryError
? &mut Allocator | &mut MonotonicClock {
  return run withBufferedPairCapacity<bool, BufferError>(
    move source,
    sourceInputCapacity,
    sourceOutputCapacity,
    move destination,
    destinationInputCapacity,
    destinationOutputCapacity,
    move callback,
  )
}

effect fn invalidPairCase(
  source: &mut MemoryByteDuplex,
  sourceInputCapacity: usize,
  sourceOutputCapacity: usize,
  destination: &mut MemoryByteDuplex,
  destinationInputCapacity: usize,
  destinationOutputCapacity: usize,
) -> bool ? &mut Allocator | &mut MonotonicClock {
  let attempted = run Effect.result(useMemoryPair(
    move source,
    sourceInputCapacity,
    sourceOutputCapacity,
    move destination,
    destinationInputCapacity,
    destinationOutputCapacity,
    abandonPair,
  ))
  return pairConstructionFailureMatches(move attempted)
}

effect<'source & 'destination> fn transferSessions<'source, 'destination>(
  source: &'source mut BufferedDuplex<'source, MemoryByteDuplex>,
  destination: &'destination mut BufferedDuplex<'destination, MemoryByteDuplex>,
) -> bool ! BufferError ? &mut MonotonicClock
where &'source mut MemoryByteDuplex provides &ByteDuplex
  from &mut ByteDuplex | &mut MonotonicClock,
  &'destination mut MemoryByteDuplex provides &ByteDuplex
  from &mut ByteDuplex | &mut MonotonicClock {
  let zero = run BufferedTransfer.transferAtMost(
    &mut source.*,
    &mut destination.*,
    usize.ZERO,
    Option.none<Instant>(),
  )
  if zero.count != usize.ZERO || zero.endObserved { return false }
  if BufferedDuplex.unread(&source.*) != usize.ZERO { return false }
  if BufferedDuplex.pending(&destination.*) != usize.ZERO { return false }

  let attempted = run Effect.result(BufferedTransfer.transferAtMost(
    &mut source.*,
    &mut destination.*,
    6,
    Option.none<Instant>(),
  ))
  if !transferFailureMatches(move attempted) { return false }
  if BufferedDuplex.unread(&source.*) != 4 { return false }
  let retained = BufferedDuplex.peek(&source.*)
  if !equal(retained, b"cdef") { drop retained return false }
  drop retained
  return BufferedDuplex.pending(&destination.*) == usize.ONE
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

effect fn emptyProviderWithCloseFailure(closeFailure: Option<i32>) -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  return run MemoryByteDuplex.make(
    Vector.make<MemoryReadEvent>(),
    Vector.make<MemoryWriteEvent>(),
    usize.ONE,
    4,
    move closeFailure,
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

effect fn failingSessionInputProvider() -> MemoryByteDuplex
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
    code: 31,
  })
  return run MemoryByteDuplex.make(
    move reads,
    Vector.make<MemoryWriteEvent>(),
    usize.ONE,
    4,
    Option.none<i32>(),
  )
}

effect fn failingSessionOutputProvider() -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let bytes = run Bytes.copy(&b"z")
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(0, 0),
    bytes: move bytes,
  })
  let mut writes = Vector.make<MemoryWriteEvent>()
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Accept {count: usize.ONE},
  })
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Failure {code: 32},
  })
  return run MemoryByteDuplex.make(move reads, move writes, 4, 6, Option.none<i32>())
}

effect fn transferSourceProvider() -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let bytes = run Bytes.copy(&b"abcdef")
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(0, 0),
    bytes: move bytes,
  })
  return run MemoryByteDuplex.make(
    move reads,
    Vector.make<MemoryWriteEvent>(),
    6,
    4,
    Option.none<i32>(),
  )
}

effect fn transferDestinationProvider() -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let mut writes = Vector.make<MemoryWriteEvent>()
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Accept {count: usize.ONE},
  })
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 0),
    action: MemoryWriteAction.Failure {code: 23},
  })
  return run MemoryByteDuplex.make(
    Vector.make<MemoryReadEvent>(),
    move writes,
    usize.ONE,
    4,
    Option.none<i32>(),
  )
}

effect fn canceledPair(state: Shared<PairCancellationState>) -> bool
! BufferError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut clock = FixedClock {}
  let mut source = PairParkingDuplex {state: Shared.clone(&state), source: true}
  let mut destination = PairParkingDuplex {state: move state, source: false}
  return run withBufferedPairCapacity<bool, BufferError>(
    &mut source,
    2,
    2,
    &mut destination,
    2,
    2,
    parkPair,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
}

fn pairCancellationReady(state: &()) -> () { return () }
fn pairCancellationComplete(state: &mut i32, value: bool) -> () {
  if value { state.* = 1 } else { state.* = -1 }
  return ()
}
fn pairCancellationParked(state: &mut i32, execution: Intrinsic.Execution<bool>) -> () {
  drop move execution
  state.* = 42
  return ()
}
effect fn pairCancellationFailed<E>(error: E) -> bool { drop error return false }

fn pairCancellationResult(state: &mut PairCancellationState) -> i32 {
  if state.sourceCloses == usize.ONE && state.destinationCloses == usize.ONE { return 42 }
  return -2
}

effect fn cancelSuspendedPair() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let state = run Shared.make<PairCancellationState>(PairCancellationState {
    sourceCloses: usize.ZERO,
    destinationCloses: usize.ZERO,
  }) |> Effect.provideMut<Allocator>(&mut allocator)
  let body = Effect.catchAll(canceledPair(Shared.clone(&state)), pairCancellationFailed)
  let execution = run Execution.make(move body, (), pairCancellationReady)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut result = 0
  run Execution.drive(
    move execution,
    &mut result,
    pairCancellationComplete,
    pairCancellationParked,
  )
  if result != 42 { drop state return result }
  return Shared.withMut(&state, pairCancellationResult)
}

effect fn program() -> i32 ! BufferError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut clock = FixedClock {}
  let mut provider = run filledProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let result = run withBufferedCapacity<i32, BufferError | OutOfMemoryError>(
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
  let buffered = BufferedOutput.writeAll(&mut output, b"ab", Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut failingOutput)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
  run buffered
  let flush = BufferedOutput.flush(&mut output, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut failingOutput)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
  let outputResult = run Effect.result(move flush)
  if !writeFailureMatches(move outputResult, usize.ZERO, usize.ONE) { return 53 }
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

  let mut constructionProvider = run emptyProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut allocationAudit = CountingAllocator {calls: usize.ZERO}
  let construction = withBufferedCapacity<i32, BufferError>(
    &mut constructionProvider,
    4,
    usize.ZERO,
    abandon,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocationAudit)
  let constructionResult = run Effect.result(move construction)
  if !constructionFailureMatches(move constructionResult) { return 61 }
  if allocationAudit.calls != usize.ZERO { return 62 }
  if MemoryByteDuplex.closeAttempts(&constructionProvider) != usize.ZERO { return 63 }

  let mut invalidPairSource = run emptyProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut invalidPairDestination = run emptyProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let invalidSourceInput = run invalidPairCase(
    &mut invalidPairSource, usize.ZERO, 4, &mut invalidPairDestination, 4, 4,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocationAudit)
  let invalidSourceOutput = run invalidPairCase(
    &mut invalidPairSource, 4, usize.ZERO, &mut invalidPairDestination, 4, 4,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocationAudit)
  let invalidDestinationInput = run invalidPairCase(
    &mut invalidPairSource, 4, 4, &mut invalidPairDestination, usize.ZERO, 4,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocationAudit)
  let invalidDestinationOutput = run invalidPairCase(
    &mut invalidPairSource, 4, 4, &mut invalidPairDestination, 4, usize.ZERO,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocationAudit)
  if !invalidSourceInput { return 73 }
  if !invalidSourceOutput { return 74 }
  if !invalidDestinationInput { return 75 }
  if !invalidDestinationOutput { return 76 }
  if allocationAudit.calls != usize.ZERO { return 703 }
  if MemoryByteDuplex.closeAttempts(&invalidPairSource) != usize.ZERO { return 704 }
  if MemoryByteDuplex.closeAttempts(&invalidPairDestination) != usize.ZERO { return 705 }

  let mut failingAllocator = FailingAllocator {calls: usize.ZERO, failAt: 2}
  let allocationFailure = useMemoryPair(
    &mut invalidPairSource, 4, 4, &mut invalidPairDestination, 4, 4, abandonPair,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut failingAllocator)
  let allocationFailureResult = run Effect.result(move allocationFailure)
  if !allocationFailureMatches(move allocationFailureResult) { return 77 }
  if failingAllocator.calls != 2 { return 78 }
  if MemoryByteDuplex.closeAttempts(&invalidPairSource) != usize.ONE { return 79 }
  if MemoryByteDuplex.closeAttempts(&invalidPairDestination) != usize.ONE { return 80 }

  let mut callbackSource = run emptyProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut callbackDestination = run emptyProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let callbackFailure = useMemoryPair(
    &mut callbackSource, 4, 4, &mut callbackDestination, 4, 4, failPair,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let callbackFailureResult = run Effect.result(move callbackFailure)
  if !callbackFailureMatches(move callbackFailureResult) { return 81 }
  if MemoryByteDuplex.closeAttempts(&callbackSource) != usize.ONE { return 82 }
  if MemoryByteDuplex.closeAttempts(&callbackDestination) != usize.ONE { return 83 }

  let mut sourceCloseFailure = run emptyProviderWithCloseFailure(Option.some<i32>(91))
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut sourceCloseDestination = run emptyProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let sourceCloseResult = run useMemoryPair(
    &mut sourceCloseFailure, 4, 4, &mut sourceCloseDestination, 4, 4, abandonPair,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if !sourceCloseResult { return 84 }
  if MemoryByteDuplex.closeAttempts(&sourceCloseFailure) != usize.ONE { return 85 }
  if MemoryByteDuplex.closeAttempts(&sourceCloseDestination) != usize.ONE { return 86 }

  let mut destinationCloseSource = run emptyProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut destinationCloseFailure = run emptyProviderWithCloseFailure(Option.some<i32>(92))
    |> Effect.provideMut<Allocator>(&mut allocator)
  let destinationCloseResult = run useMemoryPair(
    &mut destinationCloseSource, 4, 4, &mut destinationCloseFailure, 4, 4, abandonPair,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if !destinationCloseResult { return 87 }
  if MemoryByteDuplex.closeAttempts(&destinationCloseSource) != usize.ONE { return 88 }
  if MemoryByteDuplex.closeAttempts(&destinationCloseFailure) != usize.ONE { return 89 }

  let canceled = run cancelSuspendedPair()
  if canceled != 42 { return 90 }

  let mut failedReadSession = run failingSessionInputProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let readSession = run withBufferedCapacity<bool, BufferError>(
    &mut failedReadSession,
    4,
    2,
    readFailureStopsWrites,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if !readSession { return 64 }
  let readSessionOutbound = MemoryByteDuplex.outbound(&failedReadSession)
  if readSessionOutbound.length != usize.ZERO { drop readSessionOutbound return 65 }
  drop readSessionOutbound

  let mut failedWriteSession = run failingSessionOutputProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let writeSession = run withBufferedCapacity<bool, BufferError>(
    &mut failedWriteSession,
    4,
    2,
    writeFailureStopsReads,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if !writeSession { return 66 }

  let mut transferSource = run transferSourceProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut transferDestination = run transferDestinationProvider()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let transferPassed = run useMemoryPair(
    &mut transferSource,
    8,
    2,
    &mut transferDestination,
    2,
    2,
    transferSessions,
  )
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if !transferPassed { return 67 }
  if MemoryByteDuplex.closeAttempts(&transferSource) != usize.ONE { return 68 }
  if MemoryByteDuplex.closeAttempts(&transferDestination) != usize.ONE { return 69 }
  let transferSourceAudit = MemoryByteDuplex.audit(&transferSource)
  if transferSourceAudit.length != 2 { drop transferSourceAudit return 70 }
  drop transferSourceAudit
  let transferDestinationAudit = MemoryByteDuplex.audit(&transferDestination)
  if transferDestinationAudit.length != 3 { drop transferDestinationAudit return 71 }
  drop transferDestinationAudit
  let transferOutbound = MemoryByteDuplex.outbound(&transferDestination)
  if !equal(transferOutbound, b"a") { drop transferOutbound return 72 }
  drop transferOutbound
  return 0
}

effect fn failed<E>(error: E) -> i32 { drop error return 99 }

pub fn main() -> i32 { return run Effect.catchAll(program(), failed) }
`
