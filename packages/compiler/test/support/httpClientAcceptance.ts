/** Portable client acceptance for owned reuse, staging, bounded reads, deadlines, tunnels, and content decoding. */
export const httpClientAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.byte_duplex {ByteDuplex, ByteIoError, ReadTransfer}
import silk.bytes {Bytes}
import silk.effect {Effect}
import silk.execution {Execution}
import silk.shared {Shared}
import silk.http {Method, Version, Header}
import silk.http_target {RequestTarget}
import silk.http_content {
  ContentError,
  ContentReason,
  ContentProgressState,
  Mode as ContentMode,
  Limits as ContentLimits,
}
import silk.inflate {Limits as InflateLimits}
import silk.zstd {ZstdLimits}
import silk.http_body {Limits as BodyLimits, Trailers}
import silk.http_head {Limits as HeadLimits}
import silk.http_headers {Headers, Limits as ValueLimits}
import silk.http_origin {Origin}
import silk.http_request {PreparedRequest, HeaderPolicy, BodyMode, RequestError}
import silk.http_request as Request
import silk.http_client {
  Connection,
  ConnectionHandler,
  Exchange,
  Limits,
  ClientError,
  ConnectionPhase,
  RequestOptions,
  ContinuePolicy,
  Tunnel,
}
import silk.http_client as Client
import silk.http_transport {HttpTransport, TransportError, Plain, Loan}
import silk.http_transport as Transport
import silk.memory_byte_duplex {
  MemoryByteDuplex,
  MemoryByteDuplexPhase,
  MemoryReadEvent,
  MemoryWriteEvent,
  MemoryWriteAction,
}
import silk.monotonic_clock {MonotonicClock}
import silk.random {Random}
import silk.option {Option}
import silk.result {Result}
import silk.system_clock {Instant, SystemClock}
import silk.uri {Uri}
import silk.u64
import silk.usize
import silk.vector {Vector}

struct FixedClock {
  mark: Instant
}

struct FixedRandom {}

impl Random for FixedRandom {
  effect fn fillBytes(self: &mut Self, output: &mut [u8]) -> () {
    drop output
    return ()
  }
}

impl MonotonicClock for FixedClock {
  effect fn now(self: &mut Self) -> Instant {
    return SystemClock.make(SystemClock.seconds(&self.mark), SystemClock.nanoseconds(&self.mark))
  }
  effect fn getResolution(self: &mut Self) -> u64 {
    return u64.toU64(1)
  }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () {
    self.mark = move when
    return ()
  }
  effect fn waitFor(self: &mut Self, duration: u64) -> () {
    self.mark = MonotonicClock.deadlineAfter(&self.mark, duration)
    return ()
  }
}

fn valueLimits() -> ValueLimits {
  return ValueLimits {
    maxMethodBytes: 32,
    maxTargetBytes: 128,
    maxNameBytes: 64,
    maxValueBytes: 256,
    maxFields: 12,
    maxFieldBytes: 512,
    maxOwnedBytes: 2048,
  }
}

fn headLimits() -> HeadLimits {
  return HeadLimits {
    maxHeadBytes: 1024,
    maxStartLineBytes: 256,
    maxFieldLineBytes: 256,
    maxOwnedBytes: 2048,
    values: valueLimits(),
  }
}

fn bodyLimits() -> BodyLimits {
  return BodyLimits {
    maxWireBytes: 1024,
    maxPayloadBytes: 512,
    maxChunkBytes: 64,
    maxChunks: 8,
    maxChunkLineBytes: 64,
    maxExtensionBytes: 128,
    maxTrailerBytes: 256,
    maxTrailerFields: 4,
    maxOwnedBytes: 2048,
    trailerValues: valueLimits(),
  }
}

effect fn providerFor(inputBytes: &[u8], writeCount: usize) -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let input = run Bytes.copy(inputBytes)
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append(
    &mut reads,
    MemoryReadEvent.Data {readyAt: SystemClock.make(0, 0), bytes: move input},
  )
  run Vector.append(&mut reads, MemoryReadEvent.End {readyAt: SystemClock.make(0, 0)})
  let mut writes = Vector.make<MemoryWriteEvent>()
  let mut index = usize.ZERO
  while index < writeCount {
    run Vector.append(
      &mut writes,
      MemoryWriteEvent {
        readyAt: SystemClock.make(0, 0),
        action: MemoryWriteAction.Accept {count: 256},
      },
    )
    index = index + usize.ONE
  }
  // Tiny buffers can produce one read per byte; retain writes, flushes, and lifecycle events too.
  let auditCapacity = inputBytes.length + 2 * writeCount + 8
  return run MemoryByteDuplex.make(move reads, move writes, 256, auditCapacity, Option.none<i32>())
}

fn limits() -> Limits {
  return Limits {
    head: headLimits(),
    requestBody: bodyLimits(),
    responseBody: bodyLimits(),
    readCapacity: 128,
    writeCapacity: 128,
    maxRequests: 2,
    maxInformationalResponses: 16,
    maxInformationalWireBytes: 65536,
    maxDiscardWireBytes: 256,
  }
}

struct CancellationAudit {
  drops: usize
  closes: usize
}

struct TestTransport {
  memory: MemoryByteDuplex
  cancellationAudit: Option<Shared<CancellationAudit>>
}

impl Drop for TestTransport {
  fn drop(self: &mut TestTransport) -> () {
    if let Option.Some {value} = &self.cancellationAudit {
      let closes = MemoryByteDuplex.closeAttempts(&self.memory)
      let record = fn(audit: &mut CancellationAudit) -> () {
        audit.drops = audit.drops + usize.ONE
        audit.closes = closes
        return ()
      }
      Shared.withMut(&value, move record)
    }
    return ()
  }
}

impl HttpTransport for TestTransport {
  effect fn readSomeRaw(self: &mut Self, output: &mut [u8], deadline: Option<Instant>) -> ReadTransfer
  ! TransportError | OutOfMemoryError
  ? &mut MonotonicClock | &mut Allocator | &mut Random {
    let result = run Effect.result(ByteDuplex.readSome(&mut output, move deadline))
      |> Effect.provideMut<ByteDuplex>(&mut self.memory)
    return match move result {
      Result.Success {value} => move value
      Result.Failure {error} => {
        fail TransportError.Plain {error: move error}
      }
    }
  }
  effect fn writeSomeRaw(self: &mut Self, input: &[u8], deadline: Option<Instant>) -> usize
  ! TransportError | OutOfMemoryError
  ? &mut MonotonicClock | &mut Allocator | &mut Random {
    let result = run Effect.result(ByteDuplex.writeSome(input, move deadline))
      |> Effect.provideMut<ByteDuplex>(&mut self.memory)
    return match move result {
      Result.Success {value} => value
      Result.Failure {error} => {
        fail TransportError.Plain {error: move error}
      }
    }
  }
  effect fn flush(self: &mut Self, deadline: Option<Instant>) -> ()
  ! TransportError | OutOfMemoryError
  ? &mut MonotonicClock | &mut Allocator | &mut Random {
    let result = run Effect.result(ByteDuplex.flush(move deadline))
      |> Effect.provideMut<ByteDuplex>(&mut self.memory)
    return match move result {
      Result.Success {value} => value
      Result.Failure {error} => {
        fail TransportError.Plain {error: move error}
      }
    }
  }
  effect fn close(self: &mut Self) -> () ! TransportError {
    let result = run Effect.result(ByteDuplex.close())
      |> Effect.provideMut<ByteDuplex>(&mut self.memory)
    return match move result {
      Result.Success {value} => value
      Result.Failure {error} => {
        fail TransportError.Plain {error: move error}
      }
    }
  }
}

struct CallbackFailure {
  code: i32
}

struct Handler {
  request: PreparedRequest
  scenario: i32
}

impl ConnectionHandler<TestTransport, i32, ClientError | OutOfMemoryError | CallbackFailure ? &mut Allocator | &mut MonotonicClock | &mut Random> for Handler {
  effect<'call> fn handle<'call>(handler: Self, connection: &'call mut Connection<TestTransport>) -> i32
  ! ClientError | OutOfMemoryError | CallbackFailure
  ? &mut Allocator | &mut MonotonicClock | &mut Random {
    let Handler {request, scenario} = move handler
    if scenario == 17 {
      let attempted = run Effect.result(
        Client.withExchange(&mut connection.*, &request, RequestOptions.defaults(), failExchange),
      )
      match move attempted {
        Result.Success {value} => {
          drop value
          return 124
        }
        Result.Failure {error} => match move error {
          CallbackFailure cause => {
            if cause.code != 739 || connection.phase() != ConnectionPhase.Closed {
              return 125
            }
            fail move cause
          }
          ClientError cause => {
            fail move cause
          }
          OutOfMemoryError allocation => {
            fail move allocation
          }
        }
      }
    }
    if scenario == 16 {
      return run Client.withExchange(
        &mut connection.*,
        &request,
        RequestOptions.defaults(),
        canceledExchange,
      )
    }
    if scenario == 1 {
      let selectedOptions = RequestOptions {
        deadline: Option.none<Instant>(),
        continuePolicy: ContinuePolicy.Require100 {deadline: SystemClock.make(10, 0)},
      }
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        continueExchange,
      )
      if result != 0 {
        return result
      }
      return 0
    }
    if scenario == 2 {
      let selectedOptions = RequestOptions {
        deadline: Option.none<Instant>(),
        continuePolicy: ContinuePolicy.Require100 {deadline: SystemClock.make(10, 0)},
      }
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        earlyExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 3 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        chunkedExchange,
      )
      if result != 0 {
        return result
      }
      return 0
    }
    if scenario == 4 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        tunnelExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 5 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        abandonExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 6 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        limitedExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 7 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        upgradeExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 8 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        timeoutExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 9 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        partialExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 10 {
      let selectedOptions = RequestOptions {
        deadline: Option.none<Instant>(),
        continuePolicy: ContinuePolicy.Require100 {deadline: SystemClock.make(10, 0)},
      }
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        continueTimeoutExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 11 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        discardExchange,
      )
      if result != 0 {
        return result
      }
      return 0
    }
    if scenario == 12 {
      let selectedOptions = RequestOptions.defaults()
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        wireLimitExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 13 {
      let selectedOptions = RequestOptions {
        deadline: Option.some<Instant>(SystemClock.make(5, 0)),
        continuePolicy: ContinuePolicy.Disabled,
      }
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        expiredCompletedExchange,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    if scenario == 14 || scenario == 15 {
      let result = run Client.withExchange(
        &mut connection.*,
        &request,
        RequestOptions.defaults(),
        contentExchange,
      )
      if result != 0 {
        return result
      }
      if scenario == 15 && connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    let options = RequestOptions {
      deadline: Option.none<Instant>(),
      continuePolicy: ContinuePolicy.Disabled,
    }
    let result = run Client.withExchange(&mut connection.*, &request, move options, exchange)
    if result != 200 {
      return result
    }
    if connection.phase() != ConnectionPhase.Ready {
      return 12
    }
    let options2 = RequestOptions {
      deadline: Option.none<Instant>(),
      continuePolicy: ContinuePolicy.Disabled,
    }
    let second = run Client.withExchange(&mut connection.*, &request, move options2, exchange)
    if second != 201 {
      return second
    }
    return 0
  }
}

effect<'call> fn exchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let status = run Client.receive(&mut exchangeValue.*)
  if status != 200 && status != 201 {
    return 21
  }
  let mut bytes: [u8; 4] = [0, 0, 0, 0]
  let count = run Client.readSome(&mut exchangeValue.*, &mut bytes)
  if count != 4 || bytes[0] != 87 || bytes[3] != 105 {
    return 22
  }
  run Client.finishResponse(&mut exchangeValue.*)
  if status == 200 {
    return 200
  }
  return 201
}

effect fn runOwned(transport: TestTransport, origin: Origin, scenario: i32, handler: Handler) -> i32
! ClientError | OutOfMemoryError | CallbackFailure
? &mut Allocator | &mut MonotonicClock | &mut Random {
  if scenario == 0 {
    let mut owner = run Client.makeOwned(
      move transport,
      origin,
      Version.Http11,
      limits(),
      Option.none<Instant>(),
    )
    let result = run Effect.result(
      ConnectionHandler<TestTransport, i32, ClientError | OutOfMemoryError | CallbackFailure ? &mut Allocator | &mut MonotonicClock | &mut Random>.handle(
        move handler,
        &mut owner,
      ),
    )
    let closed = run Effect.result(Client.close(&mut owner))
    drop closed
    return match move result {
      Result.Success {value} => value
      Result.Failure {error} => {
        fail move error
      }
    }
  }
  let mut selectedLimits = limits()
  if scenario == 6 {
    selectedLimits.maxInformationalResponses = usize.ONE
  }
  if scenario == 12 {
    selectedLimits.maxInformationalWireBytes = 10
  }
  let mut overall = Option.none<Instant>()
  if scenario == 8 {
    overall = Option.some<Instant>(SystemClock.make(0, 0))
  }
  return run Client.withOwned(
    move transport,
    origin,
    Version.Http11,
    selectedLimits,
    move overall,
    move handler,
  )
}

effect fn scenarioProvider(scenario: i32) -> MemoryByteDuplex ! OutOfMemoryError ? &mut Allocator {
  if scenario == 10 {
    let mut reads = Vector.make<MemoryReadEvent>()
    run Vector.append(&mut reads, MemoryReadEvent.End {readyAt: SystemClock.make(20, 0)})
    let mut writes = Vector.make<MemoryWriteEvent>()
    run Vector.append(
      &mut writes,
      MemoryWriteEvent {
        readyAt: SystemClock.make(0, 0),
        action: MemoryWriteAction.Accept {count: 256},
      },
    )
    return run MemoryByteDuplex.make(move reads, move writes, 256, 32, Option.none<i32>())
  }
  if scenario != 9 {
    return run providerFor(inputFor(scenario), 16)
  }
  let reads = Vector.make<MemoryReadEvent>()
  let mut writes = Vector.make<MemoryWriteEvent>()
  run Vector.append(
    &mut writes,
    MemoryWriteEvent {
      readyAt: SystemClock.make(0, 0),
      action: MemoryWriteAction.Accept {count: 256},
    },
  )
  run Vector.append(
    &mut writes,
    MemoryWriteEvent {readyAt: SystemClock.make(0, 0), action: MemoryWriteAction.Accept {count: 2}},
  )
  run Vector.append(
    &mut writes,
    MemoryWriteEvent {readyAt: SystemClock.make(0, 0), action: MemoryWriteAction.Failure {code: 88}},
  )
  return run MemoryByteDuplex.make(move reads, move writes, 256, 32, Option.none<i32>())
}

effect fn runCase(scenario: i32, cancellationAudit: Option<Shared<CancellationAudit>>) -> i32
! ClientError | RequestError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut clock = FixedClock {mark: SystemClock.make(0, 0)}
  let mut random = FixedRandom {}
  let uri = match move Uri.parse("http://example.test/") {
    Result.Failure {error} => {
      drop error
      return 31
    }
    Result.Success {value} => value
  }
  let headerEntries: [Header<'static>; 0] = []
  let headers = match move Headers.make(&headerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 32
    }
    Result.Success {value} => value
  }
  let policy = HeaderPolicy.defaults()
  let origin = match move Origin.fromUri(&uri) {
    Result.Failure {error} => {
      drop error
      return 33
    }
    Result.Success {value} => value
  }
  let mut method = Method.get()
  if scenario == 4 {
    method = Method.connect()
  } else if scenario == 1 || scenario == 2 || scenario == 3 || scenario == 9 || scenario == 10 {
    method = Method.post()
  }
  let mut targetText = "/"
  if scenario == 4 {
    targetText = "example.test:80"
  }
  let target = match move RequestTarget.parse(&method, targetText, 128) {
    Result.Failure {error} => {
      drop error
      return 34
    }
    Result.Success {value} => value
  }
  let mut mode = BodyMode.Empty
  if scenario == 3 {
    mode = BodyMode.Chunked
  } else if scenario == 1 || scenario == 2 || scenario == 9 || scenario == 10 {
    mode = BodyMode.KnownLength {length: 4}
  }
  let request = run Request.prepare(
    &origin,
    Version.Http11,
    method,
    target,
    &headers,
    &policy,
    mode,
    scenario == 1 || scenario == 2 || scenario == 10,
    valueLimits(),
    1024,
    512,
  )
    |> Effect.provideMut<Allocator>(&mut allocator)
  let memory = run scenarioProvider(scenario)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let adapter = TestTransport {memory: move memory, cancellationAudit: move cancellationAudit}
  let handler = Handler {request: move request, scenario: scenario}
  let attempted = run Effect.result(runOwned(move adapter, origin, scenario, move handler))
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let code = match move attempted {
    Result.Success {value} => value
    Result.Failure {error} => match move error {
      CallbackFailure cause => {
        if scenario != 17 || cause.code != 739 {
          return 126
        }
        return 0
      }
      ClientError cause => {
        fail move cause
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
  if code != 0 {
    return code
  }
  if scenario == 17 {
    return 127
  }
  return 0
}

effect<'call> fn failExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! CallbackFailure {
  drop exchangeValue
  fail CallbackFailure {code: 739}
}

effect<'call> fn continueExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let observed1 = run Client.receive(&mut exchangeValue.*)
  if observed1 != 103 {
    return 63
  }
  let observed2 = run Client.receive(&mut exchangeValue.*)
  if observed2 != 100 {
    return 64
  }
  let progress = run Client.writeSome(&mut exchangeValue.*, b"Wiki")
  if progress.consumed != 4 {
    return 65
  }
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let observed3 = run Client.receive(&mut exchangeValue.*)
  if observed3 != 417 {
    return 66
  }
  let mut output: [u8; 1] = [0]
  let count = run Client.readSome(&mut exchangeValue.*, &mut output)
  if count != usize.ZERO {
    return 62
  }
  run Client.finishResponse(&mut exchangeValue.*)
  return 0
}

effect<'call> fn earlyExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let observed4 = run Client.receive(&mut exchangeValue.*)
  if observed4 != 103 {
    return 67
  }
  let observed5 = run Client.receive(&mut exchangeValue.*)
  if observed5 != 417 {
    return 68
  }
  let rejected = run Effect.result(Client.writeSome(&mut exchangeValue.*, b"Wiki"))
  match move rejected {
    Result.Success {value} => {
      drop value
      return 69
    }
    Result.Failure {error} => match move error {
      ClientError cause => {
        if !invalidState(move cause) {
          return 70
        }
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
  let mut output: [u8; 1] = [0]
  let count = run Client.readSome(&mut exchangeValue.*, &mut output)
  if count != usize.ZERO {
    return 62
  }
  run Client.finishResponse(&mut exchangeValue.*)
  return 0
}

effect<'call> fn chunkedExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let progress = run Client.writeSome(&mut exchangeValue.*, b"Wiki")
  if progress.consumed != 4 {
    return 71
  }
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let observed6 = run Client.receive(&mut exchangeValue.*)
  if observed6 != 200 {
    return 72
  }
  let mut body: [u8; 4] = [0, 0, 0, 0]
  let count = run Client.readSome(&mut exchangeValue.*, &mut body)
  if count != 4 || body[0] != 87 || body[3] != 105 {
    return 73
  }
  let mut empty: [u8; 1] = [0]
  let observed7 = run Client.readSome(&mut exchangeValue.*, &mut empty)
  if observed7 != usize.ZERO {
    return 74
  }
  let responseTrailers = run Client.trailers(&exchangeValue.*)
  match move responseTrailers {
    Option.None => {
      return 75
    }
    Option.Some {value} => {
      if Trailers.count(&value) != usize.ONE {
        return 76
      }
    }
  }
  run Client.finishResponse(&mut exchangeValue.*)
  return 0
}

effect<'call> fn tunnelExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let observed8 = run Client.receive(&mut exchangeValue.*)
  if observed8 != 200 {
    return 77
  }
  return run Client.withTunnel(&mut exchangeValue.*, tunnel)
}

effect<'call> fn tunnel<'call, 'tunnel: 'call>(channel: &'call mut Tunnel<'tunnel, TestTransport>) -> i32
! ClientError | OutOfMemoryError
? &mut Allocator | &mut MonotonicClock | &mut Random {
  let mut output: [u8; 4] = [0, 0, 0, 0]
  let transfer = run Tunnel.readSome(&mut channel.*, &mut output, Option.none<Instant>())
  return match move transfer {
    ReadTransfer.End => 78
    ReadTransfer.Data {count} => {
      if count == 4 && output[0] == 84 && output[3] == 78 {
        return 0
      }
      return 79
    }
  }
}

effect<'call> fn abandonExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let observed9 = run Client.receive(&mut exchangeValue.*)
  if observed9 != 200 {
    return 80
  }
  return 0
}

effect<'call> fn limitedExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let observed10 = run Client.receive(&mut exchangeValue.*)
  if observed10 != 103 {
    return 81
  }
  let result = run Effect.result(Client.receive(&mut exchangeValue.*))
  return match move result {
    Result.Success {value} => {
      drop value
      return 82
    }
    Result.Failure {error} => match move error {
      ClientError cause => match move cause {
        ClientError.LimitExceeded => 0
        _ => 83
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
}

effect<'call> fn upgradeExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let result = run Effect.result(Client.receive(&mut exchangeValue.*))
  return match move result {
    Result.Success {value} => {
      drop value
      return 82
    }
    Result.Failure {error} => match move error {
      ClientError cause => match move cause {
        ClientError.UnsupportedUpgrade => 0
        _ => 83
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
}

effect<'call> fn timeoutExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  let result = run Effect.result(Client.send(&mut exchangeValue.*))
  return match move result {
    Result.Success {value} => {
      drop value
      return 84
    }
    Result.Failure {error} => match move error {
      ClientError cause => match move cause {
        ClientError.Timeout => 0
        _ => 85
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
}

effect<'call> fn partialExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let result = run Effect.result(Client.writeSome(&mut exchangeValue.*, b"Wiki"))
  return match move result {
    Result.Success {value} => {
      drop value
      return 86
    }
    Result.Failure {error} => match move error {
      OutOfMemoryError allocation => {
        fail move allocation
      }
      ClientError cause => match move cause {
        ClientError.Output {error: transport, progress} => {
          if progress.consumed != 4 || progress.written != 2 || progress.totalPayload != 4 {
            return 87
          }
          return match move transport {
            TransportError.Plain {error: plain} => match move plain {
              ByteIoError.Provider {operation, code} => {
                if code == 88 {
                  return 0
                }
                return 88
              }
              _ => 89
            }
            _ => 90
          }
        }
        _ => 91
      }
    }
  }
}

effect<'call> fn continueTimeoutExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let result = run Effect.result(Client.receive(&mut exchangeValue.*))
  return match move result {
    Result.Success {value} => {
      drop value
      return 92
    }
    Result.Failure {error} => match move error {
      ClientError cause => match move cause {
        ClientError.ContinueTimeout => 0
        _ => 93
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
}

effect<'call> fn discardExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let status = run Client.receive(&mut exchangeValue.*)
  if status != 200 {
    return 94
  }
  run Client.discardRemaining(&mut exchangeValue.*, 4)
  run Client.finishResponse(&mut exchangeValue.*)
  return 0
}

effect<'call> fn wireLimitExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let result = run Effect.result(Client.receive(&mut exchangeValue.*))
  return match move result {
    Result.Success {value} => {
      drop value
      return 95
    }
    Result.Failure {error} => match move error {
      ClientError cause => match move cause {
        ClientError.LimitExceeded => 0
        _ => 96
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
}

effect<'call> fn expiredCompletedExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let status = run Client.receive(&mut exchangeValue.*)
  if status != 200 {
    return 97
  }
  let mut body: [u8; 4] = [0, 0, 0, 0]
  let read = run Client.readSome(&mut exchangeValue.*, &mut body)
  if read != 4 {
    return 98
  }
  run MonotonicClock.waitUntil(SystemClock.make(10, 0))
  let result = run Effect.result(Client.readSome(&mut exchangeValue.*, &mut body))
  match move result {
    Result.Success {value} => {
      drop value
      return 99
    }
    Result.Failure {error} => match move error {
      ClientError cause => match move cause {
        ClientError.Timeout => {}
        _ => {
          return 100
        }
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
  let finish = run Effect.result(Client.finishResponse(&mut exchangeValue.*))
  return match move finish {
    Result.Success {value} => {
      drop value
      return 101
    }
    Result.Failure {error} => {
      if invalidState(move error) {
        return 0
      }
      return 102
    }
  }
}

fn invalidState(error: ClientError) -> bool {
  return match move error {
    ClientError.InvalidState => true
    _ => false
  }
}

fn inputFor(scenario: i32) -> &'static [u8] {
  if scenario == 14 {
    return b"HTTP/1.1 200 OK\\r\\nContent-Length: 25\\r\\nContent-Encoding: gzip\\r\\n\\r\\n\\x1f\\x8b\\x08\\x00\\x00\\x00\\x00\\x00\\x02\\xff\\x4b\\xce\\xc9\\x2f\\x4e\\x05\\x00\\xc4\\x81\\x01\\x13\\x05\\x00\\x00\\x00"
  }
  if scenario == 15 {
    return b"HTTP/1.1 201 Corrupt\\r\\nContent-Length: 25\\r\\nContent-Encoding: gzip\\r\\n\\r\\n\\x1f\\x8b\\x08\\x00\\x00\\x00\\x00\\x00\\x02\\xff\\x4b\\xce\\xc9\\x2f\\x4e\\x05\\x00\\xc5\\x81\\x01\\x13\\x05\\x00\\x00\\x00"
  }
  if scenario == 1 {
    return b"HTTP/1.1 103 Early Hints\\r\\n\\r\\nHTTP/1.1 100 Continue\\r\\n\\r\\nHTTP/1.1 417 Rejected\\r\\nContent-Length: 0\\r\\n\\r\\n"
  }
  if scenario == 2 {
    return b"HTTP/1.1 103 Early Hints\\r\\n\\r\\nHTTP/1.1 417 Rejected\\r\\nContent-Length: 0\\r\\n\\r\\n"
  }
  if scenario == 3 {
    return b"HTTP/1.1 200 OK\\r\\nTransfer-Encoding: chunked\\r\\nTrailer: X-Done\\r\\n\\r\\n4\\r\\nWiki\\r\\n0\\r\\nX-Done: yes\\r\\n\\r\\n"
  }
  if scenario == 4 {
    return b"HTTP/1.1 200 Connected\\r\\nContent-Length: ignored\\r\\nTransfer-Encoding: ignored\\r\\n\\r\\nTUNN"
  }
  if scenario == 6 || scenario == 12 {
    return b"HTTP/1.1 103 Hint\\r\\n\\r\\nHTTP/1.1 103 Hint\\r\\n\\r\\n"
  }
  if scenario == 7 {
    return b"HTTP/1.1 101 Switching\\r\\n\\r\\n"
  }
  return b"HTTP/1.1 200 OK\\r\\nContent-Length: 4\\r\\n\\r\\nWikiHTTP/1.1 201 Created\\r\\nContent-Length: 4\\r\\n\\r\\nWiki"
}

fn contentLimits() -> ContentLimits {
  return ContentLimits {
    maxEncoded: 2048,
    maxIntermediate: 4096,
    maxDecoded: 4096,
    maxOwned: 2097152,
    maxDepth: 4,
    intermediateCapacity: 8,
    inflate: InflateLimits {
      maxInputBytes: 2048,
      maxOutputBytes: 4096,
      maxMembers: 8,
      maxHeaderBytes: 512,
      maxMemoryBytes: 65536,
    },
    zstd: ZstdLimits {
      inputBytes: 2048,
      outputBytes: 4096,
      frames: 8,
      skippableBytes: 512,
      windowBytes: 1024,
      workspaceBytes: 1048576,
    },
  }
}

effect<'call> fn contentExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  run Client.send(&mut exchangeValue.*)
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Failure {error} => {
      drop error
      return 23
    }
    Result.Success {value} => value
  }
  run Client.finishRequest(&mut exchangeValue.*, &trailers)
  let status = run Client.receive(&mut exchangeValue.*)
  if status != 200 && status != 201 {
    return 107
  }
  let mut emptyOutput: [u8; 0] = []
  let rawNoOp = run Client.readSome(&mut exchangeValue.*, &mut emptyOutput)
  if rawNoOp != usize.ZERO {
    return 119
  }
  let selected = run Effect.result(
    Client.beginContent(&mut exchangeValue.*, ContentMode.Decode, contentLimits()),
  )
  match move selected {
    Result.Success {value} => {
      drop value
    }
    Result.Failure {error} => {
      drop error
      return 108
    }
  }
  let contentNoOp = run Client.readContentSome(&mut exchangeValue.*, &mut emptyOutput)
  if contentNoOp.written != usize.ZERO || contentNoOp.totalEncoded != 0 || contentNoOp.totalDecoded != 0 {
    return 120
  }
  let repeated = run Effect.result(
    Client.beginContent(&mut exchangeValue.*, ContentMode.Decode, contentLimits()),
  )
  match move repeated {
    Result.Success {value} => {
      drop value
      return 109
    }
    Result.Failure {error} => {
      match move error {
        ClientError cause => {
          if !invalidState(move cause) {
            return 110
          }
        }
        _ => {
          return 110
        }
      }
    }
  }
  let mut output: [u8; 5] = [0, 0, 0, 0, 0]
  let raw = run Effect.result(Client.readSome(&mut exchangeValue.*, &mut output))
  match move raw {
    Result.Success {value} => {
      drop value
      return 112
    }
    Result.Failure {error} => match move error {
      ClientError cause => {
        if !invalidState(move cause) {
          return 113
        }
      }
      OutOfMemoryError allocation => {
        fail move allocation
      }
    }
  }
  let expected = b"close"
  let mut total = usize.ZERO
  while true {
    let attempted = run Effect.result(Client.readContentSome(&mut exchangeValue.*, &mut output))
    let progress = match move attempted {
      Result.Success {value} => move value
      Result.Failure {error} => {
        return run contentFailure(move error, status)
      }
    }
    let mut index = usize.ZERO
    while index < progress.written {
      if total + index >= expected.length || output[index] != expected[total + index] {
        return 117
      }
      index = index + usize.ONE
    }
    total = total + progress.written
    if progress.state == ContentProgressState.End {
      break
    }
  }
  if status == 201 || total != 5 {
    return 118
  }
  run Client.finishResponse(&mut exchangeValue.*)
  return 0
}

effect fn contentFailure(error: ClientError | OutOfMemoryError, status: u16) -> i32
! OutOfMemoryError {
  return match move error {
    OutOfMemoryError allocation => {
      fail move allocation
    }
    ClientError cause => match move cause {
      ClientError.Content {error: content} => {
        if status != 201 {
          return 114
        }
        return match move content.reason {
          ContentReason.Inflate {stage, wireIndex, error: codec} => {
            drop codec
            return 0
          }
          _ => 115
        }
      }
      _ => 116
    }
  }
}

struct LoanHandler<'env> {
  marker: &'env i32
}

impl<'env> ConnectionHandler<Loan<'env, TestTransport>, i32, ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random> for LoanHandler<
  'env,
> {
  effect<'call> fn handle<'call>(
    handler: Self,
    connection: &'call mut Connection<Loan<'env, TestTransport>>,
  ) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
    drop handler
    drop connection
    return 9
  }
}

effect fn borrowedLoan() -> i32 ! ClientError | RequestError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut clock = FixedClock {mark: SystemClock.make(0, 0)}
  let mut random = FixedRandom {}
  let memory = run providerFor(b"", usize.ZERO)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut transport = TestTransport {
    memory: move memory,
    cancellationAudit: Option.none<Shared<CancellationAudit>>(),
  }
  let uri = match move Uri.parse("http://example.test/") {
    Result.Failure {error} => {
      drop error
      return 103
    }
    Result.Success {value} => value
  }
  let origin = match move Origin.fromUri(&uri) {
    Result.Failure {error} => {
      drop error
      return 104
    }
    Result.Success {value} => value
  }
  let marker = 0
  let result = run Client.withConnected(
    &mut transport,
    origin,
    Version.Http11,
    limits(),
    Option.none<Instant>(),
    LoanHandler {marker: &marker},
  )
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if result != 9 {
    return 105
  }
  if transport.memory.phase() != MemoryByteDuplexPhase.Closed || transport.memory.closeAttempts() != usize.ONE {
    return 106
  }
  return 0
}

struct CancellationWake {
  wake: Intrinsic.Wake
}

fn retainCancellationWake(wake: Intrinsic.Wake) -> CancellationWake {
  return CancellationWake {wake: move wake}
}

effect<'call> fn canceledExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 {
  drop exchangeValue
  run Execution.park(retainCancellationWake)
  return 121
}

fn cancellationReady(state: &()) -> () {
  return ()
}

fn cancellationComplete(state: &mut i32, value: i32) -> () {
  state.* = value
  return ()
}

fn cancellationParked(state: &mut i32, execution: Intrinsic.Execution<i32>) -> () {
  drop move execution
  state.* = 42
  return ()
}

fn cancellationAuditResult(audit: &CancellationAudit) -> i32 {
  if audit.drops != usize.ONE {
    return 122
  }
  if audit.closes != usize.ONE {
    return 123
  }
  return 0
}

effect fn structuredCancellation() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let audit = run Shared.make<CancellationAudit>(
    CancellationAudit {drops: usize.ZERO, closes: usize.ZERO},
  )
    |> Effect.provideMut<Allocator>(&mut allocator)
  let body = Effect.catchAll(
    runCase(16, Option.some<Shared<CancellationAudit>>(Shared.clone(&audit))),
    recover,
  )
  let execution = run Execution.make(move body, (), cancellationReady)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut result = 0
  run Execution.drive(move execution, &mut result, cancellationComplete, cancellationParked)
  if result != 42 {
    return result
  }
  return Shared.with(&audit, cancellationAuditResult)
}

effect fn allCases() -> i32 ! ClientError | RequestError | OutOfMemoryError {
  // Scheduling has its own portable backend suite; this case observes the native owner release.
  static if Intrinsic.targetArchitecture() != "wasm32" {
    let canceled = run structuredCancellation()
    if canceled != 0 {
      return canceled
    }
  }
  let borrowed = run borrowedLoan()
  if borrowed != 0 {
    return borrowed
  }
  let mut scenario = 0
  while scenario < 16 {
    let result = run runCase(scenario, Option.none<Shared<CancellationAudit>>())
    if result != 0 {
      return result
    }
    scenario = scenario + 1
  }
  let failed = run runCase(17, Option.none<Shared<CancellationAudit>>())
  return failed
}

effect fn recover(error: ClientError | RequestError | OutOfMemoryError) -> i32 {
  drop error
  return 51
}

pub fn main() -> i32 {
  return run Effect.catchAll(allCases(), recover)
}
`
