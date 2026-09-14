/** Portable client acceptance for owned reuse, staging, bounded reads, deadlines, and tunnels. */
export const httpClientAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.byte_duplex {ByteDuplex, ByteIoError, ByteIoOperation, ReadTransfer}
import silk.bytes {Bytes}
import silk.effect {Effect}
import silk.http {Method, Version, Header}
import silk.http_target {RequestTarget}
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
import silk.http_transport {HttpTransport, TransportError}
import silk.memory_byte_duplex {
  MemoryByteDuplex,
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

struct TestTransport {
  failFlush: bool
  expireAfterWrite: bool
  memory: MemoryByteDuplex
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
    if self.expireAfterWrite {
      run MonotonicClock.waitUntil(SystemClock.make(5, 0))
    }
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
    if self.failFlush {
      fail TransportError.Plain {
        error: ByteIoError.Provider {operation: ByteIoOperation.Flush, code: 93},
      }
    }
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
    if scenario == 19 || scenario == 20 || scenario == 21 {
      let mut selectedOptions = RequestOptions.defaults()
      if scenario == 19 {
        selectedOptions.continuePolicy = ContinuePolicy.Require100 {
          deadline: SystemClock.make(10, 0),
        }
      }
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 132
      }
      return 0
    }
    if scenario == 18 {
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        RequestOptions {
          deadline: Option.some<Instant>(SystemClock.make(5, 0)),
          continuePolicy: ContinuePolicy.Disabled,
        },
        scenario,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 128
      }
      return 0
    }
    if scenario == 17 {
      let attempted = run Effect.result(
        invokeExchange(&mut connection.*, &request, RequestOptions.defaults(), scenario),
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
    if scenario == 1 {
      let selectedOptions = RequestOptions {
        deadline: Option.none<Instant>(),
        continuePolicy: ContinuePolicy.Require100 {deadline: SystemClock.make(10, 0)},
      }
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
      )
      if result != 0 {
        return result
      }
      return 0
    }
    if scenario == 4 {
      let selectedOptions = RequestOptions.defaults()
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
      )
      if result != 0 {
        return result
      }
      return 0
    }
    if scenario == 12 {
      let selectedOptions = RequestOptions.defaults()
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
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
      let result = run invokeExchange(
        &mut connection.*,
        &request,
        move selectedOptions,
        scenario,
      )
      if result != 0 {
        return result
      }
      if connection.phase() != ConnectionPhase.Closed {
        return 18
      }
      return 0
    }
    let options = RequestOptions {
      deadline: Option.none<Instant>(),
      continuePolicy: ContinuePolicy.Disabled,
    }
    let result = run invokeExchange(&mut connection.*, &request, move options, scenario)
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
    let second = run invokeExchange(&mut connection.*, &request, move options2, scenario)
    if second != 404 {
      return second
    }
    return 0
  }
}

// One callback identity shares the exchange bracket across runtime scenarios.
service Scenario {
  effect fn selected() -> i32 ? &Scenario
}
struct SelectedScenario {
  value: i32
}
impl Scenario for SelectedScenario {
  effect fn selected(self: &Self) -> i32 {
    return self.value
  }
}

effect fn invokeExchange(
  connection: &mut Connection<TestTransport>,
  request: &PreparedRequest,
  options: RequestOptions,
  scenario: i32,
) -> i32 ! ClientError | OutOfMemoryError | CallbackFailure
? &mut Allocator | &mut MonotonicClock | &mut Random {
  let selected = SelectedScenario {value: scenario}
  return run Client.withExchange(&mut connection.*, request, move options, dispatchExchange)
    |> Effect.provide<Scenario>(&selected)
}

effect<'call> fn dispatchExchange<'call, 'exchange: 'call>(
  value: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError | CallbackFailure
? &Scenario | &mut Allocator | &mut MonotonicClock | &mut Random {
  let scenario = run Scenario.selected()
  if scenario == 19 || scenario == 20 || scenario == 21 {
    return run flushFailureExchange(&mut value.*)
  }
  if scenario == 18 {
    return run outputTimeoutExchange(&mut value.*)
  }
  if scenario == 17 {
    return run failExchange(&mut value.*)
  }
  if scenario == 1 {
    return run continueExchange(&mut value.*)
  }
  if scenario == 2 {
    return run earlyExchange(&mut value.*)
  }
  if scenario == 3 {
    return run chunkedExchange(&mut value.*)
  }
  if scenario == 4 {
    return run tunnelExchange(&mut value.*)
  }
  if scenario == 5 {
    return run abandonExchange(&mut value.*)
  }
  if scenario == 6 {
    return run limitedExchange(&mut value.*)
  }
  if scenario == 7 {
    return run upgradeExchange(&mut value.*)
  }
  if scenario == 8 {
    return run timeoutExchange(&mut value.*)
  }
  if scenario == 9 {
    return run partialExchange(&mut value.*)
  }
  if scenario == 10 {
    return run continueTimeoutExchange(&mut value.*)
  }
  if scenario == 11 {
    return run discardExchange(&mut value.*)
  }
  if scenario == 12 {
    return run wireLimitExchange(&mut value.*)
  }
  if scenario == 13 {
    return run expiredCompletedExchange(&mut value.*)
  }
  return run exchange(&mut value.*)
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
  if status != 200 && status != 404 {
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
  return 404
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
  if scenario == 21 {
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
      MemoryWriteEvent {
        readyAt: SystemClock.make(0, 0),
        action: MemoryWriteAction.Accept {count: 256},
      },
    )
    run Vector.append(
      &mut writes,
      MemoryWriteEvent {
        readyAt: SystemClock.make(0, 0),
        action: MemoryWriteAction.Accept {count: 2},
      },
    )
    run Vector.append(
      &mut writes,
      MemoryWriteEvent {
        readyAt: SystemClock.make(0, 0),
        action: MemoryWriteAction.Failure {code: 88},
      },
    )
    return run MemoryByteDuplex.make(move reads, move writes, 256, 32, Option.none<i32>())
  }
  if scenario == 18 {
    let reads = Vector.make<MemoryReadEvent>()
    let mut writes = Vector.make<MemoryWriteEvent>()
    run Vector.append(
      &mut writes,
      MemoryWriteEvent {
        readyAt: SystemClock.make(0, 0),
        action: MemoryWriteAction.Accept {count: 2},
      },
    )
    return run MemoryByteDuplex.make(move reads, move writes, 256, 32, Option.none<i32>())
  }

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

effect fn runCase(scenario: i32) -> i32 ! ClientError | RequestError | OutOfMemoryError {
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
  } else if scenario == 1 || scenario == 2 || scenario == 3 || scenario == 9 || scenario == 10 || scenario == 19 || scenario == 20 || scenario == 21 {
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
  if scenario == 3 || scenario == 20 || scenario == 21 {
    mode = BodyMode.Chunked
  } else if scenario == 1 || scenario == 2 || scenario == 9 || scenario == 10 || scenario == 19 || scenario == 20 {
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
    scenario == 1 || scenario == 2 || scenario == 10 || scenario == 19,
    valueLimits(),
    1024,
    512,
  )
    |> Effect.provideMut<Allocator>(&mut allocator)
  let memory = run scenarioProvider(scenario)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let adapter = TestTransport {
    memory: move memory,
    expireAfterWrite: scenario == 18,
    failFlush: scenario == 19 || scenario == 20,
  }
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

effect<'call> fn flushFailureExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  let sent = run Effect.result(Client.send(&mut exchangeValue.*))
  match move sent {
    Result.Failure {error} => {
      return run checkFlushFailure(move error, false)
    }
    Result.Success {value} => {
      drop value
    }
  }
  let progress = run Client.writeSome(&mut exchangeValue.*, b"Wiki")
  if progress.consumed != 4 {
    return 133
  }
  let trailerEntries: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&trailerEntries, valueLimits()) {
    Result.Success {value} => value
    Result.Failure {error} => {
      drop error
      return 134
    }
  }
  let finished = run Effect.result(Client.finishRequest(&mut exchangeValue.*, &trailers))
  return match move finished {
    Result.Success {value} => {
      drop value
      return 135
    }
    Result.Failure {error} => {
      return run checkFlushFailure(move error, true)
    }
  }
}

effect fn checkFlushFailure(error: ClientError | OutOfMemoryError, finishing: bool) -> i32
! OutOfMemoryError {
  return match move error {
    OutOfMemoryError allocation => {
      fail move allocation
    }
    ClientError cause => match move cause {
      ClientError.Output {error: transport, progress} => {
        if progress.consumed != usize.ZERO || progress.written == usize.ZERO {
          return 136
        }
        if finishing && (progress.totalPayload != 4 || progress.totalWire != 14) {
          return 137
        }
        return match move transport {
          TransportError.Plain {error: plain} => match move plain {
            ByteIoError.Provider {operation, code} => {
              if operation == ByteIoOperation.Write && code == 88 {
                if !finishing || progress.written != 2 {
                  return 142
                }
                return 0
              }
              if operation != ByteIoOperation.Flush || code != 93 || (finishing && progress.written != 5) {
                return 138
              }
              return 0
            }
            _ => 139
          }
          _ => 140
        }
      }
      _ => 141
    }
  }
}

effect<'call> fn outputTimeoutExchange<'call, 'exchange: 'call>(
  exchangeValue: &'call mut Exchange<'exchange, TestTransport>,
) -> i32 ! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  let result = run Effect.result(Client.send(&mut exchangeValue.*))
  return match move result {
    Result.Success {value} => {
      drop value
      return 129
    }
    Result.Failure {error} => match move error {
      ClientError cause => match move cause {
        ClientError.OutputTimeout {progress} => {
          if progress.written != 2 || progress.totalWire != 2 || progress.consumed != usize.ZERO {
            return 130
          }
          return 0
        }
        _ => 131
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
  return b"HTTP/1.1 200 OK\\r\\nContent-Length: 4\\r\\n\\r\\nWikiHTTP/1.1 404 Not Found\\r\\nContent-Length: 4\\r\\n\\r\\nWiki"
}

effect fn allCases() -> i32 ! ClientError | RequestError | OutOfMemoryError {
  let mut scenario = 0
  while scenario < 14 {
    let result = run runCase(scenario)
    if result != 0 {
      return result
    }
    scenario = scenario + 1
  }
  let failed = run runCase(17)
  if failed != 0 {
    return failed
  }
  let timeout = run runCase(18)
  if timeout != 0 {
    return timeout
  }
  let headFlush = run runCase(19)
  if headFlush != 0 {
    return headFlush
  }
  let bodyFlush = run runCase(20)
  if bodyFlush != 0 {
    return bodyFlush
  }
  return run runCase(21)
}

effect fn recover(error: ClientError | RequestError | OutOfMemoryError) -> i32 {
  drop error
  return 51
}

pub fn main() -> i32 {
  return run Effect.catchAll(allCases(), recover)
}
`
