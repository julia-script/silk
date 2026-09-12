import {
  tlsClientRsaApplicationWrite,
  tlsClientRsaClientHello,
  tlsClientRsaRootPem,
  tlsClientRsaServerCloseNotify,
  tlsClientRsaServerFlight,
} from './tlsClientAcceptance.js'

const silkBytes = (bytes: Uint8Array): string =>
  `b"${[...bytes].map((byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`

// The capture's first record uses 0x0301; Silk emits 0x0303, as the TLS-client
// acceptance witness asserts. Preserve every handshake byte and compare the full output prefix.
const clientHello = tlsClientRsaClientHello.map((byte, index) => (index === 2 ? 3 : byte))

const firstFlight = tlsClientRsaServerFlight.subarray(0, 64)
const secondFlight = tlsClientRsaServerFlight.subarray(64, 576)
const thirdFlight = tlsClientRsaServerFlight.subarray(576)

const connectionSource = (
  includeFailureCases: boolean,
): string => `import silk.allocator {Allocator, OutOfMemoryError}
import silk.byte_duplex {ByteDuplex, ByteIoError, ByteIoOperation, ReadTransfer}
import silk.bytes {Bytes}
import silk.effect {Effect}
import silk.execution {Execution}
import silk.https_identity {HttpsIdentity, IdentityError, OriginHost, ReferenceIdentity}
import silk.memory_byte_duplex {
  MemoryByteDuplex,
  MemoryByteDuplexPhase,
  MemoryReadEvent,
  MemoryWriteAction,
  MemoryWriteEvent,
}
import silk.monotonic_clock {MonotonicClock}
import silk.option {Option}
import silk.random {Random}
import silk.result {Result}
import silk.shared {Shared}
import silk.system_clock {Instant, SystemClock}
import silk.tls_client {AlpnConfig, Authentication, ClientConfig, ClientLimits, NamedGroup, TlsError}
import silk.tls_connection {Connection, ConnectionError, ConnectionOptions, withClient}
import silk.tls_record {CipherSuite}
import silk.trust_snapshot {TrustLoadLimits, TrustSnapshot, TrustSourceError}
import silk.trust_source {TrustSource}
import silk.u64
import silk.usize
import silk.vector {Vector}

struct ScriptedRandom {filled: usize}
impl Random for ScriptedRandom {
  effect fn fillBytes(self: &mut Self, output: &mut [u8]) -> () {
    let mut index = usize.ZERO
    while index < output.length {
      output[index] = usize.toU8((self.filled + index + usize.ONE) % 251 + usize.ONE)
      index = index + usize.ONE
    }
    self.filled = self.filled + output.length
    return ()
  }
}

struct FixedWallClock {calls: usize}
impl SystemClock for FixedWallClock {
  effect fn now(self: &mut Self) -> Instant {
    self.calls = self.calls + usize.ONE
    return SystemClock.make(1789156800, 123456789)
  }
  effect fn getResolution(self: &mut Self) -> u64 { return u64.toU64(1) }
}

struct VirtualClock {mark: Instant nowCalls: usize waitCalls: usize}
impl MonotonicClock for VirtualClock {
  effect fn now(self: &mut Self) -> Instant {
    self.nowCalls = self.nowCalls + usize.ONE
    return SystemClock.make(SystemClock.seconds(&self.mark), SystemClock.nanoseconds(&self.mark))
  }
  effect fn getResolution(self: &mut Self) -> u64 { return u64.toU64(1) }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () {
    self.waitCalls = self.waitCalls + usize.ONE
    self.mark = move when
    return ()
  }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () {
    self.waitCalls = self.waitCalls + usize.ONE
    self.mark = MonotonicClock.deadlineAfter(&self.mark, howLong)
    return ()
  }
}

struct CountingTrust {snapshot: TrustSnapshot loads: usize}
impl CountingTrust {
  effect fn load(
    self: &mut Self,
    limits: TrustLoadLimits,
  ) -> TrustSnapshot
  ! TrustSourceError | OutOfMemoryError
  ? &mut Allocator {
    self.loads = self.loads + usize.ONE
    let copied = run TrustSnapshot.copy(&self.snapshot, limits.snapshot)
    return match move copied {
      Result<TrustSnapshot, TrustSourceError>.Success {value} => move value
      Result<TrustSnapshot, TrustSourceError>.Failure {error} => { fail move error }
    }
  }
}
impl TrustSource for CountingTrust {load: CountingTrust.load}

service CallbackAudit { effect fn invoked() -> () ? &mut CallbackAudit }
struct CallbackCounter {calls: usize}
effect fn countCallback(self: &mut CallbackCounter) -> () { self.calls = self.calls + usize.ONE return () }
impl CallbackAudit for CallbackCounter {invoked: CallbackCounter.countCallback}

struct TimeoutDuplex {
  writes: usize
  flushes: usize
  closes: usize
  deadlineSeconds: i64
  deadlineNanoseconds: i64
}
impl TimeoutDuplex {
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
    self.writes = self.writes + usize.ONE
    return input.length
  }
  unsafe effect fn flush(
    self: &mut Self,
    deadline: Option<Instant>,
  ) -> () ! ByteIoError ? &mut MonotonicClock {
    self.flushes = self.flushes + usize.ONE
    if let Option<Instant>.Some {value} = &deadline {
      self.deadlineSeconds = SystemClock.seconds(&value)
      self.deadlineNanoseconds = SystemClock.nanoseconds(&value)
    }
    fail ByteDuplex.timeout(ByteIoOperation.Flush)
  }
  unsafe effect fn shutdown(
    self: &mut Self,
    deadline: Option<Instant>,
  ) -> () ! ByteIoError ? &mut MonotonicClock {
    drop deadline
    return ()
  }
  unsafe effect fn close(self: &mut Self) -> () ! ByteIoError {
    self.closes = self.closes + usize.ONE
    fail ByteDuplex.provider(ByteIoOperation.Close, 88)
  }
}
impl ByteDuplex for TimeoutDuplex {
  readSomeRaw: TimeoutDuplex.read
  writeSomeRaw: TimeoutDuplex.write
  flushRaw: TimeoutDuplex.flush
  shutdownWriteRaw: TimeoutDuplex.shutdown
  closeRaw: TimeoutDuplex.close
}

struct CancellationState {closes: usize callbacks: usize}
fn recordCancellationClose(state: &mut CancellationState) -> () {
  state.closes = state.closes + usize.ONE
  return ()
}
fn recordCancellationCallback(state: &mut CancellationState) -> () {
  state.callbacks = state.callbacks + usize.ONE
  return ()
}
fn cancellationResult(state: &mut CancellationState) -> i32 {
  if state.closes == usize.ONE && state.callbacks == usize.ZERO { return 42 }
  return -3
}

struct SharedCallbackCounter {state: Shared<CancellationState>}
effect fn countSharedCallback(self: &mut SharedCallbackCounter) -> () {
  Shared.withMut(&self.state, recordCancellationCallback)
  return ()
}
impl CallbackAudit for SharedCallbackCounter {invoked: SharedCallbackCounter.countSharedCallback}

struct ParkingClock {}
struct ParkGuard {wake: Intrinsic.Wake}
fn retainWake(wake: Intrinsic.Wake) -> ParkGuard { return ParkGuard {wake: move wake} }
impl MonotonicClock for ParkingClock {
  effect fn now(self: &mut Self) -> Instant { return SystemClock.make(100, 0) }
  effect fn getResolution(self: &mut Self) -> u64 { return u64.toU64(1) }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () {
    drop when
    run Execution.park(retainWake)
    return ()
  }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () {
    drop howLong
    run Execution.park(retainWake)
    return ()
  }
}

struct ParkingDuplex {state: Shared<CancellationState>}
impl ParkingDuplex {
  unsafe effect fn read(
    self: &mut Self,
    output: &mut [u8],
    deadline: Option<Instant>,
  ) -> ReadTransfer ! ByteIoError ? &mut MonotonicClock {
    drop output
    let wakeAt = match move deadline {
      Option<Instant>.None => SystemClock.make(200, 0)
      Option<Instant>.Some {value} => move value
    }
    run MonotonicClock.waitUntil(move wakeAt)
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
  ) -> () ! ByteIoError ? &mut MonotonicClock { drop deadline return () }
  unsafe effect fn shutdown(
    self: &mut Self,
    deadline: Option<Instant>,
  ) -> () ! ByteIoError ? &mut MonotonicClock { drop deadline return () }
  unsafe effect fn close(self: &mut Self) -> () ! ByteIoError {
    Shared.withMut(&self.state, recordCancellationClose)
    return ()
  }
}
impl ByteDuplex for ParkingDuplex {
  readSomeRaw: ParkingDuplex.read
  writeSomeRaw: ParkingDuplex.write
  flushRaw: ParkingDuplex.flush
  shutdownWriteRaw: ParkingDuplex.shutdown
  closeRaw: ParkingDuplex.close
}

struct CallbackFailure {code: i32}

fn reference<'a>(bytes: &'a [u8]) -> ReferenceIdentity<'a> {
  let made = HttpsIdentity.reference(OriginHost<'a>.Dns {bytes: bytes})
  return match move made {
    Result<ReferenceIdentity<'a>, IdentityError>.Success {value} => value
    Result<ReferenceIdentity<'a>, IdentityError>.Failure {error} => {
      let invalid = 1 / 0
      return reference(bytes)
    }
  }
}

effect fn rootTrust() -> TrustSnapshot
! TrustSourceError | OutOfMemoryError
? &mut Allocator {
  let made = run TrustSnapshot.fromPem(${silkBytes(tlsClientRsaRootPem)}, TrustLoadLimits.defaults())
  return match move made {
    Result<TrustSnapshot, TrustSourceError>.Success {value} => move value
    Result<TrustSnapshot, TrustSourceError>.Failure {error} => { fail move error }
  }
}

fn contains(haystack: &[u8], needle: &[u8]) -> bool {
  if needle.length > haystack.length { return false }
  let mut start = usize.ZERO
  while start <= haystack.length - needle.length {
    let mut equal = true
    let mut index = usize.ZERO
    while index < needle.length {
      if haystack[start + index] != needle[index] { equal = false }
      index = index + usize.ONE
    }
    if equal { return true }
    start = start + usize.ONE
  }
  return false
}

effect fn memoryTransport(
  closeFailure: Option<i32>,
  peerClose: bool,
) -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let first = run Bytes.copy(${silkBytes(firstFlight)})
  let second = run Bytes.copy(${silkBytes(secondFlight)})
  let third = run Bytes.copy(${silkBytes(thirdFlight)})
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(100, 0),
    bytes: move first,
  })
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(100, 0),
    bytes: move second,
  })
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(100, 0),
    bytes: move third,
  })
  if peerClose {
    run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
      readyAt: SystemClock.make(100, 0),
      bytes: run Bytes.copy(${silkBytes(tlsClientRsaServerCloseNotify)}),
    })
  }
  let mut writes = Vector.make<MemoryWriteEvent>()
  let mut index = usize.ZERO
  while index < 16 {
    let mut count: usize = 65535
    if index == usize.ZERO { count = usize.ONE }
    if index == usize.ONE { count = 2 }
    run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
      readyAt: SystemClock.make(100, 0),
      action: MemoryWriteAction.Accept {count: count},
    })
    index = index + usize.ONE
  }
  return run MemoryByteDuplex.make(move reads, move writes, 4096, 64, move closeFailure)
}

effect fn emptyTransport() -> MemoryByteDuplex
! OutOfMemoryError
? &mut Allocator {
  let reads = Vector.make<MemoryReadEvent>()
  let mut writes = Vector.make<MemoryWriteEvent>()
  let mut index = usize.ZERO
  while index < 4 {
    run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
      readyAt: SystemClock.make(100, 0),
      action: MemoryWriteAction.Accept {count: 65535},
    })
    index = index + usize.ONE
  }
  return run MemoryByteDuplex.make(
    move reads,
    move writes,
    1024,
    16,
    Option.none<i32>(),
  )
}

fn validAuthentication<'a>(value: &Authentication<'a>) -> bool {
  return value.suite() == CipherSuite.ChaCha20Poly1305Sha256
    && value.group() == NamedGroup.X25519
    && value.anchorIndex() == usize.ZERO
    && value.sanIndex() == usize.ZERO
}

effect<'transport> fn useAuthenticated<'transport, P>(
  connection: &'transport mut Connection<'transport, P>,
) -> i32
! ConnectionError | OutOfMemoryError
? &mut MonotonicClock | &mut Allocator | &mut Random
where &mut P provides &ByteDuplex from &mut ByteDuplex
  | &mut MonotonicClock
  | &mut Allocator
  | &mut Random {
  let authentication = Connection.authentication(&connection.*)
  match move authentication {
    Option.None => { return 1 }
    Option.Some {value} => { if !validAuthentication(&value) { return 2 } }
  }
  let mut plaintext: [u8; 40] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]
  let read = run Connection.readSome(&mut connection.*, &mut plaintext, Option.none<Instant>())
  let count = match move read {
    ReadTransfer.Data {count: transferred} => transferred
    ReadTransfer.End => { return 3 }
  }
  let expected = b"coalesced authenticated plaintext"
  if count != expected.length { return 4 }
  let mut index = usize.ZERO
  while index < count {
    if plaintext[index] != expected[index] { return 5 }
    index = index + usize.ONE
  }
  let written = run Connection.writeSome(&mut connection.*, &b"ping", Option.none<Instant>())
  if written != 4 { return 6 }
  run Connection.flush(&mut connection.*, Option.none<Instant>())
  run Connection.shutdownWrite(&mut connection.*, Option.none<Instant>())
  let ended = run Connection.readSome(&mut connection.*, &mut plaintext, Option.none<Instant>())
  match move ended {
    ReadTransfer.Data {count: unexpectedCount} => { drop unexpectedCount return 7 }
    ReadTransfer.End => {}
  }
  return 42
}

effect<'transport> fn expectTruncation<'transport, P>(
  connection: &'transport mut Connection<'transport, P>,
) -> i32
! ConnectionError | OutOfMemoryError
? &mut MonotonicClock | &mut Allocator | &mut Random
where &mut P provides &ByteDuplex from &mut ByteDuplex
  | &mut MonotonicClock
  | &mut Allocator
  | &mut Random {
  let mut plaintext: [u8; 40] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]
  let first = run Connection.readSome(&mut connection.*, &mut plaintext, Option.none<Instant>())
  match move first {
    ReadTransfer.Data {count} => { if count != b"coalesced authenticated plaintext".length { return 1 } }
    ReadTransfer.End => { return 2 }
  }
  let truncated = run Effect.result(
    Connection.readSome(&mut connection.*, &mut plaintext, Option.none<Instant>())
  )
  match move truncated {
    Result<ReadTransfer, ConnectionError | OutOfMemoryError>.Success {value} => { return 3 }
    Result<ReadTransfer, ConnectionError | OutOfMemoryError>.Failure {error} => match move error {
      ConnectionError.Tls {error: tlsError} => match move tlsError {
        TlsError.Truncated => {}
        _ => { return 4 }
      }
      _ => { return 5 }
    }
  }
  return 42
}

effect<'transport> fn failAuthenticated<'transport, P>(
  connection: &'transport mut Connection<'transport, P>,
) -> i32
! CallbackFailure {
  match move Connection.authentication(&connection.*) {
    Option.None => { return 1 }
    Option.Some {value} => { drop value }
  }
  fail CallbackFailure {code: 77}
}

effect<'transport> fn shouldNotRun<'transport, P>(
  connection: &'transport mut Connection<'transport, P>,
) -> i32 ? &mut CallbackAudit {
  drop connection
  run CallbackAudit.invoked()
  return 1
}

effect fn canceledConnection(state: Shared<CancellationState>) -> i32
! ConnectionError | TrustSourceError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let snapshot = run rootTrust()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut trust = CountingTrust {snapshot: move snapshot, loads: usize.ZERO}
  let mut wall = FixedWallClock {calls: usize.ZERO}
  let mut monotonic = ParkingClock {}
  let mut random = ScriptedRandom {filled: usize.ZERO}
  let mut callbacks = SharedCallbackCounter {state: Shared.clone(&state)}
  let mut transport = ParkingDuplex {state: move state}
  let host = b"ExAmPlE.com"
  let config = ClientConfig {
    reference: reference(&host),
    alpn: AlpnConfig.defaults(),
    limits: ClientLimits.defaults(),
  }
  return run withClient(
    &mut transport,
    &config,
    ConnectionOptions.defaults(),
    shouldNotRun,
  )
    |> Effect.provideMut<CallbackAudit>(&mut callbacks)
    |> Effect.provideMut<TrustSource>(&mut trust)
    |> Effect.provideMut<SystemClock>(&mut wall)
    |> Effect.provideMut<MonotonicClock>(&mut monotonic)
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
}

fn cancellationReady(state: &()) -> () { return () }
fn cancellationComplete(state: &mut (), value: i32) -> () { drop value return () }
fn cancellationParked(state: &mut (), execution: Intrinsic.Execution<i32>) -> () {
  drop move execution
  return ()
}
effect fn cancellationFailed<E>(error: E) -> i32 { drop error return -4 }

effect fn cancelSuspendedConnection() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let state = run Shared.make<CancellationState>(CancellationState {
    closes: usize.ZERO,
    callbacks: usize.ZERO,
  }) |> Effect.provideMut<Allocator>(&mut allocator)
  let body = Effect.catchAll(
    canceledConnection(Shared.clone(&state)),
    cancellationFailed,
  )
  let execution = run Execution.make(
    move body,
    (),
    cancellationReady,
  ) |> Effect.provideMut<Allocator>(&mut allocator)
  let mut branch = ()
  run Execution.drive(move execution, &mut branch, cancellationComplete, cancellationParked)
  return Shared.withMut(&state, cancellationResult)
}

effect fn runCases(
  config: &ClientConfig,
  random: &mut ScriptedRandom,
) -> i32
! ConnectionError | TrustSourceError | OutOfMemoryError
? &mut TrustSource
  | &mut SystemClock
  | &mut MonotonicClock
  | &mut Allocator {
  let mut success = run memoryTransport(Option.none<i32>(), true)
  let attempted = run Effect.result(withClient(
    &mut success,
    config,
    ConnectionOptions.defaults(),
    useAuthenticated,
  ) |> Effect.provideMut<Random>(&mut random.*))
  let completed = match move attempted {
    Result<i32, ConnectionError | TrustSourceError | OutOfMemoryError>.Success {value} => value
    Result<i32, ConnectionError | TrustSourceError | OutOfMemoryError>.Failure {error} => {
      let failedAudit = MemoryByteDuplex.audit(&success)
      if failedAudit.length == usize.ONE && failedAudit[0].operation == ByteIoOperation.Close {
        return -950
      }
      return run failed(move error)
    }
  }
  if completed != 42 { return 1000 + completed }
  if MemoryByteDuplex.closeAttempts(&success) != usize.ONE { return 11 }
  if MemoryByteDuplex.phase(&success) != MemoryByteDuplexPhase.Closed { return 12 }
  let outbound = MemoryByteDuplex.outbound(&success)
  let hello = ${silkBytes(clientHello)}
  if outbound.length < hello.length { return 13 }
  let mut index = usize.ZERO
  while index < hello.length {
    if outbound[index] != hello[index] { return 14 }
    index = index + usize.ONE
  }
  if !contains(outbound, ${silkBytes(tlsClientRsaApplicationWrite)}) { return 15 }
  let audit = MemoryByteDuplex.audit(&success)
  if audit.length < 10 { return 16 }
  if audit[0].operation != ByteIoOperation.Write || audit[0].count != usize.ONE { return 17 }
  if audit[1].operation != ByteIoOperation.Write || audit[1].count != 2 { return 18 }
  if audit[audit.length - usize.ONE].operation != ByteIoOperation.Close { return 19 }
  let mut shutdownFound = false
  let mut auditIndex = usize.ONE
  while auditIndex < audit.length {
    if audit[auditIndex].operation == ByteIoOperation.ShutdownWrite {
      if audit[auditIndex - usize.ONE].operation != ByteIoOperation.Flush { return 109 }
      shutdownFound = true
    }
    auditIndex = auditIndex + usize.ONE
  }
  if !shutdownFound { return 110 }

${
  includeFailureCases
    ? `  random.filled = usize.ZERO
  let mut failing = run memoryTransport(Option.some<i32>(91), false)
  let failed = run Effect.result(withClient(
    &mut failing,
    config,
    ConnectionOptions.defaults(),
    failAuthenticated,
  ) |> Effect.provideMut<Random>(&mut random.*))
  match move failed {
    Result<i32, CallbackFailure | ConnectionError | TrustSourceError | OutOfMemoryError>.Success {value} => {
      return 20
    }
    Result<i32, CallbackFailure | ConnectionError | TrustSourceError | OutOfMemoryError>.Failure {error} => {
      match move error {
        CallbackFailure {code} => { if code != 77 { return 21 } }
        _ => { return 22 }
      }
    }
  }
  if MemoryByteDuplex.closeAttempts(&failing) != usize.ONE { return 23 }
  if MemoryByteDuplex.phase(&failing) != MemoryByteDuplexPhase.Closed { return 24 }

  random.filled = usize.ZERO
  let mut truncated = run memoryTransport(Option.none<i32>(), false)
  let truncation = run withClient(
    &mut truncated,
    config,
    ConnectionOptions.defaults(),
    expectTruncation,
  ) |> Effect.provideMut<Random>(&mut random.*)
  if truncation != 42 { return 25 }
  if MemoryByteDuplex.closeAttempts(&truncated) != usize.ONE { return 26 }

  random.filled = usize.ZERO
  let mut ended = run emptyTransport()
  let mut endedCallbacks = CallbackCounter {calls: usize.ZERO}
  let handshakeEnd = run Effect.result(withClient(
    &mut ended,
    config,
    ConnectionOptions.defaults(),
    shouldNotRun,
  )
    |> Effect.provideMut<CallbackAudit>(&mut endedCallbacks)
    |> Effect.provideMut<Random>(&mut random.*))
  match move handshakeEnd {
    Result<i32, ConnectionError | TrustSourceError | OutOfMemoryError>.Success {value} => {
      return 27
    }
    Result<i32, ConnectionError | TrustSourceError | OutOfMemoryError>.Failure {error} => {
      match move error {
        ConnectionError.Tls {error: tlsError} => match move tlsError {
          TlsError.HandshakeTruncated => {}
          _ => { return 28 }
        }
        _ => { return 29 }
      }
    }
  }
  if endedCallbacks.calls != usize.ZERO { return 30 }
  if MemoryByteDuplex.closeAttempts(&ended) != usize.ONE { return 31 }

  random.filled = usize.ZERO
  let mut timeout = TimeoutDuplex {
    writes: usize.ZERO,
    flushes: usize.ZERO,
    closes: usize.ZERO,
    deadlineSeconds: 0,
    deadlineNanoseconds: 0,
  }
  let mut callbacks = CallbackCounter {calls: usize.ZERO}
  let timed = run Effect.result(withClient(
    &mut timeout,
    config,
    ConnectionOptions.defaults(),
    shouldNotRun,
  )
    |> Effect.provideMut<CallbackAudit>(&mut callbacks)
    |> Effect.provideMut<Random>(&mut random.*))
  match move timed {
    Result<i32, ConnectionError | TrustSourceError | OutOfMemoryError>.Success {value} => {
      return 32
    }
    Result<i32, ConnectionError | TrustSourceError | OutOfMemoryError>.Failure {error} => {
      match move error {
        ConnectionError.HandshakeTimeout => {}
        _ => { return 33 }
      }
    }
  }
  if timeout.writes != usize.ONE || timeout.flushes != usize.ONE { return 34 }
  if timeout.closes != usize.ONE || callbacks.calls != usize.ZERO { return 35 }
  if timeout.deadlineSeconds != 130 || timeout.deadlineNanoseconds != 0 { return 36 }
  let canceled = run cancelSuspendedConnection()
  if canceled != 42 { return 37 }
`
    : ''
}  return 42
}

effect fn failed(error: ConnectionError | TrustSourceError | OutOfMemoryError) -> i32 {
  match move error {
    ConnectionError.HandshakeTimeout => { return -100 }
    ConnectionError.Io {error: io} => match move io {
      ByteIoError.Timeout {operation} => { return -201 }
      ByteIoError.InvalidTransferCount {operation, count, limit} => { return -202 }
      ByteIoError.Closed {operation} => match operation {
        ByteIoOperation.Read => { return -2031 }
        ByteIoOperation.Write => { return -2032 }
        ByteIoOperation.Flush => { return -2033 }
        ByteIoOperation.ShutdownWrite => { return -2034 }
        ByteIoOperation.Close => { return -2035 }
      }
      ByteIoError.Provider {operation, code} => { return -300 - code }
    }
    ConnectionError.Tls {error: tlsError} => match move tlsError {
      TlsError.PeerAlert {code} => { return -101 }
      TlsError.BadRecordMac => { return -102 }
      TlsError.ProtocolViolation {reason} => { return -103 }
      TlsError.CertificateDecode {error: detail} => { return -104 }
      TlsError.CertificatePath {error: detail} => { return -105 }
      TlsError.CertificateIdentity {error: detail} => { return -106 }
      TlsError.CertificateVerify {error: detail} => { return -107 }
      TlsError.Finished => { return -108 }
      TlsError.UnsupportedProfile => { return -109 }
      TlsError.LimitExceeded {kind, limit} => { return -110 }
      TlsError.InvalidState {operation} => { return -111 }
      TlsError.EmptyBuffer => { return -112 }
      TlsError.HandshakeTruncated => { return -113 }
      TlsError.Truncated => { return -114 }
      TlsError.NoApplicationProtocol => { return -115 }
    }
    OutOfMemoryError {} => { return -901 }
    _ => { return -902 }
  }
}

effect fn program() -> i32 ! TrustSourceError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let snapshot = run rootTrust()
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut trust = CountingTrust {snapshot: move snapshot, loads: usize.ZERO}
  let mut wall = FixedWallClock {calls: usize.ZERO}
  let mut monotonic = VirtualClock {
    mark: SystemClock.make(100, 0),
    nowCalls: usize.ZERO,
    waitCalls: usize.ZERO,
  }
  let mut random = ScriptedRandom {filled: usize.ZERO}
  let host = b"ExAmPlE.com"
  let config = ClientConfig {
    reference: reference(&host),
    alpn: AlpnConfig.defaults(),
    limits: ClientLimits.defaults(),
  }
  let result = run Effect.catchAll(
    runCases(&config, &mut random)
      |> Effect.provideMut<TrustSource>(&mut trust)
      |> Effect.provideMut<SystemClock>(&mut wall)
      |> Effect.provideMut<MonotonicClock>(&mut monotonic)
      |> Effect.provideMut<Allocator>(&mut allocator),
    failed,
  )
  if result != 42 { return result }
  if trust.loads != ${includeFailureCases ? 5 : 1} || wall.calls != ${includeFailureCases ? 5 : 1} { return -2 }
  return 42
}

effect fn startupFailed(error: TrustSourceError | OutOfMemoryError) -> i32 {
  match move error {
    OutOfMemoryError {} => { return -903 }
    _ => { return -904 }
  }
}

pub fn main() -> i32 {
  return run Effect.catchAll(program(), startupFailed)
}`

/** Full target-neutral behavior and structured-cancellation matrix for the native corpus. */
export const tlsConnectionAcceptanceSource = connectionSource(true)

/** One authenticated exchange, application I/O, and terminal release through LLVM-to-Wasm. */
export const tlsConnectionWasmSource = connectionSource(false)
