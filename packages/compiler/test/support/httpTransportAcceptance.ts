import {
  tlsClientRsaRootPem,
  tlsClientRsaServerFlight,
  tlsClientRsaApplicationRecord,
} from './tlsClientAcceptance.js'

const silkBytes = (bytes: Uint8Array): string =>
  `b"${[...bytes].map((byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`

// Reuse the established authenticated peer transcript. Only its post-authentication application
// record changes: the TLS handshake and certificate evidence remain the existing TLS witness.
const plaintext = Buffer.from('HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nWiki')
const application = tlsClientRsaApplicationRecord(plaintext)
let offset = 0
let finalRecord = 0
while (offset < tlsClientRsaServerFlight.length) {
  finalRecord = offset
  const high = tlsClientRsaServerFlight[offset + 3]
  const low = tlsClientRsaServerFlight[offset + 4]
  if (high === undefined || low === undefined) throw new Error('Truncated TLS HTTP fixture record')
  offset += 5 + high * 256 + low
}
if (offset !== tlsClientRsaServerFlight.length)
  throw new Error('Invalid TLS HTTP fixture record boundary')
const flight = Buffer.concat([tlsClientRsaServerFlight.subarray(0, finalRecord), application])

/** One authenticated HTTP exchange and failed-trust admission audit using the established TLS peer. */
export const httpTransportAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.byte_duplex {ByteDuplex, ByteIoError, ReadTransfer}
import silk.bytes {Bytes}
import silk.effect {Effect}
import silk.http {Method, Version, Header}
import silk.http_headers {Headers, Limits as ValueLimits}
import silk.http_origin {Origin}
import silk.http_request {PreparedRequest, HeaderPolicy, BodyMode, RequestError}
import silk.http_request as Request
import silk.http_client {Connection, ConnectionHandler, Exchange, Limits, ClientError, RequestOptions, ContinuePolicy}
import silk.http_client as Client
import silk.http_transport {HttpTransport, TransportError}
import silk.http_transport as Transport
import silk.memory_byte_duplex {MemoryByteDuplex, MemoryReadEvent, MemoryWriteEvent, MemoryWriteAction}
import silk.monotonic_clock {MonotonicClock}
import silk.random {Random}
import silk.option {Option}
import silk.result {Result}
import silk.system_clock {Instant, SystemClock}
import silk.uri {Uri}
import silk.u64
import silk.usize
import silk.vector {Vector}
import silk.https_identity {HttpsIdentity, IdentityError, OriginHost, ReferenceIdentity}
import silk.tls_client {AlpnConfig, ClientConfig, ClientLimits, TlsError}
import silk.tls_connection as Tls {OwnedConnection, ConnectionError, ConnectionOptions}
import silk.trust_snapshot {TrustSnapshot, TrustLoadLimits, TrustSourceError, SnapshotLimits}
import silk.trust_anchor {TrustAnchor}
import silk.shared {Shared}
struct FixedRandom {filled: usize}
impl Random for FixedRandom {
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
fn valueLimits() -> ValueLimits { let selected = Limits.defaults() return selected.head.values }
fn limits() -> Limits {
 let mut selected = Limits.defaults()
 selected.readCapacity = 128
 selected.writeCapacity = 128
 selected.maxRequests = 1
 return selected
}
effect fn providerFor(inputBytes: &[u8], writeCount: usize) -> MemoryByteDuplex
! OutOfMemoryError ? &mut Allocator {
 let input = run Bytes.copy(inputBytes)
 let mut reads = Vector.make<MemoryReadEvent>()
 run Vector.append(&mut reads, MemoryReadEvent.Data {readyAt: SystemClock.make(0, 0), bytes: move input})
 run Vector.append(&mut reads, MemoryReadEvent.End {readyAt: SystemClock.make(0, 0)})
 let mut writes = Vector.make<MemoryWriteEvent>()
 let mut index = usize.ZERO
 while index < writeCount {
  run Vector.append(&mut writes, MemoryWriteEvent {readyAt: SystemClock.make(0, 0), action: MemoryWriteAction.Accept {count: 65535}})
  index = index + usize.ONE
 }
 return run MemoryByteDuplex.make(move reads, move writes, 4096, 128, Option.none<i32>())
}
struct TlsClock {}
impl MonotonicClock for TlsClock {
 effect fn now(self: &mut Self) -> Instant { return SystemClock.make(0, 0) }
 effect fn getResolution(self: &mut Self) -> u64 { return u64.toU64(1) }
 effect fn waitUntil(self: &mut Self, when: Instant) -> () { drop when return () }
 effect fn waitFor(self: &mut Self, duration: u64) -> () { drop duration return () }
}
struct WallClock {}
impl SystemClock for WallClock {
 effect fn now(self: &mut Self) -> Instant { return SystemClock.make(1789156800, 123456789) }
 effect fn getResolution(self: &mut Self) -> u64 { return u64.toU64(1) }
}
fn reference<'a>(bytes: &'a [u8]) -> ReferenceIdentity<'a> {
 return match move HttpsIdentity.reference(OriginHost<'a>.Dns {bytes: bytes}) {
  Result<ReferenceIdentity<'a>, IdentityError>.Success {value} => value
  Result<ReferenceIdentity<'a>, IdentityError>.Failure {error} => { let invalid = 1 / 0 return reference(bytes) }
 }
}
struct TlsTransport {inner: OwnedConnection<MemoryByteDuplex>}
impl HttpTransport for TlsTransport {
 effect fn readSomeRaw(self: &mut Self, output: &mut [u8], deadline: Option<Instant>) -> ReadTransfer
 ! TransportError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  let mut adapter = Transport.secured(&mut self.inner)
  return run HttpTransport.readSome(move output, move deadline) |> Effect.provideMut<HttpTransport>(&mut adapter)
 }
 effect fn writeSomeRaw(self: &mut Self, input: &[u8], deadline: Option<Instant>) -> usize
 ! TransportError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  let mut adapter = Transport.secured(&mut self.inner)
  return run HttpTransport.writeSome(input, move deadline) |> Effect.provideMut<HttpTransport>(&mut adapter)
 }
 effect fn flush(self: &mut Self, deadline: Option<Instant>) -> ()
 ! TransportError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  let mut adapter = Transport.secured(&mut self.inner)
  return run HttpTransport.flush(move deadline) |> Effect.provideMut<HttpTransport>(&mut adapter)
 }
 effect fn close(self: &mut Self) -> () ! TransportError {
  let mut adapter = Transport.secured(&mut self.inner)
  return run HttpTransport.close() |> Effect.provideMut<HttpTransport>(&mut adapter)
 }
}
struct Handler {}
impl ConnectionHandler<TlsTransport, i32, ClientError | RequestError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random> for Handler {
 effect<'call> fn handle<'call>(handler: Self, connection: &'call mut Connection<TlsTransport>) -> i32
 ! ClientError | RequestError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
  let uri = match move Uri.parse("https://example.com/") { Result.Failure {error} => { drop error return 31 } Result.Success {value} => value }
  let entries: [Header<'static>; 0] = []
  let headers = match move Headers.make(&entries, valueLimits()) { Result.Failure {error} => { drop error return 32 } Result.Success {value} => value }
  let policy = HeaderPolicy.defaults()
  let request = run Request.fromUri(&uri, Version.Http11, Method.get(), &headers, &policy, BodyMode.Empty, false, valueLimits(), 1024, 512)
  let options = RequestOptions {deadline: Option.none<Instant>(), continuePolicy: ContinuePolicy.Disabled}
  return run Client.withExchange(&mut connection.*, &request, move options, exchange)
 }
}
effect<'call> fn exchange<'call, 'exchange: 'call>(exchangeValue: &'call mut Exchange<'exchange, TlsTransport>) -> i32
! ClientError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock | &mut Random {
 run Client.send(&mut exchangeValue.*)
 let entries: [Header<'static>; 0] = []
 let trailers = match move Headers.make(&entries, valueLimits()) { Result.Failure {error} => { drop error return 33 } Result.Success {value} => value }
 run Client.finishRequest(&mut exchangeValue.*, &trailers)
 let status = run Client.receive(&mut exchangeValue.*)
 if status != 200 { return 34 }
 let mut bytes: [u8; 4] = [0, 0, 0, 0]
 let count = run Client.readSome(&mut exchangeValue.*, &mut bytes)
 if count != 4 || bytes[0] != 87 || bytes[3] != 105 { return 35 }
 run Client.finishResponse(&mut exchangeValue.*)
 return 0
}
struct FailureAudit {postHello: bool\nclosed: bool}
struct FailureTransport {inner: MemoryByteDuplex\naudit: Shared<FailureAudit>}
fn markPostHello(audit: &mut FailureAudit) -> () { audit.postHello = true return () }
fn markClosed(audit: &mut FailureAudit) -> () { audit.closed = true return () }
impl ByteDuplex for FailureTransport {
 effect fn readSomeRaw(self: &mut Self, output: &mut [u8], deadline: Option<Instant>) -> ReadTransfer
 ! ByteIoError ? &mut MonotonicClock {
  return run ByteDuplex.readSome(move output, move deadline) |> Effect.provideMut<ByteDuplex>(&mut self.inner)
 }
 effect fn writeSomeRaw(self: &mut Self, input: &[u8], deadline: Option<Instant>) -> usize
 ! ByteIoError ? &mut MonotonicClock {
  if input.length > usize.ZERO && input[0] == 23 { Shared.withMut(&self.audit, markPostHello) }
  return run ByteDuplex.writeSome(input, move deadline) |> Effect.provideMut<ByteDuplex>(&mut self.inner)
 }
 effect fn flushRaw(self: &mut Self, deadline: Option<Instant>) -> ()
 ! ByteIoError ? &mut MonotonicClock {
  return run ByteDuplex.flush(move deadline) |> Effect.provideMut<ByteDuplex>(&mut self.inner)
 }
 effect fn shutdownWriteRaw(self: &mut Self, deadline: Option<Instant>) -> ()
 ! ByteIoError ? &mut MonotonicClock {
  return run ByteDuplex.shutdownWrite(move deadline) |> Effect.provideMut<ByteDuplex>(&mut self.inner)
 }
 effect fn closeRaw(self: &mut Self) -> () ! ByteIoError {
  Shared.withMut(&self.audit, markClosed)
  return run ByteDuplex.close() |> Effect.provideMut<ByteDuplex>(&mut self.inner)
 }
}
effect fn failedTrust() -> i32 ! OutOfMemoryError | TrustSourceError {
 let mut allocator = Allocator.systemAllocatorProvider()
 let audit = run Shared.make<FailureAudit>(FailureAudit {postHello: false, closed: false})
   |> Effect.provideMut<Allocator>(&mut allocator)
 let inner = run providerFor(${silkBytes(flight)}, 64) |> Effect.provideMut<Allocator>(&mut allocator)
 let transport = FailureTransport {inner: move inner, audit: Shared.clone(&audit)}
 let trust = match move TrustSnapshot.fromAnchors(Vector.make<TrustAnchor>(), SnapshotLimits.defaults()) {
  Result.Success {value} => move value
  Result.Failure {error} => { fail move error }
 }
 let host = b"example.com"
 let config = ClientConfig {reference: reference(&host), alpn: AlpnConfig.defaults(), limits: ClientLimits.defaults()}
 let mut wall = WallClock {}
 let mut clock = TlsClock {}
 let mut random = FixedRandom {filled: usize.ZERO}
 let attempted = run Effect.result(Tls.authenticateOwned<FailureTransport>(move transport, &config, move trust, ConnectionOptions.defaults()))
   |> Effect.provideMut<SystemClock>(&mut wall)
   |> Effect.provideMut<MonotonicClock>(&mut clock)
   |> Effect.provideMut<Random>(&mut random)
   |> Effect.provideMut<Allocator>(&mut allocator)
 match move attempted {
  Result<OwnedConnection<FailureTransport>, ConnectionError | OutOfMemoryError>.Success {value} => { drop value return 81 }
  Result<OwnedConnection<FailureTransport>, ConnectionError | OutOfMemoryError>.Failure {error} => match move error {
   ConnectionError cause => match move cause {
    ConnectionError.Tls {error: tlsError} => match move tlsError {
     TlsError.CertificatePath {error: pathError} => { drop pathError }
     _ => { return 83 }
    }
    _ => { return 84 }
   }
   OutOfMemoryError allocation => { return 85 }
  }
 }
 let correct = Shared.with(&audit, fn(value: &FailureAudit) -> bool { return value.closed && !value.postHello })
 if !correct { return 82 }
 return 0
}
effect fn authenticatedExchange() -> i32 ! ClientError | RequestError | ConnectionError | TrustSourceError | OutOfMemoryError {
 let mut allocator = Allocator.systemAllocatorProvider()
 let transport = run providerFor(${silkBytes(flight)}, 64) |> Effect.provideMut<Allocator>(&mut allocator)
 let trustResult = run TrustSnapshot.fromPem(${silkBytes(tlsClientRsaRootPem)}, TrustLoadLimits.defaults()) |> Effect.provideMut<Allocator>(&mut allocator)
 let trust = match move trustResult { Result.Success {value} => move value Result.Failure {error} => { fail move error } }
 let mut wall = WallClock {}
 let mut clock = TlsClock {}
 let mut random = FixedRandom {filled: usize.ZERO}
 let host = b"example.com"
 let config = ClientConfig {reference: reference(&host), alpn: AlpnConfig.defaults(), limits: ClientLimits.defaults()}
 let authenticated = run Tls.authenticateOwned<MemoryByteDuplex>(move transport, &config, move trust, ConnectionOptions.defaults())
   |> Effect.provideMut<SystemClock>(&mut wall)
   |> Effect.provideMut<Random>(&mut random)
   |> Effect.provideMut<MonotonicClock>(&mut clock)
   |> Effect.provideMut<Allocator>(&mut allocator)
 let uri = match move Uri.parse("https://example.com/") { Result.Failure {error} => { drop error return 36 } Result.Success {value} => value }
 let origin = match move Origin.fromUri(&uri) { Result.Failure {error} => { drop error return 37 } Result.Success {value} => value }
 return run Client.withOwned(TlsTransport {inner: move authenticated}, origin, Version.Http11, limits(), Option.none<Instant>(), Handler {})
   |> Effect.provideMut<Random>(&mut random)
   |> Effect.provideMut<MonotonicClock>(&mut clock)
   |> Effect.provideMut<Allocator>(&mut allocator)
}
effect fn allCases() -> i32 ! ClientError | RequestError | ConnectionError | TrustSourceError | OutOfMemoryError {
 let failed = run failedTrust()
 if failed != 0 { return failed }
 return run authenticatedExchange()
}
effect fn recover(error: ClientError | RequestError | ConnectionError | TrustSourceError | OutOfMemoryError) -> i32 { drop error return 51 }
pub fn main() -> i32 { return run Effect.catchAll(allCases(), recover) }
`

/** Pure native admission: unsupported targets and DNS deadlines are rejected before providers. */
export const httpNativeAdmissionAcceptanceSource = `import silk.http_client_native {Options, NativeClientError, preflight}
import silk.http_origin {Origin}
import silk.option {Option}
import silk.result {Result}
import silk.system_clock {Instant, SystemClock}
import silk.uri {Uri}
fn origin(text: string) -> Origin {
 let uri = match move Uri.parse(text) { Result.Success {value} => value Result.Failure {error} => { let invalid = 1 / 0 return origin(text) } }
 return match move Origin.fromUri(&uri) { Result.Success {value} => value Result.Failure {error} => { let invalid = 1 / 0 return origin(text) } }
}
pub fn main() -> i32 {
 let numeric = origin("http://127.0.0.1/")
 let dns = origin("https://example.com/")
 let mut options = Options.defaults()
 options.deadline = Option.some<Instant>(SystemClock.make(7, 0))
 match move preflight(&numeric, &options) {
  Result.Success {value} => { drop value }
  Result.Failure {error} => match move error {
   NativeClientError.UnsupportedTarget => { return 42 }
   _ => { return 1 }
  }
 }
 match move preflight(&dns, &options) {
  Result.Success {value} => { return 2 }
  Result.Failure {error} => match move error {
   NativeClientError.UnsupportedDeadline => {}
   _ => { return 3 }
  }
 }
 options.deadline = Option.none<Instant>()
 return match move preflight(&dns, &options) { Result.Success {value} => 42 Result.Failure {error} => 4 }
}`
