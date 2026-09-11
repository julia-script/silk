import { readFileSync } from 'node:fs'

const fixture = (name: string): Uint8Array =>
  readFileSync(new URL(`../fixtures/tls-client/${name}`, import.meta.url))
const silkBytes = (bytes: Uint8Array): string => `[${[...bytes].join(', ')}]`

const rootPem = fixture('keys/root-cert.pem')
const clientHello = fixture('captures/rsa-x25519-client-hello.bin')
const serverFlight = fixture('captures/rsa-x25519-server-flight.bin')

export const tlsClientAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.effect {Effect}
import silk.https_identity {HttpsIdentity, IdentityError, OriginHost, ReferenceIdentity}
import silk.random {Random}
import silk.result {Result}
import silk.option {Option}
import silk.slice {Slice}
import silk.system_clock {SystemClock}
import silk.tls_client {
  AlpnConfig,
  Client,
  ClientConfig,
  ClientLimits,
  ClientOperation,
  Demand,
  NamedGroup,
  Progress,
  ProtocolReason,
  TlsError,
}
import silk.trust_anchor {TrustAnchor}
import silk.tls_record {CipherSuite}
import silk.trust_snapshot {SnapshotLimits, TrustLoadLimits, TrustSnapshot, TrustSourceError}
import silk.u8
import silk.usize
import silk.vector {Vector}

// Deterministic test double only. Production code must supply a CSPRNG implementation.
struct ScriptedRandom { filled: usize }

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

fn emptyTrust() -> TrustSnapshot {
  let made = TrustSnapshot.fromAnchors(
    Vector.make<TrustAnchor>(),
    SnapshotLimits {anchors: 0, encodedBytes: 0},
  )
  return match move made {
    Result<TrustSnapshot, TrustSourceError>.Success {value} => move value
    Result<TrustSnapshot, TrustSourceError>.Failure {error} => {
      let invalid = 1 / 0
      return emptyTrust()
    }
  }
}

effect fn rootTrust() -> TrustSnapshot ! OutOfMemoryError ? &mut Allocator {
  let pem: [u8; ${rootPem.length}] = ${silkBytes(rootPem)}
  let made = run TrustSnapshot.fromPem(&pem, TrustLoadLimits.defaults())
  return match move made {
    Result<TrustSnapshot, TrustSourceError>.Success {value} => move value
    Result<TrustSnapshot, TrustSourceError>.Failure {error} => {
      let invalid = 1 / 0
      return run rootTrust()
    }
  }
}

fn reference<'a>(bytes: &'a [u8]) -> ReferenceIdentity<'a> {
  let admitted = HttpsIdentity.reference(OriginHost<'a>.Dns {bytes: bytes})
  return match move admitted {
    Result<ReferenceIdentity<'a>, IdentityError>.Success {value} => value
    Result<ReferenceIdentity<'a>, IdentityError>.Failure {error} => {
      let invalid = 1 / 0
      return reference(bytes)
    }
  }
}

fn hasBytes(haystack: &[u8], needle: &[u8]) -> bool {
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

fn isNeedOutput(result: Result<Progress, TlsError>) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => value.demand == Demand.NeedOutput
    Result<Progress, TlsError>.Failure {error} => false
  }
}

fn isNeedInput(result: Result<Progress, TlsError>) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => value.demand == Demand.NeedInput
    Result<Progress, TlsError>.Failure {error} => false
  }
}

fn isPrematureWrite(result: Result<Progress, TlsError>) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => false
    Result<Progress, TlsError>.Failure {error} => match move error {
      TlsError.InvalidState {operation} => operation == ClientOperation.WritePlaintext
      _ => false
    }
  }
}

fn isTruncated(result: Result<Progress, TlsError>) -> bool {
  return match move result {
    Result<Progress, TlsError>.Success {value} => false
    Result<Progress, TlsError>.Failure {error} => match move error {
      TlsError.Truncated => true
      _ => false
    }
  }
}

fn invalidLimit(result: Result<Client, TlsError>) -> bool {
  return match move result {
    Result<Client, TlsError>.Success {value} => false
    Result<Client, TlsError>.Failure {error} => match move error {
      TlsError.LimitExceeded {kind, limit} => limit == usize.ONE
      _ => false
    }
  }
}

fn protocolFailureCode(reason: ProtocolReason) -> i32 {
  return match reason {
    ProtocolReason.Record => 40
    ProtocolReason.UnexpectedMessage => 41
    ProtocolReason.InvalidLength => 42
    ProtocolReason.Extension => 43
    ProtocolReason.ServerHello => 44
    ProtocolReason.HelloRetryRequest => 45
    ProtocolReason.CompatibilityCcs => 46
    ProtocolReason.CertificateRequest => 47
    ProtocolReason.Certificate => 48
    ProtocolReason.PostHandshake => 49
    ProtocolReason.InternalState => 50
  }
}

fn feedFailureCode(error: TlsError) -> i32 {
  return match move error {
    TlsError.PeerAlert {code} => 30
    TlsError.BadRecordMac => 31
    TlsError.ProtocolViolation {reason} => protocolFailureCode(reason)
    TlsError.CertificateDecode {error: decodeError} => 51
    TlsError.CertificatePath {error: pathError} => 52
    TlsError.CertificateIdentity {error: identityError} => 53
    TlsError.CertificateVerify {error: verifyError} => 54
    TlsError.Finished => 55
    TlsError.UnsupportedProfile => 56
    TlsError.LimitExceeded {kind, limit} => 57
    TlsError.InvalidState {operation} => 58
    TlsError.EmptyBuffer => 59
    TlsError.HandshakeTruncated => 60
    TlsError.Truncated => 61
    TlsError.NoApplicationProtocol => 62
  }
}

effect fn cases() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut random = ScriptedRandom {filled: 0}
  let mixed = b"ExAmPlE.com"
  let config = ClientConfig {
    reference: reference(&mixed),
    alpn: AlpnConfig.defaults(),
    limits: ClientLimits.defaults(),
  }
  let trust = run rootTrust() |> Effect.provideMut<Allocator>(&mut allocator)
  let made = run Client.make(&config, move trust, SystemClock.make(1789146000, 123456789))
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut client = match move made {
    Result<Client, TlsError>.Success {value} => move value
    Result<Client, TlsError>.Failure {error} => { return 1 }
  }
  if random.filled != 64 { return 2 }
  let pending = client.pendingOutput()
  if pending.length < 6 || pending[0] != 22 || !hasBytes(pending, b"example.com") { return 3 }
  if hasBytes(pending, &mixed) { return 4 }
  let expectedHello: [u8; ${clientHello.length}] = ${silkBytes(clientHello)}
  if pending.length != ${clientHello.length} { return 10 }
  let mut helloIndex: usize = 5
  while helloIndex < pending.length {
    if pending[helloIndex] != expectedHello[helloIndex] { return 10 }
    helloIndex = helloIndex + usize.ONE
  }
  let second = pending[1]
  let total = pending.length
  drop pending
  if !isNeedOutput(client.ackWritten(usize.ONE)) { return 5 }
  let suffix = client.pendingOutput()
  if suffix.length + usize.ONE != total || suffix[0] != second { return 6 }
  drop suffix
  if !isNeedInput(client.ackWritten(total - usize.ONE)) { return 7 }
  if !isPrematureWrite(client.writePlaintext(b"blocked")) { return 8 }

  let flight: [u8; ${serverFlight.length}] = ${silkBytes(serverFlight)}
  let mut offset = usize.ZERO
  while offset < ${serverFlight.length} {
    let input = Slice.view<u8>(&flight, offset, ${serverFlight.length} - offset)
    let fed = run client.feedInput(input)
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    let accepted = match move fed {
      Result<Progress, TlsError>.Success {value} => value.consumed
      Result<Progress, TlsError>.Failure {error} => { return feedFailureCode(move error) }
    }
    if accepted == usize.ZERO { return 21 }
    offset = offset + accepted
    let mut bufferedSteps = usize.ZERO
    while bufferedSteps < 8 {
      let advanced = run client.progress()
        |> Effect.provideMut<Random>(&mut random)
        |> Effect.provideMut<Allocator>(&mut allocator)
      if let Result<Progress, TlsError>.Failure {error} = move advanced { return 22 }
      bufferedSteps = bufferedSteps + usize.ONE
    }
  }
  let mut steps = usize.ZERO
  while client.pendingOutput().length == 0 && steps < 8 {
    let advanced = run client.progress()
      |> Effect.provideMut<Random>(&mut random)
      |> Effect.provideMut<Allocator>(&mut allocator)
    if let Result<Progress, TlsError>.Failure {error} = move advanced { return 22 }
    steps = steps + usize.ONE
  }
  let finishedLength = client.pendingOutput().length
  if finishedLength == 0 { return 23 }
  let completed = client.ackWritten(finishedLength)
  let authenticated = match move completed {
    Result<Progress, TlsError>.Success {value} => value.demand == Demand.Authenticated
    Result<Progress, TlsError>.Failure {error} => false
  }
  if !authenticated { return 24 }
  let metadata = client.authentication()
  let validMetadata = match move metadata {
    Option.None => false
    Option.Some {value} => value.suite() == CipherSuite.ChaCha20Poly1305Sha256
      && value.group() == NamedGroup.X25519
      && value.anchorIndex() == 0
      && value.sanIndex() == 0
      && value.leafDer().length > 0
  }
  if !validMetadata { return 25 }

  let expectedPlaintext = b"coalesced authenticated plaintext"
  let mut plaintext: [u8; 40] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]
  let read = client.readPlaintext(&mut plaintext)
  let readLength = match move read {
    Result<Progress, TlsError>.Success {value} => value.written
    Result<Progress, TlsError>.Failure {error} => { return 26 }
  }
  if readLength != expectedPlaintext.length { return 27 }
  let mut plaintextIndex = usize.ZERO
  while plaintextIndex < readLength {
    if plaintext[plaintextIndex] != expectedPlaintext[plaintextIndex] { return 28 }
    plaintextIndex = plaintextIndex + usize.ONE
  }
  if !isNeedOutput(client.closeWrite()) { return 29 }
  let closeLength = client.pendingOutput().length
  if closeLength == 0 || !isNeedInput(client.ackWritten(closeLength)) { return 63 }
  if !isTruncated(client.endInput()) { return 64 }
  if !isTruncated(client.ackWritten(usize.ZERO)) { return 65 }

  let mut narrow = ClientLimits.defaults()
  narrow.handshakeBodyBytes = usize.ONE
  let invalidConfig = ClientConfig {
    reference: reference(&mixed),
    alpn: AlpnConfig.defaults(),
    limits: narrow,
  }
  let before = random.filled
  let rejected = run Client.make(
    &invalidConfig,
    emptyTrust(),
    SystemClock.make(1704067200, 999999999),
  )
    |> Effect.provideMut<Random>(&mut random)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if !invalidLimit(move rejected) || random.filled != before { return 9 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 98 }

pub fn main() -> i32 { return run Effect.catchAll(cases(), recover) }
`
