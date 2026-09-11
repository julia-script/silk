import { readFileSync } from 'node:fs'

interface RecordFixture {
  readonly name: string
  readonly suite: string
  readonly trafficSecret: string
  readonly contentType: string
  readonly content: string
  readonly wire: string
}

const fixtureFile = JSON.parse(
  readFileSync(
    new URL(
      '../../../../openspec/changes/implement-tls13-record-driving/fixtures.json',
      import.meta.url,
    ),
    'utf8',
  ),
) as { readonly records: ReadonlyArray<RecordFixture> }

const fixture = (name: string): RecordFixture => {
  const found = fixtureFile.records.find((candidate) => candidate.name === name)
  if (found === undefined) throw new Error(`Missing TLS record fixture ${name}`)
  return found
}

const bytes = (hex: string): string => [...Buffer.from(hex, 'hex')].join(', ')
const array = (hex: string): string => `[${bytes(hex)}]`

const rfc = fixture('rfc8448-client-finished')
const aes256 = fixture('independent-aes256-application')
const chacha = fixture('independent-chacha-alert')
const padded = fixture('independent-aes128-padded-application')
const sequenceOne = fixture('independent-aes128-sequence-one')
const admittedHeaderTamper = `1703030034${rfc.wire.slice(10, -2)}`

const dependencies = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.bytes { Bytes }
import silk.effect { Effect }
import silk.option { Option }
import silk.result { Result }
import silk.slice { Slice }`

const body = `
fn same(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index: usize = 0
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + 1
  }
  return true
}

fn queued(result: Result<OutputProgress, RecordError>, accepted: usize, demand: OutputDemand) -> bool {
  return match move result {
    Result<OutputProgress, RecordError>.Success {value} => value.accepted == accepted && value.demand == demand
    Result<OutputProgress, RecordError>.Failure {error} => false
  }
}

fn fed(result: Result<InputProgress, RecordError>, consumed: usize, demand: InputDemand) -> bool {
  return match move result {
    Result<InputProgress, RecordError>.Success {value} => value.consumed == consumed && value.demand == demand
    Result<InputProgress, RecordError>.Failure {error} => false
  }
}

fn unitError(result: Result<(), RecordError>) -> i32 {
  return match move result {
    Result<(), RecordError>.Success {value} => 0
    Result<(), RecordError>.Failure {error} => match move error {
      RecordError.InvalidSecretLength {requested, expected} => 1
      RecordError.InvalidState => 2
      RecordError.RecordOverflow {requested, maximum} => 3
      RecordError.InvalidContent => 4
      RecordError.AuthenticationFailed => 5
      RecordError.InvalidAcknowledgment {requested, pending} => 6
      RecordError.KeyUsageExhausted => 7
    }
  }
}

fn queueError(result: Result<OutputProgress, RecordError>) -> i32 {
  return match move result {
    Result<OutputProgress, RecordError>.Success {value} => 0
    Result<OutputProgress, RecordError>.Failure {error} => match move error {
      RecordError.InvalidSecretLength {requested, expected} => 1
      RecordError.InvalidState => 2
      RecordError.RecordOverflow {requested, maximum} => 3
      RecordError.InvalidContent => 4
      RecordError.AuthenticationFailed => 5
      RecordError.InvalidAcknowledgment {requested, pending} => 6
      RecordError.KeyUsageExhausted => 7
    }
  }
}

fn inputError(result: Result<InputProgress, RecordError>) -> i32 {
  return match move result {
    Result<InputProgress, RecordError>.Success {value} => 0
    Result<InputProgress, RecordError>.Failure {error} => match move error {
      RecordError.InvalidSecretLength {requested, expected} => 1
      RecordError.InvalidState => 2
      RecordError.RecordOverflow {requested, maximum} => 3
      RecordError.InvalidContent => 4
      RecordError.AuthenticationFailed => 5
      RecordError.InvalidAcknowledgment {requested, pending} => 6
      RecordError.KeyUsageExhausted => 7
    }
  }
}

fn recordIs<'a>(receiver: &'a TlsRecordReceiver, contentType: ContentType, expected: &[u8]) -> bool {
  let record = TlsRecordReceiver.record(receiver)
  return match move record {
    Option<RecordView<'a>>.None => false
    Option<RecordView<'a>>.Some {value} => value.contentType == contentType && same(value.content, expected)
  }
}

fn recordAbsent<'a>(receiver: &'a TlsRecordReceiver) -> bool {
  let record = TlsRecordReceiver.record(receiver)
  return match move record {
    Option<RecordView<'a>>.None => true
    Option<RecordView<'a>>.Some {value} => false
  }
}

effect fn fixtureRecord(
  suite: CipherSuite,
  secret: &[u8],
  contentType: ContentType,
  content: &[u8],
  expected: &[u8],
) -> bool ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let created = run (TlsRecordSender.make(suite, secret) |> Effect.provideMut(&mut allocator))
  let mut sender = match move created {
    Result<TlsRecordSender, RecordError>.Failure {error} => { return false }
    Result<TlsRecordSender, RecordError>.Success {value} => move value
  }
  if !queued(TlsRecordSender.queueRecord(&mut sender, contentType, content), content.length, OutputDemand.RecordQueued) {
    return false
  }
  if !same(TlsRecordSender.pendingOutput(&sender), expected) { return false }
  let pending = expected.length
  if !queued(TlsRecordSender.queueRecord(&mut sender, contentType, content), 0, OutputDemand.NeedOutput) {
    return false
  }
  if unitError(TlsRecordSender.ackWritten(&mut sender, 0)) != 0 { return false }
  if !same(TlsRecordSender.pendingOutput(&sender), expected) { return false }
  if unitError(TlsRecordSender.ackWritten(&mut sender, pending + 1)) != 6 { return false }
  if !same(TlsRecordSender.pendingOutput(&sender), expected) { return false }
  if unitError(TlsRecordSender.ackWritten(&mut sender, 5)) != 0 { return false }
  if !same(TlsRecordSender.pendingOutput(&sender), Slice.view<u8>(expected, 5, pending - 5)) {
    return false
  }
  if unitError(TlsRecordSender.ackWritten(&mut sender, pending - 5)) != 0 { return false }
  if TlsRecordSender.pendingOutput(&sender).length != 0 { return false }

  let received = run (TlsRecordReceiver.make(suite, secret) |> Effect.provideMut(&mut allocator))
  let mut receiver = match move received {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, expected), expected.length, InputDemand.RecordReady) {
    return false
  }
  if !recordIs(&receiver, contentType, content) { return false }
  return unitError(TlsRecordReceiver.consumeRecord(&mut receiver)) == 0
}

effect fn fragmentedRfc() -> bool ! OutOfMemoryError {
  let secret: [u8; 32] = ${array(rfc.trafficSecret)}
  let content: [u8; 36] = ${array(rfc.content)}
  let wire: [u8; 58] = ${array(rfc.wire)}
  let mut allocator = Allocator.systemAllocatorProvider()
  let created = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut receiver = match move created {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  let mut index: usize = 0
  while index < 58 {
    if index + 1 == 58 {
      if !fed(TlsRecordReceiver.feedInput(&mut receiver, Slice.view<u8>(&wire, index, 1)), 1, InputDemand.RecordReady) {
        return false
      }
    } else {
      if !fed(TlsRecordReceiver.feedInput(&mut receiver, Slice.view<u8>(&wire, index, 1)), 1, InputDemand.NeedInput) {
        return false
      }
    }
    index = index + 1
  }
  let empty: [u8; 0] = []
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, &empty), 0, InputDemand.NeedRecordConsumption) {
    return false
  }
  if !recordIs(&receiver, ContentType.Handshake, &content) { return false }
  if unitError(TlsRecordReceiver.consumeRecord(&mut receiver)) != 0 { return false }
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, &empty), 0, InputDemand.NeedInput) {
    return false
  }
  return true
}

effect fn coalescingAndFailure() -> bool ! OutOfMemoryError {
  let secret: [u8; 32] = ${array(rfc.trafficSecret)}
  let content: [u8; 36] = ${array(rfc.content)}
  let coalesced: [u8; 59] = [${bytes(rfc.wire)}, 170]
  let mut allocator = Allocator.systemAllocatorProvider()
  let first = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut receiver = match move first {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, &coalesced), 58, InputDemand.RecordReady) {
    return false
  }
  if !recordIs(&receiver, ContentType.Handshake, &content) { return false }

  let mut tagTampered: [u8; 58] = ${array(rfc.wire)}
  tagTampered[57] = tagTampered[57] ^ 1
  let second = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut failed = match move second {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  if inputError(TlsRecordReceiver.feedInput(&mut failed, &tagTampered)) != 5 { return false }
  if !recordAbsent(&failed) { return false }
  let empty: [u8; 0] = []
  if inputError(TlsRecordReceiver.feedInput(&mut failed, &empty)) != 2 { return false }

  let third = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut headerFailed = match move third {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  let badHeader: [u8; 5] = [22, 3, 3, 0, 53]
  if inputError(TlsRecordReceiver.feedInput(&mut headerFailed, &badHeader)) != 4 { return false }
  if inputError(TlsRecordReceiver.feedInput(&mut headerFailed, &empty)) != 2 { return false }

  let fourth = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut ciphertextFailed = match move fourth {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  let mut ciphertextTampered: [u8; 58] = ${array(rfc.wire)}
  ciphertextTampered[10] = ciphertextTampered[10] ^ 1
  if inputError(TlsRecordReceiver.feedInput(&mut ciphertextFailed, &ciphertextTampered)) != 5 {
    return false
  }

  let fifth = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut aadFailed = match move fifth {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  let admittedAadTamper: [u8; 57] = ${array(admittedHeaderTamper)}
  return inputError(TlsRecordReceiver.feedInput(&mut aadFailed, &admittedAadTamper)) == 5
}

effect fn plaintextAndBounds() -> bool ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut sender = run (TlsRecordSender.makePlaintext() |> Effect.provideMut(&mut allocator))
  let empty: [u8; 0] = []
  let alert: [u8; 2] = [1, 0]
  if queueError(TlsRecordSender.queueRecord(&mut sender, ContentType.Handshake, &empty)) != 4 {
    return false
  }
  if queueError(TlsRecordSender.queueRecord(&mut sender, ContentType.Alert, Slice.view<u8>(&alert, 0, 1))) != 4 {
    return false
  }
  if !queued(TlsRecordSender.queueRecord(&mut sender, ContentType.ApplicationData, &empty), 0, OutputDemand.RecordQueued) {
    return false
  }
  let emptyWire: [u8; 5] = [23, 3, 3, 0, 0]
  if !same(TlsRecordSender.pendingOutput(&sender), &emptyWire) { return false }
  if unitError(TlsRecordSender.ackWritten(&mut sender, 5)) != 0 { return false }

  let handshake: [u8; 2] = [1, 2]
  let handshakeWire: [u8; 7] = [22, 3, 3, 0, 2, 1, 2]
  if !queued(TlsRecordSender.queueRecord(&mut sender, ContentType.Handshake, &handshake), 2, OutputDemand.RecordQueued) {
    return false
  }
  if !same(TlsRecordSender.pendingOutput(&sender), &handshakeWire) { return false }

  let mut receiver = run (TlsRecordReceiver.makePlaintext() |> Effect.provideMut(&mut allocator))
  let toleratedHandshakeWire: [u8; 7] = [22, 3, 1, 0, 2, 1, 2]
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, &toleratedHandshakeWire), 7, InputDemand.RecordReady) {
    return false
  }
  if !recordIs(&receiver, ContentType.Handshake, &handshake) { return false }
  if unitError(TlsRecordReceiver.consumeRecord(&mut receiver)) != 0 { return false }

  let mut oversizedReceiver = run (TlsRecordReceiver.makePlaintext() |> Effect.provideMut(&mut allocator))
  let oversized: [u8; 5] = [23, 3, 3, 65, 1]
  if inputError(TlsRecordReceiver.feedInput(&mut oversizedReceiver, &oversized)) != 3 { return false }
  return inputError(TlsRecordReceiver.feedInput(&mut oversizedReceiver, &empty)) == 2
}

effect fn authenticatedPadding() -> bool ! OutOfMemoryError {
  let secret: [u8; 32] = ${array(padded.trafficSecret)}
  let content: [u8; 3] = ${array(padded.content)}
  let wire: [u8; 29] = ${array(padded.wire)}
  let mut allocator = Allocator.systemAllocatorProvider()
  let created = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut receiver = match move created {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, &wire), 29, InputDemand.RecordReady) {
    return false
  }
  if !recordIs(&receiver, ContentType.ApplicationData, &content) { return false }
  return unitError(TlsRecordReceiver.consumeRecord(&mut receiver)) == 0
}

effect fn sequenceOneAfterPartialAck() -> bool ! OutOfMemoryError {
  let secret: [u8; 32] = ${array(sequenceOne.trafficSecret)}
  let firstContent: [u8; 1] = [8]
  let secondContent: [u8; 1] = ${array(sequenceOne.content)}
  let secondWire: [u8; 23] = ${array(sequenceOne.wire)}
  let mut allocator = Allocator.systemAllocatorProvider()
  let madeSender = run (TlsRecordSender.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut sender = match move madeSender {
    Result<TlsRecordSender, RecordError>.Failure {error} => { return false }
    Result<TlsRecordSender, RecordError>.Success {value} => move value
  }
  let madeReceiver = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut receiver = match move madeReceiver {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }

  if !queued(TlsRecordSender.queueRecord(&mut sender, ContentType.ApplicationData, &firstContent), 1, OutputDemand.RecordQueued) {
    return false
  }
  let firstWire = TlsRecordSender.pendingOutput(&sender)
  let firstLength = firstWire.length
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, firstWire), firstLength, InputDemand.RecordReady) {
    return false
  }
  if !recordIs(&receiver, ContentType.ApplicationData, &firstContent) { return false }
  if unitError(TlsRecordReceiver.consumeRecord(&mut receiver)) != 0 { return false }

  if unitError(TlsRecordSender.ackWritten(&mut sender, 1)) != 0 { return false }
  let afterOne = TlsRecordSender.pendingOutput(&sender)
  if afterOne.length != firstLength - 1 || afterOne[0] != 3 { return false }
  if unitError(TlsRecordSender.ackWritten(&mut sender, firstLength - 1)) != 0 { return false }

  if !queued(TlsRecordSender.queueRecord(&mut sender, ContentType.ApplicationData, &secondContent), 1, OutputDemand.RecordQueued) {
    return false
  }
  if !same(TlsRecordSender.pendingOutput(&sender), &secondWire) { return false }
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, &secondWire), 23, InputDemand.RecordReady) {
    return false
  }
  if !recordIs(&receiver, ContentType.ApplicationData, &secondContent) { return false }
  return unitError(TlsRecordReceiver.consumeRecord(&mut receiver)) == 0
}

effect fn maximumContent() -> bool ! OutOfMemoryError {
  let secret: [u8; 32] = ${array(rfc.trafficSecret)}
  let mut allocator = Allocator.systemAllocatorProvider()
  let storage = run (Bytes.zeroed(16385) |> Effect.provideMut(&mut allocator))
  let created = run (TlsRecordSender.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut sender = match move created {
    Result<TlsRecordSender, RecordError>.Failure {error} => { return false }
    Result<TlsRecordSender, RecordError>.Success {value} => move value
  }
  if !queued(TlsRecordSender.queueRecord(&mut sender, ContentType.ApplicationData, Bytes.asSlice(&storage)), 16384, OutputDemand.RecordQueued) {
    return false
  }
  return TlsRecordSender.pendingOutput(&sender).length == 16406
}

effect fn constructorBounds() -> bool ! OutOfMemoryError {
  let wrong: [u8; 31] = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  ]
  let mut allocator = Allocator.systemAllocatorProvider()
  let created = run (TlsRecordSender.make(CipherSuite.Aes128GcmSha256, &wrong)
    |> Effect.provideMut(&mut allocator))
  return match move created {
    Result<TlsRecordSender, RecordError>.Success {value} => false
    Result<TlsRecordSender, RecordError>.Failure {error} => match move error {
      RecordError.InvalidSecretLength {requested, expected} => requested == 31 && expected == 32
      RecordError.InvalidState => false
      RecordError.RecordOverflow {requested, maximum} => false
      RecordError.InvalidContent => false
      RecordError.AuthenticationFailed => false
      RecordError.InvalidAcknowledgment {requested, pending} => false
      RecordError.KeyUsageExhausted => false
    }
  }
}

effect fn publicCases() -> i32 ! OutOfMemoryError {
  let rfcSecret: [u8; 32] = ${array(rfc.trafficSecret)}
  let rfcContent: [u8; 36] = ${array(rfc.content)}
  let rfcWire: [u8; 58] = ${array(rfc.wire)}
  if !(run fixtureRecord(CipherSuite.Aes128GcmSha256, &rfcSecret, ContentType.Handshake, &rfcContent, &rfcWire)) {
    return 1
  }
  let aesSecret: [u8; 48] = ${array(aes256.trafficSecret)}
  let aesContent: [u8; 19] = ${array(aes256.content)}
  let aesWire: [u8; 41] = ${array(aes256.wire)}
  if !(run fixtureRecord(CipherSuite.Aes256GcmSha384, &aesSecret, ContentType.ApplicationData, &aesContent, &aesWire)) {
    return 2
  }
  let chachaSecret: [u8; 32] = ${array(chacha.trafficSecret)}
  let chachaContent: [u8; 2] = ${array(chacha.content)}
  let chachaWire: [u8; 24] = ${array(chacha.wire)}
  if !(run fixtureRecord(CipherSuite.ChaCha20Poly1305Sha256, &chachaSecret, ContentType.Alert, &chachaContent, &chachaWire)) {
    return 3
  }
  if !(run fragmentedRfc()) { return 4 }
  if !(run coalescingAndFailure()) { return 5 }
  if !(run plaintextAndBounds()) { return 6 }
  if !(run authenticatedPadding()) { return 7 }
  if !(run sequenceOneAfterPartialAck()) { return 8 }
  if !(run maximumContent()) { return 9 }
  if !(run constructorBounds()) { return 10 }
  return 42
}

effect fn allocationFailure(error: OutOfMemoryError) -> i32 { return 99 }
`

const publicImports = `import silk.tls_record {
  CipherSuite,
  ContentType,
  InputDemand,
  InputProgress,
  OutputDemand,
  OutputProgress,
  RecordError,
  RecordView,
  TlsRecordReceiver,
  TlsRecordSender,
}`

export const tlsRecordAcceptanceSource = `${publicImports}
${dependencies}
${body}
pub fn main() -> i32 { return run Effect.catchAll(publicCases(), allocationFailure) }`

const privateCases = `
fn invalidInnerCases() -> bool {
  let allZero: [u8; 2] = [0, 0]
  let unknown: [u8; 1] = [99]
  let protectedCcs: [u8; 1] = [20]
  let first = parseInner(&allZero)
  let second = parseInner(&unknown)
  let third = parseInner(&protectedCcs)
  return parsedError(move first) == 4 && parsedError(move second) == 4 && parsedError(move third) == 4
}

fn parsedError(result: Result<ParsedRecord, RecordError>) -> i32 {
  return match move result {
    Result<ParsedRecord, RecordError>.Success {value} => 0
    Result<ParsedRecord, RecordError>.Failure {error} => match move error {
      RecordError.InvalidSecretLength {requested, expected} => 1
      RecordError.InvalidState => 2
      RecordError.RecordOverflow {requested, maximum} => 3
      RecordError.InvalidContent => 4
      RecordError.AuthenticationFailed => 5
      RecordError.InvalidAcknowledgment {requested, pending} => 6
      RecordError.KeyUsageExhausted => 7
    }
  }
}

effect fn nearCap() -> bool ! OutOfMemoryError {
  let secret: [u8; 32] = ${array(rfc.trafficSecret)}
  let content: [u8; 2] = [1, 2]
  let mut allocator = Allocator.systemAllocatorProvider()
  let madeSender = run (TlsRecordSender.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut sender = match move madeSender {
    Result<TlsRecordSender, RecordError>.Failure {error} => { return false }
    Result<TlsRecordSender, RecordError>.Success {value} => move value
  }
  let madeReceiver = run (TlsRecordReceiver.make(CipherSuite.Aes128GcmSha256, &secret)
    |> Effect.provideMut(&mut allocator))
  let mut receiver = match move madeReceiver {
    Result<TlsRecordReceiver, RecordError>.Failure {error} => { return false }
    Result<TlsRecordReceiver, RecordError>.Success {value} => move value
  }
  sender.sequence = RECORDS_PER_EPOCH - 1
  receiver.sequence = RECORDS_PER_EPOCH - 1
  if !queued(TlsRecordSender.queueRecord(&mut sender, ContentType.ApplicationData, &content), 2, OutputDemand.RecordQueued) {
    return false
  }
  let wire = TlsRecordSender.pendingOutput(&sender)
  let wireLength = wire.length
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, wire), wireLength, InputDemand.RecordReady) {
    return false
  }
  if !recordIs(&receiver, ContentType.ApplicationData, &content) { return false }
  if unitError(TlsRecordReceiver.consumeRecord(&mut receiver)) != 0 { return false }
  if unitError(TlsRecordSender.ackWritten(&mut sender, wireLength)) != 0 { return false }
  if queueError(TlsRecordSender.queueRecord(&mut sender, ContentType.ApplicationData, &content)) != 7 {
    return false
  }
  let header: [u8; 5] = [23, 3, 3, 0, 19]
  if !fed(TlsRecordReceiver.feedInput(&mut receiver, &header), 5, InputDemand.NeedInput) {
    return false
  }
  let body: [u8; 19] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  return inputError(TlsRecordReceiver.feedInput(&mut receiver, &body)) == 7
}

effect fn nativeCases() -> i32 ! OutOfMemoryError {
  let public = run publicCases()
  if public != 42 { return public }
  if !invalidInnerCases() { return 20 }
  if !(run nearCap()) { return 21 }
  return 42
}
`

export const tlsRecordNativeAcceptanceSource = (
  implementation: string,
): string => `${implementation}
import silk.bytes { Bytes }
import silk.effect { Effect }
${body}
${privateCases}
pub fn main() -> i32 { return run Effect.catchAll(nativeCases(), allocationFailure) }`

export const tlsRecordWasmSource = `${publicImports}
${dependencies}
fn ok(result: Result<OutputProgress, RecordError>) -> bool {
  return match move result {
    Result<OutputProgress, RecordError>.Success {value} => value.accepted == 0 && value.demand == OutputDemand.RecordQueued
    Result<OutputProgress, RecordError>.Failure {error} => false
  }
}
effect fn witness() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut sender = run (TlsRecordSender.makePlaintext() |> Effect.provideMut(&mut allocator))
  let empty: [u8; 0] = []
  if !ok(TlsRecordSender.queueRecord(&mut sender, ContentType.ApplicationData, &empty)) { return 1 }
  let expected: [u8; 5] = [23, 3, 3, 0, 0]
  let output = TlsRecordSender.pendingOutput(&sender)
  if output.length != 5 { return 2 }
  let mut index: usize = 0
  while index < 5 { if output[index] != expected[index] { return 3 } index = index + 1 }
  return 0
}
effect fn failed(error: OutOfMemoryError) -> i32 { return 99 }
pub fn main() -> i32 { return run Effect.catchAll(witness(), failed) }`
