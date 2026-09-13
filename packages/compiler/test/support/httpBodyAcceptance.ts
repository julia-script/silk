/** One portable runtime program exercises strict bounded HTTP message framing. */
export const httpBodyAcceptanceSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.http { Header, Method }
import silk.http_body {
  Anomaly,
  BodyError,
  BodyLimitKind,
  BodyReason,
  CompletionKind,
  Decoder,
  Encoder,
  Framing,
  Limits,
  Progress,
  ProgressState,
  Selection,
  TrailerIterator,
  TrailerPolicy,
  selectResponse,
  selectRequest,
  validateOutgoingRequest,
}
import silk.http_head {
  Limits as HeadLimits,
  ParsedRequest,
  ParsedResponse,
  RequestHead,
  RequestParser,
  ResponseHead,
  ResponseParser,
  parseRequest,
  parseResponse,
}
import silk.http_headers { Headers, Limits as ValueLimits }
import silk.option { Option }
import silk.result { Result }
import silk.string { String }
import silk.u64
import silk.usize

fn valueLimits() -> ValueLimits {
  return ValueLimits {
    maxMethodBytes: 32,
    maxTargetBytes: 128,
    maxNameBytes: 64,
    maxValueBytes: 256,
    maxFields: 8,
    maxFieldBytes: 512,
    maxOwnedBytes: 1024,
  }
}

fn headLimits() -> HeadLimits {
  return HeadLimits {
    maxHeadBytes: 1024,
    maxStartLineBytes: 256,
    maxFieldLineBytes: 256,
    maxOwnedBytes: 1536,
    values: valueLimits(),
  }
}

fn bodyLimits() -> Limits {
  return Limits {
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

fn equal(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index = usize.ZERO
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

effect fn decoderFor(input: &[u8], limits: Limits) -> Option<Decoder> ! OutOfMemoryError ? &mut Allocator {
  let parsed = match move (run parseRequest(input, headLimits())) {
    Result.Failure {error} => { return Option.none<Decoder>() }
    Result.Success {value} => move value
  }
  let head = match move RequestParser.head(&parsed.parser) {
    Result.Failure {error} => { return Option.none<Decoder>() }
    Result.Success {value} => value
  }
  let selection = match move selectRequest(head, TrailerPolicy.defaultPolicy()) {
    Result.Failure {error} => { return Option.none<Decoder>() }
    Result.Success {value} => value
  }
  let made = run Decoder.make(move selection, limits, TrailerPolicy.defaultPolicy())
  return match move made {
    Result.Failure {error} => Option.none<Decoder>()
    Result.Success {value} => Option.some<Decoder>(move value)
  }
}

effect fn encoderFor(input: &[u8]) -> Option<Encoder> ! OutOfMemoryError ? &mut Allocator {
  let parsed = match move (run parseRequest(input, headLimits())) {
    Result.Failure {error} => { return Option.none<Encoder>() }
    Result.Success {value} => move value
  }
  let head = match move RequestParser.head(&parsed.parser) {
    Result.Failure {error} => { return Option.none<Encoder>() }
    Result.Success {value} => value
  }
  let selection = match move validateOutgoingRequest(head, TrailerPolicy.defaultPolicy()) {
    Result.Failure {error} => { return Option.none<Encoder>() }
    Result.Success {value} => value
  }
  let made = run Encoder.make(move selection, bodyLimits(), TrailerPolicy.defaultPolicy())
  return match move made {
    Result.Failure {error} => Option.none<Encoder>()
    Result.Success {value} => Option.some<Encoder>(move value)
  }
}

fn progressIs(
  result: Result<Progress, BodyError>,
  consumed: usize,
  written: usize,
  totalWire: u64,
  totalPayload: u64,
  state: ProgressState,
) -> bool {
  return match move result {
    Result<Progress, BodyError>.Failure {error} => false
    Result<Progress, BodyError>.Success {value} => value.consumed == consumed
      && value.written == written
      && value.totalWire == totalWire
      && value.totalPayload == totalPayload
      && value.state == state
  }
}

effect fn fixedProbe() -> bool ! OutOfMemoryError ? &mut Allocator {
  let made = run decoderFor(
    b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nContent-Length: 4\\r\\n\\r\\n",
    bodyLimits(),
  )
  let mut decoder = match move made {
    Option<Decoder>.None => { return false }
    Option<Decoder>.Some {value} => move value
  }
  let mut first: [u8; 2] = [165, 165]
  if !progressIs(
    Decoder.step(&mut decoder, b"WikiNEXT", &mut first, true),
    2,
    2,
    u64.toU64(2),
    u64.toU64(2),
    ProgressState.NeedOutput,
  ) || !equal(&first, b"Wi") { return false }
  let mut second: [u8; 2] = [165, 165]
  if !progressIs(
    Decoder.step(&mut decoder, b"kiNEXT", &mut second, true),
    2,
    2,
    u64.toU64(4),
    u64.toU64(4),
    ProgressState.Complete,
  ) || !equal(&second, b"ki") { return false }
  return match move Decoder.completion(&decoder) {
    Option.None => false
    Option.Some {value} => value.kind() == CompletionKind.Delimited
  }
}

effect fn chunkProbe() -> bool ! OutOfMemoryError ? &mut Allocator {
  let made = run decoderFor(
    b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nTransfer-Encoding: chunked\\r\\n\\r\\n",
    bodyLimits(),
  )
  let mut decoder = match move made {
    Option<Decoder>.None => { return false }
    Option<Decoder>.Some {value} => move value
  }
  let mut output: [u8; 8] = [165, 165, 165, 165, 165, 165, 165, 165]
  let decoded = Decoder.step(
    &mut decoder,
    b"4;mode=\\x22safe\\x22\\r\\nWiki\\r\\n0\\r\\nContent-Digest: abc\\r\\n\\r\\nNEXT",
    &mut output,
    false,
  )
  let progress = match move decoded {
    Result<Progress, BodyError>.Failure {error} => { return false }
    Result<Progress, BodyError>.Success {value} => value
  }
  if progress.written != 4
    || progress.consumed != 47
    || progress.totalWire != u64.toU64(47)
    || progress.totalPayload != u64.toU64(4)
    || progress.state != ProgressState.Complete
    || output[0] != 87
    || output[1] != 105
    || output[2] != 107
    || output[3] != 105
    || output[4] != 165
    || output[5] != 165
    || output[6] != 165
    || output[7] != 165 { return false }
  let trailers = match move Decoder.trailers(&decoder) {
    Option.None => { return false }
    Option.Some {value} => value
  }
  if trailers.count() != usize.ONE { return false }
  let mut iterator = trailers.fields()
  return match move TrailerIterator.next(&mut iterator) {
    Option.None => false
    Option.Some {value} => equal(String.utf8Bytes(value.name()), b"Content-Digest")
      && equal(value.value(), b"abc")
  }
}

effect fn exactBoundaryProbe() -> bool ! OutOfMemoryError ? &mut Allocator {
  let made = run decoderFor(
    b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nTransfer-Encoding: chunked\\r\\n\\r\\n",
    bodyLimits(),
  )
  let mut decoder = match move made {
    Option.None => { return false }
    Option.Some {value} => move value
  }
  let mut output: [u8; 4] = [165, 165, 165, 165]
  return progressIs(
    Decoder.step(&mut decoder, b"4\\r\\nWiki\\r\\n0\\r\\n\\r\\nNEXT", &mut output, false),
    14,
    4,
    u64.toU64(14),
    u64.toU64(4),
    ProgressState.Complete,
  ) && equal(&output, b"Wiki")
}

fn chunkSyntaxFailure(result: Result<Progress, BodyError>) -> bool {
  return match move result {
    Result.Success {value} => false
    Result.Failure {error} => match move error.reason {
      BodyReason.ChunkSyntax => error.wireOffset == u64.MIN
        && error.consumed == 4
        && error.written == usize.ZERO
        && error.totalWire == u64.toU64(4)
        && error.totalPayload == u64.MIN
      _ => false
    }
  }
}

fn invalidStateFailure(result: Result<Progress, BodyError>) -> bool {
  return match move result {
    Result.Success {value} => false
    Result.Failure {error} => match move error.reason {
      BodyReason.InvalidState => error.consumed == usize.ZERO && error.written == usize.ZERO
      _ => false
    }
  }
}

effect fn failureProbe() -> bool ! OutOfMemoryError ? &mut Allocator {
  let made = run decoderFor(
    b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nTransfer-Encoding: chunked\\r\\n\\r\\n",
    bodyLimits(),
  )
  let mut decoder = match move made {
    Option.None => { return false }
    Option.Some {value} => move value
  }
  let mut output: [u8; 1] = [165]
  if !chunkSyntaxFailure(Decoder.step(&mut decoder, b"+1\\r\\n", &mut output, false)) {
    return false
  }
  return invalidStateFailure(Decoder.step(&mut decoder, b"", &mut output, false))
}

fn chunkLimitFailure(result: Result<Progress, BodyError>) -> bool {
  return match move result {
    Result.Success {value} => false
    Result.Failure {error} => match move error.reason {
      BodyReason.LimitExceeded {limit, allowed, attempted} => limit == BodyLimitKind.Chunks
        && allowed == u64.toU64(1)
        && attempted == u64.toU64(2)
        && error.wireOffset == u64.toU64(9)
        && error.consumed == 9
        && error.written == 1
        && error.totalWire == u64.toU64(9)
        && error.totalPayload == u64.toU64(1)
      _ => false
    }
  }
}

fn discardLimitFailure(result: Result<Progress, BodyError>) -> bool {
  return match move result {
    Result.Success {value} => false
    Result.Failure {error} => match move error.reason {
      BodyReason.DiscardLimit {allowed, attempted} => allowed == u64.toU64(6)
        && attempted == u64.toU64(7)
        && error.wireOffset == u64.toU64(6)
        && error.consumed == 6
        && error.written == usize.ZERO
        && error.totalWire == u64.toU64(6)
        && error.totalPayload == u64.toU64(1)
      _ => false
    }
  }
}

effect fn limitAndDiscardProbe() -> bool ! OutOfMemoryError ? &mut Allocator {
  let mut limited = bodyLimits()
  limited.maxChunks = 1
  let limitedMade = run decoderFor(
    b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nTransfer-Encoding: chunked\\r\\n\\r\\n",
    limited,
  )
  let mut limitedDecoder = match move limitedMade {
    Option.None => { return false }
    Option.Some {value} => move value
  }
  let mut output: [u8; 1] = [165]
  if !chunkLimitFailure(Decoder.step(
    &mut limitedDecoder,
    b"1\\r\\na\\r\\n0\\r\\n\\r\\n",
    &mut output,
    false,
  )) { return false }

  let discardMade = run decoderFor(
    b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nTransfer-Encoding: chunked\\r\\n\\r\\n",
    bodyLimits(),
  )
  let mut discardDecoder = match move discardMade {
    Option.None => { return false }
    Option.Some {value} => move value
  }
  return discardLimitFailure(Decoder.discard(
    &mut discardDecoder,
    b"1\\r\\na\\r\\n0\\r\\n\\r\\n",
    false,
    u64.toU64(6),
  ))
}

effect fn encoderProbe() -> bool ! OutOfMemoryError ? &mut Allocator {
  let made = run encoderFor(b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nTransfer-Encoding: chunked\\r\\n\\r\\n")
  let mut encoder = match move made {
    Option<Encoder>.None => { return false }
    Option<Encoder>.Some {value} => move value
  }
  let mut first: [u8; 4] = [165, 165, 165, 165]
  let started = Encoder.step(&mut encoder, b"Wiki", &mut first)
  if !progressIs(move started, 4, 4, u64.toU64(4), u64.toU64(4), ProgressState.NeedOutput)
    || !equal(&first, b"4\\r\\nW") { return false }
  let mut second: [u8; 5] = [165, 165, 165, 165, 165]
  if !progressIs(
    Encoder.step(&mut encoder, b"", &mut second),
    usize.ZERO,
    5,
    u64.toU64(9),
    u64.toU64(4),
    ProgressState.NeedInput,
  ) || !equal(&second, b"iki\\r\\n") { return false }
  let empty: [Header<'static>; 0] = []
  let trailers = match move Headers.make(&empty, valueLimits()) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  if let Result.Failure {error} = move Encoder.beginFinish(&mut encoder, &trailers) {
    return false
  }
  let mut finish: [u8; 5] = [165, 165, 165, 165, 165]
  if !progressIs(
    Encoder.continueFinish(&mut encoder, &mut finish),
    usize.ZERO,
    5,
    u64.toU64(14),
    u64.toU64(4),
    ProgressState.Complete,
  ) || !equal(&finish, b"0\\r\\n\\r\\n") { return false }
  return match move Encoder.completion(&encoder) {
    Option.None => false
    Option.Some {value} => value.kind() == CompletionKind.Delimited
  }
}

effect fn responsePrecedenceProbe() -> bool ! OutOfMemoryError ? &mut Allocator {
  let connect = match move Method.parse("CONNECT", 16) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  let parsed204 = match move (run parseResponse(
    b"HTTP/1.1 204 No Content\\r\\nContent-Length: nope\\r\\n\\r\\n",
    headLimits(),
  )) {
    Result.Failure {error} => { return false }
    Result.Success {value} => move value
  }
  let head204 = match move ResponseParser.head(&parsed204.parser) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  let selected204 = match move selectResponse(head204, &connect, TrailerPolicy.defaultPolicy()) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  let emptyFirst = match move selected204.framing() {
    Framing.Empty => selected204.anomaly() == Anomaly.ProhibitedFraming
    _ => false
  }
  if !emptyFirst { return false }
  let parsed200 = match move (run parseResponse(
    b"HTTP/1.1 200 OK\\r\\nContent-Length: nope\\r\\nTransfer-Encoding: gzip\\r\\n\\r\\n",
    headLimits(),
  )) {
    Result.Failure {error} => { return false }
    Result.Success {value} => move value
  }
  let head200 = match move ResponseParser.head(&parsed200.parser) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  return match move selectResponse(head200, &connect, TrailerPolicy.defaultPolicy()) {
    Result.Failure {error} => false
    Result.Success {value} => match move value.framing() {
      Framing.Tunnel => true
      _ => false
    }
  }
}

effect fn program() -> i32 ! OutOfMemoryError ? &mut Allocator {
  if !(run fixedProbe()) { return 1 }
  if !(run chunkProbe()) { return 2 }
  if !(run exactBoundaryProbe()) { return 3 }
  if !(run failureProbe()) { return 4 }
  if !(run limitAndDiscardProbe()) { return 5 }
  if !(run encoderProbe()) { return 6 }
  if !(run responsePrecedenceProbe()) { return 7 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 5 }

pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run Effect.catchAll(
    program() |> Effect.provideMut<Allocator>(&mut allocator),
    recover,
  )
}`
