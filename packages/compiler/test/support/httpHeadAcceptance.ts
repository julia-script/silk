/** One runtime program exercises the strict bounded HTTP-head contract without a compiler matrix. */
export const httpHeadAcceptanceSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.bytes { Bytes }
import silk.effect { Effect }
import silk.http {
  Header,
  Method,
  OwnedRequestHead,
  OwnedResponseHead,
  Status,
  ValueError,
  ValueReason,
  Version,
}
import silk.http_head {
  HeaderIterator,
  Limits,
  ParseComponent,
  ParseError,
  ParseReason,
  ParserState,
  ParsedResponse,
  Progress,
  ProgressState,
  RequestHead,
  RequestParser,
  ResponseHead,
  ResponseParser,
  parseResponse,
  requestSerializedSize,
  responseSerializedSize,
  writeRequestInto,
  writeResponseInto,
}
import silk.http_headers { Limits as ValueLimits }
import silk.option { Option }
import silk.result { Result }
import silk.slice { Slice }
import silk.usize

fn valueLimits() -> ValueLimits {
  return ValueLimits {
    maxMethodBytes: 32,
    maxTargetBytes: 256,
    maxNameBytes: 64,
    maxValueBytes: 256,
    maxFields: 4,
    maxFieldBytes: 512,
    maxOwnedBytes: 1024,
  }
}

fn limits() -> Limits {
  return Limits {
    maxHeadBytes: 1024,
    maxStartLineBytes: 256,
    maxFieldLineBytes: 256,
    maxOwnedBytes: 1152,
    values: valueLimits(),
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

fn equalPrefix(left: &[u8], right: &[u8]) -> bool {
  if left.length < right.length { return false }
  let mut index = usize.ZERO
  while index < right.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

enum ExpectedReason {
  InvalidFieldName,
  InvalidHost,
  DuplicateHost,
  InvalidLineEnding,
  InvalidStartLine,
  InvalidStatus,
  InvalidTarget,
  LimitExceeded,
  MissingHost,
  ObsFold,
  BareLf,
  UnsupportedVersion,
  WhitespaceBeforeColon,
  InvalidState,
  Truncated,
}

fn reasonIs(reason: ParseReason, expected: ExpectedReason) -> bool {
  return match move expected {
    ExpectedReason.InvalidFieldName => match move reason {
      ParseReason.InvalidFieldName => true
      _ => false
    }
    ExpectedReason.InvalidHost => match move reason {
      ParseReason.InvalidHost => true
      _ => false
    }
    ExpectedReason.DuplicateHost => match move reason {
      ParseReason.DuplicateHost => true
      _ => false
    }
    ExpectedReason.InvalidLineEnding => match move reason {
      ParseReason.InvalidLineEnding => true
      _ => false
    }
    ExpectedReason.MissingHost => match move reason {
      ParseReason.MissingHost => true
      _ => false
    }
    ExpectedReason.InvalidStartLine => match move reason {
      ParseReason.InvalidStartLine => true
      _ => false
    }
    ExpectedReason.InvalidStatus => match move reason {
      ParseReason.InvalidStatus => true
      _ => false
    }
    ExpectedReason.InvalidTarget => match move reason {
      ParseReason.InvalidTarget => true
      _ => false
    }
    ExpectedReason.LimitExceeded => match move reason {
      ParseReason.LimitExceeded {limit, allowed, attempted} => true
      _ => false
    }
    ExpectedReason.ObsFold => match move reason {
      ParseReason.ObsFold => true
      _ => false
    }
    ExpectedReason.BareLf => match move reason {
      ParseReason.BareLf => true
      _ => false
    }
    ExpectedReason.UnsupportedVersion => match move reason {
      ParseReason.UnsupportedVersion => true
      _ => false
    }
    ExpectedReason.WhitespaceBeforeColon => match move reason {
      ParseReason.WhitespaceBeforeColon => true
      _ => false
    }
    ExpectedReason.InvalidState => match move reason {
      ParseReason.InvalidState => true
      _ => false
    }
    ExpectedReason.Truncated => match move reason {
      ParseReason.Truncated => true
      _ => false
    }
  }
}

effect fn requestFails(
  input: &[u8],
  expected: ExpectedReason,
  configured: Limits,
) -> bool ! OutOfMemoryError ? &mut Allocator {
  let made = run RequestParser.make(configured)
  let mut parser = match move made {
    Result<RequestParser, ParseError>.Failure {error} => {
      return reasonIs(error.reason, expected)
    }
    Result<RequestParser, ParseError>.Success {value} => move value
  }
  return match move RequestParser.feed(&mut parser, input, true) {
    Result<Progress, ParseError>.Success {value} => false
    Result<Progress, ParseError>.Failure {error} => reasonIs(error.reason, expected)
  }
}

effect fn responseFails(
  input: &[u8],
  expected: ExpectedReason,
) -> bool ! OutOfMemoryError ? &mut Allocator {
  let made = run ResponseParser.make(limits())
  let mut parser = match move made {
    Result<ResponseParser, ParseError>.Failure {error} => { return false }
    Result<ResponseParser, ParseError>.Success {value} => move value
  }
  return match move ResponseParser.feed(&mut parser, input, true) {
    Result<Progress, ParseError>.Success {value} => false
    Result<Progress, ParseError>.Failure {error} => reasonIs(error.reason, expected)
  }
}

fn inspectRequest<'value>(result: Result<RequestHead<'value>, ParseError>) -> bool {
  let head = match move result {
    Result<RequestHead<'value>, ParseError>.Failure {error} => { return false }
    Result<RequestHead<'value>, ParseError>.Success {value} => value
  }
  if RequestHead.version(&head) != Version.Http11 || RequestHead.fieldCount(&head) != 3 {
    return false
  }
  let method = RequestHead.method(&head)
  if Method.format(&method) != "GET" { return false }
  let mut fields = RequestHead.fields(&head)
  let first = match move HeaderIterator.next(&mut fields) {
    Option<Header<'value>>.None => { return false }
    Option<Header<'value>>.Some {value} => value
  }
  if Header.name(&first) != "Host" || !equal(Header.value(&first), b"a") { return false }
  let second = match move HeaderIterator.next(&mut fields) {
    Option<Header<'value>>.None => { return false }
    Option<Header<'value>>.Some {value} => value
  }
  if Header.name(&second) != "X" || Header.value(&second).length != usize.ZERO { return false }
  let third = match move HeaderIterator.next(&mut fields) {
    Option<Header<'value>>.None => { return false }
    Option<Header<'value>>.Some {value} => value
  }
  if Header.name(&third) != "X" || !equal(Header.value(&third), b"b") { return false }
  return match move HeaderIterator.next(&mut fields) {
    Option<Header<'value>>.None => true
    Option<Header<'value>>.Some {value} => false
  }
}

effect fn splitMatrix() -> bool ! OutOfMemoryError ? &mut Allocator {
  let message = b"GET / HTTP/1.1\\r\\nHost: a\\r\\nX: \\r\\nX: b\\r\\n\\r\\nBODY"
  let headLength = message.length - 4
  let mut split = usize.ZERO
  while split <= headLength {
    let made = run RequestParser.make(limits())
    let mut parser = match move made {
      Result<RequestParser, ParseError>.Failure {error} => { return false }
      Result<RequestParser, ParseError>.Success {value} => move value
    }
    let first = RequestParser.feed(
      &mut parser,
      Slice.view<u8>(message, usize.ZERO, split),
      false,
    )
    match move first {
      Result<Progress, ParseError>.Failure {error} => { return false }
      Result<Progress, ParseError>.Success {value} => {
        if value.consumed != split { return false }
        if split < headLength && value.state != ProgressState.NeedInput { return false }
        if split == headLength && value.state != ProgressState.Complete { return false }
      }
    }
    if split < headLength {
      let second = RequestParser.feed(
        &mut parser,
        Slice.view<u8>(message, split, message.length - split),
        false,
      )
      match move second {
        Result<Progress, ParseError>.Failure {error} => { return false }
        Result<Progress, ParseError>.Success {value} => {
          if value.consumed != headLength - split || value.state != ProgressState.Complete {
            return false
          }
        }
      }
    }
    if !inspectRequest(RequestParser.head(&parser)) { return false }
    let reset = RequestParser.reset(&mut parser)
    if let Result<(), ParseError>.Failure {error} = move reset { return false }
    let reused = RequestParser.feed(&mut parser, b"GET / HTTP/1.0\\r\\n\\r\\n", true)
    if let Result<Progress, ParseError>.Failure {error} = move reused { return false }
    split = split + usize.ONE
  }
  return true
}

effect fn oneByteDelivery() -> bool ! OutOfMemoryError ? &mut Allocator {
  let message = b"GET / HTTP/1.1\\r\\nHost: a\\r\\n\\r\\n"
  let made = run RequestParser.make(limits())
  let mut parser = match move made {
    Result<RequestParser, ParseError>.Failure {error} => { return false }
    Result<RequestParser, ParseError>.Success {value} => move value
  }
  let mut index = usize.ZERO
  while index < message.length {
    let fed = RequestParser.feed(&mut parser, Slice.view<u8>(message, index, usize.ONE), false)
    match move fed {
      Result<Progress, ParseError>.Failure {error} => { return false }
      Result<Progress, ParseError>.Success {value} => {
        if value.consumed != usize.ONE { return false }
        if index + usize.ONE < message.length && value.state != ProgressState.NeedInput {
          return false
        }
        if index + usize.ONE == message.length && value.state != ProgressState.Complete {
          return false
        }
      }
    }
    index = index + usize.ONE
  }
  return RequestParser.state(&parser) == ParserState.Complete
}

effect fn failuresAndReset() -> bool ! OutOfMemoryError ? &mut Allocator {
  let made = run RequestParser.make(limits())
  let mut parser = match move made {
    Result<RequestParser, ParseError>.Failure {error} => { return false }
    Result<RequestParser, ParseError>.Success {value} => move value
  }
  let broken = RequestParser.feed(&mut parser, b"GET / HTTP/1.1\\rX", false)
  match move broken {
    Result<Progress, ParseError>.Success {value} => { return false }
    Result<Progress, ParseError>.Failure {error} => {
      if !reasonIs(error.reason, ExpectedReason.InvalidLineEnding)
        || error.component != ParseComponent.LineEnding
        || error.offset != 14
        || error.consumed != 14 {
        return false
      }
    }
  }
  match move RequestParser.failure(&parser) {
    Option<ParseError>.None => { return false }
    Option<ParseError>.Some {value} => {
      if !reasonIs(value.reason, ExpectedReason.InvalidLineEnding) { return false }
    }
  }
  match move RequestParser.feed(&mut parser, b"x", false) {
    Result<Progress, ParseError>.Success {value} => { return false }
    Result<Progress, ParseError>.Failure {error} => {
      if !reasonIs(error.reason, ExpectedReason.InvalidState) || error.consumed != usize.ZERO {
        return false
      }
    }
  }
  let reset = RequestParser.reset(&mut parser)
  if let Result<(), ParseError>.Failure {error} = move reset { return false }
  let missing = RequestParser.feed(&mut parser, b"GET / HTTP/1.1\\r\\n\\r\\n", true)
  match move missing {
    Result<Progress, ParseError>.Success {value} => { return false }
    Result<Progress, ParseError>.Failure {error} => {
      if !reasonIs(error.reason, ExpectedReason.MissingHost)
        || error.component != ParseComponent.Host {
        return false
      }
    }
  }
  let resetAgain = RequestParser.reset(&mut parser)
  if let Result<(), ParseError>.Failure {error} = move resetAgain { return false }
  let truncated = RequestParser.feed(&mut parser, b"GET / HTTP/1.1\\r\\n", true)
  return match move truncated {
    Result<Progress, ParseError>.Success {value} => false
    Result<Progress, ParseError>.Failure {error} => {
      return reasonIs(error.reason, ExpectedReason.Truncated)
        && error.offset == 16
        && error.consumed == 16
    }
  }
}

effect fn strictFailures() -> bool ! OutOfMemoryError ? &mut Allocator {
  if !run requestFails(b"\\r\\n", ExpectedReason.InvalidStartLine, limits()) { return false }
  if !run requestFails(b"GET / HTTP/1.1\\n", ExpectedReason.BareLf, limits()) { return false }
  if !run requestFails(
    b"GET / HTTP/1.1\\r\\nHost : a\\r\\n\\r\\n",
    ExpectedReason.WhitespaceBeforeColon,
    limits(),
  ) { return false }
  if !run requestFails(
    b"GET / HTTP/1.1\\r\\n Host: a\\r\\n\\r\\n",
    ExpectedReason.ObsFold,
    limits(),
  ) { return false }
  if !run requestFails(
    b"GET / HTTP/1.1\\r\\nHost: a\\r\\nhost: b\\r\\n\\r\\n",
    ExpectedReason.DuplicateHost,
    limits(),
  ) { return false }
  if !run requestFails(
    b"GET / HTTP/1.1\\r\\nHost: @\\r\\n\\r\\n",
    ExpectedReason.InvalidHost,
    limits(),
  ) { return false }
  if !run requestFails(
    b"CONNECT / HTTP/1.1\\r\\nHost: a\\r\\n\\r\\n",
    ExpectedReason.InvalidTarget,
    limits(),
  ) { return false }
  let mut oneField = limits()
  oneField.values.maxFields = usize.ONE
  if !run requestFails(
    b"GET / HTTP/1.1\\r\\nHost: a\\r\\nX: b\\r\\n\\r\\n",
    ExpectedReason.LimitExceeded,
    oneField,
  ) { return false }
  let mut noOwnedCapacity = limits()
  noOwnedCapacity.maxOwnedBytes = 1151
  if !run requestFails(b"", ExpectedReason.LimitExceeded, noOwnedCapacity) { return false }
  let mut noHeadCapacity = limits()
  noHeadCapacity.maxHeadBytes = usize.ZERO
  if !run requestFails(b"G", ExpectedReason.LimitExceeded, noHeadCapacity) { return false }
  if !run responseFails(b"HTTP/0.9 200 ok\\r\\n\\r\\n", ExpectedReason.UnsupportedVersion) {
    return false
  }
  if !run responseFails(b"HTTP/1.1 099 no\\r\\n\\r\\n", ExpectedReason.InvalidStatus) {
    return false
  }
  if !run responseFails(b"HTTP/1.1 204\\r\\n\\r\\n", ExpectedReason.InvalidStartLine) {
    return false
  }
  return run responseFails(
    b"HTTP/1.1 200 ok\\r\\n folded\\r\\n\\r\\n",
    ExpectedReason.ObsFold,
  )
}

effect fn responseWholeSlice() -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsed = run parseResponse(b"HTTP/1.1 204 \\r\\nX: \\r\\n\\r\\nBODY", limits())
  let value = match move parsed {
    Result<ParsedResponse, ParseError>.Failure {error} => { return false }
    Result<ParsedResponse, ParseError>.Success {value} => move value
  }
  let ParsedResponse {parser, progress} = move value
  return inspectResponse(ResponseParser.head(&parser), progress)
}

fn inspectResponse<'value>(
  result: Result<ResponseHead<'value>, ParseError>,
  progress: Progress,
) -> bool {
  if progress.consumed != 22 || progress.state != ProgressState.Complete { return false }
  let head = match move result {
    Result<ResponseHead<'value>, ParseError>.Failure {error} => { return false }
    Result<ResponseHead<'value>, ParseError>.Success {value} => value
  }
  let reason = ResponseHead.reason(&head)
  if ResponseHead.version(&head) != Version.Http11 || reason.length != usize.ZERO
    || ResponseHead.fieldCount(&head) != usize.ONE {
    return false
  }
  let status = ResponseHead.status(&head)
  return Status.code(&status) == 204
}

effect fn copyBorrowed<'value>(
  result: Result<RequestHead<'value>, ParseError>,
) -> Option<OwnedRequestHead> ! OutOfMemoryError ? &mut Allocator {
  let borrowed = match move result {
    Result<RequestHead<'value>, ParseError>.Failure {error} => {
      return Option.none<OwnedRequestHead>()
    }
    Result<RequestHead<'value>, ParseError>.Success {value} => value
  }
  let copied = run RequestHead.copy(&borrowed, valueLimits())
  return match move copied {
    Result<OwnedRequestHead, ValueError>.Failure {error} => Option.none<OwnedRequestHead>()
    Result<OwnedRequestHead, ValueError>.Success {value} => {
      return Option.some<OwnedRequestHead>(move value)
    }
  }
}

effect fn copyResponseBorrowed<'value>(
  result: Result<ResponseHead<'value>, ParseError>,
) -> Option<OwnedResponseHead> ! OutOfMemoryError ? &mut Allocator {
  let borrowed = match move result {
    Result<ResponseHead<'value>, ParseError>.Failure {error} => {
      return Option.none<OwnedResponseHead>()
    }
    Result<ResponseHead<'value>, ParseError>.Success {value} => value
  }
  let copied = run ResponseHead.copy(&borrowed, valueLimits())
  return match move copied {
    Result<OwnedResponseHead, ValueError>.Failure {error} => Option.none<OwnedResponseHead>()
    Result<OwnedResponseHead, ValueError>.Success {value} => {
      return Option.some<OwnedResponseHead>(move value)
    }
  }
}

effect fn copyAndSerialize() -> bool ! OutOfMemoryError ? &mut Allocator {
  let input = b"GET / HTTP/1.1\\r\\nHost: a\\r\\nX: \\r\\nX: b\\r\\n\\r\\n"
  let made = run RequestParser.make(limits())
  let mut parser = match move made {
    Result<RequestParser, ParseError>.Failure {error} => { return false }
    Result<RequestParser, ParseError>.Success {value} => move value
  }
  let fed = RequestParser.feed(&mut parser, input, true)
  if let Result<Progress, ParseError>.Failure {error} = move fed { return false }
  let copied = run copyBorrowed(RequestParser.head(&parser))
  drop parser
  let owned = match move copied {
    Option<OwnedRequestHead>.None => { return false }
    Option<OwnedRequestHead>.Some {value} => move value
  }
  let shared = OwnedRequestHead.view(&owned)
  let required = match move requestSerializedSize(&shared, valueLimits()) {
    Result<usize, ValueError>.Failure {error} => { return false }
    Result<usize, ValueError>.Success {value} => value
  }
  if required != input.length { return false }
  let mut output = run Bytes.zeroed(64)
  let written = writeRequestInto(&shared, Bytes.asMutSlice(&mut output), valueLimits())
  match move written {
    Result<usize, ValueError>.Failure {error} => { return false }
    Result<usize, ValueError>.Success {value} => {
      if value != input.length || !equalPrefix(Bytes.asSlice(&output), input) { return false }
    }
  }
  let mut short: [u8; 1] = [90]
  let rejected = writeRequestInto(&shared, &mut short, valueLimits())
  return match move rejected {
    Result<usize, ValueError>.Success {value} => false
    Result<usize, ValueError>.Failure {error} => {
      if short[0] != 90 { return false }
      return match move error.reason {
        ValueReason.OutputTooSmall {required: actual, available} => {
          return actual == required && available == usize.ONE
        }
        _ => false
      }
    }
  }
}

effect fn copyAndSerializeResponse() -> bool ! OutOfMemoryError ? &mut Allocator {
  let input = b"HTTP/1.1 204 \\r\\nX: \\r\\n\\r\\n"
  let made = run ResponseParser.make(limits())
  let mut parser = match move made {
    Result<ResponseParser, ParseError>.Failure {error} => { return false }
    Result<ResponseParser, ParseError>.Success {value} => move value
  }
  let fed = ResponseParser.feed(&mut parser, input, true)
  if let Result<Progress, ParseError>.Failure {error} = move fed { return false }
  let copied = run copyResponseBorrowed(ResponseParser.head(&parser))
  drop parser
  let owned = match move copied {
    Option<OwnedResponseHead>.None => { return false }
    Option<OwnedResponseHead>.Some {value} => move value
  }
  let shared = OwnedResponseHead.view(&owned)
  let required = match move responseSerializedSize(&shared, valueLimits()) {
    Result<usize, ValueError>.Failure {error} => { return false }
    Result<usize, ValueError>.Success {value} => value
  }
  if required != input.length { return false }
  let mut output = run Bytes.zeroed(32)
  let written = writeResponseInto(&shared, Bytes.asMutSlice(&mut output), valueLimits())
  match move written {
    Result<usize, ValueError>.Failure {error} => { return false }
    Result<usize, ValueError>.Success {value} => {
      if value != input.length || !equalPrefix(Bytes.asSlice(&output), input) { return false }
    }
  }
  let mut short: [u8; 1] = [91]
  let rejected = writeResponseInto(&shared, &mut short, valueLimits())
  return match move rejected {
    Result<usize, ValueError>.Success {value} => false
    Result<usize, ValueError>.Failure {error} => short[0] == 91
  }
}

effect fn checks() -> i32 ! OutOfMemoryError ? &mut Allocator {
  if !run splitMatrix() { return 1 }
  if !run oneByteDelivery() { return 2 }
  if !run failuresAndReset() { return 3 }
  if !run strictFailures() { return 4 }
  if !run responseWholeSlice() { return 5 }
  if !run copyAndSerialize() { return 6 }
  if !run copyAndSerializeResponse() { return 7 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 8 }

pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run Effect.catchAll(
    checks() |> Effect.provideMut<Allocator>(&mut allocator),
    recover,
  )
}
`
