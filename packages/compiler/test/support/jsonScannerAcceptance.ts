/** One native program covers slice and resumable JSON scanner behavior. */
export const jsonScannerAcceptanceSource = `import silk.json_scanner { JsonScanner, JsonScanState, JsonScanEvent, JsonScanStep, JsonToken, JsonError, JsonReason, JsonSpan, MAX_DEPTH }
import silk.option { Option }
import silk.result { Result }
import silk.slice { Slice }
import silk.usize

fn offsetMatches(offset: Option<usize>, expected: usize) -> bool {
  return match move offset {
    Option<usize>.Some { value } => value == expected
    Option<usize>.None => false
  }
}

fn rejects(input: &[u8], reason: JsonReason, offset: usize) -> bool {
  let mut scanner = JsonScanner.make(input)
  while true {
    let result = JsonScanner.next(&mut scanner)
    match move result {
      Result<JsonToken, JsonError>.Failure { error } => {
        return error.reason == reason && offsetMatches(move error.offset, offset)
      }
      Result<JsonToken, JsonError>.Success { value } => {
        match move value {
          JsonToken.EndOfInput => { return false }
          _ => {}
        }
      }
    }
  }
  return false
}

fn accepts(input: &[u8]) -> bool {
  let mut scanner = JsonScanner.make(input)
  while true {
    let result = JsonScanner.next(&mut scanner)
    match move result {
      Result<JsonToken, JsonError>.Failure { error } => { return false }
      Result<JsonToken, JsonError>.Success { value } => {
        match move value {
          JsonToken.EndOfInput => { return true }
          _ => {}
        }
      }
    }
  }
  return false
}

fn splitAccept(input: &[u8]) -> bool {
  let mut cut = usize.ZERO
  while cut <= input.length {
    let mut state = JsonScanState.make()
    let first = Slice.view<u8>(input, usize.ZERO, cut)
    let second = Slice.view<u8>(input, cut, input.length - cut)
    let firstStep = JsonScanState.step(&mut state, first, false)
    let firstToken = match move firstStep {
      Result<JsonScanStep, JsonError>.Failure { error } => { return false }
      Result<JsonScanStep, JsonError>.Success { value } => match move value.event {
        JsonScanEvent.Token { token } => true
        JsonScanEvent.NeedMore => false
        JsonScanEvent.EndOfInput => { return false }
      }
    }
    let next = JsonScanState.step(&mut state, second, true)
    match move next {
      Result<JsonScanStep, JsonError>.Failure { error } => { return false }
      Result<JsonScanStep, JsonError>.Success { value } => {
        if value.consumed != second.length { return false }
        match move value.event {
          JsonScanEvent.Token { token } => { if firstToken { return false } }
          JsonScanEvent.EndOfInput => { if !firstToken { return false } }
          JsonScanEvent.NeedMore => { return false }
        }
      }
    }
    cut = cut + usize.ONE
  }
  return true
}

pub fn main() -> i32 {
  if !accepts(b"null") || !accepts(b"true") || !accepts(b"false") { return 1 }
  if !accepts(b"{\\"a\\":[0,-12.3e+4,true,null],\\"b\\":\\"ok\\"}") { return 2 }
  if !accepts(b" \\t[ ]\\n") { return 3 }
  if !splitAccept(b"\\"a\\\\uD834\\\\uDD1E\\"") { return 4 }
  if !splitAccept(b"-12.34e+56") { return 5 }
  if !rejects(b"[1,]", JsonReason.UnexpectedByte, 3) { return 6 }
  if !rejects(b"{\\"a\\":1,}", JsonReason.UnexpectedByte, 7) { return 7 }
  if !rejects(b"01", JsonReason.InvalidNumber, 1) { return 8 }
  if !rejects(b"true false", JsonReason.TrailingContent, 5) { return 9 }
  if !rejects(b"\\"\\\\uD800\\"", JsonReason.InvalidEscape, 7) { return 10 }
  return 42
}`
