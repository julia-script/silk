const acceptedCases = [
  ['y_array_empty', '[]'],
  ['y_array_values', '[null,true,false,0,"x",[],{}]'],
  ['y_object_empty', '{}'],
  ['y_object_members', '{"x":1,"x":2,"nest":{"ok":true}}'],
  ['y_number_zero', '0'],
  ['y_number_negative_zero', '-0'],
  ['y_number_fraction', '0.001'],
  ['y_number_exponent', '1E-999'],
  ['y_string_empty', '""'],
  ['y_string_escapes', '"\\"\\\\\\/\\b\\f\\n\\r\\t"'],
  ['y_string_surrogate_pair', '"\\uD834\\uDD1E"'],
  ['y_string_utf8', '"€💩"'],
  ['y_whitespace', ' \t\r\n [ true ] \t'],
  ['y_depth_limit', '['.repeat(64) + '0' + ']'.repeat(64)],
] as const

const rejectedCases = [
  ['n_empty', []],
  ['n_array_trailing_comma', '[1,]'],
  ['n_array_double_comma', '[1,,2]'],
  ['n_object_trailing_comma', '{"a":1,}'],
  ['n_object_missing_colon', '{"a" 1}'],
  ['n_object_unquoted_key', '{a:1}'],
  ['n_object_comma', '{,"a":1}'],
  ['n_number_leading_zero', '01'],
  ['n_number_leading_plus', '+1'],
  ['n_number_minus_only', '-'],
  ['n_number_fraction_no_digit', '1.'],
  ['n_number_exponent_no_digit', '1e+'],
  ['n_number_nan', 'NaN'],
  ['n_number_infinity', 'Infinity'],
  ['n_string_unterminated', '"abc'],
  ['n_string_bad_escape', '"\\x"'],
  ['n_string_lone_high_surrogate', '"\\uD800"'],
  ['n_string_lone_low_surrogate', '"\\uDC00"'],
  ['n_string_control', [34, 10, 34]],
  ['n_string_overlong_utf8', [34, 192, 128, 34]],
  ['n_string_surrogate_utf8', [34, 237, 160, 128, 34]],
  ['n_string_above_unicode', [34, 244, 144, 128, 128, 34]],
  ['n_comment', '/*x*/null'],
  ['n_trailing_value', 'true false'],
  ['n_depth_exceeded', '['.repeat(65) + '0' + ']'.repeat(65)],
] as const

const bytesOf = (value: string | readonly number[]): readonly number[] =>
  typeof value === 'string' ? [...new TextEncoder().encode(value)] : value

const silkCase = (name: string, value: string | readonly number[], accepted: boolean): string => {
  const bytes = bytesOf(value)
  const variable = name.replaceAll(/[^a-zA-Z0-9_]/g, '_')
  return `  // ${name}\n  let ${variable}: [u8; ${bytes.length}] = [${bytes.join(', ')}]\n  if ${accepted ? '!' : ''}accepts(&${variable}) { return ${accepted ? 20 : 21} }`
}

/** One native program covers strict JSONTestSuite categories and resumable scanner boundaries. */
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

fn sameBytes(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index = usize.ZERO
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

fn rawSpans() -> bool {
  let mut scanner = JsonScanner.make(b"[\\"a\\\\n\\",12]")
  let open = JsonScanner.next(&mut scanner)
  match move open {
    Result<JsonToken, JsonError>.Failure { error } => { return false }
    Result<JsonToken, JsonError>.Success { value } => match move value {
      JsonToken.ArrayBegin => {}
      _ => { return false }
    }
  }
  let text = JsonScanner.next(&mut scanner)
  match move text {
    Result<JsonToken, JsonError>.Failure { error } => { return false }
    Result<JsonToken, JsonError>.Success { value } => match move value {
      JsonToken.String { span, escaped } => {
        if !escaped || span.offset != 1 || span.length != 5 { return false }
        if !sameBytes(JsonScanner.slice(&scanner, move span), b"\\"a\\\\n\\"") { return false }
      }
      _ => { return false }
    }
  }
  let separator = JsonScanner.next(&mut scanner)
  match move separator {
    Result<JsonToken, JsonError>.Failure { error } => { return false }
    Result<JsonToken, JsonError>.Success { value } => match move value {
      JsonToken.ValueSeparator => {}
      _ => { return false }
    }
  }
  let number = JsonScanner.next(&mut scanner)
  return match move number {
    Result<JsonToken, JsonError>.Failure { error } => false
    Result<JsonToken, JsonError>.Success { value } => match move value {
      JsonToken.Number { span } => {
        if span.offset != 7 || span.length != 2 { return false }
        return sameBytes(JsonScanner.slice(&scanner, move span), b"12")
      }
      _ => false
    }
  }
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

fn splitReject(input: &[u8], reason: JsonReason, offset: usize) -> bool {
  let mut cut = usize.ZERO
  while cut <= input.length {
    let mut state = JsonScanState.make()
    let first = Slice.view<u8>(input, usize.ZERO, cut)
    let second = Slice.view<u8>(input, cut, input.length - cut)
    let firstStep = JsonScanState.step(&mut state, first, false)
    match move firstStep {
      Result<JsonScanStep, JsonError>.Failure { error } => {
        if error.reason != reason || !offsetMatches(move error.offset, offset) { return false }
      }
      Result<JsonScanStep, JsonError>.Success { value } => {
        match move value.event {
          JsonScanEvent.NeedMore => {
            let secondStep = JsonScanState.step(&mut state, second, true)
            match move secondStep {
              Result<JsonScanStep, JsonError>.Failure { error } => {
                if error.reason != reason || !offsetMatches(move error.offset, offset) { return false }
              }
              Result<JsonScanStep, JsonError>.Success { value: _ } => { return false }
            }
          }
          _ => { return false }
        }
      }
    }
    cut = cut + usize.ONE
  }
  return true
}

fn finalBoundaries() -> bool {
  let mut empty = JsonScanState.make()
  let emptyFinal = JsonScanState.step(&mut empty, b"", true)
  match move emptyFinal {
    Result<JsonScanStep, JsonError>.Success { value } => { return false }
    Result<JsonScanStep, JsonError>.Failure { error } => {
      if error.reason != JsonReason.UnexpectedEnd || !offsetMatches(move error.offset, 0) { return false }
    }
  }
  let mut complete = JsonScanState.make()
  let token = JsonScanState.step(&mut complete, b"null", true)
  match move token {
    Result<JsonScanStep, JsonError>.Failure { error } => { return false }
    Result<JsonScanStep, JsonError>.Success { value: tokenStep } => match move tokenStep.event {
      JsonScanEvent.Token { token: _ } => {}
      _ => { return false }
    }
  }
  let end = JsonScanState.step(&mut complete, b"", true)
  match move end {
    Result<JsonScanStep, JsonError>.Failure { error } => { return false }
    Result<JsonScanStep, JsonError>.Success { value: endStep } => match move endStep.event {
      JsonScanEvent.EndOfInput => {}
      _ => { return false }
    }
  }
  let again = JsonScanState.step(&mut complete, b"", true)
  return match move again {
    Result<JsonScanStep, JsonError>.Failure { error } => false
    Result<JsonScanStep, JsonError>.Success { value: againStep } => match move againStep.event {
      JsonScanEvent.EndOfInput => true
      _ => false
    }
  }
}

fn fragmentAndLookahead() -> bool {
  let mut stringState = JsonScanState.make()
  let first = JsonScanState.step(&mut stringState, b"  \\"a\\\\", false)
  match move first {
    Result<JsonScanStep, JsonError>.Failure { error } => { return false }
    Result<JsonScanStep, JsonError>.Success { value } => {
      if value.consumed != 5 || value.fragment.offset != 2 || value.fragment.length != 3 {
        return false
      }
      match move value.event {
        JsonScanEvent.NeedMore => {}
        _ => { return false }
      }
    }
  }
  let second = JsonScanState.step(&mut stringState, b"n\\"", true)
  match move second {
    Result<JsonScanStep, JsonError>.Failure { error } => { return false }
    Result<JsonScanStep, JsonError>.Success { value } => {
      if value.consumed != 2 || value.fragment.offset != 0 || value.fragment.length != 2 {
        return false
      }
      match move value.event {
        JsonScanEvent.Token { token } => match move token {
          JsonToken.String { span, escaped } => {
            if !escaped || span.offset != 2 || span.length != 5 { return false }
          }
          _ => { return false }
        }
        _ => { return false }
      }
    }
  }
  let mut numberState = JsonScanState.make()
  let numberFirst = JsonScanState.step(&mut numberState, b"12", false)
  match move numberFirst {
    Result<JsonScanStep, JsonError>.Failure { error } => { return false }
    Result<JsonScanStep, JsonError>.Success { value } => match move value.event {
      JsonScanEvent.NeedMore => {}
      _ => { return false }
    }
  }
  let numberSecond = JsonScanState.step(&mut numberState, b",", false)
  return match move numberSecond {
    Result<JsonScanStep, JsonError>.Failure { error } => false
    Result<JsonScanStep, JsonError>.Success { value } => {
      if value.consumed != 0 || value.fragment.length != 0 { return false }
      return match move value.event {
        JsonScanEvent.Token { token } => match move token {
          JsonToken.Number { span } => span.offset == 0 && span.length == 2
          _ => false
        }
        _ => false
      }
    }
  }
}

pub fn main() -> i32 {
  if !accepts(b"null") || !accepts(b"true") || !accepts(b"false") { return 1 }
  if !accepts(b"{\\"a\\":[0,-12.3e+4,true,null],\\"b\\":\\"ok\\"}") { return 2 }
  if !accepts(b" \\t[ ]\\n") { return 3 }
  if !splitAccept(b"\\"a\\\\uD834\\\\uDD1E\\"") { return 4 }
  if !splitAccept(b"-12.34e+56") { return 5 }
  let utf8: [u8; 6] = [34, 240, 159, 146, 169, 34]
  if !splitAccept(&utf8) { return 11 }
  if !splitReject(b"\\"\\\\uD800\\"", JsonReason.InvalidEscape, 7) { return 12 }
  if !splitReject(b"1e+", JsonReason.UnexpectedEnd, 3) { return 13 }
  let invalidUtf8: [u8; 4] = [34, 226, 40, 34]
  if !splitReject(&invalidUtf8, JsonReason.InvalidUtf8, 2) { return 15 }
  if !rawSpans() || !finalBoundaries() || !fragmentAndLookahead() { return 14 }
  if MAX_DEPTH != 64 { return 16 }
  let tooDeep: [u8; 66] = [${Array(65).fill(91).join(', ')}, 48]
  if !rejects(&tooDeep, JsonReason.DepthExceeded, 64) { return 17 }
  if !rejects(b"[1,]", JsonReason.UnexpectedByte, 3) { return 6 }
  if !rejects(b"{\\"a\\":1,}", JsonReason.UnexpectedByte, 7) { return 7 }
  if !rejects(b"01", JsonReason.InvalidNumber, 1) { return 8 }
  if !rejects(b"true false", JsonReason.TrailingContent, 5) { return 9 }
  if !rejects(b"\\"\\\\uD800\\"", JsonReason.InvalidEscape, 7) { return 10 }
${acceptedCases.map(([name, value]) => silkCase(name, value, true)).join('\n')}
${rejectedCases.map(([name, value]) => silkCase(name, value, false)).join('\n')}
  return 42
}`
