/** HTTP-specific Basic preflight and owned-origin normalization, without repeating codec suites. */
export const httpClientPolicyAcceptanceSource = `import silk.http_basic as Basic
import silk.http_basic {BasicError}
import silk.http_origin {Origin, OriginError}
import silk.uri {Uri}
import silk.uri_reference {ParseError}
import silk.result {Result}
import silk.slice {Slice}
import silk.usize

fn bytesEqual(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index = usize.ZERO
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

fn basic() -> bool {
  let mut scratch: [u8; 19] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  let mut output: [u8; 34] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  let checked1 = Basic.encodeInto(b"Aladdin", b"open sesame", &mut scratch, &mut output)
  let count = match move checked1 {
    Result<usize, BasicError>.Failure {error} => { return false }
    Result<usize, BasicError>.Success {value} => value
  }
  if !bytesEqual(Slice.view<u8>(&output, usize.ZERO, count), b"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==") { return false }
  let mut untouched: [u8; 2] = [165,165]
  let checked2 = Basic.encodeInto(b"u", b"p", &mut scratch, &mut untouched)
  let failed = match move checked2 {
    Result<usize, BasicError>.Failure {error} => error == BasicError.OutputTooSmall
    Result<usize, BasicError>.Success {value} => false
  }
  if !failed || untouched[0] != 165 || untouched[1] != 165 { return false }
  let checked3 = Basic.encodeInto(b"u:s", b"p", &mut scratch, &mut untouched)
  let colon = match move checked3 {
    Result<usize, BasicError>.Failure {error} => error == BasicError.InvalidUsername
    Result<usize, BasicError>.Success {value} => false
  }
  let control: [u8; 1] = [127]
  let checked4 = Basic.encodeInto(b"u", &control, &mut scratch, &mut untouched)
  let rejected = match move checked4 {
    Result<usize, BasicError>.Failure {error} => error == BasicError.InvalidPassword
    Result<usize, BasicError>.Success {value} => false
  }
  return colon && rejected && untouched[0] == 165 && untouched[1] == 165
}

fn origin<'text>(text: string<'text>, authority: &[u8], equivalent: string, wrong: string) -> bool {
  let checked5 = Uri.parse(text)
  let parsed = match move checked5 {
    Result<Uri<'text>, ParseError>.Failure {error} => { return false }
    Result<Uri<'text>, ParseError>.Success {value} => value
  }
  let checked6 = Origin.fromUri(&parsed)
  let originValue = match move checked6 {
    Result<Origin, OriginError>.Failure {error} => { return false }
    Result<Origin, OriginError>.Success {value} => value
  }
  let mut buffer: [u8; 32] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  let checked7 = Origin.authorityInto(&originValue, &mut buffer)
  let count = match move checked7 {
    Result<usize, OriginError>.Failure {error} => { return false }
    Result<usize, OriginError>.Success {value} => value
  }
  return bytesEqual(Slice.view<u8>(&buffer, usize.ZERO, count), authority)
    && Origin.matchesAuthority(&originValue, equivalent)
    && !Origin.matchesAuthority(&originValue, wrong)
}

fn rejects<'text>(text: string<'text>) -> bool {
  let checked8 = Uri.parse(text)
  let parsed = match move checked8 {
    Result<Uri<'text>, ParseError>.Failure {error} => { return false }
    Result<Uri<'text>, ParseError>.Success {value} => value
  }
  let checked9 = Origin.fromUri(&parsed)
  return match move checked9 {
    Result<Origin, OriginError>.Failure {error} => true
    Result<Origin, OriginError>.Success {value} => false
  }
}

pub fn main() -> i32 {
  if !basic() { return 1 }
  if !origin("HTTPS://EXAMPLE.com:443/path?q=1#fragment", b"example.com", "example.COM:443", "other.com") { return 2 }
  if !origin("http://[2001:db8::1]:8080/", b"[2001:db8::1]:8080", "[2001:0db8::1]:8080", "[2001:db8::1]") { return 3 }
  if !origin("http://127.0.0.1/", b"127.0.0.1", "127.0.0.1:80", "127.0.0.1:81") { return 4 }
  if !rejects("http://user@example.com/") || !rejects("http://example.com:0/") || !rejects("ftp://example.com/") { return 5 }
  return 42
}`
