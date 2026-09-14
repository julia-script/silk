/** One compact request-preflight oracle; codec and URI parsing have separate lower-level suites. */
export const httpRequestAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.effect {Effect}
import silk.http {Method, Version, Header, ValueError}
import silk.http_headers {Headers, Limits}
import silk.http_request as Request
import silk.http_request {PreparedRequest, RequestError, HeaderPolicy, HeaderControl, BodyMode, Authorization, BasicSecurity}
import silk.uri {Uri}
import silk.result {Result}
import silk.usize
import silk.http_basic as Basic
import silk.http_basic {BasicError}
import silk.http_origin {Origin, OriginError}
import silk.uri_reference {ParseError}
import silk.slice {Slice}
import silk.http_client_native {Options, NativeClientError, preflight}
import silk.option {Option}
import silk.system_clock {Instant, SystemClock}

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

fn policyChecks() -> i32 {
  if !basic() { return 1 }
  if !origin("HTTPS://EXAMPLE.com:443/path?q=1#fragment", b"example.com", "example.COM:443", "other.com") { return 2 }
  if !origin("http://[2001:db8::1]:8080/", b"[2001:db8::1]:8080", "[2001:0db8::1]:8080", "[2001:db8::1]") { return 3 }
  if !origin("http://127.0.0.1/", b"127.0.0.1", "127.0.0.1:80", "127.0.0.1:81") { return 4 }
  if !rejects("http://user@example.com/") || !rejects("http://example.com:0/") || !rejects("ftp://example.com/") { return 5 }
  return 0
}

fn nativeOrigin(text: string) -> Origin {
 let uri = match move Uri.parse(text) { Result.Success {value} => value Result.Failure {error} => { let invalid = 1 / 0 return nativeOrigin(text) } }
 return match move Origin.fromUri(&uri) { Result.Success {value} => value Result.Failure {error} => { let invalid = 1 / 0 return nativeOrigin(text) } }
}
fn nativeAdmissionChecks() -> i32 {
 let numeric = nativeOrigin("http://127.0.0.1/")
 let dns = nativeOrigin("https://example.com/")
 let mut options = Options.defaults()
 options.deadline = Option.some<Instant>(SystemClock.make(7, 0))
 match move preflight(&numeric, &options) {
  Result.Success {value} => { drop value }
  Result.Failure {error} => match move error {
   NativeClientError.UnsupportedTarget => { return 0 }
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
 return match move preflight(&dns, &options) { Result.Success {value} => 0 Result.Failure {error} => 4 }
}

fn same(a: &[u8], b: &[u8]) -> bool {
  if a.length != b.length { return false }
  let mut index = usize.ZERO
  while index < a.length {
    if a[index] != b[index] { return false }
    index = index + usize.ONE
  }
  return true
}
fn limits() -> Limits {
  return Limits {maxMethodBytes: 32, maxTargetBytes: 256, maxNameBytes: 64,
    maxValueBytes: 512, maxFields: 16, maxFieldBytes: 2048, maxOwnedBytes: 4096}
}
effect fn program() -> i32 ! RequestError | OutOfMemoryError ? &mut Allocator {
  let uri = match move Uri.parse("http://EXAMPLE.com?x=1#discard") {
    Result.Failure {error} => { drop error return 1 }
    Result.Success {value} => value
  }
  let fields: [Header<'static>; 0] = []
  let headers = match move Headers.make(&fields, limits()) {
    Result.Failure {error} => { drop error return 2 }
    Result.Success {value} => value
  }
  let policy = HeaderPolicy.defaults()
  let prepared = run Request.fromUri(&uri, Version.Http11, Method.get(), &headers, &policy,
    BodyMode.Empty, false, limits(), 4096, 128)
  if !same(PreparedRequest.bytes(&prepared),
    b"GET /?x=1 HTTP/1.1\\r\\nHost: example.com\\r\\nUser-Agent: silk-http/1\\r\\nAccept: */*\\r\\n\\r\\n") { return 3 }
  let mut basic = HeaderPolicy.defaults()
  basic.authorization = Authorization.Basic {username: b"Aladdin", password: b"open sesame"}
  let insecure = run Effect.result(Request.fromUri(&uri, Version.Http11, Method.get(), &headers,
    &basic, BodyMode.Empty, false, limits(), 4096, 128))
  let rejected = match move insecure {
    Result.Failure {error} => match move error { RequestError.InsecureBasic => true _ => false }
    Result.Success {value} => false
  }
  if !rejected { return 4 }
  basic.basicSecurity = BasicSecurity.AllowInsecureBasic
  let explicit = run Request.fromUri(&uri, Version.Http11, Method.post(), &headers, &basic,
    BodyMode.KnownLength {length: 0}, false, limits(), 4096, 128)
  if !same(PreparedRequest.bytes(&explicit),
    b"POST /?x=1 HTTP/1.1\\r\\nHost: example.com\\r\\nUser-Agent: silk-http/1\\r\\nAccept: */*\\r\\nContent-Length: 0\\r\\nAuthorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==\\r\\n\\r\\n") { return 5 }
  let badMode = run Effect.result(Request.fromUri(&uri, Version.Http10, Method.post(), &headers,
    &policy, BodyMode.Chunked, false, limits(), 4096, 128))
  let badModeRejected = match move badMode {
    Result.Failure {error} => match move error { RequestError.InvalidBodyMode => true _ => false }
    Result.Success {value} => false
  }
  if !badModeRejected { return 6 }
  let mut wrongHost = HeaderPolicy.defaults()
  wrongHost.host = HeaderControl.Value {value: b"other.test"}
  let wrong = run Effect.result(Request.fromUri(&uri, Version.Http11, Method.get(), &headers,
    &wrongHost, BodyMode.Empty, false, limits(), 4096, 128))
  return match move wrong {
    Result.Failure {error} => match move error { RequestError.ConflictingAuthority => 0 _ => 7 }
    Result.Success {value} => { drop value return 8 }
  }
}
effect fn recover(error: RequestError | OutOfMemoryError) -> i32 { drop error return 9 }
pub fn main() -> i32 {
  let checked = policyChecks()
  if checked != 0 { return 100 + checked }
  let admitted = nativeAdmissionChecks()
  if admitted != 0 { return 200 + admitted }
  let mut allocator = Allocator.systemAllocatorProvider()
  return run Effect.catchAll(program(), recover) |> Effect.provideMut<Allocator>(&mut allocator)
}
`
