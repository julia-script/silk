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
  let mut allocator = Allocator.systemAllocatorProvider()
  return run Effect.catchAll(program(), recover) |> Effect.provideMut<Allocator>(&mut allocator)
}
`
