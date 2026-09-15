export const httpProxyPolicyAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.effect {Effect}
import silk.http {Header, Method, Version}
import silk.http_headers {Headers, Limits}
import silk.http_origin {Origin, OriginError}
import silk.http_proxy {
  BypassPolicy,
  ProxyAuth,
  ProxyAuthContextId,
  ProxyComponent,
  ProxyConfig,
  ProxyConfigId,
  ProxyError,
  ProxyLimit,
  ProxyReason,
  Route,
  RouteKey,
  RouteMode,
  prepareForward,
  selectRoute,
}
import silk.http_request as Request
import silk.http_request {
  BodyMode,
  HeaderPolicy,
  PreparedRequest,
  RequestError,
  RoutedMode,
  RoutedProxyAuthorization,
}
import silk.http_target {RequestTarget}
import silk.result {Result}
import silk.uri {Uri}
import silk.uri_reference {ParseError}
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

fn valueLimits() -> Limits {
  return Limits {
    maxMethodBytes: 32,
    maxTargetBytes: 256,
    maxNameBytes: 64,
    maxValueBytes: 512,
    maxFields: 16,
    maxFieldBytes: 2048,
    maxOwnedBytes: 4096,
  }
}

fn checkedOrigin<'text>(text: string<'text>) -> Result<Origin, ProxyError> {
  let uri = match move Uri.parse(text) {
    Result<Uri<'text>, ParseError>.Failure {error} => {
      return Result.failResult<Origin, ProxyError>(ProxyError {
        component: ProxyComponent.Configuration,
        reason: ProxyReason.InvalidProxyUri,
      })
    }
    Result<Uri<'text>, ParseError>.Success {value} => value
  }
  return match move Origin.fromUri(&uri) {
    Result<Origin, OriginError>.Failure {error} => Result.failResult<Origin, ProxyError>(ProxyError {
      component: ProxyComponent.Configuration,
      reason: ProxyReason.InvalidProxyUri,
    })
    Result<Origin, OriginError>.Success {value} => Result.succeed<Origin, ProxyError>(value)
  }
}

fn modeIs<'configuration>(route: &Route<'configuration>, expected: RouteMode) -> bool {
  return Route.mode(route) == expected
}

fn isReason(error: ProxyError, expected: ProxyReason) -> bool {
  return match move expected {
    ProxyReason.InvalidProxyUri => match move error.reason {
      ProxyReason.InvalidProxyUri => true
      _ => false
    }
    ProxyReason.InvalidAuthentication => match move error.reason {
      ProxyReason.InvalidAuthentication => true
      _ => false
    }
    ProxyReason.UnsupportedProxyTransport => match move error.reason {
      ProxyReason.UnsupportedProxyTransport => true
      _ => false
    }
    _ => false
  }
}

fn isOwnedCapacityFailure(error: ProxyError) -> bool {
  return match move error.reason {
    ProxyReason.LimitExceeded {limit, allowed, attempted} => {
      return limit == ProxyLimit.OwnedConfigurationBytes && allowed == 0 && attempted > 0
    }
    _ => false
  }
}

effect fn verify() -> bool ! OutOfMemoryError ? &mut Allocator {
  let bypassOrigin = match move checkedOrigin("http://[2001:0db8::1]") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let bypassEntries = [bypassOrigin]
  let bypass = match move run BypassPolicy.copy(&bypassEntries) {
    Result<BypassPolicy, ProxyError>.Failure {error} => { return false }
    Result<BypassPolicy, ProxyError>.Success {value} => value
  }
  let configId = ProxyConfigId.make(11)
  let authId = ProxyAuthContextId.make(21)
  let authentication = match move run ProxyAuth.preparedBasic(
    authId,
    b"QWxhZGRpbjpvcGVuIHNlc2FtZQ==",
  ) {
    Result<ProxyAuth, ProxyError>.Failure {error} => { return false }
    Result<ProxyAuth, ProxyError>.Success {value} => value
  }
  let config = match move ProxyConfig.fromUri(
    configId,
    "http://proxy.example:3128",
    move authentication,
    move bypass,
    65536,
  ) {
    Result<ProxyConfig, ProxyError>.Failure {error} => { return false }
    Result<ProxyConfig, ProxyError>.Success {value} => value
  }

  let numericEquivalent = match move checkedOrigin("http://[2001:db8::1]:80") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let direct = selectRoute(&config, numericEquivalent)
  if !modeIs(&direct, RouteMode.Direct) { return false }
  if !ProxyConfigId.equals(&Route.configId(&direct), &configId) { return false }
  if !ProxyAuthContextId.equals(&Route.authContextId(&direct), &authId) { return false }

  let insecure = match move checkedOrigin("http://service.example") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let forward = selectRoute(&config, insecure)
  if !modeIs(&forward, RouteMode.Forward) { return false }
  let proxyOrigin = match move checkedOrigin("http://proxy.example:3128") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let equalEndpointRoute = selectRoute(&config, proxyOrigin)
  if !modeIs(&equalEndpointRoute, RouteMode.Forward) { return false }

  let secure = match move checkedOrigin("https://service.example") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let tunnel = selectRoute(&config, secure)
  if !modeIs(&tunnel, RouteMode.Tunnel) { return false }

  let differentScheme = match move checkedOrigin("https://[2001:db8::1]") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let schemeRoute = selectRoute(&config, differentScheme)
  if !modeIs(&schemeRoute, RouteMode.Tunnel) { return false }
  let differentPort = match move checkedOrigin("http://[2001:db8::1]:81") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let portRoute = selectRoute(&config, differentPort)
  if !modeIs(&portRoute, RouteMode.Forward) { return false }

  let recomputed = Route.recompute(&forward, secure)
  if !modeIs(&recomputed, RouteMode.Tunnel) { return false }
  if !ProxyAuthContextId.equals(&Route.authContextId(&recomputed), &authId) { return false }
  let copiedRoute = direct
  if !RouteKey.equals(&Route.key(&direct), &Route.key(&copiedRoute)) { return false }

  let forwardUri = match move Uri.parse("http://example.com/a?b#discard") {
    Result<Uri<'static>, ParseError>.Failure {error} => { return false }
    Result<Uri<'static>, ParseError>.Success {value} => value
  }
  let forwardOrigin = match move Origin.fromUri(&forwardUri) {
    Result<Origin, OriginError>.Failure {error} => { return false }
    Result<Origin, OriginError>.Success {value} => value
  }
  let selectedForward = selectRoute(&config, forwardOrigin)
  let emptyFields: [Header<'static>; 0] = []
  let emptyHeaders = match move Headers.make(&emptyFields, valueLimits()) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  let headerPolicy = HeaderPolicy.defaults()
  let forwarded = match move run Effect.result(prepareForward(
    &selectedForward,
    &forwardUri,
    Version.Http11,
    Method.get(),
    &emptyHeaders,
    &headerPolicy,
    BodyMode.Empty,
    false,
    valueLimits(),
    4096,
    128,
  )) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  if !bytesEqual(
    PreparedRequest.bytes(&forwarded),
    b"GET http://example.com/a?b HTTP/1.1\\r\\nProxy-Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==\\r\\nHost: example.com\\r\\nUser-Agent: silk-http/1\\r\\nAccept: */*\\r\\n\\r\\n",
  ) {
    return false
  }
  if !Origin.equals(&PreparedRequest.origin(&forwarded), &forwardOrigin) { return false }
  let expectedProxyPeer = match move checkedOrigin("http://proxy.example:3128") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  if !Origin.equals(&PreparedRequest.physicalPeer(&forwarded), &expectedProxyPeer) { return false }
  if Origin.equals(&PreparedRequest.origin(&forwarded), &PreparedRequest.physicalPeer(&forwarded)) {
    return false
  }

  let connectTarget = match move RequestTarget.parse(&Method.connect(), "service.example:443", 256) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  let tunnelAdmission = match move run Effect.result(Request.prepareRouted(
    &secure,
    &expectedProxyPeer,
    RoutedMode.Connect,
    RoutedProxyAuthorization<'static>.Absent,
    Version.Http11,
    Method.connect(),
    connectTarget,
    &emptyHeaders,
    &headerPolicy,
    BodyMode.Empty,
    false,
    valueLimits(),
    4096,
    128,
  )) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  if !Origin.equals(&PreparedRequest.origin(&tunnelAdmission), &secure) { return false }
  if !Origin.equals(&PreparedRequest.physicalPeer(&tunnelAdmission), &expectedProxyPeer) { return false }
  if Origin.equals(&PreparedRequest.origin(&tunnelAdmission), &PreparedRequest.physicalPeer(&tunnelAdmission)) {
    return false
  }

  let proxyField = match move Header.make("Proxy-Authorization", b"Basic override", valueLimits()) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  let proxyFields = [proxyField]
  let proxyHeaders = match move Headers.make(&proxyFields, valueLimits()) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  let directTarget = match move RequestTarget.parse(&Method.get(), "/", 256) {
    Result.Failure {error} => { return false }
    Result.Success {value} => value
  }
  let rejectedDirect = run Effect.result(Request.prepare(
    &forwardOrigin,
    Version.Http11,
    Method.get(),
    directTarget,
    &proxyHeaders,
    &headerPolicy,
    BodyMode.Empty,
    false,
    valueLimits(),
    4096,
    128,
  ))
  match move rejectedDirect {
    Result.Failure {error} => match move error {
      RequestError.ProxyAuthorization => {}
      _ => { return false }
    }
    Result.Success {value} => { return false }
  }
  let rejectedOverride = run Effect.result(prepareForward(
    &selectedForward,
    &forwardUri,
    Version.Http11,
    Method.get(),
    &proxyHeaders,
    &headerPolicy,
    BodyMode.Empty,
    false,
    valueLimits(),
    4096,
    128,
  ))
  match move rejectedOverride {
    Result.Failure {error} => match move error {
      RequestError.ProxyAuthorization => {}
      _ => { return false }
    }
    Result.Success {value} => { return false }
  }

  let emptyOrigins: [Origin; 0] = []
  let emptyForHttps = match move run BypassPolicy.copy(&emptyOrigins) {
    Result<BypassPolicy, ProxyError>.Failure {error} => { return false }
    Result<BypassPolicy, ProxyError>.Success {value} => value
  }
  match move ProxyConfig.fromUri(
    ProxyConfigId.make(12),
    "https://proxy.example:443",
    ProxyAuth.none(ProxyAuthContextId.make(22)),
    move emptyForHttps,
    65536,
  ) {
    Result<ProxyConfig, ProxyError>.Success {value} => { return false }
    Result<ProxyConfig, ProxyError>.Failure {error} => {
      if !isReason(move error, ProxyReason.UnsupportedProxyTransport) { return false }
    }
  }

  let emptyForPath = match move run BypassPolicy.copy(&emptyOrigins) {
    Result<BypassPolicy, ProxyError>.Failure {error} => { return false }
    Result<BypassPolicy, ProxyError>.Success {value} => value
  }
  match move ProxyConfig.fromUri(
    ProxyConfigId.make(13),
    "http://proxy.example/path",
    ProxyAuth.none(ProxyAuthContextId.make(23)),
    move emptyForPath,
    65536,
  ) {
    Result<ProxyConfig, ProxyError>.Success {value} => { return false }
    Result<ProxyConfig, ProxyError>.Failure {error} => {
      if !isReason(move error, ProxyReason.InvalidProxyUri) { return false }
    }
  }

  match move run ProxyAuth.preparedBasic(ProxyAuthContextId.make(24), b"not base64") {
    Result<ProxyAuth, ProxyError>.Success {value} => { return false }
    Result<ProxyAuth, ProxyError>.Failure {error} => {
      if !isReason(move error, ProxyReason.InvalidAuthentication) { return false }
    }
  }

  let emptyForCapacity = match move run BypassPolicy.copy(&emptyOrigins) {
    Result<BypassPolicy, ProxyError>.Failure {error} => { return false }
    Result<BypassPolicy, ProxyError>.Success {value} => value
  }
  match move ProxyConfig.fromUri(
    ProxyConfigId.make(14),
    "http://proxy.example:3128",
    ProxyAuth.none(ProxyAuthContextId.make(25)),
    move emptyForCapacity,
    0,
  ) {
    Result<ProxyConfig, ProxyError>.Success {value} => { return false }
    Result<ProxyConfig, ProxyError>.Failure {error} => {
      if !isOwnedCapacityFailure(move error) { return false }
    }
  }

  let emptyForRotated = match move run BypassPolicy.copy(&emptyOrigins) {
    Result<BypassPolicy, ProxyError>.Failure {error} => { return false }
    Result<BypassPolicy, ProxyError>.Success {value} => value
  }
  let rotated = match move ProxyConfig.fromUri(
    configId,
    "http://proxy.example:3128",
    ProxyAuth.none(ProxyAuthContextId.make(99)),
    move emptyForRotated,
    65536,
  ) {
    Result<ProxyConfig, ProxyError>.Failure {error} => { return false }
    Result<ProxyConfig, ProxyError>.Success {value} => value
  }
  let rotatedRoute = selectRoute(&rotated, insecure)
  let retainedRoute = Route.recompute(&forward, insecure)
  if !ProxyAuthContextId.equals(&Route.authContextId(&retainedRoute), &authId) { return false }
  return !RouteKey.equals(&Route.key(&forward), &Route.key(&rotatedRoute))
}

effect fn recover(error: OutOfMemoryError) -> bool { return false }

pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  let checked = verify() |> Effect.provideMut<Allocator>(&mut allocator)
  if run Effect.catchAll(checked, recover) { return 42 }
  return 0
}
`
