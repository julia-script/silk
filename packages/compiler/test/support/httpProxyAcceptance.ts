export const httpProxyPolicyAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.effect {Effect}
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
  selectRoute,
}
import silk.result {Result}
import silk.uri {Uri}
import silk.uri_reference {ParseError}

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
  let config = match move ProxyConfig.fromUri(
    configId,
    "http://proxy.example:3128",
    ProxyAuth.none(authId),
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

  let differentScheme = match move checkedOrigin("https://127.0.0.1") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let schemeRoute = selectRoute(&config, differentScheme)
  if !modeIs(&schemeRoute, RouteMode.Tunnel) { return false }
  let differentPort = match move checkedOrigin("http://127.0.0.1:81") {
    Result<Origin, ProxyError>.Failure {error} => { return false }
    Result<Origin, ProxyError>.Success {value} => value
  }
  let portRoute = selectRoute(&config, differentPort)
  if !modeIs(&portRoute, RouteMode.Forward) { return false }

  let recomputed = Route.recompute(&forward, &config, secure)
  if !modeIs(&recomputed, RouteMode.Tunnel) { return false }
  let copiedRoute = direct
  if !RouteKey.equals(&Route.key(&direct), &Route.key(&copiedRoute)) { return false }

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
