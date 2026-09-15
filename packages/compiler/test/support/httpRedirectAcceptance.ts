import {
  httpProxyPolicyCommonImports,
  httpProxyPolicyImports,
  httpProxyPolicySupport,
  verifyProxyPolicy,
} from './httpProxyAcceptance.js'

export const httpRedirectPolicyImports = `import silk.http_headers {
  Limits as RedirectHeaderLimits,
  OwnedHeaders,
}
import silk.http {ValueError}
import silk.http_client {ContinuePolicy}
import silk.http_redirect {
  BodyChunk,
  BodyDecision,
  BodyProducer,
  BodySource,
  CrossOriginPolicy,
  DowngradePolicy,
  History,
  HistoryLimits,
  Mode as RedirectMode,
  NameList,
  NameListLimits,
  Policy as RedirectPolicy,
  Post301302Policy,
  PreviousResponsePolicy,
  ProducerHandler,
  RedirectComponent,
  RedirectContext,
  RedirectError,
  RedirectLimit,
  RedirectReason,
  ReplayFactory,
  Request as RedirectRequest,
  StatusDecision,
  admitOrigin,
  admitReplay,
  crossOriginHeaderPolicy,
  resolveLocation,
  sanitizeHeaders,
  statusDecision,
  transition,
}
import silk.http_origin {OriginError as RedirectOriginError}
import silk.string {String}
import silk.system_clock {SystemClock}
import silk.uri {OwnedUri}
import silk.uri_reference {ParseError as RedirectParseError}
`

export const httpRedirectPolicySupport = `enum RedirectFactoryFailure { Failed }
enum RedirectProducerFailure { Failed }

service RedirectFactoryRequirement {}
service RedirectProducerRequirement {}

struct RedirectProducer { offset: usize }

impl BodyProducer<RedirectProducerFailure ? &mut RedirectProducerRequirement> for RedirectProducer {
  fn mode(producer: &Self) -> BodyMode {
    drop producer
    return BodyMode.KnownLength {length: usize.ONE}
  }

  effect fn pull(producer: &mut Self, output: &mut [u8]) -> BodyChunk
  ! RedirectProducerFailure
  ? &mut RedirectProducerRequirement {
    if producer.offset == usize.ZERO && output.length > usize.ZERO {
      output[usize.ZERO] = 120
      producer.offset = usize.ONE
      return BodyChunk {length: usize.ONE, end: true}
    }
    return BodyChunk {length: usize.ZERO, end: true}
  }
}

struct RedirectProducerUse {}

impl ProducerHandler<
  RedirectProducer,
  i32,
  RedirectProducerFailure ? &mut RedirectProducerRequirement,
> for RedirectProducerUse {
  effect<'call> fn handle<'call>(handler: Self, producer: &'call mut RedirectProducer) -> i32
  ! RedirectProducerFailure
  ? &mut RedirectProducerRequirement {
    drop handler
    let mut output: [u8; 1] = [0]
    let chunk = run BodyProducer<
      RedirectProducerFailure ? &mut RedirectProducerRequirement
    >.pull(producer, &mut output)
    if chunk.length == usize.ONE && chunk.end && output[usize.ZERO] == 120 { return 17 }
    return -1
  }
}

struct RedirectFactory {}

impl<A, E, ?R, H: ProducerHandler<RedirectProducer, A, E ? R>> RedirectFactory {
  effect fn withProducer(factory: &mut Self, handler: H) -> A
  ! RedirectFactoryFailure | E
  ? &mut RedirectFactoryRequirement | R {
    drop factory
    let mut producer = RedirectProducer {offset: usize.ZERO}
    return run ProducerHandler<RedirectProducer, A, E ? R>.handle(move handler, &mut producer)
  }
}

impl<A, E, ?R, H: ProducerHandler<RedirectProducer, A, E ? R>> ReplayFactory<
  RedirectProducer,
  A,
  RedirectFactoryFailure,
  E,
  &mut RedirectFactoryRequirement,
  R,
  H,
> for RedirectFactory {
  withProducer: RedirectFactory.withProducer
}

pub effect fn redirectReplayRowWitness(factory: &mut RedirectFactory) -> i32
! RedirectFactoryFailure | RedirectProducerFailure
? &mut RedirectFactoryRequirement | &mut RedirectProducerRequirement {
  return run ReplayFactory<
    RedirectProducer,
    i32,
    RedirectFactoryFailure,
    RedirectProducerFailure,
    &mut RedirectFactoryRequirement,
    &mut RedirectProducerRequirement,
    RedirectProducerUse,
  >.withProducer(factory, RedirectProducerUse {})
}

fn redirectStatus(code: u16) -> Status {
  return match move Status.fromCode(code) {
    Result.Failure {error} => {
      drop error
      redirectStatus(200)
    }
    Result.Success {value} => value
  }
}

fn redirectOrigin(text: string) -> Option<Origin> {
  let uri = match move Uri.parse(text) {
    Result.Failure {error} => {
      drop error
      return Option.none<Origin>()
    }
    Result.Success {value} => value
  }
  return match move Origin.fromUri(&uri) {
    Result.Failure {error} => {
      drop error
      Option.none<Origin>()
    }
    Result.Success {value} => Option.some<Origin>(value)
  }
}

effect fn redirectTakeValue<T>(result: Result<T, ValueError>) -> T ! ValueError {
  return match move result {
    Result.Failure {error} => { fail move error }
    Result.Success {value} => move value
  }
}

effect fn redirectTakeRedirect<T>(result: Result<T, RedirectError>) -> T ! RedirectError {
  return match move result {
    Result.Failure {error} => { fail move error }
    Result.Success {value} => move value
  }
}

effect fn redirectTakeParse<T>(result: Result<T, RedirectParseError>) -> T ! RedirectParseError {
  return match move result {
    Result.Failure {error} => { fail move error }
    Result.Success {value} => move value
  }
}

effect fn redirectTakeOrigin<T>(result: Result<T, RedirectOriginError>) -> T ! RedirectOriginError {
  return match move result {
    Result.Failure {error} => { fail move error }
    Result.Success {value} => move value
  }
}

enum RedirectReasonTag {
  NameListOverlap,
  SizeOverflow,
  LocationMissing,
  LocationAmbiguous,
  LocationInvalid,
  RedirectSchemeDenied,
  RedirectOriginDenied,
  DowngradeDenied,
  HopLimit,
  RedirectLoop,
  ReplayUnavailable,
}

struct RedirectLocationCase {
  current: string<'static>
  location: &'static [u8]
  expected: string<'static>
}

impl Copy for RedirectLocationCase {}

fn redirectReasonIs(actual: RedirectReason, expected: RedirectReasonTag) -> bool {
  return match move actual {
    RedirectReason.NameListOverlap => expected == RedirectReasonTag.NameListOverlap
    RedirectReason.SizeOverflow => expected == RedirectReasonTag.SizeOverflow
    RedirectReason.LocationMissing => expected == RedirectReasonTag.LocationMissing
    RedirectReason.LocationAmbiguous => expected == RedirectReasonTag.LocationAmbiguous
    RedirectReason.LocationInvalid => expected == RedirectReasonTag.LocationInvalid
    RedirectReason.RedirectSchemeDenied => expected == RedirectReasonTag.RedirectSchemeDenied
    RedirectReason.RedirectOriginDenied => expected == RedirectReasonTag.RedirectOriginDenied
    RedirectReason.DowngradeDenied => expected == RedirectReasonTag.DowngradeDenied
    RedirectReason.HopLimit => expected == RedirectReasonTag.HopLimit
    RedirectReason.RedirectLoop => expected == RedirectReasonTag.RedirectLoop
    RedirectReason.ReplayUnavailable => expected == RedirectReasonTag.ReplayUnavailable
    _ => false
  }
}

fn redirectContextIs(
  context: RedirectContext,
  hop: usize,
  status: Option<u16>,
  hasCurrentUri: bool,
  hasNextUri: bool,
) -> bool {
  let RedirectContext {hop: actualHop, status: actualStatus, currentUri, nextUri} = move context
  let statusMatches = match move status {
    Option.None => match move actualStatus {
      Option.None => true
      Option.Some {value} => {
        drop value
        false
      }
    }
    Option.Some {value: expected} => match move actualStatus {
      Option.None => false
      Option.Some {value} => Status.code(&value) == expected
    }
  }
  let currentPresent = match move currentUri {
    Option.None => false
    Option.Some {value} => {
      drop value
      true
    }
  }
  let nextPresent = match move nextUri {
    Option.None => false
    Option.Some {value} => {
      drop value
      true
    }
  }
  return actualHop == hop && statusMatches
    && currentPresent == hasCurrentUri && nextPresent == hasNextUri
}

fn redirectFailureIs<T>(
  result: Result<T, RedirectError>,
  component: RedirectComponent,
  reason: RedirectReasonTag,
  hop: usize,
  status: Option<u16>,
) -> bool {
  return match move result {
    Result.Success {value} => {
      drop value
      false
    }
    Result.Failure {error} => {
      let RedirectError {component: actual, reason: actualReason, context} = move error
      return actual == component && redirectReasonIs(move actualReason, reason)
        && redirectContextIs(move context, hop, status, false, false)
    }
  }
}

fn redirectLimitFailureIs<T>(
  result: Result<T, RedirectError>,
  component: RedirectComponent,
  limit: RedirectLimit,
  allowed: usize,
  attempted: usize,
  hop: usize,
  status: Option<u16>,
) -> bool {
  return match move result {
    Result.Success {value} => {
      drop value
      false
    }
    Result.Failure {error} => {
      let RedirectError {component: actual, reason, context} = move error
      let reasonMatches = match move reason {
        RedirectReason.LimitExceeded {
          limit: actualLimit,
          allowed: actualAllowed,
          attempted: actualAttempted,
        } => actualLimit == limit && actualAllowed == allowed && actualAttempted == attempted
        _ => false
      }
      return actual == component && reasonMatches
        && redirectContextIs(move context, hop, status, false, false)
    }
  }
}

fn redirectDecisionIs(
  policy: &RedirectPolicy,
  status: Status,
  followedHops: usize,
  expected: StatusDecision,
) -> bool {
  return match move statusDecision(policy, status, followedHops) {
    Result.Failure {error} => {
      drop error
      false
    }
    Result.Success {value} => value == expected
  }
}

fn redirectTransitionIs(
  policy: &RedirectPolicy,
  status: Status,
  method: Method,
  expectedMethod: string,
  expectedBody: BodyDecision,
) -> bool {
  let selected = transition(policy, status, method)
  return Method.format(&selected.method) == expectedMethod && selected.body == expectedBody
}

fn redirectHeaderLimits() -> RedirectHeaderLimits {
  return RedirectHeaderLimits {
    maxMethodBytes: 32,
    maxTargetBytes: 512,
    maxNameBytes: 64,
    maxValueBytes: 512,
    maxFields: 32,
    maxFieldBytes: 4096,
    maxOwnedBytes: 8192,
  }
}

fn redirectBytesEqual(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index = usize.ZERO
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

effect fn redirectHeader(name: string<'static>, value: &'static [u8]) -> Header<'static>
! ValueError {
  return run redirectTakeValue(Header.make(name, value, redirectHeaderLimits()))
}

effect fn redirectHeaders(location: &'static [u8]) -> Headers<'static> ! ValueError {
  let entries = [run redirectHeader("Location", location)]
  return run redirectTakeValue(Headers.make(&entries, redirectHeaderLimits()))
}

effect fn redirectResolve(
  currentText: string,
  headers: &Headers,
  limit: usize,
) -> Result<OwnedUri, RedirectError>
! RedirectParseError | OutOfMemoryError
? &mut Allocator {
  let current = run redirectTakeParse(Uri.parse(currentText))
  return run resolveLocation(
    &current,
    headers,
    HistoryLimits {maxHops: 2, maxUriBytes: limit, maxHistoryBytes: 4096},
    usize.ONE,
    redirectStatus(302),
  )
}

effect fn redirectResolvedIs(
  current: string,
  headers: &Headers,
  limit: usize,
  expected: string,
) -> bool
! RedirectParseError | RedirectError | OutOfMemoryError
? &mut Allocator {
  let result = run redirectResolve(current, headers, limit)
  let owned = run redirectTakeRedirect(move result)
  let view = owned.view()
  return Uri.format(&view) == expected
}

fn redirectFormattedIs(headers: &Headers, output: &mut [u8], expected: &[u8]) -> bool {
  return match move Headers.formatInto(headers, output) {
    Result.Failure {error} => {
      drop error
      false
    }
    Result.Success {value} => value == expected.length && redirectBytesEqual(output, expected)
  }
}
`

export const redirectPolicyCompileWitness = `pub fn redirectPolicyCompileWitness() -> i32 {
  let defaults = RedirectPolicy.defaults()
  let limits = RedirectPolicy.historyLimits(&defaults)
  if RedirectPolicy.mode(&defaults) != RedirectMode.Manual
    || limits.maxHops != 10
    || limits.maxUriBytes != 8192
    || limits.maxHistoryBytes != 90112
    || RedirectPolicy.crossOrigin(&defaults) != CrossOriginPolicy.Deny
    || RedirectPolicy.downgrade(&defaults) != DowngradePolicy.Deny
    || RedirectPolicy.post301302(&defaults) != Post301302Policy.Preserve
    || NameList.count(RedirectPolicy.safeCustom(&defaults)) != usize.ZERO
    || NameList.count(RedirectPolicy.sensitive(&defaults)) != usize.ZERO { return 101 }
  match RedirectPolicy.previousResponse(&defaults) {
    PreviousResponsePolicy.Close => {}
    _ => { return 102 }
  }

  let s301 = redirectStatus(301)
  let s302 = redirectStatus(302)
  let s303 = redirectStatus(303)
  let s304 = redirectStatus(304)
  let s307 = redirectStatus(307)
  let s308 = redirectStatus(308)
  if !redirectDecisionIs(&defaults, s302, usize.MAX, StatusDecision.Final) { return 109 }
  if !redirectTransitionIs(&defaults, s301, Method.post(), "POST", BodyDecision.Retain)
    || !redirectTransitionIs(&defaults, s302, Method.post(), "POST", BodyDecision.Retain) {
    return 110
  }

  let follow = match move RedirectPolicy.make(
    RedirectMode.Follow,
    HistoryLimits {maxHops: 10, maxUriBytes: 8192, maxHistoryBytes: 90112},
    CrossOriginPolicy.Allow,
    DowngradePolicy.Deny,
    Post301302Policy.ToGet,
    PreviousResponsePolicy.Drain {
      maxDiscardWireBytes: 64,
      deadline: SystemClock.make(9, 0),
    },
    NameList.empty(),
    NameList.empty(),
  ) {
    Result.Failure {error} => {
      drop error
      return 111
    }
    Result.Success {value} => value
  }
  match RedirectPolicy.previousResponse(&follow) {
    PreviousResponsePolicy.Drain {maxDiscardWireBytes, deadline} => {
      if maxDiscardWireBytes != 64 || SystemClock.seconds(deadline) != 9 { return 112 }
    }
    _ => { return 112 }
  }
  let selected: [u16; 5] = [301, 302, 303, 307, 308]
  let mut selectedIndex = usize.ZERO
  while selectedIndex < selected.length {
    let selectedStatus = redirectStatus(selected[selectedIndex])
    if !redirectDecisionIs(&follow, selectedStatus, usize.ZERO, StatusDecision.Redirect) {
      return 113 + usize.toI32(selectedIndex)
    }
    selectedIndex = selectedIndex + usize.ONE
  }
  if !redirectDecisionIs(&follow, s304, usize.ZERO, StatusDecision.Final) { return 118 }
  if !redirectTransitionIs(&follow, s301, Method.post(), "GET", BodyDecision.Drop)
    || !redirectTransitionIs(&follow, s302, Method.post(), "GET", BodyDecision.Drop)
    || !redirectTransitionIs(&follow, s303, Method.post(), "GET", BodyDecision.Drop)
    || !redirectTransitionIs(&follow, s303, Method.head(), "HEAD", BodyDecision.Drop)
    || !redirectTransitionIs(&follow, s303, Method.patch(), "GET", BodyDecision.Drop)
    || !redirectTransitionIs(&follow, s301, Method.patch(), "PATCH", BodyDecision.Retain)
    || !redirectTransitionIs(&follow, s307, Method.post(), "POST", BodyDecision.Retain)
    || !redirectTransitionIs(&follow, s308, Method.patch(), "PATCH", BodyDecision.Retain) {
    return 119
  }

  let zero = match move RedirectPolicy.make(
    RedirectMode.Follow,
    HistoryLimits {maxHops: usize.ZERO, maxUriBytes: 64, maxHistoryBytes: 1024},
    CrossOriginPolicy.Deny,
    DowngradePolicy.Deny,
    Post301302Policy.Preserve,
    PreviousResponsePolicy.Close,
    NameList.empty(),
    NameList.empty(),
  ) {
    Result.Failure {error} => {
      drop error
      return 120
    }
    Result.Success {value} => value
  }
  if !redirectFailureIs(
    statusDecision(&zero, s301, usize.ZERO),
    RedirectComponent.Policy,
    RedirectReasonTag.HopLimit,
    usize.ZERO,
    Option.some<u16>(301),
  ) { return 121 }
  if !redirectFailureIs(RedirectPolicy.make(
    RedirectMode.Follow,
    HistoryLimits {maxHops: usize.MAX, maxUriBytes: 64, maxHistoryBytes: usize.MAX},
    CrossOriginPolicy.Deny,
    DowngradePolicy.Deny,
    Post301302Policy.Preserve,
    PreviousResponsePolicy.Close,
    NameList.empty(),
    NameList.empty(),
  ), RedirectComponent.History, RedirectReasonTag.SizeOverflow, usize.ZERO, Option.none<u16>()) {
    return 122
  }

  let empty: BodySource<'static, i32, i32> = BodySource.Empty
  let repeatable: BodySource<'static, i32, i32> = BodySource.RepeatableBytes {bytes: b"body"}
  let oneShot: BodySource<'static, i32, i32> = BodySource.OneShot {producer: 42}
  let replay: BodySource<'static, i32, RedirectFactory> = BodySource.ReplayFactory {
    factory: RedirectFactory {},
  }
  if let Result.Failure {error} = admitReplay(&empty, 9, BodyDecision.Retain) {
    drop error
    return 125
  }
  if let Result.Failure {error} = admitReplay(&repeatable, 9, BodyDecision.Retain) {
    drop error
    return 126
  }
  if let Result.Failure {error} = admitReplay(&replay, 9, BodyDecision.Retain) {
    drop error
    return 127
  }
  if let Result.Failure {error} = admitReplay(&oneShot, usize.ZERO, BodyDecision.Retain) {
    drop error
    return 127
  }
  if !redirectFailureIs(
    admitReplay(&oneShot, usize.ONE, BodyDecision.Retain),
    RedirectComponent.Body,
    RedirectReasonTag.ReplayUnavailable,
    usize.ZERO,
    Option.none<u16>(),
  ) { return 128 }
  if let Result.Failure {error} = admitReplay(&oneShot, usize.ONE, BodyDecision.Drop) {
    drop error
    return 130
  }

  let cleaned = crossOriginHeaderPolicy(HeaderPolicy {
    host: HeaderControl.Value {value: b"old.example"},
    userAgent: HeaderControl.Omit,
    accept: HeaderControl.Value {value: b"application/json"},
    authorization: Authorization.Basic {username: b"user", password: b"secret"},
    basicSecurity: BasicSecurity.AllowInsecureBasic,
    http10KeepAlive: true,
  })
  match cleaned.host { HeaderControl.Default => {} _ => { return 131 } }
  match cleaned.authorization { Authorization.Absent => {} _ => { return 132 } }
  match cleaned.userAgent { HeaderControl.Omit => {} _ => { return 133 } }
  match cleaned.accept {
    HeaderControl.Value {value} => { if !redirectBytesEqual(value, b"application/json") { return 134 } }
    _ => { return 135 }
  }

  let current = match move redirectOrigin("https://example.com") {
    Option.None => { return 136 }
    Option.Some {value} => value
  }
  let equivalent = match move redirectOrigin("https://EXAMPLE.com:443") {
    Option.None => { return 137 }
    Option.Some {value} => value
  }
  if !Origin.equals(&current, &equivalent) { return 140 }
  let changedUris: [string<'static>; 2] = ["https://example.com:444", "https://sub.example.com"]
  let mut changedIndex = usize.ZERO
  while changedIndex < changedUris.length {
    let changed = match move redirectOrigin(changedUris[changedIndex]) {
      Option.None => { return 141 }
      Option.Some {value} => value
    }
    if changedIndex == usize.ZERO && !redirectFailureIs(
      admitOrigin(&defaults, &current, &changed, usize.ONE, s302),
      RedirectComponent.Origin,
      RedirectReasonTag.RedirectOriginDenied,
      usize.ONE,
      Option.some<u16>(302),
    ) {
      return 142
    }
    match move admitOrigin(&follow, &current, &changed, usize.ONE, s302) {
      Result.Failure {error} => {
        drop error
        return 143
      }
      Result.Success {value} => { if !value { return 144 } }
    }
    changedIndex = changedIndex + usize.ONE
  }
  let downgraded = match move redirectOrigin("http://example.com") {
    Option.None => { return 145 }
    Option.Some {value} => value
  }
  if !redirectFailureIs(
    admitOrigin(&follow, &current, &downgraded, usize.ONE, s302),
    RedirectComponent.Origin,
    RedirectReasonTag.DowngradeDenied,
    usize.ONE,
    Option.some<u16>(302),
  ) {
    return 147
  }
  return 0
}`

export const verifyRedirectPolicy = `pub effect fn verifyRedirectPolicy() -> bool
! OutOfMemoryError | RedirectError | ValueError | RedirectParseError | RedirectOriginError
? &mut Allocator {
  let oneName = ["x-safe"]
  let nameLimits = NameListLimits {maxNames: 1, maxNameBytes: 6, maxOwnedBytes: 1024}
  let probe = run redirectTakeRedirect(run NameList.copy(&oneName, nameLimits))
  let exactOwned = NameList.ownedBytes(&probe)
  if NameList.count(&probe) != usize.ONE || !NameList.contains(&probe, "X-SAFE") { return false }
  let exact = run redirectTakeRedirect(run NameList.copy(
    &oneName,
    NameListLimits {maxNames: 1, maxNameBytes: 6, maxOwnedBytes: exactOwned},
  ))
  drop exact
  if !redirectLimitFailureIs(run NameList.copy(
    &oneName,
    NameListLimits {maxNames: 1, maxNameBytes: 6, maxOwnedBytes: exactOwned - usize.ONE},
  ), RedirectComponent.Policy, RedirectLimit.PolicyOwnedBytes, exactOwned - usize.ONE, exactOwned,
    usize.ZERO, Option.none<u16>()) { return false }
  if !redirectLimitFailureIs(run NameList.copy(
    &oneName,
    NameListLimits {maxNames: usize.ZERO, maxNameBytes: 6, maxOwnedBytes: exactOwned},
  ), RedirectComponent.Policy, RedirectLimit.PolicyNames, usize.ZERO, usize.ONE,
    usize.ZERO, Option.none<u16>()) { return false }
  if !redirectLimitFailureIs(run NameList.copy(
    &oneName,
    NameListLimits {maxNames: 1, maxNameBytes: 5, maxOwnedBytes: exactOwned},
  ), RedirectComponent.Policy, RedirectLimit.PolicyNameBytes, 5, 6,
    usize.ZERO, Option.none<u16>()) { return false }
  let overlapNames = ["X-SAFE"]
  let overlapSensitive = run redirectTakeRedirect(run NameList.copy(&overlapNames, nameLimits))
  if !redirectFailureIs(RedirectPolicy.make(
    RedirectMode.Follow,
    HistoryLimits {maxHops: 2, maxUriBytes: 128, maxHistoryBytes: 4096},
    CrossOriginPolicy.Allow,
    DowngradePolicy.Allow,
    Post301302Policy.Preserve,
    PreviousResponsePolicy.Close,
    move probe,
    move overlapSensitive,
  ), RedirectComponent.Policy, RedirectReasonTag.NameListOverlap, usize.ZERO,
    Option.none<u16>()) { return false }

  let locations = [
    RedirectLocationCase {
      current: "https://example/a#old",
      location: b"../b?x",
      expected: "https://example/b?x#old",
    },
    RedirectLocationCase {
      current: "https://example/b?x#old",
      location: b"#",
      expected: "https://example/b?x#",
    },
    RedirectLocationCase {
      current: "https://example/a?x#old",
      location: b"",
      expected: "https://example/a?x#old",
    },
    RedirectLocationCase {
      current: "https://example/a?x#old",
      location: b"?y",
      expected: "https://example/a?y#old",
    },
  ]
  let mut locationIndex = usize.ZERO
  while locationIndex < locations.length {
    let selected = locations[locationIndex]
    let selectedHeaders = run redirectHeaders(selected.location)
    if !run redirectResolvedIs(selected.current, &selectedHeaders, 128, selected.expected) {
      return false
    }
    locationIndex = locationIndex + usize.ONE
  }
  let relativeHeaders = run redirectHeaders(b"../b?x")
  let resolvedBytes = String.byteLength("https://example/b?x#old")
  if !redirectLimitFailureIs(run redirectResolve(
    "https://example/a#old",
    &relativeHeaders,
    resolvedBytes - usize.ONE,
  ), RedirectComponent.Uri, RedirectLimit.UriBytes, resolvedBytes - usize.ONE, resolvedBytes,
    usize.ONE, Option.some<u16>(302)) { return false }

  let noEntries: [Header<'static>; 0] = []
  let noHeaders = run redirectTakeValue(Headers.make(&noEntries, redirectHeaderLimits()))
  let requestUri = run redirectTakeParse(Uri.parse("https://example/a#start"))
  let request = RedirectRequest {
    uri: requestUri,
    method: Method.post(),
    headers: noHeaders,
    headerPolicy: HeaderPolicy.defaults(),
    version: Version.Http11,
    continuePolicy: ContinuePolicy.Disabled,
    limits: redirectHeaderLimits(),
    maxHeadBytes: 512,
    maxCredentialBytes: 128,
  }
  if Uri.format(&request.uri) != "https://example/a#start"
    || Method.format(&request.method) != "POST" { return false }
  let ownedContextUri = run requestUri.copy()
  let nextContextUri = run redirectTakeParse(Uri.parse("https://next.example/b#next"))
  let ownedNextUri = run nextContextUri.copy()
  let ownedContext = RedirectContext {
    hop: 3,
    status: Option.some<Status>(redirectStatus(302)),
    currentUri: Option.some<OwnedUri>(move ownedContextUri),
    nextUri: Option.some<OwnedUri>(move ownedNextUri),
  }
  drop request
  let currentContextCopied = match & ownedContext.currentUri {
    Option.None => false
    Option.Some {value} => {
      let view = value.view()
      Uri.format(&view) == "https://example/a#start"
    }
  }
  let nextContextCopied = match & ownedContext.nextUri {
    Option.None => false
    Option.Some {value} => {
      let view = value.view()
      Uri.format(&view) == "https://next.example/b#next"
    }
  }
  if !currentContextCopied || !nextContextCopied
    || !redirectContextIs(move ownedContext, 3, Option.some<u16>(302), true, true) {
    return false
  }
  if !redirectFailureIs(run redirectResolve("https://example/a", &noHeaders, 128),
    RedirectComponent.Location, RedirectReasonTag.LocationMissing, usize.ONE,
    Option.some<u16>(302)) { return false }
  let duplicates = [run redirectHeader("Location", b"/a"), run redirectHeader("location", b"/b")]
  let duplicateHeaders = run redirectTakeValue(Headers.make(&duplicates, redirectHeaderLimits()))
  if !redirectFailureIs(run redirectResolve("https://example/a", &duplicateHeaders, 128),
    RedirectComponent.Location, RedirectReasonTag.LocationAmbiguous, usize.ONE,
    Option.some<u16>(302)) { return false }
  let invalidHeaders = run redirectHeaders(b"/%")
  if !redirectFailureIs(run redirectResolve("https://example/a", &invalidHeaders, 128),
    RedirectComponent.Location, RedirectReasonTag.LocationInvalid, usize.ONE,
    Option.some<u16>(302)) { return false }
  let schemeHeaders = run redirectHeaders(b"ftp://example/a")
  if !redirectFailureIs(run redirectResolve("https://example/a", &schemeHeaders, 128),
    RedirectComponent.Origin, RedirectReasonTag.RedirectSchemeDenied, usize.ONE,
    Option.some<u16>(302)) { return false }

  let exactUri = "https://b.example/"
  let exactHeaders = run redirectHeaders(String.utf8Bytes(exactUri))
  if !run redirectResolvedIs(
    "https://example/a",
    &exactHeaders,
    String.byteLength(exactUri),
    exactUri,
  ) { return false }
  let current = run redirectTakeParse(Uri.parse("https://example/a"))
  let status = redirectStatus(302)
  if !redirectLimitFailureIs(run resolveLocation(
    &current,
    &exactHeaders,
    HistoryLimits {
      maxHops: 2,
      maxUriBytes: String.byteLength(exactUri) - usize.ONE,
      maxHistoryBytes: 4096,
    },
    usize.ONE,
    status,
  ), RedirectComponent.Location, RedirectLimit.UriBytes,
    String.byteLength(exactUri) - usize.ONE, String.byteLength(exactUri),
    usize.ONE, Option.some<u16>(302)) {
    return false
  }

  let firstUri = run redirectTakeParse(Uri.parse("https://EXAMPLE.com/a"))
  let sameTarget = run redirectTakeParse(Uri.parse("https://example.com:443/a#ignored"))
  let firstOrigin = run redirectTakeOrigin(Origin.fromUri(&firstUri))
  let equivalentOrigin = run redirectTakeOrigin(Origin.fromUri(&sameTarget))
  let historyLimits = HistoryLimits {maxHops: 2, maxUriBytes: 128, maxHistoryBytes: 4096}
  let mut history = run redirectTakeRedirect(run History.make(historyLimits))
  let insertedPost = run redirectTakeRedirect(
    run History.insert(&mut history, Method.post(), firstOrigin, &firstUri),
  )
  let insertedGet = run redirectTakeRedirect(
    run History.insert(&mut history, Method.get(), firstOrigin, &firstUri),
  )
  if !redirectFailureIs(
    run History.insert(&mut history, Method.get(), equivalentOrigin, &sameTarget),
    RedirectComponent.History,
    RedirectReasonTag.RedirectLoop,
    usize.ONE,
    Option.none<u16>(),
  ) { return false }

  let singleLimits = HistoryLimits {maxHops: usize.ZERO, maxUriBytes: 128, maxHistoryBytes: 4096}
  let mut probeHistory = run redirectTakeRedirect(run History.make(singleLimits))
  let insertedProbe = run redirectTakeRedirect(
    run History.insert(&mut probeHistory, Method.get(), firstOrigin, &firstUri),
  )
  let exactHistoryBytes = History.ownedBytes(&probeHistory)
  let mut exactHistory = run redirectTakeRedirect(run History.make(HistoryLimits {
    maxHops: usize.ZERO,
    maxUriBytes: 128,
    maxHistoryBytes: exactHistoryBytes,
  }))
  let insertedExact = run redirectTakeRedirect(
    run History.insert(&mut exactHistory, Method.get(), firstOrigin, &firstUri),
  )
  if History.ownedBytes(&exactHistory) != exactHistoryBytes { return false }
  let secondUri = run redirectTakeParse(Uri.parse("https://example.com/b"))
  if !redirectLimitFailureIs(
    run History.insert(&mut exactHistory, Method.get(), firstOrigin, &secondUri),
    RedirectComponent.History,
    RedirectLimit.HistoryEntries,
    usize.ONE,
    2,
    usize.ZERO,
    Option.none<u16>(),
  ) { return false }
  let mut belowHistory = run redirectTakeRedirect(run History.make(HistoryLimits {
    maxHops: usize.ZERO,
    maxUriBytes: 128,
    maxHistoryBytes: exactHistoryBytes - usize.ONE,
  }))
  if !redirectLimitFailureIs(
    run History.insert(&mut belowHistory, Method.get(), firstOrigin, &firstUri),
    RedirectComponent.History,
    RedirectLimit.HistoryBytes,
    exactHistoryBytes - usize.ONE,
    exactHistoryBytes,
    usize.ZERO,
    Option.none<u16>(),
  ) { return false }

  let safeNames = ["authorization", "cookie", "origin", "referer", "proxy-authorization", "x-safe"]
  let sensitiveNames = ["accept-language"]
  let listLimits = NameListLimits {maxNames: 8, maxNameBytes: 32, maxOwnedBytes: 1024}
  let safe = run redirectTakeRedirect(run NameList.copy(&safeNames, listLimits))
  let sensitive = run redirectTakeRedirect(run NameList.copy(&sensitiveNames, listLimits))
  let policy = run redirectTakeRedirect(RedirectPolicy.make(
    RedirectMode.Follow,
    historyLimits,
    CrossOriginPolicy.Allow,
    DowngradePolicy.Allow,
    Post301302Policy.Preserve,
    PreviousResponsePolicy.Close,
    move safe,
    move sensitive,
  ))
  let fields = [
    run redirectHeader("Connection", b"X-Hop"),
    run redirectHeader("X-Hop", b"remove"),
    run redirectHeader("Host", b"old.example"),
    run redirectHeader("Keep-Alive", b"timeout=5"),
    run redirectHeader("TE", b"trailers"),
    run redirectHeader("Trailer", b"X-End"),
    run redirectHeader("Transfer-Encoding", b"chunked"),
    run redirectHeader("Upgrade", b"websocket"),
    run redirectHeader("Proxy-Connection", b"keep-alive"),
    run redirectHeader("Proxy-Authorization", b"proxy-secret"),
    run redirectHeader("Authorization", b"origin-secret"),
    run redirectHeader("Cookie", b"cookie-secret"),
    run redirectHeader("Origin", b"https://old.example"),
    run redirectHeader("Referer", b"https://old.example/a"),
    run redirectHeader("X-Safe", b"safe-value"),
    run redirectHeader("x-safe", b"second"),
    run redirectHeader("Accept-Encoding", b"gzip"),
    run redirectHeader("Accept-Language", b"en"),
    run redirectHeader("Content-Type", b"text/plain"),
    run redirectHeader("Content-Encoding", b"identity"),
    run redirectHeader("Content-Language", b"en"),
    run redirectHeader("Content-Location", b"/old"),
    run redirectHeader("Digest", b"digest"),
    run redirectHeader("Content-Digest", b"content-digest"),
    run redirectHeader("Repr-Digest", b"repr-digest"),
    run redirectHeader("Expect", b"100-continue"),
    run redirectHeader("Content-Length", b"1"),
  ]
  let headers = run redirectTakeValue(Headers.make(&fields, redirectHeaderLimits()))
  let crossOwned = run redirectTakeValue(run sanitizeHeaders(
    &headers,
    &policy,
    true,
    BodyDecision.Retain,
    redirectHeaderLimits(),
  ))
  let cross = OwnedHeaders.view(&crossOwned)
  let mut retainOutput: [u8; 135] = [${Array.from({ length: 135 }, () => 0).join(', ')}]
  if !redirectFormattedIs(
    &cross,
    &mut retainOutput,
    b"X-Safe: safe-value\r\nx-safe: second\r\nAccept-Encoding: gzip\r\nContent-Type: text/plain\r\nContent-Encoding: identity\r\nContent-Language: en\r\n",
  ) { return false }

  let droppedOwned = run redirectTakeValue(run sanitizeHeaders(
    &headers,
    &policy,
    true,
    BodyDecision.Drop,
    redirectHeaderLimits(),
  ))
  let dropped = OwnedHeaders.view(&droppedOwned)
  let mut dropOutput: [u8; 59] = [${Array.from({ length: 59 }, () => 0).join(', ')}]
  if !redirectFormattedIs(
    &dropped,
    &mut dropOutput,
    b"X-Safe: safe-value\r\nx-safe: second\r\nAccept-Encoding: gzip\r\n",
  ) { return false }
  return true
}`

export const httpRedirectAffineEscapeDiagnosticSource = `struct RedirectEscapingProducerUse {}

impl ProducerHandler<
  RedirectProducer,
  &'static mut RedirectProducer,
  never ? never,
> for RedirectEscapingProducerUse {
  effect<'call> fn handle<'call>(
    handler: Self,
    producer: &'call mut RedirectProducer,
  ) -> &'static mut RedirectProducer {
    drop handler
    return producer
  }
}`

export const httpRedirectAffineDuplicationDiagnosticSource = `fn redirectDuplicateOneShot(
  source: BodySource<'static, RedirectProducer, RedirectFactory>,
) -> RedirectProducer {
  return match move source {
    BodySource.OneShot {producer} => {
      let duplicate = move producer
      drop duplicate
      return move producer
    }
    _ => RedirectProducer {offset: usize.ZERO}
  }
}`

export const httpRedirectPolicyFragments = `${httpRedirectPolicyImports}
${httpRedirectPolicySupport}
${redirectPolicyCompileWitness}
${verifyRedirectPolicy}`

export const httpProxyRedirectPolicyMain = `struct RedirectFactoryRequirementProvider {}
struct RedirectProducerRequirementProvider {}

impl RedirectFactoryRequirement for RedirectFactoryRequirementProvider {}
impl RedirectProducerRequirement for RedirectProducerRequirementProvider {}

effect fn recoverProxyPolicy(error: OutOfMemoryError) -> bool {
  drop error
  return false
}

effect fn recoverRedirectPolicy(
  error: OutOfMemoryError | RedirectError | ValueError | RedirectParseError | RedirectOriginError,
) -> bool {
  drop error
  return false
}

effect fn recoverRedirectReplay(
  error: RedirectFactoryFailure | RedirectProducerFailure,
) -> i32 {
  drop error
  return -1
}

pub fn main() -> i32 {
  let compileWitness = redirectPolicyCompileWitness()
  let mut allocator = Allocator.systemAllocatorProvider()
  let proxyWitness = run Effect.catchAll(
    verifyProxyPolicy() |> Effect.provideMut<Allocator>(&mut allocator),
    recoverProxyPolicy,
  )
  let redirectWitness = run Effect.catchAll(
    verifyRedirectPolicy() |> Effect.provideMut<Allocator>(&mut allocator),
    recoverRedirectPolicy,
  )
  let mut factory = RedirectFactory {}
  let mut factoryRequirement = RedirectFactoryRequirementProvider {}
  let mut producerRequirement = RedirectProducerRequirementProvider {}
  let replay = redirectReplayRowWitness(&mut factory)
    |> Effect.provideMut<RedirectFactoryRequirement>(&mut factoryRequirement)
    |> Effect.provideMut<RedirectProducerRequirement>(&mut producerRequirement)
  let replayWitness = run Effect.catchAll(move replay, recoverRedirectReplay)
  if proxyWitness && redirectWitness && compileWitness == 0 && replayWitness == 17 { return 42 }
  return 0
}`

export const httpProxyRedirectPolicyAcceptanceSource = `${httpProxyPolicyCommonImports}
${httpProxyPolicyImports}
${httpRedirectPolicyImports}
${httpProxyPolicySupport}
${httpRedirectPolicySupport}
${verifyProxyPolicy}
${redirectPolicyCompileWitness}
${verifyRedirectPolicy}
${httpProxyRedirectPolicyMain}`
