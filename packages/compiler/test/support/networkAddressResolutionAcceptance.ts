export const networkAddressValueAcceptanceSource = `
fn sameText(value: &Ipv4Address, expected: &[u8]) -> bool {
  let mut output: [u8; 15] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  return match move Ipv4Address.formatInto(value, &mut output) {
    Result<usize, AddressError>.Failure {error} => false
    Result<usize, AddressError>.Success {value: count} => {
      if count != expected.length { return false }
      let mut index = usize.ZERO
      while index < count {
        if output[index] != expected[index] { return false }
        index = index + usize.ONE
      }
      return true
    }
  }
}
fn sameV6Text(value: &Ipv6Address, expected: &[u8]) -> bool {
  let mut output: [u8; 39] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  return match move Ipv6Address.formatInto(value, &mut output) {
    Result<usize, AddressError>.Failure {error} => false
    Result<usize, AddressError>.Success {value: count} => {
      if count != expected.length { return false }
      let mut index = usize.ZERO
      while index < count {
        if output[index] != expected[index] { return false }
        index = index + usize.ONE
      }
      return true
    }
  }
}
fn acceptedV4(input: &[u8], expected: &[u8]) -> bool {
  return match move Ipv4Address.parse(input) {
    Result<Ipv4Address, AddressError>.Failure {error} => false
    Result<Ipv4Address, AddressError>.Success {value} => sameText(&value, expected)
  }
}
fn rejectedV4(input: &[u8]) -> bool {
  return match move Ipv4Address.parse(input) {
    Result<Ipv4Address, AddressError>.Failure {error} => true
    Result<Ipv4Address, AddressError>.Success {value} => false
  }
}
fn acceptedV6(input: &[u8], expected: &[u8]) -> bool {
  return match move Ipv6Address.parse(input) {
    Result<Ipv6Address, AddressError>.Failure {error} => false
    Result<Ipv6Address, AddressError>.Success {value} => sameV6Text(&value, expected)
  }
}
fn rejectedV6(input: &[u8]) -> bool {
  return match move Ipv6Address.parse(input) {
    Result<Ipv6Address, AddressError>.Failure {error} => true
    Result<Ipv6Address, AddressError>.Success {value} => false
  }
}
fn acceptedDomain(input: &[u8]) -> bool {
  return match move DomainHost.make(input) {
    Result<DomainHost, AddressError>.Failure {error} => false
    Result<DomainHost, AddressError>.Success {value} => DomainHost.length(&value) == input.length
  }
}
fn rejectedDomain(input: &[u8]) -> bool {
  return match move DomainHost.make(input) {
    Result<DomainHost, AddressError>.Failure {error} => true
    Result<DomainHost, AddressError>.Success {value} => false
  }
}
pub fn main() -> i32 {
  if !acceptedV4(b"0.0.0.0", b"0.0.0.0") { return 1 }
  if !acceptedV4(b"192.0.2.255", b"192.0.2.255") { return 2 }
  if !rejectedV4(b"127.1") || !rejectedV4(b"2130706433") || !rejectedV4(b"0x7f.0.0.1") { return 3 }
  if !rejectedV4(b"01.2.3.4") || !rejectedV4(b"256.0.0.1") || !rejectedV4(b"1.2.3.4.5") { return 4 }
  if !acceptedV6(b"2001:0DB8:0:0:0:0:0:1", b"2001:db8::1") { return 5 }
  if !acceptedV6(b"2001:db8::1", b"2001:db8::1") { return 6 }
  if !acceptedV6(b"::ffff:192.0.2.1", b"::ffff:c000:201") { return 7 }
  if !acceptedV6(b"2001:0:0:1:0:0:1:1", b"2001::1:0:0:1:1") { return 8 }
  if !acceptedV6(b"2001:db8:0:1:1:1:1:1", b"2001:db8:0:1:1:1:1:1") { return 9 }
  if !rejectedV6(b"2001:::1") || !rejectedV6(b"1::2::3") || !rejectedV6(b"1:2:3") { return 10 }
  if !acceptedDomain(b"Example.COM") || !acceptedDomain(b"intranet") || !acceptedDomain(b"\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x61\x2e\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x62\x2e\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x63\x2e\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64\x64") { return 11 }
  if !rejectedDomain(b"") || !rejectedDomain(b"a..b") || !rejectedDomain(b"example.") { return 12 }
  if !rejectedDomain(b"-a.example") || !rejectedDomain(b"a-.example") || !rejectedDomain(b"example.123") { return 13 }
  let upper = match move DomainHost.make(b"Example.COM") {
    Result<DomainHost, AddressError>.Failure {error} => { return 14 }
    Result<DomainHost, AddressError>.Success {value} => value
  }
  let lower = match move DomainHost.make(b"example.com") {
    Result<DomainHost, AddressError>.Failure {error} => { return 15 }
    Result<DomainHost, AddressError>.Success {value} => value
  }
  if !DomainHost.equals(&upper, &lower) { return 16 }
  let zero = Port.make(0)
  let maximum = Port.make(65535)
  let overflow = Port.make(65536)
  match move zero {
    Result<Port, AddressError>.Failure {error} => { return 17 }
    Result<Port, AddressError>.Success {value} => {
      if Port.value(&value) != 0 { return 18 }
    }
  }
  match move maximum {
    Result<Port, AddressError>.Failure {error} => { return 19 }
    Result<Port, AddressError>.Success {value} => {
      if Port.value(&value) != 65535 { return 20 }
    }
  }
  match move overflow {
    Result<Port, AddressError>.Success {value} => { return 21 }
    Result<Port, AddressError>.Failure {error} => {}
  }
  match move Host.parse(b"192.0.2.1") {
    Result<Host, AddressError>.Failure {error} => { return 22 }
    Result<Host, AddressError>.Success {value} => match move value {
      Host.Ipv4 {value: numericV4} => {}
      Host.Domain {value: wrongDomain} => { return 23 }
      Host.Ipv6 {value: wrongV6} => { return 23 }
    }
  }
  match move Host.parse(b"[::ffff:192.0.2.1]") {
    Result<Host, AddressError>.Failure {error} => { return 24 }
    Result<Host, AddressError>.Success {value} => match move value {
      Host.Ipv6 {value: numericV6} => {}
      Host.Domain {value: wrongDomain} => { return 25 }
      Host.Ipv4 {value: wrongV4} => { return 25 }
    }
  }
  return 42
}
`

export const networkAddressPortableCorpusSource = `
import silk.network_address {AddressError, DomainHost, Host, Ipv4Address, Ipv6Address, Port}
import silk.result {Result}
import silk.usize

${networkAddressValueAcceptanceSource}
`

const resolverPolicyNativeSource = `
struct ProbeResolver { calls: usize }

impl Resolver for ProbeResolver {
  effect fn resolveDomain(
    self: &mut Self,
    request: &ResolveRequest,
    domain: &DomainHost,
  ) -> ResolvedEndpoints
  ! ResolverError | OutOfMemoryError
  ? &mut Allocator {
    self.calls = self.calls + usize.ONE
    return match move ResolvedEndpoints.make(ResolveRequest.maxResults(request)) {
      Result<ResolvedEndpoints, ResolverError>.Failure {error} => { fail move error }
      Result<ResolvedEndpoints, ResolverError>.Success {value} => value
    }
  }
}

struct ProbeClock { nowValue: Instant calls: usize }

impl MonotonicClock for ProbeClock {
  effect fn now(self: &mut Self) -> Instant {
    self.calls = self.calls + usize.ONE
    return SystemClock.make(
      SystemClock.seconds(&self.nowValue),
      SystemClock.nanoseconds(&self.nowValue),
    )
  }
  effect fn getResolution(self: &mut Self) -> u64 { return 1 }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () { return () }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () { return () }
}

effect fn numericRequest(
  family: FamilySelection,
  deadline: Option<Instant>,
) -> ResolveRequest ! ResolverError {
  return match move ResolveRequest.make(
    Host.Ipv4 {value: Ipv4Address.fromOctets([192, 0, 2, 1])},
    Port.fromU16(443),
    move family,
    2,
    move deadline,
  ) {
    Result<ResolveRequest, ResolverError>.Failure {error} => { fail move error }
    Result<ResolveRequest, ResolverError>.Success {value} => value
  }
}

fn timedOut(result: Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>) -> bool {
  return match move result {
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Success {value} => {
      drop value
      return false
    }
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Failure {error} => match move error {
      ResolverError.Timeout => { return true }
      _ => { return false }
    }
  }
}

fn noAddress(result: Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>) -> bool {
  return match move result {
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Success {value} => {
      drop value
      return false
    }
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Failure {error} => match move error {
      ResolverError.NoAddress => { return true }
      _ => { return false }
    }
  }
}

effect fn policyProgram() -> i32
! ResolverError | OutOfMemoryError {
  let request = run numericRequest(
    FamilySelection.Any,
    Option.some<Instant>(SystemClock.make(20, 0)),
  )
  let mut resolver = ProbeResolver {calls: usize.ZERO}
  let mut clock = ProbeClock {nowValue: SystemClock.make(10, 0), calls: usize.ZERO}
  let mut allocator = Allocator.systemAllocatorProvider()
  let resolved = run Resolver.resolve(&request)
    |> Effect.provideMut<Resolver>(&mut resolver)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  if ResolvedEndpoints.length(&resolved) != usize.ONE { return 1 }
  if resolver.calls != usize.ZERO { return 2 }
  if clock.calls != usize.ONE { return 3 }

  let equal = run numericRequest(
    FamilySelection.Any,
    Option.some<Instant>(SystemClock.make(20, 0)),
  )
  clock.nowValue = SystemClock.make(20, 0)
  let equalResult = run Effect.result(
    Resolver.resolve(&equal)
      |> Effect.provideMut<Resolver>(&mut resolver)
      |> Effect.provideMut<MonotonicClock>(&mut clock)
      |> Effect.provideMut<Allocator>(&mut allocator)
  )
  if !timedOut(move equalResult) { return 4 }

  let past = run numericRequest(
    FamilySelection.Any,
    Option.some<Instant>(SystemClock.make(20, 0)),
  )
  clock.nowValue = SystemClock.make(21, 0)
  let pastResult = run Effect.result(
    Resolver.resolve(&past)
      |> Effect.provideMut<Resolver>(&mut resolver)
      |> Effect.provideMut<MonotonicClock>(&mut clock)
      |> Effect.provideMut<Allocator>(&mut allocator)
  )
  if !timedOut(move pastResult) { return 5 }

  let filtered = run numericRequest(FamilySelection.V6, Option.none<Instant>())
  let filteredResult = run Effect.result(
    Resolver.resolve(&filtered)
      |> Effect.provideMut<Resolver>(&mut resolver)
      |> Effect.provideMut<MonotonicClock>(&mut clock)
      |> Effect.provideMut<Allocator>(&mut allocator)
  )
  if !noAddress(move filteredResult) { return 6 }
  if resolver.calls != usize.ZERO { return 7 }
  if clock.calls != 3 { return 8 }
  return 42
}

`

export const nativeResolverAcceptanceSource = `
import silk.effect {Effect}

struct NativeProbeClock {}
impl MonotonicClock for NativeProbeClock {
  effect fn now(self: &mut Self) -> Instant { return SystemClock.make(0, 0) }
  effect fn getResolution(self: &mut Self) -> u64 { return 1 }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () { return () }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () { return () }
}

effect fn nativeProgram() -> i32
! ResolverError | OutOfMemoryError {
  let domain = match move DomainHost.make(b"intranet") {
    Result<DomainHost, AddressError>.Failure {error} => { return 0 }
    Result<DomainHost, AddressError>.Success {value} => value
  }
  let request = match move ResolveRequest.make(
    Host.Domain {value: move domain},
    Port.fromU16(443),
    FamilySelection.Any,
    4,
    Option.none<Instant>(),
  ) {
    Result<ResolveRequest, ResolverError>.Failure {error} => { fail move error }
    Result<ResolveRequest, ResolverError>.Success {value} => value
  }
  let mut provider = NativeSystemResolver.make()
  let mut clock = NativeProbeClock {}
  let mut allocator = Allocator.systemAllocatorProvider()
  let resolved = run Resolver.resolve(&request)
    |> Effect.provideMut<Resolver>(&mut provider)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  return usize.toI32(ResolvedEndpoints.length(&resolved))
}

effect fn recoverNativeResolver(error: ResolverError) -> i32 ! OutOfMemoryError { return 0 }
effect fn recoverNativeAllocation(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 {
  return run nativeProgram()
    |> Effect.catch<ResolverError>(recoverNativeResolver)
    |> Effect.catchAll(recoverNativeAllocation)
}
`

export const deadlineResolverFixtureSource = `
import silk.effect {Effect}
import silk.execution {Execution}

struct HeldRegistration { wake: Intrinsic.Wake }
struct CompletedRegistration {}

fn holdRegistration(wake: Intrinsic.Wake) -> HeldRegistration {
  return HeldRegistration {wake: move wake}
}
fn completeRegistration(wake: Intrinsic.Wake) -> CompletedRegistration {
  Intrinsic.wake(move wake)
  return CompletedRegistration {}
}

struct DeterministicDeadlineResolver { complete: bool }

struct DeadlineFixtureClock {}
impl MonotonicClock for DeadlineFixtureClock {
  effect fn now(self: &mut Self) -> Instant { return SystemClock.make(0, 0) }
  effect fn getResolution(self: &mut Self) -> u64 { return 1 }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () { return () }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () { return () }
}

impl Resolver for DeterministicDeadlineResolver {
  effect fn resolveDomain(
    self: &mut Self,
    request: &ResolveRequest,
    domain: &DomainHost,
  ) -> ResolvedEndpoints
  ! ResolverError | OutOfMemoryError
  ? &mut Allocator {
    let made = ResolvedEndpoints.make(ResolveRequest.maxResults(request))
    let mut result = match move made {
      Result<ResolvedEndpoints, ResolverError>.Failure {error} => { fail move error }
      Result<ResolvedEndpoints, ResolverError>.Success {value} => value
    }
    let admitted = run ResolvedEndpoints.admit(
      &mut result,
      Endpoint.make(
        IpAddress.V4 {value: Ipv4Address.fromOctets([192, 0, 2, 9])},
        ResolveRequest.port(request),
      ),
    )
    if self.complete {
      run Execution.park(completeRegistration)
    } else {
      run Execution.park(holdRegistration)
    }
    return move result
  }
}

effect fn parkedFixture() -> i32
! ResolverError | OutOfMemoryError {
  let domain = match move DomainHost.make(b"fixture.invalid") {
    Result<DomainHost, AddressError>.Failure {error} => { return 0 }
    Result<DomainHost, AddressError>.Success {value} => value
  }
  let request = match move ResolveRequest.make(
    Host.Domain {value: domain},
    Port.fromU16(80),
    FamilySelection.Any,
    1,
    Option.some<Instant>(SystemClock.make(10, 0)),
  ) {
    Result<ResolveRequest, ResolverError>.Failure {error} => { fail move error }
    Result<ResolveRequest, ResolverError>.Success {value} => value
  }
  let mut resolver = DeterministicDeadlineResolver {complete: false}
  let mut clock = DeadlineFixtureClock {}
  let mut allocator = Allocator.systemAllocatorProvider()
  let result = run Resolver.resolve(&request)
    |> Effect.provideMut<Resolver>(&mut resolver)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
  return usize.toI32(ResolvedEndpoints.length(&result))
}

effect fn recoverFixtureResolver(error: ResolverError) -> i32 ! OutOfMemoryError { return 0 }
effect fn recoverFixtureAllocation(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 {
  return run parkedFixture()
    |> Effect.catch<ResolverError>(recoverFixtureResolver)
    |> Effect.catchAll(recoverFixtureAllocation)
}
`

export const nativeResolverStubSource = `
#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <stddef.h>
#include <string.h>

static int silk_mode = 0;
static int silk_calls = 0;
static int silk_frees = 0;
static int silk_arguments_ok = 0;
static struct addrinfo silk_nodes[3];
static struct sockaddr_in silk_addresses[3];

void silk_resolver_stub_reset(int mode) {
  silk_mode = mode;
  silk_calls = 0;
  silk_frees = 0;
  silk_arguments_ok = 0;
  memset(silk_nodes, 0, sizeof(silk_nodes));
  memset(silk_addresses, 0, sizeof(silk_addresses));
}

int silk_resolver_stub_calls(void) { return silk_calls; }
int silk_resolver_stub_frees(void) { return silk_frees; }
int silk_resolver_stub_arguments_ok(void) { return silk_arguments_ok; }

static void silk_address(size_t index, unsigned last) {
  silk_addresses[index].sin_family = AF_INET;
  silk_addresses[index].sin_addr.s_addr = htonl(0xc0000200u + last);
  silk_nodes[index].ai_family = AF_INET;
  silk_nodes[index].ai_socktype = SOCK_STREAM;
  silk_nodes[index].ai_protocol = IPPROTO_TCP;
  silk_nodes[index].ai_addrlen = (socklen_t)sizeof(struct sockaddr_in);
  silk_nodes[index].ai_addr = (struct sockaddr *)&silk_addresses[index];
}

int getaddrinfo(
  const char *node,
  const char *service,
  const struct addrinfo *hints,
  struct addrinfo **result
) {
  silk_calls += 1;
  silk_arguments_ok = node != NULL && strcmp(node, "intranet.") == 0
    && service != NULL && strcmp(service, "443") == 0
    && hints != NULL && hints->ai_family == AF_UNSPEC
    && hints->ai_socktype == SOCK_STREAM && hints->ai_protocol == IPPROTO_TCP
    && (hints->ai_flags & AI_NUMERICSERV) != 0 && result != NULL;
  if (silk_mode == 3) {
    errno = EIO;
    return EAI_SYSTEM;
  }
  silk_address(0, 1);
  silk_address(1, silk_mode == 0 ? 1 : 2);
  silk_nodes[0].ai_next = &silk_nodes[1];
  if (silk_mode == 0) {
    silk_address(2, 2);
    silk_nodes[1].ai_next = &silk_nodes[2];
  }
  if (silk_mode == 1) {
    silk_nodes[1].ai_addrlen = 1;
  }
  if (silk_mode == 2) {
    silk_address(2, 3);
    silk_nodes[1].ai_next = &silk_nodes[2];
  }
  *result = &silk_nodes[0];
  return 0;
}

void freeaddrinfo(struct addrinfo *result) {
  if (result != NULL) silk_frees += 1;
}
`

export const nativeResolverStubAcceptanceSource = `
import silk.allocator {Allocator, OutOfMemoryError}
import silk.effect {Effect}
import silk.layout {Layout}
import silk.monotonic_clock {MonotonicClock}
import silk.native_resolver {NativeSystemResolver}
import silk.network_address {AddressError, DomainHost, Host, Ipv4Address, Port}
import silk.option {Option}
import silk.resolver {FamilySelection, ResolveRequest, ResolvedEndpoints, Resolver, ResolverError}
import silk.result {Result}
import silk.system_clock {Instant, SystemClock}
import silk.u64
import silk.usize

${resolverPolicyNativeSource}

unsafe extern "C" fn silk_resolver_stub_reset(mode: i32) -> ()
unsafe extern "C" fn silk_resolver_stub_calls() -> i32
unsafe extern "C" fn silk_resolver_stub_frees() -> i32
unsafe extern "C" fn silk_resolver_stub_arguments_ok() -> i32

struct StubClock {}
impl MonotonicClock for StubClock {
  effect fn now(self: &mut Self) -> Instant { return SystemClock.make(0, 0) }
  effect fn getResolution(self: &mut Self) -> u64 { return 1 }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () { return () }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () { return () }
}

struct RefusingAllocator {}
effect fn allocate(self: &mut RefusingAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  return run Allocator.outOfMemory()
}
impl Allocator for RefusingAllocator { allocate: RefusingAllocator.allocate }

effect fn request(maximum: usize, deadline: Option<Instant>) -> ResolveRequest ! ResolverError {
  let domain = match move DomainHost.make(b"intranet") {
    Result<DomainHost, AddressError>.Failure {error} => {
      drop error
      fail ResolverError.UnsupportedForm
    }
    Result<DomainHost, AddressError>.Success {value} => value
  }
  return match move ResolveRequest.make(
    Host.Domain {value: move domain},
    Port.fromU16(443),
    FamilySelection.Any,
    maximum,
    move deadline,
  ) {
    Result<ResolveRequest, ResolverError>.Failure {error} => { fail move error }
    Result<ResolveRequest, ResolverError>.Success {value} => value
  }
}

effect fn resolveSystem(maximum: usize, deadline: Option<Instant>) -> ResolvedEndpoints
! ResolverError | OutOfMemoryError {
  let admitted = run request(maximum, move deadline)
  let mut resolver = NativeSystemResolver.make()
  let mut clock = StubClock {}
  let mut allocator = Allocator.systemAllocatorProvider()
  return run Resolver.resolve(&admitted)
    |> Effect.provideMut<Resolver>(&mut resolver)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
}

effect fn resolveRefused() -> ResolvedEndpoints ! ResolverError | OutOfMemoryError {
  let admitted = run request(2, Option.none<Instant>())
  let mut resolver = NativeSystemResolver.make()
  let mut clock = StubClock {}
  let mut allocator = RefusingAllocator {}
  return run Resolver.resolve(&admitted)
    |> Effect.provideMut<Resolver>(&mut resolver)
    |> Effect.provideMut<MonotonicClock>(&mut clock)
    |> Effect.provideMut<Allocator>(&mut allocator)
}

fn counters(calls: i32, frees: i32, arguments: i32) -> bool {
  let observedCalls = unsafe silk_resolver_stub_calls()
  let observedFrees = unsafe silk_resolver_stub_frees()
  let observedArguments = unsafe silk_resolver_stub_arguments_ok()
  return observedCalls == calls && observedFrees == frees && observedArguments == arguments
}

effect fn nativeCases() -> i32 {
  let policy = run Effect.result(policyProgram())
  match move policy {
    Result<i32, ResolverError | OutOfMemoryError>.Failure {error} => {
      drop error
      return 21
    }
    Result<i32, ResolverError | OutOfMemoryError>.Success {value} => {
      if value != 42 { return 22 }
    }
  }

  unsafe { silk_resolver_stub_reset(0) }
  let success = run Effect.result(resolveSystem(2, Option.none<Instant>()))
  match move success {
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Failure {error} => {
      drop error
      return 1
    }
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Success {value} => {
      if ResolvedEndpoints.length(&value) != 2 { return 2 }
    }
  }
  if !counters(1, 1, 1) { return 3 }

  unsafe { silk_resolver_stub_reset(1) }
  let malformed = run Effect.result(resolveSystem(2, Option.none<Instant>()))
  match move malformed {
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Success {value} => {
      drop value
      return 4
    }
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Failure {error} => match move error {
      ResolverError.InvalidNativeResult {reason, index} => {}
      _ => { return 5 }
    }
  }
  if !counters(1, 1, 1) { return 6 }

  unsafe { silk_resolver_stub_reset(2) }
  let limited = run Effect.result(resolveSystem(2, Option.none<Instant>()))
  match move limited {
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Success {value} => {
      drop value
      return 7
    }
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Failure {error} => match move error {
      ResolverError.LimitExceeded {maximum} => { if maximum != 2 { return 8 } }
      _ => { return 9 }
    }
  }
  if !counters(1, 1, 1) { return 10 }

  unsafe { silk_resolver_stub_reset(3) }
  let system = run Effect.result(resolveSystem(2, Option.none<Instant>()))
  match move system {
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Success {value} => {
      drop value
      return 11
    }
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Failure {error} => match move error {
      ResolverError.NativeFailure {operation, eai, errno} => match move errno {
        Option<i32>.None => { return 12 }
        Option<i32>.Some {value} => {}
      }
      _ => { return 13 }
    }
  }
  if !counters(1, 0, 1) { return 14 }

  unsafe { silk_resolver_stub_reset(0) }
  let deadline = run Effect.result(resolveSystem(
    2,
    Option.some<Instant>(SystemClock.make(10, 0)),
  ))
  match move deadline {
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Success {value} => {
      drop value
      return 15
    }
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Failure {error} => match move error {
      ResolverError.DeadlineUnsupported => {}
      _ => { return 16 }
    }
  }
  if !counters(0, 0, 0) { return 17 }

  unsafe { silk_resolver_stub_reset(0) }
  let refused = run Effect.result(resolveRefused())
  match move refused {
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Success {value} => {
      drop value
      return 18
    }
    Result<ResolvedEndpoints, ResolverError | OutOfMemoryError>.Failure {error} => match move error {
      OutOfMemoryError {} => {}
      _ => { return 19 }
    }
  }
  if !counters(1, 1, 1) { return 20 }
  return 42
}

pub fn main() -> i32 { return run nativeCases() }
`

/**
 * Complete shared-corpus entry for JUL-190. It needs no memory imports or non-default runtime
 * components: the registered standard-library actors and default native runtime are sufficient.
 */
export const networkAddressResolutionCorpusProgram = Object.freeze({
  name: 'network-address-resolution',
  source: networkAddressPortableCorpusSource,
  nativeSource: nativeResolverStubAcceptanceSource,
  nativeCSources: Object.freeze({ native_resolver_stub: nativeResolverStubSource }),
  expected: Object.freeze({ _tag: 'Completes' as const, result: 42 }),
})
