## Purpose

Define owned native TCP and pathname-Unix listeners that admit one independently scoped byte
connection at a time with validated metadata, cooperative deadlines, and exact descriptor cleanup.

## ADDED Requirements

### Requirement: Listen options are finite and explicit

Canonical `silk.native_socket` SHALL expose `ListenOptions` with a requested kernel backlog,
reuse-address flag, and positive accept-poll interval. Defaults SHALL request backlog 128, disable
address reuse, and use the existing one-millisecond native-socket polling interval. Construction
SHALL accept backlog 1 through 65535 and polling intervals 1 through 1,000,000,000 nanoseconds and
SHALL reject every other value as a typed invalid-option result before native work. The kernel MAY
cap the requested backlog. The option SHALL control only `SO_REUSEADDR`; this capability SHALL NOT
enable `SO_REUSEPORT`, alter process-wide signals or umask, or create an application accept queue.

#### Scenario: Construct default policy

- **WHEN** an application requests default listen options
- **THEN** it receives backlog 128, disabled reuse-address, and a one-millisecond positive polling
  interval

#### Scenario: Reject an invalid option locally

- **WHEN** backlog is zero or greater than 65535, or the polling interval is zero or greater than
  one second
- **THEN** construction reports the exact invalid field and value without creating a socket

#### Scenario: Enable only address reuse

- **WHEN** reuse-address is enabled for listener creation
- **THEN** the native setup requests `SO_REUSEADDR` and never requests `SO_REUSEPORT`

### Requirement: TCP listeners publish the actual bound endpoint

`listen(endpoint, options)` SHALL create one affine nonblocking, close-on-exec stream `Listener` for
an IPv4 or IPv6 numeric `Endpoint`, bind it, begin listening, and publish it only after all setup
succeeds. Port zero SHALL be valid. After successful listening, the listener SHALL query and own the
actual native bound endpoint, including the assigned nonzero port. IPv6 listeners SHALL explicitly
set IPv6-only operation and SHALL NOT create an implicit IPv4 descriptor. IPv4-mapped IPv6 values
SHALL remain IPv6 identities.

#### Scenario: Discover an assigned IPv4 port

- **WHEN** an IPv4 loopback endpoint with port zero is successfully listened
- **THEN** the listener's bound metadata contains the same IPv4 address and the nonzero port returned
  by the native socket

#### Scenario: Bind one IPv6-only descriptor

- **WHEN** an IPv6 endpoint is listened
- **THEN** setup explicitly enables IPv6-only operation and publishes exactly one IPv6 listener
  descriptor

#### Scenario: Reject setup before publication

- **WHEN** socket options, bind, listen, or bound-address discovery fails
- **THEN** no listener is published and every descriptor acquired by that attempt is closed exactly
  once

### Requirement: Unix listeners preserve caller pathname ownership

`listenUnix(path, options)` SHALL admit only a nonempty, absolute, NUL-free byte pathname that fits
the selected platform's `sockaddr_un` including one terminal NUL. It SHALL reject abstract addresses
and SHALL copy the admitted path into listener metadata before publication. It SHALL never unlink a
path before bind or after listener close. An occupied or stale pathname SHALL report
`AddressInUse`; creating a private directory and removing the socket entry remain caller policy.

#### Scenario: Listen on an admitted pathname

- **WHEN** an absolute pathname fits the target capacity and bind plus listen succeed
- **THEN** the listener owns the exact pathname spelling and the filesystem entry remains
  caller-owned

#### Scenario: Refuse destructive stale-path cleanup

- **WHEN** bind finds an existing pathname entry
- **THEN** listener creation reports `AddressInUse` without unlinking or replacing that entry

#### Scenario: Leave the entry after close

- **WHEN** a pathname listener closes successfully
- **THEN** its socket descriptor is terminally released and the pathname entry remains present

### Requirement: Accept transfers one independent owned connection

`accept(listener, deadline)` SHALL require one exclusive mutable listener borrow and SHALL return an
affine accepted result containing one owned existing `Connection` and owned peer metadata. The
listener borrow SHALL prevent overlapping pending accepts. Ownership SHALL transfer only after the
accepted descriptor is configured and its peer address is validated. Closing the listener SHALL
prevent future accepts but SHALL NOT close, invalidate, or otherwise couple any previously
transferred connection; closing a transferred connection SHALL NOT close its listener.

The actor SHALL expose a higher-ranked accepted-connection scope that consumes one accepted result,
lends its connection as the concrete `ByteDuplex` provider together with borrowed peer metadata,
and closes the connection after success, typed failure, or structured cancellation without
replacing the protected outcome. It SHALL create no task, application queue, concurrency limit, or
background accept loop.

The actor SHALL also expose compile-time-only `AcceptedContext<A, E, ?R, ?Q>` selection whose
higher-ranked `use(context, view)` operation consumes one owned context independently from the
temporary accepted view. `withAcceptedContext` SHALL consume the accepted owner and context, invoke
that operation at most once, and use the same nonparking structured connection release. `R` SHALL
preserve caller-selected requirements while `Q` SHALL name requirements introduced by the concrete
context; the operation exposes `R | Q`. Exact `R in Without<R, ByteDuplex>` and
`Q in Without<Q, ByteDuplex>` constraints SHALL make that union exclude `ByteDuplex`, so the
accepted connection cannot alias an ambient provider.

#### Scenario: Use a connection after listener close

- **WHEN** an accepted result has transferred and the application then closes its listener
- **THEN** the accepted connection remains usable until its own independent scope closes it

#### Scenario: Reject overlapping acceptance

- **WHEN** source tries to begin a second accept or close through the same listener while one
  exclusive accept borrow remains live
- **THEN** ownership analysis rejects the overlapping listener use

#### Scenario: Preserve callback failure

- **WHEN** an accepted-connection callback fails and terminal connection close also reports an error
- **THEN** the connection is closed once and the callback failure remains observable

#### Scenario: Consume an owned accepted context

- **WHEN** a concrete context implementation is selected for one accepted result
- **THEN** its owned state is consumed once with a temporary view and the connection closes once
  after the protected use

#### Scenario: Reject context loan escape and provider alias

- **WHEN** a context implementation tries to return its view or connection loan, or requires an
  ambient `ByteDuplex`
- **THEN** analysis rejects the invalid lifetime or requirement-row constraint

#### Scenario: Cancel an accepted context use

- **WHEN** the selected context use parks and its dormant structured execution is cancelled
- **THEN** its Wake and context are released, the accepted connection closes exactly once, the
  primary cancellation exit is preserved, and no late callback work occurs

### Requirement: Peer metadata is owned and length-validated

Accepted TCP peers SHALL be reported as owned numeric `Endpoint` values selected from the returned
IPv4 or IPv6 family. Accepted Unix peers SHALL distinguish `Unnamed` from `Pathname(bytes)`.
Unnamed Unix peers SHALL be recognized from the platform-specific returned address length without
reading `sun_path`. Pathname peer bytes SHALL preserve the returned spelling, independent of the
local bind API's absolute-path rule. Abstract Unix peers SHALL report `UnsupportedPeerAddress`.

Every peer and actual-bound-address conversion SHALL validate the returned native length before
reading a field, SHALL reject unknown families or malformed lengths as `InvalidNativeAddress`, and
SHALL copy all published bytes rather than retain the borrowed native address buffer. Any such
failure SHALL close only the provisional accepted descriptor and leave the listener open.

#### Scenario: Accept an unnamed Unix peer

- **WHEN** an unbound Unix client connects to a pathname listener and native accept returns the
  platform's unnamed-peer shape (Darwin's family-bearing padded record with a leading-NUL pathname,
  or only the family header on GNU)
- **THEN** the peer is `Unnamed` and conversion does not inspect bytes beyond that returned length

#### Scenario: Preserve a relative peer pathname

- **WHEN** native accept returns a non-abstract pathname peer spelling that is not valid as a local
  bind pathname
- **THEN** peer metadata owns that spelling without applying the listener's absolute-path admission
  rule

#### Scenario: Reject an abstract peer safely

- **WHEN** a Unix peer address begins with the platform's abstract marker
- **THEN** accept reports `UnsupportedPeerAddress`, closes the provisional connection once, and
  leaves the listener usable

#### Scenario: Reject a short native address

- **WHEN** native accept or bound-address discovery reports fewer bytes than the selected family
  requires
- **THEN** conversion reports `InvalidNativeAddress` without reading outside the returned range

### Requirement: Native descriptor setup precedes publication

Listener and accepted descriptors SHALL be nonblocking and close-on-exec before publication. On
GNU/Linux, listener creation SHALL request atomic socket flags and acceptance SHALL use `accept4`
with atomic nonblocking and close-on-exec flags. On Darwin, listener creation and each `accept` result
SHALL immediately apply the required status and descriptor `fcntl` flags before any other
publication or fallible peer processing. The implementation SHALL NOT assume accepted descriptors
inherit listener flags.

Every accepted connection SHALL receive the established native socket policy before transfer:
disabled positive linger, Darwin per-socket SIGPIPE suppression, GNU per-send `MSG_NOSIGNAL`, and
default Nagle behavior. A setup failure SHALL close the provisional descriptor without invoking
application code.

#### Scenario: Accept atomically on GNU

- **WHEN** a GNU/Linux listener accepts a connection
- **THEN** `accept4` requests nonblocking and close-on-exec atomically before peer publication

#### Scenario: Configure immediately on Darwin

- **WHEN** Darwin `accept` returns a descriptor
- **THEN** both nonblocking and close-on-exec flags plus per-socket SIGPIPE policy are applied before
  address conversion or ownership transfer

#### Scenario: Close a rejected configured descriptor

- **WHEN** accepted-socket setup fails after the native accept succeeds
- **THEN** the provisional descriptor is invalidated and closed exactly once and the callback is not
  invoked

### Requirement: Acceptance waits cooperatively on one deadline

Acceptance SHALL attempt the nonblocking native operation without first parking. On temporary
unavailability it SHALL inspect listener readability with zero-time polling, then wait on the active
`MonotonicClock` until the earlier of the next positive configured interval and the supplied
absolute deadline. The same deadline SHALL remain unchanged for the entire accept; `None` SHALL
permit indefinite waiting. The operation SHALL check deadline equality before external work and
immediately before each later poll, SO_ERROR inspection, and accepted-descriptor setup call, and
again after suspension before another native call, and SHALL report `Timeout` without renewing the
deadline.

An interrupted accept, interrupted poll, readiness followed by `EAGAIN`, or another retryable
result SHALL take a positive clock wait before retrying and SHALL never spin. GNU/Linux SHALL also
retry `ECONNABORTED`, `ENETDOWN`, `EPROTO`, `ENOPROTOOPT`, `EHOSTDOWN`, `ENONET`, `EHOSTUNREACH`,
`EOPNOTSUPP`, and `ENETUNREACH` as pending network conditions. Other resource, permission, invalid-
state, and native failures SHALL remain terminal and typed. Cancellation SHALL follow the existing
same-thread structured Effect contract; cross-thread listener closure or host-thread preemption is
not promised.

#### Scenario: Accept immediately without waiting

- **WHEN** a connection is pending before the deadline
- **THEN** accept transfers it without invoking a clock wait

#### Scenario: Wait positively after a readiness race

- **WHEN** poll reports readable but the following accept returns temporary unavailability
- **THEN** the operation performs one positive clock wait before another accept attempt

#### Scenario: Retry a GNU pending network error

- **WHEN** GNU/Linux accept reports one documented pending network error
- **THEN** the listener remains open and acceptance waits positively before retrying under the same
  deadline

#### Scenario: Stop at deadline equality

- **WHEN** the active monotonic mark equals the supplied deadline after a wait
- **THEN** acceptance reports `Timeout` before performing another poll or accept

#### Scenario: Cancel a parked accept

- **WHEN** structured cancellation destroys an accept parked in its clock wait
- **THEN** timer authority is released, no connection is published, and the listener remains owned
  and terminally closable by its surrounding scope

### Requirement: Listener failures and close remain precise

The public listener error surface SHALL distinguish invalid options or bind paths,
`AddressInUse`, `PermissionDenied`, `FamilyUnsupported`, `PathTooLong`, `SystemResources`, `Timeout`,
`Closed`, `InvalidNativeAddress`, `UnsupportedPeerAddress`, and otherwise retain the exact native
operation plus signed errno. Listener and connection close SHALL be terminal, idempotent,
nonparking, and independent. Close SHALL invalidate ownership before one GNU `close` or Darwin
`close$NOCANCEL` call and SHALL never retry `EINTR` or `EINPROGRESS`, flush, drain, wait for a peer,
or unlink a pathname. Structured finalization SHALL preserve an earlier success, typed failure, or
cancellation; fatal traps remain outside the cleanup guarantee.

#### Scenario: Close a listener repeatedly

- **WHEN** explicit listener close is invoked more than once
- **THEN** the descriptor is released at most once and every later call observes terminal state

#### Scenario: Preserve an accept failure during release

- **WHEN** accept returns a typed failure and listener release also encounters a close error
- **THEN** scoped finalization attempts one close and preserves the accept failure

#### Scenario: Refuse acceptance after close

- **WHEN** accept is requested on a terminally closed listener
- **THEN** it reports `Closed` without polling, waiting, or calling native accept

### Requirement: Availability, ABI evidence, and publication remain bounded

Native listener members SHALL be selected only for aarch64 Darwin with Apple system libc and
aarch64 or x86-64 GNU/Linux with GNU libc, matching the existing native connection actor. Windows,
musl, no-libc, and WebAssembly profiles SHALL expose no native listener member or foreign import.
The implementation SHALL remain ordinary selected Silk source over libc declarations and SHALL add
no compiler-known listener, runtime socket shim, reactor, DNS bind policy, TLS server, or global
signal policy.

Independent platform-header witnesses SHALL verify every used function signature, errno and option
constant, address size/alignment/offset, flag, and returned-length assumption. The standard-library
manifest, generated embedding, public reference, executable deterministic example, and shared
native acceptance corpus SHALL publish and exercise the complete surface without public-network or
live-DNS access. Native evidence SHALL include controlled IP loopback and pathname-Unix cases;
focused registered-import analysis SHALL prove unsupported-target exclusion without adding a no-op
portable backend row or duplicating a feature-specific native executable.

#### Scenario: Exclude listeners on WebAssembly

- **WHEN** a WebAssembly analysis imports the native listener actor
- **THEN** every requested listener member receives the exact unavailable-member diagnostic and a
  separately selected actor snapshot emits no listener foreign symbol

#### Scenario: Prove loopback ownership

- **WHEN** the shared native corpus binds local loopback, accepts one client, closes the listener,
  and transfers bytes over the accepted connection
- **THEN** the connection remains usable and listener plus connection descriptors each close once

#### Scenario: Prove pathname ownership and unnamed peers

- **WHEN** the shared native corpus accepts an unbound Unix client in a caller-owned private
  directory and closes the listener
- **THEN** the peer is `Unnamed`, every descriptor closes once, and the socket entry remains for
  explicit caller cleanup
