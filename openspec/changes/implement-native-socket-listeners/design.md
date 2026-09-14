## Context

See `proposal.md` for motivation and `specs/native-socket-listeners/spec.md` for the observable
contract. At work base `240c0ab74b4750b0c5a2cbbe42c256f183ce89c2`, `silk.native_socket`
already owns the selected Darwin/GNU layouts, errno policy, affine descriptor and `Connection`,
exact-prefix ByteDuplex provider, checked timer-assisted readiness loop, and nonretrying terminal
close. `Endpoint` already admits port zero. `Effect.useReleaseNonParking` supplies higher-ranked
structured release, and `MonotonicClock` remains provider-replaceable for deterministic scripts.

No listener actor exists. A separate source module also could not safely construct the existing
`Connection`: its descriptor and fields are deliberately private. The current native-socket design
requires one source owner for every socket descriptor and forbids exporting raw native handles.

## Goals / Non-Goals

**Goals:**

- Extend the existing descriptor owner without exposing an integer fd or adding compiler privilege.
- Keep listener, provisional accepted descriptor, accepted connection, peer bytes, and timer
  authority independently owned across every structured exit.
- Make the serial one-accept-at-a-time policy explicit while leaving application concurrency and
  active-connection limits outside the transport.
- Reuse and factor the delivered connection setup, readiness, address, error, and finalizer behavior
  rather than create a subtly different listener path.
- Prove native ABI and loopback behavior once per shared corpus profile and keep exhaustive failure
  policy at cheaper analysis/scripted tiers.

**Non-Goals:**

- A reactor, epoll/kqueue service, event registration API, worker pool, cross-thread close/cancel,
  background accept loop, connection supervisor, router, or application concurrency policy.
- DNS-based binding, hostname advertisement, dual-stack two-descriptor policy, Unix abstract
  addresses, automatic pathname removal, descriptor adoption, socket activation, or inherited fds.
- TLS server setup, HTTP handling, WebSocket negotiation, client dialing, or a portable simulated
  listener implementation.

## Decisions

### 1. Extend `native_socket.silk` so descriptor ownership remains sealed

Add `Listener`, `ListenOptions`, bound/peer address values, accepted ownership, listener operations,
and new native declarations to the existing selected actor. Private `Descriptor` and `Connection`
construction remain in one module, so accept can transfer a fully configured connection without a
public raw-fd constructor or a compiler-recognized library declaration.

`Listener` stores one `Descriptor`, Open/Closed phase, validated owned bound metadata, and the
positive accept polling interval. `Accepted` owns one `Connection` plus peer metadata. Neither has
a `Copy` conformance, and an explicit move cannot be reused. `accept(&mut Listener, deadline)`
returns this independent affine value. A
higher-ranked `withAccepted` consumes it, lends `&mut Connection` and `&PeerAddress`, and uses
`Effect.useReleaseNonParking` to preserve callback success/failure/cancellation while recovering a
connection-close error. The existing connect scopes factor through the same private connection
release helper where useful.

`AcceptedContext<A, E, ?R, ?Q>` is the compile-time-only counterpart for consumers such as the HTTP
server whose handler state is affine or once-callable. `R` is the caller-selected row and `Q` is the
additional row fixed by the concrete context; `use` and `withAcceptedContext` expose their union.
Its higher-ranked `use(context, view)` operation consumes `Self` independently from the temporary
`AcceptedView` loan.
`withAcceptedContext` captures the context in the once-callable protected use, consumes it exactly
once, and uses the same `Effect.useReleaseNonParking` bracket and recovered connection release as
`withAccepted`. Exact `Without<R, ByteDuplex>` and `Without<Q, ByteDuplex>` constraints make their
union exclude `ByteDuplex` and prevent an ambient provider alias; interface selection adds no
runtime service slot or requirement member.

Returning a bare numeric descriptor or adding public `Connection.fromFd` was rejected because it
would bypass configuration, state, and close policy. Making `accept` callback-only was rejected
because the canonical contract requires ownership transfer and a connection that can outlive a
closed listener. Exposing only the owned result with no scope helper was rejected because every
HTTP adapter would have to reconstruct protected-outcome cleanup.

### 2. Listener creation returns an owner and offers a separate resource scope

`listen(endpoint, options)` and `listenUnix(path, options)` acquire and return one affine Listener.
An ordinary `withListener` helper consumes that value and brackets a higher-ranked listener callback
for applications that do not need to compose the owner manually. Explicit `Listener.close` is
terminal and idempotent. The two creation entry points validate all pure input before socket work,
build the exact target sockaddr on the stack, configure the descriptor, bind, listen, and only then
publish the owner.

For numeric binds, `getsockname` runs after successful `listen`; conversion owns the actual endpoint
and thus reports a kernel-selected port. IPv6 setup always sets `IPV6_V6ONLY = 1`. Unix setup uses the
existing per-target `sockaddr_un` shape and admission policy but copies the path into an owned public
bound-address value. No branch calls unlink.

A callback-only `listen` was rejected because long-running applications need to retain one listener
across separately scoped server iterations. A dual-stack convenience that creates two descriptors
was rejected because it obscures admission and cleanup and contradicts explicit IPv6-only policy.

### 3. Listen options separate admission policy from connection-attempt policy

`ListenOptions.make` validates `kernelBacklog`, `reuseAddress`, and
`pollIntervalNanoseconds`; `defaults` selects 128, false, and 1 ms. The listener retains only these
values. The accepted `Connection` receives a private I/O policy constructed from default Nagle plus
the listener's polling interval; the existing outbound maximum-attempt field is not meaningful for
acceptance and is not exposed as listener policy.

Refactor `Connection`'s private stored state from the full public `ConnectOptions`, if necessary, to
the minimum no-delay/poll-interval record used by ByteDuplex. This does not change the existing
connect API. Adding `ConnectOptions.maxAttempts` to Listener or silently using it as an accept cap
was rejected because `None` explicitly permits indefinite admission. A fixed nonconfigurable accept
interval was rejected because it would remove the existing latency/idle-poll trade-off from server
applications.

### 4. Native setup is platform-selected and publication-ordered

GNU listener `socket` requests `SOCK_NONBLOCK | SOCK_CLOEXEC`; accepted descriptors use `accept4`
with the same flags. Darwin uses `socket`/`accept`, followed immediately by `F_GETFL` +
`F_SETFL(O_NONBLOCK)` and `F_GETFD` + `F_SETFD(FD_CLOEXEC)`. Listener setup conditionally applies only
`SO_REUSEADDR` and always applies `IPV6_V6ONLY` for IPv6 before bind. The accepted descriptor receives
disabled positive linger and Darwin `SO_NOSIGPIPE`; GNU writes continue using `MSG_NOSIGNAL`.

Each fallible stage holds a private Descriptor owner. On failure, state is invalidated before the
same GNU `close` or Darwin `close$NOCANCEL` path used by connections. Application code and peer
metadata cannot observe a descriptor before all stages complete.

Assuming accepted flags inherit was rejected by both platform contracts. Using `accept` plus fcntl
on GNU was rejected because `accept4` supplies the required atomic flags. Adding `SO_REUSEPORT` was
rejected because it changes load distribution and multi-process ownership rather than merely
address rebinding.

### 5. One fixed native storage value supports safe returned-address decoding

Add a private target-compatible 128-byte, eight-byte-aligned `sockaddr_storage` representation and
pass its address plus initialized capacity to `accept`/`accept4` and `getsockname`. Decode only after
validating the kernel-returned `socklen_t`. Family-specific parsing then requires the selected
minimum/exact length before reading port or address bytes.

Public `BoundAddress` and `PeerAddress` own their values. TCP forms contain existing `Endpoint`.
Unix bound metadata uses a fixed maximum pathname store plus length. Unix peer metadata is
`Unnamed` when GNU returns only the family header, or when Darwin returns its family-bearing padded
record with a leading-NUL pathname for an unbound peer; Darwin has no abstract Unix namespace. A
zero-length form is admitted only when the accepting listener is known to be Unix. Otherwise it
examines only the returned `sun_path` range, rejects GNU's leading NUL as abstract, strips at most
the returned terminal NUL/padding, and copies the exact pathname spelling without requiring it to
be absolute. Unknown families, short lengths, and contradictory lengths return
`InvalidNativeAddress`.

Retaining a slice into stack storage was rejected because it would escape the accept frame. Casting
every result through one C overlay was rejected because Darwin and GNU disagree on family headers.
Applying local path admission to peers was rejected because unnamed and relative peer names are
valid remote identity forms.

### 6. Accept readiness reuses the checked positive-wait state machine

Factor the existing checked absolute-deadline comparison/addition and zero-time poll behavior into
private helpers shared by connect, ByteDuplex, and accept. Accept performs: deadline check, one
nonblocking syscall, errno classification, optional `poll(..., 0)` for readability, then one positive
`MonotonicClock.waitUntil` when progress is unavailable. A ready-then-EAGAIN race, EINTR, interrupted
poll, and each GNU pending-network error all route through the positive wait before retry. The same
borrowed deadline is never renewed.

The listener remains owned while a wait parks. Cancellation first releases the scheduler timer/Wake
guard and destroys any provisional Descriptor owner, then surrounding listener scope may close the
listener. Closing from another thread while accept is pending is explicitly unsupported.

A blocking poll timeout was rejected because it stalls the local scheduler. Retrying immediately
was rejected because signal/readiness races can spin. Adding a finite accept-attempt count was
rejected because it does not bound elapsed time and would conflict with explicit `None` semantics.

### 7. Error mapping stays closed and operation-specific

`ListenOptionsError` owns pure construction failures. Extend `NativeSocketOperation` only with the
native boundaries needed to identify listener failures: reuse/IPv6 option setup remains Configure,
while Bind, Listen, GetSocketName, Accept, and peer conversion/publication receive stable operation
or structured variants. Extend `NativeSocketError` with AddressInUse, InvalidBindAddress,
PathTooLong, InvalidNativeAddress, and UnsupportedPeerAddress while reusing PermissionDenied,
FamilyUnsupported, SystemResources, Timeout, Closed, and NativeFailure.

Errno classification is selected inside the native actor. Linux's documented pending network error
set is accepted only after accept and always waits before retry. Configuration, resource, permission,
bad-descriptor, and unknown errors remain terminal. The first provider uses fixed owned address
storage, so no post-accept allocation is necessary; the provisional-owner structure still closes on
any future fallible publication step.

Creating a listener-only arbitrary errno wrapper was rejected because callers need stable recovery
branches. Translating accepted connection I/O into listener errors was rejected because the existing
Connection/ByteDuplex boundary remains authoritative after transfer.

### 8. Native evidence combines real loopback with controlled fault injection

Add one focused listener analysis file with narrow target-specific symbol/MIR probes, one
declaration-only ownership snapshot, a canonical public documentation example, and focused
WebAssembly exclusion. Ticket-local support exports one profile-agnostic corpus entry. Its native
program uses only loopback and a private temporary pathname: a C helper creates the peer before
synchronous accept, allowing serial
execution without threads; the TCP case closes the listener before transferring bytes, and the Unix
case uses an unbound client and confirms the filesystem entry survives close. A compact C fault
boundary scripts setup/accept errno, returned lengths/families, readiness races, close counters, and
virtual-clock observations without one binary per behavior.

The deterministic boundary counts every positive virtual-clock wait and executes one actually
parked accept through `Execution.park`; destroying that dormant execution must release its retained
Wake, run the listener scope finalizer once, and perform no late accept or poll. On Darwin the same
boundary interposes immediate `accept`, all four required `fcntl` operations, and returned
`sa_len`, including configured-descriptor and contradictory-length failures before publication.

Independent Darwin and GNU C translation units assert socket signatures, sockaddr and storage
size/alignment/offsets, flags, options, errno constants, and unnamed-address lengths from system
headers. The shared harness runs the single native program once with optimization enabled. Focused
registered-import analysis proves exact unsupported-target diagnostics without adding a no-op
portable backend row. Documentation examples use deterministic local paths, show the owned
accepted-context adapter, and make caller unlink ownership explicit.

Live public-network tests, wall-clock timing assertions, one native binary per requirement, and
duplicated Connection transfer suites were rejected under the repository's test-economics policy.

## Risks / Trade-offs

- **[Accept polling scales poorly at very high listener counts]** → Document the configurable
  latency/idle-work trade-off and keep a future reactor as a separate design.
- **[Darwin cannot atomically set accepted flags]** → Apply both fcntl settings immediately while the
  private provisional owner is armed and exclude cross-thread fork safety.
- **[Kernel sockaddr lengths differ or contain padding]** → Validate returned lengths before every
  field read and pin assumptions with independent platform headers.
- **[Linux reports protocol failures from pending clients through accept]** → Retry only the
  documented pending-network set, always after a positive wait; preserve all other errno values.
- **[Unix path cleanup is surprising to examples]** → Use a caller-owned private directory and show
  explicit unlink after listener close; never hide namespace mutation in finalization.
- **[Owned accept results can be mis-scoped by callers]** → Make the higher-ranked accepted scope the
  documented path, retain affine Drop as the ownership backstop, and prove protected outcomes.
- **[A listener is closed while another thread accepts]** → State that only same-thread structured
  cancellation before close is supported; no concurrency promise is implied.

## Migration Plan

Extend the existing native socket source and ticket-local evidence first, then register the new
public aliases, regenerate the embedded standard-library catalog, add the reference index entry,
and wire the exported program into the shared native corpus profiles. Existing outbound
connection call sites and public behavior remain unchanged. JUL-201 then consumes the documented
accepted-connection scope for one serial HTTP server connection per admission call.

Rollback removes the listener additions, ticket-local fixtures, manifest aliases, generated source,
corpus entry, reference page/index link, and this unarchived change together; no persisted data or
automatic Unix pathname migration exists.
