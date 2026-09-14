## 1. Listener ownership and public values

- [x] 1.1 Extend the selected `silk.native_socket` actor with finite `ListenOptions`, listener
      phase, owned bound-address, owned peer-address, and affine accepted-result values; verify narrow
      target-specific analysis checks native symbols and MIR while focused declaration analysis
      rejects user Copy conformances, implicit reuse, and raw-descriptor access.
- [x] 1.2 Factor the existing private descriptor, connection I/O policy, close, deadline arithmetic,
      and target sockaddr definitions so listen/accept can reuse them without changing outbound
      connection behavior; verify the existing JUL-146 analysis and shared native corpus retain their
      exact diagnostics, foreign inventories, transfer results, and cleanup counts.
- [x] 1.3 Add higher-ranked `withListener` and accepted-connection scopes that consume one owner,
      preserve arbitrary callback error/requirement channels, and recover finalizer close failures;
      verify ownership analysis rejects escaped listener/connection loans, overlapping accepts,
      provider aliases, and use after move while MIR retains nonparking cleanup on cancellation.
- [x] 1.4 Add compile-time-only `AcceptedContext` selection and `withAcceptedContext` over the same
      accepted-owner bracket, separating caller-selected from context-owned requirement rows;
      verify a concrete owned context succeeds, their union rejects ambient `ByteDuplex`,
      view/connection escape is rejected, and typed failure/cancellation close once without late
      callback work or replacement of the protected outcome.

## 2. TCP and pathname-Unix listener acquisition

- [x] 2.1 Implement numeric TCP listener creation with pure option preflight, atomic GNU or immediate
      Darwin nonblocking/CLOEXEC setup, optional `SO_REUSEADDR` only, mandatory IPv6-only policy,
      bind/listen, and post-listen `getsockname`; verify scripted/native cases cover IPv4 port zero,
      IPv6 identity, configured option inventories, exact setup order, and one close on every
      pre-publication failure.
- [x] 2.2 Implement pathname-Unix listener creation with nonempty absolute NUL-free capacity checks,
      exact target sockaddr length, and copied bound metadata; verify tabled Darwin/GNU cases reject
      empty, relative, NUL, oversized, abstract, occupied, and stale paths without pre-unlinking and
      confirm explicit listener close leaves a successful socket entry present.
- [x] 2.3 Implement terminal idempotent Listener close with state invalidation before one selected
      close call and no retry, flush, drain, wait, peer operation, or unlink; verify explicit success,
      close failure, repeated close, protected callback failure, and structured cancellation each
      observe at most one descriptor close while preserving the protected outcome.

## 3. Serial accept and peer publication

- [x] 3.1 Implement exclusive nonblocking accept with GNU `accept4` atomic flags and Darwin
      accept-plus-immediate-fcntl, then apply disabled positive linger and selected SIGPIPE policy
      before publication; verify configuration failures close only the provisional accepted
      descriptor, never invoke the callback, and leave the listener usable.
- [x] 3.2 Implement the unchanged-deadline accept state machine with deadline pre/post checks,
      zero-time readability polling, checked positive waits, readiness-race handling, EINTR handling,
      and the closed GNU pending-network error set; verify virtual-clock/native scripts distinguish
      immediate acceptance, `None`, equality timeout, nonadvancing-spin exclusion, overflow,
      retryable errors, terminal errors, and cooperative cancellation without deadline renewal.
- [x] 3.3 Decode peer and actual-bound addresses only after returned-length validation into fixed
      owned metadata, preserving IPv4/IPv6 identity and Unix pathname spelling while distinguishing
      unnamed peers and rejecting abstract/unknown/malformed forms; verify boundary-length fixtures
      prove no out-of-range read, allocation, borrow retention, descriptor leak, or listener closure.
- [x] 3.4 Transfer one independent accepted Connection and peer value, then scope the connection
      through the shared nonparking bracket; verify one controlled TCP loopback closes the listener
      before successful connection I/O, and a pathname case accepts an unbound client as Unnamed,
      with listener and connection cleanup independently exact once.

## 4. Error and platform boundaries

- [x] 4.1 Extend the closed native socket error/operation surface for invalid listener input,
      address-in-use, permission, family, path capacity, resources, timeout, closed state, malformed
      native address, unsupported peer, and retained operation/errno failures; verify focused tables
      cover precedence and platform-specific errno classification without widening accepted
      Connection ByteDuplex errors.
- [x] 4.2 Add Darwin/system-libc and GNU/Linux/glibc listener declarations plus independent C header
      witnesses for every used signature, errno/option/flag, sockaddr/storage size/alignment/offset,
      and returned-length assumption; verify admitted targets realize exact foreign inventories while
      musl, no-libc, Windows, and WebAssembly emit no listener member or foreign symbol.

## 5. Consolidated publication and documentation

- [x] 5.1 Add ticket-local focused and native-acceptance support that consolidates setup faults,
      accept races, peer decoding, cancellation, TCP loopback, Unix unnamed-peer/path retention, and
      descriptor counters into one optimized shared program; verify it reuses the
      canonical shared native corpus plus focused unsupported-target analysis rather than compiling
      feature-local binaries or using public network/DNS.
- [x] 5.2 Register every listener public alias in the standard-library manifest/generated catalog
      and wire the ticket-local corpus program into shared native integration; verify registered
      imports resolve on exactly the JUL-146 native profiles, exact WebAssembly import diagnostics
      prove exclusion without a no-op backend row, and generated source/toolchain metadata stays
      current.
- [x] 5.3 Add the native listener reference page with one executable deterministic acquisition
      example covering options and actual port discovery; document serial admission, accepted
      ownership and owned contexts, deadlines/None, transient retries, error mapping, cancellation,
      caller-owned Unix cleanup, and target exclusions, and verify the reference index links it.
