## 1. Generic native symbol spelling

- [x] 1.1 Extend the shared `ForeignSymbol.isValidSpelling` tail to admit `$` while retaining the
      existing leading-character, ASCII, NUL, and reserved-symbol rules; verify focused declaration
      and ABI-manifest cases accept dollar-bearing import/export/data catalog spellings and reject
      leading `$`, whitespace, NUL, non-ASCII, and reserved names.
- [x] 1.2 Add a generic quoted-symbol lowering/object-link fixture, including Darwin's exact
      `close$NOCANCEL` alias, and verify LLVM, ABI inspection, emitted object symbols, and a separately
      linked C definition preserve the byte-exact admitted spelling while inactive declarations add
      no GNU or Wasm import.

## 2. Selected socket actor and acquisition policy

- [x] 2.1 Add `native_socket.silk` with selected Darwin/GNU declarations, private target layouts and
      constants, affine Connection phases, bounded ConnectOptions, typed attempt/acquisition errors,
      and unsupported-target exclusion; verify focused analysis checks public shapes, option
      precedence, move-only ownership, and exact foreign inventories on each target family.
- [x] 2.2 Implement `connectResolvedOwned` and `connectUnixOwned` with pre-publication descriptor
      guards, then implement the higher-ranked `connectResolved` and `connectUnix` scopes as
      nonparking use/release conveniences over those owners; verify analysis rejects copied owners,
      escaped scoped borrows, raw descriptor transfer, provider aliasing, and publication before
      Open.
- [x] 2.3 Implement TCP socket setup for nonblocking/CLOEXEC, disabled positive linger, per-platform
      SIGPIPE suppression, default Nagle, and explicit no-delay; verify target fixture records prove
      GNU atomic flags, Darwin pre-publication fcntl, no global signal change, and no reuse/local bind.
- [x] 2.4 Implement pathname Unix admission and sockaddr construction with exact target capacity,
      terminator, family, and length rules; verify tabled Darwin/GNU cases reject empty, relative,
      NUL, oversized, and abstract paths without socket creation or unlinking.

## 3. Connect and cooperative readiness state machines

- [x] 3.1 Implement ordered fresh-descriptor connect attempts with one overall deadline, exact
      max-attempt accounting, terminal/nonterminal failure policy, EINPROGRESS plus mandatory
      SO_ERROR completion, and last-failure AttemptsExhausted data; verify scripted candidates prove
      order, no deadline renewal, no retry after terminal failure, and close-before-advance.
- [x] 3.2 Implement fresh-descriptor retries after connect EINTR and GNU pathname-Unix backlog EAGAIN;
      verify every retry waits positively and counts, and a full-backlog fixture proves stale
      writable plus SO_ERROR-zero cannot falsely publish Open.
- [x] 3.3 Implement zero-time readiness polling plus checked `min(now + pollInterval, deadline)`
      clock waits for connect/read/write, including pre-I/O and post-resume deadline checks;
      verify virtual-clock scripts cover unready, EINTR, ready-but-EAGAIN, equality timeout,
      nonadvancing-spin exclusion, None, and TimeRangeError without arithmetic overflow.

## 4. ByteDuplex transfer and release behavior

- [x] 4.1 Implement Connection reads with local empty handling, capped native lengths, exact positive
      prefixes, sticky recv-zero End, HUP draining, actual POLLERR/SO_ERROR mapping, and POLLNVAL
      invalidation; verify consolidated scripted/native cases distinguish every outcome and never
      recode cancellation as EOF.
- [x] 4.2 Implement Connection writes, unbuffered flush, and idempotent SHUT_WR with local empty
      handling, ssize_t-safe caps, exact accepted prefixes, no replay, invalid-zero terminalization,
      and reads after WriteClosed; verify the shared program records byte-exact partial sends,
      flush semantics, one directional shutdown, and closed subsequent writes.
- [x] 4.3 Implement exact-once terminal close after timer-guard release with state invalidation before
      GNU `close` or Darwin `close$NOCANCEL`, no EINTR/EINPROGRESS retry, and protected-outcome
      preservation; verify success, typed failure, setup failure, explicit close error, and scheduled
      cancellation cases record one close and no later fd/timer use.
- [x] 4.4 Implement the closed native-to-ByteIoError translation at the concrete provider boundary;
      verify timeout/count/closure use their canonical variants and other native failures retain the
      exact ByteIoOperation plus stable i32 provider code without widening callback errors.
- [x] 4.5 Resample one supplied absolute deadline immediately before every later descriptor setup,
      connect/readiness/SO_ERROR, transfer, and shutdown native boundary; extend the shared native
      corpus with sequenced-clock, setup-failure, readiness-failure, shutdown, post-close,
      close-ordering, and scheduled-cancellation counters that distinguish suppressed later work.

## 5. ABI evidence and publication

- [x] 5.1 Add independent Darwin and GNU C witnesses plus ticket-local native acceptance support for
      socket signatures, symbols, layouts, constants, options, connect completion, partial I/O,
      hangup, shutdown, backlog, interruption, and close; verify the exported profile-agnostic corpus
      program is sufficient for the shared optimized native harness without live DNS or
      public network access.
- [x] 5.2 Register `silk.native_socket` in the standard-library manifest/generated catalog and wire
      the ticket-local program into the shared corpus/profile table; verify registered imports resolve
      only on admitted native profiles, shared native acceptance executes both OS-family witnesses,
      and LLVM-to-Wasm observes no native socket or Darwin close symbol.
- [x] 5.3 Add the native socket reference page and executable local-fixture examples, including
      supported profiles, Unix pathname ownership, overall and None deadline behavior, polling
      latency/throughput, kernel-acceptance flush semantics, rich/ByteIo error mapping, and structured
      cleanup boundaries; verify documentation generation/doctests cover the public surface and the
      reference index links the page.
