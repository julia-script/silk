# Native system and monotonic clocks: implementation research

Audience: Silk compiler and standard-library implementers  
Date: 2026-08-27  
Scope: portable Silk services and native implementations for the repository's current Linux and
macOS targets; Unix-family portability strategy; evaluator, intrinsic, runtime, testing, and
documentation impact. Windows and WebAssembly/WASI implementation are excluded.

## Executive answer

The services fit Silk's existing architecture without a new language feature. They require two
portable service modules, two ordinary-source native provider modules, five small native-only
intrinsic operations (system read/resolution and monotonic read/resolution/absolute wait), two
injected evaluator host capabilities, and five reachability-selected C runtime symbols. The current
native targets are all 64-bit and already compile a POSIX.1-2008 C shim, so
`clock_gettime`, `clock_getres`, and `nanosleep` fit the existing toolchain.

Linux can wait with `clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, ...)`; macOS exposes
`clock_gettime` and `clock_getres` but not public `clock_nanosleep`, so it must repeatedly read
`CLOCK_MONOTONIC`, compute the positive remainder, and call `nanosleep`. Both paths retry signal
interruptions against the original absolute deadline. `waitFor` should take one monotonic reading,
add the `u64` nanosecond duration with checked split-field arithmetic, and delegate to `waitUntil`.

The supplied monotonic prose and signature disagree: WASI deliberately uses a nominal `u64` mark,
while the requested Silk surface uses `system_clock.Instant`. This proposal preserves the explicit
Silk signature, deletes the inapplicable 584-year `u64`-mark claim, and documents that absolute marks
are meaningful only with the provider timeline that produced them. This is less type-safe than a
nominal monotonic mark but is the user-specified public surface. The supplied waits also differ from
WASI's async operations; the first native implementation is explicitly thread-blocking and does
not claim Fiber/scheduler timer integration.

Silk's service protocol makes the clock requirements exclusive (`&mut`) even though the OS
providers are stateless, because deterministic source providers need to advance scripted readings
and record waits. `Instant` keeps the supplied private fields and adds a validating constructor plus
read-only accessors, so external providers can construct canonical values without making invalid
fractions freely mutable.

## Evidence and implications

### Public semantics

- POSIX defines `CLOCK_REALTIME` as system-wide real time and represents clock values with
  `timespec`, whose value is `tv_sec * 10^9 + tv_nsec`. The OS system provider can therefore retain
  split seconds and nanoseconds without flattening or losing pre-epoch instants. See
  [POSIX.1-2024 general realtime definitions](https://pubs.opengroup.org/onlinepubs/9799919799/functions/V2_chap02.html).
- POSIX defines `CLOCK_MONOTONIC` by non-settability and absence of backward jumps; the origin and
  exact rate are not a portable application contract. Equal reads are valid. See the
  [POSIX.1-2024 clock rationale](https://pubs.opengroup.org/onlinepubs/9799919799/xrat/V4_xsh_chap01.html).
- The WASI source behind the request uses distinct timestamp shapes: system time is split for wide
  epoch range, while a monotonic mark is a `u64` nanosecond count. See the
  [WASI clocks design discussion](https://github.com/WebAssembly/wasi-clocks) and
  [WASI monotonic-clock WIT](https://github.com/WebAssembly/wasi-clocks/blob/main/wit-0.3.0-draft/monotonic-clock.wit).
- Both requested waits are async in current WASI WIT, whereas POSIX sleep calls block the current
  host thread. Silk's current `LocalScheduler` explicitly excludes timers, I/O reactors, and
  cross-thread wake delivery, so mapping the API to a Fiber-aware timer would be a different,
  substantially larger capability.

### Linux

- Linux `CLOCK_REALTIME` is Unix-epoch wall time and can jump; `CLOCK_MONOTONIC` does not jump
  backwards, can repeat, and excludes suspended time. See
  [Linux `clock_gettime(3)`](https://man7.org/linux/man-pages/man3/clock_gettime.3.html).
- `clock_gettime` and `clock_getres` return `0` or `-1` with `errno`. By contrast,
  `clock_nanosleep` returns `0` or a positive error number directly. Confusing these conventions is
  a concrete implementation hazard.
- With `TIMER_ABSTIME`, a past deadline returns immediately, an interrupted call returns `EINTR`,
  the remainder output is unnecessary, and retrying the same deadline prevents cumulative drift.
  See [Linux `clock_nanosleep(2)`](https://man7.org/linux/man-pages/man2/clock_nanosleep.2.html) and
  [POSIX `clock_nanosleep`](https://pubs.opengroup.org/onlinepubs/009696899/functions/clock_nanosleep.html).

### macOS and the Unix fallback

- Apple's public headers expose `CLOCK_REALTIME`, `CLOCK_MONOTONIC`, `clock_gettime`, and
  `clock_getres` beginning with macOS 10.12, plus `nanosleep`, but no public `clock_nanosleep`.
  See [Apple `_time.h`](https://github.com/apple-oss-distributions/Libc/blob/main/include/_time.h).
- Apple documents its `CLOCK_MONOTONIC` as advancing during system sleep; Linux documents the
  opposite. This difference is compatible with the requested non-decreasing contract and must stay
  unspecified. See [Apple `clock_gettime(3)`](https://github.com/apple-oss-distributions/Libc/blob/main/gen/clock_gettime.3).
- Mach clocks are unnecessary here. `mach_absolute_time` excludes sleep and
  `mach_continuous_time` includes it, but both use platform tick units and do not supply a matching
  portable absolute sleep operation. Staying on one POSIX clock for read and wait avoids a
  timebase mismatch. See [XNU `mach_time.h`](https://github.com/apple-oss-distributions/xnu/blob/main/osfmk/mach/mach_time.h).
- Unix is not one ABI. FreeBSD and NetBSD expose `clock_nanosleep`; OpenBSD and macOS need the
  deadline-loop fallback, and clock identifiers are platform constants rather than portable
  integers. The design must branch on API availability in C and must not add an unverified generic
  Unix target triple. Relevant primary manuals:
  [FreeBSD `clock_nanosleep`](https://man.freebsd.org/cgi/man.cgi?n=1&query=clock_nanosleep&sektion=2),
  [NetBSD `clock_nanosleep`](https://man.netbsd.org/clock_nanosleep.2),
  [OpenBSD `clock_gettime`](https://man.openbsd.org/clock_gettime), and
  [OpenBSD `nanosleep`](https://man.openbsd.org/nanosleep).

### Representation and boundary checks

- Preserve system readings as split `i64` seconds and canonical nanoseconds. Flattening system
  instants into `u64` nanoseconds would discard negative dates and impose an unnecessary year-2554
  limit.
- Validate `0 <= tv_nsec < 1_000_000_000` and that `time_t` fits `i64` before committing outputs.
  All current Silk native targets are 64-bit; adding a 32-bit or older Unix target would require a
  separate time-width audit.
- Convert resolution with checked `seconds * 1_000_000_000 + nanoseconds`; returning only
  `tv_nsec` is wrong for any resolution of at least one second.
- Treat `clock_getres` as the provider-reported nominal tick resolution, not measured precision.
  Apple explicitly permits the reported resolution to be a lower bound on actual precision.
- Convert `u64` durations with quotient and remainder. A `u64` nanosecond duration spans about 584
  years but only about 18.4 billion seconds, so the quotient fits `i64`; the deadline addition can
  still overflow if a custom provider starts near `i64::MAX` and must trap rather than wrap.
- Compare and subtract split monotonic marks lexicographically with normalized carry/borrow. Do not
  flatten an `Instant` to `i64` or `u64` nanoseconds.

### Repository impact

- The current canonical modules live under `packages/compiler/stdlib/silk` and are registered in
  `packages/compiler/stdlib/manifest.json`; `Stdlib.generated.ts` and documentation are generated
  outputs.
- Native providers already follow an ordinary-source/unsafe-intrinsic pattern. Intrinsic metadata
  in `Intrinsic.ts` assigns evaluator+LLVM availability, `LowerExpression.ts` produces generic MIR
  `OsCall`, `NativeProgram.ts` derives runtime declarations and reachable symbol inventory, and
  `OsRuntime.ts` emits only selected C entry points into `ToolchainPlan.shimSource`.
- `BootstrapOsIntrinsics.ts` owns evaluator execution of OS calls. Its current status-output
  assumption must be generalized for the clock protocol, and `BootstrapEvaluation.ts`,
  `BootstrapTrace.ts`, inspector presentation, and new host actor modules must carry the independent
  injected clock capabilities.
- Feature tests should follow `StandardInput.test.ts` and `HostInput.test.ts`: ordinary-source
  provider semantics at the evaluator tier, missing-host blockage, exact OS-call inventory,
  native-runtime symbol pay-for-use, platform C source/compilation checks, and a small native
  acceptance case only for genuinely target-specific behavior.
- `OsRuntime.source` currently prepends filesystem-heavy common C to every OS symbol. Clock support
  requires capability-specific prelude selection so a clock-only artifact does not inherit
  unrelated filesystem macros, helpers, or Unix assumptions.

## Material limitations and rejected alternatives

- A nominal `MonotonicClock.Mark` would prevent accidental system/monotonic mixing and align with
  WASI. It is rejected for this change because the user supplied `SystemClock.Instant` as the
  expected Silk signature. The limitation is documented and no false static guarantee is claimed.
- A scheduler-aware timer would avoid blocking all local tasks. It is rejected because it requires
  timer registration, lifetime/cancellation rules, wake delivery, and scheduler integration beyond
  the requested native clock services. The blocking behavior is normative for this slice.
- `CLOCK_BOOTTIME`, `CLOCK_MONOTONIC_RAW`, and Mach-specific clocks are rejected because they would
  create target-specific suspend/rate semantics not required by the portable contract.
- Direct Wasm clock imports are deferred to WASI integration as requested.
- The no-extra-library Linux implementation targets `glibc >= 2.17`; supporting an older glibc
  clock ABI that needs `librt` is a separate target-baseline decision.

## Claim-to-source ledger

| Claim family | Source | Publisher / date | Access note |
|---|---|---|---|
| POSIX clock meanings, timespec value, resolution | [POSIX.1-2024 General Information](https://pubs.opengroup.org/onlinepubs/9799919799/functions/V2_chap02.html) | IEEE / The Open Group, 2024 | Public HTML |
| POSIX monotonic rationale | [Rationale for System Interfaces](https://pubs.opengroup.org/onlinepubs/9799919799/xrat/V4_xsh_chap01.html) | IEEE / The Open Group, 2024 | Public HTML |
| Absolute sleep semantics | [POSIX `clock_nanosleep`](https://pubs.opengroup.org/onlinepubs/009696899/functions/clock_nanosleep.html) | IEEE / The Open Group | Older public edition; current behavior corroborated by Linux manual |
| Linux clock behavior and return convention | [Linux `clock_gettime(3)`](https://man7.org/linux/man-pages/man3/clock_gettime.3.html) | Linux man-pages project | Public HTML |
| Linux absolute sleep and EINTR | [Linux `clock_nanosleep(2)`](https://man7.org/linux/man-pages/man2/clock_nanosleep.2.html) | Linux man-pages 6.18, 2025-10-29 | Public HTML |
| Apple API availability | [Apple `_time.h`](https://github.com/apple-oss-distributions/Libc/blob/main/include/_time.h) | Apple Open Source | Current public header |
| Apple clock meanings | [Apple `clock_gettime(3)`](https://github.com/apple-oss-distributions/Libc/blob/main/gen/clock_gettime.3) | Apple, 2016-01-26 | Public source manual |
| Apple Mach time behavior | [XNU `mach_time.h`](https://github.com/apple-oss-distributions/xnu/blob/main/osfmk/mach/mach_time.h) | Apple Open Source | Current public header |
| WASI source API and type rationale | [WASI clocks repository](https://github.com/WebAssembly/wasi-clocks), [monotonic WIT](https://github.com/WebAssembly/wasi-clocks/blob/main/wit-0.3.0-draft/monotonic-clock.wit) | WebAssembly Community Group, draft dated 2025-09-16 | Repository archived after merge into main WASI |

## Research stopping point

The consequential semantic, return-convention, target-availability, conversion, and scheduler gaps
have primary support and map to concrete repository seams. Additional Unix variants would not
change the current target implementation; they should be researched when a specific new target
triple is proposed. Illumos, AIX, and HP-UX were therefore left outside the verified support claim.
