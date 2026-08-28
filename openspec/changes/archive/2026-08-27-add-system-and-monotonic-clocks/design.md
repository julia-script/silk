## Context

See [proposal.md](proposal.md) for motivation and
[report-source.md](report-source.md) for the evidence ledger. The compiler already has the required
architectural seams:

- canonical standard-library source and a deterministic manifest;
- ordinary source services with explicit lexical provider replacement;
- native-only unsafe `Intrinsic.os*` operations lowered to one generic MIR `OsCall` shape;
- evaluator OS boundaries with independently injected hosts and explicit missing-host blockage;
- LLVM declarations whose reachable symbols select C implementations from `OsRuntime`; and
- a POSIX.1-2008 native shim compiled and linked through Clang for the three current 64-bit native
  targets.

The constraints are more important than the syscall choice. Public services must remain ordinary
source and receive no spelling privilege. Direct Wasm must stay unsupported until WASI integration.
The current local scheduler has no timer reactor and explicitly excludes cross-thread wake delivery,
so a libc sleep cannot honestly be presented as task-only suspension.

## Goals / Non-Goals

**Goals:**

- Preserve the requested public value and service shapes while making their units, normalization,
  provenance, failure, and blocking behavior complete.
- Use the same native clock for every monotonic read, resolution query, and wait on one platform.
- Keep the compiler primitive surface target-neutral and smaller than the public abstractions.
- Make evaluator hosts deterministic and independently replaceable.
- Preserve reachable-only target validation, runtime linking, and artifact cost.

**Non-Goals:**

- A distinct nominal monotonic mark, duration actor, time arithmetic library, calendar, timezone,
  formatting, parsing, or clock-setting API.
- Fiber-aware timers, cancellation or interruption of waits, concurrent scheduler progress while a
  native wait blocks, non-blocking polling, or an I/O reactor.
- A new native target triple or a claim that every historical Unix implements the same ABI.
- Windows, direct-Wasm, WASI, or browser clock providers; ambient or compiler-installed defaults.

## Decisions

### Preserve the requested shared Instant and state its lost guarantee

The public portable source is:

```silk
// silk/system_clock
pub struct Instant {
  seconds: i64
  nanoseconds: i64
}

pub service SystemClock {
  effect fn now() -> Instant ? &mut SystemClock
  effect fn getResolution() -> u64 ? &mut SystemClock
}

// silk/monotonic_clock
pub service MonotonicClock {
  effect fn now() -> Instant ? &mut MonotonicClock
  effect fn getResolution() -> u64 ? &mut MonotonicClock
  effect fn waitUntil(when: Instant) -> () ? &mut MonotonicClock
  effect fn waitFor(howLong: u64) -> () ? &mut MonotonicClock
}
```

`silk/system_clock` additionally exposes `make(seconds, nanoseconds)`, `seconds(&Instant)`, and
`nanoseconds(&Instant)`. `make` traps unless the fraction is canonical. Keeping the fields private
prevents arbitrary mutation while still allowing ordinary user-defined providers to construct and
observe values. Each module also exposes an ordinary wrapper with the same name as each service
operation. Requirements are exclusive because deterministic providers commonly advance scripted
state or record waits. The official stateless providers still follow the repository's uniform
service-provider shape by taking an unused `self: &mut Os…Clock` receiver.

This intentionally differs from WASI's nominal `mark = u64`. The supplied Silk signatures reuse
`SystemClock.Instant`, so this change keeps that explicit request and removes the copied statements
about a 584-year `u64` mark. The consequence is not hidden: Silk cannot statically prevent passing a
system instant or another provider's mark to `waitUntil`. Documentation makes same-provider
provenance a precondition and tests do not manufacture a false type-safety claim.

A new private-field `Mark` was considered and is the cleaner green-field design in isolation, but it
would replace the requested API rather than implement it. Flattening both clocks to `u64`
nanoseconds was rejected because system time needs negative and much wider epoch range. Changing the
public nanoseconds field to `u32` would better encode normalization but also changes the supplied
definition; the provider validates and constructs the requested `i64` field instead.

### Canonical split fields are the only Instant representation rule

All returned values have `0 <= nanoseconds < 1_000_000_000`. The total value is interpreted with a
floor split, so `{ -1, 999_999_999 }` is one nanosecond before zero. This maps directly to a valid
POSIX `timespec` and makes ordering lexicographic by `(seconds, nanoseconds)`.

Source helpers compare, add, and subtract split values without flattening:

- compare seconds first, then nanoseconds;
- split a `u64` duration by quotient and remainder at one billion;
- add quotient, fraction, and at most one carry with checked `i64` arithmetic; and
- subtract a future deadline from `now` with one normalized borrow.

The maximum `u64` nanosecond duration has a seconds quotient of about 18.4 billion, so that quotient
always converts to `i64`; adding it to an arbitrary custom-provider mark can still overflow and
traps. Resolution conversion checks the complete platform `timespec`, including its seconds field,
rather than returning only `tv_nsec`. The result is the provider-reported nominal resolution and may
be a lower bound on actual observable precision; the implementation never attempts timing-based
precision discovery.

Normalizing arbitrary constructor inputs was considered. It would make several different field
pairs silently name one mark and would still not solve provider provenance. `make` instead traps on
a noncanonical fraction. Native `waitUntil` accepts only the canonical non-negative POSIX form;
evaluator hosts retain the complete signed-`i64` domain so deterministic providers with a negative
origin can compare and advance their own timeline.

### Five independently consumed intrinsics are the minimum native clock surface

Add these conceptual operations to the existing native-only `Os` intrinsic actor (their public
spelling remains `Intrinsic.os...` through the current catalog):

```silk
Intrinsic.osSystemClockNow(
  seconds: &mut i64,
  nanoseconds: &mut i64
) -> Effect<bool>

Intrinsic.osSystemClockResolution(
  nanoseconds: &mut u64
) -> Effect<bool>

Intrinsic.osMonotonicClockNow(
  seconds: &mut i64,
  nanoseconds: &mut i64
) -> Effect<bool>

Intrinsic.osMonotonicClockResolution(
  nanoseconds: &mut u64
) -> Effect<bool>

Intrinsic.osMonotonicClockWaitUntil(
  seconds: i64,
  nanoseconds: i64
) -> Effect<bool>
```

Each operation has exactly one canonical provider consumer in the current intrinsic catalog, so the
two providers stay independently reachable without a selector that couples their identities. The C
boundary chooses its private platform constant and never exposes that value to Silk. A read commits
both outputs only after a successful, valid platform result. Resolution commits one positive whole-
nanosecond value only after checked conversion. Wait succeeds only after the selected monotonic
deadline.

False is deliberately the complete low-level failure surface. The requested public services have no
typed error, and exposing `errno` would invite provider-dependent recovery the public contract cannot
express. Ordinary OS-provider source translates false, invalid output, or arithmetic overflow to the
existing fatal arithmetic trap. The evaluator similarly maps an injected provider failure or throw
to false; a missing provider remains explicit blockage before any result exists.

A separate relative-wait intrinsic was rejected because `waitFor` is exactly expressible as
one `now`, checked source arithmetic, and the absolute primitive. Public `Instant` construction,
validation, service types, fatal policy, and platform selection remain outside the compiler.

The native symbols derived by the existing naming rule are:

```text
silk_os_system_clock_now_v1
silk_os_system_clock_resolution_v1
silk_os_monotonic_clock_now_v1
silk_os_monotonic_clock_resolution_v1
silk_os_monotonic_clock_wait_until_v1
```

Their direct C signatures are respectively `int32_t (int64_t *, int64_t *)`,
`int32_t (uint64_t *)`, `int32_t (int64_t *, int64_t *)`, `int32_t (uint64_t *)`, and
`int32_t (int64_t, int64_t)`. The `int32_t` result is the canonical four-byte Silk `bool` lane.
Pointer outputs are written only on success; the two
component outputs may be committed sequentially after validation because Silk observes them only
after the call returns.

`BootstrapOsIntrinsics.execute` currently assumes every `OsCall` ends with filesystem-style reason
and native-code outputs. Clock operations are dispatched before that assumption, using their own
smaller protocol; existing operations and status traces remain unchanged.

### Portable and native actors stay separate

Four manifest modules follow the existing service/provider split:

| Module | Layer | Public actor |
|---|---|---|
| `silk/system_clock` | portable | `Instant`, `SystemClock`, wrappers |
| `silk/monotonic_clock` | portable | `MonotonicClock`, wrappers |
| `silk/os_system_clock` | native provider | stateless `OsSystemClock`, `make` |
| `silk/os_monotonic_clock` | native provider | stateless `OsMonotonicClock`, `make` |

Constructors perform no host operation. Each provider maps service operations to sibling actor
functions and confines unsafe calls to private raw helpers. Portable modules import no OS provider;
the monotonic module imports only the shared `Instant` type and portable scalar actors. Applications
provide clocks explicitly at their outer boundary, and unused provider source contributes no native
symbol.

One `OsClocks` module containing both providers was rejected because it couples independent
capabilities and makes source closure pay for an unrelated actor. Putting the OS implementation
inside either portable module was rejected because pure providers must remain valid on direct Wasm.

### CLOCK_REALTIME and CLOCK_MONOTONIC are the portable native mapping

`ToolchainPlan.shimSource` emits one translation-unit preamble immediately after its leading comment,
before standard streams, termination support, or any selected runtime fragment can include a system
header. That preamble defines `_DARWIN_C_SOURCE` on Apple or `_GNU_SOURCE` on Linux and defines
`_POSIX_C_SOURCE` as `200809L`; capability fragments then include only the headers they need. The clock
fragment includes `<time.h>`. System read and resolution use `CLOCK_REALTIME`; monotonic read and
resolution use `CLOCK_MONOTONIC`. The C boundary validates `tv_nsec`, the `time_t` to `int64_t` round
trip, and checked resolution conversion before committing outputs.

This choice intentionally leaves suspend behavior platform-defined. Linux documents
`CLOCK_MONOTONIC` as excluding suspend; macOS documents it as including suspend. Both satisfy the
portable non-decreasing contract, and each platform's waits use exactly the clock returned by
`now`. `CLOCK_BOOTTIME`, `CLOCK_MONOTONIC_RAW`, `CLOCK_UPTIME_RAW`, and Mach clocks were rejected:
they either are not portable, change rate/suspend semantics, or require a second timebase for wait.

All current native targets have 64-bit pointers and `time_t`. The runtime still checks conversion.
Adding a 32-bit or non-POSIX target is a new target proposal, not permission for silent truncation.
The no-extra-library Linux path pins the current target baseline at `glibc >= 2.17`, where
`clock_gettime` and `clock_nanosleep` are provided by libc; an older `librt` baseline is a separate
target/toolchain decision.

`OsRuntime.source(selected)` must stop prepending one filesystem-heavy common block to every OS
symbol. It returns only minimal selected fragments for the existing filesystem, standard-input,
child-process, and host-input groups and the new clock group; shared helpers are emitted only when a
selected group needs them. `ToolchainPlan.shimSource` is the sole owner of the translation-unit
preamble and places it before those fragments. This keeps a clock-only program portable across the
POSIX boundary it actually uses and preserves the existing pay-for-use contract; it is a runtime-
source organization change, not a second ABI or compatibility path.

### Linux waits absolutely; the common Unix fallback rechecks the deadline

On Linux the wait symbol builds a validated `timespec` and calls:

```c
clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &deadline, NULL)
```

It returns success on zero, retries the unchanged deadline when the function returns `EINTR`, and
returns false for every other positive error number. It must not inspect `errno` for this function:
unlike `clock_gettime` and `clock_getres`, `clock_nanosleep` returns the error number directly.

The non-Linux POSIX branch, including macOS, loops:

1. read `CLOCK_MONOTONIC`;
2. return success when `now >= deadline`;
3. compute the normalized positive remainder;
4. call `nanosleep` for that remainder;
5. after a zero return, loop to step 1 and re-read against the original deadline; and
6. after `EINTR`, discard the relative remainder and also loop to step 1.

Re-reading after both success and interruption prevents an early return or time spent in signal
handling from completing the wait too soon, and stays correct on a platform whose monotonic clock
advances during system sleep. Any other read or non-`EINTR` sleep error returns false. A completed
wait may run late because timer granularity and scheduling impose no upper latency bound.

Using the fallback for every platform was considered and is semantically valid, but Linux's native
absolute API is simpler and directly avoids restart drift. `mach_wait_until` was rejected because it
uses Mach absolute ticks, which are not the selected macOS `CLOCK_MONOTONIC` timeline.

### waitFor is source-derived and waits block the process thread

`OsMonotonicClock.waitFor` reads its own provider once, converts `howLong` by quotient/remainder,
adds a checked deadline, and calls its sibling `waitUntil`. A zero duration may still perform the
single read but must not require positive clock advancement. The public service includes `waitFor`
because the supplied surface requires each provider to define it; only the official OS provider's
implementation is canonically derived.

Both wait operations are Effects because they use a service and may use a host boundary, not because
they are asynchronous. A virtual provider may advance its private logical timeline synchronously.
The official OS provider blocks the calling native thread; if called while `LocalScheduler` is
driving a Fiber, it also prevents every other local task from running until the syscall returns.
This is the same honest boundary style as existing blocking standard input and child execution.

Parking only the current Execution would require a timer registration lifetime, one-shot Wake
ownership, cancellation cleanup, scheduler integration, and a host event source capable of making
progress while no Silk task runs. That is a future timer capability, not a hidden implementation
detail of these service definitions.

### Evaluator clock hosts are exact and independent

Add `SystemClock.ts` and `MonotonicClock.ts` boundary actors. Host values use `bigint` for seconds,
nanoseconds, and resolution so evaluator behavior never rounds through JavaScript `number`.
Validated scripted constructors return explicit construction-failure data for seconds outside
signed 64-bit range, a malformed fraction, a resolution outside positive `u64`, or a decreasing
monotonic sequence. Validation uses comparisons, never `BigInt.asIntN`/`asUintN` wrapping. A
monotonic wait records its deadline and advances to it deterministically only when it is in the
future; a past wait never moves time backwards. Separate failing-host constructors produce explicit
boundary failures for fatal-path tests without admitting malformed clock values.

`BootstrapEvaluation.Options`, evaluator state, and `BootstrapOsIntrinsics.ExecutionContext` carry
the two hosts independently. Missing hosts produce `MissingSystemClock` and
`MissingMonotonicClock` in `BootstrapTrace.BlockedReason`, with corresponding inspector and flow
presentation. An `OsCall` trace retains the operation and completed/false result, and preserves the
original cause if an injected provider throws while translating the Silk result to false. It never
records ambient real timestamps, preserving deterministic traces.

One combined clock host was rejected because it would prevent tests and applications from granting
or replacing system and monotonic authority separately. Falling back to `Date.now` or
`performance.now` was rejected because evaluation is deterministic data unless the caller injects
real time explicitly.

### Tests prove semantics at the cheapest tier

One clock feature test file should share analysis snapshots and cover:

- canonical system values including pre-epoch representation, constructor rejection, and accessors;
- exclusive service requirements and ordinary fixed/scripted providers;
- monotonic equality, advancement, past deadlines, zero waits, and exact `u64` resolution;
- independent injected hosts, missing-host blocked reasons, boundary-failure traps, and trace order;
- exactly five admitted intrinsic identities and the smallest reachable native-symbol inventory;
- direct-Wasm rejection only when an OS operation is reachable; and
- standard-library manifest, navigation, documentation, and generated-output integration through
  existing global suites.

Native tests prove ABI/link/run for reads, resolutions, past deadlines, and a tiny positive wait
without asserting elapsed wall time. Interrupt/retry and deadline arithmetic use a deterministic C
harness that substitutes clock and sleep functions, proving scripted `EINTR`, clock advancement,
past-deadline return, and non-drifting retries. Linux CI exercises the absolute branch; a dedicated
`macos-15` arm64 job exercises the fallback and the repository's `aarch64-apple-darwin` native path.
No correctness test uses timing thresholds, instruction counts, or fresh-process feature
determinism. If a portable result program is useful to the global native
differential, add it once to `test/support/corpus.ts` instead of another feature-local compile loop.

The final native-executable artifact cache key includes the exact generated shim source for
`artifact.nativeRuntimeSymbols`, not a termination-only shim. Distinct selected clock symbol sets
therefore cannot reuse an executable cached for a different runtime inventory; the lower-level shim
cache retains the same source-derived property.

## Risks / Trade-offs

- [The shared `Instant` permits cross-clock and cross-provider marks] → State the provenance
  precondition wherever monotonic marks are introduced or consumed; do not claim static safety. A
  future breaking proposal may replace it with a nominal `Mark` if the requested API is revisited.
- [A native wait blocks all tasks on the local scheduler] → Make blocking normative and prominent;
  design scheduler-aware timers as a separate capability with cancellation and wake ownership.
- [System and monotonic suspend behavior differs across Linux and macOS] → Specify only
  non-decreasing behavior and require waits to use the same platform clock as reads.
- [A host failure has no typed recovery channel] → Keep the five primitives atomic, return false,
  and trap once in ordinary provider source; never fabricate a clock value.
- [Signal-heavy waits can reveal retry bugs] → Use an unchanged Linux absolute deadline and a
  macOS/common fallback that re-reads the original deadline; test both with substituted C calls.
- [Claims about generic Unix may outrun supported targets] → Promise executable support only for
  current triples; keep a POSIX fallback design ready, and verify each new triple before adding it.
- [New native symbols can accidentally enter unused artifacts] → Extend exact reachable inventory,
  shim-source, cache-key, and direct-Wasm tests.

## Migration Plan

This is additive. Land the intrinsic catalog and evaluator/native boundary support with the four
canonical source modules, manifest update, generated embedding, generated reference, and tests in
one change. No provider is ambient, so existing entries and artifacts are unchanged unless source
reaches a new OS clock primitive.

Rollback removes the four manifest modules, regenerated artifacts, five intrinsic catalog entries,
two evaluator hosts and blocked reasons, five C runtime symbols, and feature tests together. There
is no stored data, compatibility shim, migration alias, or partial runtime protocol to retain.
