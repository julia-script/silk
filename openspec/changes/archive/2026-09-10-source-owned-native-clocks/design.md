## Context

See proposal.md. System-clock source already uses libc, while the monotonic provider still reaches three generated adapters. All selected targets have 64-bit signed time_t/long and a 16-byte, 8-aligned timespec. Independent C compilation has additionally established Darwin clockid_t is unsigned 32-bit; GNU clockid_t is signed 32-bit. Existing Darwin i32 declarations will be corrected.

## Goals / Non-Goals

**Goals:** exact selected declarations, source validation and retry policy, complete adapter deletion and reproducible object/C evidence.

**Non-Goals:** scheduler redesign, timer queues, VDSO/raw clocks, typed clock failures, or a Wasm clock host boundary.

## Decisions

### One source boundary shared by both providers

A selected `silk.native_clock` owns Timespec, selected clock identifier scalar types, clock_gettime/getres and platform wait declarations. This avoids two nominal record types for the same foreign signature. Darwin uses u32 IDs 0/6; GNU uses i32 IDs 0/1. Each timespec field is i64 at offsets 0/8. Source-only wrappers operate on these ordinary records. The compiler does not recognize clock names. Pointer parameters are noncapturing; no pure/read-only claim is made about the external operation.

### Preserve failure-free services and clock domains

Construction has no foreign calls. System time permits negative seconds and external backward adjustment. Native monotonic reads/waits require nonnegative seconds, canonical fractions, and one consistent CLOCK_MONOTONIC domain. Resolution must be canonical, positive and fit u64; checked arithmetic traps before wrap. Host failures and invalid values trap through the existing generic fatal boundary. No errno read is needed for clock_gettime/getres because their service policy does not branch on the error code.

### One deadline, two selected wait conventions

GNU uses clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, deadline, null). Retry only direct status EINTR=4, retaining the identical deadline. Status zero succeeds, other statuses trap; errno is never consulted. A nonnegative past deadline is accepted by the platform and completes immediately.

Darwin first reads CLOCK_MONOTONIC and returns when the deadline is reached. Otherwise it subtracts canonical split components, applying a fractional borrow, and calls nanosleep(remaining, null). Status zero rereads time. Status -1 captures __error immediately; EINTR rereads time, other errors trap. Other nonzero statuses trap without retry. Successful or interrupted relative sleeps never replace the original deadline. Checked subtraction traps on an unrepresentable remainder; native nonnegative marks bound it to i64.

waitFor retains exactly one initial read followed by MonotonicClock.deadlineAfter. That helper owns carry and overflow; waitUntil and waitFor share the same absolute wait policy. Scheduler task-local parking remains separate from direct blocking OS waits.

### Exact authorities and deliberate prior-art divergences

`supplies.json` pins Apple SDK 15.5/deployment 11.0, GNU glibc 2.36/GCC 12 and LLVM 22.1.8, plus exact headers and retrieved prior-art hashes. Independent C checks validate sizes, alignment, offsets, signedness, constants and function-pointer signatures. Hand-authored catalog declarations remain unverified until the full source/C lanes execute; no metadata claims completion from research alone.

Pinned Zig Threaded.sleepPosix distinguishes direct libc status from raw syscall errno. Its relative nanosleep path reuses returned remainder and accepts unexpected errors; Silk deliberately recomputes from the original deadline and traps unexpected errors. Rust unix Instant selects Darwin CLOCK_UPTIME_RAW, unlike Silk's existing CLOCK_MONOTONIC suspend behavior; do not copy that selection. Rust's checked arithmetic tests inform range boundaries. The pinned Zig Linux tests cover timerfd rather than this wait contract, so there is no claimed matching retry test. Neither project's tests were executed; timing assertions from prior art are excluded from Silk correctness tests.

### Evidence at the cheapest useful tier

Reuse clock analysis/StaticEvaluation tests for canonical fields, positive resolution, deadline carry/overflow and source availability. Replace generated-C OsClockRuntime fixtures with separately compiled C receivers linked to source exports, testing exact read/resolution/error and wait scripts at O0/O2. Required native lanes execute Darwin ARM64 and GNU x86-64/ARM64 using existing supplies/containers; missing lanes fail and LTO is rejected. Real-clock behavior remains in the shared native corpus with semantic invariants and no elapsed-time threshold. Portable replacements lower to Wasm with no clock imports.

## Risks / Trade-offs

- Darwin/GNU clock IDs differ in signedness → independently compile selected signatures against headers and inspect source imports.
- errno can be changed by another call → capture it only after failed nanosleep, before any clock reread.
- Interruptions or early relative success can introduce drift → scripted receivers assert each remaining interval comes from the original deadline.
- Fatal traps cannot promise cleanup → retain the existing failure-channel-free contract without claiming recoverability.

## Migration Plan

Implement the shared ordinary source boundary, migrate both providers and consumers, delete all three intrinsic/runtime families and clock C prelude, update catalogs/docs, execute all conformance and required repository gates, then publish via gh stack. This greenfield migration keeps no old path. Rollback is a whole commit revert, not runtime fallback.
