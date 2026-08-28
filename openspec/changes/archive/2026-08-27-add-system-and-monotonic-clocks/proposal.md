## Why

Silk has no provider-replaceable way to read civil time, measure elapsed time, or wait for a
monotonic deadline. Applications must either invent non-portable host boundaries or cannot express
these basic capabilities at all, while the existing service and native-runtime architecture is now
ready to expose them without granting standard-library declarations compiler privilege.

## What Changes

- Add canonical `silk/system_clock` source containing the public `Instant` value and an exclusive,
  infallible `SystemClock` service for Unix-epoch time and clock resolution.
- Add canonical `silk/monotonic_clock` source containing an exclusive, infallible `MonotonicClock`
  service for non-decreasing marks, resolution, absolute waits, and relative waits; the official
  native provider implements those waits by blocking its host thread.
- Preserve the requested shared `Instant { seconds: i64, nanoseconds: i64 }` representation for
  both clocks. Returned values are canonical (`0 <= nanoseconds < 1_000_000_000`); monotonic marks
  are meaningful only with the provider that produced them.
- Add separate stateless `OsSystemClock` and `OsMonotonicClock` ordinary-source providers over the
  smallest native-only unsafe clock intrinsics.
- Implement the native boundary for the repository's Linux and macOS targets through POSIX
  `CLOCK_REALTIME`, `CLOCK_MONOTONIC`, and `clock_getres`; use Linux absolute
  `clock_nanosleep`, with a portable Unix `nanosleep`/re-read fallback used on macOS and any future
  supported Unix target lacking `clock_nanosleep`.
- Pin the no-extra-library Linux implementation to the current `glibc >= 2.17` native baseline;
  older glibc targets that require `librt` are not introduced by this change.
- Add explicit evaluator clock hosts, deterministic scripted-host coverage, reachable-only runtime
  linking with capability-specific runtime preludes, generated source/documentation updates, and
  native acceptance tests.
- Exclude WASI/direct-Wasm clock imports, Windows, timezone/calendar formatting, clock mutation,
  non-blocking timers, scheduler/reactor integration, and ambient default providers.

## Capabilities

### New Capabilities

- `bootstrap-clock-services`: public system and monotonic clock semantics, shared `Instant`
  invariants, blocking waits, ordinary native providers, platform behavior, and pay-for-use target
  limits.

### Modified Capabilities

- `bootstrap-intrinsic-boundary`: admit only the target-neutral native clock reads, resolution
  query, and monotonic absolute-wait primitive needed by the ordinary-source providers.
- `bootstrap-evaluation`: accept independent injected system-clock and monotonic-clock hosts and
  block a reachable native-provider call when its matching host is absent.
- `bootstrap-silk-stdlib`: ship the two portable clock modules and two separate OS-provider modules
  as canonical documented source without ambient provision or module-name privilege.

## Impact

The change affects the standard-library source manifest and generated embedding, intrinsic/HIR/MIR
inventories, evaluator host configuration and blocked reasons, the reachability-selected C runtime
shim, native symbol declarations, generated standard-library documentation, and compiler/native
acceptance tests. It adds no external package dependency and does not expand the current target
identity set.
