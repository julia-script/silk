## Context

See `proposal.md`. `OsSystemClock` currently writes scalar outputs through
`Intrinsic.osSystemClockNow` and `Intrinsic.osSystemClockResolution`; native lowering maps them to
generated `silk_os_system_clock_*_v1` functions and evaluation maps them to a bespoke TypeScript
provider. The ordinary foreign-call path already supports C-layout record pointers, libc linkage,
evaluator availability, and direct-Wasm imports.

## Goals / Non-Goals

**Goals:**

- Make the standard-library source the sole owner of system-clock ABI selection and validation.
- Remove every compiler-specific system-clock route in the same change.
- Keep target behavior explicit and deterministic when no host binding is supplied.
- Reuse existing clock tests and native acceptance without another native process.

**Non-Goals:**

- Migrate the monotonic clock or another OS subsystem.
- Add evaluator memory mutation to foreign-host bindings.
- Add a default direct-Wasm clock implementation.
- Change the portable `SystemClock` service or `Instant` representation.

## Decisions

### Bind the POSIX real-time clock directly in the provider

`silk/os_system_clock` declares a C-layout `Timespec { seconds: i64, nanoseconds: i64 }`,
`clock_gettime`, and `clock_getres`. Both supported Unix-family targets use `CLOCK_REALTIME = 0`
and 64-bit fields for this ABI. Source validates the fraction, converts resolution to positive
whole nanoseconds with checked Silk arithmetic semantics, and traps through the existing public
provider policy when libc fails or returns unusable data.

Alternative: retain the intrinsic as a portability shim. Rejected because it preserves exactly the
compiler privilege this migration exists to remove and is unnecessary for the supported targets.

### Use ordinary foreign availability on non-native execution surfaces

Evaluator preflight reports `ForeignHostUnavailable` for the first missing exact binding. The
current foreign-host API cannot write through a logical pointer, so this change defines absence and
does not pretend to supply an evaluator clock. Direct WebAssembly emits versioned foreign imports;
the embedding host owns memory mutation and return status if it chooses to provide them.

Alternative: keep the bespoke `systemClock` evaluator option. Rejected as a compatibility path
whose semantics would differ from the source-visible extern declarations.

### Delete the obsolete host actor and blocked reason

The TypeScript `SystemClock` actor, evaluation option, `MissingSystemClock` blocked reason, and
inspector branches exist only for the deleted intrinsic route. They are removed. Shared validation
types still needed by the monotonic evaluator host move into the `MonotonicClock` actor.

### Keep test cost on existing tiers

Pure clock analysis asserts both supported target triples retain the two foreign imports and no
system-clock runtime symbol. Existing evaluator and direct-Wasm tests assert explicit
unavailability/imports. The existing serialized native clock acceptance continues to compile and
run both providers, so no additional process is introduced.

## Risks / Trade-offs

- [A platform ABI differs from the declared record] → Pin target layout analysis for Darwin and
  Linux and execute the existing native acceptance on each CI host.
- [Resolution arithmetic overflows or returns zero] → Ordinary checked scalar operations trap,
  matching the provider's documented fatal boundary.
- [A stale compiler route survives] → Inventory, runtime-source, reserved-symbol, generated-source,
  and repository string checks prove the system-clock identities are absent.
