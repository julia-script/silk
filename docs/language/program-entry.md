# Program entry

A Silk executable starts from one public function named `main` in the root module. The entry may be
an ordinary function or an effect function. A private function named `main` is an ordinary
module-local function, not an executable entry point.

## ENTRY-001 — `main` must be public

**Status:** Confirmed

The executable entry must be declared with `pub`. The supported shapes are a zero-argument ordinary
`main` explicitly returning `()` or `i32`, or a zero-argument effect `main` succeeding with `()` and
carrying no unresolved requirements. Only the effect entry may omit its unit result annotation.

```silk
pub fn main() -> () {
}
```

```silk
pub fn main() -> i32 {
  return 0
}
```

```silk
pub effect fn main() {
}
```

An empty effect entry succeeds with `()`.

**Boundary:** Removing `pub` makes the function private and leaves the executable without a usable
entry.

```silk,ignore
effect fn main() {
}
```

**Diagnostics:** A private root `main` must produce an entry diagnostic that identifies the missing
`pub` visibility at the declaration. An ordinary `main` without an explicit result receives the
ordinary missing-result diagnostic rather than entry-specific unit inference. No stable diagnostic
code is currently assigned.

**Implementation:** Entry discovery retains private visibility as its own reason, and the CLI
reports `No entry point: \`main\` must be public`.

**Evidence:** [entry-instance requirements](../../openspec/specs/bootstrap-instances/spec.md),
[entry selection](../../packages/compiler/src/Instances.ts),
[CLI entry messages](../../packages/compiler-cli/src/Report.ts).

## ENTRY-002 — The compiler executes an effect entry

**Status:** Confirmed

The three entry forms are:

```silk
pub fn main() -> () {
}
```

```silk
pub fn main() -> i32 {
  return 0
}
```

```silk
pub effect fn main() {
}
```

All three forms take no parameters and declare no generic parameters. The compiler recognizes an
`effect fn main`, constructs its Effect, and executes it exactly once through the generated program
entry boundary. Source does not call `run main()`. An omitted result annotation on the effect entry
means `()`, so the explicit spelling `pub effect fn main() -> ()` is equivalent but unnecessary.

**Boundary:** Entry kind follows the declaration, not its return type. An ordinary `fn main` must
explicitly return `()` or `i32`; returning `Effect<()>` does not make it an effect entry. An
`effect fn main` must succeed with `()`.

**Diagnostics:** An invalid ordinary result must identify the allowed `()` and `i32` entry results.
An invalid effect success type must identify the required `()` success type. Stable source
diagnostic codes for invalid entry shapes are not yet assigned. Omitting the effect entry's unit
result is valid and must not produce an entry diagnostic; omitting an ordinary entry result remains
invalid.

**Evidence:** [entry-instance requirements](../../openspec/specs/bootstrap-instances/spec.md),
[effect-entry tests](../../packages/compiler/test/EffectEntry.test.ts).

## ENTRY-003 — Unhandled effect-entry failures become process failures

**Status:** Confirmed

An effect entry may retain any concrete, valid typed failure type. The generated entry boundary
converts an unhandled failure into a runtime error report and a nonzero process status. Successful
completion returns process status zero.

The failure remains typed inside the program. Only the generated host boundary converts it into
process behavior. Declaring the failure in `main` is the explicit decision that it may reach that
boundary; no marker interface or second opt-in is required.

```silk
pub struct ProblemError {}

pub effect fn main() ! ProblemError {
  fail ProblemError {}
}
```

When `ProblemError` reaches the generated boundary, the adapter reports its canonical type
identity, retains its hidden logical Effect trace, releases its owned payload, and exits
unsuccessfully. A later optional formatting protocol may customize the report, but custom
formatting is not a condition for being a typed failure. See
[typed-failure cleanup and diagnostic context](typed-failures.md#fail-006--typed-failure-applies-ordinary-cleanup-and-preserves-diagnostic-context).

**Boundary:** Entry failures must still satisfy the ordinary failure-type rules: the type is
concrete, every possible payload is owned and detached from lexical borrows and providers, and no
unresolved generic remains. Requirement closure remains the separate ENTRY-004 boundary.

**Diagnostics:** A valid concrete failure type receives no entry-specific diagnostic. Invalid
failure types and payloads receive the ordinary typed-failure diagnostics at their source. Exact
process-report rules are defined in
[program termination and reporting](program-termination-and-reporting.md).

**Implementation:** Analysis, entry discovery, evaluation, and backend planning accept the example
without marker conformance. The retained failure metadata contains its canonical type identity and
ordinary cleanup plan.

**Evidence:** [entry-instance requirements](../../openspec/specs/bootstrap-instances/spec.md),
[effect-entry runtime tests](../../packages/compiler/test/EffectEntry.test.ts).

## ENTRY-004 — Effect-entry requirements must be resolved

**Status:** Confirmed

An effect entry must have an empty requirement row after composition. Every dependency must be
provided explicitly before the entry Effect completes. The compiler does not currently synthesize
an implementation for a missing requirement.

```silk
import silk.effect as Effect

service Clock {}

struct SystemClock {}

impl Clock for SystemClock {}

effect fn work() -> () ? &Clock {
  return ()
}

pub effect fn main() {
  let clock = SystemClock {}
  return run Effect.provide<Clock>(work(), &clock)
}
```

**Boundary:** An entry that retains a requirement is invalid:

```silk,ignore
service Clock {}

effect fn work() -> () ? &Clock {
  return ()
}

pub effect fn main() ? &Clock {
  return run work()
}
```

**Diagnostics:** An open effect entry must be rejected before backend emission. The entry diagnostic
must list every unresolved requirement. A stable source diagnostic code is not yet assigned.

Only dependency-eligible services may appear in the row, as defined by
[SERV-002](requirements-and-services.md#serv-002--only-services-may-be-effect-requirements).

**Deferred direction:** A future proposal may let an entry adapter supply target-specific defaults
for selected capabilities, such as a standard-output logger, while allowing explicit source
provision to replace the default. No implicit provider, selection rule, or override rule is part of
the current language.

**Evidence:** [entry-instance requirements](../../openspec/specs/bootstrap-instances/spec.md),
[effect-entry provision tests](../../packages/compiler/test/EffectEntry.test.ts).

## ENTRY-005 — An ordinary entry explicitly returns `()` or `i32`

**Status:** Confirmed

An ordinary entry has one of two valid result shapes: `pub fn main() -> ()` or
`pub fn main() -> i32`. The declaration kind and explicit result annotation determine the entry
shape; the integer does not turn the function into an Effect entry.

```silk
pub fn main() -> () {
}
```

```silk
pub fn main() -> i32 {
  return 7
}
```

The generated host outcomes for these shapes, including native and Wasm status behavior, are
defined by [TERM-001](program-termination-and-reporting.md#term-001--an-ordinary-entry-explicitly-returns-unit-or-one-status-value).

**Boundary:** The explicit return annotation is required in both ordinary forms. Supporting
`pub fn main() {}` belongs to a future general return-omission decision rather than an entry-only
exception.

**Diagnostics:** Any ordinary entry result other than `()` or `i32` reports an invalid entry shape
and names both permitted types. A missing ordinary result annotation receives the ordinary
missing-result diagnostic.

**Evidence:** [confirmed stabilization decision](README.md),
[TERM-001](program-termination-and-reporting.md#term-001--an-ordinary-entry-explicitly-returns-unit-or-one-status-value).

Exact process-report behavior is defined in
[program termination and reporting](program-termination-and-reporting.md). A later diagnostic pass
will assign stable codes to invalid entry shapes. Default entry providers are not current
semantics and require an explicit future specification.
