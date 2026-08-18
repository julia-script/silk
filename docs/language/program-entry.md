# Program entry

A Silk executable starts from one public function named `main` in the root module. The entry may be
an ordinary function or an effect function. A private function named `main` is an ordinary
module-local function, not an executable entry point.

## ENTRY-001 — `main` must be public

**Status:** Candidate

The executable entry must be declared with `pub`. The two supported shapes are a zero-argument
ordinary `main` returning `i32`, or a zero-argument effect `main` succeeding with `()` and carrying
no unresolved requirements. The effect entry may omit its unit result annotation.

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
`pub` visibility at the declaration. No stable diagnostic code is currently assigned.

**Current compiler:** The compiler currently reports:

```text
No entry point: `main` must declare a resolved return type
```

The return type is resolved. Entry discovery groups private visibility together with unresolved
typing under one `UntypedEntry` reason, so the message hides the actionable requirement. The source
diagnostic should say that `main` must be public, or that the root module has no public entry.

**Evidence:** [entry-instance requirements](../../openspec/specs/bootstrap-instances/spec.md),
[entry selection](../../packages/compiler/src/Instances.ts),
[CLI entry messages](../../packages/compiler-cli/src/Report.ts).

## ENTRY-002 — The compiler executes an effect entry

**Status:** Confirmed

The two entry forms are:

```silk
pub fn main() -> i32 {
  return 0
}
```

```silk
pub effect fn main() {
}
```

Both forms take no parameters and declare no generic parameters. The compiler recognizes an
`effect fn main`, constructs its Effect, and executes it exactly once through the generated program
entry boundary. Source does not call `run main()`. An omitted result annotation on the effect entry
means `()`, so the explicit spelling `pub effect fn main() -> ()` is equivalent but unnecessary.

**Boundary:** Entry kind follows the declaration, not its return type. An ordinary `fn main` must
return `i32`; returning `Effect<()>` does not make it an effect entry. An `effect fn main` must
succeed with `()`.

**Diagnostics:** An invalid ordinary result must identify the required `i32` entry result. An
invalid effect success type must identify the required `()` success type. Stable source diagnostic
codes for invalid entry shapes are not yet assigned. Omitting the unit result is valid and must not
produce an entry diagnostic.

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
failure types and payloads receive the ordinary typed-failure diagnostics at their source. The exact
process-report rendering remains to be stabilized.

**Current compiler:** Disputed. `silk check` currently accepts the example above, but `silk build`
rejects it during entry discovery with:

```text
No entry point: every effectful `main` failure must conform to `Report`
```

Entry discovery requires the compiler-sealed, operation-free `Report` marker even though the marker
contributes no formatting or runtime behavior. That requirement is not part of the stabilized
language rule and must be removed during implementation reconciliation; analysis and build must
also agree on entry validity.

**Evidence:** [entry-instance requirements](../../openspec/specs/bootstrap-instances/spec.md),
[effect-entry runtime tests](../../packages/compiler/test/EffectEntry.test.ts).

## ENTRY-004 — Effect-entry requirements must be resolved

**Status:** Confirmed

An effect entry must have an empty requirement row after composition. Every dependency must be
provided explicitly before the entry Effect completes. The compiler does not currently synthesize
an implementation for a missing requirement.

```silk
import silk.effects as Effect

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

## Pending rules

Later passes will cover exact process-report rendering, ordinary-entry exit semantics, and the
stable diagnostic assigned to each invalid entry shape. Removing the obsolete `Report` marker from
the compiler, standard library, specifications, tests, and generated documentation belongs to the
later implementation-reconciliation pass. Default entry providers require a separate language
proposal before they can become current semantics.
