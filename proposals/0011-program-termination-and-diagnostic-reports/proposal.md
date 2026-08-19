# SLP-0011: Program termination and diagnostic reports

SLP: 0011
Status: Draft
Revision: 3
Author: Julia Ortiz
Created: 2026-08-19
Updated: 2026-08-19
Discussion: —
Review record: —
Depends on: SLP-0008, SLP-0009
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Silk should distinguish three program terminations: an ordinary `main` explicitly returns `()` for
status zero or `i32` for an application-chosen status; a successful effect entry returns zero; and
an unhandled typed failure produces a structured failure termination with conventional status `1`.
Fatal traps remain a distinct abnormal outcome with no stable numeric status. A standalone host
adapter renders failures and reportable traps to its diagnostic stream, while embedded targets
receive equivalent structured termination data without inventing a console. Every unhandled
failure report names the canonical error type and its logical source trace, but never requires
`Report` conformance or reflectively prints payload fields.

The author confirmed TERM-001–012 as the stabilization direction. Their identifiers remain
proposal-local until Candidate preparation and any later OpenSpec handoff.

## Problem and evidence

The stabilized entry rules already say that an effectful `main` may expose any valid concrete error
type, the compiler runs the Effect once, success becomes status zero, and an unhandled error becomes
a nonzero process failure. Typed-failure rules preserve hidden diagnostic context and require an
unhandled report to distinguish typed failure from trap.

The remaining contract is unclear:

- whether an ordinary `main` may return unit and whether a nonzero return is itself an error report;
- whether failure alternatives choose different process statuses;
- which parts of an error value are printed without the rejected `Report` marker;
- what trace survives optimization and suspension;
- how native executables and import-free or embedded Wasm expose equivalent outcomes; and
- which behavior is guaranteed for a fatal trap.

The current implementation makes one-based structural-union member tags double as effect-entry
return statuses and records canonical report identities in artifacts. That is useful bootstrap
machinery, but it leaks representation ordering into an application-visible status and still does
not define a complete user-facing report.

## Driving examples: current and desired

### Case: complete normally or return an application status

#### Intent

Let an ordinary program return unit for success or choose its own host status without manufacturing
an error.

#### Current Silk

```silk
pub fn main() -> i32 {
  return 7
}
```

The compiler already preserves `7` as the machine-entry result. The desired ordinary unit entry is
currently rejected during entry discovery because the compiler requires `i32`:

```silk,ignore
pub fn main() -> () {
}
```

#### Desired Silk

Both explicit forms are valid:

```silk
pub fn main() -> () {
}
```

```silk
pub fn main() -> i32 {
  return 7
}
```

#### Observable result

The unit form maps to machine status `0`. The integer form returns exact `i32` value `7`. Neither
emits an automatic error report. A native operating system may expose only its target-defined
process-status subset, such as the low byte on POSIX. A direct Wasm caller observes the exact
returned `i32`.

#### Boundary case

Omitting the return type from ordinary `main` is invalid in this initial model; unit must be written
as `-> ()`. Returning nonzero from ordinary `main` does not create a typed failure, capture an
Effect trace, or print an error. It is application-selected normal completion.

### Case: leave one typed error unhandled

#### Intent

Permit an ordinary error value to reach the entry boundary without a marker interface.

#### Current Silk

```silk
pub struct NotFoundError {
  resourceId: i32
}

pub effect fn main() ! NotFoundError {
  fail NotFoundError { resourceId: 42 }
}
```

Analysis accepts this intended model, while current build discovery rejects it unless the error
implements the operation-free compiler-known `Report` marker.

#### Desired Silk

The same program is valid without additional ceremony.

```silk
pub struct NotFoundError {
  resourceId: i32
}

pub effect fn main() ! NotFoundError {
  fail NotFoundError { resourceId: 42 }
}
```

#### Observable result

The failure payload is cleaned exactly once. A standalone adapter emits a diagnostic headed by the
canonical identity of `NotFoundError`, includes the failure origin and logical Effect trace, and
returns failure status `1`. It does not automatically expose `resourceId`; source that wants a
custom message must catch and report the error explicitly.

#### Boundary case

Adding a second possible error type does not assign status `2` to it. Union layout or declaration
order cannot change the public process status.

### Case: distinguish a fatal trap

#### Intent

Make an arithmetic invariant failure visibly different from an ordinary typed error.

#### Current Silk

```silk
pub fn main() -> i32 {
  return 1 / 0
}
```

#### Desired Silk

The source remains a trapping program.

#### Observable result

Execution terminates abnormally. When runtime state permits, the host reports a fatal trap, its
reason, source origin, and available logical call path. It does not label the event as an unhandled
typed error and does not promise a portable numeric exit status or cleanup.

#### Boundary case

A checked division API may return ordinary data or a typed error and can then be handled. The `/`
operator's trap cannot be caught after it occurs.

## Goals and non-goals

### Goals

- Give ordinary completion, typed failure, and fatal trap distinct observable outcomes.
- Preserve useful logical stack information across Effect composition and explicit suspension.
- Keep error values ordinary and require no reporting interface.
- Avoid leaking union tags, layout ordering, or backend representation into process statuses.
- Define honest behavior for standalone native, direct Wasm, and embedded execution boundaries.
- Keep automatic reporting outside source-visible Logger, Console, filesystem, or allocator rows.

### Non-goals

- A general formatting, reflection, serialization, or debugging interface for arbitrary values.
- Automatically printing error payload fields.
- Custom exit-code protocols for typed error types.
- A source-visible exception, defect, panic, or recoverable trap channel.
- A stable C ABI, Wasm component ABI, or embeddable runtime API.
- Exact terminal colors, paths, source excerpts, or platform-specific signal numbers.
- Defining Logger or Console standard-library services.

An optional future error-formatting interface is not excluded, but it must customize presentation
without becoming a condition for typed-error eligibility.

## Current language model

- `pub fn main() -> ()` and `pub fn main() -> i32` are the two ordinary entries; unlike the effect
  entry, the ordinary unit form does not omit its return annotation.
- `pub effect fn main() -> () ! E` is automatically constructed and run once when requirements are
  closed.
- A successful effect entry produces status zero.
- An unhandled typed failure retains an owned payload and hidden logical diagnostic context.
- A trap is fatal, uncatchable, and has no cleanup guarantee.
- The compiler currently requires obsolete `Report` conformance during build discovery and maps
  normalized failure members to one-based status tags.
- Direct Wasm currently remains import-free, so it cannot assume a diagnostic stream.

## Proposed language model

### TERM-001 — An ordinary entry explicitly returns unit or one machine status value

`pub fn main() -> ()` completes normally with machine status `0`. `pub fn main() -> i32` completes
normally by returning that exact `i32` from Silk's generated machine-entry contract. Neither form
emits an automatic diagnostic, even when the integer value is nonzero. No other ordinary result is
a valid entry result.

The ordinary declaration must write its result type explicitly. `pub fn main() {}` does not infer
the special entry result `()`; Silk may consider that general shorthand separately rather than
creating an entry-only omission rule. A process host may narrow or otherwise expose an `i32` status
according to its documented platform rules; Silk does not pretend POSIX, Windows, and embedded
hosts have identical process models.

### TERM-002 — A successful effect entry returns zero

`pub effect fn main()` completes successfully only after its Effect succeeds with `()`. The
generated machine entry then returns `0`. The unit result itself is not formatted or printed.

### TERM-003 — Every unhandled typed failure uses conventional status one

When any valid concrete failure reaches the effect entry boundary, the generated adapter produces
one structured unhandled-failure outcome and returns machine status `1`. The error's nominal or
union identity remains in the structured outcome and report, never in a numeric status ordinal.
Adding, removing, or reordering failure alternatives cannot change this status contract.

### TERM-004 — The minimum failure report names identity, origin, and logical path

Every unhandled typed-failure report contains:

1. an explicit `unhandled error` classification;
2. the canonical fully qualified error type identity;
3. the source origin at which that failure was created, when valid source provenance exists; and
4. the logical named function and Effect path from the failure origin outward to the entry.

The report may include source excerpts, colors, target details, or additional debug frames, but
those decorations cannot replace or reorder the stable minimum.

### TERM-005 — Automatic reporting does not inspect payload fields

The entry adapter does not reflect over or generically format the error payload. It cannot expose
private fields accidentally, add formatting requirements to every error, or fail because one field
has no display operation. The payload remains available to source recovery handlers before the
boundary and is cleaned before terminal reporting completes.

A future opt-in error-formatting interface may customize this presentation. It must remain ordinary
source policy and must never be required for a value to be used as an error.

### TERM-006 — Recovery history forms explicit causal context

If a recovery handler fails while handling an earlier failure, the new failure is primary. Its
report includes the retained earlier identity and trace as a `while handling` causal section.
Ordinary propagation preserves one context rather than adding duplicate causes; a successful
handler removes the handled context from the final outcome.

### TERM-007 — Logical traces survive optimization and suspension

The stable trace names source-declared function and Effect boundaries, not compiler helper frames,
physical machine frames, or backend-specific coroutine steps. Inlining cannot erase a logical
frame required by the report. `Effect.suspend` preserves the suspended invocation's position in the
logical path and does not create a user-visible scheduler or continuation frame.

Debug builds may add internal detail. Release builds must retain the stable minimum for fallible
entry paths rather than reducing an error to an unexplained status code.

### TERM-008 — Fatal traps use a distinct best-effort report

A trap terminates abnormally outside the typed-failure outcome. When state remains trustworthy, the
host report contains an explicit `fatal trap` classification, the trap reason, source origin, and
available logical path. Memory corruption or a violated unsafe contract may prevent a complete
report. Silk guarantees neither a portable numeric trap status nor structured cleanup after the
trapping operation.

### TERM-009 — Entry reporting is host-adapter behavior, not an ambient service

Automatic terminal reporting belongs to the compiler-generated entry adapter and matched
toolchain support. It does not add Logger, Console, filesystem, environment, allocator, or other
source requirements and does not make those facilities ambient inside the program. Failure while
writing a best-effort report cannot become a new typed error or replace the original termination.

### TERM-010 — Standalone and embedded hosts expose equivalent termination data

A standalone executable writes the automatic report to its host diagnostic stream, conventionally
standard error. A target without such a stream, including an import-free direct Wasm module, must
not synthesize one or gain a hidden host import. Its runner or embedding boundary instead receives
structured termination data containing the same classification, identity/reason, origin, and
logical path and may choose how to present it.

Exact embedding ABI is target/toolchain policy. Semantic parity requires equivalent information,
not identical bytes or an imaginary process abstraction.

### TERM-011 — Reporting happens after typed-failure cleanup

For an unhandled typed failure, every exited owner except the failure payload is cleaned during
propagation, then the payload itself is cleaned exactly once at the entry boundary. Hidden
diagnostic context survives that cleanup so reporting never needs to keep user-owned payload
storage alive. Trap reporting retains the existing no-cleanup guarantee.

### TERM-012 — Custom presentation and statuses are explicit source policy

A program wanting a custom message, payload rendering, telemetry event, or application-specific
status handles its typed error before it reaches the automatic boundary. It may use explicit
services and return an ordinary `i32` entry result after closing every Effect error and requirement.
No error naming convention, marker interface, or automatically discovered operation changes entry
behavior.

## Worked language experience

### Multiple errors share one failure status

```silk
pub struct NotFoundError {}
pub struct OfflineError {}

effect fn start(offline: bool) -> () ! NotFoundError | OfflineError {
  if offline {
    fail OfflineError {}
  }
  fail NotFoundError {}
}

pub effect fn main() ! NotFoundError | OfflineError {
  return run start(true)
}
```

Either unhandled member returns status `1`. The report identifies the actual canonical error type;
the structural-union member ordering is not observable through the status.

### Custom status requires handling

Illustrative standard-library operation names:

```silk
fn chooseStatus(error: NotFoundError | OfflineError) -> i32 {
  return match move error {
    NotFoundError {} => 4
    OfflineError {} => 5
  }
}

pub fn main() -> i32 {
  let outcome = run Effect.result(start(true))
  return match move outcome {
    Success {} => 0
    Failure { error } => chooseStatus(move error)
  }
}
```

The program closes the typed-failure channel and deliberately owns its host-status policy. Exact
`Effect.result` result syntax remains ordinary standard-library API rather than entry privilege.

### Causal recovery report

```silk
effect fn recover(_: NotFoundError) -> i32 ! OfflineError {
  fail OfflineError {}
}

pub effect fn main() ! OfflineError {
  drop run Effect.catch<NotFoundError>(load(), recover)
}
```

If `load()` fails with `NotFoundError` and `recover` then fails with `OfflineError`, the report names
`OfflineError` as primary and retains `NotFoundError` under `while handling`. The handler receives
only the ordinary `NotFoundError` value; causal context is not part of its type.

## Semantic sketch

1. The generated adapter invokes the selected user entry exactly once.
2. Ordinary unit completion produces status zero; ordinary integer completion forwards its `i32`.
   Neither produces an automatic report.
3. Effect success converts unit to machine status zero.
4. Effect failure completes structured cleanup while retaining hidden diagnostic context.
5. The adapter records the actual canonical failure identity, cleans the payload, then exposes the
   structured unhandled-failure termination and status one.
6. A standalone host renders that data to its diagnostic stream; an embedded host receives it
   through target-private runner machinery.
7. A trap bypasses the typed outcome and attempts only the distinct best-effort fatal report.

## Compiler–standard library boundary

### Compiler necessity

Only the compiler-generated host adapter can translate a closed Silk entry outcome into a native,
Wasm, or embedded termination contract after user cleanup is complete. Ordinary Silk source cannot
catch traps or write to a host diagnostic stream without acquiring explicit services.

### Smallest target-neutral primitive

No source-callable intrinsic is required. The compiler and matched runtime support need one private
structured termination representation carrying normal status, unhandled-failure metadata, or trap
metadata. Target adapters translate that representation to their honest host boundary.

### Standard-library construction

Custom formatting, logging, telemetry, error mapping, and application exit policy remain ordinary
Silk functions and Effects using explicit services. The standard library may later offer helpers,
but no declaration is recognized by name and no `Report` conformance is required.

### Privilege audit

The adapter needs only metadata already required by typed-failure propagation and trap provenance.
Reflective payload printing, compiler-known formatting interfaces, per-error exit codes, and ambient
logging are unnecessary privileges. A library-only adapter cannot close the actual host entry or
report fatal traps, so the private target boundary is irreducible.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Not affected — no new syntax | Existing entry and error declarations suffice. |
| Types and abstraction | Affected | Error identity is canonical type identity; no marker conformance or payload reflection. |
| Execution contracts | Affected | Normal status, Effect success, typed failure, and trap receive distinct terminal behavior. |
| Ownership and resources | Affected | Typed payload cleanup completes before reporting; traps retain no-cleanup semantics. |
| Runtime and targets | Affected | Native diagnostic streams and embedded structured outcomes preserve semantic parity without identical host APIs. |
| Compiler | Affected | Entry discovery, MIR, evaluator, adapters, native/Wasm lowering, and artifact metadata must stop using union ordinals as public statuses. |
| Standard library | Not affected — custom policy remains ordinary source | Future formatting helpers receive no compiler privilege. |
| Tooling and diagnostics | Affected | Runners render stable classifications, canonical identities, origins, logical paths, and causal sections. |
| Learning and use | Affected | Teach three terminal outcomes and the explicit route to custom reporting or status selection. |

## Scope cohesion

Entry status and terminal diagnostics are one decision because the observable boundary must classify
the same closed outcome before choosing both presentation and host status. General logging,
formatting, reflection, embedding ABIs, and async scheduling solve independent problems and remain
out of scope.

## Complexity and subtraction budget

The proposal removes `Report`, removes member-ordinal exit behavior, and avoids a generic reflection
system. It adds one stable minimum report and a structured embedded-host outcome. Logical trace
retention has runtime and artifact cost on fallible entry paths; the cost is justified by the
already-confirmed promise that failure cleanup does not erase diagnostic context.

## Surface displacement

- ENTRY-001 and ENTRY-002 admit explicit ordinary `()` alongside explicit ordinary `i32`.
- ENTRY-003 gains exact automatic failure status and report content.
- Ordinary-entry completion becomes explicit rather than pending, without introducing omitted
  ordinary return syntax.
- FAIL-006's retained diagnostic context receives its user-facing minimum rendering.
- FAIL-007's trap report receives a stable classification but no false portability promise.
- Existing `Report` requirements, one-based failure statuses, and ordinal report identities become
  implementation mismatches rather than language contracts.

## Drawbacks and risks

- Retaining logical traces in release artifacts costs metadata and some per-failure-path bookkeeping.
- Automatic reports intentionally omit payload details, so a bare error may be less informative
  until the programmer adds explicit recovery/reporting.
- Embedded structured termination needs target-private storage or runner support even when a module
  remains import-free.
- Conventional status `1` sacrifices the current ability to distinguish error alternatives by
  numeric return alone; that distinction was representation-derived rather than source-authored.
- Platform process-status narrowing means ordinary `i32` results are exact at the machine-entry
  contract but not necessarily exact in a parent shell.

## Alternatives and prior art

### Status quo

Require `Report` and return a one-based failure-member tag. The marker supplies no behavior, while
the status changes under unrelated union edits and still does not produce a useful portable report.

### Smaller primitive or library solution

Return only status `1` and retain no report metadata. This is smaller but contradicts the confirmed
diagnostic-context contract and makes production failures nearly impossible to locate.

### Strongest competing language model

Require every entry error to implement an explicit formatting interface and let that operation
render the payload. This supports rich messages but adds ceremony to ordinary error values, risks
formatting failure during terminal cleanup, requires services or allocation at the boundary, and
reintroduces the distinction the author explicitly rejected with `Report`.

## Falsifiers and acceptance blockers

- The stable logical trace cannot survive suspension and optimization without turning every Effect
  into an ambient allocation or service requirement.
- Import-free embedded targets cannot expose structured failure data without changing observable
  source requirements or violating target memory safety.
- Payload cleanup cannot complete before rendering the identity and trace.
- One universal failure status prevents a necessary host integration that cannot obtain structured
  termination data by another explicit boundary.
- A concrete security case shows canonical type identity or retained source provenance must be
  configurable rather than universally emitted.

## Open realization questions

- Exact text format, path normalization, source-excerpt policy, and color negotiation.
- The private structured-termination representation used by evaluator, native, and Wasm runners.
- Whether direct Wasm exposes termination metadata through a target-private export, memory record,
  or runner-owned wrapper while preserving its import-free contract.
- Trace compression and symbol metadata that preserve the stable minimum with low release cost.
- Stable compiler diagnostic codes for invalid entry shapes; these are compile-time diagnostics,
  not runtime termination codes.

## Future directions

- An explicit safe embedding ABI or Wasm component contract.
- An opt-in error-formatting interface and standard-library report combinators that do not affect
  error eligibility.
- Application helpers for conventional command-line exit statuses.
- Configurable redaction or symbol/source-path policies for hardened builds.
- Async task/fiber traces once runtime parking and scheduling are separately defined.

## OpenSpec realization map

If accepted, one OpenSpec change should reconcile entry discovery, typed-failure metadata, logical
trace preservation, cleanup order, evaluator outcomes, native/Wasm adapters, runner presentation,
ordinary and effect status behavior, trap distinction, obsolete `Report` removal, ordinal-status
removal, documentation, and differential target tests.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-19 | Drafted TERM-001–012 to close the final initial-reference gap: exact ordinary status, effect success zero, universal typed-failure status one, minimum identity-and-logical-trace reports, distinct best-effort traps, and honest standalone/embedded boundaries without Report or payload reflection. |
| 2 | 2026-08-19 | Author confirmed TERM-001 with two explicit ordinary entry results: `()` maps to zero and `i32` selects the application status; omitted ordinary return syntax remains future work rather than an entry exception. |
| 3 | 2026-08-19 | Author confirmed TERM-002–012 together and preserved a future opt-in error-formatting interface that may customize reports but can never gate typed-error eligibility. |
