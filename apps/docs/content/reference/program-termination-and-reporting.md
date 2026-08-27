# Program termination and reporting

Silk distinguishes normal completion, unhandled typed failure, and fatal trap. These rules describe
the generated host boundary after a valid `main` has been selected. They do not add a source-visible
exception system, ambient console, or formatting requirement to error values.

TERM-001–012 are confirmed stabilization rules.

## TERM-001 — An ordinary entry explicitly returns unit or one status value

**Status:** Confirmed

`pub fn main() -> ()` completes normally with machine status `0`. `pub fn main() -> i32` completes
normally with that exact `i32`. Neither form automatically reports an error, even when the integer
is nonzero.

```silk
pub fn main() -> () {
}
```

```silk
pub fn main() -> i32 {
  return 7
}
```

**Boundary:** The result annotation is required. `pub fn main() {}` remains invalid until Silk
adopts a general return-omission rule. A native host may expose only a target-defined subset of the
`i32`; a direct Wasm caller observes the exact machine-entry result.

**Diagnostics:** Any other ordinary result reports an invalid entry shape naming `()` and `i32` as
the permitted results. A missing annotation receives the ordinary missing-result diagnostic.

**Evidence:** [ordinary entry rule](program-entry.md#entry-005--an-ordinary-entry-explicitly-returns--or-i32).

## TERM-002 — A successful effect entry returns zero

**Status:** Confirmed

`pub effect fn main()` is constructed and executed exactly once. When it succeeds with `()`, the
generated machine entry returns status `0` and prints nothing automatically.

```silk
pub effect fn main() {
}
```

**Boundary:** Effect-entry success must be unit. Returning an integer success value does not turn it
into an ordinary entry or choose a process status.

**Diagnostics:** A non-unit effect-entry success type reports the invalid entry shape before
backend emission.

**Evidence:** [effect entry execution](program-entry.md#entry-002--the-compiler-executes-an-effect-entry),
[effect entry failure boundary](program-entry.md#entry-003--unhandled-effect-entry-failures-become-process-failures).

## TERM-003 — Every unhandled typed failure uses status one

**Status:** Confirmed

Any valid concrete error reaching the effect-entry boundary produces one structured unhandled-error
outcome and machine status `1`. The actual error identity belongs to the structured outcome and
report, not to a numeric status ordinal.

```silk
pub struct NotFoundError {}
pub struct OfflineError {}

pub effect fn main() ! NotFoundError | OfflineError {
  fail OfflineError {}
}
```

This terminates with status `1` whether the active error is `NotFoundError` or `OfflineError`.

**Boundary:** Adding, removing, or reordering structural-union members cannot change the public
status. Application-specific statuses require handling the error explicitly before entry.

**Diagnostics:** A valid unhandled entry error receives no compile-time diagnostic. The runtime
outcome is an unhandled error, not a source diagnostic.

**Implementation:** Internal entry tags still select the active failure identity, but standalone
adapters normalize every recognized failure tag to public status `1`; evaluator outcomes expose
that same public status independently of member order.

**Evidence:** [entry failure rule](program-entry.md#entry-003--unhandled-effect-entry-failures-become-process-failures),
[current backend contract](../../../../openspec/specs/bootstrap-backend/spec.md).

## TERM-004 — A failure report has one stable minimum

**Status:** Confirmed

Every unhandled typed-error report contains, in order:

1. an explicit `unhandled error` classification;
2. the canonical fully qualified type identity of the active error;
3. the source origin where the failure was created, when valid provenance exists; and
4. the logical named function and Effect path from that origin outward to `main`.

```text
unhandled error: app.NotFoundError
  at app.load (src/app.silk:12)
  at app.main (src/app.silk:27)
```

The exact wording is illustrative; the information and order are the contract.

**Boundary:** Colors, source excerpts, absolute path spelling, target information, and additional
debug frames are optional decoration and cannot replace the stable minimum.

**Diagnostics:** No compile-time diagnostic applies. Missing provenance caused by a compiler defect
must not be disguised as an empty successful report.

**Evidence:** [failure diagnostic context](typed-failures.md#fail-006--typed-failure-applies-ordinary-cleanup-and-preserves-diagnostic-context).

## TERM-005 — Automatic reporting does not inspect error payload fields

**Status:** Confirmed

The entry adapter reports canonical error identity and retained diagnostic context. It does not
reflect over, serialize, or generically format the payload's fields.

```silk
pub struct NotFoundError {
  resourceId: i32
}
```

An automatic report identifies `NotFoundError`; it does not automatically print `resourceId`.

**Boundary:** The adapter cannot expose private fields accidentally, require a display operation,
or fail because one payload field has no formatter. Source recovery code may inspect and report the
payload explicitly before it reaches entry. A future opt-in error-formatting interface may customize
the report, but implementing it must never be required for a value to participate in the typed-error
channel or reach the automatic entry boundary.

**Diagnostics:** No marker interface or formatting diagnostic applies to a valid error type.

**Evidence:** [ordinary error values](typed-failures.md#fail-001--any-concrete-detached-value-may-be-a-typed-failure),
[removal of Report ceremony](program-entry.md#entry-003--unhandled-effect-entry-failures-become-process-failures).

## TERM-006 — Recovery history becomes causal report context

**Status:** Confirmed

If a recovery handler fails while handling an earlier error, the new error is primary. Its report
retains the earlier identity and logical trace under an explicit `while handling` cause. Ordinary
propagation does not duplicate the same cause, and a successful handler removes the handled context
from the final outcome.

```text
unhandled error: app.OfflineError
  at app.recover (...)
while handling: app.NotFoundError
  at app.load (...)
```

**Boundary:** Causal context remains hidden runtime metadata. It does not change `E`, enter pattern
matching, or wrap the handler's ordinary error parameter.

**Diagnostics:** No compile-time diagnostic applies. Causal presentation must not reorder the
primary and handled errors.

**Evidence:** [failure recovery context](typed-failures.md#fail-006--typed-failure-applies-ordinary-cleanup-and-preserves-diagnostic-context).

## TERM-007 — Logical traces survive optimization and explicit suspension

**Status:** Confirmed

The stable trace contains source-declared function and Effect boundaries. It excludes compiler
helpers, physical machine frames, and private coroutine transitions. Optimization cannot erase a
logical frame required by the report, and `Effect.suspend` preserves the suspended invocation's
place in the logical path.

**Boundary:** Debug builds may add internal detail. Release builds still retain the stable minimum
for fallible entry paths. This rule does not promise future async task or scheduler traces.

**Diagnostics:** No compile-time diagnostic applies. A runner must not present a physical helper
frame as if the programmer declared it.

**Evidence:** [Effect suspension rules](effect-suspension.md),
[failure diagnostic context](typed-failures.md#fail-006--typed-failure-applies-ordinary-cleanup-and-preserves-diagnostic-context).

## TERM-008 — Fatal traps use a distinct best-effort report

**Status:** Confirmed

A trap terminates abnormally outside the typed-error outcome. When runtime state remains
trustworthy, the host report contains an explicit `fatal trap` classification, the reason, source
origin, and available logical path.

```text
fatal trap: division by zero
  at app.calculate (src/app.silk:8)
  at app.main (src/app.silk:14)
```

**Boundary:** Corrupted memory or a violated unsafe contract may prevent a complete report. Silk
guarantees neither a portable numeric trap status nor structured cleanup after the trap.

**Diagnostics:** A compile-time trap in required constant evaluation is a source diagnostic. A
runtime trap is abnormal termination and must not be reported as an unhandled typed error.

**Evidence:** [fatal trap rule](typed-failures.md#fail-007--a-trap-is-fatal-and-remains-outside-effect-outcomes),
[trap cleanup boundary](ownership-and-borrowing.md#trap-001--a-trap-has-no-cleanup-guarantee).

## TERM-009 — Automatic entry reporting is not an ambient service

**Status:** Confirmed

Terminal reporting belongs to the compiler-generated host adapter and matched toolchain support. It
adds no Logger, Console, filesystem, environment, allocator, or other source requirement and makes
none of those facilities ambient inside the program.

**Boundary:** Failure while writing a best-effort report cannot become a new typed error or replace
the original termination. Ordinary program logging still requires its explicit service.

**Diagnostics:** No requirement-row entry is added for automatic reporting. A source log operation
with an unresolved service remains an ordinary open-requirement error.

**Evidence:** [generated adapter boundary](runtime-and-standard-library.md#runtime-005--the-compiler-generated-adapter-is-the-only-mandatory-program-runtime-boundary),
[no ambient facilities](runtime-and-standard-library.md#runtime-004--silk-has-no-ambient-runtime-facilities).

## TERM-010 — Standalone and embedded hosts expose equivalent termination data

**Status:** Confirmed

A standalone executable writes its automatic report to the host diagnostic stream, conventionally
standard error. A target without such a stream, including import-free direct Wasm, receives
structured termination data through its runner or embedding boundary and chooses how to present it.

**Boundary:** Silk does not invent a console or hidden import for embedded targets. Exact embedding
ABI is target policy; semantic parity requires the same classification, identity or reason, origin,
and logical path rather than identical output bytes.

**Diagnostics:** A target incapable of satisfying its declared runner contract is a toolchain or
target-compatibility failure, not a source error in `main`.

**Evidence:** [runtime layers](runtime-and-standard-library.md#runtime-001--language-public-source-target-providers-and-toolchain-runtime-support-are-distinct-layers),
[direct Wasm entry contract](../../../../openspec/specs/bootstrap-backend/spec.md).

## TERM-011 — Typed-failure reporting follows completed cleanup

**Status:** Confirmed

On unhandled typed failure, every exited owner except the failure payload is cleaned during
propagation. The entry boundary then cleans the payload exactly once while retaining its hidden
diagnostic context for reporting.

**Boundary:** Reporting does not keep arbitrary user payload storage alive. Fatal traps retain their
separate no-cleanup guarantee.

**Diagnostics:** Invalid cleanup ownership receives its ordinary compile-time diagnostic. A valid
terminal failure adds no cleanup diagnostic.

**Evidence:** [typed-failure cleanup](typed-failures.md#fail-006--typed-failure-applies-ordinary-cleanup-and-preserves-diagnostic-context),
[cleanup follows ownership](ownership-and-borrowing.md#cleanup-001--cleanup-follows-ownership).

## TERM-012 — Custom presentation and statuses are explicit source policy

**Status:** Confirmed

A program wanting a custom message, payload rendering, telemetry event, or application-specific
status handles the typed error before it reaches the automatic entry boundary. It closes every
Effect error and requirement and returns an ordinary `i32` entry result when it wants to select a
status.

**Boundary:** No error naming convention, marker interface, or automatically discovered operation
changes entry behavior. Future standard-library helpers remain ordinary source APIs.

**Diagnostics:** An ordinary entry that attempts to `run` an Effect with unhandled errors or
requirements receives the existing boundary diagnostic before backend emission.

**Evidence:** [ordinary execution boundary](effects-and-execution.md),
[explicit entry requirements](program-entry.md#entry-004--effect-entry-requirements-must-be-resolved),
[style guide](style-guide.md).

## Future direction: custom error formatting

Silk may later define an ordinary opt-in interface for customizing how an unhandled error payload
is presented. Such an interface must remain separate from error eligibility: every valid detached
owned value can still be an error without implementing it. The future design must also define how
formatting obtains allocation or output capabilities, what happens if formatting cannot complete,
and how embedded targets request structured rather than textual output.
