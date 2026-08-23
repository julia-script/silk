# SLP-0004: Silk-native testing

SLP: 0004
Status: Candidate
Revision: 31
Author: Julia Ortiz
Created: 2026-08-22
Updated: 2026-08-23
Discussion: —
Review record: —
Review state: —
Depends on: —
Split from: —
Split into: SLP-0005, SLP-0006, SLP-0007, SLP-0008
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

This Candidate proposes a minimal Silk-native testing architecture for both user projects and the
Silk standard library. A top-level “test” marker identifies an otherwise ordinary zero-parameter
function. Test compilations expose a deterministic inventory of opaque test-function handles. An
ordinary Silk runner inspects that inventory and invokes each handle through “Test.run”.

The initial assertion surface is “Test.assert(condition: bool)” plus one pressure-driven helper,
“Test.equalBytes(actual: &[u8], expected: &[u8])”. A failed assertion emits a structured
“AssertionFailed” event to a runner-provided “Test.Reporter” service and then returns
“Test.Failure”. The event carries no values, message, or source location. When that typed failure
escapes the test, “Test.run” returns the same logical stack path already used by evaluator
termination diagnostics. The runner combines the assertion event with that path, so the standard
reporter can print a useful stack while another runner may display or store it differently.

The first version deliberately supports only the features needed by the immediate standard-library
pressure programs: scalars, byte slices through “Test.equalBytes”, other scalar slices through
explicit loops, and structs field by field. That feature limit does not restrict where tests may be
declared or run. Generic equality, diagnostic rendering, caller provenance, skipping,
source-visible build configuration, advanced selection, and compiled-target matrices are separate
future work. They are not dependencies of the initial runner.

## Problem and evidence

Silk authors need to exercise package behavior directly in Silk, whether the package is user-owned
or part of the standard library. The immediate pressure comes from the standard library: the
existing Random service needs deterministic-provider tests for both scalar words and filled byte
buffers. Today those checks must be encoded indirectly in TypeScript compiler tests or collapsed
into an executable “main”; Silk has no test declaration, discoverable test inventory, per-case
invocation, or replaceable reporting contract.

Both examples need much less than structural equality or rich diagnostics:

- Random tests can use “Random.seeded”, provide the resulting “Random.Random” service, and assert
  the scalar result of “Random.nextU64”.
- “Random.fillBytes” tests can use a non-generic byte-slice helper implemented as the same length
  check and element loop ordinary source would otherwise repeat.

The missing architectural seam is therefore not a universal assertion library. It is discovery,
controlled invocation, and reporter-independent failure signaling.

## Driving examples: current and desired

### Case: test the seeded Random service

#### Intent

Install the deterministic provider already exposed by “silk.random”, call its public operation, and
report a failed boolean claim through the runner's reporter.

#### Current Silk

~~~silk
import silk.effect as Effect
import silk.random as Random

pub fn main() -> i32 {
  let mut provider = Random.seeded(0)
  let first = run Random.nextU64()
    |> Effect.provideMut<Random.Random>(&mut provider)
  return if first == 0x99ec5f36cb75f2b4 { 0 } else { 1 }
}
~~~

The program has one aggregate status but no named case, inventory entry, replaceable report sink, or
way for an ordinary Silk runner to invoke the check independently.

#### Desired Silk

~~~silk
import silk.effect as Effect
import silk.random as Random
import silk.test as Test

test effect fn seededZeroStartsWithPublishedWord() -> ()
! Test.Failure | Test.ReportError
? &mut Test.Reporter {
  let mut provider = Random.seeded(0)
  let first = run Random.nextU64()
    |> Effect.provideMut<Random.Random>(&mut provider)
  run Test.assert(first == 0x99ec5f36cb75f2b4)
}
~~~

#### Observable result

The marked function appears once in the inventory. The standard runner installs a fresh reporter,
invokes it once, and records a pass when it returns normally. When the condition is false, the
reporter observes “AssertionFailed”, “Test.run” returns a failed outcome, and the runner records a
failure for “seededZeroStartsWithPublishedWord”.

#### Boundary case

Calling “Test.assert” inside an unmarked function does not make that function a test. Only the
“test” declaration marker changes inventory membership.

### Case: compare seeded Random bytes with a known vector

#### Intent

Test the existing “Random.fillBytes” mapping without requiring slice equality or a generic “Equal”
interface.

#### Current Silk

Without a reusable test helper, the complete check must be repeated inside an executable entry or a
host-language harness:

~~~silk
import silk.effect as Effect
import silk.random as Random
import silk.u8 as u8
import silk.usize as usize

pub fn main() -> i32 {
  let mut actual = [
    u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0),
    u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0)
  ]
  let expected = [
    u8.toU8(0xb4), u8.toU8(0xf2), u8.toU8(0x75), u8.toU8(0xcb),
    u8.toU8(0x36), u8.toU8(0x5f), u8.toU8(0xec), u8.toU8(0x99)
  ]
  let mut provider = Random.seeded(0)
  run Random.fillBytes(&mut actual)
    |> Effect.provideMut<Random.Random>(&mut provider)

  let mut index = usize.ZERO
  while index < expected.length {
    if actual[index] != expected[index] {
      return 1
    }
    index = index + usize.ONE
  }
  return 0
}
~~~

#### Desired Silk

~~~silk
import silk.test as Test
import silk.effect as Effect
import silk.random as Random
import silk.u8 as u8

test effect fn seededFillBytesMatchesFirstWord() -> ()
! Test.Failure | Test.ReportError
? &mut Test.Reporter {
  let mut actual = [
    u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0),
    u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0)
  ]
  let expected = [
    u8.toU8(0xb4), u8.toU8(0xf2), u8.toU8(0x75), u8.toU8(0xcb),
    u8.toU8(0x36), u8.toU8(0x5f), u8.toU8(0xec), u8.toU8(0x99)
  ]
  let mut provider = Random.seeded(0)
  run Random.fillBytes(&mut actual)
    |> Effect.provideMut<Random.Random>(&mut provider)
  run Test.equalBytes(&actual, &expected)
}
~~~

#### Observable result

“Random.fillBytes” consumes the documented first seeded word and writes its least-significant byte
first, producing “b4 f2 75 cb 36 5f ec 99”. “Test.equalBytes” performs the length and element checks
in ordinary standard-library source. The first false condition reports the test as failed. The
initial report does not identify the mismatching index or values; richer assertion utilities may be
added later without changing test discovery or runner ownership.

#### Boundary case

The explicit byte helper does not make arbitrary slices or structs comparable. The “test” marker
introduces no implicit equality, rendering, or reflection semantics.

### Case: run a focused subset

#### Intent

Run only byte-filling tests while iterating on Random.

#### Current Silk

There is no Silk test inventory or test command to select. A host-language harness must choose its
own files or cases, and an executable “main” must hard-code its selection.

#### Desired Silk

~~~text
silk test fillBytes
~~~

#### Observable result

The command passes “fillBytes” unchanged to the selected Silk runner. The standard runner invokes only
inventory entries whose fully qualified IDs contain that exact case-sensitive substring, preserving
their canonical relative order.

#### Boundary case

~~~text
silk test fillbytes
~~~

This matches nothing when the ID contains camel-cased “fillBytes”; the standard runner reports “0
matched” and returns status “2”. The compiler does not reinterpret either filter.

### Case: capture assertion events in a custom reporter

#### Intent

Let a custom runner count assertion failures without using the standard terminal presentation.

#### Current Silk

Silk has no test invocation boundary or assertion-report service to replace. An ordinary helper can
print or return a value, but a runner cannot install one report sink around a discovered case.

#### Desired Silk

Illustrative ordinary-source provider:

~~~silk
struct CountingReporter {
  failedAssertions: usize
}

effect fn record(self: &mut CountingReporter, event: Test.Event) -> () {
  match event {
    Test.AssertionFailed => self.failedAssertions = self.failedAssertions + 1
  }
}

impl Test.Reporter for CountingReporter {
  report: CountingReporter.record
}

let mut reporter = CountingReporter {failedAssertions: 0}
let outcome = run Test.run(function)
  |> Effect.provideMut<Test.Reporter>(&mut reporter)
~~~

#### Observable result

One failed “Test.assert” increments “failedAssertions” once. “Test.run” still returns
“Test.Failed {path}”, and the custom runner may serialize, aggregate, or suppress presentation.

#### Boundary case

Returning normally from “CountingReporter.record” does not recover the assertion. Reporting and
test failure are separate steps: after the event is accepted, “Test.assert” still returns
“Test.Failure”.

## Goals and non-goals

### Goals

- Make behavior in user packages and the standard library directly testable in Silk.
- Mark named tests without changing ordinary function semantics.
- Expose enough low-level inventory and invocation primitives for a runner written in Silk.
- Keep reporting behind an ordinary replaceable “Test.Reporter” service.
- Provide one minimal boolean assertion with a typed failure.
- Provide a non-generic byte-slice equality helper layered over that boolean assertion.
- Let the standard runner select tests with basic substring filters.
- Give the standard runner deterministic execution and an automation-safe aggregate status.
- Keep the default assertion utilities and runner policy in ordinary standard-library source while
  allowing user-defined runners and reporter providers.

### Non-goals

- Generic equality, whole-struct comparison, or automatic comparison for arbitrary slice elements.
- Expected/actual rendering, custom messages, source locations, or stack traces in assertion events.
- Skips, tags, glob/regular-expression/exclusion filters, shuffling, parallelism, retries,
  snapshots, fuzzing, coverage, or watch mode.
- Source-visible target/build configuration or target-conditioned declarations.
- Native or Wasm test execution in the initial delivery.
- A general reflection API or first-class erased function pointers.
- Replacing the host-language compiler correctness suite.

## Current language model

Silk selects a conventional public “main” as a program entry. Ordinary source cannot mark private
functions for a separate test inventory or dynamically invoke a compiler-selected set of
heterogeneous declarations. Services can already express replaceable runtime capabilities and
lexical provider replacement; that existing mechanism is sufficient for both application services
under test and the test reporter.

## Proposed language model

### Test declarations

“test” is a marker on a top-level named function declaration. It does not imply “effect”, add a
failure or requirement row, change visibility, or alter calls within the body.

An eligible declaration:

- is private to ordinary imports;
- is named, top-level, non-generic, and zero-parameter;
- returns “()”;
- may be ordinary or effectful;
- may fail with any typed failure; and
- when effectful, has no residual service requirement other than “&mut Test.Reporter”.

Application services such as Random must be provided inside the test body. This keeps every
inventory entry invocable under the runner-owned reporting scope without asking the runner to
synthesize arbitrary application providers. “pub test” is rejected because inventory membership is
not ordinary package API visibility.

### Test inventory

A test compilation exposes marked declarations from its rooted module closure, whether that closure
belongs to a user project or the standard library. Inventory order is deterministic: canonical
module identity lexically, then source order within each module. Each entry is an opaque
“Test.Function” handle. Ordinary Silk cannot construct a handle, call it directly, or convert it
into a general function value.

The minimal metadata is the canonical module identity and declaration name. This lets a runner
associate events and outcomes with stable test identities. The compiler does not filter, group, or
format the inventory.

### Runner input and basic filtering

The selected runner is an ordinary Silk function with one test-command entry parameter: a borrowed
slice of raw filter strings supplied after “silk test”. This is runner input, not a source-visible
general process-argument API:

~~~silk
pub effect fn main(filters: &[String]) -> i32
~~~

The compiler and intrinsic inventory do not interpret or apply those strings. A custom runner may
give them different semantics.

The standard runner treats each argument as a case-sensitive substring of a fully qualified test
ID, written “canonical/module::declarationName”. With no filters, it selects the complete inventory.
With multiple filters, a test is selected when any filter matches; canonical relative order is
preserved. Empty filters, glob syntax, regular expressions, exclusions, and tags receive no special
meaning.

If filters select no tests, the standard runner reports “0 matched” and returns status “2”. This
makes a misspelled CI filter fail rather than silently succeeding.

### Invocation

“Test.run(function)” invokes exactly one inventory entry inside the caller's current lexical service
environment and returns a closed outcome:

~~~silk
pub enum Outcome {
  Passed
  Failed { path: StackPath }
}
~~~

Normal return produces “Passed”. Any unhandled typed failure—including “Test.Failure”, an
application failure, or a reporter failure—produces “Failed” with its logical stack path. The path
uses the evaluator's existing ordered logical frames: canonical function identity plus source span.
The initial outcome intentionally does not expose the erased failure value. Assertion details have
already crossed the Reporter boundary when available; arbitrary application failures have no
general rendering contract.

“StackPath” is an opaque, immutable value owned by the failed outcome. Ordinary runner operations
may inspect its logical frames but cannot construct or mutate it. Display-path resolution uses the
same project source metadata and resolver as existing termination diagnostics; the testing
primitive does not establish a second filesystem-path convention.

A runtime trap remains fatal and aborts the runner. Recoverable trap isolation would require a
different execution boundary and is outside the initial model.

### Reporter and assertion

“Test.Reporter” is an ordinary mutable service:

~~~silk
pub enum Event {
  AssertionFailed
}

pub service Reporter {
  effect fn report(event: Event) -> ()
  ! ReportError
  ? &mut Reporter
}
~~~

The exact service declaration syntax is illustrative; the semantic requirement is one lexically
replaceable mutable reporter per test invocation.

“Test.assert(condition: bool)” is an ordinary standard-library operation:

~~~silk
pub effect fn assert(condition: bool) -> ()
! Failure | ReportError
? &mut Reporter
~~~

When “condition” is true, it returns without reporting. When false, it reports
“AssertionFailed” and then returns “Failure”. If reporting itself fails, “ReportError” escapes and
the case fails. No original value must survive the assertion call.

Libraries may later build scalar or slice helpers by computing a boolean and calling “Test.assert”.
Such helpers do not require compiler privilege.

The first version includes exactly one such helper for the existing Random byte-buffer pressure
program:

~~~silk
pub effect fn equalBytes(actual: &[u8], expected: &[u8]) -> ()
! Failure | ReportError
? &mut Reporter {
  run assert(actual.length == expected.length)

  let mut index = usize.ZERO
  while index < expected.length {
    run assert(actual[index] == expected[index])
    index = index + usize.ONE
  }
}
~~~

It short-circuits on a length mismatch or the first unequal byte. It emits the same
“AssertionFailed” event as “assert” and deliberately carries no mismatch index or byte values.

### Standard runner

The standard library supplies a default runner equivalent to:

~~~silk
pub effect fn main(filters: &[String]) -> i32 {
  let mut failed = 0
  let mut selected = 0

  for function in Test.functions() {
    if !StandardRunner.matchesAny(Test.id(function), filters) {
      continue
    }

    selected = selected + 1
    let reporter = StandardReporter.forTest(Test.id(function))
    let outcome = run Test.run(function)
      |> provideMut(Test.Reporter, reporter)

    match outcome {
      Test.Passed => StandardReporter.pass(Test.id(function))
      Test.Failed {path} => {
        StandardReporter.fail(Test.id(function), path)
        failed = failed + 1
      }
    }
  }

  if selected == 0 && filters.length > 0 {
    StandardReporter.noMatches(filters)
    return 2
  }
  return if failed == 0 { 0 } else { 1 }
}
~~~

This sketch fixes ownership, not final syntax. Each test receives a fresh reporter and ordinary
Effect scope within one runner process. The initial runner executes every selected inventory entry
in canonical order. It exits “0” when all selected tests pass, “1” when any selected test fails, and
“2” when explicit filters match no tests. An invalid or unavailable test compilation is a
command/tooling error rather than a runner outcome.

A simple standard report is sufficient:

~~~text
PASS std/random::seededZeroStartsWithPublishedWord
FAIL std/random::seededFillBytesMatchesFirstWord: assertion failed
  at std/random::seededFillBytesMatchesFirstWord
  at silk/test::equalBytes

1 passed; 1 failed
~~~

Custom runners may choose another presentation or aggregation policy by consuming the same
inventory, raw filters, metadata, invocation, and Reporter service contracts.

The basic command experience is:

~~~text
silk test random
silk test fillBytes seededZero
~~~

The first command selects IDs containing “random”. The second selects IDs containing either
“fillBytes” or “seededZero”.

### Initial execution boundary

The initial “silk test” command has one execution path: it passes its raw filters to the selected
Silk runner and runs that runner once through the existing evaluator for the project's ordinary
host target. It has no engine selector, target matrix, or special Wasm/native behavior. Test
declarations and inventory identities remain target-neutral so the source model need not change if
another execution mode is justified later.

## Worked language experience

A first standard-library test file can contain only the real Random pressure program:

~~~silk
import silk.effect as Effect
import silk.random as Random
import silk.test as Test

test effect fn seededZeroStartsWithPublishedWord() -> ()
! Test.Failure | Test.ReportError
? &mut Test.Reporter {
  let mut provider = Random.seeded(0)
  let first = run Random.nextU64()
    |> Effect.provideMut<Random.Random>(&mut provider)
  run Test.assert(first == 0x99ec5f36cb75f2b4)
}
~~~

Running “silk test” discovers the private declaration, executes the standard Silk runner once
through the evaluator, and prints:

~~~text
PASS std/random::seededZeroStartsWithPublishedWord

1 passed; 0 failed
~~~

Changing the expected word makes the reporter receive “AssertionFailed”; the same command prints:

~~~text
FAIL std/random::seededZeroStartsWithPublishedWord: assertion failed
  at std/random::seededZeroStartsWithPublishedWord

0 passed; 1 failed
~~~

No equality interface, value renderer, tracked-caller feature, target query, or compiled backend
participates in either execution. The failure stack reuses the evaluator's existing logical path.

## Semantic sketch

For a rooted module closure M, let tests(M) be the sequence of eligible marked declarations ordered
by canonical module identity and declaration source order.

Test compilation materializes one opaque handle h(d) for every d in tests(M). “Test.functions()”
borrows that ordered sequence. Metadata operations project only the canonical module identity and
declaration name.

“Test.run(h(d))” evaluates d() under the caller's current service environment:

- normal “()” return maps to “Passed”;
- any escaping typed failure maps to “Failed” with the existing logical path at that boundary; and
- a trap terminates execution.

“Test.assert(true)” returns “()”. “Test.assert(false)” performs
“Reporter.report(AssertionFailed)” and, if reporting succeeds, fails with “Test.Failure”.

“Test.equalBytes(actual, expected)” first asserts equal lengths, then asserts element equality in
ascending index order and returns after the first failure.

For ordered inventory F and raw filter sequence Q, the standard runner selects all of F when Q is
empty. Otherwise it selects each f in F for which at least one q in Q is a case-sensitive substring
of the fully qualified ID of f. A nonempty Q with an empty selection returns “2”; any selected
failure returns “1”; otherwise the runner returns “0”. A reporter failure is a selected test failure,
while a trap terminates before aggregation.

## Compiler–standard library boundary

### Compiler necessity

The compiler must recognize the “test” declaration marker, preserve it in semantic facts, collect
marked private declarations across the rooted module closure, and invoke a selected heterogeneous
declaration from an opaque handle. Ordinary source cannot implement those capabilities because it
cannot reflect over private declarations or safely erase their failure types.

The test command must also invoke the selected runner with raw borrowed filter strings. Silk has no
general source-visible process-argument API, so ordinary runner source cannot obtain those values
without a narrow test-entry adapter.

### Smallest target-neutral primitive

All source-callable compiler operations live in sealed “Intrinsic” and expose only:

- the ordered opaque inventory for the current test compilation;
- canonical identity metadata for one handle; and
- invocation of one eligible handle with normal completion versus erased typed failure and its
  existing logical stack path.

The primitive does not know the names “Test”, “Reporter”, “assert”, or the standard runner. It does
not compare values, render diagnostics, select tests, print output, or aggregate statuses.

Separately, the test-entry adapter passes raw strings to the selected runner's declared borrowed
slice parameter. It neither exposes those strings as ambient configuration nor interprets them.

### Standard-library construction

Ordinary “silk.test” source wraps the intrinsic inventory as “Test.Function”, defines
“Test.Outcome”, exposes metadata and “Test.run”, defines the Reporter service, boolean assertion,
and byte-slice helper, and implements the default runner and reporter.

### Privilege audit

The proposed privilege is irreducible to the marker, inventory, metadata, and erased invocation
boundary. Every policy above it remains replaceable ordinary source. No semantic-analysis,
evaluation, HIR, MIR, or backend phase recognizes a standard-library declaration by spelling.
The narrow runner parameter avoids both compiler-owned filtering and a new general process API;
substring matching, no-match status, ordering, and presentation remain ordinary runner policy.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Parsing and formatting accept and preserve “test” on named top-level function declarations. Marked declarations remain private to ordinary name resolution. |
| Types and abstraction | Affected | Eligibility requires a non-generic zero-parameter unit function. Opaque “Test.Function” and “StackPath” prevent construction or callable conversion; no equality interface is introduced. |
| Execution contracts | Affected | Ordinary and Effect tests retain their declared rows. “Test.run” reifies normal return or escaping typed failure; reporter failure fails the case and a trap remains fatal. |
| Ownership and resources | Affected | Inventory handles are borrowed and cannot outlive the inventory. A failed outcome owns its immutable stack path. Each invocation receives a fresh Reporter and ordinary Effect scope. |
| Runtime and targets | Affected | The initial command executes once through the evaluator on the ordinary host target. Native/Wasm test execution, matrices, and trap isolation have no initial contract. |
| Compiler | Affected | Grammar, semantic facts, test-compilation inventory, canonical metadata, opaque invocation, failure-path capture, and the filter-bearing runner entry adapter are required. |
| Standard library | Affected | “silk.test” owns Reporter, Event, Failure, Outcome, safe inventory wrappers, “assert”, “equalBytes”, filtering policy, standard runner, and presentation. |
| Tooling and diagnostics | Affected | “silk test” constructs the test compilation, passes raw filters, selects the configured or standard runner, resolves logical frame paths with existing source metadata, and diagnoses invalid test declarations. |
| Learning and use | Affected | Authors learn one marker, boolean assertion, explicit byte helper, stable filter IDs, reporter provision, and the distinction between recoverable typed failure and fatal traps. |

## Scope cohesion

The marker, inventory, invocation, Reporter service, boolean assertion, byte helper, and default
runner form one usable vertical slice. The byte helper is ordinary source justified by the existing
Random buffer test; it adds no compiler privilege. Generic equality, rendering, skipping,
configuration, advanced selection, and additional engines are independently useful and can be added
without changing this core. Basic substring filtering is a small policy layered over metadata the
runner already needs.

## Complexity and subtraction budget

The proposal spends compiler complexity only where ordinary Silk lacks authority: declaration
marking, inventory construction, and heterogeneous invocation. It intentionally accepts weak first
reports and manual comparisons to avoid new interface selection, reflection, static evaluation,
tracked callers, general process arguments, or cross-target runner machinery.

## Surface displacement

This replaces the need for ad hoc “main” functions and host-language wrappers for ordinary package
behavior tests, including standard-library tests. It does not replace compiler semantic/backend
tests or documentation examples.

## Drawbacks and risks

- A failed assertion event reports no values, index, message, or direct source location; the failed
  invocation still returns its logical stack path.
- A byte-slice mismatch does not report its index or values; other scalar slices still require an
  explicit loop.
- Structs must be checked field by field.
- An application failure that escapes a test has no rendered payload in the initial runner.
- A trap aborts later tests.
- The one initial host/evaluator path cannot prove backend-specific behavior.
- Keeping application requirements out of the test signature requires explicit providers in each
  test body.

These are accepted initial limits, not hidden promises. The architecture leaves richer utilities and
runners in ordinary source whenever their required language facilities exist.

## Alternatives and prior art

### Zig

Zig demonstrates the useful separation between compiler-known test discovery and a replaceable
runner. This proposal follows that ownership shape without importing Zig's full assertion or build
system surface.

### Status quo

Host-language harnesses can test Silk indirectly, but they cannot provide one Silk-native authoring
and runner contract shared by user projects and the standard library.

### Smaller primitive or library solution

A naming convention plus “main” avoids a keyword but cannot enumerate private declarations or
invoke them safely. A compiler-owned complete runner is smaller in source API but prevents the
customization goal.

### Strongest competing language model

Treat tests as an ordinary statically declared list of functions passed to a library runner. This
avoids discovery privilege, but it requires every suite to maintain registration manually and still
cannot store effect functions with heterogeneous failure rows as one ordinary callable type.

### Rejected direction: rich equality in the initial slice

Generic equality, diagnostic rendering, and preserved typed values make early reports better but
are unnecessary for Random scalar and byte-buffer checks. They remain independent Drafts, not
prerequisites.

### Rejected direction: assertion-owned output

Printing directly inside “Test.assert” would be simple, but it would couple test utilities to one
display and prevent a runner from capturing or structuring events. The Reporter service is retained
as the important architectural seam.

### Chosen direction: reuse the existing logical failure path

The assertion event does not capture its caller. Instead, when the assertion's typed failure escapes
the test, “Test.run” retains the same logical path already constructed for evaluator termination
diagnostics. This avoids tracked-caller semantics and a second path-resolution mechanism while
letting custom runners choose how much of the stack to display.

### Chosen direction: basic runner-owned substring filters

The compiler transports raw filter strings to the selected runner but never filters the inventory.
The standard runner implements case-sensitive substring matching over fully qualified IDs, ORs
multiple filters, and preserves canonical order. This provides useful focused runs without a glob
language, tags, compiler-side selection policy, or general source-visible process arguments.

### Rejected direction: skips and target configuration

Those policies do not help the initial Random tests and pull build configuration into the critical
path. Skipping needs a separate outcome extension.

## Falsifiers and acceptance blockers

- The existing seeded Random service cannot be provided inside an eligible test body.
- The documented seeded “Random.fillBytes” vector cannot be checked using ordinary
  “Test.equalBytes”.
- The opaque inventory cannot be lowered without introducing general source-callable erased
  function pointers.
- The existing evaluator logical path cannot be captured at an individual “Test.run” failure
  boundary without changing its semantics.
- Raw filter strings cannot be passed to the selected runner without introducing compiler-owned
  filtering or a general source-visible process API.
- Reporter provision cannot be fresh and lexical for each invocation.
- Erasing arbitrary typed failures at “Test.run” makes ordinary Effect cleanup unsound.
- The evaluator cannot invoke marked private declarations while preserving their normal semantics.

Any of these findings reopens the proposed boundary during Candidate review.

## Open realization questions

- The exact intrinsic names and opaque-handle representation.
- The exact source representation and accessors for immutable “StackPath” frames.
- The exact project configuration that selects a custom runner.
- The exact standard output formatting.

These are realization details only if they preserve the ownership and observable semantics above.

## Future directions

- More scalar and scalar-slice assertion helpers with better failure context.
- Generic equality and value rendering if broader package pressure justifies them.
- Known-vector tests for a future SHA implementation using the same “Test.equalBytes” helper.
- Optional assertion messages, richer frame formatting, or exact assertion-callsite provenance.
- Glob/regular-expression/exclusion filtering, skipping, tags, and source-visible target
  configuration.
- Native and Wasm execution modes with defined trap isolation.
- Parallel or process-isolated custom runners.

## OpenSpec realization map

After acceptance, the language/compiler marker and inventory boundary, standard-library Test actor,
project test command, and focused end-to-end acceptance cases may become separate DAG-ordered
OpenSpec changes. This Candidate does not create those artifacts.

## Revision and decision record

| Revision | Date | Change |
| --- | --- | --- |
| 1 | 2026-08-22 | Initial Draft seeded with an ordinary-Silk test-plan hypothesis and explicit unknowns. |
| 2 | 2026-08-22 | Replaced retained suite storage with the recommended selected-module/public-function model after callable-storage and entry-path inspection. |
| 3 | 2026-08-22 | Author chose first-class test declarations; Draft records a compiler-known “test” marker while keeping assertion APIs ordinary source. |
| 4 | 2026-08-22 | Author chose marker-only semantics: “test fn” and “test effect fn” preserve the normal function kind and explicit rows. |
| 5 | 2026-08-22 | Author chose any closed unit test: arbitrary typed failures fail, traps remain distinct, and all source requirements are provided inside the case. |
| 6 | 2026-08-22 | Recentered the direction on a Zig-like Silk-authored runner over a compiler-provided test inventory; assertion helpers emit live expected/actual diagnostics before returning a small failure marker. |
| 7 | 2026-08-22 | Author chose an ordered inventory of opaque “Test.Function” handles with metadata operations and “Test.run”, avoiding general erased callable values. |
| 8 | 2026-08-22 | Author chose a runner-supplied per-test report sink over assertion-owned stderr; the sink-injection mechanism reopens the prior zero-parameter/closed-requirement contract. |
| 9 | 2026-08-22 | Author chose an ordinary mutable “Test.Reporter” service, lexically provided around “Test.run”; it is the only permitted residual test requirement. |
| 10 | 2026-08-22 | Author chose structured reporter events with separately rendered assertion fields, allowing providers to present the same report differently without retaining typed values. |
| 11 | 2026-08-22 | Author chose general equality and debug-rendering interfaces; split them into prerequisite SLP-0005 and SLP-0006 instead of introducing a test-specific value witness. |
| 12 | 2026-08-23 | Author chose per-invocation reporter/Effect isolation in one runner process; typed failures remain per-case outcomes, while traps are fatal and abort the suite. |
| 13 | 2026-08-23 | Author chose deterministic inventory order by canonical module identity and then declaration source order; runner code may filter or reorder it. |
| 14 | 2026-08-23 | Author chose case-sensitive substring filters over fully qualified test IDs, ORed by the standard Silk runner without compiler-side filtering. |
| 15 | 2026-08-23 | Author chose standard status “0” for a nonempty all-pass selection, “1” for any typed test failure, and “2” for no matches or runner/reporting errors; traps terminate nonzero outside aggregation. |
| 16 | 2026-08-23 | Author chose a general tracked caller-location facility for assertion events; split the independent whole-language feature into prerequisite SLP-0007. |
| 17 | 2026-08-23 | Author reversed the testing dependency on tracked caller provenance: equality events retain rendered values, while “Test.run” returns the shared post-termination logical path and the runner combines both records. SLP-0007 remains only an independent Draft. |
| 18 | 2026-08-23 | Author chose the target-aware evaluator as the default execution engine for ordinary semantic tests; native and Wasm execution are explicit modes for target-specific claims rather than automatic duplicate legs. |
| 19 | 2026-08-23 | Author chose ordinary source-controlled skipping over declaration target constraints: “Test.skip” emits a structured reason and escapes through “Test.Skipped”, while target conditions use general source-visible build configuration split into SLP-0008 rather than “Test.engine”. |
| 20 | 2026-08-23 | Author chose success status “0” for a nonempty all-skipped selection, with explicit passed/skipped counts; no-match remains infrastructure status “2”. |
| 21 | 2026-08-23 | Author chose one unambiguous evaluator target per test run: CLI override, then dedicated project test target, then sole build target; unresolved multiple targets return infrastructure status “2” rather than selecting the first or running a matrix. |
| 22 | 2026-08-23 | Author chose private test metadata: “pub test” is rejected, while the test inventory sees marked declarations across the rooted module closure without making them ordinary imports or adding test-only visibility. |
| 23 | 2026-08-23 | SLP-0008 narrowed source-visible configuration from a structured constant to pure “Config.target()”: ordinary Silk constructs the descriptor from sealed “Intrinsic.targetId()” without requiring general static evaluation. |
| 24 | 2026-08-23 | Author reset the scope to the minimal usable runner: “test”, opaque inventory, “Test.run”, a replaceable Reporter service, and “Test.assert(bool)”. Equality, rendering, paths, skips, configuration, filtering, and compiled modes are deferred and no longer dependencies. |
| 25 | 2026-08-23 | Author limited the initial command to one ordinary host/evaluator execution path, with no engine selection, target matrix, or special Wasm/native behavior. |
| 26 | 2026-08-23 | Clarified that Silk-native testing applies equally to user projects and the standard library; Random and SHA define only the minimum first-version feature set. |
| 27 | 2026-08-23 | Restored the existing logical stack path on “Test.run.Failed”. Assertion events remain location-free; runners combine their captured event with post-failure provenance without tracked callers. |
| 28 | 2026-08-23 | Restored basic standard-runner filtering: raw CLI filters are passed to the selected Silk runner, which applies case-sensitive substring matching over fully qualified IDs, ORs multiple filters, preserves order, and returns “2” when nothing matches. |
| 29 | 2026-08-23 | Added non-generic “Test.equalBytes” for borrowed “u8” slices, implemented in ordinary standard-library source as a length check and element loop over “Test.assert”. |
| 30 | 2026-08-23 | Author promoted the minimal general-purpose testing direction to Candidate after adding the missing current SHA workaround, focused-filter and custom-Reporter cases, complete interaction map, and runner-input privilege clarification. |
| 31 | 2026-08-23 | Replaced unimplemented SHA-256 Candidate evidence with the existing seeded “Random.fillBytes” known vector; SHA remains only a future use of the same byte helper. |
