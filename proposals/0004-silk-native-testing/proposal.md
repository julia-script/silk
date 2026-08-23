# SLP-0004: Silk-native testing

SLP: 0004
Status: Accepted direction
Revision: 36
Author: Julia Ortiz
Created: 2026-08-22
Updated: 2026-08-23
Discussion: —
Review record: [r001](reviews/r001.md), [r002](reviews/r002.md), [r003](reviews/r003.md)
Review state: Cap — bounded review ended at r003; author resolution completed in revision 35; targeted [audit a001](audits/resolution-a001.md) passed; revision 36 preserves the accepted direction while resolving the handoff naming conflict
Depends on: —
Split from: —
Split into: SLP-0005, SLP-0006, SLP-0007, SLP-0008
Supersedes: —
Superseded by: —
Revisit when: Owned logical StackPath capture cannot be realized target-neutrally without hidden unbounded allocation, unsound cleanup, or a change to the accepted source semantics.
Resolution: Author accepted the minimal Silk-native testing direction after closing the two r003 blockers in revision 34 and its targeted audit. Revision 36 clarifies the shipped assertion error name as `Test.AssertionError`, preserving the accepted behavior: assertion helpers emit that specific typed error, while `Outcome.Failed` remains the broader result for any unhandled typed failure. The selected model retains closed marked tests, explicit inventory roots plus a separate runner root, opaque per-test invocation, runner-facing structured Reporter events with public fields, silent boolean and byte-slice assertions, ASCII case-insensitive byte filters, deterministic evaluator execution, and statuses 0/1/2. Eligibility diagnostics and presentation filtering are delegated to OpenSpec; owned StackPath representation remains an explicit realization evidence gate.
OpenSpec handoff: [establish-silk-test-inventory](../../openspec/changes/establish-silk-test-inventory/proposal.md), [add-silk-test-standard-library](../../openspec/changes/add-silk-test-standard-library/proposal.md), [add-silk-test-command](../../openspec/changes/add-silk-test-command/proposal.md), [prove-silk-native-testing-sufficiency](../../openspec/changes/prove-silk-native-testing-sufficiency/proposal.md)

## Summary

This accepted direction proposes a minimal Silk-native testing architecture for both user projects
and the Silk standard library. A top-level “test” marker identifies a zero-parameter Effect
function. Test compilations expose a deterministic inventory of opaque test-function handles. An
ordinary Silk runner inspects that inventory and invokes each handle through “Test.run”.

The initial assertion surface is “Test.assert(condition: bool)” plus one pressure-driven helper,
“Test.equalBytes(actual: &[u8], expected: &[u8])”. A failed assertion returns
“Test.AssertionError”. When that or any other typed failure escapes the test, “Test.run” returns the
same logical stack path already used by evaluator termination diagnostics. Ordinary runner source
turns the closed outcome into a structured case event and sends it to a replaceable
“Test.Reporter” service, so the standard
reporter can print a useful stack while another runner may display or store it differently. Tests
themselves have no Reporter requirement and the compiler never selects a library service identity.

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
! Test.AssertionError {
  let mut provider = Random.seeded(0)
  let first = run Random.nextU64()
    |> Effect.provideMut<Random.Random>(&mut provider)
  run Test.assert(first == 0x99ec5f36cb75f2b4)
}
~~~

#### Observable result

The marked function appears once in the inventory. The standard runner invokes it once and records
a pass when it returns normally. When the condition is false, “Test.run” returns a failed outcome;
the runner sends one failed-case event with its logical path to the fresh standard reporter.

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
! Test.AssertionError {
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

The command passes “fillBytes” unchanged to the selected Silk runner. The standard runner invokes
only inventory entries whose fully qualified IDs contain that ASCII case-insensitive byte
substring, preserving their canonical relative order.

#### Boundary case

~~~text
silk test fill-bytes
~~~

The punctuation has no special meaning and that byte sequence occurs nowhere in the ID, so the
standard runner reports “0 matched” and returns status “2”. The compiler does not reinterpret
either filter.

### Case: capture case outcomes in a custom reporter

#### Intent

Let a custom runner count failed cases without using the standard terminal presentation.

#### Current Silk

Silk has no test invocation boundary or case-report service to replace. An ordinary helper can
print or return a value, but a runner cannot install one report sink around a discovered case.

#### Desired Silk

Complete single-file custom runner and test root:

~~~silk
import silk.effect as Effect
import silk.test as Test
import silk.usize as usize

struct CountingReporter {
  failedCases: usize
}

effect fn record(self: &mut CountingReporter, event: Test.Event) -> () {
  match move event {
    Test.Event { value } => match move value {
      Test.PassedCase { function } => ()
      Test.FailedCase { function, path } => {
        self.failedCases = self.failedCases + usize.ONE
        drop path
      }
    }
  }
}

impl Test.Reporter for CountingReporter {
  report: CountingReporter.record
}

test effect fn alwaysFails() -> () ! Test.AssertionError {
  run Test.assert(false)
}

pub effect fn main() ! Test.AssertionError | Test.ReportError {
  let functions = Test.functions()
  run Test.assert(functions.length == usize.ONE)

  let function = functions[usize.ZERO]
  let outcome = run Test.run(function)
  let event = Test.event(function, move outcome)
  let mut reporter = CountingReporter {failedCases: usize.ZERO}
  let reported = run Test.report(move event)
    |> Effect.provideMut<Test.Reporter>(&mut reporter)

  run Test.assert(reporter.failedCases == usize.ONE)
}
~~~

#### Observable result

The file is both the selected custom-runner root and one configured test root, so its private
“alwaysFails” declaration is the sole inventory entry. That failure increments “failedCases” once,
and the final assertion returns normally. “Test.run” itself remains closed; the custom runner
chooses whether and where to report its Event. Reusing this reporter across several events is
permitted and accumulates the count; fresh state is a standard-runner choice.

#### Boundary case

Omitting “Test.report” does not change the invocation outcome. Reporting is ordinary runner policy,
not a side effect of assertions or opaque invocation; a reporter failure is runner infrastructure
failure rather than a different test outcome.

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
- Expected/actual rendering, custom messages, or exact assertion-callsite events.
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

“test” is a marker on a top-level named Effect function declaration. It does not add a failure or
requirement row, change visibility, or alter calls within the body.

An eligible declaration:

- is private to ordinary imports;
- is named, top-level, non-generic, and zero-parameter;
- returns “()”;
- is effectful;
- may fail with any typed failure; and
- has an empty residual service requirement row.

Application services such as Random must be provided inside the test body. This keeps every
inventory entry invocable without asking the runner to synthesize arbitrary application providers.
Reporter is runner-facing and therefore does not participate in test eligibility. “pub test” is
rejected because inventory membership is not ordinary package API visibility. A marked function
with an ordinary signature, parameters, a non-unit result, public visibility, generics, or any
residual requirement is diagnosed and does not produce a partial inventory.

### Test roots and inventory

A test compilation starts from an explicit ordered root set and takes the union of those roots'
transitive module closures. A user manifest may declare an optional “[test]” table with a nonempty
“roots” array of manifest-relative Silk source paths. Each resolved path must be contained by the
package's existing “source-root”; its canonical module identity is derived relative to that same
root. When the table is absent, the package's ordinary “package.root” is the sole test root. The
standard-library test target supplies canonical roots through the toolchain's deterministic
standard-library test catalog. A source file that is neither a root nor transitively imported by
one is not silently scanned for tests.

The compiler de-duplicates modules reached through several roots. It exposes every eligible marked
declaration in the resulting closure, whether the roots belong to a user project or the standard
library. Inventory order does not depend on root order: canonical module identity lexically, then
source order within each module. Each entry is an opaque “Test.Function” handle. Ordinary Silk
cannot construct a handle, call it directly, or convert it into a general function value.
“Test.functions()” returns a borrowed slice in that order; Function is an immutable Copy token, so
ordinary length, indexing, and while loops can inspect and pass a handle without ownership tricks.

The selected runner module is a distinct executable root. Entry discovery resolves its ordinary
zero-parameter “main”; its transitive closure is composed with the test-root closure for analysis
and lowering, but declarations found only through the runner closure do not enter the test
inventory. Shared modules are de-duplicated by canonical identity. The standard runner is the
default executable root; the exact project syntax selecting a custom runner remains a realization
question.

The minimal metadata is the canonical module identity and declaration name. This lets a runner
associate events and outcomes with stable test identities. The compiler does not filter, group, or
format the inventory.

### Runner input and basic filtering

The selected runner keeps Silk's ordinary zero-parameter entry shape. The test command seeds the
existing low-level host-input adapter with its argument zero followed by the filter arguments. The
ordinary standard runner constructs “OsHostInput” and an Allocator, provides them lexically to
“HostInput” operations, and reads indices one and later. The bytes and their order are unchanged.
No test-only parameter-bearing entry adapter or ambient service provision is introduced, and the
compiler and intrinsic inventory do not interpret or apply the arguments. A custom runner may
construct the same provider or ignore the arguments and give selection another meaning.

The standard runner compares each argument with the UTF-8 encoding of a fully qualified test ID,
written “canonical/module::declarationName”, using ASCII case-insensitive byte-substring matching.
For comparison only, bytes `A` through `Z` fold to their lowercase ASCII byte; every other byte is
exact. This makes `fillBytes`, `FillBytes`, and `fillbytes` equivalent without introducing Unicode
normalization or full case folding. Filters need not be valid UTF-8: invalid and non-ASCII bytes
remain exact and normally do not occur in a valid encoded ID. With no filters, the runner selects
the complete inventory. With multiple filters, a test is selected when any filter matches;
canonical relative order is preserved. Empty filters, glob syntax, regular expressions,
exclusions, and tags receive no special meaning.

If filters select no tests, the standard runner reports “0 matched” and returns status “2”. This
makes a misspelled CI filter fail rather than silently succeeding.

### Invocation

“Test.run(function)” invokes exactly one closed inventory entry and returns a closed outcome. The
ordinary-source representation uses the structural unions Silk already supports:

~~~silk
pub struct Passed {}
pub struct Failed { path: StackPath }
pub struct Outcome { value: Passed | Failed }
~~~

Normal return produces “Passed”. Any unhandled typed failure—including “Test.AssertionError” or an
application failure—produces “Failed” with its logical stack path. The path
uses the evaluator's existing ordered logical frames: canonical function identity plus source span.
The initial outcome intentionally does not expose the erased failure value, so arbitrary
application failures have no general rendering contract.

“StackPath” is an opaque, immutable value owned by the failed outcome. Ordinary runner operations
may inspect its logical frames but cannot construct or mutate it. Display-path resolution uses the
same project source metadata and resolver as existing termination diagnostics; the testing
primitive does not establish a second filesystem-path convention.

A runtime trap remains fatal and aborts the runner. Recoverable trap isolation would require a
different execution boundary and is outside the initial model.

### Reporter and assertion

“Test.Reporter” is an ordinary mutable runner service. Its initial event representation uses
existing structs and structural unions and consumes one completed case outcome:

~~~silk
pub struct PassedCase { pub function: Function }
pub struct FailedCase { pub function: Function, pub path: StackPath }
pub struct Event { pub value: PassedCase | FailedCase }

pub service Reporter {
  effect fn report(event: Event) -> ()
  ! ReportError
  ? &mut Reporter
}
~~~

The ordinary “Test.event(function, outcome)” operation consumes an outcome and constructs the
corresponding Event; “Test.report(event)” delegates it to Reporter. A custom runner may deliberately
share one Reporter across cases, replace it, or omit reporting. Fresh per-case reporter state is
default-runner policy. Reporter failure is runner infrastructure failure and cannot change the test
outcome that was already produced.

“Test.assert(condition: bool)” is an ordinary standard-library operation:

~~~silk
pub effect fn assert(condition: bool) -> ()
! AssertionError
~~~

When “condition” is true, it returns normally. When false, it returns “AssertionError”. No original value
must survive the assertion call. A test may intentionally recover that ordinary typed failure; if
it then returns normally, Test.run produces Passed because no assertion-side report has escaped the
test's control flow.

Libraries may later build scalar or slice helpers by computing a boolean and calling “Test.assert”.
Such helpers do not require compiler privilege.

The first version includes exactly one such helper for the existing Random byte-buffer pressure
program:

~~~silk
pub effect fn equalBytes(actual: &[u8], expected: &[u8]) -> ()
! AssertionError {
  run assert(actual.length == expected.length)

  let mut index = usize.ZERO
  while index < expected.length {
    run assert(actual[index] == expected[index])
    index = index + usize.ONE
  }
}
~~~

It short-circuits on a length mismatch or the first unequal byte and deliberately retains no
mismatch index or byte values.

### Standard runner

The standard library supplies a default runner with a closed ordinary entry. “runFromOsHost” is an
ordinary-source edge helper: it constructs OsHostInput and the standard Allocator, obtains owned
filter bytes under lexical provision, constructs the output provider, catches HostInput,
allocation, and ReportError failures as status “2”, and runs this core:

~~~silk
effect fn runSuite(filters: &[Bytes]) -> i32 ! Test.ReportError {
  let mut failed = 0
  let mut selected = 0
  let functions = Test.functions()
  let mut index = usize.ZERO

  while index < functions.length {
    let function = functions[index]
    if StandardRunner.matchesAny(Test.id(function), filters) {
      selected = selected + 1
      let outcome = run Test.run(function)
      let caseFailed = Test.isFailed(&outcome)
      let event = Test.event(function, move outcome)
      let mut reporter = StandardReporter.forTest(Test.id(function))
      let reported = run Test.report(move event)
        |> Effect.provideMut<Test.Reporter>(&mut reporter)
      if caseFailed { failed = failed + 1 }
    }
    index = index + usize.ONE
  }

  if selected == 0 && filters.length > 0 {
    StandardReporter.noMatches(filters)
    return 2
  }
  return if failed == 0 { 0 } else { 1 }
}

pub fn main() -> i32 {
  return StandardRunner.runFromOsHost(runSuite)
}
~~~

The sketch fixes the entry, ownership, and sequence-access model while leaving helper names open.
Each standard-runner case receives fresh reporter state within one runner process. The initial
runner executes every selected inventory entry in canonical order. It exits “0” when all selected
tests pass, “1” when any selected test fails, and “2” when explicit filters match no tests or runner
input/output/reporting infrastructure fails. An invalid or unavailable test compilation is a
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

The initial “silk test” command has one execution path: it seeds the evaluator's existing low-level
host-input adapter with the runner argument followed by the raw filters, then runs the selected
zero-parameter Silk runner once through the evaluator for the project's ordinary host target. The
ordinary runner constructs and lexically provides the source-level OsHostInput and Allocator. It has
no engine selector, target matrix, or special Wasm/native behavior. Test
declarations and inventory identities remain target-neutral so the source model need not change if
another execution mode is justified later.

## Worked language experience

A first standard-library test file can contain only the real Random pressure program. The
toolchain's standard-library test catalog lists that file as a test root; a user package uses the
same model with this “silk.toml” entry:

~~~toml
[package]
name = "random-tests"
version = "0.1.0"
root = "src/main.silk"
source-root = "src"

[test]
roots = ["src/tests/random.silk"]
~~~

~~~silk
import silk.effect as Effect
import silk.random as Random
import silk.test as Test

test effect fn seededZeroStartsWithPublishedWord() -> ()
! Test.AssertionError {
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

Changing the expected word makes Test.run return Failed; the runner turns it into one FailedCase
event and the standard reporter prints:

~~~text
FAIL std/random::seededZeroStartsWithPublishedWord: assertion failed
  at std/random::seededZeroStartsWithPublishedWord

0 passed; 1 failed
~~~

No equality interface, value renderer, tracked-caller feature, target query, or compiled backend
participates in either execution. The failure stack reuses the evaluator's existing logical path.

## Semantic sketch

For source-root-contained test roots R, let M(R) be the de-duplicated union of their transitive
module closures, and let tests(M(R)) be the sequence of eligible closed marked declarations ordered
by canonical module identity and declaration source order.

Test compilation materializes one opaque Copy handle h(d) for every d in tests(M(R)).
“Test.functions()” borrows that ordered sequence. Metadata operations project only the canonical
module identity and declaration name. The separate runner root supplies the sole executable entry
and is excluded from inventory membership unless it is also named by R.

“Test.run(h(d))” evaluates d():

- normal “()” return maps to “Passed”;
- any escaping typed failure maps to “Failed” with the existing logical path at that boundary; and
- a trap terminates execution.

“Test.assert(true)” returns “()”. “Test.assert(false)” fails with “Test.AssertionError”.

“Test.equalBytes(actual, expected)” first asserts equal lengths, then asserts element equality in
ascending index order and returns after the first failure.

After each invocation, ordinary runner source consumes its Outcome into PassedCase or FailedCase and
reports that Event. For ordered inventory F and raw filter-byte sequence Q, the standard runner
selects all of F when Q is empty. Otherwise it selects each f in F for which at least one q is an
ASCII-folded byte substring of the UTF-8 fully qualified ID of f. A nonempty Q with an empty selection or
input/allocation/reporting infrastructure failure returns “2”; any selected failed outcome returns
“1”; otherwise the runner returns “0”. A trap terminates before aggregation.

## Compiler–standard library boundary

### Compiler necessity

The compiler must recognize the “test” declaration marker, preserve it in semantic facts, validate
that marked declarations are closed, and invoke a selected heterogeneous declaration from an opaque
handle. Ordinary source cannot implement those capabilities because it
cannot reflect over private declarations or safely erase their failure types.

The test command invokes the selected runner through the existing zero-parameter program-entry
contract. Existing multi-root project closure machinery composes the selected runner root and
tooling-supplied test roots. The command seeds raw filter bytes into the existing low-level host
adapter; ordinary runner source constructs providers. Neither operation needs a new source-callable
compiler primitive.

### Smallest target-neutral primitive

All source-callable compiler operations live in sealed “Intrinsic” and expose only:

- the ordered opaque inventory for the current test compilation;
- canonical identity metadata for one handle; and
- invocation of one eligible handle with normal completion versus erased typed failure and its
  existing logical stack path.

The primitive does not know the names “Test”, “Reporter”, “assert”, or the standard runner. It does
not compare values, render diagnostics, select tests, print output, or aggregate statuses.

Implementations may represent each handle as a test-compilation ordinal dispatched through
compiler-generated uniform per-test adapters. Those adapters erase only the already-validated
failure row into normal-versus-failed outcome; they do not create general source-callable function
pointers. Adapter rooting and code-size cost belong to the test artifact and are absent from normal
program builds.

### Standard-library construction

Ordinary “silk.test” source wraps the intrinsic inventory as “Test.Function”, defines
“Test.Outcome”, exposes metadata and “Test.run”, defines runner-facing case Events and the Reporter
service, defines the boolean assertion and byte-slice helper, and implements the default runner and
reporter.

### Privilege audit

The proposed privilege is irreducible to the marker, closed test inventory, metadata, uniform
per-test adapters, and erased invocation boundary. Every policy above it remains replaceable
ordinary source. No semantic-analysis, evaluation, HIR, MIR, or backend phase recognizes a
standard-library declaration by spelling or selects a Reporter service identity. Existing project
closure and host-input machinery carry roots and filters; matching, no-match status, ordering,
event construction, reporting, and presentation remain ordinary runner policy.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Parsing and formatting accept and preserve “test” on named top-level function declarations. Marked declarations remain private to ordinary name resolution. |
| Types and abstraction | Affected | Eligibility requires a non-generic zero-parameter closed Effect function returning unit. Opaque “Test.Function” and “StackPath” prevent construction or callable conversion; no equality interface is introduced. |
| Execution contracts | Affected | Effect tests retain their declared failure row. “Test.run” reifies normal return or escaping typed failure; runner reporting occurs after the outcome and a trap remains fatal. |
| Ownership and resources | Affected | Inventory handles are Copy tokens borrowed from a process-lifetime inventory. A failed outcome owns its immutable stack path; consuming it into a case Event transfers that path to the Reporter. The standard runner creates fresh reporter state per case. |
| Runtime and targets | Affected | The initial command executes once through the evaluator on the ordinary host target. Native/Wasm test execution, matrices, and trap isolation have no initial contract. |
| Compiler | Affected | Grammar, semantic facts, closed-test validation, canonical metadata, uniform opaque invocation adapters, and failure-path capture are required. Existing multi-root closure machinery receives test roots from tooling. |
| Standard library | Affected | “silk.test” owns Reporter, Event, AssertionError, Outcome, safe inventory wrappers, “assert”, “equalBytes”, filtering policy, standard runner, and presentation. |
| Tooling and diagnostics | Affected | “silk test” composes a distinct runner root with source-root-contained test roots, seeds raw host-input bytes, resolves logical frame paths with existing source metadata, and diagnoses invalid test declarations. |
| Learning and use | Affected | Authors learn one marker, boolean assertion, explicit byte helper, stable filter IDs, reporter provision, and the distinction between recoverable typed failure and fatal traps. |

## Scope cohesion

The marker, inventory, invocation, runner-facing Reporter service, boolean assertion, byte helper,
and default runner form one usable vertical slice. Reporter has no effect on eligibility or
invocation and remains ordinary replaceable presentation policy. The byte helper is ordinary source justified by the existing
Random buffer test; it adds no compiler privilege. Generic equality, rendering, skipping,
configuration, advanced selection, and additional engines are independently useful and can be added
without changing this core. Basic substring filtering is a small policy layered over metadata the
runner already needs.

## Complexity and subtraction budget

The proposal spends compiler complexity only where ordinary Silk lacks authority: declaration
marking, closed inventory construction, uniform adapters, and heterogeneous invocation. Existing
project closure and HostInput mechanisms carry roots and filters. It intentionally accepts weak first
reports and manual comparisons to avoid new interface selection, reflection, static evaluation,
tracked callers, test-specific entry arguments, or cross-target runner machinery.

## Surface displacement

This replaces the need for ad hoc “main” functions and host-language wrappers for ordinary package
behavior tests, including standard-library tests. It does not replace compiler semantic/backend
tests or documentation examples.

## Drawbacks and risks

- A failed case event reports no erased failure value, mismatch index, message, or exact assertion
  callsite; it owns the failed invocation's logical stack path.
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
display. Making Reporter a test requirement would instead force an ordinary library service
identity into compiler eligibility and duplicate the already-closed Outcome. The chosen seam keeps
assertions silent and lets ordinary runner source consume each Outcome into a structured Event for
its replaceable Reporter.

### Chosen direction: reuse the existing logical failure path

When an assertion's typed failure escapes the test, “Test.run” retains the same logical path already
constructed for evaluator termination diagnostics. Ordinary runner source moves that path into its
FailedCase event. This avoids tracked-caller semantics and a second path-resolution mechanism while
letting custom reporters choose how much of the stack to display.

### Chosen direction: basic runner-owned substring filters

The test command seeds the existing low-level host-input adapter with raw filter arguments, and the
compiler never filters the inventory. Ordinary runner source constructs and provides OsHostInput;
the standard runner implements ASCII case-insensitive byte-substring matching over encoded fully
qualified IDs, ORs multiple filters, and preserves canonical order. This
provides useful focused runs without a glob language, tags, compiler-side selection policy, or a
test-specific entry adapter.

### Rejected direction: skips and target configuration

Those policies do not help the initial Random tests and pull build configuration into the critical
path. Skipping needs a separate outcome extension.

## Falsifiers and acceptance blockers

- The existing seeded Random service cannot be provided inside an eligible test body.
- The documented seeded “Random.fillBytes” vector cannot be checked using ordinary
  “Test.equalBytes”.
- The opaque inventory cannot be lowered without introducing general source-callable erased
  function pointers.
- An explicit test-root set cannot include user and standard-library test modules without silently
  scanning unrelated files or weakening ordinary module-closure semantics.
- A distinct runner executable root cannot compose with test inventory roots while preserving
  canonical module identity and excluding runner-only declarations from the inventory.
- The existing evaluator logical path cannot be captured at an individual “Test.run” failure
  boundary without changing its semantics.
- Existing low-level host-input data cannot be consumed through explicitly constructed ordinary
  OsHostInput and Allocator providers at the zero-parameter runner edge.
- Runner-owned outcome reporting cannot remain fully ordinary source with replaceable providers.
- Erasing arbitrary typed failures at “Test.run” makes ordinary Effect cleanup unsound.
- The evaluator cannot invoke marked private declarations while preserving their normal semantics.

Any of these findings returns this Accepted direction to Candidate review.

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

The language/compiler marker and inventory boundary, standard-library Test actor, project test
command, and focused end-to-end acceptance cases may become separate DAG-ordered OpenSpec changes.
The handoff must include invalid eligibility diagnostic scenarios, distinguish complete StackPath
semantics from standard presentation filtering, and prove owned evaluator path capture plus cleanup
before implementation commits to a representation. Failure of that path evidence gate returns the
SLP to Candidate rather than weakening the accepted contract. This SLP does not itself create the
OpenSpec artifacts.

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
| 32 | 2026-08-23 | Round 1 review replaced the compiler-known Reporter assumption with a source-supplied service witness, defined explicit user and standard-library test roots, limited freshness to standard-runner policy, reused HostInput instead of a test-only entry adapter, and expressed events/outcomes with existing structs and structural unions. |
| 33 | 2026-08-23 | Round 2 review removed Reporter from test eligibility and moved structured reporting to ordinary runner consumption of closed outcomes; distinguished runner and inventory roots, constrained test paths to the package source root, closed the standard runner through explicit OsHostInput/Allocator provision, defined bytewise filters and infrastructure status, and exposed slice-style inventory access plus uniform test adapters. |
| 34 | 2026-08-23 | Author resolution made PassedCase, FailedCase, and Event fields public for external Reporter providers and changed standard filtering to ASCII case-insensitive byte-substring matching while leaving non-ASCII bytes exact. |
| 35 | 2026-08-23 | Author accepted the direction after audit a001 passed, completed the custom Reporter example, delegated diagnostic and presentation mechanics to OpenSpec, and retained owned StackPath realization as an explicit revisit gate. |
| 36 | 2026-08-23 | Author named the silent assertion failure `Test.AssertionError`; the accepted behavior is unchanged, and `Outcome.Failed` remains the broader result for any unhandled typed failure. |
