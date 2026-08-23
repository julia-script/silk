## Context

`establish-silk-test-inventory` supplies only opaque inventory, metadata, invocation, and path
inspection. Canonical Silk already has structs, structural unions, services, slices, loops,
HostInput, OsHostInput, allocation, and standard streams. This slice composes those features in
ordinary shipped source.

## Goals / Non-Goals

**Goals:**

- Provide the smallest useful assertion, event, Reporter, filter, and default-runner surface.
- Make custom runner and Reporter policy possible without compiler-selected service identity.
- Preserve complete paths while keeping standard output concise and source-resolved.

**Non-Goals:**

- Add generic equality, value rendering, messages, skips, tags, configuration, or concurrency.
- Let assertions print, report, or capture expected and actual values.

## Decisions

### Wrap sealed values in one ordinary Test actor

Canonical `silk.test` defines the public opaque aliases/wrappers and the structural-union Outcome
and Event values from SLP-0004. `Function` remains a Copy token. `StackPath` is affine because it
owns the transferred evaluator snapshot. `pathLength(&StackPath)` and
`pathFrame(&StackPath, usize) -> StackFrame | None` expose immutable borrowed frame views. Each
public StackFrame view names the complete canonical function identity, canonical source-module
identity, and half-open source byte offsets. Out-of-range lookup returns None and path inspection
never allocates, moves, or mutates the path. `Test.functions`, `Test.id`, and `Test.run` are thin
safe wrappers over the sealed operations. All validation and policy above the irreducible dispatch
seam remains visible Silk source.

Alternatives rejected: compiler-known Test types violate minimal privilege; exposing the failure
payload creates an unrelated erasure and rendering contract.

### Keep assertions silent and recoverable

`Test.assert` is one ordinary Effect branch that returns unit or `Test.AssertionError`.
`Test.equalBytes` calls it for length and then each byte from zero upward. Neither operation writes,
reports, allocates, or retains compared data. An ordinary caller can recover AssertionError, in
which case the outer test outcome follows the resulting control flow exactly.

Alternatives rejected: assertion-owned output prevents custom presentation and can report a failure
that the test later recovers; generic equality reopens interface selection and rendering decisions.

### Separate outcome construction from Reporter provision

`Test.run` has no Reporter requirement. The runner first classifies the closed Outcome without
moving it through allocation-free `Test.isFailed(&Outcome)`, consumes it with `Test.event`, then
optionally calls `Test.report` under a lexical mutable Reporter provider. All Event payload fields
are public so external source can implement Reporter. `ReportError` is a public empty detached
struct, so any ordinary Reporter implementation can construct and fail with it. A report failure
is returned separately after the case outcome exists; standard aggregation never rewrites the case.

Alternatives rejected: supplying Reporter around test invocation would make eligibility depend on
an ordinary library service or allow report failure to masquerade as a test failure.

### Implement matching bytewise in source

The standard matcher compares the canonical UTF-8 ID bytes with every raw filter. A small
`asciiFold` helper maps only 0x41–0x5a to 0x61–0x7a. The matcher does a direct sliding substring
loop, ORs filters, and emits selected handles in original inventory order. It performs no decode,
normalization, pattern parse, or sorting. An empty filter is therefore the ordinary empty
substring and matches every ID; bytes that look like glob, regular-expression, exclusion, or tag
syntax remain literal. No-match is decided only after the complete OR filter set selects zero
entries.

Alternatives rejected: String conversion excludes invalid bytes and imports Unicode policy; CLI
filtering prevents custom runners from assigning their own meaning.

### Give each standard report fresh source-owned state

Canonical module `silk/test_runner` is the default distinct runner root and exports exactly the
ordinary executable entry `pub fn main() -> i32`. That entry delegates to the closed OS-host edge;
the module enters inventory only if tooling separately names it as a test root. The runner source
and `silk.test` actor are both listed in the canonical stdlib manifest and generated source table.

The standard runner keeps suite counts as ordinary locals, but creates a new StandardReporter for
each completed case and provides it only around one `Test.report`. The closed edge constructs
ordinary OsHostInput, Allocator, and standard-output providers, skips host argument zero, and copies
indices one onward into owned filter Bytes in exact order. StandardReporter captures borrowed
access to the edge-owned output provider and Allocator, translates every formatting, allocation,
and output failure into its sole `Test.ReportError` channel, and leaves no residual requirement on
`Test.report`. Previously acquired filters, formatting buffers, Events, and paths are reclaimed on
every success and failure route.

StandardReporter writes one exact `PASS <id>\n` or `FAIL <id>\n` line for each completed case,
followed by the failed Event's retained logical function identities as `  at <function>\n`, then
one `<passed> passed; <failed> failed\n` aggregate summary after the selected suite completes. It
cannot call an assertion failure an assertion failure because the accepted Outcome intentionally
erases the failure value. A complete explicit filter miss writes `0 matched\n` and no case or
aggregate summary line.

Standard path presentation walks the complete outer-to-inner StackPath. Physical entry adapters are
already absent from the prerequisite logical path. The reporter omits exactly
`silk/test_runner::main`, `silk/test_runner::runFromOsHost`,
`silk/test_runner::runSuite`, and `silk/test::run`; no spelling-only or prefix predicate is used.
Test-body and called helper frames, including `equalBytes`, remain. The initial exact report prints canonical logical
function identities and does not invent a source-visible filesystem resolver. The path still
retains canonical source-module identity and source offsets; tooling that resolves physical paths
must use the existing project source metadata and FileSourceResolver convention rather than adding
filesystem-path policy to the testing primitive. The Event's path stays logical and unchanged.

Alternatives rejected: one global mutable reporter makes per-case freshness implicit; truncating the
owned path would prevent richer custom reporters; a second path resolver would drift from existing
termination diagnostics.

An empty inventory with no filters follows the final SLP runner sketch: it is an all-pass complete
inventory and returns 0 after writing `0 passed; 0 failed\n`. Only a nonempty explicit filter set
whose complete OR selection is empty receives `0 matched\n` and status 2.

## Risks / Trade-offs

- **Ordinary source lacks one needed path or byte accessor** → add only the smallest sealed accessor
  justified by the inventory slice; do not move filtering or presentation into the compiler.
- **Reporter failure hides a completed outcome** → classify and retain the outcome before reporting,
  stop later cases, reclaim the Event/path, and return infrastructure status 2 regardless of prior
  case status.
- **Presentation filtering removes a useful helper** → identify only known runner infrastructure
  functions and verify custom Reporter access to the unchanged complete path.
- **Generated standard-library source metadata drifts** → update the manifest and regenerate the
  canonical source table in the same task, then test public source resolution.
