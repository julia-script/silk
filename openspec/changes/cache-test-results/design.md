# Design

## Context

See `proposal.md` for motivation. `TestDiscovery.Info.fingerprint` hashes only one declaration's
canonical authored header/body and deliberately excludes helpers and configuration. The frontend
already publishes a completed `CompilationProfile.identity` and a dependency-sensitive
`ProfileBootstrap.Completion.bootstrapIdentity`. `Instances.Discovery` owns concrete runtime,
callable, Effect, provider, cleanup, intrinsic, and foreign-call relationships, but its current
artifact-wide observations are not yet partitioned by test. `Storage` provides bounded reads and
atomic publication; `Workflow.test` currently compiles one executable and `Program.run` inherits
stdout/stderr and returns only the exit code.

This change depends on `add-basic-test-runner`; its runner delta removes that change's explicit
no-result-cache requirement. It also reuses `jul-219-compiler-storage` without changing Storage's
generic record contract. The result cache is optional derived data, while the source runner remains
the execution/selection authority.

## Goals / Non-Goals

**Goals:**

- Publish one independently reusable identity per discovered test with conservative completeness.
- Give the identity and result-record tasks disjoint ownership and a fixed manifest contract.
- Keep machine result authority separate from inherited user output.
- Recover from expected optional-cache failures without swallowing interruption or defects.

**Non-Goals:**

- General semantic/backend cache redesign, persistent HIR, remote/shared cache routing, eviction,
  watch mode, parallel execution, process-per-test isolation, fixtures, or automatic external-input
  tracking.
- Treating a test pass as portable across projects, profiles, compiler distributions, native
  supplies, or runner policy.

## Decisions

### 1. A compiler-owned TestExecution actor publishes the only reusable per-test manifest

Add a public `TestExecution` actor in the compiler package. Its immutable `Manifest` contains the
catalog identity, shared execution-environment identity, and canonical catalog-order entries. Each
entry retains `TestDiscovery.Info` plus either `Eligible { identity }` or
`Ineligible { reason }`. `Driver.Compiled` exposes the manifest only for a successfully completed
test-purpose native executable. The CLI consumes it but cannot construct or strengthen an
ineligible identity.

`TestDiscovery` continues to own declaration identity, presentation, and the local authored
fingerprint; its fingerprint contract does not change. `Instances` owns graph completeness. It
adds an execution-closure projection rooted at the exact zero-argument test instance and includes
runtime calls, callable/Effect bodies, cleanup/finalizer targets, selected provider edges,
intrinsic/foreign operations, and per-specialization static-dependency observations.
`Residualization` records demanded compile-time helpers/defaults/predicates by the residual
application that demanded them rather than only in one artifact-wide map. `TestExecution` owns
canonical sorting, framing, eligibility, and the final digest.

The final `silk-test-execution-v1` digest frames:

1. canonical test declaration identity and its local authored header/body digest;
2. the canonical rooted dependency graph, including node/edge kind, specialization/evidence,
   selected provider roles/types, and canonical authored fingerprints for every runtime and
   compile-time source dependency;
3. the published `CompilationProfile.identity` and dependency-sensitive
   `ProfileBootstrap.bootstrapIdentity`;
4. bundled runner/host-runtime policy identity and compiler distribution digest; and
5. a resolved external native-execution identity.

The native-execution identity is derived from the resolved link environment but explicitly omits
the generated program object, output path, and final whole-artifact digest, any of which would let
an unrelated test invalidate every entry. It includes selected target/toolchain/supply identities,
runtime/helper policy, and content digests for explicit and transitively resolved external native
inputs. Physical paths enter only where the platform ABI embeds them; otherwise their selected
content identity is used.

If a graph node, static dependency, provider edge, or native input cannot be attributed
completely, that entry becomes `Ineligible`. There is no whole-file, whole-catalog, artifact-plan,
or native-link-plan fallback. This favors execution over stale authorization and allows unrelated
eligible tests to reuse.

The alternative—using `TestDiscovery.Info.fingerprint`, `SemanticQuery.Completed.fingerprint`,
`ArtifactPlan.identity`, or the final object digest—either omits executed bodies or invalidates
unrelated tests. Those identities remain valid for their current owners and are not repurposed.

### 2. The CLI owns small pass records over project-local Storage

Add a `TestResult` actor in the CLI package. It uses `Storage.fileSystemService` rooted at
`<project build.output-dir>/.silk-cache`, namespace `test-results-v1`, key equal to the 64-character
execution digest, and a maximum complete-record size of 4096 bytes. Consequently records persist
across CLI processes, remain project/output-root scoped, and are removed by the existing
`silk clean` behavior. Changing the record schema advances the namespace; no migration or legacy
decoder is retained.

The strict record is canonical UTF-8 data containing exactly schema version, execution identity,
`Passed`, and an integrity digest over the framed payload. Lookup accepts only an exact key/payload
match. The actor converts typed `StorageError` and decode/admission failures into observable misses;
publication converts typed expected failures into an observable skipped write. It catches no
interruption or defect.

Only a completed, fully validated runner receipt can request publication. A receipt may publish
the executed passes from a completed mixed pass/fail run, but never failures or cached entries. A
trap, signal, incomplete receipt, or operational exchange failure publishes nothing from that run.

The alternative—putting result meaning in generic `Storage`—would couple opaque storage to test
policy. Storing one suite record would also prevent independent reuse and is rejected.

### 3. Program owns a scoped two-file binary exchange; stdout/stderr stay inherited

Add a `TestExchange` actor for a closed length-framed binary schema. `Program.runTest` creates a
unique owner-private scratch directory with `Effect.acquireUseRelease`, writes a complete input
plan, passes the plan/result paths as internal runner arguments, executes with inherited stdin/
stdout/stderr, reads one result receipt, and releases the scratch directory after success, typed
failure, defect, or interruption without replacing the primary exit.

Both files start with fixed magic/version, a fresh 256-bit nonce, and discovered count. The plan
contains one canonical-order entry per discovered declaration: ordinal, declaration identity,
optional opaque execution identity, and `Execute` or `Cached`. Only an eligible entry can carry an
execution identity and only an admitted record can select `Cached`. The source runner validates
the declaration sequence, treats the execution identity as opaque compiler-owned data, and echoes
it in each selected disposition. The receipt also echoes the nonce and plan digest, contains one
ordered disposition for every selected test (`Cached`, `Passed`, or `Failed`), and ends with
discovered, selected, cached, executed, passed, failed, and status totals. Duplicate, missing,
out-of-order, or mismatched entries reject the complete receipt.

The maximum plan or receipt size is `4096 + 256 * discoveredCount` bytes with checked safe-integer
arithmetic and a 16 MiB hard ceiling. If a manifest cannot fit, the workflow uses an all-execute
bounded plan and publishes no results. The bundled runner validates the complete plan before the
first test, remains the authority for file/name filtering, and writes the completion frame only
after the sequential loop and cleanup finish.

The CLI never captures or parses user stdout/stderr, so arbitrary test output cannot be mistaken
for a result. A normally exiting process whose receipt is invalid becomes status 2. Abnormal
termination remains abnormal and produces no admitted receipt. The alternative—sentinels on
stdout/stderr—cannot provide an authority boundary; one-process-per-test would change shared-state
and isolation semantics.

### 4. Workflow integrates hits without moving selection policy out of Silk

`TestCommand` adds a boolean `--no-cache` flag and `TestSelection` carries `cacheResults`, defaulting
to true. `Workflow.test` still compiles the complete discovery catalog before filters execute. For
an eligible manifest it opens the project-local result store and attempts one lookup per eligible
entry; unselected lookups are permitted so the CLI does not duplicate source selection policy.
The plan marks only exact admitted hits. The source runner applies filters, reports selected hits
as cached, and executes all selected misses in catalog order.

After `Program.runTest`, Workflow validates the complete receipt, publishes only its executed
passes, reports cache read/write events separately from test failures, and returns the authoritative
runner status. `--no-cache` performs no `TestResult.lookup` or `TestResult.publishPass` calls and
passes an all-execute plan; it does not set `Driver.CompileRequest.cache = false`, so compiler
caches retain their existing behavior.

The summary expands to discovered/selected/cached/executed/passed/failed. Cached is a subset of
passed; executed equals passed-executed plus failed. Existing status 0/1/2, zero-selection,
diagnostic, and abnormal-termination behavior remains.

### 5. External state and cross-test side effects stay explicit assumptions

The identity covers declared compiler/profile/source/native inputs, not arbitrary files,
environment variables, network responses, clocks, nondeterminism, or mutable state established by
an earlier test. Reusing a pass deliberately does not replay that test's side effects. Tests that
depend on such state use `--no-cache`; automatic observation of those inputs would require a new
isolation/input model outside this change.

## Risks / Trade-offs

- [A missed provider, cleanup, or compile-time edge could authorize a stale pass] → Centralize
  completeness in `Instances`/`Residualization`, mark uncertain entries ineligible, and exercise
  helper, static-helper, provider, finalizer, and shared-dependency mutations independently.
- [A shared environment component can over-invalidate tests] → Keep profile/compiler/runner/native
  environment identity explicit and shared, but exclude generated whole-program objects and
  unrelated authored declarations.
- [The exchange adds filesystem work to every test run] → Keep it bounded and scoped; it is small
  metadata, while stdout/stderr remain streaming and uncaptured.
- [Skipping a passing test can remove state later tests expected] → Document the declared-input
  contract and provide `--no-cache`; do not pretend to reproduce side effects.
- [Optional cache failures could hide cancellation] → Recover only typed Storage/codec events and
  let interruption and defects propagate unchanged.

## Migration Plan

1. Add `TestExecution` dependency closure/identity and expose the manifest on completed test
   compilations, keeping local fingerprints unchanged.
2. Add `TestResult`, `TestExchange`, `Program.runTest`, and bounded source-runner plan/receipt
   support without yet enabling persistent hits in the workflow.
3. Add default-on Workflow/TestCommand integration, pass-only publication, expanded reporting,
   documentation, and the `--no-cache` bypass.
4. Delete the unconditional-rerun path and any temporary dual exchange shape in the same change.
   Rollback is a normal revert; derived records under the versioned namespace need no migration and
   incompatible leftovers are ignored.
