## Context

The compiler already has lossless contextual syntax, canonical declaration identities, multi-root
project analysis, closed Effect contracts, uniform evaluator termination with logical paths, and a
sealed intrinsic catalog. It does not preserve a test marker, distinguish test-root reachability,
or invoke a compiler-selected heterogeneous set of private functions. See `proposal.md` and the
eight delta specs for the contract.

## Goals / Non-Goals

**Goals:**

- Make inventory membership a canonical compiler fact while leaving selection and reporting in source.
- Reuse the existing evaluator failure path and cleanup machinery at a narrower per-test boundary.
- Keep handles uniform and inspectable without adding a general runtime function-pointer model.

**Non-Goals:**

- Add compiled-engine test invocation, trap isolation, failure-value erasure APIs, or source runner policy.
- Recognize any canonical standard-library actor by spelling.

## Decisions

### Retain the marker on the canonical function header

The parser records one contextual marker node on a function declaration. Declaration collection
publishes the marker beside the canonical header identity; after header contract normalization a
dedicated eligibility pass either emits one available inventory candidate or a structured
diagnostic. It checks the ordinary resolved function facts rather than maintaining a second
signature parser. The marker does not alter body resolution or callable use.

Alternatives rejected: a test block would introduce a second declaration kind, while an attribute
system is disproportionate to the single accepted marker and would defer the actual semantics.

### Compose ordinary root closures with separate project reachability facts

The role-aware ProjectRequest contains an ordered test-root set and one runner root. It performs the
existing one-root CompilationRequest load for each constituent root and composes their results into
one canonical de-duplicated project union. Each module fact carries two reachability bits: reached
from any test root and reached from the runner. Inventory collection consumes the first bit only;
entry selection consumes the runner root. Canonical sorting discards root input order after
reachability is known, while an ordinary CompilationRequest retains its current one-root contract.

Alternatives rejected: loading independent projects would duplicate shared module identities and
facts; scanning the source root would violate explicit closure semantics.

### Represent a handle as an inventory-local ordinal token

The internal inventory stores canonical declaration identity, source-order rank, normalized closed
contract, and a generated per-entry invocation adapter. The source-visible opaque handle contains
only an inventory-local ordinal and is Copy for the lifetime of the test compilation. Metadata and
invocation validate and index through the current immutable inventory; the token cannot become a
general callable value. Public metadata exposes the canonical UTF-8 ID as an immutable borrowed byte
view.

The adapter specializes the known declaration directly. It catches that test's complete typed
failure row only after ordinary cleanup and maps it to the closed internal failed variant. Distinct
failure rows therefore do not require one erased source-callable function-pointer signature.

Alternatives rejected: source-visible function values would expand nominal callable storage and
erased dispatch, while name-based invocation would be forgeable and unstable under overload or
module resolution.

### Transfer the evaluator's existing logical path snapshot

Before choosing a source representation, add a focused evaluator characterization at the exact
unhandled-failure point used by existing termination. The prototype runs a nested marked Effect
whose frames own affine cleanup witnesses and verifies that the evaluator's already-materialized
logical path can be transferred into one opaque owned result after all exited frames clean exactly
once. The source `StackPath` owns that immutable snapshot; `pathLength` and checked `pathFrame`
operations expose immutable frame facts without a second filesystem mapping.

The characterization must also prove that capture adds no second unbounded copy: it transfers the
existing termination snapshot whose depth is bounded by the evaluator activation limit. The
escaping failure payload itself owns a cleanup witness, so the gate proves it is destroyed exactly
once rather than merely not retained. Repeated outcomes then prove that dropping Failed, moving the
path through an equivalent case container, and downstream consumption failure each transfer and
reclaim the snapshot exactly once. If the current termination representation cannot support those
properties, implementation stops and routes SLP-0004 back to Candidate. It must not substitute a
borrowed trace, truncated path, leaked failure payload, or presentation-selected frames.

Alternatives rejected: printing at the failure point couples policy to invocation; retaining the
failure value requires general erasure and rendering; recomputing from a released evaluator stack
risks provenance and cleanup drift.

### Keep identities target-neutral and initial execution evaluator-only

The marker, canonical IDs, inventory order, opaque handle semantics, and closed Outcome contract do
not encode a target or engine. The initial test compilation materializes per-entry adapters only for
the evaluator workflow. The canonical intrinsic catalog marks the testing operations supported by
evaluation for every semantic target evaluation accepts and by no artifact-emission backend. A
reachable native LLVM or direct WebAssembly call is rejected by existing availability planning
before lowering; an unreachable wrapper remains pay-for-use. A later execution-mode proposal can
expand the supported-engine set while preserving the same source identities and outcomes.

Ordinary non-test compilation does not request inventory materialization, so it roots neither the
handle table nor per-test adapters even when reachable source contains marked declarations.

### Keep non-outcome evaluator termination outside the outcome union

Only the existing unhandled typed-failure termination is intercepted by the per-entry adapter.
Every other existing evaluator termination retains its existing classification outside Outcome and
therefore stops the current runner execution rather than being fabricated as Passed or Failed.
Runtime traps remain fatal as required by the accepted direction. This avoids promising recovery
or redefining evaluator limits and blocked states without a separate execution-boundary decision.

## Risks / Trade-offs

- **Owned path transfer proves impossible with current cleanup order** → stop at the characterization
  gate and return the SLP to Candidate; do not implement later slices.
- **Runner reachability accidentally contaminates inventory** → publish both reachability facts in
  deterministic inspection and test runner-only, shared, and reordered-root cases.
- **Opaque ordinal leaks into general callability** → keep construction and dispatch sealed, and
  audit callable facts, HIR, MIR, and intrinsic inventory for conversion paths.
- **Evaluator-only support leaks into a partial backend path** → encode the exact supported-engine
  set in the canonical availability catalog and gate reachable native/Wasm calls before lowering.
- **Eligibility diagnostics cascade from malformed contracts** → run after canonical header
  normalization and retain existing causal-unavailable evidence rather than fabricating candidates.
