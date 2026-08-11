## Context

Silk packages canonical standard-library source as one navigable module graph. Some future providers
must call irreducible platform operations, but loading those declarations must not make every program
platform-specific. The intrinsic catalog already describes compiler operations; this change makes
supported-target metadata enforceable at the executable boundary.

The mechanism is intentionally generic. Filesystem and stdout providers are consumers, not concepts
known to reachability or target validation.

## Goals / Non-Goals

**Goals:**

- enforce a target set for each sealed intrinsic operation;
- validate only operations reachable from the selected executable entry;
- produce stable, actionable diagnostics before evaluation or emission;
- omit unused target-specific runtime symbols, imports, and adapters.

**Non-Goals:**

- conditional parsing, target-specific source module loading, or source annotations;
- provider-name or service-name recognition;
- automatic backend substitution;
- a hosted-Wasm platform ABI or polyfill mechanism.

## Decisions

### Supported targets live in the intrinsic catalog

Availability is part of an operation's sealed compiler contract alongside identity and signature.
The catalog stores normalized target families for evaluation and backend requests. Inventory tests
encode the data deterministically. Ordinary Silk cannot grant itself target privilege.

### Validation runs over executable operation closure

Module closure remains responsible for source availability and semantic analysis. After entry and
instance discovery, executable planning collects the intrinsic identities that survive reachable
calls and validates that set against the explicit evaluator/backend target. This permits canonical
source to contain an unused native provider while a pure program still emits direct Wasm.

Validating at parse or module-closure time was rejected because it would make packaged declarations
contaminate unrelated programs and defeat pay-for-use.

### One diagnostic precedes execution or artifact construction

Unsupported reachable operations produce a stable target-unavailable diagnostic containing the
canonical intrinsic identity, selected target, and call provenance. Evaluation and backends receive
only a validated plan, so they do not need ad hoc fallback behavior or partial-output cleanup.

### Runtime support follows the retained inventory

Native link planning and Wasm import construction use the same reachable intrinsic inventory. If an
operation is absent, its shim, import, and host adapter are absent. This makes pay-for-use observable
and testable rather than an optimizer accident.

## Risks / Trade-offs

- Executable closure must be complete before target validation. Inventory parity tests will compare
  call planning, evaluation, and both backends.
- Generic target families may eventually need finer host capabilities. The catalog representation
  should allow additive normalized selectors without introducing source annotations now.
- Semantically analyzing unreachable platform wrappers can still report ordinary source errors.
  Only target incompatibility is deferred; this preserves module correctness and tooling.

## Migration Plan

Add all-target metadata to every current intrinsic, enforce inventory completeness, then insert
reachable-only validation before evaluator/backend entry. Existing operations remain behaviorally
unchanged. Later restricted operations can declare narrower sets. Rollback restores documentary
metadata and removes validation; no user data migration is involved.

## Open Questions

None for the first restricted intrinsic consumer. Hosted environments and runtime capability
negotiation require separate proposals.
