## Context

Silk already publishes canonical type, ownership, cleanup, callable-capture, and borrow facts. It
has no thread-transfer operation yet, but SLP-0002 requires the local restriction to be semantic
data before a future parallel model consumes it. See `proposal.md` for motivation and the two delta
specs for the behavior contract.

## Goals / Non-Goals

**Goals:**

- Give the sealed local-shared core one canonical type identity independent of source actor names.
- Make affinity recursively visible through aggregates and executable captures.
- Preserve one affine obligation per live handle without treating `T` as duplicated.

**Non-Goals:**

- Define thread APIs, transfer syntax, construction, lifecycle operations, or target representation.
- Infer local affinity from an ordinary declaration spelling such as `Shared`.

## Decisions

### Record execution affinity as a closed semantic type property

Resolved types gain a canonical execution-affinity fact with `Unrestricted`, `ParameterDependent`,
`LocalExecution`, and `Unavailable` outcomes. `ParameterDependent` carries a non-empty canonically
ordered set of type-parameter identities; it is available semantic data, not a diagnostic recovery
state. `Intrinsic.SharedCore<T>` is intrinsically `LocalExecution`; aggregates, references, borrowed
views, callable environments, Effect environments, and later execution frames join the property
recursively.

The join is total and ordered. Any unavailable component makes the result `Unavailable`, retaining
every distinct causal diagnostic identity in canonical traversal order. Otherwise any
`LocalExecution` component makes the result `LocalExecution`. Otherwise parameter-dependent
components produce `ParameterDependent` with the canonical ordered union of their parameter
identities. Otherwise the result is `Unrestricted`. Nominals, arrays, normalized unions, callable
environments, Effect environments, and later execution frames join their components. A reference or
borrowed view joins its exposed type with its canonical borrow-root dependency so a projection
cannot erase a local root's affinity. Generic specialization substitutes the concrete arguments and
normalizes the same join again; it does not retain a stale parameter-dependent verdict.

This property describes where a value may move, not where its allocation resides. Ordinary moves
between frames in one same-thread local execution domain, including suspension, parking, resumption,
and transfer between independently resumable frames, remain legal and retain the same fact and
obligation. `LocalExecution` contains no execution-instance, fiber, or Scheduler identity: later
SLP-0001 sufficiency work may prove movement between fibers managed by one local Scheduler without
changing this fact. A future parallel-transfer proposal may define an operation that consumes
`LocalExecution` and rejects it. This change only publishes the deterministic semantic outcome; it
does not add Scheduler policy, transfer syntax, a transfer-eligibility query, or a dormant transfer
diagnostic.

Alternatives rejected: attaching affinity only to a future thread API would let current semantic
artifacts lose the property, while recognizing `Shared` by source spelling would violate the sealed
intrinsic boundary and fail for renamed wrappers or nested aggregates.

### Classify every core handle as affine independently of `T`

The ownership category of `Intrinsic.SharedCore<T>` is always affine. A move transfers one handle
obligation, a consumed source becomes dead, and no structural `Copy` derivation may admit the core.
The element type keeps its own category and cleanup facts; the core's generic argument does not
turn a Copy `T` into a Copy core or an affine `T` into duplicated payload ownership.

Copy failures remain phase-owned. A non-consuming source use that would duplicate a core is the
ordinary ownership `OWN0003` explicit-move-required violation at the attempted read. A source
`impl Copy` for an aggregate containing a core is rejected earlier by conformance validation as
`SEM0083` at the implementation declaration, and no Copy evidence reaches ownership. If `T` cannot
be resolved, specialization preserves the causal unavailable evidence and publishes neither a
Copy/unrestricted claim nor a satisfied ownership verdict.

Alternatives rejected: Copy handles cannot account for count transitions, and classifying by `T`
would make `SharedCore<i32>` silently duplicable while still requiring dynamic lifetime accounting.

### Publish a sealed role rather than implementation lanes

Semantic facts expose the canonical intrinsic nominal identity, `LocalSharedStrong` ownership role,
affine category, local affinity, and `T`. They do not expose count lanes, addresses, access bits, or
reclaim tickets. Those are target realization details owned by later slices.

## Risks / Trade-offs

- **Risk: affinity joins become inconsistent across type forms** → centralize the recursive join in
  the canonical type-property analysis and gate its deterministic encoding with aggregate,
  reference, open-parameter, specialization, and executable-capture cases.
- **Risk: future transfer work needs more than a binary local property** → keep the fact as a closed
  semantic outcome that can be extended by a future language proposal rather than predefining a
  parallel-memory model here.
