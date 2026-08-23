## Context

The first three changes establish the language boundary, ordinary standard-library actor, and
project command. This final slice supplies connected evidence against the concrete goal that
motivated SLP-0004: test current standard-library operations, especially seeded Random, through the
same surface available to users.

## Goals / Non-Goals

**Goals:**

- Prove the minimal surface on both user and standard-library root sources.
- Exercise default and custom runner/reporting policy plus filter and status edges.
- Produce a checked-in privilege and scope findings report.

**Non-Goals:**

- Add SHA solely for a testing example or broaden the assertion surface.
- Claim native, Wasm, parallel, process-isolated, or recoverable-trap execution.

## Decisions

### Use seeded Random as the connected standard-library witness

The standard-library catalog adds closed marked tests beside the existing Random actor. One test
constructs the documented seeded provider inside its body and uses `Test.assert` on the published
first scalar. A second fills a byte buffer and passes borrowed actual and published expected bytes
to `Test.equalBytes`. A small user fixture repeats the same public operations through manifest roots.

Alternatives rejected: introducing SHA would mix an unimplemented library feature into testing
evidence; host-language assertions would fail to prove the Silk helpers.

### Cover customization with one runnable custom-runner fixture

One source-root-contained module is both an explicit test root and custom runner. Its private marked
failure enters inventory because of the test-root role, while runner-only helper modules do not. The
runner iterates Function handles, shares a mutable counting Reporter, consumes public Event fields,
and returns an exact nonzero aggregate status derived from that Reporter state; the command must
preserve it. A paired fixture omits reporting to prove Reporter is optional.

Alternatives rejected: isolated snippets do not prove root composition or service provision;
separate host mocks could accidentally bypass the Silk runner.

### Keep acceptance at the cheapest evaluator tier

All semantic claims use one shared project-analysis snapshot per source graph. This final slice
cites frozen prerequisite evidence for isolated marker, inventory, filter, Reporter, status,
cleanup, path, and privilege properties. It adds only compact connected evaluator witnesses that
can fail specifically because Random, user/stdlib placement, command forwarding, and ordinary-source
policy are composed. No per-feature native or Wasm agreement leg is added because this SLP
explicitly admits only evaluator execution.

The corpus includes exact byte injection at the post-parser HostInput seam for invalid UTF-8
matching, while ordinary CLI acceptance covers every platform-admitted byte case. A non-ASCII
positive match plus a normalization/case-fold near-miss distinguishes exact byte matching; an
invalid-byte filter ORed with a matching ASCII filter proves invalid bytes neither decode nor abort
selection. This separates matcher semantics from platform shell limitations without introducing a
String contract.

### Make the findings gate auditable

The checked-in report maps each SLP goal and falsifier to a fixture or artifact assertion, records
the owned StackPath and platform-byte gate results, inventories testing-specific syntax, semantic,
HIR, MIR, evaluator, intrinsic, backend, and command branches, and classifies every discovered wall
as language, standard-library, compiler defect, tooling/ergonomics, or performance/cost. Passing
requires every prerequisite gate and falsifier to be proven, no source actor spelling in privileged
artifacts, and no feature from the complete SLP future list added to complete the witness. A failed
owned-path or admitted-platform-byte gate returns SLP-0004 to Candidate.

## Risks / Trade-offs

- **Random fixture drifts from its published vector** → derive expected bytes from the existing
  committed documentation/fixture and keep one source of truth.
- **Acceptance duplicates compiler semantics tests** → reserve pressure cases for connected command
  behavior and keep eligibility/path unit claims in prerequisite changes.
- **Privilege audit relies on source grep alone** → inspect syntax, semantic facts, HIR, MIR,
  evaluator operations, intrinsic catalog, backend branches, and command artifacts plus a renamed
  equivalent fixture.
- **Scope expands when a richer report seems convenient** → record the gap in findings and leave it
  for the future SLPs rather than changing the accepted minimal surface.
