# Silk language reference

This directory is the programmer-facing reference for Silk's language rules. It is being assembled
during language stabilization from existing decisions, OpenSpec requirements, tests, and compiler
behavior.

## Audience

The primary reader is a programmer writing Silk who knows general programming concepts but does not
know Silk compiler internals. Familiarity with Rust, Effect, or Effect TypeScript may help, but is
not assumed.

Language and compiler contributors are a secondary audience. Evidence links connect each rule to
the detailed artifacts used to verify or reconcile it.

## Authority during stabilization

Each rule carries one status:

- **Confirmed** — the language author has explicitly confirmed the intended programmer-visible
  behavior during stabilization.
- **Candidate** — existing decisions, specifications, tests, and implementation support one
  coherent rule, but the author has not yet confirmed it.
- **Disputed** — authoritative-looking artifacts disagree or the implementation contradicts the
  intended rule.
- **Unsupported** — the boundary is deliberate and programmer-visible.
- **Unresolved** — no coherent rule has been selected.

Confirmed rules describe intended Silk semantics but do not create a compatibility promise before
1.0. Existing OpenSpec requirements and tests remain trace evidence; stabilization work must
reconcile them with confirmed rules rather than silently preserving a contradiction.

## Reference shape

Every rule uses the same compact structure:

1. **Status**
2. **Rule** — one programmer-visible statement
3. **Example** — the smallest useful valid program or fragment
4. **Boundary** — the nearest program that is invalid or behaves differently
5. **Diagnostics** — the source error required at an invalid boundary. Rules that only define valid
   behavior say that no diagnostic applies.
6. **Evidence** — decisions, specifications, tests, and known implementation mismatches

A required diagnostic describes the language contract independently from the current compiler. It
states the invalid condition, the offending source location, and the information the error must
communicate. A stable diagnostic code is recorded when one exists; an unassigned code remains
explicit rather than borrowing an unrelated current code. Exact message wording belongs to the
generated diagnostic catalog, not to the language rule.

Defining intended semantics comes before a systematic implementation audit. A current compiler
mismatch is recorded when stabilization work encounters one, but discovering every mismatch is not
a prerequisite for confirming a language rule.

Compiler representations such as HIR, MIR, generated runners, layouts, and backend instructions do
not belong in a language rule unless a program can observe them.

## Contents

- [Program entry](program-entry.md) — ordinary and effectful `main`, automatic Effect execution,
  unhandled typed failures, and requirement closure.
- [Statements and discarded values](statements-and-discarding.md) — which expression results may be
  ignored and why the current rule still needs confirmation.
- [Effects and execution](effects-and-execution.md) — effect construction, success values, `run`,
  and nested Effects. Initial rules are confirmed.
- [Effect contracts](effect-contracts.md) — success, failure, and requirement channels; declaration
  bounds; generic failure types and requirement rows; and concrete Effect identity.
- [Typed failures](typed-failures.md) — ordinary failure values, propagation, recovery, cleanup,
  diagnostic context, and fatal traps.
- [Language style guide](style-guide.md) — non-semantic naming conventions for readable,
  discoverable Silk APIs.
- [Ownership and borrowing](ownership-and-borrowing.md) — Copy and affine values, moves, borrows,
  mutation, and cleanup. Initial rules are candidates pending review.

The following domains will receive pages only when their first rules are ready:

- remaining syntax and expressions;
- modules, names, and visibility beyond entry discovery;
- values and types;
- functions, callables, and control flow;
- generics, interfaces, and specialization;
- targets, unsafe code, and intrinsics; and
- standard-library boundaries.

Tutorials, task-oriented guides, design rationale, compiler architecture, and standard-library API
documentation remain separate from this reference.
