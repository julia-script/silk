# Silk prescriptive language reference

This directory is the prescriptive programmer-facing reference for Silk's language rules. It states
the intended language behavior against which the compiler, standard library, tests, and other
documentation are reconciled. It is assembled during language stabilization from author decisions,
OpenSpec requirements, tests, and compiler behavior.

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

Confirmed rules prescribe intended Silk semantics but do not create a compatibility promise before
1.0. Candidate, disputed, and unresolved rules mark the exact places where the prescription is not
yet final. Existing OpenSpec requirements and tests remain trace evidence; stabilization work must
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
  ignored and how `drop` makes intentional discard explicit.
- [Effects and execution](effects-and-execution.md) — effect construction, success values, `run`,
  and nested Effects.
- [Effect suspension and stack-safe recursion](effect-suspension.md) — the explicit suspension
  boundary, covered recursive cycles, exact channel preservation, ownership and cleanup, logical
  depth, cross-engine parity, pay-for-use, and the boundary with future async execution.
- [Independently resumable Effect executions](independent-execution.md) — explicit caller-funded
  Execution ownership, activation, external parking, Wake readiness, cancellation, cleanup, and the
  ordinary-source scheduling boundary.
- [Single-threaded schedulers and Fibers](single-threaded-fibers.md) — explicit scheduler entry,
  atomic child publication, affine observation, deterministic readiness, structured cancellation,
  typed shutdown, and provider reuse.
- [Effect contracts](effect-contracts.md) — success, failure, and requirement channels; declaration
  bounds; generic failure types and requirement rows; and finite compatible Effect joins.
- [Requirements and services](requirements-and-services.md) — runtime-provided capabilities,
  dependency-eligible interfaces, conformances, roles, access, provision, and requirement-row
  operations.
- [Typed failures](typed-failures.md) — ordinary failure values, propagation, recovery, cleanup,
  diagnostic context, and fatal traps.
- [Language style guide](style-guide.md) — non-semantic naming and data-first actor-function
  conventions for readable, composable, and extensible Silk APIs.
- [Doc comment style guide](documentation-style-guide.md) — required public comment coverage,
  standard sections, complete examples, symbol links, and ASD-STE100 writing rules.
- [Ownership and borrowing](ownership-and-borrowing.md) — Copy and affine values, moves, borrows,
  mutation, captures, cleanup, allocation lifecycles, and returned views.
- [Allocation-backed local shared ownership](local-shared-ownership.md) — `Shared<T>` construction,
  affine strong handles, callback-scoped access, conflict traps, exact last-handle cleanup, and the
  explicit cycle and thread-transfer boundaries.
- [Functions, callables, and control flow](functions-callables-and-control-flow.md) — named function
  contracts, Effect-block terminal inference, ordered calls, returns, callable sections, pipelines,
  conditionals, loops, and matches.
- [Patterns and destructuring](patterns-and-destructuring.md) — one pattern language across exact
  union-member matches, scalar enum member matches, irrefutable local destructuring, and
  conditional `if let`.
- [Values and types](values-and-types.md) — foundational scalars, literals, nominal structs, fixed
  arrays, lexical views, structural unions, scalar enums, precise inference, and compatibility.
- [Expressions and operators](expressions-and-operators.md) — deterministic evaluation, expression
  composition, scalar and explicitly declared custom operators, scalar enum equality,
  short-circuiting, assignment, atomic replacement, and explicit conversion.
- [Modules, names, and visibility](modules-names-and-visibility.md) — path-derived module identity,
  source-root lookup, contextual import-path segments, static imports, cycles, namespace bindings,
  aliases, collisions, file-named struct and contract scopes, public declaration boundaries,
  redundancy, explicit standard-library imports, and re-export boundaries.
- [Generics, interfaces, and specialization](generics-interfaces-and-specialization.md) — generic
  parameters and inference, compile-time interface contracts, conformances, coherence, and finite
  static specialization, including exact and opaque callable and Effect representations.
- [Unsafe code, intrinsics, and targets](unsafe-intrinsics-and-targets.md) — lexical unsafe
  acknowledgement, caller-owned unsafe contracts, sealed compiler primitives, and reachable target
  availability. Rules are confirmed.
- [Runtime and standard-library boundary](runtime-and-standard-library.md) — language versus
  library semantics, ordinary portable and target-provider source, private runtime support,
  distribution contents, and pay-for-use.
- [Program termination and reporting](program-termination-and-reporting.md) — ordinary and effect
  statuses, unhandled typed-error reports, logical traces, fatal traps, and host boundaries.

Every domain in the initial stabilization map now has a reference page. Individual pages continue
to name unresolved boundaries rather than inventing rules to make the table look complete.

Tutorials, task-oriented guides, design rationale, compiler architecture, and standard-library API
documentation remain separate from this reference.
