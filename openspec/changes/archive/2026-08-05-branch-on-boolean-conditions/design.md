## Context

Slice 1's final change: `Bool`, comparisons, and `if`/`else`, per issue 02's "only `Bool`
controls conditionals, no truthiness" and issue 08's `if node.isLeaf { return 1 }` shape.

## Goals / Non-Goals

- Goals: the four keywords, boolean literals, `Bool` as a declared type, comparison and `not`
  built-ins with per-operation contracts, condition and argument type checking (`SEM0011`,
  `SEM0012`), HIR conditionals, user-authored `Branch` lowering with joins, arm-scoped
  ownership with per-return exits, interpreter/native arm parity, lab coverage.
- Non-goals: expression `if`, pattern conditions, loops, guards, `match` (next milestone),
  logical and/or operators, path-sensitive ownership.

## Decisions

### Statement `if` with a mandatory trailing return

Arms are brace-delimited statement sequences (bindings, nested conditionals, returns); the
function body still ends in exactly one trailing return statement. Every path therefore returns
without needing an exhaustiveness analysis: arms may return early, otherwise control falls
through. Expression `if` waits for evidence (issue 08 shows statement position only).

### `Bool` is `i32` zero-or-one below MIR

MIR gains the `Bool` logical type; the backend stores it as `i32` (comparisons emit
`icmp` + `zext`, branches reuse the nonzero test). Rationale: one register class keeps the
backend's local map and the existing `Branch` emission untouched; `i1` locals would ripple
through every operand path for no bootstrap benefit.

### Comparisons are `Binary` operators; `not` lowers through equality

The `Binary` operator vocabulary grows with six non-trapping comparisons producing `Bool`.
`Bool.not(x)` lowers to `Equals(x, false-literal)` — no new operation kind. Rejected: a unary
operation class for one operation.

### Conservative conditional moves

A move inside any arm consumes for everything after the conditional (flow-insensitive
worklist, sound and simple). Path-sensitive liveness waits for real pressure. Arm bindings
release at their arm boundary; each return statement is its own exit in the cleanup plan.

### Type checking arrives where `Bool` makes it real

`SEM0011` (condition not `Bool`), `SEM0012` (argument/parameter type mismatch — user and
built-in calls both, using the existing mapping machinery). Return compatibility already
compares types and generalizes for free.

## Risks / Trade-offs

- Join-block lowering introduces multi-block functions from real programs for the first time →
  the MIR verifier and the differential corpus (both-arm programs) gate it.
- Conservative moves reject some sound programs → diagnosable, and strictly a subset of what
  structs will need anyway.

## Migration Plan

Purely additive. Existing goldens unchanged; branching fixtures gain their own.

## Open Questions

None — the proposal's open questions (`Bool` representation, statement-only `if`) are decided
above.
