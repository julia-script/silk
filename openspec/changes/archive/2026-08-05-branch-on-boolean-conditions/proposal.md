## Why

With bindings and arithmetic, programs compute — but they cannot decide. Every user-visible
CFG is still one path plus compiler-generated trap checks. `Bool`, comparisons, and `if` are
the smallest decision-making surface: they make lowering emit user-authored `Branch`
terminators, give the MIR lab real diamonds, and make the differential harness cover programs
whose interpreter and native runs must agree arm by arm. Issue 02 pins the semantics tightly —
only `Bool` controls conditionals, no truthiness — so the feature is small and closed.

## What Changes

- Add `Bool` as the second scalar type: `true`/`false` keyword literals, `Bool` type
  annotation in parameters and returns, `Bool`-typed bindings.
- Add compiler-known comparison operations on the `I32` actor (`equals`, `lessThan`, and the
  ordinary companions) returning `Bool`, plus the `Bool` actor's `not` (issue 08's
  `flag |> Bool.not` example, minus the pipe) — riding the built-in-actor and
  qualified-callee machinery from the arithmetic change.
- Parse `if condition { … } else { … }` as a statement per issue 08's accepted shape
  (`if node.isLeaf { return 1 }`): brace-delimited arms, no parentheses, optional `else`;
  condition must elaborate to `Bool` (issue 02: no truthiness — an `I32` condition is a
  semantic diagnostic, never a coercion).
- Elaborate `if` to HIR with both arms; ownership checks each arm's bindings and produces
  per-exit cleanup plans (a binding scoped to an arm drops at that arm's end — the first
  real per-path cleanup divergence).
- Lower to MIR `Branch` with join blocks; the interpreter executes both shapes and traces
  taken arms; the backend's existing `Branch` emission carries it natively.
- Extend encoders, goldens, corpus (branching programs, arm-divergent results, arm-scoped
  drops), and the differential harness across both arms of every program.
- Inspector: the MIR CFG lab shows user-authored diamonds with hover provenance; the
  evaluation lab traces which arm ran.

## Capabilities

### Modified Capabilities

- `bootstrap-lexer`: `if`, `else`, `true`, `false` keywords.
- `bootstrap-syntax`: `if`/`else` statements, `Bool` annotations, boolean literals.
- `bootstrap-declaration-index`: comparison and `Bool` built-in operations.
- `bootstrap-hir`: `Bool` type, conditional statements, condition type checking.
- `bootstrap-semantic-facts`: non-`Bool`-condition and arm-typing diagnostics.
- `bootstrap-ownership`: per-arm scopes and per-exit cleanup plans.
- `bootstrap-mir`: user-authored `Branch` lowering with joins.
- `bootstrap-evaluation`: branch execution and arm trace events.
- `bootstrap-backend`: `Bool` representation (`i1`/`i32` — design decides) at the LLVM level.
- `bootstrap-syntax-inspector`: CFG diamonds and arm traces in the labs.

## Impact

Completes slice 1: after this change the language computes, binds, and decides, and every
phase of the spine has processed real instances of every construct it models. The type system
gains its second scalar, so elaboration's type checking stops being vacuous (`Bool` vs `I32`
mismatches become possible everywhere types meet). Sets up the Next milestone: structs and
unions arrive with `match`, which reuses this change's arm/join lowering shape. Design open
questions: `Bool`'s MIR/LLVM representation, and whether `if` is statement-only for now
(issue 08 shows statement position; expression `if` can wait for evidence).

## Plan References

- [Roadmap — Now: widen the language, slice 1](../../../roadmaps/project.md)
- [Issue 02](../../../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md):
  "Only `Bool` controls ordinary conditionals, loops, and guards. There is no truthiness." —
  and the scalar set that `Bool` belongs to.
- [Issue 08](../../../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md):
  the accepted `if` shape (`if node.isLeaf { return 1 }`) and `Bool.not` as a qualified actor
  operation.
- [Issue 01](../../../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md):
  cleanup on *every* structured exit — per-arm exits are where that plural first matters.
