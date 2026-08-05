## Why

The realigned spine runs end to end, but every function body is a single `return` of an
expression tree. There are no local bindings, so the ownership phase has nothing to check
(every value is a copyable parameter or temporary), lowering never emits `Drop`, and the
cleanup-plan artifact the phase exists to produce is always empty. Issue 01 deliberately
deferred bindings and moves to issue 08's syntax work; issue 08 is resolved and the grammar
freeze is lifted. `let` is the smallest feature that makes ownership real: bindings come into
existence, live across statements, and end — producing the first genuine liveness ranges, drop
points, and cleanup edges the whole spine was built to carry.

## What Changes

- Widen the grammar with statement sequences: a function body becomes a brace-delimited
  sequence of `let name = expression` statements followed by exactly one `return expression`,
  per issue 08's accepted surface (`let source = run FileSystem.read(...)` shape, minus flows).
  Single-statement bodies keep today's form.
- Add `let` and `move` as keywords in the lexer; add binding-statement and `move name` operand
  nodes to the lossless syntax tree with bounded recovery and deterministic diagnostics.
- Elaborate bindings into HIR: a binding introduces a typed local whose type is inferred from
  its initializer (issue 02: local inference never consults later statements); name resolution
  sees parameters and earlier bindings, flat and non-shadowing (issue 04); reading an unknown
  or not-yet-bound name is a semantic diagnostic.
- Make the ownership phase non-vacuous: per-binding liveness ranges (`liveFrom`/`liveTo` become
  real spans across statements), moves as consuming uses, use-after-move as an ownership
  verdict with a diagnostic cause, and a cleanup plan listing live owners at each structured
  exit in last-acquired, first-released order (issue 01).
- Lower bindings to MIR locals and emit `Drop` operations from the cleanup plan at exits;
  the interpreter executes them and traces binding lifetimes; the backend already handles
  `Move`/`Drop`.
- Extend every touched encoder (syntax, HIR, ownership, MIR) and its goldens; extend the
  corpus with binding programs and the differential harness over them.
- Inspector: the ownership lab's timeline shows real liveness ranges and drop points; the MIR
  lab shows emitted drops; syntax lab shows the new nodes.

## Capabilities

### Modified Capabilities

- `bootstrap-lexer`: `let` and `move` keywords.
- `bootstrap-syntax`: statement sequences, binding statements, `move` operands.
- `bootstrap-hir`: typed locals, binding statements, local name resolution.
- `bootstrap-semantic-facts`: binding facts and resolution diagnostics.
- `bootstrap-ownership`: real liveness, moves, use-after-move, populated cleanup plans.
- `bootstrap-mir`: lowering emits locals and cleanup `Drop`s (model already carries them).
- `bootstrap-evaluation`: binding execution and lifetime trace events.
- `bootstrap-syntax-inspector`: ownership timeline and MIR labs over real bindings.

## Impact

Touches the full spine by design — this is the extend-don't-replace economics the widening
slice exists to prove. No public API breaks: all artifacts gain nodes, none change shape.
Open question for design: what `move` means on a copyable value while `I32` is the only type
— error, no-op, or strictly affine (binding invalidated anyway). Issue 01 pins affine
semantics for non-copy values but leaves copyable moves unspecified; the design must pick one
and record why, knowing the answer only bites for real once structs land.

## Plan References

- [Roadmap — Now: widen the language, slice 1](../../../roadmaps/project.md)
- [Issue 01](../../../wayfinder/bootstrap-language/issues/01-ownership-lifetimes-and-scoped-allocation.md):
  affine single ownership, immutable-by-default bindings, automatic cleanup in
  last-acquired-first-released order at every structured exit — and the explicit deferral:
  "Whether concrete syntax spells a move explicitly is deliberately unresolved."
- [Issue 08](../../../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md):
  resolves that deferral — `let name = expression` bindings and the `move name` spelling for
  consuming uses are part of the accepted surface.
- [Issue 02](../../../wayfinder/bootstrap-language/issues/02-bootstrap-type-system-and-values.md):
  "Initialized local bindings … may be inferred from their local declaration body; inference
  never consults callers or later statements."
- [Realignment — Track 3, proposal 7](../../../roadmaps/compiler-realignment.md): "Resolved:
  stay frozen; bindings/moves arrive with issue 08's syntax work" — this change is that
  arrival.
