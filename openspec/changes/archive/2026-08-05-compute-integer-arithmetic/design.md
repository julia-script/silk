## Context

Second slice-1 change: signed literals and trapping integer arithmetic as qualified built-in
actor operations (`I32.add(40, 2)`), per issue 08's no-infix surface and issue 02's
trap-on-overflow semantics.

## Goals / Non-Goals

- Goals: minus and dot tokens, signed literals with full `I32` range, qualified callees, the
  built-in `I32` actor (`add`, `subtract`, `multiply`, `divide`, `remainder`), HIR builtin calls,
  a trapping MIR binary operation, exact interpreter semantics, checked LLVM emission, corpus
  trap parity, lab coverage.
- Non-goals: infix operators (not in the accepted surface), pipe insertion (`|>`), checked/
  wrapping/saturating variants, comparisons and `Bool` (next change), unary minus on non-literals,
  user-defined actors.

## Decisions

### Built-ins live in a compiler-known actor table, not the declaration index

**Deviation from the proposal**, which said built-ins would be "visible to the declaration
index." The index is source-declaration truth: built-ins have no source module, no syntax, no
canonical declaration id, and indexing them would force fake modules through instance discovery
and backend symbols. Instead elaboration resolves qualified callees against a closed table and
produces a dedicated HIR `BuiltinCall`; discovery and the backend never see them as functions.
Issue 07's runtime actors will revisit whether a richer registry is needed.

### MIR gets a `Binary` operation with trapping semantics, not pre-expanded diamonds

**Deviation from the proposal**, which imagined overflow checks lowered to explicit MIR
branches. Expanding at MIR level would need overflow-predicate arithmetic spelled out per
operation, tripling every arithmetic block for no consumer benefit — both consumers (interpreter,
backend) can implement trapping directly and correctly. So: MIR `Binary` traps by definition
(overflow, `/0`, `MIN/-1`); the interpreter checks exact ranges; the backend expands to
`s{add,sub,mul}.with.overflow` + conditional branches to a per-function trapping block, and
guards division explicitly. The checked expansion is visible in the LLVM IR lab rather than the
MIR lab. Division truncates toward zero, remainder takes the dividend's sign (LLVM `sdiv`/`srem`
semantics, matching issue 02's conservative mapping).

### Diagnostics

`SEM0009` unknown actor, `SEM0010` unknown actor operation. `SEM0002` keeps its code with the
range widened to signed `I32` (reason data gains the minimum). Wrong built-in arity reuses the
positional-contract machinery (`SEM0007`-style checking against the built-in's two-parameter
contract).

### Syntax shapes

Signed literal: minus token + decimal token inside `IntegerLiteralExpression`, only when the
minus directly precedes a literal in expression position. Qualified callee: actor identifier,
dot, operation identifier as direct children of `CallExpression` before the argument list; bare
callees keep today's shape.

## Risks / Trade-offs

- Trapping semantics hidden inside one MIR op → pinned in the spec and enforced by the
  differential corpus (overflow, div-zero programs run natively).
- Per-function trapping block in the backend adds LLVM blocks not present in MIR → they carry
  the causing operation's provenance; the MIR↔IR correspondence stays readable.
- `-2147483648` parses as minus + `2147483648` where the bare digits alone would be out of range
  → the signed value computes from the sign and digits together before range checking.

## Migration Plan

Purely additive. Existing goldens unchanged; new fixtures gain their own goldens.

## Open Questions

None — both proposal-level questions (MIR representation, built-in home) are decided above.
