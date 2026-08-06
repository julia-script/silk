## Context

Structural unions already give Silk a canonical set of nominal members, a compiler-owned layout,
and explicit widening operations. The missing inverse operation is a match that can select the active
member, expose its payload at the right ownership mode, and join the selected arm back into an
expression value.

This change crosses every compiler representation. The parser must preserve the source spelling,
semantic analysis must prove coverage and type the arm-local bindings, ownership must distinguish
inspection from transfer, and both backends must realize dispatch without leaking numeric tags or
backend control into compiler-owned relationships. HIR and MIR remain DAGs: a backend may flatten a
verified match into its preferred control shape, but the compiler will not require another backend
to reconstruct structured control from a CFG.

Silk does not yet have general borrow expressions or borrowed function contracts. The `&` and
`&mut` modes introduced here are therefore deliberately match-local views whose lifetime ends with
the selected arm. They establish the ownership semantics needed by matching without prematurely
choosing the language's eventual general reference model.

## Goals / Non-Goals

**Goals:**

- Parse lossless match expressions in every expression position, including access modes, nominal
  patterns, nested field bindings, explicit rest patterns, guards, and a universal arm.
- Diagnose exhaustive, duplicate, unreachable, and guard-dependent coverage deterministically from
  canonical nominal member identities.
- Give each reachable arm a precisely narrowed scrutinee and scoped pattern bindings.
- Preserve affine ownership across Copy, consuming, shared, and exclusive matching, including exact
  cleanup of omitted fields and guard fallthrough.
- Join arm results canonically, with explicit conversions when nominal members widen to a structural
  union.
- Carry matches through acyclic HIR and MIR regions and execute them consistently in the evaluator,
  native LLVM backend, and direct WebAssembly backend.
- Publish enough immutable facts and provenance for the formatter, language integrations, analysis
  facade, and unified `/labs` inspector to consume rather than reconstruct.

**Non-Goals:**

- General borrow expressions, reference types, borrowed parameters, or borrowed return values.
- Array, literal, range, alternative, or user-extensible patterns.
- Typed failure handling or catch patterns.
- A public numeric union-tag ABI or backend-specific control graph in semantic facts, HIR, or MIR.
- Compatibility with source that previously used `match` as an identifier.

## Decisions

### Match is a primary expression with an explicit access mode

The syntax parser will treat `match` as a primary expression so a match can appear wherever any
other expression can. It evaluates its scrutinee exactly once, then selects one arm and produces
that arm's expression result. The access spelling immediately follows `match`:

- bare `match value` is permitted only when the scrutinee is Copy;
- `match move value` consumes the whole scrutinee;
- `match &value` creates a shared match-local view; and
- `match &mut value` creates an exclusive match-local view and requires a mutable live place.

Access is recorded independently from the scrutinee's semantic type. In particular, `&` does not
manufacture a general `Borrow<T>` type that could escape the arm.

### Patterns resolve to canonical nominal members and field paths

Semantic analysis resolves every nominal pattern to a canonical member identity. A nominal
scrutinee is treated as a one-member set; a structural union contributes its normalized member set.
Pattern fields retain source order for diagnostics and tooling while also carrying canonical field
identities for typing, ownership, and lowering.

`field` binds the field under its own name, `field: localName` renames it, and nested nominal
patterns extend the canonical field path. Omitting fields is legal only when the pattern includes
an explicit `..`. `_` is the universal pattern and introduces no payload binding. Pattern bindings
have arm-local scope and may not shadow one another in the same pattern tree.

This dual source/canonical representation keeps formatting and selection faithful without making
coverage or ownership depend on source spelling.

### Coverage is an ordered fold over a canonical remaining-member set

Coverage checking starts with the scrutinee's canonical member set and visits arms in source order.
An unguarded nominal arm subtracts its member. A guarded arm can be selected at runtime but proves
no coverage and therefore does not subtract. An unguarded `_` arm covers the remaining set and must
be terminal.

An arm is rejected when its member is not in the scrutinee set, when a previous unguarded arm has
already exhausted that member, or when universal coverage has already been reached. This also
rejects a guarded arm placed after an exhaustive arm for the same member: at that point there is no
remaining value for it to observe. A match is exhaustive only when the remaining set is empty.

The fold publishes per-arm before/after sets and final missing members. Diagnostics are ordered by
source location and print canonical member names, making results independent of hash or discovery
order.

### Guard evaluation is provisional; ownership commits only after selection

Pattern matching may expose candidate bindings to a guard, but the guard is an inspection phase.
It cannot consume a pattern binding or perform the final transfer from a moved scrutinee. If the
guard is false, no ownership transfer or cleanup has occurred and later arms may safely inspect the
same candidate value.

Once an arm is selected, ownership commits according to the match mode:

- Copy mode copies bound values and leaves the original live.
- Move mode transfers bound fields, cleans omitted owned fields exactly once, and consumes the whole
  scrutinee.
- Shared mode exposes read-only place views and forbids moves or mutation through them.
- Exclusive mode exposes mutable place views for the selected arm and restores availability of the
  original owner when the arm ends.

Borrowed bindings cannot escape through the arm result, storage, closure capture, or a call contract.
This is checked as a match-local lifetime rule rather than a general reference-type rule.

### Arm results use one canonical join operation

Semantic analysis elaborates each reachable arm result precisely. `Never` contributes no result
member. If every contributing arm has the same type, the match keeps that type. Otherwise, if every
result is nominal or already a structural union, their members are normalized through the existing
union constructor. Other mixtures, including incompatible scalars or aggregates, are rejected.

When the joined type is a structural union, HIR inserts an explicit `UnionConvert` at each arm whose
result must widen. The conversion records a `MatchArm` provenance context so later phases and tools
do not need to rediscover why widening occurred. Keeping this as the sole join algorithm prevents
semantic analysis, HIR, and MIR from developing subtly different result rules.

### HIR and MIR encode a structured acyclic match region

HIR will represent a match expression with one evaluated scrutinee, its access mode, ordered arm
regions, canonical pattern and coverage facts, optional guard expressions, and one result join. Arm
regions can refer to values defined by their ancestors but never back to a later arm or join.

MIR lowering will preserve the same DAG shape as a structured selection region: dispatch by
canonical member, candidate binding and guard evaluation, selection-time ownership operations, the
arm body, and a single join destination. Nested matches lower to nested regions whose result is an
ordinary local, so they remain valid in any expression position. Verification rejects cyclic region
references, missing or duplicate members, invalid guard edges, inconsistent joins, and cleanup that
does not agree with the selected access mode.

Neither representation exposes numeric tags, LLVM basic blocks, WebAssembly labels, or a general
compiler CFG. Deterministic encoders traverse canonical member order while retaining source arm
order where it affects behavior.

### Existing union layout remains the only runtime layout authority

Instance discovery follows pattern payloads, guard and arm types, join conversions, and cleanup
obligations, but matching creates no new layout family. Dispatch uses the existing union sum shape,
member ordinals, payload offsets, and calling shapes. A nominal value uses the same logical
one-member treatment without forcing an artificial tagged allocation.

The evaluator selects by logical active-member identity and projects the logical payload. Native
LLVM privately lowers a verified match to basic blocks and switches or comparisons. The direct
WebAssembly backend privately chooses structured blocks, `if`, or `br_table` based on the verified
layout. Those choices are backend implementation details; neither backend feeds a flattened graph
back into compiler-owned MIR.

### Tooling consumes published match facts

The analysis facade exposes immutable match, pattern, coverage, binding, narrowing, join, and
cross-phase provenance facts. The unified `/labs` inspector adds coordinated syntax, semantic, HIR,
MIR, evaluation, layout, and backend views for match presets; it does not introduce a separate match
inspector. Formatter and highlighting integrations consume the accepted syntax vocabulary and
remain usable on recovered trees.

The compiler-driver corpus exercises the same valid, invalid, trapping, ownership, and determinism
cases through the evaluator, native LLVM, and direct WebAssembly engines.

## Risks / Trade-offs

- **Match-local views may accidentally become an implicit general borrow system.** Access-qualified
  place views are kept internal to pattern facts and ownership regions, are non-storable, and are
  forbidden from escaping an arm.
- **Guards can create double-move or early-cleanup bugs.** Guard evaluation is explicitly
  non-consuming, and transfer/cleanup operations exist only on the selected-arm path after a true
  guard.
- **Structured regions could drift into an ad hoc cyclic CFG.** HIR and MIR verifiers enforce
  ancestor-only references and one forward join, while backends own any flattening they require.
- **Result inference could differ across phases.** One semantic join fact and explicit
  `UnionConvert` nodes are authoritative for HIR, MIR, evaluation, and backends.
- **Backend dispatch could diverge despite shared semantics.** Layout remains compiler-owned and the
  three-engine differential corpus compares results, traps, diagnostics, and cleanup behavior.
- **Large unions and many guards may expand lowering work.** Coverage uses normalized member sets and
  lowering shares member dispatch before evaluating source-ordered guards for that member.

## Migration Plan

1. Reserve the new token vocabulary and add lossless syntax, recovery, formatting, and highlighting.
2. Add pattern, coverage, narrowing, access, and result-join facts without enabling backend emission.
3. Introduce verified HIR and MIR match regions plus evaluator execution and ownership checks.
4. Lower verified matches in native LLVM and direct WebAssembly using the existing union layout.
5. Expose the facts through the facade and unified `/labs`, then enable the complete differential
   and fresh-process corpus.
6. Run the repository and release-candidate gates because public compiler representations, exports,
   and packaged language integrations may change.

There is no persisted-data migration. Rollback is a source-compatible revert only for programs that
do not use the newly reserved `match` keyword; no compatibility shim will be maintained during the
alpha stage.
