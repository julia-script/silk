## Context

See `proposal.md` for motivation. HIR already represents the failing source faithfully as
`Run(EffectTransform(Map, EffectProvide(...)))`. Lowering, however, has separate paths for a
runtime `EffectValue`, a stored transform, and several execution-only recipes. The transform path
tries to lower its protected expression as a runtime value; provision is only understood as a
recipe beneath `Run`, so the composition returns unavailable after reserving a region. Direct
effectful entries then lose their generated runner, while stored transforms publish a trap.

## Goals / Non-Goals

**Goals:**

- Give `run` one recursive lowering model for all valid nested Effect recipes.
- Preserve the language's construction-versus-execution timing, capture access, provider loan, and
  cleanup semantics.
- Exercise the public source boundary with a compact pairwise pipeline matrix and real lexer
pressure rather than tests of lowering helpers.

## Implementation outcome

The public behavior and evidence in this design shipped, but the planned recursive privileged
recipe mechanism did not become the final architecture. While implementing the matrix, the
follow-up `make-effects-library-definable` change made Effects ordinary higher-order values and
moved `map`, `flatMap`, `tap`, `catch`, `retry`, `provide`, and `provideWith` into visible Silk
source over `Effect.result` and `Effect.bindRequirement`. Consequently, composed pipelines now
lower through ordinary calls and the two closed core operations; the former stored-recipe
bookkeeping, combinator HIR/MIR operations, and fallback traps were removed entirely. The
requirements below remain the accepted observable contract, while the mechanism described in the
original decisions is retained only as the migration history that exposed the deeper seam.
- Make incomplete valid lowering an explicit compiler invariant failure during development, never a
  generated program behavior.

**Non-Goals:**

- Adding new Effect operators, syntax, scheduling, fibers, or ambient services.
- Materializing every combinator as a universal runtime interpreter object.
- Promising identical MIR text for intentionally different source shapes; observable semantics and
  per-source determinism are the contract.

## Decisions

### Lower execution recipes recursively

Extract the recipe-specific logic currently embedded in `Run` into one recursive lowering path.
The path accepts either a runtime Effect value or an HIR recipe and returns the successful local
after emitting the necessary runner invocation, callback application, provider handling, failure
mapping, and cleanup. A transform recursively executes its protected recipe; provision augments
the available requirement set around recursive execution; recovery, retry, and acquisition retain
their specialized control but delegate their protected portions to the same path.

This is preferred over adding a special `map(provide(...))` branch because the same asymmetry can
appear between every neighboring combinator pair. It is also preferred over a universal runtime
Effect node because Silk's hidden construction-site runners keep dispatch and allocation costs
static and pay-for-use.

### Preserve construction-time state for stored pipelines

Stored recipe metadata SHALL retain already-evaluated callback/provider captures or references as
required by the HIR ownership facts, while recursive execution consumes that stored state later.
Binding a composition must not defer eager callable construction, extend a lexical loan beyond its
accepted lifetime, or turn unavailable lowering into a trap.

This extends the existing stored recipe/transform bookkeeping rather than normalizing source by
duplicating or reordering expressions, which could violate left-to-right pipeline evaluation.

### Test pairwise composition at public seams

The stress suite will use complete Silk programs and the normal compiler pipeline. A small
pairwise matrix covers every operator individually and representative adjacent combinations in
both orders, with direct, grouped, and stored forms where lowering differs. Selected cases then run
through evaluator, native, and Wasm; invalid cases assert source diagnostics. Existing pressure
programs provide affine allocation and fresh-process evidence without inventing synthetic runtime
types.

This is preferred over a Cartesian product: it detects dispatch seams while keeping the suite
readable and bounded.

### Treat fallback traps as compiler defects for valid HIR

Lowering may still produce explicit traps for source-defined abnormal termination, but it SHALL
not silently compile an unavailable valid Effect recipe into a trap. The public compilation path
must report source invalidity before MIR or complete the lowering.

## Risks / Trade-offs

- **[Risk]** Recursive lowering changes provider and callback evaluation order. **Mitigation:** add
  order-sensitive fixtures and compare direct, grouped, and stored forms.
- **[Risk]** Stored affine callbacks or providers are duplicated. **Mitigation:** include consuming
  mapper and allocation-cleanup cases with ownership rejection on reuse.
- **[Risk]** The pairwise matrix becomes slow across three engines. **Mitigation:** run the full
  matrix through analysis/evaluation and select boundary representatives for native/Wasm and fresh
  processes.
- **[Risk]** Fixing one lowering path leaves evaluator/backend assumptions inconsistent.
  **Mitigation:** assert MIR verification and differential outcomes before migrating examples.

## Migration Plan

Land the compiler behavior and regression matrix together, then migrate representative examples to
the newly accepted pipeline shape. No user-source migration is required because the change makes
already-valid source executable. Rollback is the single change commit if differential gates expose
an unanticipated semantic difference.
