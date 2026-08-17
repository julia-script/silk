## Why

Generic contract-row decomposition currently commits to the first canonical match, so an ordinary
Silk provider combinator can remove the wrong requirement while `Intrinsic.bindRequirement`
produces the expected remainder through a separate, operation-specific typing path. Contract rows
need explicit algebra and one constraint/evidence system shared by source declarations and sealed
intrinsics.

## What Changes

- Add a first-class, kind-preserving `Without<R, S>` expression for total failure-row and
  requirement-row difference, including set-to-set removal.
- Treat `Without` as a forward-computed row expression rather than an invertible inference pattern:
  its operands must be bound independently before it is reduced.
- Model failure rows as finite nominal sets and requirement rows as finite capability-role keyed
  rows whose access label has explicit union, membership, subset, and exact-difference rules.
- Preserve symbolic row expressions—including distinct row parameters and lifted ordinary member
  parameters—through generic analysis, renormalize after substitution, use definitional equality
  for open expressions, and require concrete extensional rows before any row-dependent instance
  phase.
- Replace first-match decomposition with constraint solving that distinguishes absence, uniqueness,
  and ambiguity without using canonical row order as evidence.
- Add checked membership, subset, and provider-selection constraints. Declaration constraints are
  assumptions inside generic bodies and obligations at applications, with symbolic evidence
  forwarded through generic HIR and resolved during instance specialization.
- Allow a kind-correct contiguous explicit generic prefix, including failure- and requirement-row
  arguments, while inferring only the remaining suffix from supplied values and constraints.
- Express shared, exclusive, and owned provider selection as separate source-spellable parameter
  modes over the same relation. Each mode yields the exact singleton requirement row `S`, including
  its stored access and role, and places `S` first for ordinary positional generic calls.
- Give the sealed requirement-binding operations canonical callable contracts using the same
  generic binders, constraints, capture-access calculation, `Without` result, and solver used by
  equivalent ordinary Silk functions. Generic binding HIR may carry assumed proof evidence; the
  intrinsic post-contract hook retains only mode-appropriate place validation and HIR construction.
- Rewrite `Effect.bindRequirement`, `Effect.provide`, `Effect.provideMut`, and acquisition-based
  provision around the whole input row, selected row, and `Without`.
- Migrate singleton-nominal failure handling to a sealed canonical contract with checked membership
  plus `Without`, eliminating independent row filtering and result-type reconstruction while making
  the resulting selective dispatch executable on the evaluator, WebAssembly, and native targets.
- Keep public `Without` syntax scoped to failure-row and requirement-row positions; structural value
  unions share deterministic finite-row infrastructure without gaining a public subtraction form.
- **BREAKING**: Remove the intrinsic-only role-filtering call form in favor of explicitly selecting
  the complete requirement row member through ordinary generic arguments and constraints.
- **BREAKING**: Reserve `Effect.catch` for singleton-nominal selection, inferred from its handler or
  supplied as `<S>`, and remove the whole-row `Effect.catch` alias; whole-row recovery uses
  `Effect.catchAll`.
- **BREAKING**: Replace flat member-plus-forwarded-parameter contract rows and their serialized
  compiler surfaces with symbolic row expressions and constraint evidence.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-type-generics`: Define forward-only symbolic row computation, access-aware requirement
  algebra, checked constraints, generic givens/wanteds, evidence forwarding, and early instance
  concretization.
- `bootstrap-flow-functions`: Define fixed-mode provider selection, uniform intrinsic/source binding,
  provision variants, public binding wrappers, and singleton failure handling through `Without`.
- `bootstrap-syntax`: Parse contextual row difference, row generic arguments, and checked
  declaration constraints losslessly and recoverably.
- `silk-source-formatting`: Give row algebra and constraint lists one deterministic canonical layout.
- `bootstrap-semantic-facts`: Retain source-shaped row expressions, constraints, substitutions, and
  evidence identities in semantic facts.
- `bootstrap-module-semantic-surface`: Encode row contracts and neutral provider evidence
  deterministically across module boundaries.
- `bootstrap-intrinsic-boundary`: Derive intrinsic inventory and admission from canonical callable
  contracts and proof-consuming hooks.
- `bootstrap-intrinsic-target-availability`: Keep selective failure handling in the ordinary
  executable intrinsic inventory on every supported target, with no temporary availability error.
- `bootstrap-diagnostics`: Add stable row/constraint diagnostics with span-free semantic payloads
  and separate ordered source locations.
- `bootstrap-hir`: Carry symbolic row expressions and constraint evidence before specialization.
- `bootstrap-instances`: Establish the concrete row/evidence frontier before reachable-instance
  discovery and row-dependent consumers.
- `bootstrap-mir`: Require fully concrete row contracts and provider evidence before MIR lowering.
- `bootstrap-silk-stdlib`: Express binding, provision, acquisition, and singleton catch as ordinary
  Silk wrappers over the minimum sealed primitives.
- `silk-standard-library-documentation`: Document `Without`, exact provider selection, fixed modes,
  and singleton-versus-whole-row failure handling.

The change also affects observable syntax, semantic facts, intrinsic inventory, diagnostics,
module-surface serialization, specialization, HIR, MIR, formatting, and standard-library
documentation. Dedicated delta specs define each affected capability in this change.

## Impact

- Syntax and tooling: contextual row expressions and constraints, parser recovery, formatting,
  semantic occurrences, navigation, presentation, and signature help.
- Semantic model: keyed row normalization, access algebra, symbolic substitution, definitional and
  concrete equality, constraints, evidence, diagnostics, and module-surface serialization.
- Call analysis: one origin-independent callable contract model, exact given/wanted entailment,
  residual constraints carried by partial callable values, and typed provider-conformance lookup.
- Intrinsic boundary: inventory rendering and semantic admission derive from the same contract;
  binding loses its early custom typing path, and singleton catch lowers from the same specialized
  concrete contract on the evaluator, WebAssembly, and native targets.
- Specialization: symbolic rows and evidence are resolved before dependency discovery, witness
  reachability, layout, row-dependent ownership, or lowering; MIR and backends remain concrete-only.
- Standard library: binding, provision, acquisition, and exact failure-handling declarations plus
  generated source and reference documentation.
- Verification: algebra laws, generic-body entailment, access and role matrices, partial application,
  structural single-path checks, module-surface determinism, and the Logger/Clock regression.
