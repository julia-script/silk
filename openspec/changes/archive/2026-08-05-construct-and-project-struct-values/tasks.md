## 1. Lossless struct-value syntax

- [x] 1.1 Add struct-literal, labeled-initializer, and field-projection node vocabulary to the concrete tree, public syntax APIs, visitors, and deterministic encoders.
- [x] 1.2 Parse empty and labeled struct literals in every expression position while retaining target paths, punctuation, trivia, initializer source order, and exact spans.
- [x] 1.3 Parse repeated postfix field projections above prefix, infix, equality, and pipeline precedence, and keep qualified calls distinct from value projection.
- [x] 1.4 Implement bounded recovery for missing literal targets, braces, labels, colons, initializer expressions, separators, projection subjects, and projection members.
- [x] 1.5 Add lossless parser fixtures for empty, reordered, nested, moved-field, chained-projection, qualified-call, and every recovery boundary.

## 2. Semantic construction and projection facts

- [x] 2.1 Add immutable literal facts that retain source initializer order, canonical nominal identity, defining-module authority, and per-initializer field lookup and type outcomes.
- [x] 2.2 Build the complete declaration-ordered initializer mapping only when every field is supplied exactly once with an available compatible value.
- [x] 2.3 Add stable diagnostics and explicit unavailable states for external raw construction, unknown, duplicate, missing, inaccessible, and mistyped fields without discarding independent failures.
- [x] 2.4 Add immutable projection-step facts for subject type, canonical field identity, visibility, declared result type, provenance, and causally unavailable outer steps.
- [x] 2.5 Diagnose non-struct projection, unknown fields, private external fields, and unavailable subjects without alternate lookup or fabricated value facts.
- [x] 2.6 Add semantic fixtures for reordered and empty literals, nested chains, cross-module factories, every invalid field-set combination, private access, and deterministic fact ordering.

## 3. Typed HIR and affine ownership

- [x] 3.1 Widen HIR contracts, expressions, bindings, calls, parameters, and results to the shared built-in-or-canonical-nominal `Type` vocabulary.
- [x] 3.2 Add canonical HIR construction with declaration-ordered typed initializers and canonical HIR projection with subject type, field identity, result type, access mode, and provenance.
- [x] 3.3 Preserve invalid construction as unavailable HIR and preserve consuming projected access for ownership to reject rather than partially elaborating either case.
- [x] 3.4 Replace the all-Copy ownership shortcut with explicit Copy scalar and move-only nominal classifications, requiring `move` for consuming transfers from bound nominal values.
- [x] 3.5 Track whole-value liveness per structured control-flow path, transfer ownership through lets, calls, and returns, and conservatively join branch states.
- [x] 3.6 Reject partial moves from projected fields while allowing non-consuming Copy scalar field reads to leave the complete owner live.
- [x] 3.7 Plan exact exit cleanup for live whole values: owners in reverse binding order and recursively owned fields in declaration-defined order, including explicit zero-action cleanup.
- [x] 3.8 Add HIR and ownership fixtures for fresh-value flow, explicit bound moves, implicit-copy refusal, use after move, branch-local ownership, scalar field reads, partial-move refusal, and nested cleanup.

## 4. Runtime reachability and compiler-owned aggregate ABI

- [x] 4.1 Extend instance discovery to follow canonical nominal types through reachable contracts, bindings, construction, projection, and cleanup while omitting unused declarations.
- [x] 4.2 Key aggregate-bearing instances by canonical nominal identity and recursively discover the nominal field types required at runtime in stable worklist order.
- [x] 4.3 Select the exact declaration-wide target layout catalog entries for reachable aggregate types and propagate original unavailable causes without recomputation.
- [x] 4.4 Add an immutable compiler-owned aggregate calling-shape actor whose lanes recursively flatten Copy scalar leaves by canonical declaration-order field paths.
- [x] 4.5 Represent empty structs as logical nominal values with zero lanes and encode nominal identities, paths, scalar representations, and lane order deterministically.
- [x] 4.6 Verify catalog/plan identity, recursive lane completeness, parameter/result shape agreement, and the absence of backend vocabulary or backend-selected ABI facts.
- [x] 4.7 Add discovery and target-plan fixtures for unused, equal-shaped nominally distinct, empty, nested, unavailable, target-varying, and fresh-process deterministic aggregates.

## 5. Aggregate MIR and evaluation

- [x] 5.1 Widen MIR locals and function contracts to canonical nominal logical types and associate each reachable nominal use with its selected layout and calling shape.
- [x] 5.2 Add explicit MIR construction and projection operations with canonical fields, declaration-ordered operands, typed destinations, and exact provenance.
- [x] 5.3 Lower whole-value moves, calls, returns, and ownership-planned drops over logical aggregate locals without exposing backend types or independently flattening values.
- [x] 5.4 Extend MIR verification for construction completeness, field membership and order, operand/result types, plan availability, whole-value linearity, and aggregate call-shape agreement.
- [x] 5.5 Extend textual MIR encoding with canonical nominal types, field paths, calling shapes, aggregate operations, and deterministic provenance ordering.
- [x] 5.6 Add immutable evaluator aggregate values in declaration order and implement construction, projection, whole moves, calls, returns, and cleanup without backend representation leakage.
- [x] 5.7 Validate aggregate calls and returns against the selected calling shape and emit compact deterministic construction, transfer, projection, and cleanup traces.
- [x] 5.8 Add MIR and evaluation fixtures for reordered, empty, nested, moved, called, returned, projected, cleanup-bearing, verifier-invalid, and repeat-encoding cases.

## 6. Native, WebAssembly, and driver realization

- [x] 6.1 Extend native function signatures, locals, calls, results, construction, projection, moves, and drops to realize the compiler-selected lane sequence and physical layout facts exactly.
- [x] 6.2 Extend direct WebAssembly signatures, scalar locals, multi-value internal results, calls, construction, projection, moves, and drops from the same selected lane sequence.
- [x] 6.3 Preserve zero-lane nominal contracts in both emitters and reject missing or contradictory aggregate plans with typed failures before creating partial artifacts.
- [x] 6.4 Remove scalar-only aggregate rejection paths and prove neither backend recalculates field order, offsets, padding, flattening, or indirect conventions.
- [x] 6.5 Thread aggregate declaration, ownership, reachability, layout, ABI, MIR, evaluation, and emission outcomes through driver phase reporting while keeping the host entry boundary scalar.
- [x] 6.6 Add driver corpus programs for public factories, cross-module transfers, nested and empty aggregates, reordered construction, projection, cleanup, invalid construction, and invalid ownership.
- [x] 6.7 Add native/Wasm/evaluator parity, stable-symbol, IR/WAT/binary determinism, and typed incompatible-plan tests on every supported target.

## 7. Facade and unified `/labs` workbench

- [x] 7.1 Extend Analysis snapshots with immutable literal, projection, nominal HIR, ownership, cleanup, aggregate reachability, catalog, calling-shape, MIR, evaluation, and codegen facts.
- [x] 7.2 Add public facade queries and explicit package exports that preserve canonical identities and prevent tooling from reconstructing completeness, lookup, ownership, or lane order.
- [x] 7.3 Extend the unified `/labs` registry and coordinated panes for struct-value syntax, source/canonical field mappings, projection chains, ownership, layout/ABI, MIR, traces, and both artifacts.
- [x] 7.4 Add accessible text equivalents and browser-local presets for every valid, invalid, empty, reordered, nested, cross-module, move, projection, visibility, and partial-move scenario.
- [x] 7.5 Extend facade-only import-boundary, immutable-answer, reload-state, and fresh-process deterministic inspector tests; do not introduce or extend a standalone legacy lab.

## 8. Verification and release gates

- [x] 8.1 Run focused syntax, semantic, HIR, ownership, discovery, layout, MIR, evaluator, backend, driver, facade, and labs suites as each layer lands.
- [x] 8.2 Run fresh-process determinism and three-engine parity over the complete aggregate corpus, including zero-lane and invalid-plan cases.
- [x] 8.3 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check` in repository order and resolve every change-owned failure.
- [x] 8.4 Run `pnpm release:candidate` because public data models and package exports change.
- [x] 8.5 Run `openspec validate construct-and-project-struct-values --strict` and review every normative scenario against implementation evidence before handoff.
