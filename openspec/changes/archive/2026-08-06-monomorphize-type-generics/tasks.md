## 1. Generic syntax and source tooling

- [x] 1.1 Add lossless type-parameter lists, generic type applications, and explicit call-specialization nodes to the compiler syntax actors and deterministic encoders.
- [x] 1.2 Parse declaration and type-position angles contextually, and parse expression specialization only as a complete type list followed by the call postfix.
- [x] 1.3 Add focused parser and recovery fixtures covering nested applications, qualified callees, comparisons, reserved template starts, missing arguments, and missing closing angles.
- [x] 1.4 Extend Silk source formatting with idempotent generic declaration/application output and damaged-syntax preservation tests.
- [x] 1.5 Extend CodeMirror, TextMate, and generated VS Code fixtures so generic angles remain distinct from comparisons and reserved templates.

## 2. Canonical generic types and semantic facts

- [x] 2.1 Extend the `Type` actor with declaration-owned parameter identities, applied nominal arguments, recursive keys/equality/encoding, substitution, parameter collection, and concrete-type validation.
- [x] 2.2 Add ordered canonical type parameters to struct and function declaration facts, including duplicate, unbound, non-generic-application, missing-application, and arity diagnostics.
- [x] 2.3 Resolve generic struct fields, function contracts, nested fixed arrays, imports, and qualified applications through declaration-local type environments without textual identity.
- [x] 2.4 Implement complete explicit call specialization and all-argument inference from supplied values only, followed by the existing substituted argument-compatibility checks.
- [x] 2.5 Add semantic fixtures for inferred, explicit, conflicting, missing, excess, return-only, nested-structure, and cross-module generic calls.
- [x] 2.6 Publish deterministic type-parameter, applied-type, inference-constraint, substitution, specialization, unavailable-cause, and source-provenance facts.

## 3. Generic-aware HIR and ownership

- [x] 3.1 Extend HIR declarations and calls with canonical type parameters and ordered type arguments while retaining one body per source declaration.
- [x] 3.2 Encode generic HIR deterministically and add provenance tests showing multiple calls share one checked declaration body.
- [x] 3.3 Check open parameters conservatively as move-only and potentially cleanup-bearing, rejecting unconstrained Copy- or nominal-specific operations before specialization.
- [x] 3.4 Represent compiler-owned Copy and cleanup properties symbolically in generic ownership facts and substitute them without re-elaborating concrete bodies.
- [x] 3.5 Add ownership and cleanup fixtures spanning generic whole moves, concrete Copy instances, move-only nominal instances, and rejected concrete-only behavior.

## 4. Finite instance discovery and target layout

- [x] 4.1 Change `InstanceKey.typeArguments` to ordered canonical semantic types and include recursive type keys in equality, ordering, encodings, and deterministic symbols.
- [x] 4.2 Carry each discovered instance's substitution, substitute generic call targets while walking HIR, and record a concrete key before following its dependencies.
- [x] 4.3 Reject parameter-changing recursive generic calls and add direct, mutual, same-argument, unused-specialization, and polymorphic-expansion fixtures.
- [x] 4.4 Derive concrete applied-struct field catalogs and reachable runtime layouts from substitutions while retaining open generic declarations without speculative physical layout.
- [x] 4.5 Add per-target layout and fresh-process goldens for repeated, nested, unused, and layout-distinct generic nominal applications.

## 5. Monomorphic MIR and logical evaluation

- [x] 5.1 Add one specialization view that substitutes contracts, expressions, locals, calls, aggregate types, and cleanup facts before MIR lowering.
- [x] 5.2 Key MIR functions and calls by concrete instance identity, retain generic declaration/type-argument provenance, and reject every residual open parameter or missing layout.
- [x] 5.3 Extend MIR verification, deterministic encoding, and structured-DAG goldens with inferred and explicit concrete specializations.
- [x] 5.4 Execute concrete generic-origin functions and nominal values in the evaluator using only MIR instance and layout facts, with no interpreter-owned generic representation.
- [x] 5.5 Add bounded deterministic traces and result fixtures for scalar, nominal, nested, recursive, and move-only concrete instances.

## 6. Native, WebAssembly, and differential acceptance

- [x] 6.1 Derive deterministic specialization symbols from canonical instance keys and emit distinct LLVM definitions/calls using compiler-selected layouts and calling shapes.
- [x] 6.2 Emit the same concrete specialization set through direct WebAssembly with deterministic functions/calls and no runtime type descriptors or backend-owned layout.
- [x] 6.3 Add driver corpus programs covering inference, explicit specialization, two layouts from one declaration, same-argument recursion, invalid arity/inference, and generic syntax ambiguity.
- [x] 6.4 Gate evaluator/native/WebAssembly parity and fresh-process determinism for every valid generic corpus program, and prove invalid specializations stop before instances, layout, and MIR.

## 7. Analysis, labs, and decision records

- [x] 7.1 Extend the immutable analysis facade with canonical queries linking generic syntax, parameters, applications, substitutions, instances, ownership, layouts, MIR, and diagnostics.
- [x] 7.2 Add coordinated generic facts, rows, selections, diagnostics, and presets to the unified `/labs` workbench without a standalone inspector or browser-side specialization.
- [x] 7.3 Update deterministic inspector/facade encodings and fixtures so one selected call traces through its concrete specialization across every available phase.
- [x] 7.4 Amend the Wayfinder syntax/type/pipeline decisions with the accepted angle grammar, full-explicit-or-all-inferred rule, conservative one-time checking, and pre-MIR monomorphization.
- [x] 7.5 Update the project roadmap with the reviewed dependency chain: lexical runtime slices, scoped allocation and typed slots/drop hooks, Silk-written Vector/scanner acceptance, then bulk byte memory.

## 8. Verification

- [x] 8.1 Run focused compiler, formatter, language-tooling, facade, labs, evaluator, LLVM, WebAssembly, and driver tests while updating only intentional deterministic goldens.
- [x] 8.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`, documenting any pre-existing failure exactly.
- [x] 8.3 Run `pnpm check` and `pnpm release:candidate` because compiler/tooling package contents and exported type identities change.
- [x] 8.4 Run `openspec validate monomorphize-type-generics --strict` and confirm every proposal task and delta capability is satisfied before handoff.
