## 1. Syntax, Recovery, and Formatting

- [x] 1.1 Add the complete-identifier `union` token to lexical, token-presentation, and generated token consumers, and verify lexer tests distinguish `union` from identifier prefixes.
- [x] 1.2 Add lossless CST nodes for union declarations, unit variants, named-field variants, and parent-qualified variant selectors, and verify syntax snapshots retain trivia, separators, fields, and exact spans.
- [x] 1.3 Implement parser entry points for generic union declarations, constructor qualifiers with explicit argument prefixes, and fully applied variant patterns, and verify focused parser tests cover valid mixed variants and reject empty named-field bodies.
- [x] 1.4 Implement variant-local parser recovery for missing names, types, separators, and braces, and verify damaged-variant tests preserve valid siblings and following declarations.
- [x] 1.5 Extend the formatter, syntax correspondence, and source presentation for union declarations, constructors, and patterns, and verify formatting is idempotent and preserves comments.
- [x] 1.6 Add stable structured diagnostic catalog entries for union-specific syntax and semantic failures, regenerate catalog artifacts, and verify diagnostic tests assert codes, spans, related spans, and details rather than message text.

## 2. Canonical Declarations and Module Surfaces

- [x] 2.1 Add canonical `UnionFact`, subordinate variant identities, and variant-scoped field ownership while generalizing shared field facts away from struct-only owners, and verify identity tests distinguish same-spelled variants and fields under different parents.
- [x] 2.2 Collect unions in the ordinary cross-kind module namespace with parent parameters and source-ordered variants before bodies, and verify forward declarations, duplicates, empty unions, and cross-kind collisions in declaration-index tests.
- [x] 2.3 Resolve every variant field type, visibility exposure, generic reference, and inline aggregate dependency before body analysis, and verify invalid fields preserve sibling facts while making the complete parent non-executable.
- [x] 2.4 Encode union declarations in deterministic module semantic surfaces, and verify encode/decode, equality, and dependency invalidation respond to variant order, kind, field, type, visibility, bound, and availability changes but ignore body-only edits.
- [x] 2.5 Extend semantic occurrence, navigation, completion, documentation, and Analysis facade queries for parent, variant, and field identities, and verify go-to-definition/reference tests use canonical facts rather than syntax reconstruction.

## 3. Type Application and Variant Construction

- [x] 3.1 Teach nominal-type lookup and substitution to distinguish union declarations while keeping the complete parent application as the only value type, and verify no variant type or structural member is created.
- [x] 3.2 Refactor struct-literal field checking into a shared aggregate-field elaborator without changing existing struct facts or diagnostics, and verify the existing struct construction and generic-inference suites remain byte-for-byte stable where golden data exists.
- [x] 3.3 Implement two-stage variant constructor resolution—parent declaration and explicit prefix first, field-only suffix inference second—and verify zero-prefix, partial-prefix, conflicting, and parent-only uninferred argument cases.
- [x] 3.4 Implement unit and named-field construction with complete field initialization, construction authority, visibility fences, type compatibility, represented fields, and precise parent result types, and verify cross-module private fields block raw construction.
- [x] 3.5 Preserve every variant through generic specialization, including equal and `never` payloads while independently renormalizing structural-union fields, and verify specialization facts never collapse or flatten variants.
- [x] 3.6 Reject direct parent-union field projection and common-field synthesis while retaining diagnostic facts, and verify `result.value` is unavailable until a variant pattern binds its payload.
- [x] 3.7 Admit interface, operator, Copy, and Drop declarations against nominal union parents through the ordinary conformance/coherence path, and verify variant names do not become lookup or implementation targets.

## 4. Variant Patterns and Hierarchical Coverage

- [x] 4.1 Extend the shared pattern representation with fully applied variant selectors and struct-like named-field bindings, omissions, nesting, borrows, moves, and writes, and verify unit and payload pattern diagnostics match struct field policy.
- [x] 4.2 Replace flat match coverage keys with canonical selection paths that retain structural roots, applied nominal parents, and variants, and verify ordinary structural-union and scalar-enum coverage behavior remains unchanged.
- [x] 4.3 Implement direct variant subtraction through structural-union roots plus whole-parent subtree subtraction, and verify exhaustive, duplicate, unreachable, wildcard, and fully qualified missing-path diagnostics.
- [x] 4.4 Preserve nominal union roots as atomic `A | B` members during injection, widening, normalization, pattern selection, and specialization, and verify matching a leaf never changes the structural member set.
- [x] 4.5 Keep guarded affine variant selections provisional until guard success, and verify a false guard leaves both tag levels, complete payload ownership, and cleanup available to a later arm.
- [x] 4.6 Cover generic and uninhabited cases, and verify `Option<i32> | Option<bool>` retains distinct fully applied paths and `Result<A, never>.Failure` remains a required coverage leaf without becoming constructible.

## 5. Ownership, Represented Fields, and Cleanup

- [x] 5.1 Apply affine-by-default ownership and explicit Copy validation across every specialized variant field, and verify all-Copy payloads remain affine without `impl Copy` while one affine field rejects the implementation.
- [x] 5.2 Build active-variant cleanup plans that reuse nominal Drop ordering and clean only initialized fields of the selected variant, and verify success, typed-failure, and ordinary scope exits release each owned payload exactly once.
- [x] 5.3 Implement moved and borrowed variant-pattern ownership, including branch-local cleanup of omitted fields and rejection of invalid partial moves, and verify extracted and omitted fields have one final owner.
- [x] 5.4 Realize callable-bounded fields only inside the active variant using exact static callable storage and access rules, and verify unsupported representations retain the pre-MIR storage fence.
- [x] 5.5 Realize Effect-bounded fields only inside the active variant with lazy runner, environment, suspension, access, and cleanup facts, and verify unsupported shapes retain the pre-MIR storage fence.

## 6. Target Layout and Calling Shapes

- [x] 6.1 Extend the inline dependency graph and nominal layout catalog to include complete non-generic unions and mixed struct/union cycles, and verify unused private and unavailable union entries appear before runtime reachability.
- [x] 6.2 Add a distinct nominal-union representation plan with deterministic private tags, source-order ordinals, payload offset, maximum size/alignment, total padding, and per-variant aggregate layouts, and verify unit, padded multi-field, and `never` payload cases.
- [x] 6.3 Specialize reachable generic union layouts without speculative open-generic entries, and verify equivalent concrete applications reuse one catalog identity while distinct applications receive distinct physical plans.
- [x] 6.4 Publish a backend-neutral tag-plus-payload calling shape with complete per-variant logical-field mappings, and verify call/return plans for heterogeneous variants are deterministic and unavailable dependencies stop before MIR.
- [x] 6.5 Extend layout encoding, verification, and Analysis projections with nominal-union facts under unambiguous internal names, and verify no nominal tag, padding, or ABI detail becomes source-observable.

## 7. HIR, MIR, and Verification

- [x] 7.1 Add explicit HIR construction and variant-selection nodes carrying applied parent, canonical variant, specialized fields, source mapping, access, selection path, and cleanup identity, and verify HIR snapshots retain both outer and inner selections.
- [x] 7.2 Lower union construction and hierarchical patterns through the verified layout and ownership plans, and verify a direct nested arm produces an outer structural decision followed by the nominal variant decision.
- [x] 7.3 Add monomorphic MIR operations for nominal construction, tag selection, dominated payload projection, and active copy/drop dispatch, and verify MIR rejects foreign parents, fields, layouts, tags, and inactive cleanup.
- [x] 7.4 Extend MIR verification with selection-dominance and hierarchical-coverage checks, and verify an incomplete path or backend-default fallback is rejected before execution.
- [x] 7.5 Add deterministic nominal-union MIR encoding and committed in-process goldens, and verify equivalent discovery traversals produce identical instance, variant, field, path, layout, and cleanup ordering.

## 8. Evaluation and Backends

- [x] 8.1 Represent evaluator values by semantic parent, active variant, and complete payload, and verify construction, movement, matching, storage, calls, returns, and active cleanup without evaluating inactive storage.
- [x] 8.2 Implement direct WebAssembly nominal-union construction, transport, nested tag dispatch, payload mapping, and active cleanup from verified MIR/layout plans, and verify focused Wasm tests cover codegen-specific representation claims.
- [x] 8.3 Implement native LLVM nominal-union construction, transport, nested tag dispatch, payload mapping, and active cleanup from the same plans, and verify target-specific lowering tests contain no backend-owned tag or offset decisions.
- [x] 8.4 Add representative unit, payload, generic, represented-field, structural-root, and cleanup programs to the shared evaluator/Wasm assertions and native differential corpus, and verify all engines agree without adding per-feature native-agreement tests.

## 9. Carrier-Neutral Intrinsic Migration

- [x] 9.1 Replace checked scalar intrinsic result contracts with generic present/absent exact `once fn` carriers while keeping the intrinsic operation inventory count unchanged, and verify catalog audit tests contain no Option identity or spelling.
- [x] 9.2 Lower and execute checked carrier selection with exactly one callback invocation and cleanup of the unused callable environment, and verify evaluator, Wasm, and native tests cover success, absence, affine captures, and traps.
- [x] 9.3 Delete `Intrinsic.effectResult` and its analysis, HIR, MIR, evaluator, Wasm, LLVM, layout-discovery, and suspension-metadata support without replacement, and verify the intrinsic inventory plus repository searches contain no completed-outcome primitive or compatibility path.
- [x] 9.4 Replace handle-producing file and directory open results with affine-safe success/failure `once fn` carriers, and verify success transfers one initialized `OsHandle` plus close obligation while failure creates no handle or optionally initialized place.
- [x] 9.5 Replace optional count-producing OS filesystem, standard-input, child-process, and process-input results with primitive `bool` plus initialized count/reason/code outputs, and verify host-boundary tests distinguish zero-length success, absence, and refusal without constructing Option in compiler code.
- [x] 9.6 Remove `Type.option`, old Result/member helpers, detached outcome construction, and Option/Result-specific branches from analysis, HIR, MIR, evaluation, and backends, and verify repository searches plus intrinsic audits find no compiler recognition by standard-library module or declaration spelling.

## 10. Atomic Standard-Library Migration

- [x] 10.1 Replace `option.silk` with the public nominal union and direct `some`/`none` helpers, and verify its combinators construct and match direct variants with public payload access and no wrapper field.
- [x] 10.2 Replace `result.silk` with the public nominal union and direct `succeed`/`failResult` helpers, and verify its combinators accept structural error unions without flattening Success or Failure.
- [x] 10.3 Update integer, character, string, allocation, and other checked wrappers to supply carrier-neutral intrinsic adapters and return direct Option variants, and verify checked success/absence tests use the canonical nominal representation.
- [x] 10.4 Implement ordinary Silk `Effect.result` by mapping success into `Result.Success` and applying general `Effect.catchAll` to map the complete typed failure into `Result.Failure`; migrate direct consumers and verify one nominal layer, compound failure unions, preserved requirements, move-only branch values, and an equivalent user-defined result-like union.
- [x] 10.5 Migrate filesystem, process, formatting, random, collection, and remaining canonical Silk modules from detached member imports and wrapper-field matches to qualified parent variants, and verify the complete stdlib source closure compiles.
- [x] 10.6 Delete detached `Some`, `None`, `Success`, and `Failure` declarations, wrapper structs, aliases, dual paths, stale imports, and old generated embeddings, then regenerate the deterministic stdlib manifest and verify a repository-wide removal test finds no superseded representation.

## 11. Tooling, Documentation, and Acceptance

- [x] 11.1 Extend syntax highlighting/token consumers, hover, completion, signature help, rename, references, and inspector/labs projections for union declarations and qualified variants, and verify LSP and tooling snapshots navigate through canonical parent/variant/field facts.
- [x] 11.2 Add the prescriptive nominal-union reference documentation covering declaration syntax, generic qualification/inference, visibility, ownership, layout abstraction, matching, and distinction from `enum` and `A | B`, and verify documentation examples compile as doctests where supported.
- [x] 11.3 Rewrite Option, Result, Effect, integer, and error-model documentation/examples for qualified direct variants and structural error composition, and verify no documentation search finds detached member types or wrapper `.value` matches.
- [x] 11.4 Add or update acceptance corpus cases for `Result<Data, HttpError | OutOfMemoryError>`, direct hierarchical matching, generic variants, Copy/Drop, recursion rejection, represented fields, and diagnostics, and verify each claim is tested at the cheapest policy-approved tier.

## 12. Final Verification

- [x] 12.1 Run the focused lexer, parser, formatter, declaration, semantic, matching, ownership, layout, HIR, MIR, evaluator, Wasm, native-corpus, intrinsic, stdlib, LSP, and doctest suites and verify every delta-spec scenario has direct evidence.
- [x] 12.2 Run `pnpm typecheck` and fix every introduced type error, recording any unrelated pre-existing failure exactly.
- [x] 12.3 Run `pnpm exec biome check .` and fix every introduced formatting or lint failure, recording any unrelated pre-existing failure exactly.
- [x] 12.4 Run `pnpm test` and fix every introduced test failure, recording any unrelated pre-existing failure exactly.
- [x] 12.5 Run `pnpm check` and verify the repository-wide required gate completes, or report the exact pre-existing blocker without describing the change as complete.
- [x] 12.6 Run `pnpm release:candidate` because compiler package contents change, and verify package contents, exports, stdlib embeddings, and release artifacts are internally consistent.
- [x] 12.7 Run `openspec validate add-nominal-unions --strict` and verify proposal, all delta specs, design, and tasks remain coherent after implementation discoveries.
