# Audit control flow, patterns, modules, names, and visibility

Type: audit
Status: resolved
Blocked by: 03

## Question

For the confirmed conditional, loop, transfer, match, pattern, module, import, namespace,
collision, cycle, visibility, and re-export rules, what does the repository implement, partially
implement, contradict, or omit?

## Scope

- `IF-001`, `LOOP-001`, `TRANSFER-001`, and `MATCH-001–005` from
  `docs/language/functions-callables-and-control-flow.md`;
- all 19 rules in `docs/language/patterns-and-destructuring.md`; and
- all 22 rules in `docs/language/modules-names-and-visibility.md`.

`STMT-001` was classified by ticket 01, and ownership-specific loop and match behavior was
classified by ticket 02. This audit classifies the remaining 49 rules exactly once.

## Answer

### Audit result

The existing statement, loop, nominal-match, module-closure, name-resolution, and visibility
systems are mature. Of 49 scoped rules, 27 are implemented, 8 are partial, 3 are contradicted, and
11 are not implemented. No rule is unknown. Diagnostics have the same 27 aligned, 8 partial, 3
contradicted, and 11 missing split.

The missing rules form one coherent shared-pattern handoff: the parser currently calls
`parsePattern` only for match arms, `let` accepts only an identifier, and `if` accepts only a
boolean expression. Existing nominal match patterns provide the core coverage, narrowing,
ownership, and cleanup semantics, but there is no shared pattern grammar, irrefutable local
destructuring, `if let`, or exact non-nominal type pattern yet. General match result joins are also
blocked by ticket 03's nominal-only structural unions.

The module model has three deliberate old-policy conflicts: redundant aliases are rejected,
repeated imports of one target are rejected, and every standard-library manifest namespace is
seeded as an implicit prelude. The confirmed model keeps those redundancies semantically harmless,
leaves cleanup to the LSP, and requires explicit imports for ordinary standard-library actors.

The focused current-behavior suite passed: 14 test files and 178 tests across mutable loops,
matching, parsing, module closure, imports, visibility, tooling, auto-import, and standard-library
namespace injection.

### Evidence anchors

- Control flow and current match semantics:
  [`Parser.ts`](../../../packages/compiler/src/Parser.ts),
  [`MutableLoops.test.ts`](../../../packages/compiler/test/MutableLoops.test.ts),
  [`ExhaustiveMatching.test.ts`](../../../packages/compiler/test/ExhaustiveMatching.test.ts), and
  [`WholeMemberBinding.test.ts`](../../../packages/compiler/test/WholeMemberBinding.test.ts).
- Pattern surface and elaboration:
  [`Parser.ts`](../../../packages/compiler/src/Parser.ts),
  [`Elaboration.ts`](../../../packages/compiler/src/Elaboration.ts),
  [`bootstrap-exhaustive-matching`](../../../openspec/specs/bootstrap-exhaustive-matching/spec.md),
  and [`SLP-0010`](../../../proposals/0010-shared-patterns-and-conditional-destructuring/proposal.md).
- Modules and names:
  [`ModuleClosure.ts`](../../../packages/compiler/src/ModuleClosure.ts),
  [`NameResolution.ts`](../../../packages/compiler/src/NameResolution.ts),
  [`ModuleClosure.test.ts`](../../../packages/compiler/test/ModuleClosure.test.ts),
  [`NameResolution.test.ts`](../../../packages/compiler/test/NameResolution.test.ts), and
  [`ImportPlan.test.ts`](../../../packages/compiler/test/ImportPlan.test.ts).
- Prelude and tooling:
  [`StdlibNamespaceAcceptance.test.ts`](../../../packages/compiler/test/StdlibNamespaceAcceptance.test.ts),
  [`AutoImport.test.ts`](../../../packages/compiler/test/AutoImport.test.ts), and
  [`bootstrap-name-resolution`](../../../openspec/specs/bootstrap-name-resolution/spec.md).

### Control flow and exhaustive matching

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| IF-001 | Implemented | Aligned | Boolean statement conditionals evaluate once, select only one arm, reject truthiness, and preserve arm-local semantic facts. `if let` is separately absent under PATT-007. |
| LOOP-001 | Implemented | Aligned | `while` is a boolean pre-test loop with path-compatible ownership state, scoped iteration locals, and no value-producing or labeled extension. |
| TRANSFER-001 | Implemented | Aligned | `break` and `continue` target the innermost loop, carry no value, reject outside loops, and route structured cleanup through their target edges. |
| MATCH-001 | Implemented | Aligned | Bare Copy, `move`, `&`, and `&mut` scrutinee access are explicit, evaluated once, and reflected in pattern bindings and ownership diagnostics. |
| MATCH-002 | Implemented | Aligned | Nominal field, renamed, nested, whole-member, and `..` patterns enforce completeness, visibility, binding uniqueness, and cleanup. |
| MATCH-003 | Implemented | Aligned | Source-order coverage, guards, wildcard reachability, exhaustiveness, and provisional guard ownership use the confirmed rules and specific diagnostics. |
| MATCH-004 | Implemented | Aligned | Selected arms narrow to their canonical nominal member without changing the scrutinee's type outside the arm. |
| MATCH-005 | Partial | Partial | Equal, `never`, and distinct nominal arm results join deterministically. General ordinary-value joins remain unavailable because unions and member patterns are nominal-only. |

### Shared patterns and destructuring

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| PATT-001 | Not implemented | Missing | `parsePattern` is entered only by `parseMatchArm`; local bindings and conditionals have separate identifier/boolean grammars. |
| PATT-002 | Partial | Partial | Existing match scrutinees select Copy, move, shared, or exclusive access exactly once. The same access surface does not exist for destructuring `let` or `if let`. |
| PATT-003 | Partial | Partial | Canonical nominal identity and whole-member binding work in match. The form is unavailable in the other contexts and cannot select exact non-nominal members. |
| PATT-004 | Partial | Partial | Recursive explicit field destructuring, renaming, `..`, and field diagnostics work in match only. |
| PATT-005 | Partial | Partial | Match bindings are fresh, flat, arm-local declarations with collision diagnostics. Successful-body and unconditional destructuring scopes do not exist yet. |
| PATT-006 | Not implemented | Missing | `let` parses one identifier, so irrefutability proofs, destructuring initialization, and the rejection of `let _ = value` in favor of `drop` are absent. |
| PATT-007 | Not implemented | Missing | There is no `if let` parser, HIR node, flow analysis, or mismatch branch. |
| PATT-008 | Not implemented | Missing | No consuming conditional exists to consume before testing, transfer selected payload ownership, and clean the mismatch payload. |
| PATT-009 | Not implemented | Missing | No borrowed conditional exists to create body-local loans while preserving the owner after the complete conditional. |
| PATT-010 | Not implemented | Missing | An irrefutable `if let` cannot be expressed, so the compiler cannot accept it while leaving redundancy to the LSP. |
| PATT-011 | Partial | Partial | Existing match patterns use compiler-defined nominal tests/projections and execute no user code. Ordinary type tests and the other binding contexts are absent. |
| PATT-012 | Partial | Partial | Existing match failures have focused codes for members, coverage, fields, guards, and ownership. Refutability, contextual availability, and `if let` recovery codes are unassigned. |
| PATT-013 | Implemented | Aligned | Existing match source order, guards, exhaustiveness, reachability, narrowing, explicit access, and nominal result joins retain their confirmed behavior. |
| PATT-014 | Partial | Partial | Match correctly exposes only the small existing pattern set and rejects unsupported extensions, but the promised initial `let` and `if let` surface is absent. |
| PATT-015 | Not implemented | Missing | Whole-value selectors accept nominal declarations, not exact scalar, array, view, callable, Effect, or other ordinary union members. |
| PATT-016 | Not implemented | Missing | Coverage has canonical nominal identities but cannot calculate exact ordinary-type member coverage. |
| PATT-017 | Not implemented | Missing | The parser cannot express an ordinary type selector, so it cannot enforce the one-normalized-member restriction or diagnose union selectors. |
| PATT-018 | Not implemented | Missing | Non-nominal selections cannot yet inherit bare Copy, move, shared, and exclusive pattern access. |
| PATT-019 | Not implemented | Missing | Generic type selectors do not exist, so no declared-constraint distinctness proof or overlap diagnostic is implemented. |

### Modules, imports, names, and visibility

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| MODULE-001 | Implemented | Aligned | Exact case-sensitive extensionless module identities derive from logical source-root paths; missing and reserved identities are diagnosed. |
| MODULE-002 | Implemented | Aligned | Source text has no module declaration and cannot override the resolver-assigned identity. |
| MODULE-003 | Implemented | Aligned | Project imports are absolute from the selected source root and never probe relative or alternate layouts. |
| MODULE-004 | Implemented | Aligned | Module closure follows transitive imports, loads each identity once, and excludes unreachable sources. |
| MODULE-005 | Implemented | Aligned | Imports contribute compile-time bindings without runtime initialization, allocation, provider activation, or re-export behavior. |
| MODULE-006 | Implemented | Aligned | Deterministic SCC handling accepts import cycles while independently diagnosing invalid value cycles and bodies. |
| IMPORT-001 | Implemented | Aligned | Namespace imports bind the final path segment, remain compile-time names, and distinguish unknown from private member access. |
| IMPORT-002 | Implemented | Aligned | Namespace aliases replace the default local name and preserve canonical module identity. Redundant-alias policy is classified under IMPORT-005. |
| IMPORT-003 | Implemented | Aligned | Selective imports bind only listed public members, support member aliases, and distinguish absent, private, and colliding bindings. |
| IMPORT-004 | Implemented | Aligned | One declaration may combine a namespace or alias with selected members; each created binding is checked independently. |
| NAME-001 | Implemented | Aligned | Top-level declarations are indexed before bodies, so same-module lookup is source-order independent while locals remain lexical. |
| NAME-002 | Implemented | Aligned | Top-level declarations and import bindings occupy one flat module namespace with kind-independent collisions. |
| NAME-003 | Implemented | Aligned | Collisions make the spelling unavailable and never select a source-order or declaration-kind winner. |
| NAME-004 | Implemented | Aligned | Explicit namespace and selected-member aliases resolve collisions without changing declaration identity or runtime behavior. |
| VIS-001 | Implemented | Aligned | Top-level declarations are private by default; `pub` permits explicit cross-module access without changing other contracts. |
| VIS-002 | Implemented | Aligned | Private declarations remain fully available everywhere inside their defining module and nowhere merely by directory proximity. |
| VIS-003 | Implemented | Aligned | Qualified and selective lookup retain distinct unknown-member and inaccessible-member outcomes. |
| VIS-004 | Implemented | Aligned | Public contracts reject exposed private nominal types while allowing private representation behind private fields. |
| IMPORT-005 | Contradicted | Contradicted | `NameResolution` emits `SEM0013` and withholds the binding for an alias equal to its default. The confirmed rule keeps it valid and leaves simplification to tooling. |
| IMPORT-006 | Contradicted | Contradicted | Module closure/name resolution emit `MOD0003` for every repeated canonical target. The confirmed rule judges only the bindings and leaves combination/removal to tooling. |
| PRELUDE-001 | Contradicted | Contradicted | Current resolution and completion seed loaded standard-library manifest namespaces without imports. The confirmed rule makes only language bindings and `Intrinsic` implicit. |
| EXPORT-001 | Implemented | Aligned | Imports remain module-local bindings, `pub import` is unsupported, and third-party lookup never follows them as re-exports. |

### Superseded artifacts

| Superseded model | Current artifact evidence |
| --- | --- |
| Patterns are match-arm syntax rather than a shared grammar | parser entry points, syntax OpenSpec, and current HIR pattern ownership |
| Structural-union members and selectors are nominal only | exhaustive-match elaboration, nominal coverage facts, and `SEM0042` |
| Redundant aliases are invalid | `SEM0013`, name-resolution tests, LSP deletion action, and current name-resolution OpenSpec |
| One import declaration per canonical target | `MOD0003`, module-closure duplicate facts, and import cleanup tests |
| Standard-library manifest namespaces form an implicit prelude | name-resolution seed bindings, lexical manifest discovery, qualified completion, and namespace acceptance tests |

### Ordered implementation handoffs

1. **Create one shared pattern syntax and semantic representation.** Reuse the existing nominal
   match forms in match arms, local destructuring, and conditional destructuring; keep patterns
   compiler-defined and non-executable.
2. **Add irrefutable local destructuring.** Extend `let` from identifier binding to patterns,
   prove irrefutability, preserve recursive field access and ownership, and make standalone
   `let _ = value` diagnose in favor of `drop value`.
3. **Add `if let` as a statement.** Implement success-only bindings, optional mismatch bodies,
   consume-on-both-outcomes behavior for `move`, loan-scoped behavior for borrows, cleanup, and
   flow diagnostics. Keep irrefutable conditionals valid and make their simplification LSP-only.
4. **Generalize member patterns and result joins with ordinary unions.** After ticket 03's union
   handoff, admit one exact normalized ordinary type per selector, reuse coverage/narrowing and
   access semantics, and require generic distinctness proofs rather than specialization-time
   behavior changes.
5. **Make import redundancy non-semantic.** Remove `SEM0013` and `MOD0003` from compilation,
   preserve every valid binding, and retain optional LSP warnings/fixes for unchanged aliases,
   exact duplicates, and combinable repeated imports.
6. **Remove the implicit standard-library prelude.** Build closure and scope from explicit imports,
   retain reserved-manifest source resolution and auto-import discovery, and make completion insert
   imports instead of exposing invisible bindings.

### Next frontier

The next frontier is
[05 — runtime, standard library, targets, termination, and tooling](05-runtime-termination-targets-and-tooling.md).
