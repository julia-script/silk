# Audit values, generics, representations, and operators

Type: audit
Status: resolved
Blocked by: 02

## Question

For the confirmed value, type, generic, representation, expression, operator, assignment, and
conversion rules, what does the repository implement, partially implement, contradict, or omit?

## Scope

- all 21 rules in `docs/language/values-and-types.md`;
- `GEN-001–007` and `REP-001–006` from
  `docs/language/generics-interfaces-and-specialization.md` (interface and conformance rules were
  classified by ticket 01); and
- all 26 rules in `docs/language/expressions-and-operators.md`.

This audit classifies 60 rules exactly once.

## Answer

### Audit result

The compiler's scalar, aggregate, generic-specialization, representation, evaluation-order,
assignment, and conversion foundations are strong. Of 60 scoped rules, 48 are implemented, 6 are
partial, and 6 are contradicted. No rule is unknown or wholly absent.

Seven seams explain the differences:

1. structural unions still admit only nominal members;
2. nominal Copy behavior still follows ticket 02's inconsistent always-affine/structural-Copy split;
3. raw struct construction is restricted to the defining module even when every field is public;
4. `string` is given special borrow/slice exclusions instead of ordinary value behavior, and scalar
   traversal still exposes `u32` rather than proven `char`;
5. generic struct construction infers representation arguments but not ordinary type arguments;
6. generic failure parameters retain ticket 01's separate row kind; and
7. custom operators are selected by privileged operation names, while short-circuit right operands
   are subject to an extra impurity ban not applied to ordinary conditional branches.

Exact and opaque representations are not a stabilization problem: all six confirmed REP rules are
implemented with finite static identity, deterministic specialization, visibility fences, and no
existential runtime package or indirect dispatch.

The focused current-behavior suite passed: 25 test files and 229 tests across scalars, characters,
text, structs, arrays, unions, generics, exact/opaque representations, operators, and engines.

### Evidence anchors

- Types, normalization, inference, and representation identity:
  [`Type.ts`](../../../packages/compiler/src/Type.ts),
  [`TypeGenerics.test.ts`](../../../packages/compiler/test/TypeGenerics.test.ts),
  [`bootstrap-type-generics`](../../../openspec/specs/bootstrap-type-generics/spec.md), and
  [`bootstrap-representation-parameters`](../../../openspec/specs/bootstrap-representation-parameters/spec.md).
- Scalars, text, structs, arrays, and unions:
  [`Scalar.ts`](../../../packages/compiler/src/Scalar.ts),
  [`Elaboration.ts`](../../../packages/compiler/src/Elaboration.ts),
  [`StringSemantics.test.ts`](../../../packages/compiler/test/StringSemantics.test.ts),
  [`StructValues.test.ts`](../../../packages/compiler/test/StructValues.test.ts),
  [`FixedArraySemantics.test.ts`](../../../packages/compiler/test/FixedArraySemantics.test.ts), and
  [`StructuralUnionRuntime.test.ts`](../../../packages/compiler/test/StructuralUnionRuntime.test.ts).
- Opaque and exact representations:
  [`OpaqueRealization.ts`](../../../packages/compiler/src/OpaqueRealization.ts),
  [`OpaqueRealization.test.ts`](../../../packages/compiler/test/OpaqueRealization.test.ts),
  [`OpaqueRepresentationEngines.test.ts`](../../../packages/compiler/test/OpaqueRepresentationEngines.test.ts),
  and [`bootstrap-opaque-representation-results`](../../../openspec/specs/bootstrap-opaque-representation-results/spec.md).
- Operators, assignment, and conversions:
  [`Operator.ts`](../../../packages/compiler/src/Operator.ts),
  [`Operator.test.ts`](../../../packages/compiler/test/Operator.test.ts),
  [`ShortCircuitOperatorAcceptance.test.ts`](../../../packages/compiler/test/ShortCircuitOperatorAcceptance.test.ts),
  [`IntegerScalars.test.ts`](../../../packages/compiler/test/IntegerScalars.test.ts), and
  [`FloatingPointScalars.test.ts`](../../../packages/compiler/test/FloatingPointScalars.test.ts).

### Values and types

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| TYPE-001 | Implemented | Aligned | Foundational scalar and special type spellings are lowercase, closed, and distinct; removed uppercase aliases do not resolve. |
| TYPE-002 | Implemented | Aligned | Unit has one value, `never` is bottom, unit fallthrough works, and unreachable bottom paths do not force a value. |
| TYPE-003 | Partial | Partial | Exact equality, callable access admission, union widening, and Effect subsumption are explicit. The named relations still operate over nominal-only unions and the superseded failure-row kind. |
| INT-001 | Implemented | Aligned | Signed and unsigned fixed widths plus target-word `isize`/`usize` have exact ranges, layouts, and cross-engine operations. |
| INT-002 | Implemented | Aligned | Integer literals retain exact magnitude until immediate context, default to `i32`, and reject contextual overflow or already-typed mismatch before MIR. |
| FLOAT-001 | Implemented | Aligned | Floating literals select `f32` from immediate context and otherwise default to `f64`, with direct correctly rounded parsing. |
| CHAR-001 | Partial | Partial | Character literals and the scalar type exclude surrogates and preserve Unicode scalar identity. String scalar traversal still exposes `u32` because the checked `u32`-to-`char` library conversion is absent. |
| TEXT-001 | Implemented | Aligned | `string` is immutable UTF-8 text, byte literals are distinct immutable byte views, and engines preserve exact content without normalization. |
| TEXT-002 | Contradicted | Contradicted | Named byte/scalar units and explicit conversions exist, but declaration indexing specially rejects `&string`, `&mut string`, and `&[string]` with `SEM0094`, contrary to ordinary-value borrowing and slicing. |
| STRUCT-001 | Implemented | Aligned | Structs are nominal by canonical module/name identity, preserve field declaration order, and support zero-field markers. |
| STRUCT-002 | Contradicted | Contradicted | Construction is complete and validates fields, but `analyzeStructLiteral` authorizes raw literals only in the defining module (plus a compiler exception), reporting `SEM0021` even when every field is public. |
| STRUCT-003 | Implemented | Aligned | Projection resolves the declaration field, preserves exact type, and diagnoses unknown or inaccessible fields before lowering. |
| STRUCT-004 | Implemented | Aligned | Direct and mutual inline representation cycles are rejected while harmless module reference cycles remain valid. |
| STRUCT-005 | Contradicted | Missing | Struct ownership does not use the confirmed explicit sealed Copy contract. Ownership treats nominals move-only while cleanup validation separately derives structural Copy, as classified in ticket 02. |
| ARRAY-001 | Implemented | Aligned | Fixed-array identity includes element type and exact length, including zero-length arrays. |
| ARRAY-002 | Implemented | Aligned | Nonempty literals infer one homogeneous element type; empty literals require context; every element is evaluated and retained in source order. |
| INDEX-001 | Implemented | Aligned | Array and slice indexing requires `usize`, validates fixed indices statically when possible, and traps dynamic out-of-bounds access before replacement evaluation. |
| VIEW-001 | Implemented | Aligned | `&T`, `&mut T`, `&[T]`, and `&mut [T]` preserve shared/exclusive access in their type; ownership and returned provenance are separate rules. |
| UNION-001 | Contradicted | Contradicted | Normalization is deterministic, associative, duplicate-free, and erases `never`, but `Type.union` accepts only nominal members and rejects ordinary types such as integers and strings with `SEM0039`. |
| UNION-002 | Partial | Partial | Precise binding inference and immediate expected-boundary injection/widening work for admitted nominal unions. They cannot operate over the confirmed ordinary value-type universe. |
| INFER-001 | Implemented | Aligned | A binding retains its initializer's precise nominal, union, callable, Effect, representation, or scalar type rather than widening from later uses. |

### Generics and representations

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| GEN-001 | Partial | Contradicted | Declaration-local canonical identities and duplicate rejection work. Failure binders are still a separate `!E` row kind instead of ordinary type parameters. |
| GEN-002 | Implemented | Aligned | Applied nominal types have canonical kinded arguments, exact arity, deterministic identity, and stable substitution. |
| GEN-003 | Implemented | Partial | Calls infer forward from supplied arguments and declared constraints, accept explicit prefixes, and refuse return-only inference. Failure/requirement diagnostics still render the old channel kinds. |
| GEN-004 | Implemented | Aligned | Generic bodies are checked once against open contracts and specializations substitute the proof rather than re-authorizing concrete-only operations. |
| GEN-005 | Implemented | Aligned | Reachable applications discover finite concrete instances before layout/MIR and produce deterministic monomorphic LLVM/Wasm symbols. |
| GEN-006 | Implemented | Aligned | Same-argument recursion remains finite; direct and mutual parameter-changing recursion are rejected before unbounded instance growth. |
| GEN-007 | Partial | Partial | Struct literals infer omitted callable and Effect representation arguments, including after a written prefix. Ordinary nominal type arguments generally remain mandatory instead of being inferred from fields. |
| REP-001 | Implemented | Aligned | Representation parameters retain one exact callable/Effect implementation separately from the access contract it may satisfy. |
| REP-002 | Implemented | Aligned | Construction infers exact identities, generic forwarding preserves them, and concrete identity reaches HIR, instances, layout, MIR, and engines. |
| REP-003 | Implemented | Aligned | Representation-dependent values join only at exact identity; consuming distinct values inside branches may still converge to an ordinary result. |
| REP-004 | Implemented | Aligned | `some` hides one representation behind a public callable/Effect contract without allocation, packaging, or runtime dispatch. |
| REP-005 | Implemented | Aligned | Each producer specialization has one opaque family, divergent returns and realization cycles are rejected, and capture-shape changes invalidate dependents. |
| REP-006 | Implemented | Aligned | `typeof(item)` names resolved visible exact callable identities and prevents private identity leakage. |

### Expressions and evaluation

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| EVAL-001 | Implemented | Aligned | Eager children evaluate once left-to-right across calls, literals, operators, construction, indexing, and replacement. |
| EVAL-002 | Implemented | Aligned | Short-circuit, conditional, match, Effect construction, and delayed callable forms state and preserve which children are conditional or deferred. |
| EXPR-001 | Implemented | Aligned | Every available HIR expression carries one precise type and produces one value; unavailable facts retain a cause instead of inventing a fallback value. |
| EXPR-002 | Implemented | Aligned | Calls, projections, indexing, run, and postfix forms bind before infix operators and compose left-to-right. |
| EXPR-003 | Implemented | Aligned | Parentheses alter parsing/grouping only; ownership, Effect execution, and evaluation count remain those of the enclosed expression. |
| EXPR-004 | Implemented | Aligned | Assignment and bootstrap `if` are statement forms and cannot be used as ordinary expression values. |
| EXPR-005 | Implemented | Aligned | Non-unit/non-`never` expression statements receive `SEM0087`; explicit `drop` consumes without executing an Effect. |

### Operators

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| OP-001 | Implemented | Aligned | The parser has one fixed precedence/associativity table, rejects comparison chains, and lets grouping override it. |
| OP-002 | Partial | Contradicted | Primitive eager operators select one static intrinsic operation. Generic user operators use hidden name lookup and concrete user types have no matching surface, so selection is not the confirmed uniform explicit operation. |
| OP-003 | Implemented | Aligned | Numeric binary operators require identical concrete integer or float types; literals may receive the other operand's immediate context but typed mixed widths are rejected. |
| OP-004 | Implemented | Aligned | Ordinary integer arithmetic traps on overflow/division faults; wrapping and checked variants are explicit named operations. |
| OP-005 | Implemented | Aligned | Float operations preserve width and deterministic conservative IEEE behavior, including NaN and signed-zero boundaries. |
| OP-006 | Implemented | Aligned | Equality and ordering availability is closed and explicit for admitted primitive/text types; structs and unions do not gain structural comparison implicitly. |
| OP-007 | Implemented | Aligned | `&&`, `||`, and `!` require booleans, and binary forms skip the right operand when the left decides the result. |
| OP-008 | Implemented | Aligned | Bitwise operations accept identical integer types only and use the same static target-neutral operator path. |
| OP-009 | Contradicted | Contradicted | There is no explicit operator declaration. Generic lookup privileges bound operations named `add`, `lessThan`, and similar compiler spellings, while equivalent concrete operations are unavailable. |
| OP-010 | Contradicted | Contradicted | Path-local conditional execution exists, but `impurityOf` recursively rejects `run`, Effect-result sites, and `move` on the right with `SEM0096`; ordinary conditional branches have no such rule. |

### Assignment and conversion

| Rule | Semantics | Diagnostics | Current evidence and boundary |
| --- | --- | --- | --- |
| ASSIGN-001 | Implemented | Aligned | Assignment replaces one complete mutable place, checks exact compatibility, cleans the displaced owner, and leaves the root initialized. |
| ASSIGN-002 | Implemented | Aligned | Place validity and dynamic bounds are established before the replacement expression begins, so a failed destination cannot trigger replacement effects. |
| ASSIGN-003 | Implemented | Aligned | Overlap analysis rejects a replacement that consumes its own destination with `OWN0004`. |
| ASSIGN-004 | Implemented | Aligned | Sealed `Intrinsic.replace` swaps a valid place and returns the previous complete owner without creating partial initialization. |
| ASSIGN-005 | Implemented | Aligned | Compound assignment has no current parser form; users spell the read/operation/replacement explicitly. |
| CONV-001 | Implemented | Aligned | Conversion is a named scalar/library operation; operators and assignments do not insert hidden numeric conversions. |
| CONV-002 | Implemented | Aligned | Integer APIs distinguish trapping conversions from checked `Option`-style range outcomes and preserve target-word bounds. |
| CONV-003 | Implemented | Aligned | Float/integer conversion operations state rounding and invalid/range traps and agree across evaluator, LLVM, and Wasm. |
| CONV-004 | Implemented | Aligned | `fromBits`/`toBits` preserve exact representation and remain distinct from numeric conversion. |

### Superseded artifacts

| Superseded model | Current artifact evidence |
| --- | --- |
| Structural unions contain nominal members only | `Type.union`, `bootstrap-structural-unions`, syntax tests, and `SEM0039` |
| Raw struct construction is module-private regardless of public fields | `analyzeStructLiteral`, `bootstrap-struct-values`, and `StructValues.test.ts` |
| Strings cannot participate in ordinary reference/slice type formation | declaration indexing, `SEM0094`, and string semantic tests |
| Only representation arguments are inferred in struct literals | `analyzeStructLiteral` and representation inference tests |
| Operator extension follows reserved operation names | operator target selection, bound-operation elaboration, and the current operator specs |
| Short-circuit branches must be “pure” by a special recursive walk | `impurityOf`, `SEM0096`, and current short-circuit scenarios |

### Ordered implementation handoffs

1. **Generalize structural unions to ordinary detached value types.** Replace nominal-only member
   storage and normalization, then update compatibility, layout, MIR tags, ownership/Copy,
   evaluation, backends, matching, and diagnostics together. Ticket 04 should consume this handoff
   for exact non-nominal patterns rather than inventing a second union model.
2. **Make public struct construction truly field-based.** Remove the module-wide authorization
   gate, require every named initializer field to be visible, preserve private-field construction
   fences and factories, and update the conflicting OpenSpec/tests atomically.
3. **Treat text as an ordinary value and finish scalar typing.** Remove the `SEM0094` type
   exceptions, route references/slices through normal ownership, and add the checked scalar
   conversion needed for string traversal to return `char`.
4. **Complete ordinary struct-argument inference.** Infer omitted ordinary generic parameters from
   all supplied fields using the same forward-only conflict rules already used by calls and
   representation fields; retain explicit prefixes and deterministic ambiguity diagnostics.
5. **Replace operator-name privilege with explicit declarations.** Implement the confirmed
   operator marker on interface operations, use the same static conformance path for generic and
   concrete operands, and delete compiler-name lookup.
6. **Remove the short-circuit impurity pass.** Analyze the right operand as an ordinary conditional
   branch with path-local ownership, Effect, cleanup, and type rules; retain only boolean typing and
   conditional runtime evaluation.
7. **Reuse existing foundational handoffs.** Ticket 01 owns ordinary failure parameters and ticket
   02 owns the sealed Copy property; their implementations must update GEN-001, TYPE-003,
   STRUCT-005, union Copy, and related diagnostics without compatibility shims.

### Next frontier

The next frontier is
[04 — control flow, patterns, modules, names, and visibility](04-control-patterns-modules-and-visibility.md).
