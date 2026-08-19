# SLP-0003: Canonical value types and narrow compatibility

SLP: 0003
Status: Draft
Revision: 7
Author: Julia Ortiz
Created: 2026-08-18
Updated: 2026-08-18
Discussion: —
Review record: —
Depends on: —
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Provisional direction: Silk values have canonical, exact types. An already-typed value is compatible
with the same type except for a closed set of named relations: `never` may inhabit any expected
result, a precise value may inject into a containing structural union, a smaller structural union
may widen into a containing one, and callable or Effect access may weaken under their own rules.
Context may select the type of an exact, not-yet-typed literal, but it never converts an existing
numeric, text, aggregate, or borrowed value implicitly.

## Problem and evidence

Silk already implements scalars, nominal structs, fixed arrays, borrowed slices, immutable text,
structural unions, generic parameters, callable values, and Effects. Their rules are distributed
across specifications written at different stages. Some artifacts use obsolete source spellings such
as `Array<T, N>`, some describe all structs as affine despite the confirmed user-requested `Copy`
marker, and no compact source explains which apparent conversions are literal typing, union
compatibility, access weakening, or invalid source.

The result is the same stabilization failure that motivated the language reference: when a value is
rejected, the author cannot reliably tell whether the language prohibits it, the compiler is
incomplete, or an implementation rule was never deliberately selected.

## Driving examples: current and desired

### Case: Preserve precise values while composing nominal alternatives

#### Intent

Construct precise nominal and array values, then return one nominal value through a declared union
without losing the precise type of intermediate bindings.

#### Current Silk

```silk
struct Token { kind: u8 }
struct End {}

fn token() -> Token {
  let bytes = [1, 2, 3]
  return Token { kind: bytes[0] }
}

fn next(done: bool) -> Token | End {
  if done {
    return End {}
  }
  let value = token()
  return move value
}
```

The compiler has enough machinery to type most of this program, but the programmer-visible model is
spread across scalar, array, struct, union, ownership, and control-flow artifacts.

#### Desired Silk

```silk
struct Token { kind: u8 }
struct End {}

fn tokenBytes() -> [u8; 3] {
  return [1, 2, 3]
}

fn token() -> Token {
  let bytes = tokenBytes()
  return Token { kind: bytes[0] }
}

fn next(done: bool) -> Token | End {
  if done {
    return End {}
  }
  let value = token()
  return move value
}
```

The return type of `tokenBytes` contextually selects `u8` for each exact integer literal, so
`bytes[0]` is `u8`. The `value` binding remains precisely `Token`; only the return boundary injects
it into `Token | End`.

#### Observable result

Every expression has one locally explainable type. Diagnostics identify the first incompatible
boundary and never claim an implicit conversion occurred.

#### Boundary case

```silk
struct Token { kind: u8 }

fn token() -> Token {
  let bytes = [1, 2, 3]
  return Token { kind: bytes[0] }
}
```

Without the return context supplied by `tokenBytes`, the array literal is `[i32; 3]` and indexing
produces `i32`. The field initializer is invalid even though each runtime integer happens to fit in
`u8`; `bytes[0]` is already typed.

## Goals and non-goals

### Goals

- Define the canonical identity of foundational scalars, unit, bottom, nominal structs, arrays,
  slices, immutable text, and structural unions.
- Separate contextual literal selection from compatibility between already-typed values.
- Define the complete ordinary expected-type compatibility relation.
- Preserve precise inferred types instead of rewriting bindings from later uses.
- Give every invalid boundary a diagnostic stated in programmer terms.

### Non-goals

- Specify the complete arithmetic, comparison, bitwise, or conversion API.
- Stabilize generic inference, interface conformance, or representation parameters beyond their use
  in canonical type identity.
- Define module import syntax or the full visibility system.
- Expand top-level constants or static evaluation.
- Define unsafe primitives, target ABI, layout details, or serialization formats.

## Current language model

The compiler has a closed semantic type vocabulary and one `TypeCompatibility` relation. Exact
identity, `never`, nominal injection, union widening, callable-mode weakening, and Effect-access
weakening are accepted. Literal elaboration can use an immediate expected type before the resulting
value becomes typed. Specs and older examples do not consistently distinguish this model from
source spelling, ownership, or runtime representation.

## Proposed language model

The language reference will describe source-visible type identity independently from compiler data
structures and layout. Every value expression has one precise type. Expected contexts may select an
untyped literal or apply one of the closed compatibility relations, but do not mutate the expression
or a prior binding's type.

## Worked language experience

To be expanded as the author reviews scalar defaults, text and byte views, struct construction,
fixed arrays, runtime slices, and structural-union compatibility.

## Semantic sketch

- Canonical source types compare by semantic identity, not spelling aliases or physical layout.
- Exact compatibility is the default.
- `never` is compatible with every expected type because it produces no value.
- A precise value may inject into a containing structural union of ordinary value types.
- A structural union may widen only when every source member is present in the target.
- Callable mode and Effect access may weaken only as defined by their own confirmed contracts.
- Exact integer and floating literals remain untyped until an immediate numeric context selects a
  representable type; otherwise they use their language defaults.
- Once typed, numeric values never widen, narrow, reinterpret, or change signedness implicitly.
- Borrowed views never become owned values implicitly, and owned aggregates never decay to views.

## Compiler–standard library boundary

### Compiler necessity

Parsing, literal contextualization, canonical type identity, expected-context checking, and union
conversion are source-semantic operations unavailable to ordinary library code.

### Smallest target-neutral primitive

No new source-callable intrinsic is proposed. The compiler needs only the closed type vocabulary,
literal selection rules, and compatibility relation required to analyze ordinary source.

### Standard-library construction

Numeric conversions, validation, allocation, owning text, collections, and other value operations
remain ordinary source APIs built over narrowly typed intrinsics where machine operations are
necessary.

### Privilege audit

The compatibility relation recognizes semantic type forms, not standard-library declaration names.
Nominal values remain ordinary declarations; only foundational scalars, borrowed view forms,
callables, Effects, arrays, `string`, `never`, and union syntax require language representation.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Canonical source spellings and aggregate forms must be fixed. |
| Types and abstraction | Affected | This proposal defines identity, literal selection, and compatibility. |
| Execution contracts | Affected | `never`, Effects, and callable access participate in compatibility without implicit execution. |
| Ownership and resources | Affected | Copy/affine classification and borrowed-view boundaries must agree with type identity. |
| Runtime and targets | Affected | `usize`/`isize` and float widths are target-aware or fixed while source identity stays stable. |
| Compiler | Affected | Parsing, elaboration, compatibility, diagnostics, HIR, and MIR must agree. |
| Standard library | Affected | Explicit conversions and owning types remain ordinary APIs. |
| Tooling and diagnostics | Affected | Hover, inlay hints, navigation, and diagnostics must present canonical source types. |
| Learning and use | Affected | The model must explain a value's type and every permitted compatibility step locally. |

## Scope cohesion

The central thesis is one compatibility model over one canonical value-type vocabulary. Numeric
operation semantics, generic solving, module resolution, constant evaluation, and unsafe behavior
solve independent problems and remain separate stabilization areas. If review exposes an
independent design fork for text or unions, this Draft should split that thesis into linked SLPs.

## Complexity and subtraction budget

The provisional model spends complexity on exact literal contextualization and precise union
conversion, while rejecting general subtyping, numeric promotion, array decay, text conversion, and
borrow-to-owner conversion. Every additional compatibility relation requires its own motivating
program and diagnostic boundary.

## Surface displacement

This Draft primarily reconciles existing machinery and source forms. Confirmed choices may require
removing stale spellings and current restrictions rather than adding broad new runtime behavior.

## Drawbacks and risks

- Exact numeric typing requires explicit conversions in mixed-width code.
- General structural unions require type-pattern matching and deterministic tags for non-nominal
  alternatives as well as nominal ones.
- Immediate literal context must remain predictable; overly broad contextual inference would make
  nearby annotations change unrelated expressions.
- A single page can become too large if operation APIs or generic mechanics leak into it.

## Alternatives and prior art

### Status quo

Keep the distributed specifications and let compiler behavior serve as the effective type model.
This preserves implementation momentum but reproduces the uncertainty stabilization is intended to
remove.

### Smaller primitive or library solution

Libraries can provide explicit conversions and owning wrappers, but cannot define literal typing,
struct or array syntax, union identity, `never`, or expected-context compatibility.

### Strongest competing language model

Adopt broad coercion and subtyping: numeric widening, array-to-slice decay, and automatic text
ownership. This reduces explicit source at call boundaries but makes ownership, allocation, runtime
representation, generic inference, and overload selection depend on hidden conversions.

## Falsifiers and acceptance blockers

- A common real program that becomes dominated by ceremonial conversions may justify one additional
  narrow compatibility relation.
- If general unions cannot preserve precise ownership, lifetime, and deterministic runtime identity
  across ordinary members, their member boundary must be reconsidered before Candidate promotion.
- Candidate promotion requires complete reviewed examples and the author's explicit request.

## Open realization questions

- Which existing diagnostics need new codes rather than corrected wording?
- Which current specs use canonical compiler encodings such as `Array<T, N>` where source spelling
  must be `[T; N]`?
- How should tooling display target-dependent `usize` and `isize` ranges without changing identity?

## Future directions

General constant evaluation, value-producing conditionals, pattern-conditioned `if`, additional
owned text types, and new explicit conversion APIs remain separate directions.

## OpenSpec realization map

After direction acceptance, reconcile foundational scalars and literals, nominal aggregates, fixed
arrays and views, immutable text and bytes, structural unions, expected-context compatibility, and
diagnostics as independently auditable slices.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-18 | Initial Draft with exact canonical types and narrow compatibility as the provisional thesis. |
| 2 | 2026-08-18 | Confirmed the lowercase foundational vocabulary, unit and bottom meanings, and the closed exact-compatibility model. |
| 3 | 2026-08-18 | Confirmed exact integer identities, pointer-width integer identity, contextual numeric literal selection, `i32` and `f64` defaults, and explicit-only conversion of typed numeric values. |
| 4 | 2026-08-18 | Confirmed Unicode-scalar `char`, immutable UTF-8 `string`, explicit text units and conversions, and removal of the compiler's special prohibition on references and slices containing `string` values. |
| 5 | 2026-08-18 | Confirmed nominal struct identity, complete construction, projection, finite inline dependencies, and visibility-based raw construction in place of defining-module privilege. |
| 6 | 2026-08-18 | Confirmed fixed-array identity and literal inference, checked `usize` indexing, explicit array borrowing, and access-bearing reference and slice types. |
| 7 | 2026-08-18 | Confirmed general structural unions over ordinary value types, immediate injection and widening, internal runtime tags, precise binding inference, and the corresponding generalization of match-result joining. |
