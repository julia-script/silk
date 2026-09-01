## Context

This is an author-approved direct OpenSpec change. The design is intentionally complete enough to
stand without an SLP: all source-visible choices below were settled with the author before drafting.

A scalar enum is distinct from Silk's structural unions. A structural union selects among payload
types and keeps its runtime tag private. A scalar enum declares one nominal, payload-free type with a
closed set of named values and deliberately exposes each member's declared backing integer through
`value`.

```silk
enum AssertionResult {
  Pass,
  Fail,
  Skip,
}

enum(i8) Status {
  Unknown = -1,
  Ready = 1,
}
```

## Goals / Non-Goals

Goals:

- A small, unsurprising closed-set type for statuses, modes, protocol codes, and state-machine labels.
- Nominal safety in expressions, calls, equality, and matching.
- Exact, explicit fixed-width layout suitable for ABI-facing and systems code.
- Exhaustive matching over member identity without exposing arbitrary integer inhabitants.
- Identical behavior in analysis, evaluation, Wasm, and native execution.

Non-goals: payload cases, generic enum declarations, inferred minimum-width representations,
arbitrary discriminant expressions, integer-to-enum conversion, enum ordering, flags, member-level
visibility, or unifying enums with structural unions.

## Decisions

### Members are values of one closed nominal type

`AssertionResult.Pass` is a value of nominal type `AssertionResult`. Member access requires a
qualified enum path; an unqualified `Pass` is not an enum member expression. Construction takes no
parentheses, performs no allocation, and carries no payload. Two enums
with identical member names and discriminants remain different types.

The declaration must contain at least one member. Member names are unique within the enum. The enum's
visibility controls access to the type and all members; members have no independent visibility.

### Representation is fixed-width and explicit by default

Omitting the representation means exactly `u8`; it does not ask the compiler to infer a width.
Allowed explicit representations are `u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, and `i64`.
`usize`, `isize`, aliases, nominal wrappers, and non-integer types are not enum representations.

This makes the common case concise while keeping layout stable and reviewable. If a default enum no
longer fits in `u8`, the declaration must choose a wider representation explicitly.

### Discriminants form one checked declaration-order sequence

The first implicit discriminant is `0`. Every later implicit discriminant is the immediately
preceding member's discriminant plus one, whether that predecessor was explicit or implicit.
Initially, an explicit discriminant is only an optionally negative decimal integer literal; arbitrary
expressions and constant references are rejected syntactically or semantically as appropriate.

Every discriminant must be representable by the selected integer type. A negative value under an
unsigned representation, an explicit out-of-range value, or overflow while computing an implicit
successor is a compile-time diagnostic. Discriminants must be unique even when different spellings or
implicit assignment produce the same numeric value. Duplicate member-name and discriminant
diagnostics retain a related span to the first declaration.

### Layout is exactly the integer representation

An enum has the representation integer's size, alignment, and calling shape, with no hidden metadata.
A default enum and `enum(u8)` are physically identical to `u8`; a one-member enum still occupies one
byte. Logical type information retains the nominal enum identity through verified MIR, while backend
physical lowering uses the chosen integer lane.

Only declared members inhabit a safe enum value. The compiler never creates or accepts an arbitrary
backing integer as an enum value, so no runtime validity check is required for values produced by
well-typed Silk source.

### Every enum is Copy and cleanup-free

A scalar enum owns no resources and carries no payload. All enum values are `Copy`, have no `Drop`
conformance or cleanup obligation, and can be read repeatedly under the existing Copy rules. This is
a sealed compiler-proved property of the enum kind, not a user-selectable conformance.

### `value` is the sole built-in conversion

`AssertionResult.value(result)` returns the exact representation type and exposes the selected
member's discriminant. Each enum declaration contributes this generated associated wrapper from the
enum's canonical identity, not from a standard-library actor recognized by spelling. Its body lowers
to the sealed target-neutral `Intrinsic.enumValue` primitive; backends never recognize `value` by
name.

There is no built-in integer-to-enum conversion, checked or unchecked. A future user-defined trait or
library API may map integers by ordinary comparisons and member construction, but this change grants
no compiler privilege for that direction.

### Equality is nominal; ordering is numeric only after conversion

`==` and `!=` accept two values only when both have the same canonical enum type and compare member
identity. Equality between distinct enums, or between an enum and an integer, is invalid. Enums do
not directly support `<`, `<=`, `>`, or `>=`; callers compare `EnumName.value(left)` and
`EnumName.value(right)` when numeric ordering is intended.

### Matching covers qualified member identities

```silk
match result {
  AssertionResult.Pass => 0
  AssertionResult.Fail => 1
  AssertionResult.Skip => 2
}
```

An enum pattern must be a qualified member path; an unqualified member name is not an enum pattern.
It binds no payload. Coverage starts with the canonical ordered member set. Each unguarded member arm removes that member; `_` removes all remaining members.
A match without `_` must remove every member. A duplicate member arm or any arm after `_` is
unreachable. A member from another enum is a type error, and integer patterns do not match enum
values. Coverage uses member identity rather than unrestricted integer-pattern semantics.

## Diagnostics

The diagnostic catalog receives stable dedicated codes and structured payloads for:

- empty enum;
- unsupported representation;
- duplicate member name, with the first declaration as a related span;
- duplicate discriminant, with the first declaration as a related span;
- explicit discriminant outside the representation range;
- implicit successor overflow;
- negative discriminant under an unsigned representation;
- unknown member on a resolved enum;
- member accessed through the wrong enum identity;
- integer where an enum is expected and enum where an integer is expected;
- equality between distinct enum types;
- direct enum ordering;
- missing enum members in a match;
- duplicate or post-wildcard unreachable enum arm;
- foreign-enum member in a match; and
- integer pattern against an enum.

Tests assert codes, primary spans, related spans, and structured details rather than message text.
Recovery preserves valid sibling declarations and members and never invents a valid member or
representation after damage.

## Compiler / Standard-Library Boundary

Compiler support is necessary because ordinary Silk source cannot create a closed nominal type with a
primitive scalar ABI, reserve member identities, or make exhaustive coverage aware of the complete
member set.

The enum declaration, qualified members, checked discriminant assignment, nominal equality, and
member-pattern coverage are core language semantics rather than source-callable compiler operations.
The sole source-callable primitive is sealed as `Intrinsic.enumValue`: it projects the already
verified member's discriminant through the enum's representation plan. Each declaration-generated
`EnumName.value` associated wrapper calls that primitive, so the public surface remains concept-led
and backends do not recognize a wrapper or standard-library declaration by spelling.

No standard-library policy is required. Future conversion traits may be ordinary Silk source. The
compiler does not recognize a library declaration by spelling, and backends consume verified MIR
representation plans rather than re-deciding enum validity or width. A smaller primitive cannot
realize the required backing-value observation; any larger primitive would unnecessarily add
integer-to-enum construction, policy, or backend privilege.

A smaller integer-alias model fails because it admits arbitrary integers, cannot preserve nominal
identity, and cannot prove exhaustive member coverage. A structural-union encoding fails because it
models payload alternatives, has a private implementation tag, and does not provide the requested
fixed integer representation.

## Whole-Language Interaction Map

| Surface                 | Status       | Analysis                                                                                                                                                     |
| ----------------------- | ------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Syntax and names        | Affected     | Declaration grammar, representation clause, members, qualified values and patterns, formatting, recovery, module visibility, and one flat nominal namespace. |
| Types and abstraction   | Affected     | Closed nominal identity; no payloads or generics; member paths have the enum type; representation does not create interchangeability.                        |
| Execution contracts     | Not affected | Construction, equality, conversion, and matching are eager, total, failure-free, and requirement-free.                                                       |
| Ownership and resources | Affected     | Every enum is sealed `Copy`, has no cleanup, and follows ordinary Copy binding and move rules.                                                               |
| Runtime and targets     | Affected     | Exact fixed-width integer layout and calling shape; evaluator, Wasm, and native parity; no hidden metadata.                                                  |
| Compiler                | Affected     | Declaration index, resolution, typing, coverage, HIR, ownership facts, MIR, layout, evaluation, and backend lowering.                                        |
| Standard library        | Not affected | No required declaration or compiler-known actor; future integer conversion policy stays in source.                                                           |
| Tooling and diagnostics | Affected     | Dedicated diagnostics, facade queries, canonical formatting, highlighting parity, hover/reference data, and labs snapshots.                                  |
| Learning and use        | Affected     | Teach enum members as values, default `u8`, explicit widening, `.value`, nominal equality, and exhaustive matching; distinguish structural unions.           |

No surface remains unknown.

## Alternatives Rejected

- **Infer the smallest fitting representation, like Zig.** Rejected in favor of deterministic `u8`
  defaulting and explicit widening. Inference makes layout change when members are added and obscures
  ABI review.
- **Always require `enum(u8)`.** Rejected as needless noise for the common representation; omitted
  syntax already has one exact meaning.
- **Allow arbitrary discriminant expressions or root constants.** Rejected for the first version;
  Silk does not support the assumed root-constant model, and literal-only discriminants keep ordering
  and diagnostics local.
- **Call the conversion `toRepresentation`, `toU8`, `tag`, or `raw`.** Rejected. `value` is concise,
  representation-independent, and does not suggest the reverse integer-to-member operation.
- **Provide integer-to-enum conversion.** Rejected because not every backing value denotes a member;
  policy belongs in a future user-implementable API.
- **Permit direct ordering.** Rejected because declaration order and numeric order need not express the
  same intent; `.value` makes numeric ordering explicit.
- **Reuse structural unions.** Rejected because their member types, payloads, and private tags are a
  different abstraction.

## Risks and Evidence Gates

- Logical enum identity could be erased too early and accidentally admit enum/integer mixing. HIR and
  MIR tests must retain canonical enum and member identities through verification.
- Signed values and implicit successor overflow can diverge across JavaScript, Wasm, and LLVM numeric
  models. Discriminant analysis must use checked integer arithmetic independent of host number limits,
  and boundary cases must run through all relevant engines.
- Match coverage could be incorrectly implemented as integer matching. Tests must reject foreign
  members and integer patterns even when their discriminants coincide.
- Backend code could independently choose a lane or synthesize undeclared values. Layout/MIR owns the
  representation plan; backend tests assert exact widths and cross-engine values.
- Parser recovery already exists before semantics. Semantic implementation must consume recovered
  nodes without throwing and preserve unaffected declarations.

The direction is falsified if exact representation layout cannot be maintained across supported
targets without hidden metadata, or if well-typed source can create an enum value not corresponding
to one declared member.
