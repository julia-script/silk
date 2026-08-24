# Values and types

Every Silk expression has one precise type. Expected contexts may select the type of an exact
literal before it becomes a value, but an already-typed value changes type only through one of the
language's explicitly defined compatibility relations.

Ownership behavior is defined by [ownership and borrowing](ownership-and-borrowing.md). Function,
callable, and match result types are defined by
[functions, callables, and control flow](functions-callables-and-control-flow.md). This page defines
the identities, construction rules, and ordinary compatibility of foundational values.

## Terminology

- A **value** is the result represented by a successfully typed expression.
- A **type** classifies a value and determines which source operations may accept it.
- A **precise type** is the expression's own type before an expected boundary performs a compatible
  injection, widening, or access weakening.
- An **expected context** is a source position that already requires a type, such as a declared
  return, parameter, struct field, array element, or assignment destination.
- **Contextual literal selection** chooses a representable type for an exact literal that has not
  yet become a typed value.
- **Compatibility** determines whether an already-typed source value may satisfy an expected type.
- A **scalar** is `bool`, `char`, an integer type, or a floating-point type.
- A **nominal type** is identified by its declaration rather than by the shape of its fields.
- An **aggregate** is a value containing other values, such as a struct, fixed array, or structural
  union payload.
- A **view** provides lexical access to storage it does not own. References, slices, and `string`
  values may retain such access.
- **Injection** places one precise value into a structural union containing its type.
- **Widening** converts one structural union into another containing every source member.

## Foundational type identity and compatibility

### TYPE-001 — Foundational type spellings are lowercase and distinct

**Status:** Confirmed

Silk defines these scalar types:

| Category | Types |
| --- | --- |
| Boolean | `bool` |
| Unicode scalar | `char` |
| Unsigned integers | `u8`, `u16`, `u32`, `u64`, `usize` |
| Signed integers | `i8`, `i16`, `i32`, `i64`, `isize` |
| Floating point | `f32`, `f64` |

The foundational non-scalar types are unit `()`, bottom `never`, and immutable UTF-8 view
`string`. Every spelling denotes a distinct type. Uppercase forms such as `I32`, `Bool`, and
`String` are not aliases; `String` may instead name an ordinary standard-library owning type.

```silk
fn classify(flag: bool, scalar: char, count: usize) -> f64 {
  return 0.0
}
```

**Boundary:** Distinct foundational types are not interchangeable merely because their runtime
representations could have the same width. `u32`, `char`, and `f32` remain three different types.

**Diagnostics:** An unresolved type spelling reports `SEM0001`. A value declaration used where a
type is required reports `SEM0018`.

**Evidence:** [scalar catalog](../../packages/compiler/src/Scalar.ts),
[integer scalar specification](../../openspec/specs/bootstrap-integer-scalars/spec.md),
[floating scalar specification](../../openspec/specs/bootstrap-floating-point-scalars/spec.md),
[string specification](../../openspec/specs/bootstrap-string/spec.md).

### TYPE-002 — Unit has one value and `never` has none

**Status:** Confirmed

`()` is both the unit type's spelling and its sole value. A function result omitted from its
declaration is `()`, bare `return` returns `()`, and reaching the end of a unit body produces `()`.

`never` is uninhabited: no expression completes by producing a `never` value. An expression of type
`never` is compatible with every expected type because control does not reach that boundary with a
value.

```silk
struct StopError {}

effect fn stop() -> never ! StopError {
  fail StopError {}
}

effect fn number() -> i32 ! StopError {
  return run stop()
}
```

The return is valid. `stop` either propagates its typed failure or does not return; it never creates
an `i32` or a bottom payload.

**Boundary:** `never` is not a default value, null value, or runtime union tag. Source cannot
construct, store, inspect, or return a completed `never` value.

**Diagnostics:** Unit return mismatches use the ordinary return diagnostic. A `never` expression
receives no conversion diagnostic when used at another expected type.

**Evidence:** [unit result rule](effect-contracts.md#eff-010--omitting-the-result-annotation-declares-unit),
[failure bottom rule](typed-failures.md#fail-002--fail-follows-ordinary-ownership-rules-and-has-type-never),
[type compatibility](../../packages/compiler/src/TypeCompatibility.ts).

### TYPE-003 — Compatibility is exact except for closed named relations

**Status:** Confirmed

An already-typed value satisfies an expected type when the two types are identical or one of these
relations applies:

1. `never` satisfies any expected type.
2. A precise value injects into a structural union containing its type.
3. A structural union widens into another containing every source member.
4. Callable invocation mode weakens under CALLABLE-003.
5. Effect run access weakens under the Effect ownership rules.

No other subtyping or implicit conversion exists.

```silk
fn preserve(value: u8) -> u8 {
  return value
}
```

**Boundary:** Numeric width, signedness, representation, array length, ownership, text encoding, and
borrow access do not create compatibility. In particular, `i32` does not become `u8` because its
runtime value fits, an array does not decay into a slice, and `string` does not become owned
`String`.

**Diagnostics:** The enclosing boundary selects the diagnostic: arguments use `SEM0012`, struct
fields `SEM0025`, array elements `SEM0030`, assignments `SEM0037`, and incompatible union widening
`SEM0040`. Return mismatches still need one stable general semantic code.

**Evidence:** [compatibility implementation](../../packages/compiler/src/TypeCompatibility.ts),
[callable modes](functions-callables-and-control-flow.md#callable-003--invocation-mode-describes-access-to-the-callable-environment),
[Effect access](ownership-and-borrowing.md#effect-own-002--effect-run-access-derives-from-how-each-run-uses-the-environment).

## Scalar values and literals

### INT-001 — Integer types have exact signedness and width

**Status:** Confirmed

The fixed-width integer name states both signedness and value width. `u8` through `u64` are
unsigned; `i8` through `i64` are signed. `usize` and `isize` use the selected target's pointer width:
64 bits on required native targets and 32 bits on `wasm32-unknown-unknown`.

Every integer type is Copy and cleanup-free. Integer types remain distinct even when two selected
types have the same physical width.

```silk
fn nativeIndex(value: usize) -> usize {
  return value
}

fn fixedIndex(value: u64) -> u64 {
  return value
}
```

`usize` and `u64` are not compatible on a 64-bit target despite using equally wide values there.

**Boundary:** Silk does not promote a narrower integer, change signedness, or convert between fixed
and pointer-sized integers implicitly. Such transformations require explicit operations.

**Diagnostics:** An out-of-range integer literal reports `SEM0002`. A negative literal selected as
`usize` reports `SEM0060`. An already-typed mismatch uses the diagnostic of its expected boundary.

**Evidence:** [integer scalar specification](../../openspec/specs/bootstrap-integer-scalars/spec.md),
[`usize` specification](../../openspec/specs/bootstrap-usize/spec.md),
[scalar catalog](../../packages/compiler/src/Scalar.ts).

### INT-002 — Integer literals are exact until an immediate context selects their type

**Status:** Confirmed

An integer literal retains its exact mathematical magnitude until typed. An immediate expected
integer type selects that type when the magnitude is representable. Without a numeric context, the
literal defaults to `i32`.

```silk
struct Header {
  code: u8
}

fn header() -> Header {
  return Header { code: 255 }
}
```

The field contract selects `u8` for `255`. The literal is not first made `i32` and then narrowed.
Immediate contexts include concrete parameters, returns, struct fields, contextual array elements,
assignment destinations, and a known homogeneous operator operand.

**Boundary:** Context does not retype an existing value:

```silk,ignore
fn invalid() -> Header {
  let code = 255
  return Header { code }
}
```

`code` is already `i32`, so the field reports a mismatch. A later use cannot retroactively change
the binding to `u8`.

**Diagnostics:** A literal outside the selected type's range reports `SEM0002` before MIR lowering.
The diagnostic must retain its exact magnitude rather than a rounded host-number approximation.

**Evidence:** [integer literal specification](../../openspec/specs/bootstrap-integer-scalars/spec.md),
[literal elaboration tests](../../packages/compiler/test/IntegerScalars.test.ts).

### FLOAT-001 — Floating literals select `f32` contextually and otherwise default to `f64`

**Status:** Confirmed

`f32` and `f64` are distinct Copy scalar types using IEEE binary32 and binary64 values. A floating
literal retains its exact source value until a contextual floating type rounds it; without such a
context it defaults to `f64`.

```silk
fn small() -> f32 {
  return 1.25
}

fn defaulted() -> f64 {
  let value = 1.25
  return value
}
```

Basic floating behavior preserves signed zero and keeps NaN unordered. The complete operation and
explicit conversion APIs belong to the expressions and operators reference.

**Boundary:** Integers do not become floats implicitly, and `f32` does not widen to `f64` after it
has been typed. A conversion must be explicit even when mathematically exact.

**Diagnostics:** A floating spelling that cannot produce a supported floating value reports
`SEM0095`. Contextual type mismatches use the enclosing boundary's ordinary diagnostic.

**Evidence:** [floating scalar specification](../../openspec/specs/bootstrap-floating-point-scalars/spec.md),
[floating tests](../../packages/compiler/test/FloatingPointScalars.test.ts).

### CHAR-001 — `char` holds exactly one Unicode scalar value

**Status:** Confirmed

`char` is a Copy 32-bit scalar whose valid values are Unicode scalar values: `0` through
`0x10ffff`, excluding the surrogate range `0xd800` through `0xdfff`. A character literal contains
exactly one scalar, not one byte.

```silk
fn snowman() -> char {
  return '\u{2603}'
}
```

`'é'` is also one `char` even though its UTF-8 encoding uses more than one byte. `char` supports
equality and ordering by scalar value.

**Boundary:** `char` is not an integer type. Arithmetic is unavailable, and conversion to or from
an integer requires an explicit checked or named operation.

```silk
import silk.char { fromU32, toU32 }
import silk.option { Option }

fn checked(value: u32) -> Option<char> {
  return fromU32(value)
}

fn scalarNumber(value: char) -> u32 {
  return toU32(value)
}
```

`fromU32` returns `Some<char>` for `0...0xd7ff` and `0xe000...0x10ffff`. It returns
`None` for surrogate values and larger integers, without truncating or trapping. `toU32` is total
because every existing `char` is already a valid scalar. Canonical string traversal returns
`char`; callers choose `toU32` explicitly when they need its integer value.

**Diagnostics:** A literal containing zero or multiple scalar values reports `LEX0007`. Malformed
escapes and invalid scalar spellings receive their literal diagnostic without constructing a
partial `char`. Supplying `u32` where `char` is required, or `char` where `u32` is required, uses
the ordinary type-mismatch diagnostic; `fromU32` represents an invalid integer as `None` rather
than a diagnostic or trap.

**Evidence:** [character literal specification](../../openspec/specs/bootstrap-lexer/spec.md),
[character scalar catalog](../../packages/compiler/src/Scalar.ts),
[character tests](../../packages/compiler/test/CharacterScalar.test.ts),
[conversion and engine tests](../../packages/compiler/test/IntegerScalars.test.ts).

### TEXT-001 — `string` is immutable UTF-8 text and byte strings are byte views

**Status:** Confirmed

`string` is a Copy immutable view of valid UTF-8, distinct from every scalar and from `&[u8]`.
An ordinary text literal has type `string` and program lifetime. A byte-string literal has type
`&[u8]` and preserves exact bytes.

```silk
fn greeting() -> string {
  return "olá"
}

fn firstByte() -> u8 {
  return b"ok"[0]
}
```

Text-literal equality compares exact decoded Unicode scalar sequences, equivalently their canonical
UTF-8 bytes. It performs no normalization, case folding, or locale-sensitive comparison.

**Boundary:** A byte view remains binary data even when its bytes are valid UTF-8. Forming a
`string` from runtime bytes requires validation or a narrow unsafe operation. A runtime `string`
view retains the lifetime of its backing owner; copying the view does not detach that loan.

**Diagnostics:** Invalid UTF-8, malformed escapes, and invalid byte values report `SEM0085` without
publishing partial static data. Lifetime violations use the ordinary borrow diagnostics.

**Evidence:** [static text specification](../../openspec/specs/bootstrap-static-text/spec.md),
[string specification](../../openspec/specs/bootstrap-string/spec.md),
[string ownership tests](../../packages/compiler/test/StringOwnership.test.ts).

### TEXT-002 — Text conversions and access units are explicit

**Status:** Confirmed

Silk does not implicitly convert between `string`, owned `String`, and `&[u8]`. Conversions that
copy into owned storage, borrow owned storage, expose UTF-8 bytes, or validate bytes are explicit
operations. No conversion allocates invisibly.

`string` has no index operator and no unitless `length`. APIs name their observation unit, such as
byte length, UTF-8 bytes, Unicode scalars, or grapheme clusters.

**Boundary:** A text literal does not become an owning `String` because a field or parameter expects
one. `text[0]` is invalid because the intended unit could be a byte, Unicode scalar, or grapheme.
Because `string` is an ordinary Copy view value, it may appear in valid aggregate positions and may
itself be borrowed: `&string`, `&mut string`, and `&[string]` follow the ordinary reference,
mutation, slice, aggregate-storage, and nested-loan rules. Mutating through `&mut string` may replace
the view value; it does not make the viewed UTF-8 storage mutable. A containing value cannot outlive
a runtime string view's backing owner merely because the view is nested.

**Diagnostics:** Indexing `string` reports the non-indexable-type diagnostic. Implicit text
conversions use the enclosing type-mismatch diagnostic.

**Evidence:** [string access and conversion specification](../../openspec/specs/bootstrap-string/spec.md),
[string type diagnostics](../../packages/compiler/test/DeclarationIndex.test.ts).

## Nominal struct values

### STRUCT-001 — A struct declaration creates one nominal type

**Status:** Confirmed

A top-level `struct` declaration creates a type identified by its canonical module and declaration,
including its generic arguments when present. Field shape, import alias, source traversal order, and
target layout do not participate in nominal identity.

```silk
struct ScreenPosition { x: i32 }
struct WorldPosition { x: i32 }
```

`ScreenPosition` and `WorldPosition` are incompatible even though their fields have the same name
and type. A zero-field struct such as `struct End {}` is an ordinary nominal marker type.

Every field has one explicit type, fields are ordered by declaration, and field names are unique.
Structs and fields are private by default; `pub` exposes them under the module visibility rules.

**Boundary:** Silk has no shape-based struct compatibility. A public contract cannot expose a
private nominal type, and a direct or mutual inline field cycle does not acquire hidden indirection.

**Diagnostics:** Duplicate fields report `SEM0017`; a public contract exposing a private type
reports `SEM0019`; inline recursive layouts report `SEM0020`.

**Evidence:** [struct type specification](../../openspec/specs/bootstrap-struct-types/spec.md),
[declaration index](../../packages/compiler/src/DeclarationIndex.ts).

### STRUCT-002 — Raw struct construction is complete and visibility-based

**Status:** Confirmed

A raw struct literal is available wherever every required field is visible. It must initialize
every field exactly once with a compatible value. Initializers evaluate in source order, while the
completed value retains canonical declaration field order.

```silk
struct Point {
  pub x: i32
  pub y: i32
}

fn origin() -> Point {
  return Point { y: 0, x: 0 }
}
```

Because both fields are public, another module may construct `Point` directly. A type with any
private required field instead preserves construction control and exposes visible ordinary
constructor functions when external construction is intended.

**Boundary:** Unknown, duplicate, missing, inaccessible, or mistyped initializers produce no
partially initialized value. Construction never fills omitted fields with defaults. A diagnostic
for hidden required fields does not expose their names or types. A declarationless opaque nominal
type, such as a runtime handle, has no source constructor; an empty semantic field list does not
turn it into an ordinary zero-field struct.

**Diagnostics:** Unknown fields report `SEM0022`; duplicates `SEM0023`; missing visible fields
`SEM0024`; incompatible field values `SEM0025`. Inaccessible construction needs one stable semantic
code that does not reveal hidden field details.

**Current compiler:** Aligned. Construction resolves every named initializer to its canonical field,
checks that field's visibility, and uses `SEM0021` when a required field is inaccessible or the
nominal type has no source struct declaration.

**Evidence:** [struct value tests](../../packages/compiler/test/StructValues.test.ts),
[struct literal elaboration](../../packages/compiler/src/Elaboration.ts).

### STRUCT-003 — Field projection follows the declared nominal field

**Status:** Confirmed

`value.field` resolves the subject's nominal type and the field declared by that type. Nested
projection associates from left to right and preserves the access mode of the underlying place.

```silk
struct Position { pub start: i32 }
struct Token { pub position: Position }

fn start(token: &Token) -> i32 {
  return token.position.start
}
```

The final expression has the declared type `i32`. Reading that Copy leaf does not move `token` or
its `Position` field.

**Boundary:** A non-struct has no fields. A field name is resolved only against the subject's actual
nominal declaration; Silk does not search other same-shaped structs or extension namespaces.
Private fields are inaccessible outside their defining module.

**Diagnostics:** Projection from a non-struct reports `SEM0026`; an unknown field reports `SEM0027`;
an inaccessible field reports `SEM0028`.

**Evidence:** [struct projection specification](../../openspec/specs/bootstrap-struct-values/spec.md),
[projection ownership](ownership-and-borrowing.md#own-002--ordinary-reads-copy-copy-values-and-never-consume-an-affine-owner).

### STRUCT-004 — Inline aggregate dependencies must be finite

**Status:** Confirmed

Every value stored directly inside a struct contributes to its finite inline type dependency.
Acyclic nesting is valid regardless of declaration order. A direct or mutual cycle made only of
inline fields is invalid because it has no finite complete value.

```silk
struct Position { value: i32 }
struct Span {
  start: Position
  end: Position
}
```

**Boundary:** `struct Node { next: Node }` is invalid. The compiler does not guess a pointer or make
the field optional. Recursion requires an explicit finite or indirect representation supplied by
ordinary library types. A zero-length `[Node; 0]` still retains `Node` in its type identity and does
not erase an otherwise recursive dependency.

**Diagnostics:** Every member of one inline recursive component receives the canonical `SEM0020`
cycle diagnostic without losing its nominal declaration identity.

**Evidence:** [inline dependency specification](../../openspec/specs/bootstrap-struct-types/spec.md),
[inline reach tests](../../packages/compiler/test/InlineStructReach.test.ts).

### STRUCT-005 — Struct ownership follows the explicit Copy contract

**Status:** Confirmed

A struct is affine unless it explicitly requests `Copy`. The compiler accepts `impl Copy` only when
every field is Copy and the struct has no cleanup behavior. Copying never runs user code.

Aggregate moves, partial-move prohibition, mutation, and cleanup are defined by the ownership
reference rather than by field shape alone.

**Boundary:** A field-only scalar struct does not become Copy automatically. An owner of allocated
memory cannot request Copy merely because its physical representation contains a copyable pointer.

**Diagnostics:** Invalid implicit copying reports `OWN0003`. An invalid `impl Copy` reports
`SEM0083` and identifies the first affine, cleanup-bearing, cyclic, or unavailable reason.

**Evidence:** [owned value classification](ownership-and-borrowing.md#own-001--every-value-type-is-either-copy-or-affine).

## Fixed arrays, references, and slices

### ARRAY-001 — A fixed array type includes its element type and length

**Status:** Confirmed

`[T; N]` is one inline fixed-array type whose identity contains the canonical element type `T` and
non-negative integer length `N`. Different lengths are different types. Nested and zero-length
arrays retain every element type and length in their identity.

```silk
fn pair(values: [i32; 2]) -> [i32; 2] {
  return values
}
```

`[i32; 2]` is incompatible with `[i32; 3]`. `[Token; 0]` remains distinct from `[End; 0]` even
though neither contains a runtime element.

**Boundary:** `Array<T, N>` is a compiler display encoding found in older artifacts, not valid Silk
source syntax. Length is not inferred across a declared parameter or result type.

**Diagnostics:** Malformed fixed-array syntax receives a parser diagnostic. A contextual array
literal with the wrong length reports `SEM0031`.

**Evidence:** [fixed-array source syntax](../../openspec/specs/bootstrap-syntax/spec.md),
[fixed-array specification](../../openspec/specs/bootstrap-fixed-arrays/spec.md),
[array ownership](ownership-and-borrowing.md#own-007--array-ownership-derives-from-the-element-type).

### ARRAY-002 — An array literal constructs one homogeneous complete value

**Status:** Confirmed

An array literal evaluates elements once from left to right. With an expected `[T; N]`, every
element is analyzed in immediate `T` context and the written length must be `N`. Without an expected
array type, the first available element selects the precise element type, later elements must be
compatible with it, and the written count becomes the length.

```silk
fn bytes() -> [u8; 3] {
  return [1, 2, 3]
}

fn defaults() -> [i32; 3] {
  let values = [1, 2, 3]
  return values
}
```

The first literal uses contextual `u8`; the second defaults its first element to `i32` and retains
that element type. `[]` requires an expected array type because it has no element from which to
select `T`.

**Boundary:** An uncontextualized heterogeneous literal does not invent a union or numeric common
type. Invalid elements do not create a partially initialized array.

**Diagnostics:** An empty literal without context reports `SEM0029`; an incompatible element
reports `SEM0030` at that element; a contextual length mismatch reports `SEM0031` at the literal.

**Evidence:** [fixed-array specification](../../openspec/specs/bootstrap-fixed-arrays/spec.md),
[array elaboration](../../packages/compiler/src/Elaboration.ts).

### INDEX-001 — Array and slice indexing uses checked `usize`

**Status:** Confirmed

`subject[index]` requires a fixed array or slice subject and a `usize` index. A known out-of-range
literal is rejected during analysis. A dynamic index checks `index < length` at runtime and traps
before reading, projecting, or evaluating a replacement value.

```silk
fn read(values: [i32; 3], index: usize) -> i32 {
  return values[index]
}
```

Fixed arrays use their type-level length; slices use their runtime length. Zero-length and
zero-sized-element values retain their logical bounds.

**Boundary:** An `i32` variable is not an index even when non-negative. Indexing a `string` is
invalid because text access must name its unit. Ownership determines whether reading or replacing
the selected element is valid.

**Diagnostics:** A non-indexable subject reports `SEM0032`; a non-`usize` index `SEM0033`; a known
out-of-bounds index `SEM0034`. A dynamic overrun is a runtime trap rather than a typed failure.

**Evidence:** [fixed-array indexing](../../openspec/specs/bootstrap-fixed-arrays/spec.md),
[slice indexing](../../openspec/specs/bootstrap-runtime-slices/spec.md),
[index diagnostics](../../packages/compiler/src/Diagnostic.ts).

### VIEW-001 — References and slices include access mode in their type

**Status:** Confirmed

`&T` and `&mut T` are shared and exclusive lexical references to one complete `T`. `&[T]` and
`&mut [T]` are shared and exclusive runtime-length contiguous views. Slice type identity includes
element type and access mode, but not the source array's length.

```silk
fn first(values: &[i32]) -> i32 {
  return values[0]
}

fn use() -> i32 {
  let values = [1, 2]
  return first(&values)
}
```

The explicit borrow converts neither array ownership nor array type. It creates a view for the
borrow's lexical lifetime. Arrays of different lengths may therefore be borrowed for the same
`&[T]` parameter.

**Boundary:** There is no implicit array-to-slice decay. Shared access cannot strengthen to
exclusive access, and borrowed views cannot become owned values. Valid type positions, reborrowing,
returned views, storage restrictions, and loan endings follow the ownership reference.

**Diagnostics:** Invalid borrowed-view positions report `SEM0054`; invalid borrow positions and operands use
`SEM0055` and `SEM0056`; exclusive borrowing of an immutable root uses `SEM0057`; invalid reborrowing
uses `SEM0058`; implicit array decay uses `SEM0059`.

**Evidence:** [runtime slice specification](../../openspec/specs/bootstrap-runtime-slices/spec.md),
[borrow rules](ownership-and-borrowing.md#borrow-001--shared-borrows-permit-only-shared-access-while-live),
[returned views](ownership-and-borrowing.md#view-001--an-ordinary-function-may-return-one-source-borrowed-view).

## Structural unions and inference

### UNION-001 — A structural union is a normalized set of ordinary value types

**Status:** Confirmed

`A | B` denotes a finite, unordered, duplicate-free set of canonical ordinary value types. Nested
unions flatten, member order does not affect identity, duplicate members disappear, `never` is the
empty union, and a one-member union normalizes to that member. Members need not be nominal: scalars,
arrays, `string`, and other detached concrete value types use the same union operation. A callable
or Effect member must additionally retain one finite exact, opaque, or composite representation;
its bare structural contract has no standalone storage layout.

```silk
struct Token { kind: i32 }
struct End {}

fn next(done: bool) -> Token | End {
  if done {
    return End {}
  }
  return Token { kind: 1 }
}
```

`Token | End`, `End | Token`, and `Token | (End | Token)` are the same type.

```silk
fn describe(code: i32) -> i32 | string {
  if code == 0 {
    return "none"
  }
  return code
}
```

Executable members use their ordinary representation syntax:

```silk
fn add(left: i32, right: i32) -> i32 { return left + right }

fn selected() -> typeof(add) | i32 {
  return add
}
```

`typeof(add)` contributes the callable's exact finite environment plan. An opaque result binder can
do the same for an Effect construction without exposing its private runner identity. By contrast,
`fn(i32) -> i32 | i32` and `Effect<i32> | i32` are invalid: those structural contracts alone do not
identify storage.

**Boundary:** Union formation does not erase ownership, lifetime, Effect requirements, callable
access, or another member property. A lexical borrow cannot become an owned union member; a union
never makes it detached. Requirement rows remain capability rows rather than value unions.

Generic unions normalize again after monomorphic substitution. If `A | B` specializes with both
parameters equal to `i32`, the instance carries `i32`, not two indistinguishable tags. HIR retains
the authored mapping and MIR deterministically recomputes the concrete mapping and canonical order.

**Diagnostics:** An unresolved or otherwise unavailable member reports that member's ordinary type
diagnostic. A borrow or bare executable contract reports `SEM0039` because it has no detached finite
storage plan. A valid non-nominal or represented executable member produces no diagnostic.

**Current compiler:** Aligned. Normalization, compatibility, target layout, ownership, cleanup,
HIR/MIR mappings, evaluation, LLVM, and Wasm consume canonical ordinary member identities. Exact and
opaque executable representations remain compiler-private while their public contract spelling is
preserved.

**Evidence:** [union normalization](../../packages/compiler/src/Type.ts),
[ordinary failure values](typed-failures.md#fail-001--any-concrete-detached-value-may-be-a-typed-failure).

### UNION-002 — Precise injection and union widening occur only at immediate expected boundaries

**Status:** Confirmed

A precise value is compatible with an expected union containing its type. A union value is
compatible with a wider expected union only when the target contains every source member.

```silk
struct Token { kind: i32 }
struct End {}
struct Fault {}

fn widen(value: Token | End) -> Token | End | Fault {
  return move value
}
```

The conversion preserves the active member and complete payload. Immediate union contexts
include declared returns, parameters, struct fields, contextual array elements, and assignments.

**Boundary:** Compatibility never subtracts or guesses a member. `Token | Fault` cannot become
`Token | End`, and a value does not narrow merely because control reaches a use that wants one
member. Pattern matching performs explicit narrowing.

Union conversion does not rewrite the source expression's precise type. Runtime tags distinguish
members even when their layouts happen to match, but remain internal deterministic compiler data
with no source-visible, serialization, or stable ABI identity.

**Diagnostics:** A target missing any source member reports `SEM0040`, naming the source, target,
and uncovered members. Ownership diagnostics still apply when moving an affine payload.

**Evidence:** [structural union specification](../../openspec/specs/bootstrap-structural-unions/spec.md),
[compatibility implementation](../../packages/compiler/src/TypeCompatibility.ts),
[match narrowing](functions-callables-and-control-flow.md#match-004--matching-narrows-only-inside-the-selected-arm).

### INFER-001 — A binding keeps the precise type of its initializer

**Status:** Confirmed

An unannotated local binding receives the precise type of its initializer after literal selection.
Later uses do not widen, narrow, or otherwise rewrite that type. Each use is checked independently
against its own expected context.

```silk
struct Token { kind: i32 }
struct End {}

fn accept(value: Token | End) -> i32 {
  return match move value {
    Token { kind } => kind
    End {} => 0
  }
}

fn use() -> i32 {
  let token = Token { kind: 42 }
  return accept(move token)
}
```

`token` remains `Token`; only the call argument injects it into `Token | End`.

**Boundary:** Expected types do not flow backward through an already-completed binding:

```silk,ignore
fn acceptByte(value: u8) {}

fn invalid() {
  let value = 1
  acceptByte(value)
}
```

`value` is `i32`, so the call is invalid. Writing the literal directly as `acceptByte(1)` permits
the parameter to select `u8` before the literal becomes a value.

**Diagnostics:** Binding inference itself produces no diagnostic when the initializer has a type.
An unavailable initializer leaves the binding unavailable. A later incompatible use reports at that
use, such as `SEM0012` for the call above.

**Evidence:** [semantic binding facts](../../openspec/specs/bootstrap-semantic-facts/spec.md),
[literal contextualization](../../packages/compiler/src/Elaboration.ts),
[union conversion specification](../../openspec/specs/bootstrap-structural-unions/spec.md).
