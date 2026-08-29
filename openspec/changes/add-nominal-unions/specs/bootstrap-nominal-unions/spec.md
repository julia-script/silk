## Purpose

Define closed nominal tagged unions whose unit and named-field variants share one declared generic
type while remaining distinct from scalar enums and open structural unions.

## ADDED Requirements

### Requirement: A union declares one closed nominal variant set

`union Name { ... }` SHALL declare one nonempty, source-ordered set of uniquely named variants under
one canonical nominal type. A variant SHALL be either unit or contain one or more named fields;
`Variant {}` SHALL be rejected in favor of the unit spelling `Variant`. Tuple variants,
variant-local generic parameters, discriminants, and representation clauses SHALL NOT be admitted.
The union declaration's visibility SHALL govern its variant names, while payload fields SHALL use
the same default-private visibility, public-exposure, and declaration rules as struct fields.

#### Scenario: Declare mixed unit and field variants

- **WHEN** `union HttpError { Timeout, Dns { pub code: u16 } }` is analyzed
- **THEN** `Timeout` and `Dns` are canonical variants of one nominal `HttpError` type and `Dns.code` is a canonical field of that variant

#### Scenario: Reject duplicate variants

- **WHEN** one union declares the same variant name twice
- **THEN** analysis reports the later declaration with the first variant's span related and does not invent a second canonical identity

#### Scenario: Reject an empty union

- **WHEN** a union declaration contains no variants
- **THEN** analysis reports a deterministic empty-union diagnostic while preserving unrelated declarations

#### Scenario: Reject an empty named-field variant

- **WHEN** a union declares `Empty {}` instead of unit variant `Empty`
- **THEN** analysis reports the empty named-field body and retains no second unit-like variant form

### Requirement: Variant construction selects an instantiated parent union

A constructor qualifier SHALL resolve a canonical union declaration plus a contiguous explicit
prefix of its parent arguments. Unit selection SHALL require a complete parent application and
construct it directly. Named-field construction MAY complete an omitted argument suffix from its
supplied fields under the ordinary struct-construction inference rules. It SHALL initialize every
declared field exactly once, and every field SHALL be accessible at the construction site; any
inaccessible required field SHALL fence off raw construction for the complete variant. Variant names
SHALL NOT create detached nominal types or unqualified module bindings.

#### Scenario: Construct an explicitly applied unit variant

- **WHEN** source constructs `Option<i32>.None`
- **THEN** the expression has the precise nominal type `Option<i32>` without a payload or allocation

#### Scenario: Infer a payload argument

- **WHEN** `Option.Some { value: 42 }` supplies the only field of `Some<T>` and no type argument
- **THEN** construction infers `T = i32` from that field and produces `Option<i32>`

#### Scenario: Keep parent-only parameters explicit

- **WHEN** `Result.Success { value: 42 }` leaves error parameter `E` absent from every supplied field
- **THEN** construction reports `E` as uninferred even if an expected result type mentions `Result<i32, Problem>`

#### Scenario: Fence raw construction with one private field

- **WHEN** another module selects a public union variant containing one required private field
- **THEN** raw construction is unavailable even when every public field is supplied, while visible factory functions remain callable

### Requirement: Payload access requires active variant selection

A value of the parent union type SHALL expose no directly projectable payload field, even when every
variant declares the same field spelling and type. Source SHALL select a variant through a pattern
before it can bind, borrow, move, or write that variant's fields. Failed whole-union projection SHALL
retain the parent and candidate field facts without fabricating a common field identity.

#### Scenario: Reject projection from the parent value

- **WHEN** source evaluates `result.value` where `result` has type `Result<A, E>`
- **THEN** analysis rejects the projection and requires successful variant selection before payload access

### Requirement: Invalid variants make the parent non-executable

A duplicate, unidentified, unresolved, visibility-invalid, or otherwise unavailable variant or
field SHALL make the complete applied union unavailable for construction, exhaustive coverage,
layout, HIR, MIR, and execution. The canonical parent identity and independent sibling facts SHALL
remain queryable for parser recovery, diagnostics, navigation, and editing.

#### Scenario: Preserve siblings without publishing a partial union

- **WHEN** one payload field type is unresolved beside valid unit and field variants
- **THEN** analysis retains every independent declaration fact but publishes no executable application of the incomplete parent union

### Requirement: Nominal unions follow nominal struct behavior

A union SHALL be affine by default and SHALL admit `Copy`, `Drop`, operator, and interface
implementations under the same declaration, bound, coherence, and admissibility rules as a nominal
struct. Generic payload fields SHALL be checked once under declared bounds. Direct inline recursive
storage SHALL be rejected by the same finite-layout rule as structs, while explicit indirection MAY
make recursion finite.

#### Scenario: Admit conditional Copy

- **WHEN** `impl<T: Copy> Copy for Option<T>` is checked and every variant payload is Copy under that bound
- **THEN** the implementation is accepted for matching concrete applications and the union is not otherwise inferred Copy

#### Scenario: Reject inline recursive storage

- **WHEN** a variant field stores its own union type directly with no indirection
- **THEN** layout analysis reports the recursive nominal cycle and publishes no finite layout

### Requirement: Enum structural-union and nominal-union concepts remain distinct

A scalar `enum` SHALL remain a fieldless fixed-width enumeration, `A | B` SHALL remain an open
normalized structural union of types, and a declared `union` SHALL remain one closed nominal type
with subordinate variants. No spelling, shape, or specialization SHALL implicitly convert among
those three declaration or type concepts.

#### Scenario: Preserve three distinct abstractions

- **WHEN** a program declares a scalar enum, a payload-bearing nominal union, and a structural union containing that nominal union
- **THEN** analysis retains three distinct canonical concepts and never treats a nominal variant as a detached structural member
