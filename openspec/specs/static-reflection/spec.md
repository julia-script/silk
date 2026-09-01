# Static Reflection Specification

## Purpose

Define finite compile-time inspection and heterogeneous generation over aggregate types while
preserving nominal typing, visibility, explicit staging, and a reflection-free runtime program.

## Requirements

### Requirement: Aggregate reflection produces finite static descriptors

Static reflection SHALL expose one deterministic type descriptor for a concrete tuple, anonymous
record, or named aggregate type and one ordered field descriptor for every field visible at the
reflection site. A type descriptor SHALL retain aggregate kind and nominal identity. A field
descriptor SHALL retain its owner, declaration ordinal, positional ordinal or source label, concrete
field type, visibility authorization, and source provenance. Descriptors SHALL be static-only,
identity-free values with no runtime representation, address, ownership, callable behavior, or
backend metadata.

Separate nominal aggregate types MUST retain separate type and field descriptor identities even
when their visible members have equal names, positions, types, or layouts. Reflection MUST NOT make
assignment, equality, overload selection, interface conformance, or construction structural.

The descriptor surface SHALL consist of sealed phase-only intrinsic nominals `Type<Owner>`,
`Fields<Owner>`, and `Field<Owner, Value>`. Their generic arities and owner/value relationships are
part of the type contract. A descriptor occurrence in a residual signature, binding, or call MUST
report a phase violation before runtime HIR is published.

#### Scenario: Reflect an anonymous named argument pack

- **WHEN** static source reflects the type inferred for `.{ name: "Julia", age: 32 }`
- **THEN** it receives one anonymous-record descriptor followed by `name: string` and `age: i32` field descriptors in literal declaration order

#### Scenario: Keep equal aggregate shapes nominally distinct

- **WHEN** two distinct named structs expose equal public fields
- **THEN** their type descriptors, field owners, and projection authority remain distinct

### Requirement: Reflection preserves aggregate encapsulation

Reflection SHALL expose every position of a tuple or anonymous positional aggregate and every field
of an anonymous named aggregate produced by the reflected value. For a source-declared named
aggregate, reflection SHALL expose only fields ordinarily visible from the reflecting declaration.
An inaccessible field MUST NOT become nameable, iterable, projectable, or diagnosable through its
hidden spelling merely because a generic function specializes on the owner type.

#### Scenario: Exclude a private named field

- **WHEN** a generic formatter outside a struct's module reflects that struct
- **THEN** its public fields are available in declaration order and its private fields contribute no descriptor or hidden name

#### Scenario: Expose every anonymous record field

- **WHEN** a caller supplies an anonymous record as a generic argument pack
- **THEN** every literal field is statically iterable and projectable through that occurrence's anonymous nominal identity

### Requirement: Static aggregate values support ordinary typed member projection

An aggregate value admitted to static evaluation SHALL support ordinary member projection using its
existing nominal field identity and concrete substituted field type. Projection SHALL return the
already-admitted nested static value and SHALL create no runtime projection, borrow, ownership,
cleanup, or residual fact. This change MUST NOT implicitly admit static union pattern matching.

#### Scenario: Inspect a homogeneous static parser record

- **WHEN** a static function projects the enum mode and byte-range fields of an admitted `Part` struct
- **THEN** each projection returns its concrete static value and no runtime aggregate access is published

#### Scenario: Reject an unknown static member

- **WHEN** static source projects a member that is absent from the aggregate's nominal declaration
- **THEN** ordinary member resolution reports the missing member and no static or runtime projection is produced

### Requirement: Static sequences are immutable value-semantic compiler data

Silk SHALL provide an ordinary source abstraction for finite homogeneous static sequences. Static
source MAY create an empty sequence, append or concatenate admitted values by producing a new
sequence, inspect its length, and read an in-bounds element. A sequence SHALL be deterministic,
identity-free, freely reusable, and accounted for by the existing static retained-value and step
budgets. It MUST NOT have a runtime layout, allocator, capacity, address, reference, mutable alias,
destructor, or observable copy count.

The sequence abstraction SHALL be available only during static evaluation. Attempting to retain a
sequence in residual code, place it in a runtime calling shape, or derive a runtime reference from it
SHALL report a static-phase violation.

#### Scenario: Build a template plan by complete replacement

- **WHEN** a static function loops over text and repeatedly replaces a local sequence with the result of appending one parsed token
- **THEN** it returns one finite canonical sequence without requiring a source-visible allocator, borrow, or in-place mutation

#### Scenario: Reject a sequence at runtime

- **WHEN** a mixed function passes a static sequence to an ordinary runtime operation
- **THEN** specialization reports a phase violation and publishes no partial residual body

### Requirement: Static for re-elaborates one body per static element

`static for <binding> in <expression> { <statements> }` SHALL require a finite statically evaluated
iterable. It SHALL elaborate its body independently in deterministic element order, binding the
current element as a static value whose concrete type and descriptor facts are available to that
iteration. A heterogeneous iterable MAY therefore give different concrete binding types to
different iterations. The body MAY use runtime bindings and retain ordinary runtime operations;
those operations SHALL be appended to residual control flow in iteration order and SHALL NOT
execute during compilation.

A zero-element iteration SHALL contribute no residual operation. The form SHALL be a statement and
MUST NOT introduce declarations or infer a runtime loop. Runtime iterables, unbounded producers,
Effects, services, host inputs, and external access SHALL remain unavailable.

Expansion SHALL be atomic. If iterable evaluation, budget charging, or any iteration fails, no HIR,
instance-selection, ownership, or cleanup fact generated by an earlier element may become
executable or published. Step, call-depth, retained-value, and residual-growth budgets SHALL be
charged while each element is evaluated and re-elaborated.

#### Scenario: Generate one display operation per tuple position

- **WHEN** a mixed function statically iterates the reflected fields of `(string, i32)` and displays each projected runtime value
- **THEN** specialization elaborates one `string` display call and one `i32` display call in positional order without a runtime reflection loop

#### Scenario: Iterate an empty static sequence

- **WHEN** `static for` receives an empty admitted sequence
- **THEN** its body is never semantically elaborated and it contributes no runtime operation or diagnostic

#### Scenario: Roll back a failed later iteration

- **WHEN** an earlier element generates a valid call and a later element fails interface selection or a static budget
- **THEN** specialization publishes none of the loop's generated HIR, instance, ownership, or cleanup facts

### Requirement: Static field projection retains ordinary runtime access

A static field descriptor SHALL authorize projection of exactly its field from a shared runtime
reference to its concrete owner type. Each specialized projection SHALL have the descriptor's
concrete field reference type and SHALL residualize as an ordinary field projection with no runtime
descriptor parameter. Projection MUST reject the wrong owner type, an inaccessible field, a runtime
descriptor, an owned-value projection that would bypass movement rules, or any use that would let a
reference outlive its owner.

#### Scenario: Project a field from a borrowed temporary

- **WHEN** a generic function receives `&.{ name: "Julia", age: 32 }` and projects `age` through its static descriptor
- **THEN** the residual program reads `i32` through the hidden temporary owner's shared borrow and cleans the owner after the borrow ends

#### Scenario: Reject a descriptor-owner mismatch

- **WHEN** source applies a field descriptor from one nominal struct to a reference of another equal-shaped struct
- **THEN** specialization reports the owner mismatch and emits no projection

### Requirement: Static reflection leaves no runtime reflection system

Successful specialization SHALL erase type descriptors, field descriptors, static sequences,
static iterators, and static loop bindings before runtime ownership, MIR, evaluation, or backend
lowering. Only the generated ordinary constants, projections, calls, control flow, and source
provenance SHALL remain. Equal source, target, type arguments, evidence, static arguments, and
reflection inputs SHALL produce byte-identical iteration order, residual operations, diagnostics,
and specialization identities.

#### Scenario: Inspect a reflection-generated specialization

- **WHEN** semantic inspection and every execution engine consume a function generated from static field iteration
- **THEN** they observe the same ordinary residual field projections and calls and no runtime descriptor, reflection table, iterator, or template metadata
