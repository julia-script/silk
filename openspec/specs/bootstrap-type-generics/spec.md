# bootstrap-type-generics Specification

## Purpose

Define type-only generic declarations and calls whose reachable concrete uses are checked once,
specialized finitely, and erased into deterministic monomorphic runtime instances.

## Requirements

### Requirement: Declarations bind canonical type parameters

Struct and function declarations SHALL accept ordered type-only parameters whose identities are
local to that declaration and distinct from nominal types and parameters with the same spelling in
another declaration. A type parameter SHALL be available throughout its declaration's fields,
parameter types, return type, and body, and duplicate or unbound parameters MUST produce
deterministic diagnostics.

#### Scenario: Bind one generic struct parameter
- **WHEN** `pub struct Box<T> { pub value: T }` is analyzed
- **THEN** the field type refers to the canonical `T` parameter owned by `Box`, not to a nominal type named `T`

#### Scenario: Reject a duplicate parameter
- **WHEN** a declaration introduces `<T, T>`
- **THEN** analysis reports the second parameter as a deterministic duplicate without fabricating another identity

### Requirement: Generic applications are explicit canonical types

Applying a generic nominal declaration SHALL require exactly one concrete type argument per declared
parameter and SHALL produce a canonical applied type identified by the declaration plus normalized
arguments. Applying arguments to a non-generic declaration, omitting arguments in a required type
position, or supplying the wrong arity MUST remain explicit semantic failures.

#### Scenario: Reuse one applied type identity
- **WHEN** independent declarations refer to `Box<Token>`
- **THEN** both references resolve to the same canonical applied type identity

#### Scenario: Reject the wrong arity
- **WHEN** `Pair<I32>` refers to a declaration with two type parameters
- **THEN** analysis reports the expected and actual argument counts and produces no available applied type

### Requirement: Calls infer only from supplied arguments

A complete generic call SHALL either supply the complete ordered type-argument list or infer every
type argument from its supplied call arguments. Forming an automatic leading-argument section SHALL
infer from the supplied trailing arguments and retain any unresolved parameter that is determined
by the omitted leading parameter in the section's callable type; applying that section SHALL
complete inference from the leading argument. Partial explicit type-argument lists, expected return
types, and uses after the complete application MUST NOT contribute inference. Missing, conflicting,
or excess arguments MUST produce deterministic diagnostics at the responsible section or
application.

#### Scenario: Infer identity from its argument
- **WHEN** `identity(value)` calls `identity<T>(value: T)` with a `Token`
- **THEN** the call specializes `T` as `Token`

#### Scenario: Infer through a generic section

- **WHEN** a generic data-first function forms a section from trailing arguments and is then piped a leading `Token`
- **THEN** the complete application resolves one canonical `Token` specialization

#### Scenario: Refuse return-only inference
- **WHEN** `empty()` calls `empty<T>() -> T` without explicit type arguments
- **THEN** specialization fails even when the call result is later used where `Token` is expected

#### Scenario: Specialize explicitly
- **WHEN** `empty<Token>()` calls `empty<T>() -> T`
- **THEN** the call records the concrete `Token` specialization

### Requirement: Callable specialization remains finite and monomorphic

Generic function references, sections, callable fields, and higher-order applications SHALL reach
runtime only through deterministic concrete callable instances. Specialization MUST NOT introduce
runtime generic dictionaries, type descriptors, or unbounded polymorphic closure families.

#### Scenario: Specialize one generic mapper twice

- **WHEN** the same generic mapper section is reached for `I32` and `Token`
- **THEN** instance discovery records exactly two concrete callable environments and terminates

### Requirement: Generic bodies are checked once

The compiler SHALL elaborate and check each generic body once over its canonical type parameters.
Concrete specialization MUST substitute the verified generic facts and MUST NOT enable undeclared
operations through concrete duck typing or type-directed source branching. Copyability and cleanup
SHALL remain compiler-owned type properties available to generic ownership checking.

#### Scenario: Preserve a generic whole-value move
- **WHEN** `identity<T>(value: T)` returns `move value`
- **THEN** ownership checks that transfer once over `T` and every concrete specialization reuses the proof

#### Scenario: Reject undeclared concrete behavior
- **WHEN** an unconstrained generic body calls an operation unavailable for its type parameter
- **THEN** the declaration is rejected before any concrete specialization can make the call appear valid

### Requirement: Runtime specialization is finite and monomorphic

Runtime instance discovery SHALL key each generic function by its canonical declaration and
normalized concrete type arguments, record the key before following dependencies, and require every
recursive generic call to preserve its current type arguments. MIR, evaluation, and backend emission
MUST receive only concrete monomorphic instances and MUST NOT require runtime generic dictionaries or
type descriptors.

#### Scenario: Discover two concrete instances
- **WHEN** the entry reaches `identity<I32>` and `identity<Token>`
- **THEN** discovery records exactly two deterministic instance keys and lowering produces two concrete MIR functions

#### Scenario: Terminate ordinary generic recursion
- **WHEN** `walk<T>` recursively calls `walk<T>`
- **THEN** discovery reuses the already recorded instance key rather than expanding a new instance

#### Scenario: Reject polymorphic recursion
- **WHEN** a recursive generic call changes its current type arguments
- **THEN** analysis rejects the call before instance discovery can expand indefinitely

### Requirement: Generic artifacts are deterministic

Canonical applied types, substitutions, instance keys, concrete symbols, layouts, encodings, and
diagnostics SHALL be deterministic across fresh processes for equivalent source and target inputs.

#### Scenario: Repeat specialization artifacts
- **WHEN** the same multi-specialization program is compiled repeatedly in fresh processes
- **THEN** its generic facts, instance ordering, layouts, MIR text, and emitted symbols are byte-identical
