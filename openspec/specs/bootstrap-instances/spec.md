# bootstrap-instances Specification

## Purpose
Deterministic discovery of the concrete runtime instances reachable from the user entry:
instance keys over canonical declaration identities, and the recorded worklist whose
record-before-follow discipline makes ordinary recursion terminate — the structure the full
generic language inherits, degenerate while the slice has no type or contract-row arguments.
## Requirements
### Requirement: Instances are discovered from the entry by a recorded worklist

Instance discovery SHALL start from the user entry — the root module's unique zero-parameter
public `I32` `main` — and SHALL follow resolved local and cross-module calls in HIR transitively.
The deterministic worklist SHALL record an instance before following it, so directly and mutually
recursive programs terminate with each canonical instance discovered exactly once, in deterministic
discovery order. Declarations of the closure that are not reachable from the entry SHALL NOT become
instances, whether or not their modules are imported.

#### Scenario: Discover a call chain once each

- **WHEN** `main` returns `identity(identity(42))`
- **THEN** discovery records exactly the `main` and `identity` instances in that order

#### Scenario: Discover a cross-module call chain

- **WHEN** root `main` calls a selectively imported public function which calls a function in a third module
- **THEN** discovery records all three instances once under their canonical module-qualified keys in call-discovery order

#### Scenario: Terminate on recursion

- **WHEN** `main` returns `main()`
- **THEN** discovery records the `main` instance exactly once and terminates

#### Scenario: Terminate on cross-module mutual recursion

- **WHEN** two imported public functions call one another and one is reachable from `main`
- **THEN** discovery records each canonical instance exactly once and terminates

#### Scenario: Exclude unreachable declarations

- **WHEN** the closure contains a declaration no reachable body calls
- **THEN** it produces no instance

### Requirement: Instance keys are canonical and normalized

An instance key SHALL consist of the canonical declaration identity plus normalized concrete
type and contract-row arguments — both empty in the frozen slice — and equal keys SHALL identify
the same instance.

#### Scenario: Key the degenerate slice

- **WHEN** any instance is discovered in the frozen slice
- **THEN** its key carries the canonical declaration identity with empty type and contract-row arguments

### Requirement: An unavailable entry stays explicit

When the root module has no unique valid entry — missing, ambiguous, parameterized, or not
returning resolved `I32` — discovery SHALL report an explicitly unavailable entry with its
reason and SHALL record no instances, rather than choosing a declaration or failing.

#### Scenario: Report a missing entry

- **WHEN** the root module declares no `main`
- **THEN** discovery reports an unavailable entry with a missing-entry reason and an empty instance list


### Requirement: Runtime aggregate reachability follows nominal values

Instance discovery SHALL follow canonical nominal types appearing in reachable parameters, results,
bindings, struct constructions, projections, and cleanup behavior. It SHALL recursively include
each nominal field type needed to realize those values while continuing to omit declarations and
types that no runtime instance reaches.

#### Scenario: Discover a factory's aggregate result

- **WHEN** `main` calls a reachable factory returning `Token` and projects `Token.kind`
- **THEN** discovery records the factory instance, canonical `Token` runtime type, and its recursively required field types

#### Scenario: Omit an unused nominal declaration

- **WHEN** another valid struct is declared but never appears in a reachable value path
- **THEN** it remains in the declaration-wide catalog but is absent from runtime aggregate reachability

### Requirement: Aggregate-bearing instance keys stay canonical

Function instances whose contracts contain nominal structs SHALL key those types by canonical
module and declaration identity, never by field shape, literal spelling, source field order, or
backend representation. Repeated discovery SHALL produce identical worklist and instance ordering.

#### Scenario: Distinguish equal-shaped parameters

- **WHEN** reachable functions accept equal-shaped structs declared in different modules
- **THEN** discovery records distinct canonical instance keys for the two nominal parameter types

### Requirement: Runtime reachability follows array types and values

Instance discovery SHALL follow canonical array types through reachable contracts, bindings,
construction, indexing, projection, and cleanup and SHALL recursively include the element types
needed for layout and runtime behavior. Unused array declarations or types MUST NOT enter the plan.

#### Scenario: Discover a nested array result

- **WHEN** a reachable factory returns `Array<Array<I32, 4>, 3>`
- **THEN** discovery records the exact outer and inner canonical array types in stable worklist order

### Requirement: Array-bearing instance keys include exact lengths

Instance keys and encodings SHALL distinguish array contracts by canonical element identity and every
nested length without structural abbreviation or backend representation.

#### Scenario: Distinguish equal-layout zero arrays

- **WHEN** two reachable functions use `Array<A, 0>` and `Array<B, 0>`
- **THEN** their type-bearing instance facts remain nominally distinct despite both having zero runtime lanes

### Requirement: Runtime reachability follows canonical unions

Instance discovery SHALL include each concrete normalized union appearing in a reachable contract,
local, aggregate, array, conversion, or cleanup plan and SHALL follow every nominal member required
to compute storage, calling shape, and cleanup. Equivalent spelling orders and nested forms SHALL
produce one instance-key type, one worklist entry, and one deterministic member dependency order.

#### Scenario: Discover an aggregate-contained union

- **WHEN** a reachable struct field has type `Token | End`
- **THEN** discovery records the canonical union and follows both nominal member layouts exactly once

#### Scenario: Deduplicate equivalent union spellings

- **WHEN** reachable contracts use both `Token | End` and `End | (Token | End)`
- **THEN** their normalized instance keys identify the same runtime type

### Requirement: Runtime reachability follows match patterns and results

Instance discovery SHALL follow the scrutinee type, every canonical nominal member named or covered
by an executable arm, recursively bound field types, guard and result expressions, joined result
type, and branch cleanup requirements. Unreachable arms SHALL contribute no runtime instance, while
equivalent match spelling SHALL preserve canonical worklist and dependency order.

#### Scenario: Discover a nested payload pattern

- **WHEN** a reachable match destructures `Token | End` and a `Token` field contains `Span`
- **THEN** discovery records the union, `Token`, `End`, `Span`, and required result and cleanup types exactly once

#### Scenario: Omit an unreachable arm

- **WHEN** a universal arm precedes a diagnosed unreachable nominal arm
- **THEN** the unreachable arm's otherwise-unused result and pattern types do not enter runtime reachability

### Requirement: Generic instance keys carry normalized concrete arguments

Every discovered generic runtime instance SHALL be identified by its canonical declaration plus an
ordered normalized concrete type-argument list. Worklist discovery SHALL record an instance before
following calls, values, cleanup, and runtime helpers reachable through its substitution, and its
ordering and encoding SHALL remain deterministic.

#### Scenario: Distinguish specializations
- **WHEN** the entry reaches `identity<I32>` and `identity<Bool>`
- **THEN** discovery records two keys differing only in their concrete argument lists

#### Scenario: Exclude an unused specialization
- **WHEN** a generic declaration can accept `Token` but no reachable call uses that argument
- **THEN** no `Token` instance is discovered merely from the declaration

### Requirement: Slice-bearing instances key element type without source length

A slice type in a reachable function contract SHALL contribute its canonical element type and access
mode to the instance key and deterministic encoding, but MUST NOT contribute the length of any fixed
array borrowed at a call site. Generic slice functions SHALL specialize by normalized concrete
element arguments under the existing finite monomorphization rules.

#### Scenario: Reuse one function for two source lengths

- **WHEN** one `fold(values: &[I32])` declaration is called with shared borrows of `Array<I32, 3>` and `Array<I32, 6>`
- **THEN** discovery records one `fold` instance and one emitted function symbol

#### Scenario: Distinguish generic element specializations

- **WHEN** a generic slice function is reached with `&[I32]` and `&[Token]`
- **THEN** discovery records distinct concrete element-type instances without adding either source array length to their keys

### Requirement: Slice reachability follows element behavior

Instance discovery SHALL follow the concrete element type of every reachable slice for layout,
Copy, projection, replacement, and cleanup requirements while keeping the borrowed source owner and
its fixed length local to the caller.

#### Scenario: Reach a move-only aggregate through a slice

- **WHEN** a reachable function accepts `&mut [Token]` and replaces an indexed `Token`
- **THEN** instance discovery includes the canonical `Token` layout and cleanup behavior without creating a runtime slice owner

### Requirement: Usize participates in ordinary instance identity

Instance discovery SHALL include canonical `Usize` types and operations in signatures and reachable
bodies. Literal magnitude and selected target width MUST NOT create separate generic instances;
target selection belongs to the layout and lowering inputs for the same canonical instance.

#### Scenario: Reuse a generic Usize instance

- **WHEN** one generic identity function is called with several `Usize` magnitudes on one target
- **THEN** discovery produces one concrete `Usize` instance

### Requirement: Effect discovery follows static bodies and handlers

Instance discovery SHALL reach each statically selected Effect body and catch handler with its concrete
type arguments. It MUST NOT specialize instances by runtime success/failure outcome, payload value,
failure tag, or capture value.

#### Scenario: Reuse one Effect instance

- **WHEN** one generic Effect is constructed with different values for the same concrete type
- **THEN** discovery produces one body instance and stable handler reachability

### Requirement: Instance discovery follows Effect and storage reachability

Instance discovery SHALL reach concrete Effect bodies, handlers, retry policies, allocator witnesses,
layouts, raw-buffer operations, Drop hooks, and Silk Vector specializations from executable roots.
Equivalent concrete uses SHALL reuse canonical instances, and unused allocator implementations or
container specializations MUST NOT enter the plan.

Every reachable Effect construction site SHALL create one canonical hidden instance per enclosing
monomorphized function instance. Distinct sites MUST remain distinct even when their public Effect
contracts are structurally equal.

#### Scenario: Discover one Vector specialization

- **WHEN** several effects append `Token` values through the same `Vector<Token>` operations
- **THEN** discovery records one canonical Vector specialization, its Drop behavior, allocation witness calls, and required Token cleanup

### Requirement: Allocation reachability remains finite and type-directed

Instance discovery SHALL follow reachable allocator conformance witnesses, allocation/reclaim
operations, concrete `RawBuffer<T>` and `Slot<T>` operations, restricted Drop hooks, and every
transitively cleaned field. Instance keys SHALL include canonical concrete types, roles, targets,
and callable contracts where already required, but MUST NOT include runtime counts, allocation
ordinals, provider object identities, logical addresses, or cleanup-event identities.

#### Scenario: Reuse one typed-storage instance across counts

- **WHEN** one generic raw-buffer helper is called for the same canonical `T` with several runtime counts
- **THEN** discovery records one concrete helper instance and retains each count only as runtime data

#### Scenario: Discover cleanup through an uncalled path

- **WHEN** a reachable owner type has a restricted Drop hook but one execution path never constructs it
- **THEN** discovery still includes the statically reachable hook exactly once without inventing a runtime owner
