# bootstrap-instances Specification

## Purpose

Deterministic discovery of the concrete runtime instances reachable from the user entry:
instance keys over canonical declaration identities, and the recorded worklist whose
record-before-follow discipline makes ordinary recursion terminate — the structure the full
generic language inherits, degenerate while the slice has no type or contract-row arguments.

## Requirements

### Requirement: Instances are discovered from the entry by a recorded worklist

Instance discovery SHALL start from one of the root module's three valid user entries: a unique
zero-parameter public ordinary `main() -> ()`, a unique zero-parameter public ordinary
`main() -> i32`, or a unique zero-parameter public `effect fn main() -> () ! E` whose requirement
row is empty and whose failure members are concrete detached owned values. Discovery SHALL retain
the selected entry kind and normalized failure metadata. When the selected target's kind is native,
discovery SHALL additionally seed the worklist, after the entry, with every `export "C"` function
declared in the loaded module closure, in canonical module then declaration order, and SHALL
record each export root with its native symbol so later phases can name the instance it selects.
Export roots are monomorphic by declaration restriction and SHALL be specialized, ownership-checked,
and closed exactly as call-reached instances. For a WebAssembly target exports SHALL NOT be roots.

The compiler SHALL select the concrete compilation target before constructing the executable
worklist. For each demanded concrete application, the realization coordinator SHALL first evaluate
its static arguments and static body operations for that target, record the resulting canonical
specialization key, and obtain one residual typed HIR body. It SHALL close a private candidate graph
from direct residual calls and cleanup-edge prepass facts without publishing executable
reachability. After that graph closes, it SHALL run ownership and cleanup exactly once over each
successful residual specialization before admitting the resulting local and cross-module runtime
call closure. The deterministic candidate worklist SHALL record a specialization before following
direct calls, so directly and mutually recursive programs terminate with each canonical
specialization discovered exactly once in deterministic order. Declarations and static applications
not reachable from the entry or from a native export root SHALL NOT become runtime instances merely
because their modules are loaded or imported.

#### Scenario: Discover a call chain once each

- **WHEN** ordinary `main` returns `identity(identity(42))`
- **THEN** discovery records exactly the `main` and `identity` runtime specializations in that order

#### Scenario: Discover an effectful entry chain

- **WHEN** effectful `main` runs one reachable effect function and can fail with one concrete detached owned type
- **THEN** discovery records `main`, the reachable residual function, the failure runtime type, and its cleanup hooks deterministically

#### Scenario: Discover a cross-module call chain

- **WHEN** root `main` calls a selectively imported public mixed function which residualizes a call into a third module
- **THEN** discovery records all three residual runtime specializations once under their canonical module-qualified keys in call-discovery order

#### Scenario: Terminate on recursion

- **WHEN** one residual `main` specialization returns `main()` with the same static application
- **THEN** discovery records that specialization exactly once and terminates

#### Scenario: Terminate on cross-module mutual recursion

- **WHEN** two imported public residual functions call one another with the same canonical static applications and one is reachable from `main`
- **THEN** discovery records each canonical specialization exactly once and terminates

#### Scenario: Distinguish static applications

- **WHEN** one reachable caller applies the same declaration with two unequal canonical static argument values
- **THEN** discovery records two distinct specializations and follows each residual body's calls independently

#### Scenario: Exclude an inactive static call

- **WHEN** a call appears only in the arm not selected by `static if`
- **THEN** that call produces no worklist entry or runtime instance

#### Scenario: Exclude unreachable declarations

- **WHEN** the closure contains a declaration no residual reachable body calls and that is not a native export
- **THEN** it produces no runtime instance and none of its static functions execute

#### Scenario: Discover an uncalled export

- **WHEN** the target is native, `main` calls nothing, and a loaded module declares one exported function
- **THEN** discovery records the `main` specialization followed by the export's specialization and one export record naming it

#### Scenario: Discover export roots deterministically

- **WHEN** the same closure with exports is discovered in fresh processes
- **THEN** the instance order and export records are byte-identical

### Requirement: Instance keys are canonical and normalized

An instance key SHALL consist of the canonical declaration identity plus normalized concrete
type and contract-row arguments — both empty in the frozen slice — and equal keys SHALL identify
the same instance.

#### Scenario: Key the degenerate slice

- **WHEN** any instance is discovered in the frozen slice
- **THEN** its key carries the canonical declaration identity with empty type and contract-row arguments

### Requirement: An unavailable entry stays explicit

When the root module has no unique valid entry — missing, ambiguous, generic, parameterized,
ordinary with a result other than `()` or `i32`, effectful with a non-`()` result, or effectful with
unresolved requirements — discovery SHALL report an explicitly
unavailable entry with its reason and SHALL record no instances, rather than choosing a declaration
or failing.

#### Scenario: Report a missing entry

- **WHEN** the root module declares no `main`
- **THEN** discovery reports an unavailable entry with a missing-entry reason and an empty instance list

#### Scenario: Report an open effect entry

- **WHEN** the root module's effectful `main` retains a capability requirement
- **THEN** discovery reports an unavailable entry with an unresolved-requirements reason and an empty instance list

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

- **WHEN** a reachable factory returns `Array<Array<i32, 4>, 3>`
- **THEN** discovery records the exact outer and inner canonical array types in stable worklist order

### Requirement: Array-bearing instance keys include exact lengths

Instance keys and encodings SHALL distinguish array contracts by canonical element identity and every
nested length without structural abbreviation or backend representation.

#### Scenario: Distinguish equal-layout zero arrays

- **WHEN** two reachable functions use `Array<A, 0>` and `Array<B, 0>`
- **THEN** their type-bearing instance facts remain nominally distinct despite both having zero runtime lanes

### Requirement: Runtime reachability follows canonical unions

Instance discovery SHALL include each concrete normalized union appearing in a reachable contract,
local, aggregate, array, conversion, or cleanup plan and SHALL follow every normalized member
required to compute storage, calling shape, and cleanup. Equivalent spelling orders and nested
forms SHALL produce one instance-key type, one worklist entry, and one deterministic member
dependency order.

#### Scenario: Discover an aggregate-contained union

- **WHEN** a reachable struct field has type `Token | i32 | Array<i32, 2>`
- **THEN** discovery records the canonical union and follows all represented member layouts exactly once

#### Scenario: Discover a represented executable member

- **WHEN** a reachable union contains an exact callable or opaque Effect value with a finite capture environment
- **THEN** discovery follows that executable representation and every captured member layout required by its storage plan

#### Scenario: Deduplicate equivalent union spellings

- **WHEN** reachable contracts use both `Token | i32` and `i32 | (Token | i32)`
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

- **WHEN** the entry reaches `identity<i32>` and `identity<bool>`
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

- **WHEN** one `fold(values: &[i32])` declaration is called with shared borrows of `Array<i32, 3>` and `Array<i32, 6>`
- **THEN** discovery records one `fold` instance and one emitted function symbol

#### Scenario: Distinguish generic element specializations

- **WHEN** a generic slice function is reached with `&[i32]` and `&[Token]`
- **THEN** discovery records distinct concrete element-type instances without adding either source array length to their keys

### Requirement: Slice reachability follows element behavior

Instance discovery SHALL follow the concrete element type of every reachable slice for layout,
Copy, projection, replacement, and cleanup requirements while keeping the borrowed source owner and
its fixed length local to the caller.

#### Scenario: Reach a move-only aggregate through a slice

- **WHEN** a reachable function accepts `&mut [Token]` and replaces an indexed `Token`
- **THEN** instance discovery includes the canonical `Token` layout and cleanup behavior without creating a runtime slice owner

### Requirement: usize participates in ordinary instance identity

Instance discovery SHALL include canonical `usize` types and operations in signatures and reachable
bodies. Literal magnitude and selected target width MUST NOT create separate generic instances;
target selection belongs to the layout and lowering inputs for the same canonical instance.

#### Scenario: Reuse a generic usize instance

- **WHEN** one generic identity function is called with several `usize` magnitudes on one target
- **THEN** discovery produces one concrete `usize` instance

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

### Requirement: Parametric conformances monomorphize per instantiation

Instance discovery SHALL resolve capability dispatch and Drop cleanup for generic nominal types
through parametric and conditional conformances, substituting the instantiation's concrete kinded
arguments into the conformance head and proving every strict-subterm requirement. Each reachable
provider/interface instantiation SHALL produce exactly one concrete witness or hook instance with a
canonical normalized key, and the worklist SHALL remain finite without runtime dictionaries.

#### Scenario: One parametric Drop serves two element types

- **WHEN** a program makes `Vector<Token>` and `Vector<i32>` reachable under one `impl<T> Drop for Vector<T>`
- **THEN** discovery yields exactly two concrete Drop hook instances whose keys carry the normalized concrete arguments, and no third instance for the unsubstituted form

#### Scenario: Parametric witnesses dispatch like concrete ones

- **WHEN** a capability requirement is satisfied by a provider whose conformance is parametric
- **THEN** the run site dispatches to the substituted concrete operation identically to an equivalent hand-written concrete conformance

#### Scenario: Conditional witness follows its proof dependencies

- **WHEN** a reachable mapped provider requires and finds one concrete source-provider witness
- **THEN** discovery records the source witness and one mapped witness before lowering the static target

### Requirement: Instance discovery closes arbitrary service and primitive conformances

Instance discovery SHALL realize source-defined service witnesses and ordinary interface
conformances from canonical declarations rather than a compiler-known capability list. Generic
numeric wrappers SHALL specialize per concrete scalar conformance, and provided service operations
SHALL specialize per provider type and role without runtime nominal lookup.

#### Scenario: Discover a generic integer conformance

- **WHEN** a reachable function instantiates generic addition for two integer types
- **THEN** discovery records two concrete interface instances that select their respective intrinsic operations

#### Scenario: Discover a user service implementation

- **WHEN** a reachable provision binds a user-declared service to a conforming provider
- **THEN** discovery includes the mapped provider functions and no service-name-specific root

### Requirement: Generic interface witnesses specialize mapped targets

Instance discovery SHALL retain mapped generic witness functions with their inferred ordinary type,
failure-row, requirement-row, and representation arguments. The canonical witness key SHALL include
the concrete provider/interface application and mapped target arguments, and MIR SHALL receive one
direct target with no runtime dictionary.

#### Scenario: Discover two mapped target specializations

- **WHEN** two concrete providers select one generic witness declaration with different kinded arguments
- **THEN** discovery records two concrete witness target instances in deterministic order

#### Scenario: Reject an unresolved target binder

- **WHEN** a mapped witness target has a generic binder not inferable from its conformance and operation contract
- **THEN** analysis rejects the mapping before instance discovery can create an open key

### Requirement: Complete applications concretize row contracts before discovery

One complete-application frontier SHALL substitute row and member parameters, renormalize
collisions, discharge member-well-formedness obligations, group equal specialized constraints,
solve every wanted, and upgrade every used assumed proof to concrete evidence before row-dependent
dependency discovery, witness reachability, layout, ownership specialization, or lowering.

The resulting specialized contract/evidence bundle SHALL be branded concrete so residual
parameters, symbolic members, `Without`, assumed evidence, unsatisfied constraints, or ambiguity are
unrepresentable to downstream consumers. Instance identity SHALL use concrete extensional row keys
after this frontier. Nested quantified obligations of an unapplied callable schema SHALL remain
nested metadata and SHALL not be mistaken for enclosing-instance obligations.

#### Scenario: Reject a residual contract before discovery

- **WHEN** a complete reachable application retains an open row, symbolic member, obligation, assumed proof, or ambiguous selection
- **THEN** specialization rejects it before dependency and witness discovery consume it

#### Scenario: Deduplicate extensionally equal concrete rows

- **WHEN** two definitionally different open expressions specialize to the same concrete row
- **THEN** their instance keys are equal and discovery records one instance

#### Scenario: Preserve an unapplied callable schema

- **WHEN** a concrete enclosing instance creates and drops an unapplied constrained callable
- **THEN** its nested obligations remain schema metadata and erase with the callable without blocking the enclosing instance

### Requirement: Instance discovery follows reflection-generated residual calls

Concrete specialization SHALL complete static reflection, template validation, and heterogeneous
iteration before publishing direct runtime call candidates. Each generated field operation SHALL
select evidence and contribute call edges using that iteration's concrete field type. Equal
reflection and template applications SHALL reuse one residual specialization; unequal template,
aggregate type, visibility authority, generic argument, evidence, or static value inputs MUST NOT be
conflated.

#### Scenario: Discover heterogeneous Display instances

- **WHEN** one template selects a `string` field and an `i32` field
- **THEN** the executable closure contains the independently selected `Display<string>` and `Display<i32>` runtime operations and no descriptor instance

#### Scenario: Keep templates distinct

- **WHEN** the same formatting function is reached with two unequal static template values over the same argument type
- **THEN** discovery retains two canonical residual specialization keys even if later optimization makes their emitted bytes equal

### Requirement: Instance keys include canonical static values

A mixed function's instance key SHALL consist of its canonical declaration identity, normalized
concrete type and contract-row arguments, selected evidence, and the canonical encoding of every
static value argument in parameter order. The selected target SHALL belong to the enclosing
realization identity rather than a runtime argument. Equal keys SHALL share one residual instance;
unequal static values MUST NOT be conflated even when their residual bodies happen to encode
identically.

#### Scenario: Deduplicate an equal static application

- **WHEN** two reachable calls apply one mixed function with equal types, evidence, and canonical static values
- **THEN** discovery records one shared runtime specialization without losing either call site's provenance

#### Scenario: Keep target realizations separate

- **WHEN** the same source application is realized for WebAssembly and for one native target
- **THEN** each target obtains its own deterministic residual closure without placing the target in a runtime instance key or ABI lane

### Requirement: Generated aggregates participate in canonical runtime reachability

Instance discovery SHALL follow every named tuple or anonymous aggregate nominal identity that
appears in a reachable contract, local, generic substitution, construction, projection, borrow, or
cleanup plan. Generic instance keys SHALL use the complete occurrence-based nominal identity for an
anonymous aggregate, so repeated uses of one bound value share a specialization while distinct
same-shaped literal occurrences remain distinct concrete type arguments.

Generated aggregate reachability and ordering SHALL be deterministic and SHALL recursively follow
member types through the existing nominal struct rules. An unused anonymous literal in an
unreachable declaration MUST NOT enter runtime reachability merely because its synthesized
declaration is present in semantic facts.

#### Scenario: Specialize a generic formatter-shaped consumer

- **WHEN** one anonymous record binding is passed repeatedly to the same reachable generic function
- **THEN** instance discovery records one concrete specialization for that occurrence-based aggregate type

#### Scenario: Distinguish separate anonymous arguments

- **WHEN** separate same-shaped record literals are passed to one generic function
- **THEN** instance discovery retains two concrete nominal type arguments rather than merging them by shape

#### Scenario: Omit an unreachable generated type

- **WHEN** an anonymous aggregate occurs only inside an unreachable declaration
- **THEN** its semantic declaration remains inspectable but is absent from runtime aggregate reachability

### Requirement: Pointer-bearing instance keys include pointee and mutability

Instance keys SHALL treat `*const T` and `*mut T` as ordinary concrete runtime types whose
canonical form includes the pointee type and mutability, and reachability SHALL NOT follow a
pointee's construction, cleanup, or conformance instances merely because a pointer to it is
reachable.

#### Scenario: Key two pointer instances distinctly

- **WHEN** a generic function is applied to `*const i32` and to `*mut i32`
- **THEN** discovery records two specializations with different canonical keys

#### Scenario: A pointer does not reach the pointee

- **WHEN** a program's only use of `Vector<i32>` is a `*mut Vector<i32>` parameter
- **THEN** discovery records no `Vector<i32>` cleanup or method instance
