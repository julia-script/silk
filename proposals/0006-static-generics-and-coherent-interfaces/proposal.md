# SLP-0006: Static generics and coherent interfaces

SLP: 0006
Status: Candidate
Revision: 10
Author: Julia Ortiz
Created: 2026-08-19
Updated: 2026-08-19
Discussion: —
Review record: —
Depends on: SLP-0003, SLP-0004, SLP-0005
Split from: —
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Silk uses static parametric abstraction. A generic declaration is checked once
against its declared contracts, and every reachable application becomes one finite concrete
specialization. Interfaces describe compile-time operation contracts; one coherent conformance
selects ordinary Silk functions or inline conformance bodies without creating interface values,
runtime method lookup, or generic dictionaries. Every interface has one implicit compile-time
provider type named `Self`; interface arguments describe only the contract's additional type and
row parameters, while `impl Interface<Arguments> for Provider` binds `Self` to the provider.

## Problem and evidence

Silk already has generic structs and functions, call-site inference, explicit type arguments,
generic callable sections, interface bounds, mapped witnesses, conditional conformances, and
monomorphic instance discovery. These pieces are spread across several OpenSpec changes and carry
contradictory assumptions:

- the current generic specification treats failures as a separate `!E` row kind, while the
  confirmed Effect model makes `E` an ordinary type or union;
- simple bounds use `T: Decoder`, conditional bounds use shapes such as `T: Decoder<T>`, and
  conformances repeat the provider as `impl Decoder<Schema> for Schema`;
- current operator behavior guesses interface operations from names, while SLP-0004 requires
  explicit operator eligibility;
- interface and service witness paths have accumulated different behavior even though a service is
  only a dependency-eligible interface; and
- general conformances accept mapped actor functions but not the already confirmed inline form.

The programmer needs a smaller model: what a generic parameter means, where inference comes from,
what operations a bound grants, which conformance is selected, whether that selection is static,
and what happens when no unique proof exists.

## Driving examples: current and desired

### Case: Reuse one generic function without hidden capabilities

#### Intent

Move any value through a function while preserving its exact type and ownership.

#### Current Silk

```silk
fn identity<T>(value: T) -> T {
  return move value
}

pub fn main() -> i32 {
  return identity(42)
}
```

This works, but the surrounding generic contract is not yet gathered into one language reference.

#### Desired Silk

```silk
fn identity<T>(value: T) -> T {
  return move value
}

pub fn main() -> i32 {
  return identity(42)
}
```

#### Observable result

The call infers `T = i32`, returns `42`, and reaches runtime as a concrete `identity<i32>`
application. Checking `identity` does not depend on discovering that later specialization.

#### Boundary case

```silk,ignore
fn make<T>() -> T {
  // No value can be produced merely because a caller later wants a T.
}

let value: i32 = make()
```

The expected result does not infer `T`; the call must supply a type argument and the body must still
construct a value of that type legally.

### Case: Use one operation through a static interface contract

#### Intent

Write one decoding function and specialize it for each type with a declared decoder conformance.

#### Current Silk

```silk
interface Decoder<T> {
  fn decode(value: &T) -> i32
}

struct Schema {}

fn decodeSchema(value: &Schema) -> i32 {
  return 42
}

impl Decoder<Schema> for Schema {
  decode: Schema.decodeSchema
}

fn decode<T: Decoder>(value: &T) -> i32 {
  return Decoder.decode(value)
}
```

The compiler supports this static witness path. The example also exposes the unresolved provider
model: `Schema` is both the argument to `Decoder` and the `for` type, while `T: Decoder` elides the
corresponding application.

#### Desired Silk

The provider is implicit in the interface contract and named `Self`:

```silk,ignore
interface Decoder<A> {
  fn decode(value: &Self) -> A
}

impl Decoder<i32> for Schema {
  decode: Schema.decodeSchema
}

fn decode<T: Decoder<i32>>(value: &T) -> i32 {
  return Decoder.decode(value)
}
```

The body is checked once using only the declared `Decoder` contract. Each concrete call selects one
coherent conformance and one static operation target.

#### Observable result

Calling the function with `Schema` selects its conformance and returns `42`. No interface object,
method table lookup, runtime type test, or name-based duck typing occurs.

#### Boundary case

```silk,ignore
fn decode<T>(value: &T) -> i32 {
  return Decoder.decode(value)
}
```

Without a `Decoder` bound, the generic body cannot use that operation even when every currently
known call happens to pass a conforming type.

### Case: Preserve an effectful interface contract exactly

#### Intent

Abstract over an effectful decoder without flattening nested Effects or turning failure values into
a separate value-level row type.

#### Current Silk

The current complete-interface specification uses a distinct failure-row binder:

```silk,ignore
interface Decoder<S, A, !E, ?R> {
  effect fn decode(value: &S) -> A ! E ? R
}
```

That conflicts with the confirmed Effect rule that `E` is an ordinary type.

#### Desired Silk

The provider uses `Self`, and the channel model uses ordinary failure type `E`:

```silk,ignore
interface Decoder<A, E, ?R> {
  effect fn decode(value: &Self) -> A ! E ? R
}
```

`E` is an ordinary type or union everywhere. `?R` remains a requirement-row parameter. A mapped or
inline implementation must satisfy the complete operation contract after specialization.

#### Observable result

Calling `Decoder.decode(value)` constructs exactly one Effect layer. If `A` itself is an Effect,
the result is nested and requires an explicit additional `run` or `Effect.flatten`; interface
dispatch does not unwrap it.

#### Boundary case

```silk,ignore
interface Decoder<E, ?R> {
  effect fn decode(value: &Self) -> i32 ! E ? R
}

// Invalid if this function constructs Effect<i32> rather than i32.
effect fn mapped(value: &Schema) -> Effect<i32> {
  return inner(value)
}
```

An implementation returning `Effect<i32>` cannot satisfy an operation promising success `i32`.

### Case: Derive a container conformance from its contained value

#### Intent

Make every `Box<T>` displayable when its contained `T` is displayable, without registering a
runtime dictionary or writing one conformance per concrete element type.

#### Current Silk

Current conditional conformances repeat the provider inside the interface application:

```silk
impl<T: Display<T>> Display<Box<T>> for Box<T> {
  display: Box.display
}
```

The compiler already proves the bound statically, rejects overlapping heads, and requires proof to
descend structurally, but the source follows the old explicit-provider model.

#### Desired Silk

```silk,ignore
impl<T: Display> Display for Box<T> {
  display: Box.display
}
```

The provider appears once. The bound says that contained provider `T` must implement `Display`.

#### Observable result

`Box<User>` implements `Display` exactly when `User` does. `Box<Box<User>>` produces a finite proof
chain through `Box<User>` to `User`, and the resulting specialization calls one static operation
target.

#### Boundary case

```silk,ignore
impl<T: Display> Display for Box<T> { /* ... */ }
impl Display for Box<i32> { /* ... */ }
```

The two heads overlap at `Box<i32>` and are rejected. Silk does not choose the concrete declaration
as more specific.

### Case: Return executable behavior without exposing its representation

#### Intent

Construct a capturing callable or Effect behind a public static contract without boxing it or
making its private construction site part of the public signature.

#### Current Silk

Silk already implements a contextual opaque-result binder:

```silk
fn add(left: i32, right: i32) -> i32 {
  return left + right
}

pub fn makeAdder(value: i32) -> some<F: fn(i32) -> i32> F {
  return add(value)
}
```

#### Desired Silk

Retain this form. `some<F: Contract> Result` introduces one producer-owned opaque representation
over the complete result. Callers know its use contract while the compiler privately retains its
exact construction, captures, layout, cleanup, and static target.

#### Observable result

`makeAdder(40)` returns a callable that produces `42` when invoked with `2`. Calls capturing
different integers share the producer's opaque representation type. Different producer
declarations, and different generic specializations of one producer, have distinct opaque types.
No box, runtime type tag, allocation, type choice, or dynamic dispatch is introduced.

#### Boundary case

```silk,ignore
fn decimal(value: i32) -> i32 { return value }
fn hexadecimal(value: i32) -> i32 { return value }

pub fn choose(hex: bool) -> some<F: fn(i32) -> i32> F {
  if hex { return hexadecimal }
  return decimal
}
```

This is invalid because one producer specialization would have two exact realizations. When an
exact visible named representation is deliberately public, `typeof(decimal)` is the explicit
alternative; otherwise the producer uses one opaque realization.

## Goals and non-goals

### Goals

- Define generic parameter identity, application, inference, body checking, and specialization.
- Define interfaces as ordinary compile-time contracts with complete function and Effect channels.
- Define inline and mapped conformances, locality, completeness, and compatibility.
- Define unique static conformance selection, conditional proof, overlap, and termination.
- Reuse the same interface machinery for services after the dependency-eligibility check.
- Give missing, incompatible, ambiguous, overlapping, and non-terminating abstractions distinct
  diagnostics.

### Non-goals

- Introduce existential interface values, trait objects, runtime reflection, or dynamic dispatch.
- Introduce inheritance, structural duck typing, method-call syntax, or open type namespaces.
- Define higher-kinded types, arbitrary compile-time type functions, or general const generics.
- Revisit Effect execution, ownership, module visibility, or operator evaluation except where their
  already confirmed contracts constrain generic abstraction.
- Finalize static value composition from SLP-0001.

## Current language model

Generic declarations bind canonical parameters of several compiler kinds and infer them from call
arguments. Generic bodies are intended to check once and reachable calls become concrete instances.
Interfaces are nominal declarations whose operations are selected through conformances. Conditional
conformance proof is static and overlap is rejected conservatively.

The current model is harder to state at source level. It distinguishes failure rows from ordinary
types, duplicates interface providers, permits name-based operator witness selection, uses special
service witness paths, and supports only mapped general conformances plus narrow compiler-owned hook
exceptions.

## Proposed language model

The generic foundation has one direction: explicit static contracts and finite specialization.
Ordinary parameters range over ordinary types, including failure unions. `?R` ranges only over a
finite normalized requirement row. Representation parameters preserve exact callable or Effect
construction identities where storage needs them; they do not expose runtime reflection. A
declaration-owned opaque result exposes the representation's use contract while retaining that
exact construction privately for static specialization.

A generic body is checked once. Only its parameters, ordinary type operations, compiler-owned type
properties, and explicitly declared interface bounds are available. Supplied arguments and declared
constraints infer a call specialization; an expected result never invents one.

Interfaces remain compile-time contracts and conformances remain canonical static facts. `Self` is
an implicit declaration-local type available inside an interface contract. It is not an interface
argument and creates no method receiver. `impl Interface<Arguments> for Provider` binds `Self` to
`Provider`; a bound such as `T: Interface<Arguments>` states the same relationship for open `T`.

## Worked language experience

The first reference batch records generic fundamentals. The second uses the selected `Self` model
to define interface declarations, bounds, operation contracts, and conformance implementation.
Every qualified operation call must resolve one application from its supplied operands or current
bounds. Zero-operand and otherwise ambiguous concrete selection is exposed through an ordinary
generic actor helper whose explicit arguments establish the unique bound; Silk adds no separate
conformance-expression syntax.

Multiple bounds on one parameter use `+` as an unordered compile-time conjunction, such as
`T: Hashable + Display`. Every listed goal must hold independently. The conjunction is neither a
runtime witness bundle nor a general intersection type, and the first stable model adds no bound
aliases or `where` clause.

Conditional conformances remain in the stable model. Their bounds may refer only to provider
parameters structurally contained by the implemented provider, every proof step must strictly
descend, and every concrete goal must prove the complete finite chain. Potentially overlapping heads
are rejected without consulting their bounds; Silk has no implicit conformance specialization,
negative bounds, or source-order priority.

Static proof has no runtime or cross-area side effects. Service eligibility is checked once before
ordinary conformance machinery; bounds do not provision dependencies. Proof neither runs Effects
nor moves values, operator participation still requires an explicit operation marker, imports do
not activate witnesses, and every target receives the same already-selected static operation. A
generic helper may select `SchemaOf` without thereby making its call const-evaluable; static value
execution remains SLP-0001's concern.

Generic struct construction may infer every omitted argument kind from supplied fields, not only
compiler representation parameters. A literal may write an ordered prefix and infer the remainder;
expected assignment or return types never invent missing arguments, and a parameter absent from all
fields must remain explicit.

Callable and Effect representation parameters remain an explicit declaration mechanism for stored
behavior. Construction infers the exact representation, generic signatures preserve it, reachable
applications specialize it to static code, and representation-dependent values join only when
their complete types match. Silk never inserts boxing, erasure, allocation, or indirect dispatch to
merge distinct implementations that share one structural contract.

Confirmed cross-area decisions constrain this Candidate:

- a service receives only one extra permission beyond an interface: dependency eligibility;
- conformances may define inline bodies or map existing actor functions;
- only the module defining a nominal provider type may declare its conformances;
- generic Effect failure `E` is an ordinary type, while `?R` is a special requirement row;
- nested Effects remain nested through interface calls; and
- operators may use only explicitly operator-eligible interface operations.

## Semantic sketch

- Each generic parameter has a canonical identity local to its declaration.
- Generic nominal types require complete kind-correct applications in type positions.
- Generic calls may write an ordered prefix and infer the remainder from supplied arguments and
  declared constraints, never from expected results.
- Generic struct literals follow the same prefix rule and infer only from their supplied fields and
  declared constraints.
- Callable and Effect representation parameters preserve one exact implementation through generic
  forwarding and must be concrete before execution.
- Representation-dependent values join only when their complete applications are equal; distinct
  representations may instead be consumed before a common ordinary result joins.
- `some<F: Contract> Result` gives one producer specialization one private exact realization while
  exposing only its static use contract; `typeof(item)` deliberately exposes a visible named exact
  representation.
- A generic body checks once against open parameters and declared bounds.
- `+` joins independent bounds on one parameter without creating a runtime value or ordering proof.
- Each reachable complete application selects concrete arguments and static conformance evidence.
- Runtime code is monomorphic; generic and conformance metadata is erased.
- Polymorphic recursion and unresolved generic values may not create an unbounded instance family.
- An interface declaration is not a runtime value or implicit service slot.
- `Self` names the interface's provider type and is substituted by each bound or `impl` goal.
- A conformance supplies every operation exactly once with a compatible complete contract.
- Distinct conformances that could match one concrete goal are rejected rather than ranked by source
  order or bounds.
- Conditional conformance proof follows strictly smaller contained providers and is erased after
  selecting static operation targets.

## Compiler–standard library boundary

### Compiler necessity

Generic binding, type inference, static contract checking, conformance coherence, proof selection,
and monomorphic specialization cannot be implemented by ordinary runtime Silk functions.

### Smallest target-neutral primitive

No new source-callable intrinsic is required. The compiler needs static parameter,
constraint, conformance, witness, and specialization machinery. Operator participation remains the
declaration marker selected by SLP-0004 rather than a compiler-known interface name.

### Standard-library construction

Standard-library interfaces and services are ordinary Silk declarations. Numeric, ordering,
collection, Effect, and service APIs define their policy in source and map to sealed intrinsics only
where an operation ultimately needs machine behavior.

### Privilege audit

The compiler must not recognize `Decoder`, `Order`, `Integer`, `SchemaOf`, or any service by spelling.
It may recognize sealed language properties such as Copy eligibility and Drop structure without
turning them into ordinary user-customizable interfaces. A service-specific branch is permitted
only at the dependency-eligibility boundary; all later conformance behavior reuses interfaces.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Affected | Parameter lists, bounds, interface providers, `impl`, operation qualification, and visibility need one source model. |
| Types and abstraction | Affected | Parameter kinds, inference, nominal application, interface contracts, and conformance proof are central. |
| Execution contracts | Affected | Effect operations preserve ordinary failure types, requirement rows, and exact nesting. |
| Ownership and resources | Affected | Parameter modes are literal; generic bodies and witnesses obey moves, borrows, Copy, Drop, and lifetimes. |
| Runtime and targets | Affected | Reachable applications specialize to deterministic monomorphic evaluator, native, and Wasm artifacts. |
| Compiler | Affected | Parsing, indexing, elaboration, proof search, instance discovery, lowering, and diagnostics participate. |
| Standard library | Affected | Source interfaces express policy without compiler-known actor names. |
| Tooling and diagnostics | Affected | Hover, navigation, inferred arguments, selected conformances, and proof failures need source explanations. |
| Learning and use | Affected | The model must explain bounds and implementations without exposing internal witness machinery first. |

## Scope cohesion

Generics and interfaces share one question: how does a declaration state exactly which operations
and executable representations are valid before its concrete types are known? Conditional
conformance, representation parameters, opaque results, and specialization are the proof and
realization layers of that same static-parametric answer. An opaque result is the source-level
abstraction boundary over one exact static representation, not a second runtime existential model.

## Complexity and subtraction budget

Prefer one static contract path over special interfaces for operators, services, intrinsics, or
concrete types. Reject runtime dictionaries, overload ranking, expected-result inference, implicit
duck typing, retroactive foreign conformances, and type-directed source branching.

## Surface displacement

The Candidate replaces failure-row binders with ordinary failure types, duplicated provider
arguments with implicit `Self`, name-based operator witness selection with explicit eligibility,
service-specific conformance behavior with shared interface machinery, and mapped-only conformances
with the confirmed dual inline/mapped model.

## Drawbacks and risks

- Monomorphization can increase code size and requires a finite reachability model.
- Strict coherence prevents retroactive foreign type/interface combinations without adapters.
- Rejecting expected-result inference may require explicit arguments for result-only parameters.
- Conservative overlap checking can reject conformances that a more complex solver could prove
  disjoint.
- Representation parameters may expose compiler complexity that ordinary users rarely need.

## Alternatives and prior art

### Status quo

Keep the current mixture of explicit and implicit provider applications, kinded failure rows,
service-specific paths, and name-based witness selection. This minimizes immediate changes but does
not produce a teachable language model.

### Smaller primitive or library solution

Use only generic functions with explicit function arguments and no interfaces. This is expressive
for many algorithms, but it makes repeated multi-operation contracts verbose and cannot serve the
already chosen service and operator models without manually threading operation bundles.

### Strongest competing language model

Use runtime interface values with existential storage, dynamic method tables, structural
conformance, and open retroactive implementations. This supports heterogeneous collections and
runtime substitution but contradicts Silk's static specialization, ownership transparency, and
coherence direction.

## Falsifiers and acceptance blockers

- Finite specialization cannot carry required higher-order generic callables through realistic
  programs without runtime dictionaries or uncontrolled code growth.
- Provider-local coherence prevents a necessary library-extension pattern that an owned adapter or
  ordinary actor function cannot express acceptably.
- Interface Effect contracts require behavior inconsistent with ordinary named function contracts.

## Open realization questions

None that may reverse the Candidate direction. Concrete parser, diagnostic-code, semantic-surface,
and migration sequencing decisions belong to a later OpenSpec handoff and must preserve the static,
coherent, allocation-free model defined here.

## Future directions

Existential interface values, explicit type erasure, specialization controls, associated types,
higher-kinded parameters, general const generics, compile-time reflection, and derived conformances
remain separate future directions.

## OpenSpec realization map

- Reconcile generic parameter kinds, ordered-prefix inference, complete nominal applications, and
  finite specialization with the confirmed ordinary failure-type model.
- Replace the duplicated interface-provider representation with implicit `Self`, complete static
  bounds, inline-or-mapped conformances, provider-local coherence, and terminating conditional
  proof.
- Reconcile exact and opaque callable and Effect representations with construction inference,
  result visibility, static joins, layout, and target lowering.
- Migrate services, operator eligibility, module semantic surfaces, diagnostics, and documentation
  onto the shared conformance model without introducing compiler-known library actors.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-19 | Created the Draft with a static finite-specialization thesis, recorded confirmed cross-area constraints, and isolated the duplicated interface-provider model as the first decision frontier. |
| 2 | 2026-08-19 | Chose implicit compile-time `Self` as the interface provider, removed the provider from ordinary interface arguments, and retained `impl Interface<Arguments> for Provider` as the binding form. |
| 3 | 2026-08-19 | Required every qualified interface call to resolve one static application and chose ordinary generic actor helpers, rather than a new conformance-expression syntax, for zero-operand or otherwise ambiguous concrete selection. |
| 4 | 2026-08-19 | Chose `+` as an unordered compile-time conjunction of independent bounds, rejected duplicate applications, and left bound aliases and `where` clauses outside the first stable model. |
| 5 | 2026-08-19 | Kept conditional conformances with strictly descending structural proof, complete static requirement chains, conservative head coherence, and no overlap ranking, negative bounds, or implicit specialization. |
| 6 | 2026-08-19 | Generalized struct-literal field inference from representation-only omission to every inferable generic kind, retained ordered explicit prefixes, and rejected expected-context and phantom-parameter inference. |
| 7 | 2026-08-19 | Retained explicit callable and Effect representation parameters with construction inference, generic preservation, concrete static specialization, exact join equality, and no implicit erasure or dispatch. |
| 8 | 2026-08-19 | Retained declaration-owned `some<F: Contract> Result` opaque families as the public executable abstraction boundary, required one realization per producer specialization, preserved `typeof(item)` for deliberately exposed exact named representations, and prohibited runtime packaging or dispatch. |
| 9 | 2026-08-19 | Confirmed contiguous ordered explicit generic prefixes for calls and struct construction, with remaining arguments inferred from supplied values and constraints, superseding the older all-or-none rule. |
| 10 | 2026-08-19 | Removed independent conformance visibility: an `impl` has no `pub` or private mode and is usable wherever both canonical endpoints are semantically available. |
