# bootstrap-type-generics Specification

## Purpose

Define kinded generic declarations and applications across value, row, and executable-
representation parameters whose reachable concrete uses are checked once, specialized finitely,
and erased into deterministic monomorphic runtime instances.

## Requirements

### Requirement: Monomorphic ordinary unions renormalize after substitution

Every complete generic application SHALL substitute its concrete ordinary type arguments into each
reachable union and normalize the resulting member set before instance discovery, layout,
ownership, or lowering. Members that become identical SHALL collapse to one member; canonical order,
conversion mappings, and runtime tags SHALL be recomputed from the concrete set. Generic checking
SHALL NOT require symbolic parameters to prove they remain distinct under every specialization.

#### Scenario: Collapse a generic union to one member

- **WHEN** a declaration containing `A | B` is specialized with `A = i32` and `B = i32`
- **THEN** the concrete application carries `i32` and no union layout or duplicate runtime tag is produced

#### Scenario: Preserve distinct specialized members

- **WHEN** the same declaration is specialized with `A = i32` and `B = string`
- **THEN** the concrete application carries the canonical `i32 | string` member set and recomputed mappings

### Requirement: Declarations bind canonical type parameters

Struct and function declarations SHALL accept ordered ordinary type parameters, failure-row
parameters, requirement-row parameters, callable representation parameters, and Effect
representation parameters. Every parameter identity SHALL be local to its declaration and distinct
from nominal types and parameters with the same spelling elsewhere. A parameter SHALL be available
only in positions admitted by its kind, and duplicate or unbound parameters MUST produce
deterministic diagnostics.

#### Scenario: Bind one generic struct parameter
- **WHEN** `pub struct Box<T> { pub value: T }` is analyzed
- **THEN** the field type refers to the canonical `T` parameter owned by `Box`, not to a nominal type named `T`

#### Scenario: Bind a representation parameter
- **WHEN** `pub struct Mapper<A, B, F: fn(A) -> B> { transform: F }` is analyzed
- **THEN** `F` is canonical to `Mapper` and can appear only as a represented callable value

#### Scenario: Reject a duplicate parameter
- **WHEN** a declaration introduces `<T, T>`
- **THEN** analysis reports the second parameter as a deterministic duplicate without fabricating another identity

### Requirement: Generic applications are explicit canonical types

Applying a generic nominal declaration SHALL produce a canonical type identified by the declaration
plus normalized ordered arguments. In required type positions, every kind-correct argument SHALL
remain explicit. Named struct construction MAY instead supply a contiguous explicit prefix of
ordinary value arguments and SHALL infer its omitted ordinary suffix from all supplied fields;
construction MAY also infer concrete representation arguments from corresponding field
initializers. Applying arguments to a non-generic declaration, supplying the wrong kind, leaving a
parameter uninferred, or producing conflicting field constraints MUST remain explicit semantic
failures. Expected result types and later uses MUST NOT participate in construction inference.

#### Scenario: Reuse one applied type identity
- **WHEN** independent declarations refer to `Box<Token>`
- **THEN** both references resolve to the same canonical applied type identity

#### Scenario: Infer a construction representation
- **WHEN** `Mapper` construction supplies a named function for field `F`
- **THEN** the complete applied type includes that exact representation argument

#### Scenario: Infer an ordinary construction suffix

- **WHEN** `Pair<A, B>` construction writes `Pair<i32> { first: 1, second: true }`
- **THEN** the complete nominal type is `Pair<i32, bool>` using only the supplied fields

#### Scenario: Reject the wrong arity

- **WHEN** `Pair<i32>` appears in a required type position for a declaration with two parameters
- **THEN** analysis reports the expected and actual argument counts and produces no available applied type

#### Scenario: Reject conflicting construction evidence

- **WHEN** two supplied fields imply distinct arguments for the same omitted parameter
- **THEN** inference retains both field origins, reports the conflict, and produces no applied nominal type

### Requirement: Calls infer only from supplied arguments

A generic call MAY supply a contiguous, ordered, kind-correct prefix of explicit generic arguments,
including value-type, failure-row, and requirement-row arguments. Analysis SHALL bind that prefix
positionally and infer every remaining suffix argument only from supplied call arguments and checked
constraints according to each constraint's binding policy: membership and subset are checking-only,
while provider selection may bind its selected row only by the unique-candidate rule. Expected
return types and uses after complete application MUST NOT bind generic arguments or prune constraint
candidates.

Forming an automatic leading-argument section SHALL infer from supplied trailing arguments and
retain every unresolved binder and constraint determined by the omitted leading parameter in the
section's semantic callable type. Applying that section SHALL complete inference from the leading
argument. A missing, conflicting, wrong-kind, or excess explicit argument MUST produce a
deterministic diagnostic at the responsible prefix or application.

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

#### Scenario: Supply a requirement-row prefix and infer the suffix

- **WHEN** `Effect.provideMut<Logger>(effect, &mut provider)` supplies first binder `?S` and leaves later binders implicit
- **THEN** analysis accepts the requirement-row argument, fixes `S`, and infers the suffix only from `effect`, `provider`, and their checked constraint

#### Scenario: Supply a row prefix through a pipeline

- **WHEN** `effect |> Effect.provideMut<Logger>(&mut provider)` supplies the same first row binder on a trailing-argument section
- **THEN** the section retains the omitted Effect-dependent suffix and completes it from the pipeline input without consulting the expected result

#### Scenario: Lift a failure singleton into a failure-row prefix

- **WHEN** a call whose first binder is `!E` supplies nominal `Problem` as its first explicit generic argument
- **THEN** analysis lifts `Problem` to the singleton failure row `Problem` and infers the remaining suffix from supplied arguments and constraints

#### Scenario: Reject a wrong-kind explicit prefix

- **WHEN** an explicit argument cannot form a member of the binder's row domain, such as `&Logger` for a failure-row binder or non-capability nominal `Problem` for a requirement-row binder
- **THEN** analysis reports a kind mismatch at that explicit generic argument rather than treating every value type as a valid row singleton

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

- **WHEN** the same generic mapper section is reached for `i32` and `Token`
- **THEN** instance discovery records exactly two concrete callable environments and terminates

### Requirement: Generic bodies are checked once

The compiler SHALL elaborate and check each generic body once over its canonical type parameters.
Concrete specialization MUST substitute the verified generic facts and MUST NOT enable undeclared
operations through concrete duck typing or type-directed source branching. A type parameter SHALL
carry compiler-owned Copy evidence only when its declaration has an explicit `Copy` bound, and that
symbolic evidence SHALL propagate through nested generic calls.

#### Scenario: Propagate Copy evidence through a generic call

- **WHEN** `outer<T: Copy>` calls `inner<T>` whose parameter is also bounded by `Copy`
- **THEN** constraint solving forwards the caller's symbolic evidence and accepts the call without concrete specialization

#### Scenario: Reject an unbounded structural guess

- **WHEN** an unconstrained type parameter is used where `Copy` is required
- **THEN** generic checking rejects the use even if one later specialization would contain only Copy fields

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
- **WHEN** the entry reaches `identity<i32>` and `identity<Token>`
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
### Requirement: Generics distinguish ordinary types from requirement rows

Generic declarations SHALL bind ordinary type parameters and requirement-row parameters as distinct
canonical kinds. A failure parameter `E` SHALL be an ordinary type parameter declared as `E`; the
`!` token SHALL appear only where an Effect contract labels its failure channel. A requirement-row
parameter SHALL remain declared as `?R` and SHALL be accepted only in requirement-row positions.

#### Scenario: Reuse a failure parameter as an ordinary value type

- **WHEN** a generic declaration binds `<E>` and returns `Effect<A ! E>`
- **THEN** the same canonical `E` may also type a parameter, local, field, handler input, or return value

#### Scenario: Reject a requirement row as a value

- **WHEN** a body uses requirement-row parameter `R` as a field or ordinary parameter value type
- **THEN** analysis reports a deterministic kind mismatch before specialization

### Requirement: Failure algebra is ordinary union algebra

Failure types SHALL use the same normalized finite structural unions, checked containment, and
`Without<E, S>` difference as ordinary value types. `never` SHALL be the empty type. A concrete
selected type or union `S` is contained in `E` only when every alternative in `S` belongs to `E`.
Difference SHALL remove those alternatives and SHALL be total as a type operation; operations that
promise to handle a selection MUST carry a separate checked containment constraint.

Open generic containment and difference SHALL remain static compiler facts, specialize
deterministically, and introduce no runtime dictionary. `Without` SHALL remain forward-computed:
expected result types MUST NOT infer `E` or `S` backwards. Declaration constraints SHALL be
assumptions while checking a generic body, and complete applications SHALL substitute and prove
those assumptions before dependency discovery, ownership specialization, layout, or lowering.

Requirement rows SHALL retain canonical service-role keys and store access demand separately from
identity. Requirement union SHALL join colliding keys to the stronger access. Requirement
membership, subset, intersection, and difference SHALL compare service-role keys; `Without<R, K>`
SHALL remove the complete matching entry regardless of its stored access. Provider compatibility is
separate and MAY allow an exclusive or owned provider to satisfy a shared stored requirement. A
requirement-row `Without` is also forward-computed and MUST NOT be inverted from an expected
remainder.

Open ordinary type expressions and requirement-row expressions SHALL use deterministic definitional
normal forms. Generic-to-generic substitution MUST compose parameter identity and assumed evidence
without demanding premature concreteness. Partial application SHALL retain quantified binders,
constraints, substitutions, and evidence until static application; they remain compile-time
metadata and never become runtime dictionaries. Callable-constraint semantics SHALL be independent
of whether a contract originated in Silk source or the sealed intrinsic inventory.

#### Scenario: Subtract one ordinary failure alternative

- **WHEN** `Without<ProblemError | OtherError, ProblemError>` is specialized
- **THEN** it normalizes to ordinary type `OtherError`

#### Scenario: Subtract an ordinary failure union

- **WHEN** `Without<FirstError | SecondError | ThirdError, FirstError | ThirdError>` is specialized
- **THEN** it normalizes to ordinary type `SecondError`

#### Scenario: Treat an absent difference member as a no-op

- **WHEN** `Without<First | Second, Other>` is specialized and `Other` is absent
- **THEN** it normalizes to `First | Second` without reporting an absent-member diagnostic

#### Scenario: Join colliding requirement access

- **WHEN** `&Logger | &mut Logger` is normalized
- **THEN** it contains exactly the exclusive entry `&mut Logger` for the default role key

#### Scenario: Remove an exclusive requirement with an access-independent selector

- **WHEN** `Without<&mut Logger, Logger>` is specialized
- **THEN** the complete default-role Logger entry is removed

#### Scenario: Remove a shared requirement with an access-independent selector

- **WHEN** `Without<&Logger, Logger>` is specialized
- **THEN** the complete default-role Logger entry is removed

#### Scenario: Remove the exact stored requirement

- **WHEN** `Without<&mut Logger | &Clock, Logger>` is specialized
- **THEN** it normalizes to the singleton requirement row `&Clock`

#### Scenario: Keep access compatibility outside key membership

- **WHEN** a declaration selects `Logger` from a row containing `&mut Logger`
- **THEN** key membership succeeds and provider access is validated separately by the consuming operation

#### Scenario: Preserve an open ordinary difference

- **WHEN** a generic declaration contains `Without<ProblemError | E, ProblemError>` and `E` remains open
- **THEN** analysis preserves the equivalent open ordinary type difference until specialization

#### Scenario: Reject inverse ordinary difference inference

- **WHEN** the only evidence for `E` is an expected type equivalent to `Without<E, ProblemError> = OtherError`
- **THEN** analysis reports `E` as underconstrained instead of choosing an inverse solution

#### Scenario: Reject membership-driven selector inference

- **WHEN** the only possible source for ordinary `S` is a checked constraint `S in Problem | Other`
- **THEN** analysis reports `S` as underconstrained rather than enumerating the source row or selecting either member

#### Scenario: Forward an open selected type through a generic wrapper

- **WHEN** generic `outer<T, E> where T in E` calls an operation whose selected type `S` is specialized to still-open caller type `T`
- **THEN** ordinary type substitution preserves `T`, composes assumed containment evidence, and defers concrete union containment to `outer`'s complete applications

#### Scenario: Rewrite a symbolic requirement member through generic forwarding

- **WHEN** a generic requirement member `&mut P at Audit` is substituted with still-open caller capability parameter `Q`
- **THEN** row substitution produces residual `&mut Q at Audit` with fixed exclusive access and resolved `Audit` role, and the retained well-formedness obligation refers to `Q`

#### Scenario: Concretize a valid symbolic requirement member

- **WHEN** residual requirement member `&mut P at Audit` is completely specialized with capability `P = Logger`
- **THEN** substitution produces exactly concrete member `&mut Logger at Audit` and discharges its member-well-formedness obligation

#### Scenario: Reject an invalid symbolic requirement member

- **WHEN** residual requirement member `&mut P at Audit` is completely specialized with a non-capability value type
- **THEN** substitution reports invalid requirement singleton before row normalization or any row-dependent consumer

#### Scenario: Renormalize after substitution collision

- **WHEN** substituting `C = Logger` and `D = Logger` changes `&C | &mut D` into colliding requirement entries
- **THEN** the substituted row renormalizes to exactly `&mut Logger` before any equality, selection, or difference

#### Scenario: Use a declaration constraint inside its generic body

- **WHEN** a generic body calls an operation whose provider-selection obligation is entailed by the body's declared provider-selection constraint
- **THEN** the body type-checks once over open parameters and forwards symbolic evidence without enumerating the open requirement row

#### Scenario: Carry an open selected type through a generic wrapper

- **WHEN** a generic wrapper declares ordinary `S` with `where S in E`, uses `Without<E, S>` in its result, and calls an operation requiring the same membership
- **THEN** declaration checking forwards assumed ordinary containment evidence without requiring `S` to be concrete

#### Scenario: Specialize a generic selected-union wrapper validly

- **WHEN** that wrapper is completely applied with `S = FirstError | ThirdError` and concrete `E = FirstError | SecondError | ThirdError`
- **THEN** specialization proves complete containment and computes remainder `SecondError`

#### Scenario: Reject an invalid generic selected-type specialization

- **WHEN** that wrapper is completely applied with `S = never` or a type containing an alternative absent from `E`
- **THEN** specialization rejects the selection before difference, dependency discovery, or operation-specific availability

#### Scenario: Check a selected union

- **WHEN** generic `S` is constrained by `S in E` and specializes to `FirstError | ThirdError` within concrete `E`
- **THEN** the common constraint solver accepts the complete selected subset without lifting `S` into a row kind

#### Scenario: Retain a constraint through partial application

- **WHEN** a provider argument is captured before the protected Effect row is supplied
- **THEN** the callable value's semantic type, identity, substitution, and serialized surface retain the quantified provider-selection obligation and solve it when the remaining argument is statically applied

#### Scenario: Drop an unapplied constrained callable

- **WHEN** a concrete enclosing instance creates and then drops a provider section without applying its protected Effect argument
- **THEN** the section's nested quantified obligation is erased with the callable and is not rejected as an unresolved obligation of the enclosing instance

#### Scenario: Reject a constrained callable that escapes static application

- **WHEN** a constrained callable with unresolved quantified row obligations reaches an opaque module boundary, runtime representation, or indirect invocation without a statically visible complete application
- **THEN** after any closed-world identity-flow analysis needed to prove the absence of a complete terminal application, analysis rejects the escape before runtime representation, row-dependent consumers, or lowering can treat it as a runtime-polymorphic callable

#### Scenario: Forward a constrained callable through a closed static chain

- **WHEN** a constrained callable is passed through one or more statically visible functions and the closed chain ends in a complete application
- **THEN** its quantified binders, constraints, substitutions, and evidence follow the callable through the chain and are solved at that complete application before representation or lowering

#### Scenario: Do not infer selection from an expected remainder

- **WHEN** a provider matches multiple requirements and an expected result row would reveal which member must have been subtracted
- **THEN** analysis still reports provider ambiguity because expected results may validate the computed remainder but MUST NOT bind the selected row or invert `Without`

#### Scenario: Reject a residual row before instance consumers

- **WHEN** a reachable instance still has an open row, unresolved difference, unsatisfied obligation, or ambiguous selection after its substitution is known
- **THEN** analysis rejects that instance before row-dependent dependencies, witnesses, layout, ownership specialization, or lowering consume it

#### Scenario: Solve equivalent source and intrinsic contracts uniformly

- **WHEN** an ordinary Silk declaration and a sealed intrinsic operation have equivalent binders, fixed parameter modes, row expressions, and checked constraints
- **THEN** calls to both produce the same substitutions, evidence, normalized result, diagnostic identity, and canonical candidate payload, with spans local to each call

#### Scenario: Compute a selected failure remainder

- **WHEN** independently known failure type `ProblemError | OtherError` selects `ProblemError`
- **THEN** ordinary difference computes `OtherError` without inferring either operand from the expected remainder

#### Scenario: Infer a requirement remainder for provide

- **WHEN** a generic provider selects `Clock at Primary` from `&Clock at Primary | Rest`
- **THEN** inference binds `Rest` to every other normalized requirement and rejects a provider with incompatible access or role

### Requirement: Channel specialization remains erased and deterministic

Generic ordinary failure types and requirement-row arguments SHALL be checked once, concretized
through the reachable monomorphic worklist, included in canonical specialization identity, and
erased from runtime representation. Equivalent normalized unions and rows MUST produce identical
instances and artifacts across fresh processes; no row dictionary, type descriptor, capability
name, or role string may be required at runtime.

#### Scenario: Reuse equivalent row specializations

- **WHEN** two calls infer the same ordinary failure union and requirement members in different source orders
- **THEN** they reach the same canonical specialization and emit no runtime row object

### Requirement: Generic parameters retain bound conjunctions

A type parameter MAY declare an unordered conjunction of interface or service applications with
`+`. Every conjunct SHALL be preserved as an independent static proof obligation. The conjunction
SHALL NOT create a runtime value, witness bundle, Effect requirement, or general intersection type.

#### Scenario: Call operations from two bounds

- **WHEN** a generic parameter is bounded by `First + Second` and its body calls one qualified operation from each contract
- **THEN** both calls resolve through the parameter's declared static evidence

#### Scenario: Specialize a conjunction

- **WHEN** a concrete generic call supplies a provider with one coherent conformance for every bound conjunct
- **THEN** specialization substitutes all selected witnesses into finite monomorphic code

#### Scenario: Reject a missing conjunct

- **WHEN** a concrete provider satisfies only some conjuncts
- **THEN** the generic application reports the complete missing provider-contract goal before lowering

#### Scenario: Reject a duplicate conjunct

- **WHEN** one bound repeats the same normalized contract application
- **THEN** the compiler reports the later duplicate regardless of source ordering

### Requirement: Static operations share one specialization path

Qualified operations selected from interface bounds, service bounds, and concrete conformances SHALL
use the same implicit-`Self` substitution and canonical witness selection. Services SHALL NOT
introduce a separate generic call or specialization identity.

#### Scenario: Specialize a service bound

- **WHEN** a generic function calls a qualified operation under a service bound and receives a concrete conforming provider
- **THEN** the call specializes to the provider's ordinary static witness with no runtime dependency lookup

### Requirement: Requirement selectors name access-independent keys

A requirement-row key SHALL consist of one canonical service identity and one canonical nominal
role identity. Omitting `at Role` SHALL select `DefaultRole`. Shared and exclusive access SHALL be
stored as the demand associated with a key and SHALL NOT participate in key identity. Union SHALL
retain the strongest demand for colliding keys. `Without<R, K>` SHALL accept the selector
`Service` or `Service at Role` and remove the complete matching key regardless of its demand.

#### Scenario: Merge repeated access demands

- **WHEN** a requirement union contains `&Clock at Primary | &mut Clock at Primary`
- **THEN** it normalizes to the single key `Clock at Primary` with exclusive demand

#### Scenario: Subtract an exclusive requirement by key

- **WHEN** `Without<&mut Clock at Primary | &Logger, Clock at Primary>` is specialized
- **THEN** it normalizes to `&Logger`

#### Scenario: Keep independently declared roles distinct

- **WHEN** two modules each declare a visible role named `Primary`
- **THEN** their canonical identities remain distinct and same-spelled selectors do not collide

### Requirement: Generic pattern selectors renormalize after substitution

A generic body SHALL check pattern selectors against its symbolic normalized member set. Every
complete application SHALL substitute and renormalize selectors and coverage before MIR lowering.
When source-distinct selectors collapse to one concrete member, the first source-ordered reachable
selector SHALL win and later equivalent selectors SHALL emit no duplicate runtime test or new
source diagnostic.

#### Scenario: Collapse two selectors

- **WHEN** source-ordered `A` and `B` patterns over `A | B` specialize with both parameters equal to `i32`
- **THEN** MIR tests one `i32` member and selects the first source arm

#### Scenario: Preserve distinct selectors

- **WHEN** the same patterns specialize with `A = i32` and `B = bool`
- **THEN** both canonical members remain covered by their source-ordered selections
