# bootstrap-declaration-index Specification

## Purpose
The closure-wide declaration index: every top-level declaration header collected and given a
canonical identity before any body resolves, header-level signatures resolved with explicit
unresolved states, and the result published as an immutable, canonically ordered fact table that
downstream phases key against.
## Requirements
### Requirement: Canonical identities precede body resolution

Header collection SHALL visit every public and private top-level function declaration of every
module in the loaded closure and SHALL assign canonical declaration identities — the canonical
module identity plus the declaration name — before any import member or body reference resolves.
The first present occurrence of a name within a module SHALL own the canonical identity; a later
duplicate SHALL remain an explicit duplicate state carrying the original's identity and the
duplicate diagnostic's identity as its cause; a declaration whose name is unavailable after
recovery SHALL remain an explicit unidentified state without a fabricated name. Visibility SHALL
be retained independently from canonical identity.

#### Scenario: Assign identities across modules

- **WHEN** two loaded modules each declare a present function named `answer`
- **THEN** the index contains two headers with distinct canonical identities qualified by their own module identities

#### Scenario: Index public and private declarations

- **WHEN** one module declares `pub fn parse` and `fn helper`
- **THEN** both headers receive canonical identities in concrete order and retain public and private visibility respectively

#### Scenario: Keep duplicates explicit with causes

- **WHEN** one module declares the same present name twice
- **THEN** the first header owns the canonical identity and the second is a duplicate state carrying the first's identity and the `SEM0003` diagnostic's identity as its cause

#### Scenario: Keep unavailable names unidentified

- **WHEN** a declaration's name is a missing token after parser recovery
- **THEN** its header remains in the index as an unidentified state and no module or semantic diagnostic repeats the parser's mistake

### Requirement: Header signatures resolve at the header level

Every header SHALL resolve its explicit public or default-private visibility and complete signature
without touching imports or any body: ordered parameter names and declared types, duplicate
parameter names, return type, and the exact parameter-count contract. Unknown declared types SHALL
produce their stable diagnostics at exact spans, and unresolved or unavailable states SHALL remain
explicit rather than defaulting. Header collection SHALL complete for the entire reachable closure
before cross-module member binding or body elaboration begins.

#### Scenario: Resolve a complete public signature

- **WHEN** a module declares `pub fn choose(left: i32, right: i32) -> i32`
- **THEN** its header is public and resolves two ordered `i32` parameters, an `i32` return type, and a parameter count of two

#### Scenario: Resolve a complete private signature

- **WHEN** a module declares `fn helper(value: i32) -> i32`
- **THEN** its header is private and resolves its parameter and result without consulting a function body

#### Scenario: Diagnose header-level unknown types

- **WHEN** a parameter or return type names an unknown type
- **THEN** the header retains the unresolved type state and the index carries the `SEM0001` diagnostic at the type's exact span

### Requirement: The declaration index is an immutable canonical fact table

The published index SHALL be immutable, SHALL order headers by canonical module identity and then
concrete declaration order, and SHALL answer name lookups per module distinguishing exactly one
match, no match, and multiple matches without discarding any collected header. Identical closures
SHALL produce identical indexes across fresh processes.

#### Scenario: Order headers canonically

- **WHEN** a closure loads modules whose identities sort differently from their traversal order
- **THEN** the index lists headers grouped by canonical module identity in identity order, each module's headers in concrete source order

#### Scenario: Repeat index collection

- **WHEN** the same closure is collected repeatedly in fresh processes
- **THEN** the resulting headers, identities, signature states, lookup outcomes, and diagnostics are identical

#### Scenario: Look up declarations per module

- **WHEN** a module declares one present unique `main`
- **THEN** looking up `main` in that module resolves its header while an undeclared name reports no match

### Requirement: bool is a built-in declared type

Declared parameter and return types SHALL resolve `bool` as a built-in semantic type alongside
`i32`. Any other spelling SHALL keep the existing `SEM0001` unknown-type diagnostic.

#### Scenario: Resolve a bool parameter and return

- **WHEN** `pub fn negate(flag: bool) -> bool { return flag }` is collected
- **THEN** the parameter and return types resolve to `bool` with no diagnostics

#### Scenario: Keep unknown types diagnosed

- **WHEN** a return type spells `Boolean`
- **THEN** the type stays unresolved with one `SEM0001` diagnostic

### Requirement: Downstream references reuse indexed declarations

The published closure-wide index SHALL be the declaration-header authority for name resolution and
body elaboration. A resolved import binding, call-reference fact, HIR call, instance key, and lowered
call SHALL identify the same indexed canonical declaration rather than a phase-local recollection of
its source module. Damaged declarations SHALL retain their indexed recovery state for every
downstream query.

#### Scenario: Reuse one imported declaration identity

- **WHEN** a public function is selected into another module and called from its body
- **THEN** the import binding, reference fact, HIR call, discovered instance, and lowered call all carry the canonical identity assigned by the closure-wide index

#### Scenario: Preserve an indexed unavailable declaration

- **WHEN** a selected member names a declaration whose header cannot form a canonical identity
- **THEN** downstream resolution remains explicitly unavailable and does not synthesize a replacement declaration

### Requirement: Nominal structs join the canonical declaration index

Header collection SHALL visit every public and private top-level struct alongside functions before
body resolution. Structs and functions SHALL share one module-level declaration namespace and one
canonical identity rule, while each header SHALL retain its declaration kind. A same-name collision
between any two declaration kinds SHALL keep the first present declaration as canonical and retain
every later declaration as an explicit duplicate with the same stable diagnostic family.

#### Scenario: Index functions and structs together

- **WHEN** a module declares a struct and a function with distinct names
- **THEN** both headers appear in concrete order with canonical identities and distinct declaration kinds

#### Scenario: Reject a cross-kind duplicate

- **WHEN** a module declares `struct Token {}` and `fn Token() -> i32`
- **THEN** the struct owns the canonical module-level identity and the function remains an explicit duplicate

#### Scenario: Order mixed declarations canonically

- **WHEN** multiple modules contain interleaved struct and function declarations
- **THEN** the index groups them by canonical module identity and preserves each module's concrete declaration order

### Requirement: Struct field headers resolve before bodies

Each identified struct header SHALL publish its ordered field headers and resolve their type paths
against the completed closure-wide declaration and module scopes before any function body is
elaborated. Field resolution SHALL preserve missing, unknown, inaccessible, conflicting, duplicate,
and recursive states without replacing them with scalar defaults. Repeated collection of an
identical closure SHALL produce byte-identical struct headers and dependency facts.

#### Scenario: Resolve a forward nominal field

- **WHEN** a struct field names another struct declared later in the same module
- **THEN** the field resolves to the later struct's canonical identity without source-order dependence

#### Scenario: Resolve a cross-module field

- **WHEN** a field names a public struct through an imported namespace alias
- **THEN** the field header identifies the imported struct's canonical declaration identity

#### Scenario: Preserve an inaccessible field type

- **WHEN** a field names a private struct from another module
- **THEN** the field retains the inaccessible candidate and diagnostic cause without an available type

### Requirement: Function headers publish flow kind and failure contracts

Declaration indexing SHALL retain whether each declaration is ordinary or flow and SHALL resolve
every declared failure member to canonical nominal identity. Damaged, non-nominal, inaccessible, or
unknown members SHALL remain explicit unavailable facts with their originating diagnostics.

#### Scenario: Index a public flow contract

- **WHEN** a public flow declares a normalized row of imported nominal errors
- **THEN** its header exposes the flow kind and canonical row independently of body analysis order

### Requirement: Parametric conformances join the canonical index

The declaration index SHALL record parametric conformances with their complete kinded parameter
lists, conditional interface requirements, canonical provider/interface heads, mapped operations,
visibility, overlap state, and structural-termination facts. It SHALL validate restricted Drop hook
shape against the generic target and SHALL conservatively reject possibly overlapping conditional
heads without consulting their bounds. Validation that depends on a concrete instantiation SHALL be
deferred to specialization.

#### Scenario: Index a parametric Drop hook

- **WHEN** the index processes `impl<T> Drop for Vector<T>` whose hook is `fn drop(self: &mut Vector<T>) -> ()`
- **THEN** the hook validates with `T` in scope and the conformance fact records the parameter list and generic target

#### Scenario: Index a conditional user conformance

- **WHEN** a wrapper conformance declares one strict-subterm provider requirement
- **THEN** the canonical header records that requirement and its finite structural measure

#### Scenario: Reject overlapping conditional heads

- **WHEN** two declarations have provider/interface heads that may unify despite different bounds
- **THEN** indexing reports deterministic overlap before either bound is proved

#### Scenario: Header validation still rejects malformed hooks

- **WHEN** a parametric Drop hook declares extra parameters, a failure row, or a mismatched self type
- **THEN** the index reports the existing invalid-Drop-hook diagnostic without waiting for an instantiation

### Requirement: Constants join the canonical declaration index

The declaration index SHALL collect each top-level constant before body analysis with its canonical
identity, visibility, declared primitive type syntax, literal initializer syntax, duplicate state,
and exact source provenance. Constants SHALL share the module's flat top-level namespace with
functions and structs.

#### Scenario: Detect a cross-kind duplicate

- **WHEN** a constant and a function in one module declare the same name
- **THEN** the first declaration remains canonical and the later declaration records the ordinary duplicate identity and diagnostic

#### Scenario: Publish a constant header before function bodies

- **WHEN** a function precedes or follows a valid constant in source order
- **THEN** its body resolves the same canonical constant header without order dependence

### Requirement: The declaration index stores one canonical contract fact

Interface and service headers SHALL lower to one canonical contract fact containing implicit
`Self`, ordered operation contracts, explicit generic parameters, visibility, and dependency
eligibility. Any interface-only or service-only collection exposed by inspectors SHALL be a
projection of those facts rather than an independent semantic identity.

#### Scenario: Index an interface and a service

- **WHEN** one module declares an interface and a service with equivalent operation shapes
- **THEN** both declarations have the same contract fact shape and differ only in declaration identity and dependency eligibility

#### Scenario: Preserve ordered operations

- **WHEN** a contract declares multiple operations and a conformance mixes inline bodies with mapped functions
- **THEN** the index resolves one witness table in contract operation order

### Requirement: The declaration index stores one canonical conformance identity

A conformance identity SHALL consist of one contract application, one provider type, normalized
conditional requirements, and one resolved witness table. Provider matching, proof search, static
calls, and service provision SHALL reuse that identity rather than synthesizing service-specific or
duplicated-provider witnesses.

#### Scenario: Reuse one service conformance for provision and bounds

- **WHEN** one provider conforms to a service used both as a generic bound and as an Effect dependency
- **THEN** bound proof and provider selection reference the same indexed conformance identity

#### Scenario: Withhold an invalid witness

- **WHEN** completeness, signature, locality, overlap, termination, or visibility validation rejects a conformance
- **THEN** the index publishes no witness for static calls, proof search, or provider selection
