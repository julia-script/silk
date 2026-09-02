# language-server-completion Specification

## Purpose

Defines deterministic, recovery-aware Silk completions selected from compiler scope, type, member,
intrinsic, and syntax context rather than protocol-side spelling heuristics.

## Requirements

### Requirement: Type completion offers canonical integer types

Type completion SHALL derive the ordered lowercase integer vocabulary plus `bool`, `()`, and `never` from semantic identities and MUST NOT offer removed uppercase aliases.

#### Scenario: Complete an integer type

- **WHEN** completion is requested in a type position
- **THEN** all fixed- and target-width integer spellings are offered deterministically

### Requirement: The server advertises context-aware completion

The language server SHALL advertise completion support, including qualified-member triggering after
`.` where the client supports trigger characters, and SHALL interpret request positions using the
negotiated position encoding.

#### Scenario: Client initializes completion support

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise completion support and `.` as a member trigger

### Requirement: Expression completion follows visible semantic scope

At an expression position, completion SHALL include visible local bindings, pattern bindings,
parameters, callable declarations, value constructors, imported values, and applicable language
keywords. Candidates SHALL follow compiler visibility and lexical shadowing; the LSP MUST NOT
reconstruct scope or merge same-spelled declarations independently.

#### Scenario: Complete a shadowed local

- **WHEN** completion is requested inside a nested block where a local shadows an outer binding
- **THEN** the candidate for that spelling identifies the nearest compiler-selected binding

#### Scenario: Complete at the start of an expression

- **WHEN** a user begins an expression in a valid function body
- **THEN** completion includes the visible values, callable declarations, and expression keywords valid at that position

### Requirement: Type completion follows type context

At a declared-type or type-argument position, completion SHALL include visible nominal types, type
parameters, built-in types, and imported types that are accessible in that context. Value-only
bindings and expression-only keywords MUST NOT be offered as type candidates.

#### Scenario: Complete a function parameter type

- **WHEN** completion is requested after `parameter:` in a function declaration
- **THEN** completion includes accessible types and type parameters but excludes local values

#### Scenario: Complete a generic type argument

- **WHEN** completion is requested inside an incomplete type argument list
- **THEN** completion remains available from the recovered type context

### Requirement: Qualified completion uses resolved actor and value semantics

After a qualifier or typed subject followed by `.`, completion SHALL return exactly the items the
resolved subject exposes: associated items of a nominal declaration, public root declarations of a
module namespace, or fields and receiver methods of a typed value. Completion after a nominal
qualifier MUST NOT include root declarations of the declaring module, and completion after a
namespace MUST NOT include inherent members. Intrinsic operations SHALL come from the same
authoritative catalog used by analysis and hover.

#### Scenario: Complete an Effect operation

- **WHEN** completion is requested after `Effect.`
- **THEN** the result includes the supported Effect operations with source-like signature detail

#### Scenario: Complete an allocator operation

- **WHEN** completion is requested after `SystemAllocator.`
- **THEN** the result includes `make` with the same signature presented by hover

#### Scenario: Complete a struct field

- **WHEN** completion is requested after a value whose available type has accessible fields
- **THEN** the result includes those fields and excludes fields unavailable by visibility or subject type

#### Scenario: Exclude root declarations after a nominal qualifier

- **WHEN** completion is requested after `Option.` and `silk/option` declares a private root helper
- **THEN** the result lists the variants and inherent members and excludes the helper

#### Scenario: Exclude members after a namespace

- **WHEN** completion is requested after `OptionModule.` for `import silk.option as OptionModule`
- **THEN** the result lists `Option` and any public root declarations and excludes `map`

### Requirement: Completion remains deterministic under recovery

Completion SHALL use available facts from the exact synchronized snapshot even when the token at
the cursor or unrelated source is incomplete. Missing, inaccessible, ambiguous, or conflicting
facts MUST NOT be guessed from spelling. Candidate ordering, labels, kinds, insertion text, and
signature detail SHALL be deterministic for identical source and position.

#### Scenario: Complete after a partial identifier

- **WHEN** the user requests completion after typing a partial identifier in otherwise analyzable source
- **THEN** available candidates are returned without requiring the partial spelling to resolve as a complete reference

#### Scenario: Ambiguous recovered qualifier

- **WHEN** a recovered qualifier remains semantically ambiguous
- **THEN** completion does not invent one candidate member set from source order

#### Scenario: Repeat completion

- **WHEN** identical snapshots are queried repeatedly at the same position
- **THEN** the ordered completion items and their semantic details are identical

### Requirement: Type completion offers float types

Type completion SHALL offer canonical `f32` and `f64` items derived from semantic type identities.

#### Scenario: Complete a float type

- **WHEN** completion is requested in a type position
- **THEN** `f32` and `f64` appear deterministically

### Requirement: Completion keeps intrinsics explicit

Qualified completion on `Intrinsic` SHALL offer the deterministic catalog of compiler primitives
with concrete signatures and safety markers. Ordinary expression and actor completion SHALL offer
visible source APIs and MUST NOT leak unqualified intrinsic operations or former compiler-known
actor members. Standard-library APIs SHALL remain the preferred completion path outside the sealed
namespace.

#### Scenario: Complete integer operations

- **WHEN** completion is requested after `Intrinsic.` and after an ordinary integer API qualifier
- **THEN** the first result set contains concrete primitives and the second contains source-defined numeric operations

### Requirement: Pattern-local completion follows visible semantic scope

Expression completion SHALL include shared pattern bindings only where their semantic scope is
active. Irrefutable let bindings SHALL appear after their declaration; match-arm and if-let
bindings SHALL appear only in the selected body and SHALL NOT appear in a mismatch body or after
their lexical scope.

#### Scenario: Complete inside if-let

- **WHEN** completion is requested inside the taken and mismatch bodies of one if-let
- **THEN** the pattern binding appears only in the taken-body result

### Requirement: Type-qualified completion lists associated members

After a nominal qualifier followed by `.`, completion SHALL list that declaration's associated
items: variants, enum members, generated operations, declared contract operations, and accessible
inherent members, each labeled by kind so a receiver method, an associated function, and a variant
are distinguishable. Inherent members SHALL present the same signature hover presents, including the
receiver as the first parameter. Private members SHALL be listed only inside their declaring module.

#### Scenario: Complete Option members

- **WHEN** completion is requested after `Option.` with `impl<T> Option<T>` declaring `none`, `some`, and `map`
- **THEN** the result lists `None`, `Some`, `none`, `some`, and `map` with `map` labeled as a method and `some` as an associated function

#### Scenario: Exclude a private member outside its module

- **WHEN** completion is requested after `Counter.` from another module and `Counter` has a private inherent member
- **THEN** the private member is absent and public members are present

### Requirement: Value-qualified completion lists receiver methods

After a typed value followed by `.`, completion SHALL list accessible fields and, for a nominal or
bounded generic subject, the receiver methods available to that subject: inherent receiver
methods, or for a generic subject the receiver operations of its bounds, labeled as methods and presented
with their receiver-bound signature. Associated functions without a receiver SHALL NOT be listed
after a value. Completion after a chained receiver such as `a.b.` or `f().` is outside this
requirement.

#### Scenario: Complete methods and fields on a value

- **WHEN** completion is requested after `option.` with `option: Option<i32>`
- **THEN** the result lists `map`, `flatMap`, and `unwrapOr` as methods with `T` shown as `i32`, and excludes `none` and `some`

#### Scenario: Complete a bound's operation on a generic value

- **WHEN** completion is requested after `value.` inside `fn show<T: Printable>(value: &T)`
- **THEN** the result lists `print` from the `Printable` bound and nothing from any concrete conformance

### Requirement: Catalog namespaces are explicit-import completion candidates

In expression and actor-name contexts, completion SHALL include applicable preferred namespace
spellings from the deterministic distribution catalog even when no same-named source declaration
exists and even when the spelling at the cursor is partial. Each such candidate SHALL identify its
canonical module and SHALL remain an explicit-import option when the complete non-type spelling is
present but unavailable as a namespace binding. Completion MUST NOT turn catalog metadata into
semantic scope. In declared-type and type-argument contexts, the closed language `Effect` type
candidate SHALL remain import-free and the same spelling MUST NOT cause a namespace import edit.

#### Scenario: Complete a partial Effect namespace

- **WHEN** completion is requested after `Eff` in an expression or actor-name context without an `Effect` namespace binding
- **THEN** the results include an `Effect` candidate identified as coming from `silk/effect` with an explicit namespace-import edit

#### Scenario: Complete the full unavailable namespace spelling

- **WHEN** completion is requested on a complete non-type `Effect` spelling without the namespace import
- **THEN** the explicit-import `Effect` candidate remains available

#### Scenario: Keep Effect type completion import-free

- **WHEN** completion is requested for `Effect` in a declared-type or type-argument context
- **THEN** the closed language type candidate is offered without an import edit and no namespace-import candidate is added solely for that type use

#### Scenario: Complete imported Effect members

- **WHEN** `silk/effect` is imported under namespace `Effect` and completion is requested after `Effect.`
- **THEN** completion exposes the module's accessible public source operations through the ordinary namespace binding

#### Scenario: Repeat partial namespace completion

- **WHEN** identical source and catalog snapshots request completion for the same partial namespace spelling repeatedly
- **THEN** candidate labels, module identity, ordering, insertion text, and import edits are identical
