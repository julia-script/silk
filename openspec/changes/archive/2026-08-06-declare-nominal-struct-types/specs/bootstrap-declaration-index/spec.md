## ADDED Requirements

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

- **WHEN** a module declares `struct Token {}` and `fn Token() -> I32`
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
