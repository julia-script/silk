## ADDED Requirements

### Requirement: Nominal unions join the canonical declaration index

The declaration index SHALL publish each union's canonical nominal identity, ordered type
parameters, visibility, ordered variants, variant kind, named fields, field visibility and declared
types, syntax, and explicit validity state before expression bodies are resolved. Variant and field
identities SHALL be subordinate to the canonical parent union rather than detached declarations.
Union declarations SHALL join functions, structs, enums, and other top-level declarations in the
ordinary module namespace and cross-kind duplicate policy.

#### Scenario: Index a generic union before bodies

- **WHEN** one module declares `union Result<A, E> { Success { value: A }, Failure { error: E } }`
- **THEN** later headers and bodies resolve the same canonical parent, variant, field, and parameter identities independent of source traversal order

#### Scenario: Preserve damaged declaration facts

- **WHEN** one variant field is unavailable but sibling variants are valid
- **THEN** the index retains explicit unavailable state for the damaged field and queryable canonical facts for the valid siblings

#### Scenario: Reject a cross-kind union collision

- **WHEN** a module declares `struct Result {}` and then `union Result { Success }`
- **THEN** the struct retains the canonical module-level identity and the union remains an explicit cross-kind duplicate

### Requirement: Union field headers resolve before bodies

Every identified union header SHALL publish ordered variant and field headers and resolve each field
type against completed closure-wide declaration and module scopes before any expression body is
elaborated. Forward and cross-module type paths SHALL use canonical identities; missing, unknown,
inaccessible, conflicting, duplicate, recursive, and visibility-invalid states SHALL remain explicit
without fabricated fallback types.

#### Scenario: Resolve a forward variant field

- **WHEN** a variant field names a public nominal type declared later in the same module
- **THEN** its header resolves that later canonical identity without source-order dependence

#### Scenario: Preserve an inaccessible variant field type

- **WHEN** a public variant field exposes a private nominal type
- **THEN** its field dependency remains queryable but unavailable with the ordinary exposure diagnostic before body analysis
