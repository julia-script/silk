## MODIFIED Requirements

### Requirement: Option is ordinary canonical Silk source

The standard library SHALL define `Option<T>` as an ordinary shipped nominal union with unit
variant `None` and named-field variant `Some { pub value: T }`. The parent union SHALL be public, so
its variants are externally selectable, and the payload field SHALL be public for direct construction
and matching. Recoverable integer operations and every other optional-value consumer SHALL use this
declaration without an Option-shaped compiler collection primitive. The `none`, `some`, `map`,
`flatMap`, and `unwrapOr` operations SHALL be inherent members declared in `impl<T> Option<T>`,
reachable as `Option.member`, and MUST NOT be root declarations of the module. The former
transparent wrapper struct, detached `Some<T>` and `None` structs, compatibility aliases, and dual
representations MUST NOT remain.

#### Scenario: Return checked success

- **WHEN** checked integer arithmetic succeeds
- **THEN** it returns the canonical `Option<T>.Some` variant containing the exact value

#### Scenario: Return checked failure

- **WHEN** checked integer arithmetic cannot represent a result
- **THEN** it returns canonical `Option<T>.None`

#### Scenario: Remove the wrapper representation

- **WHEN** standard-library source, manifests, documentation, and tests are inspected after migration
- **THEN** `Option<T>` is the direct nominal union and no detached `Some<T>`, detached `None`, wrapper `value` field, alias, or compatibility path remains

#### Scenario: Reach Option operations as members

- **WHEN** source imports `silk.option { Option }` and evaluates `Option.some(2) |> Option.map(addOne)`
- **THEN** both operations resolve to inherent members of `Option` and `import silk.option { some }` reports an unknown root member

### Requirement: Nonprimitive operation modules expose importable scope actors

Each canonical nonprimitive standard-library operation module SHALL declare its public operations
as inherent members of one public owner nominal: the module's principal data type when it has one,
otherwise an ordinary public zero-data owner struct. Selecting the owner SHALL expose exactly its
associated items under that qualifier, without compiler privilege, without a runtime
representation, and without exposing any other root declaration of the module. Primitive modules
SHALL remain intrinsic namespaces.

#### Scenario: Select the RawBuffer scope actor

- **WHEN** source imports `silk.raw_buffer { RawBuffer }` and calls `RawBuffer.from<T>`
- **THEN** name resolution reaches the inherent member `from` of `RawBuffer` and reports no missing-member diagnostic

#### Scenario: Preserve an example qualifier

- **WHEN** a documented example that qualified operations through the owner is compiled after migration
- **THEN** every `Owner.operation` qualifier in the example remains unchanged and resolves to the inherent member

#### Scenario: Keep primitive modules as namespaces

- **WHEN** source uses operations from `silk.u8`, `silk.u32`, or `silk.usize`
- **THEN** the canonical import is the unaliased module import and the lowercase primitive qualifier remains available

#### Scenario: Scope actors remain ordinary source

- **WHEN** tooling navigates an imported standard-library owner or one of its members
- **THEN** it reaches a public declaration in canonical Silk source with no compiler-known actor or module-origin exception

#### Scenario: Effect operations are members of Effect

- **WHEN** source imports `silk.effect { Effect }` and evaluates `computation |> Effect.provide(&clock)`
- **THEN** `provide` resolves to an inherent member of the `Effect` owner declared in `silk/effect`

## ADDED Requirements

### Requirement: Standard-library membership is declared, not projected

No standard-library module SHALL rely on its basename to expose operations through a type. Every
public operation intended to be qualified by a type SHALL be declared inside that type's inherent
impl, and every remaining public root function SHALL be reachable only unqualified, by selective
import, or through a namespace import. Renaming a standard-library file SHALL NOT change which
operations any type exposes.

#### Scenario: Audit the shipped sources

- **WHEN** the shipped standard-library sources are inspected after migration
- **THEN** no module exposes an operation through a type without an inherent impl declaring it, and the source-table check passes

#### Scenario: Rename a module without changing its API

- **WHEN** a test copies a migrated module under a different file name and imports its owner type
- **THEN** every `Owner.member` call resolves exactly as before
