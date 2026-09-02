## MODIFIED Requirements

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
