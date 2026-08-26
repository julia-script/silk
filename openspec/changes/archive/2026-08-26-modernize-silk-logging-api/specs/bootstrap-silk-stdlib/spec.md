## MODIFIED Requirements

### Requirement: Logging is canonical visible Silk source

The standard library SHALL ship canonical `.silk` declarations for the scalar `LogLevel` enum,
`LogError`, Logger, `Effect.log`, `Effect.logAt`, `Effect.logTrace`, `Effect.logDebug`,
`Effect.logInfo`, `Effect.logWarning`, `Effect.logError`, the initial stdout and in-memory
providers, and provider-owned recorded observation values where needed. These declarations SHALL
participate in the deterministic standard-library manifest, retain ordinary source spans in
diagnostics and editor facts, and receive no semantic privilege from their module identity.

#### Scenario: Navigate to the Logger contract

- **WHEN** editor tooling resolves a Logger implementation, a `LogLevel` member, or an Effect logging helper
- **THEN** go-to-definition opens the canonical shipped Silk declaration rather than a generated TypeScript signature

#### Scenario: Copy the logging contract into user source

- **WHEN** equivalent enum, service, provider, and helper declarations are written in a user module
- **THEN** they receive the same parsing, conformance, ownership, Effect, and lowering behavior without intrinsic registration

### Requirement: Semantic text boundaries use string

Shipped standard-library APIs SHALL use `string` for complete logging messages, normalized path
construction and resolution, path text accessors, and native filesystem roots. This SHALL include
`Effect.log`, `Effect.logAt`, every level-specific Effect logging helper, and `Logger.log`.
Implementations SHALL request UTF-8 byte views explicitly where text reaches byte storage, standard
streams, or raw OS operations. APIs whose domain is arbitrary bytes, including `Bytes`, whole-file
contents, and standard streams, SHALL remain byte-oriented.

#### Scenario: Log semantic text

- **WHEN** source submits a complete message through any Effect logging helper or `Logger.log`
- **THEN** the API accepts `string` and a provider converts it to bytes only if its output boundary requires an encoding

#### Scenario: Construct and inspect paths as text

- **WHEN** source constructs, joins, resolves, or inspects a normalized `Path`
- **THEN** the textual inputs and borrowed textual outputs use `string` without exposing the path's owned byte storage

#### Scenario: Preserve binary boundaries

- **WHEN** source reads file contents, writes standard streams, or manipulates arbitrary byte collections
- **THEN** those APIs continue to use byte-oriented values rather than reclassifying binary data as text
