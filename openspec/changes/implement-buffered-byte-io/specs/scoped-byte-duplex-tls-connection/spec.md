## ADDED Requirements

### Requirement: Buffered duplex scopes one exclusive transport lease

Canonical `silk.buffered_duplex` SHALL expose `withBuffered`, which exclusively borrows one explicit
concrete `ByteDuplex` provider, allocates separate fixed input and output buffers, and invokes one
higher-ranked callback with a scoped session. The session SHALL bind its private provider reborrow
internally, SHALL omit ByteDuplex from callback requirements, and SHALL prevent the session,
provider loan, or peeked view from escaping. Structured exit SHALL terminally close the provider
through the delivered nonparking bracket while preserving the callback outcome; it SHALL NOT
implicitly flush pending buffered output.

#### Scenario: Close after callback success or failure

- **WHEN** a buffered callback succeeds or returns a typed failure
- **THEN** terminal close runs exactly once and the callback's original outcome remains observable

#### Scenario: Reject ambient transport alias

- **WHEN** a callback attempts independent ByteDuplex access while the scoped session retains its lease
- **THEN** requirement-row ownership rejects the program before execution

### Requirement: Buffered byte delivery remains portable and bounded

The buffering modules SHALL be registered in the standard-library manifest and generated embedding,
documented through executable public API examples, and covered on native and LLVM-generated
WebAssembly where the scripted provider is supported. Default evidence SHALL use the cheapest
adequate structured analysis and shared corpus boundaries and SHALL require no physical socket,
filesystem stream, network, general Stream actor, or provider-type inspection.

#### Scenario: Verify the shipped buffered surface

- **WHEN** manifest, generated source, reference, ownership analysis, and portable runtime checks run
- **THEN** all public buffered actors resolve and their retained-prefix, progress, deadline, and teardown contracts hold without network access
