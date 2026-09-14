## ADDED Requirements

### Requirement: Buffered duplex scopes one exclusive transport lease

Canonical `silk.buffered_duplex` SHALL expose `withBuffered`, which exclusively borrows one explicit
concrete `ByteDuplex` provider, allocates separate fixed input and output buffers, and invokes one
higher-ranked callback with a scoped session. The session SHALL bind its private provider reborrow
internally, SHALL omit ByteDuplex from callback requirements, and SHALL prevent the session,
provider loan, or peeked view from escaping. Every structured Effect exit (success, typed failure,
or structured cancellation/interruption) SHALL terminally close the provider through the delivered
nonparking bracket while preserving the callback outcome; it SHALL NOT implicitly flush pending
buffered output. Fatal traps SHALL follow the language rule that bypasses finalizers and `Drop` and
are outside this release guarantee. The same actor SHALL expose an explicitly bounded paired scope
for transfer, validate all four capacities before allocation or lease acquisition, publish two
independently borrowed sessions to one higher-ranked callback, and close both providers exactly once
on every structured Effect exit without replacing the callback outcome.

The actor SHALL also expose `withBufferedCapacityContext`, which consumes one affine context value
into the same validated exclusive provider bracket. A compile-time `BufferedContext` witness SHALL
select a named adapter that consumes the context and receives an independently higher-ranked
temporary session; selection SHALL add no runtime service or requirement-row member. Neither the
session nor a derived peek MAY escape through the context or result.

#### Scenario: Close after callback success or failure

- **WHEN** a buffered callback succeeds or returns a typed failure
- **THEN** terminal close runs exactly once and the callback's original outcome remains observable

#### Scenario: Reject ambient transport alias

- **WHEN** a callback attempts independent ByteDuplex access while the scoped session retains its lease
- **THEN** requirement-row ownership rejects the program before execution

#### Scenario: Consume callback context without leaking the session

- **WHEN** contextual acquisition selects a named adapter for one owned affine context
- **THEN** the adapter may consume that state while ownership rejects returning the session or a
  borrow derived from it, and no runtime service is introduced

#### Scenario: Acquire two transfer sessions atomically

- **WHEN** paired buffered acquisition receives four valid capacities and distinct transports
- **THEN** it publishes both sessions together and closes both transports exactly once after the callback

#### Scenario: Reject an invalid paired capacity before allocation

- **WHEN** any of the four paired capacities is invalid
- **THEN** neither session allocates and neither transport lease is acquired

#### Scenario: Release a suspended pair after structured cancellation

- **WHEN** a paired callback suspends and its execution is structurally canceled or interrupted
- **THEN** terminal close runs exactly once for each transport

#### Scenario: Preserve the paired callback outcome across close failures

- **WHEN** either transport close returns a typed failure after paired acquisition has completed its callback
- **THEN** release still attempts the other close and preserves the callback's structured outcome

#### Scenario: Exclude fatal traps from resource unwinding

- **WHEN** execution ends in a fatal trap instead of a structured Effect exit
- **THEN** the scope makes no finalizer or `Drop` execution guarantee

### Requirement: Buffered byte delivery remains portable and bounded

The buffering modules SHALL be registered in the standard-library manifest and generated embedding,
documented through executable public API examples, and covered on native and LLVM-generated
WebAssembly where the scripted provider is supported. Default evidence SHALL use the cheapest
adequate structured analysis and shared corpus boundaries and SHALL require no physical socket,
filesystem stream, network, general Stream actor, or provider-type inspection.

#### Scenario: Verify the shipped buffered surface

- **WHEN** manifest, generated source, reference, ownership analysis, and portable runtime checks run
- **THEN** all public buffered actors resolve and their retained-prefix, progress, deadline, and teardown contracts hold without network access
