## ADDED Requirements

### Requirement: Analyze nested expression facts recursively
Every concrete call used as an argument SHALL produce a recursive call-expression fact with its
own exact syntax provenance, target-resolution state, ordered argument facts, positional contract,
and result-type state. Analysis SHALL resolve and type nested expressions from their leaves outward
while retaining concrete source order and stable identities at every depth. A missing, ambiguous,
damaged, or type-unavailable inner expression SHALL make only its dependent outer contract or type
unavailable and MUST NOT invent a target, binding, value, or duplicate parser-owned diagnostic.

#### Scenario: Analyze one nested identity call
- **WHEN** `main` returns `identity(identity(42))` and both calls resolve uniquely
- **THEN** the outer argument contains a nested call fact whose literal argument, positional contract, result type, call-site span, and target identity are all available

#### Scenario: Preserve nested sibling order
- **WHEN** a call has two nested call arguments in concrete source order
- **THEN** both outer argument ordinals and every nested argument ordinal remain deterministic and match their respective concrete lists

#### Scenario: Propagate an unavailable inner target
- **WHEN** an inner call target is missing or ambiguous
- **THEN** the inner resolution and provenance remain visible while its result type and the dependent outer contract are unavailable without selecting a target or inventing a binding

#### Scenario: Keep inner and outer diagnostics phase-owned
- **WHEN** malformed inner syntax already has a parser diagnostic or a uniquely resolved inner call has the wrong arity
- **THEN** analysis preserves the parser-owned error or emits the applicable inner semantic diagnostic exactly once without adding a speculative outer mismatch diagnostic

#### Scenario: Repeat nested analysis
- **WHEN** an equivalent nested program is analyzed repeatedly in fresh processes
- **THEN** every nested identity, resolution state, contract, type, provenance item, and diagnostic appears in the same order
