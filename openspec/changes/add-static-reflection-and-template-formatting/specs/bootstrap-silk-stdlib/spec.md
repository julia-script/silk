## ADDED Requirements

### Requirement: Static reflection and sequences are canonical ordinary source actors

The standard library SHALL ship canonical documented ordinary source actors for static type and
field reflection and immutable static sequences. Public operations SHALL wrap the minimum sealed
intrinsic seam, retain ordinary source identities and spans, and receive no compiler privilege from
their module, actor, type, or operation spelling. Equivalent user source over the same intrinsics
SHALL receive equivalent behavior.

#### Scenario: Navigate a reflection operation

- **WHEN** tooling resolves a public field-reflection or static-sequence operation
- **THEN** go-to-definition opens its canonical `.silk` declaration and only the irreducible primitive call resolves to `Intrinsic`

### Requirement: Template formatting extends the canonical Format actor

The canonical `silk.format` source module SHALL define the static-template formatting operation by
composing static text inspection, static sequences, reflection, `Display`, `Formatter`, and `Writer`.
The placeholder grammar, validation policy, traversal, and Writer composition MUST remain visible
ordinary source and MUST NOT be implemented by a compiler-known Format declaration or a monolithic
format intrinsic.

The module SHALL also provide ordinary-source `Display<string>` by forwarding the borrowed string's
UTF-8 bytes through the existing Writer surface. It MUST preserve Writer prefix/failure behavior and
MUST NOT allocate an intermediate String or introduce a second text-writing path.

#### Scenario: Copy template formatting into user source

- **WHEN** equivalent template parsing and reflection composition is written under another legal module and operation name
- **THEN** it validates and residualizes through the same public and intrinsic contracts without compiler registration

#### Scenario: Navigate string display

- **WHEN** tooling resolves the canonical `Display<string>` implementation
- **THEN** go-to-definition opens its ordinary `silk.format` source declaration and no compiler-known formatting operation

#### Scenario: Package the new source actors

- **WHEN** the compiler package or toolchain distribution is assembled
- **THEN** manifest verification includes the canonical reflection, static-sequence, and updated format source files byte-for-byte
