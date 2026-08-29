## ADDED Requirements

### Requirement: Over-budget expression syntax remains a deterministic lossless artifact

A `SyntaxFile` produced from source exceeding the expression-nesting limit SHALL own the complete
original token stream, an explicit recovered error branch for every maximal over-budget expression
region, and the corresponding parser diagnostics. Traversing its concrete tree SHALL encounter
every original token exactly once, and concatenating every non-end-of-file token slice in source
order SHALL reconstruct the original bytes exactly. Equivalent source identities and bytes SHALL
produce identical recovered trees, token identities, spans, diagnostics, and textual encodings.

#### Scenario: Reconstruct a substantially over-budget source

- **WHEN** a source contains an expression substantially deeper than 256 followed by valid syntax
- **THEN** the recovered `SyntaxFile` reconstructs every original byte and retains the following syntax outside the error branch

#### Scenario: Traverse each original token once

- **WHEN** the recovered tree for an over-budget expression is flattened to its original tokens
- **THEN** the flattened token sequence contains the same token objects as the artifact token stream in the same order with no omissions or duplicates

#### Scenario: Repeat over-budget artifact construction

- **WHEN** identical over-budget source is parsed repeatedly in fresh processes
- **THEN** the recovered tree, element identities, diagnostic sequence, and textual encoding are byte-identical across runs
