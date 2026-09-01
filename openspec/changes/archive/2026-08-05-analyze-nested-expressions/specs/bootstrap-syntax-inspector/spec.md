## ADDED Requirements

### Requirement: Inspect recursive semantic expression facts

The Syntax Inspector SHALL provide valid, unresolved, incompatible, and syntax-damaged nested-call
presets. Its semantic view SHALL expose every nested call, argument, target-resolution state,
positional mapping, contract, result type, and exact source provenance as a hierarchy matching the
concrete expression nesting. Known inner facts SHALL remain visible when a dependent outer fact is
unavailable, and the view MUST NOT collapse nested calls into flat arguments or imply an AST, HIR,
or MIR.

#### Scenario: Inspect nested identity semantics

- **WHEN** a developer selects the `identity(identity(42))` preset
- **THEN** the semantic view shows the inner literal-to-parameter contract, the inner call result as the outer argument, and the outer contract with links to both call spans

#### Scenario: Inspect an unavailable inner relationship

- **WHEN** a developer selects a preset whose inner target is missing or ambiguous
- **THEN** the inner candidates or missing state remain visible and the dependent outer contract ends in a labeled unavailable state without an invented edge

#### Scenario: Inspect the temporary evaluation boundary

- **WHEN** a developer evaluates a semantically valid nested-call preset before recursive evaluation is available
- **THEN** the inspector shows the unsupported-nested-expression reason, partial trace, and exact inner source span while the nested semantic facts remain inspectable
