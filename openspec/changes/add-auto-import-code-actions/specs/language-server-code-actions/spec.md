## MODIFIED Requirements

### Requirement: A diagnostic without an edit produces no action

A diagnostic that carries no `Edit` SHALL contribute no diagnostic-derived replacement action, and
the server MUST NOT invent corrected bytes from diagnostic prose. A compiler-owned source-action
query MAY nevertheless use the same accepted semantic snapshot and diagnostic identity to offer a
candidate-generating correction whose applicability and complete change plan are established
independently of `Diagnostic.Edit`. A request for which neither mechanism applies SHALL return an
empty list.

#### Scenario: A non-actionable diagnostic remains without a fix

- **WHEN** a code-action request covers a diagnostic with no edit and no applicable compiler-owned source action
- **THEN** the server returns no action for that diagnostic

#### Scenario: An unresolved name offers auto-import choices

- **WHEN** a code-action request covers an unresolved name with importable declarations in two modules
- **THEN** the server returns the two compiler-owned auto-import actions even though the unresolved-name diagnostic carries no embedded edit

### Requirement: Quick fixes are deterministic and range-limited

Code actions SHALL be returned in deterministic order for one accepted snapshot. Diagnostic-derived
actions SHALL preserve diagnostic order, and candidate-generating actions for one diagnostic SHALL
preserve their compiler-owned ranking. Only diagnostics and source-action targets whose published
range intersects the requested range SHALL contribute an action.

#### Scenario: Two fixable diagnostics in one file

- **WHEN** a code-action request covers a file holding two edit-carrying diagnostics
- **THEN** the server returns their actions in the same relative order as the diagnostics

#### Scenario: Order several auto-import candidates

- **WHEN** one unresolved reference has several applicable auto-import candidates
- **THEN** the candidates appear contiguously in their deterministic compiler-owned order

#### Scenario: Limit actions to the requested range

- **WHEN** a code-action request covers a range that excludes the only edit-carrying diagnostic and every source-action target in the file
- **THEN** the server returns no code action
