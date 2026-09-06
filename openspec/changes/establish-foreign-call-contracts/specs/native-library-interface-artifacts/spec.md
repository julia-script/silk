## MODIFIED Requirements

### Requirement: The ABI manifest is versioned canonical data

The JSON manifest SHALL contain schema marker `silkForeignAbi: 2`, the canonical target id, and
`exports` and `imports` arrays. Every entry SHALL contain its symbol, explicit ABI `C`, lowercase
direction, and kind. Function entries SHALL contain canonical parameter and result classes and the complete normalized behavioral contract; data
entries SHALL contain their canonical type class. Entries SHALL be ordered by symbol and then kind,
object fields SHALL have one stable order, and the document SHALL end with one newline.

#### Scenario: Record the complete native ABI

- **WHEN** a library artifact retains imported and exported functions and data symbols
- **THEN** the manifest records every retained entry once under its direction with the target-qualified ABI classes

#### Scenario: Emit deterministic target-specific bytes

- **WHEN** the same admitted source is rendered repeatedly for one native target
- **THEN** the manifest bytes are identical and target-sized integers use that target's fixed-width class

#### Scenario: Reject incompatible behavioral interfaces

- **WHEN** supplied imported function records disagree with a visible contract despite equal machine classes
- **THEN** interface validation rejects the mismatch and retains both origins; obsolete type-only schema records are rejected
