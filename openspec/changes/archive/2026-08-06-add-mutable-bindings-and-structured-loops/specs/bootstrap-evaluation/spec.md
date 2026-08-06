## ADDED Requirements

### Requirement: Evaluation executes writes as complete replacement

Evaluation SHALL execute `WritePlace` by checking selectors, evaluating the right-hand value, applying
replacement cleanup, and committing one complete logical root value in the specified order. Logical
struct and array values SHALL remain immutable snapshots; a successful write SHALL publish a new root
value rather than expose backend storage or aliases.

#### Scenario: Evaluate an indexed increment

- **WHEN** a loop assigns `values[index] = values[index] + 1`
- **THEN** evaluation changes only the selected logical element and later reads observe the new complete array

### Requirement: Evaluation executes the structured control DAG directly

Evaluation SHALL traverse ordered DAG regions and implement loop repetition from the explicit loop
region's condition, repeat, and exit outcomes. It MUST NOT first flatten the program into a cyclic CFG.
Condition, transfer, write, cleanup, and trap events SHALL remain compact deterministic data with
canonical region and source provenance.

#### Scenario: Evaluate continue and break

- **WHEN** a loop continues for early elements and breaks on a later element
- **THEN** evaluation follows the canonical repeat and exit outcomes and reports the exact iteration order

#### Scenario: Trap before an out-of-bounds write

- **WHEN** a loop attempts a dynamic array write outside its canonical length
- **THEN** evaluation traps at that selector before evaluating or committing the right-hand replacement
