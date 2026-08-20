## MODIFIED Requirements

### Requirement: Evaluation preserves borrowed backing identity

Logical slices SHALL retain the complete selector path from their backing cell to a nested fixed
array, so shared reads and exclusive writes operate on the selected original subplace. Hidden
temporary cells SHALL remain live until their final derived loan ends.

#### Scenario: Mutate a runtime selected inner array

- **WHEN** evaluation runs `edit(&mut matrix[index])`
- **THEN** the checked inner array in `matrix` changes and no copied temporary receives the write

### Requirement: Evaluation executes callable values exactly

Evaluation SHALL preserve successive section capture order independently of original parameter
order and SHALL end non-escaping reusable capture loans after their last statically known invocation.

#### Scenario: Evaluate a staged section

- **WHEN** evaluation runs `combine(3)(2)(1)`
- **THEN** it invokes `combine(1, 2, 3)` after evaluating each supplied value once
