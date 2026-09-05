## MODIFIED Requirements

### Requirement: Loop transfers are lexical and cleanup-safe

`break` and `continue` SHALL target the innermost enclosing loop within the current execution boundary and SHALL be invalid outside such a loop. Ordinary match-arm blocks, including those nested in larger expressions, SHALL introduce no loop or execution boundary. Explicit nested callable and Effect bodies SHALL preserve their existing transfer boundaries. A while body SHALL establish its loop transfer target; its condition SHALL retain the surrounding transfer context rather than targeting the loop whose condition is being evaluated.
Neither form carries a value during bootstrap. Before any `continue`, `break`, or `return` leaves its
current lexical regions, every live owner acquired in those regions SHALL be cleaned exactly once in
the established reverse acquisition order.

#### Scenario: Continue cleans one iteration

- **WHEN** an iteration creates a move-only local and then continues
- **THEN** that local is cleaned before the next condition evaluation and is not cleaned again

#### Scenario: Break from a nested conditional

- **WHEN** a conditional inside a loop executes `break`
- **THEN** its arm-local and iteration-local owners are cleaned before control reaches the statement after the loop

#### Scenario: Reject a transfer outside a loop

- **WHEN** a function body contains `break` or `continue` without an enclosing loop
- **THEN** analysis reports the invalid transfer while preserving unrelated function facts

#### Scenario: Transfer from an expression-nested arm

- **WHEN** a loop evaluates an initializer or call argument whose selected ordinary match arm executes `break` or `continue`
- **THEN** the transfer targets that loop, stops the containing expression and remaining statements on its path, and performs ordinary lexical cleanup

#### Scenario: Reject a transfer hidden inside a nested callable

- **WHEN** an explicit callable or Effect inside an arm contains `break` or `continue` but contains no loop of its own
- **THEN** analysis reports `SEM0038` at the invalid transfer span even if a loop surrounds the outer match

#### Scenario: Reject an ordinary block transfer without a loop

- **WHEN** an ordinary match arm executes `break` or `continue` outside any enclosing loop in the current boundary
- **THEN** analysis reports `SEM0038` at the transfer span while preserving independent arm facts

#### Scenario: Break the loop declared inside the arm

- **WHEN** an ordinary block contains its own loop and a break followed by another block statement
- **THEN** the break targets the inner loop and execution continues at that following statement rather than leaving the arm
