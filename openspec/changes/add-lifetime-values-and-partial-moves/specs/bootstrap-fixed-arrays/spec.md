## MODIFIED Requirements

### Requirement: Array ownership and cleanup derive from elements

An array SHALL be Copy exactly when its element type has the compiler's sealed Copy property;
otherwise it SHALL be an affine value. Moving or returning a definitely initialized element at a statically known in-bounds index SHALL follow ordinary partial-move, loan, restoration, and Drop-boundary rules, while moving the complete initialized array SHALL transfer one ownership obligation. Dynamic-index extraction SHALL remain rejected. Cleanup SHALL visit live elements exactly once in ascending index order and recursively
use each element's canonical cleanup.

#### Scenario: Copy an explicitly Copy nominal array

- **WHEN** `Token` validly declares `impl Copy` and source reads an `Array<Token, 3>`
- **THEN** the complete array is copied and the source remains live

#### Scenario: Keep a field-only nominal array affine

- **WHEN** `Token` has only Copy fields but declares no `impl Copy`
- **THEN** `Array<Token, 3>` remains affine and whole-value transfer requires `move`

#### Scenario: Copy a scalar array

- **WHEN** a bound `Array<i32, 3>` is passed to an array parameter
- **THEN** the value is copied and the source remains live

#### Scenario: Reject moving one struct element

- **WHEN** code requests `move values[index]` at a dynamic index from `Array<Token, 3>`
- **THEN** ownership rejects the partial move and retains the whole array's explicit ownership state

#### Scenario: Clean up nested elements

- **WHEN** a live `Array<Token, 3>` exits its scope
- **THEN** cleanup visits initialized indices zero, one, and two exactly once using `Token`'s declaration-defined cleanup

#### Scenario: Move one known array element

- **WHEN** source moves values[1] from a complete eligible fixed array
- **THEN** index 1 becomes uninitialized, disjoint initialized indices remain available, and exit cleanup skips the moved element
