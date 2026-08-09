## ADDED Requirements

### Requirement: Native address-root materialization is path-correct

LLVM emission SHALL keep private address storage for every address-taken mutable root valid on all
runtime control-flow paths where a post-call reload can occur. A borrow materialized on one branch
MUST NOT cause another branch to reload uninitialized or stale storage. Defining and mutating a
root SHALL preserve its complete compiler-planned lanes, active union discriminants, and cleanup
obligation without a type- or collection-specific backend branch.

#### Scenario: Skip an exclusive-borrow branch

- **WHEN** an affine mutable root is borrowed exclusively on one branch but execution takes another branch and later crosses a call
- **THEN** native execution reloads the root's original complete value rather than uninitialized address storage

#### Scenario: Take the exclusive-borrow branch

- **WHEN** execution takes the branch that passes the root by exclusive reference and the callee mutates it
- **THEN** native execution reloads the complete callee-updated value and retains exactly one cleanup obligation

#### Scenario: Compare path-sensitive affine roots across engines

- **WHEN** taken and untaken borrow cases run through evaluation, native LLVM, and direct WebAssembly
- **THEN** all three engines produce the same scalar observations and successful exactly-once cleanup outcome
