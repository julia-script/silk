## Why

The tutorial needs evidence that learners can transfer the architecture to a new feature rather than only reproduce guided code. Adding remainder is small enough to finish independently while touching every relevant compiler stage.

## What Changes

- Add a reduced-guidance exercise for the `%` operator.
- Specify lexer, multiplication-level precedence, AST, signed `srem` lowering, and test success criteria without giving the finished implementation.
- Add `isOdd` and mixed-precedence validation inputs.
- Ask learners to explain each modified stage and confirm unrelated modules remain unchanged.
- Consolidate next steps, including the optional factorial demonstration and compile-only playground.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds the final tutorial lesson, hidden validation fixture, and exercise criteria. The reference solution affects only tutorial example code.

