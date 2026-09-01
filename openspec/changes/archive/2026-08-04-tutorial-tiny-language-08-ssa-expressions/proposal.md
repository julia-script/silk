## Why

Recursive expression lowering introduces LLVM's typed SSA value model. The lesson must explicitly connect AST nodes to one-assignment instruction results so learners do not mistake SSA names for mutable variables.

## What Changes

- Extend expression lowering for `+`, `-`, `*`, `/`, unary negation, `<`, and `>`.
- Introduce SSA with a mutable-source versus versioned-value comparison and an AST-to-SSA diagram.
- Map signed Tiny operations to `add`, `sub`, `mul`, `sdiv`, `icmp`, `zext`, and `FunctionBody.negate`.
- Normalize comparison results to language-level `i32` zero or one.
- Add IR and native checkpoints for precedence and comparison behavior.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Extends the tutorial compiler example and tests. It exercises existing arithmetic, comparison, and cast APIs only.
