## Why

Correct calculation order is one of the tutorial's central learning goals. Learners need to see precedence represented in the AST before LLVM lowering so they understand that parsing, not LLVM, decides expression structure.

## What Changes

- Add immutable expression AST data for literals, names, unary operations, binary operations, calls, and conditionals.
- Teach recursive descent with precedence climbing for `+`, `-`, `*`, and `/`.
- Cover parentheses, unary negation, and left associativity.
- Add AST diagrams and snapshot tests for representative precedence cases.
- Provide a cursor/minimum-precedence trace for recovery from common parser mistakes.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds tutorial AST and arithmetic-parser code plus diagrams and tests. No package API changes are required.
