## Why

Nested calls can now be parsed and analyzed recursively, but a reachable nested argument still
stops at an intentional evaluator boundary. Evaluating that expression inside out completes the
smallest genuinely composable Silk program while keeping the result and its explanation exact.

## What Changes

- Evaluate nested argument expressions recursively in concrete left-to-right order before binding
  their resulting `I32` values to the enclosing call's parameters.
- Replace the temporary unsupported-nested-expression outcome with completed nested evaluation or
  an exact existing blocked reason from the reachable inner expression.
- Extend deterministic traces so inner calls, bindings, reads, returns, and failures remain
  distinguishable by their semantic identities and source provenance.
- Apply the existing active-call cycle protection across calls reached through nested arguments.
- Add successful, blocked, and cyclic nested-evaluation presets and trace feedback to the hidden
  Syntax Inspector.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-evaluation`: Evaluate recursive semantic expressions and retain exact nested results,
  failure provenance, call cycles, and trace order.
- `bootstrap-syntax-inspector`: Display nested outcomes and traces beside their semantic and syntax
  relationships.

## Impact

This changes the bootstrap evaluator, blocked-reason and trace handling, evaluator fixtures and
tests, and the hidden inspector's evaluation presets and trace rendering. It consumes the recursive
semantic facts from `analyze-nested-expressions` and does not add syntax or a new intermediate
representation.
