## Why

Call arguments currently stop at integer literals and bare identifiers, so the first evaluated
language slice cannot compose one call inside another. Parsing nested calls is the smallest next
step toward a recursive expression grammar and tests whether the concrete parser can grow without
introducing an AST or skipping recovery behavior.

## What Changes

- Let call arguments contain call expressions in addition to integers and identifiers.
- Preserve every nested call, argument list, separator, token, and owner-qualified byte span in the
  lossless concrete tree.
- Recover malformed inner calls locally without consuming the outer argument list or following
  function declaration.
- Publish nested argument expressions as explicitly unavailable semantic placeholders until the
  following analysis change teaches semantic resolution to recurse into them.
- Add nested-call presets and concrete-tree feedback to the hidden Syntax Inspector.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Extend the existing call-argument grammar and recovery contract recursively.
- `bootstrap-semantic-facts`: Preserve newly parsed nested arguments without prematurely resolving
  or misclassifying them.
- `bootstrap-syntax-inspector`: Make valid and malformed nested call syntax directly visible.

## Impact

This changes the bootstrap parser, concrete grammar documentation, parser fixtures and tests, and
semantic placeholder facts, and the hidden inspector presets. It does not add nested semantic
resolution or evaluation yet; those remain explicit follow-up changes.
