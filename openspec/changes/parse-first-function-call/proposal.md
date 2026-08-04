## Why

Collected declarations still cannot appear as expressions, so there is nothing for name resolution
to connect. A zero-argument function call in return position is the smallest new expression shape
that exposes a real reference while keeping resolution itself in the following change.

## What Changes

- Extend the return-expression grammar from a decimal integer to either that integer or
  `Identifier()` with zero arguments.
- Add a lossless `CallExpression` concrete node with bounded recovery for its callee and parentheses.
- **BREAKING**: Replace integer-only body facts with a closed returned-expression fact that can
  preserve an unresolved call without pretending resolution has occurred.
- Keep call compatibility unavailable and emit no unknown-name diagnostic until the resolver exists.
- Add valid and damaged call presets to the inspector and visualize the call subtree and unresolved
  semantic state.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Recognize and recover the first zero-argument call expression in return
  position alongside the existing integer expression.
- `bootstrap-semantic-facts`: Preserve present and syntax-unavailable call facts while explicitly
  withholding resolution and compatibility.
- `bootstrap-syntax-inspector`: Display call-expression syntax and its intentionally unresolved
  semantic state as visual feedback.

## Impact

This depends on the synced `collect-bootstrap-declarations` change. It affects syntax node kinds,
parser and semantic result unions, fixtures/tests, README and release documentation, and the hidden
inspector. Arguments, parameters, general postfix expressions, and resolution remain out of scope.
