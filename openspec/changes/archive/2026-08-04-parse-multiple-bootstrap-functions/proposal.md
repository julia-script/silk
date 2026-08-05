## Why

The bootstrap parser proves one function in isolation, but the milestone needs a source file with
multiple declarations before declaration collection or references can be tested. Repeating the
existing function grammar is the smallest syntax-only step and keeps call syntax out of scope.

## What Changes

- Parse one or more existing public parameterless function declarations before end-of-file.
- Preserve exact tokens, trivia, declaration order, and bounded recovery across function boundaries.
- Keep semantic analysis explicitly limited to the first function during this syntax-only change.
- Add a two-function inspector preset and show both top-level concrete branches as visual feedback.
- Defer declaration collection, duplicate-name rules, calls, resolution, AST, HIR, and lowering.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Extend the source-file grammar and recovery contract from one function to one
  or more repetitions of the existing function declaration.
- `bootstrap-syntax-inspector`: Make multiple top-level function branches directly inspectable while
  labeling the still-first-function-only semantic boundary honestly.

## Impact

This changes `Parser.parse` and its fixtures/tests, the concrete-tree expectations, compiler README
grammar documentation, and the hidden inspector. It is the prerequisite for
`collect-bootstrap-declarations` and intentionally does not change the semantic-analysis API.
