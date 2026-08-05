## Why

Silk functions can name and call one another, but they cannot move a value across that relationship. The next smallest language step is to preserve typed parameters and simple call arguments in syntax without prematurely building general expressions or a full type checker.

## What Changes

- Add `:` and `,` to the bootstrap lexer vocabulary.
- Parse zero or more `<name>: <type>` function parameters and zero or more simple call arguments.
- Recognize decimal integers and bare identifiers as bootstrap argument expressions while retaining the existing returned integer and call forms.
- Keep parameter, argument, separator, trivia, missing-token, and unexpected-token syntax lossless and locally recoverable.
- Preserve exact parameter counts in declaration facts while leaving parameter-reference resolution and argument checking explicitly unavailable for later changes.
- Add inspector presets that make valid and malformed parameter/argument syntax visible.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-lexer`: Recognize the punctuation needed by typed parameter and argument lists.
- `bootstrap-syntax`: Generalize parameter lists and calls from empty lists to the first small value-carrying grammar.
- `bootstrap-semantic-facts`: Preserve declaration parameter counts and explicitly defer meaning for the new reference and argument syntax.
- `bootstrap-syntax-inspector`: Show parameter and argument concrete branches, spans, recovery, and deferred semantic state.

## Impact

This changes compiler token kinds, parser node kinds and grammar, semantic fact shapes for declarations and unsupported expressions, compiler tests and fixtures, public package documentation, release-candidate validation, and the hidden docs inspector. It intentionally does not resolve parameter names, check arguments, evaluate calls, or introduce AST/HIR/MIR/LLVM lowering.
