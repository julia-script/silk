## Why

The first implementation phase should produce an early, inspectable artifact while teaching how source characters become compiler data. A focused lexer lesson establishes tokens and source spans without mixing in parsing or LLVM concerns.

## What Changes

- Add `Token` and `Lexer` actor modules to the tutorial example.
- Recognize Tiny keywords, identifiers, signed-language integer lexemes, punctuation, and operators.
- Track start and end source offsets and emit a typed lexical diagnostic for unsupported input.
- Add exact token-stream tests, including EOF.
- Include debugging guidance for keyword classification, cursor advancement, and span errors.

## Capabilities

### New Capabilities

None. This change adds documentation and tutorial-example material, so the change opts out of behavioral specs with `skip_specs: true`.

### Modified Capabilities

None.

## Impact

Adds tutorial example source and tests for lexing. It uses Effect error channels and repository TypeScript conventions but does not modify `@silklang/llvm`.

