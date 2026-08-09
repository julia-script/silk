## Why

Parser recovery currently turns one incomplete construct into many equal-weight diagnostics, so an
empty editor buffer reports a synthetic function's ten missing tokens and an identifier before a
closing brace is misclassified as an assignment. Semantic analysis then adds dependent errors for
the parser's invented structure, obscuring the source mistake that a developer can actually fix.
The same cascade remains after a real declaration prefix such as `pub`: once the parser reports the
missing `fn`, it continues reporting every token implied by the incomplete declaration at the same
end-of-file position.

## What Changes

- **BREAKING**: Treat an empty source file as a valid empty module instead of synthesizing a missing
  function declaration and ten parser diagnostics.
- Classify an identifier-led statement as an assignment only when its full place syntax is followed
  by `=`; a final identifier expression without `return` is instead recovered as the return
  expression with one missing-return-keyword diagnostic.
- Report missing required return statements as one construct-level diagnostic while retaining
  detailed missing-token leaves in the lossless CST.
- After the first syntax diagnostic in one recovery episode, retain subsequent missing or damaged
  CST elements without reporting dependent diagnostics until a concrete synchronization token is
  consumed; then resume ordinary diagnostic reporting for later independent mistakes.
- Render missing-token messages with source spellings such as `` `return` `` and `` `=` `` rather
  than internal token-kind names such as `ReturnKeyword` and `Equals`.
- Suppress invalid-assignment-place diagnostics when the destination is already unavailable because
  of an originating name-resolution or parser diagnostic.
- **BREAKING**: Generalize `SEM0006` terminology from an unknown parameter to an unknown value name,
  matching its existing lookup across parameters, local bindings, and pattern bindings.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Empty modules, assignment lookahead, recovered final expressions, and
  construct-level missing-return recovery change their observable CST and diagnostic behavior.
- `bootstrap-diagnostics`: Parser and semantic recovery suppress dependent cascades while retaining
  their originating provenance.
- `bootstrap-semantic-facts`: Missing local value references use value-name terminology and do not
  generate an additional invalid-place diagnostic when recovery misclassifies or damages a write.

## Impact

The change affects parser entry and block recovery, diagnostic constructors and reason data,
elaboration of assignment destinations, parser and elaboration tests, compiler documentation, and
the Syntax Inspector's unified diagnostic output. No dependency or backend behavior changes.
