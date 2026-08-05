## Context

See `proposal.md` for motivation. This change assumes declaration collection has been synced. The
lexer already distinguishes identifiers and both parentheses, so the new behavior begins in parser
expression selection. Existing semantic function facts contain only integer return expressions.

## Goals / Non-Goals

**Goals:**

- Add exactly one new expression form: a named zero-argument call in return position.
- Preserve call syntax and semantic provenance through malformed input.
- Represent the not-yet-run resolution phase honestly.
- Make the call subtree and unresolved state visually inspectable.

**Non-Goals:**

- Name resolution, arguments, parameters, nested/postfix calls, general expressions, AST, or HIR.
- Emitting an unknown-name diagnostic before a resolver exists.

## Decisions

### Return expressions branch on the next significant token

After `return`, `DecimalInteger` selects the existing integer parser and `Identifier` or a recovery
parenthesis selects the call parser. Other tokens recover toward the nearest valid expression start
or closing brace. This bounded choice avoids a general Pratt/parser-expression framework for two
atomic forms.

### `CallExpression` directly owns the callee and empty parentheses

The concrete call node contains the callee identifier, left parenthesis, trivia, and right
parenthesis. No `ArgumentList` node is introduced because arguments are explicitly unsupported.
Concrete tokens found between the parentheses become an error child so the tree stays lossless.

The alternative of modeling calls as a generic postfix chain anticipates member access, chaining,
and arguments that this milestone has not earned.

### Returned-expression facts become a closed integer-or-call union

Each semantic function fact will replace its integer-only expression field with a returned-expression
union. The integer variant preserves the existing fact. The call variant owns callee state and
syntax provenance. This keeps expression ownership local without introducing an AST or generic fact
framework.

### A present call is `Unresolved`, not erroneous

This change preserves a present callee but does not query declarations. Its reference state is
`Unresolved`, its compatibility is `Unavailable`, and it produces no semantic diagnostic. Missing
callee syntax is an unavailable state owned by parser diagnostics. The dependent resolver change
will replace this staging state with resolved/missing/ambiguous outcomes.

### The inspector shows the intentional phase boundary

Presets will cover a valid call, missing right parenthesis, and unsupported argument. The semantic
card will display the callee spelling, unresolved state, and unavailable compatibility beside the
concrete call subtree and phase-owned diagnostics.

## Risks / Trade-offs

- **A public unresolved state is intentionally short-lived** → Mark the dependency clearly and remove
  it atomically in `resolve-first-function-call`; do not preserve compatibility aliases.
- **Recovery may consume the block's closing brace** → Synchronize missing call elements on
  `RightBrace` and test damaged calls followed by later functions.
- **Users may mistake unresolved for an error** → Use neutral inspector language and keep semantic
  diagnostics empty until resolution runs.

## Migration Plan

Land only after declaration collection is synced. Update expression unions and all exhaustive
consumers together. Rollback removes `CallExpression` and restores integer-only returned-expression
facts; no persisted format is involved.
