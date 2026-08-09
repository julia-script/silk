## Context

See `proposal.md` for motivation. The parser is a lossless recursive-descent parser over immutable
state. Its generic `expect` helper currently couples every inserted `MissingToken` leaf to one
`PAR0001`, `parse` always enters declaration recovery once, and `parseBlock` classifies every
identifier-led statement as an assignment before checking for `=`. Elaboration already represents
missing references with a diagnostic identity, but assignment validation treats their absent root
as a second invalid-place error. Although empty input now bypasses declaration parsing, an
incomplete concrete prefix such as `pub` still enters a function production whose later `expect`
calls all diagnose at end-of-file because parser state does not record that recovery is already in
progress.

The CST must remain lossless and useful during editing, explicit `return` remains part of the
language, and diagnostics from every phase remain deterministic data.

## Goals / Non-Goals

**Goals:**

- Separate detailed recovery structure from the number of user-facing diagnostics.
- Suppress dependent syntax diagnostics within one recovery episode while resuming diagnostics at
  the next concrete grammar anchor.
- Make assignment recognition follow the complete place grammar without duplicating that grammar.
- Preserve useful semantic analysis of a final expression recovered after a missing `return`.
- Prevent assignment validation from diagnosing consequences of unavailable syntax or resolution.

**Non-Goals:**

- Adding Rust-style implicit tail returns to valid Silk syntax.
- Globally ranking, hiding, or filtering diagnostics in the Syntax Inspector UI.
- Redesigning every existing parser recovery site in this change.

## Decisions

### Empty input bypasses declaration recovery

`parse` will initialize an empty declaration collection and enter its declaration loop only when
the next significant token is not end-of-file. The source-file root still owns the lexer-provided
EOF token, so losslessness and stable source ownership do not require a synthetic declaration.

Alternative: retain the missing function and filter its diagnostics in the UI. Rejected because
all compiler consumers would still observe fictional syntax and the compiler facade would remain
noisy outside the inspector.

### Assignment lookahead reuses the place parser speculatively

Before the block commits an identifier-led construct to `parseAssignmentStatement`, it will parse
the same identifier/projection chain against immutable state and inspect whether the following
significant token is `=`. The speculative result and any speculative diagnostics are discarded;
the committed parse still runs once through the normal assignment function.

This avoids a second hand-written scanner for nested index expressions and keeps assignment
lookahead aligned with place parsing. The small duplicate parse is limited to identifier-led
statement boundaries and is preferable to grammar drift.

When lookahead does not find `=`, a required-return function block falls through to existing
missing-return-keyword recovery. Thus a final `foo` becomes the concrete expression of a recovered
return statement, yielding one parser error for the absent keyword and allowing name resolution to
produce the independent unknown-value diagnostic.

### A wholly absent return has one dedicated diagnostic

When a required-return block reaches a closing boundary with neither a return keyword nor a
recoverable expression, a dedicated recovery constructor will create the missing return keyword
and missing expression leaves without invoking `expect` twice. It will emit one new parser
diagnostic for the missing return statement. Ordinary isolated missing tokens continue to use
`PAR0001`.

Alternative: deduplicate diagnostics after parsing by span. Rejected because distinct mistakes can
legitimately share an insertion span, while the parser knows which leaves belong to one recovery
decision.

### Parser state tracks one recovery episode

Parser state will record whether a syntax diagnostic has started recovery. The first missing-token,
unexpected-token, reserved-syntax, or construct-level parser diagnostic enters recovery and is
retained. Additional diagnostics created while no concrete grammar token can be consumed are
suppressed, while the CST continues to retain every missing token and error node required for
losslessness.

Successfully consuming a concrete token through `expect` ends the episode. That token is a grammar
anchor proving the parser has reached a position it understands, so a later mistake can begin a new
episode and receive its own diagnostic. Consuming trivia or skipping unexpected tokens does not end
recovery. End-of-file may end the internal episode but does not cause suppressed diagnostics to be
re-emitted.

For the input `pub`, `Expected \`fn\`` starts recovery. The remaining function structure is retained
as missing CST elements, but its dependent name, parameter, return-type, block, and return
diagnostics are suppressed because no concrete anchor appears before end-of-file. A following valid
declaration starter can instead synchronize the damaged declaration and remain available to the
top-level parse loop.

Alternative: keep every parser diagnostic and deduplicate those sharing a span. Rejected because
independent errors can share a span and cascades can span different offsets. Alternative: cap each
declaration at one diagnostic. Rejected because it would hide a later independent body error after
the parser had already resumed successfully.

### Token descriptions belong to the Token actor

The Token module will expose one exhaustive source-language description for every token kind.
`Diagnostic.missingToken` will use that description in its message while preserving the original
kind in structured reason data. This keeps spelling policy out of parser control flow and prevents
diagnostic wording from drifting across call sites.

### Invalid-place diagnostics require an otherwise available destination

Assignment and `Place.replace` validation will emit `SEM0036` only when the destination syntax is
available and destination analysis did not already originate a diagnostic. Unknown references keep
their `SEM0006` cause; parser-damaged destinations keep parser ownership. Resolved literals, calls,
or other non-place expressions still receive `SEM0036`.

### `SEM0006` becomes unknown-value terminology

The stable code remains `SEM0006`, but its exported constant, constructor, reason tag, message,
tests, documentation, and inspector preset will use `UnknownValueReference` / “Unknown value.” No
compatibility alias is retained because the project is pre-release and explicitly rejects
compatibility debt.

## Risks / Trade-offs

- [Speculative place parsing performs duplicate work] → Limit it to identifier-led block positions
  and reuse immutable parser state so it cannot mutate the committed result.
- [Recovered final identifiers could look like valid tail returns to tooling] → Retain the missing
  `ReturnKeyword` leaf and parser diagnostic; the grammar still considers the function damaged.
- [Suppressing `SEM0036` could hide a real place error] → Suppress only when syntax is unavailable
  or destination analysis already produced a causal diagnostic; resolved non-place destinations
  remain diagnosed.
- [Changing reason tags breaks exhaustive consumers] → Make the break explicit in the proposal and
  update all in-repository consumers in the same change.
- [Recovery could suppress a later independent error] → Leave recovery only when `expect` consumes
  a concrete grammar token; focused tests cover both EOF cascades and reporting after a valid
  synchronization anchor.

## Migration Plan

Update parser and diagnostic behavior together, then update compiler tests, documentation, and lab
presets before running the full repository checks. The change is source-compatible for valid Silk
programs; only malformed-source CST/diagnostic output and the public `SEM0006` reason identifier
change. Rollback is a normal source revert because there is no persisted data migration.
