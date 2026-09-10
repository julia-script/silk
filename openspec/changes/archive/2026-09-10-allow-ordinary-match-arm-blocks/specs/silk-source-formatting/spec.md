## MODIFIED Requirements

### Requirement: Match expressions format as canonical arm blocks

Complete match syntax SHALL format with one space around its access mode and fat arrows, one arm per
line, two-space indentation inside the match body, canonical nested pattern spacing, and stable guard
placement before `=>`. Compact nominal patterns MAY remain on one line when they fit the existing
width policy; broken field-pattern lists SHALL use one item per line and a trailing comma. Formatting
MUST preserve arm order, bindings, guards, `..`, comments, expression grouping, and explicit expression-or-block arm body kind. Ordinary arm blocks SHALL use canonical ordinary statement layout with their opening brace after `=>`, statements indented two additional spaces, and their closing brace aligned with the arm pattern. Empty uncommented blocks SHALL format as `{}`; blocks containing only comments SHALL preserve those comments under ordinary block layout. Formatting SHALL NOT insert returns, convert blocks to Effects, or treat a final expression statement as a block value.

#### Scenario: Format a multi-arm match

- **WHEN** complete source contains an irregularly spaced consuming match with two arms
- **THEN** formatting emits canonical mode spacing and one ordered indented arm per line

#### Scenario: Format twice

- **WHEN** canonical nested guarded match syntax is parsed and formatted again
- **THEN** the second output is byte-identical to the first

#### Scenario: Format mixed sequential and expression arms

- **WHEN** a complete match contains an empty block, a multi-statement guarded block, a nested match block, a transferring block, and an expression arm
- **THEN** canonical output preserves every body kind and statement order, places ordinary statements at their nested indentation, and reparses to equivalent syntax

#### Scenario: Preserve comments around arm delimiters

- **WHEN** complete ordinary arm syntax has comments before and after arrows, beside braces, and between statements or arms
- **THEN** canonical output retains comment content and stable attachment and a second parse-and-format pass is byte-identical

#### Scenario: Format a non-unit trailing expression statement

- **WHEN** complete arm syntax contains `{ 42 }` with a semantic `SEM0087` error
- **THEN** formatting preserves the ordinary expression statement and succeeds without semantic repair or an inserted return

#### Scenario: Reject a recovered arm block

- **WHEN** parser recovery retained a malformed or missing arm brace while preserving a later arm or declaration
- **THEN** formatting returns its existing typed source-formatting failure and emits no replacement bytes, synthesized delimiters, or partial formatted artifact
