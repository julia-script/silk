## ADDED Requirements

### Requirement: Match expressions format as canonical arm blocks

Complete match syntax SHALL format with one space around its access mode and fat arrows, one arm per
line, two-space indentation inside the match body, canonical nested pattern spacing, and stable guard
placement before `=>`. Compact nominal patterns MAY remain on one line when they fit the existing
width policy; broken field-pattern lists SHALL use one item per line and a trailing comma. Formatting
MUST preserve arm order, bindings, guards, `..`, comments, and expression grouping.

#### Scenario: Format a multi-arm match

- **WHEN** complete source contains an irregularly spaced consuming match with two arms
- **THEN** formatting emits canonical mode spacing and one ordered indented arm per line

#### Scenario: Format twice

- **WHEN** canonical nested guarded match syntax is parsed and formatted again
- **THEN** the second output is byte-identical to the first
