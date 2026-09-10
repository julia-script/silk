## MODIFIED Requirements

### Requirement: Blank lines are bounded but meaningful

Consecutive top-level import declarations SHALL form one block even when comments or other trivia
intervene. Within that block, authored blank lines SHALL collapse and imports SHALL render on
consecutive lines, with intervening comments retaining source order and existing attachment.
Formatting SHALL NOT sort, hoist, merge, deduplicate, or move imports across a non-import declaration.
Existing width-aware import-member layout SHALL continue to apply. Every other top-level declaration
boundary, including either direction between an import block and a non-import, SHALL contain exactly
one blank line. Trailing comments SHALL remain on their owning declaration; boundary separation
SHALL follow its trailing comment and precede standalone comments belonging to the next declaration.
Module documentation before the first import SHALL retain its existing attachment and spacing.
All-import files SHALL retain exactly one final newline. Within a
block, a run of one or more author-supplied blank lines between statements or standalone comment
groups SHALL become exactly one blank line; formatting SHALL NOT introduce an interior blank line
where the source had none. Runs of blank lines in every other grammatical context SHALL collapse to
the single canonical separation required by that context.

#### Scenario: Separate top-level declarations

- **WHEN** two top-level declarations, at least one of which is not an import, are adjacent or separated by several blank lines
- **THEN** the formatted source contains exactly one blank line between them

#### Scenario: Preserve one block grouping line

- **WHEN** statements inside a block are separated by one or more blank lines
- **THEN** the formatted block retains exactly one blank line at that boundary

#### Scenario: Keep adjacent statements adjacent

- **WHEN** two statements inside a block have no blank line between them
- **THEN** formatting places them on consecutive lines without introducing a blank line

#### Scenario: Compact an import block

- **WHEN** three imports occur without an intervening non-import declaration, with authored blank lines or comments between them
- **THEN** formatting removes interior blank lines, preserves import and comment order and trailing comment attachment, and keeps each short import on its own line

#### Scenario: Keep both import block boundaries

- **WHEN** a non-import precedes an import block and another non-import follows it
- **THEN** formatting retains exactly one blank line at each block boundary without moving imports

#### Scenario: Format an all-import file with module documentation

- **WHEN** a file contains module documentation followed only by imports
- **THEN** formatting preserves the module documentation before the compact import block and emits exactly one final newline
