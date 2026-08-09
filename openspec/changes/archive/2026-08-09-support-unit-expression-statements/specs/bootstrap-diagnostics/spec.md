## ADDED Requirements

### Requirement: Unexpected syntax diagnostics identify token and context

An unexpected-syntax diagnostic SHALL identify the encountered source token or bounded construct
and the grammatical context in which it was rejected. Its structured reason SHALL retain the
unexpected token kinds and the expected source-language token spellings or grammatical roles.
Generic wording such as `Unexpected token sequence` without encountered or expected context MUST
NOT be the sole user-facing explanation when the parser can determine that context.

#### Scenario: Describe an unexpected block token

- **WHEN** punctuation that cannot begin any statement appears directly inside a block
- **THEN** the diagnostic names that punctuation, says it was encountered while parsing a statement, and describes the valid statement starts or closing brace

#### Scenario: Describe a bounded malformed construct

- **WHEN** recovery groups multiple concrete tokens into one error region while parsing a known construct
- **THEN** one diagnostic identifies the bounded construct or its first decisive token and retains every unexpected token kind in structured reason data

### Requirement: Statement recovery remains inside its owning block

When malformed syntax begins where a statement is expected, recovery SHALL retain one error or
unavailable statement branch in the current block, synchronize at the next valid statement or the
current block's closing brace, and continue parsing that block. Tokens following the malformed
statement MUST NOT be reinterpreted as a top-level declaration solely because of that recovery.
Missing tokens synthesized only by the primary recovery SHALL not produce independent diagnostics.

#### Scenario: Keep return after a damaged run expression

- **WHEN** a malformed standalone run expression is followed by a valid `return ()` in the same block
- **THEN** one primary diagnostic describes the malformed run expression, the return remains a sibling statement in that block, and no phantom function or missing-brace cascade is reported

#### Scenario: Recover unexpected punctuation before a statement

- **WHEN** unexpected punctuation appears before a valid binding or return statement
- **THEN** the punctuation is retained in one block-owned recovery branch and the following statement parses normally without a dependent declaration diagnostic

#### Scenario: End recovery at the owning right brace

- **WHEN** a malformed final statement reaches its block's concrete closing brace
- **THEN** recovery retains that brace as the block delimiter and does not consume it into the malformed statement
