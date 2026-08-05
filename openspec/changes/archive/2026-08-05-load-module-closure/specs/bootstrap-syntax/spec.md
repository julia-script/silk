## ADDED Requirements

### Requirement: Minimal import declarations parse losslessly

The parser SHALL accept `import <module>` as an unconditional top-level declaration wherever a
function declaration may begin, where `<module>` is one identifier naming a logical module
identity. The spelling is deliberately provisional and owned by the syntax-prototype issue. The
import declaration SHALL retain its keyword, name, and adjacent trivia with exact source-owned
spans as its own concrete branch. A missing import name SHALL become an explicit missing token
with a parser diagnostic, and recovery SHALL keep following top-level declarations parseable.

#### Scenario: Parse an import before a function

- **WHEN** the source spells `import math` followed by a complete function declaration
- **THEN** the tree contains one import-declaration branch retaining the keyword and name, followed by the complete function branch

#### Scenario: Recover a missing import name

- **WHEN** the source spells `import` immediately followed by a function declaration
- **THEN** the import branch contains a missing identifier with one parser diagnostic and the following function remains a separate complete branch

#### Scenario: Parse multiple imports losslessly

- **WHEN** a source begins with two import declarations separated by trivia
- **THEN** both imports are separate concrete branches in source order and every token and trivia slice is retained exactly once
