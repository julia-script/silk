## ADDED Requirements

### Requirement: Editor analysis executes frontend phases only

The language server SHALL produce each ordinary synchronized-document result from an immutable
frontend analysis snapshot. Diagnostics, hover, completion, navigation, symbols, inlay hints, and
formatting SHALL NOT require instance discovery, target selection, layout planning, MIR lowering,
evaluation, or code generation. This computation choice SHALL NOT weaken version matching, atomic
project revision commit, recovery, or compiler ownership of semantic facts.

#### Scenario: Serve editor features from frontend analysis

- **WHEN** an accepted document revision completes analysis and receives hover, definition, completion, symbol, inlay-hint, formatting, and diagnostic requests
- **THEN** every request uses the committed frontend snapshot for that exact document version and no runtime realization phase executes

#### Scenario: Preserve coherent replacement

- **WHEN** a newer frontend snapshot atomically replaces the committed project revision
- **THEN** protocol requests observe its document text and semantic facts together under the existing latest-wins scheduling rules
