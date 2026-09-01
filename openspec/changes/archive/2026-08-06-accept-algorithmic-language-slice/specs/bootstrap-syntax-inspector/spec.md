## ADDED Requirements

### Requirement: Inspect the algorithmic acceptance program in the unified workbench

The existing `/labs` workbench SHALL provide the canonical multi-module remaining-member fold as one
browser-local preset. Its coordinated panes MUST expose source closure, semantic facts, HIR,
ownership, instances, target-aware layout, MIR, logical evaluation, and both backend realizations
through the analysis facade without adding a standalone acceptance inspector.

#### Scenario: Follow the coverage fold across every representation

- **WHEN** a user selects the algorithmic acceptance preset and follows one guarded union decision
- **THEN** the unified workbench coordinates its source, canonical compiler representations, result `42`, and target-private native and WebAssembly realization

### Requirement: The acceptance preset remains identical to the tested program

The browser-local preset SHALL retain the same root identity and module bytes exercised by the
three-engine acceptance suite so tooling cannot demonstrate a simpler substitute.

#### Scenario: Verify the canonical fixture boundary

- **WHEN** preset and differential fixtures are checked
- **THEN** their root identity, module identities, and exact source bytes agree
