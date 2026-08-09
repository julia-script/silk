## ADDED Requirements

### Requirement: Constant declarations have one canonical layout

The formatter SHALL render a complete constant declaration as `[pub] const name: type = literal`,
preserving comments through the existing attachment policy and separating adjacent top-level
declarations with the canonical module spacing.

#### Scenario: Format a constant idempotently

- **WHEN** a complete constant declaration contains irregular spaces and attached comments
- **THEN** two formatting passes produce identical canonical source without changing tokens or comment content
