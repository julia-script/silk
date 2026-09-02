## ADDED Requirements

### Requirement: Type alias declarations have one canonical layout

The formatter SHALL render a complete type alias declaration as `[pub] type Name = <type>`, laying
out the target with the existing canonical type and union policies, preserving comments through the
existing attachment policy, and separating adjacent top-level declarations with the canonical module
spacing.

#### Scenario: Format a union alias idempotently

- **WHEN** a complete alias declaration whose target is a multi-member union contains irregular spaces and attached comments
- **THEN** two formatting passes produce identical canonical source without changing tokens or comment content
