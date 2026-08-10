## REMOVED Requirements

### Requirement: Allocator exposes service-facing provision

**Reason**: `Allocator.provide` gives one service capability a compiler-registered API that
user-defined services cannot reproduce and duplicates the general exclusive borrowed provision
operation.

**Migration**: Replace `Allocator.provide(&mut allocator)` with
`Effect.provideMut(&mut allocator)` in pipe sections, or call
`Effect.provideMut(effect, &mut allocator)` in data-first form.

#### Scenario: Migrate a custom allocator pipeline

- **WHEN** an allocation-requiring Effect is composed with a conforming custom allocator
- **THEN** `Effect.provideMut` satisfies the exclusive `Allocator` requirement through ordinary capability dispatch without allocator-specific compiler behavior
