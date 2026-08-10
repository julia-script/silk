## ADDED Requirements

### Requirement: Effect recipes compose uniformly

Every semantically valid nesting of Effect construction, transformation, recovery, retry, and
service provision SHALL retain the same contract and execution behavior in data-first calls,
left-associated pipelines, explicitly grouped expressions, and stored intermediate values. `run`
SHALL execute exactly the composed outer Effect regardless of source shape. Construction-time
callable and provider evaluation, run-time operation order, failure and requirement rows, capture
access, and cleanup MUST remain equivalent across those forms.

#### Scenario: Map a provided Effect directly from an effectful entry

- **WHEN** an effectful `main` runs `source |> Capability.provide(provider) |> Effect.map(mapper)`
- **THEN** the provider satisfies the source requirement, the mapper receives the success once, and the entry completes with the mapped result

#### Scenario: Reverse the transformation and provision order

- **WHEN** a requirement-preserving transformation is applied before the required provider is supplied
- **THEN** provision satisfies the transformed Effect's requirement and execution agrees with the equivalent provision-first form

#### Scenario: Store a composed pipeline before running it

- **WHEN** a valid multi-combinator Effect pipeline is bound and run later
- **THEN** it behaves like the direct expression while preserving construction-time captures and without introducing a trap

#### Scenario: Preserve affine success through a mapped provided Effect

- **WHEN** a provided Effect succeeds with an affine value that a mapper consumes
- **THEN** the mapper receives ownership exactly once and every remaining owned component is cleaned exactly once

