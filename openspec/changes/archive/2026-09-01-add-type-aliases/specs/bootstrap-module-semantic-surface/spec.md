## ADDED Requirements

### Requirement: Module surfaces encode public type aliases by erased target

A module semantic surface SHALL encode every type alias header as its name, declaration kind,
visibility, and erased canonical target type, under the same header rules as every other
declaration. It SHALL NOT encode the alias's source spelling of the target, intermediate aliases,
or source spans. A change to the erased target SHALL change the surface and invalidate direct
dependents; a re-spelling of the target that erases to the same canonical type SHALL leave the
surface equal.

#### Scenario: Round-trip a public union alias

- **WHEN** a module exports `pub type FetchError = HttpError | JsonError`
- **THEN** encode and decode preserve the name, visibility, and the normalized union target

#### Scenario: Invalidate a dependent after a target change

- **WHEN** the exported alias target gains the member `Timeout`
- **THEN** the module surface changes and every direct dependent is selected for dependency-surface recomputation

#### Scenario: Ignore an equivalent re-spelling

- **WHEN** the exported alias target is rewritten from `HttpError | JsonError` to `JsonError | HttpError`
- **THEN** the module surface remains equal and no dependent is invalidated
