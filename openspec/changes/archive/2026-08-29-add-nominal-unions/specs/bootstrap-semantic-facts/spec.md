## ADDED Requirements

### Requirement: Semantic facts expose nominal union declarations and variant construction

Semantic analysis SHALL publish canonical facts for each union parent, parent parameter, source-
ordered variant, variant kind, field, visibility, availability, explicit constructor argument prefix,
field inference origin, completed application, selected variant, and exact source provenance. A
failed field, qualifier, visibility check, or inference step SHALL retain every independent fact and
make only the dependent construction and parent executability outcomes unavailable.

#### Scenario: Inspect an inferred variant constructor

- **WHEN** `Option.Some { value: 42 }` completes its omitted parent argument from the payload field
- **THEN** facts expose the canonical Option declaration, Some variant, value field, `T = i32` inference origin, complete `Option<i32>` application, and precise result

#### Scenario: Preserve a damaged union declaration

- **WHEN** one variant field is unresolved beside valid siblings
- **THEN** facts retain the canonical parent and every independent sibling while marking construction, coverage, layout, and execution of the complete parent unavailable

### Requirement: Parent-union projections remain explicit failures

A projection fact whose subject is a nominal union SHALL retain the subject type, requested spelling,
candidate variant fields, exact provenance, and unavailable outcome. It MUST NOT synthesize a common
field identity from same-spelled fields in multiple variants or expose an inactive payload place.

#### Scenario: Inspect a rejected parent projection

- **WHEN** `result.value` is analyzed for `Result<A, E>`
- **THEN** facts retain the Result subject and requested field while the projection remains unavailable until a variant pattern binds the payload

## MODIFIED Requirements

### Requirement: Match facts retain source arms and canonical coverage

Semantic analysis SHALL publish the scrutinee type and access mode, source-ordered arms, resolved
structural roots, applied nominal parents, canonical variants, complete selection paths, source and
canonical field mappings, pattern bindings, guard outcomes, remaining path set before and after each
arm, reachability, result type, and complete-or-unavailable match outcome. Whole-member selection
SHALL retain the covered descendant paths, while direct variant selection SHALL retain its exact
root-parent-variant leaf without representing that leaf as a structural member. Failed lookups,
damaged patterns, incompatible guards, and unavailable results SHALL retain all independent facts
with exact provenance and causal diagnostics.

#### Scenario: Inspect coverage arm by arm

- **WHEN** `Token` and `End` unguarded arms cover `Token | End`
- **THEN** facts show the canonical set before each arm and the empty remaining set after the second

#### Scenario: Retain an unknown member pattern

- **WHEN** one arm names an unresolved nominal type beside an independently valid arm
- **THEN** both arm facts remain queryable and only the dependent match outcome is unavailable

#### Scenario: Inspect hierarchical coverage arm by arm

- **WHEN** direct variant arms cover every leaf of `HttpError` inside `HttpError | OutOfMemoryError`
- **THEN** facts retain each complete selection path, each subtraction step, and the unchanged normalized structural root identities
