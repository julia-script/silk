## Purpose

Define profile-specific module declarations and dependency discovery through ordinary bounded static Silk selection.

## ADDED Requirements

### Requirement: Module conditions select declaration groups

Module scope SHALL accept `static if condition { declarations }` with optional `else { declarations }` or `else static if`. Nested groups SHALL publish selected declarations into their containing module namespace using ordinary declaration visibility. Every arm of a loaded source SHALL be parsed. Inactive arms MUST NOT undergo ordinary name, type, Effect, ownership or backend analysis. A failed condition SHALL admit neither arm.

#### Scenario: Mutually exclusive surfaces

- **WHEN** mutually exclusive arms declare the same public function under two distinct profiles
- **THEN** each profile exposes exactly its selected declaration without a duplicate-name error

#### Scenario: Loaded inactive syntax

- **WHEN** an inactive arm of a loaded module has a syntax error and unresolved runtime names
- **THEN** its syntax error is reported and its ordinary semantic references are not checked

### Requirement: Conditions demand ordinary Silk dependencies

A condition SHALL evaluate to a static bool through the ordinary bounded static semantics. Unconditional declarations and imports SHALL be forward-referenceable. Conditional dependencies SHALL demand their enclosing conditions before becoming available. Required helper signatures, types, constants and static bodies SHALL be checked; unrelated declarations SHALL NOT be checked to evaluate the condition. Missing, inactive-only, cyclic, non-static and non-boolean dependencies SHALL produce structured diagnostics with source spans and dependency origins. Conditions SHALL NOT access declarations whose availability recursively depends on those conditions.

#### Scenario: Imported helper

- **WHEN** a condition calls an imported ordinary static helper using admitted constants, types and profile configuration
- **THEN** its required dependencies are resolved and type checked and its bounded bool result selects the arm

#### Scenario: Availability cycle

- **WHEN** a condition depends on a declaration inside its own arm, directly or through helpers or another condition
- **THEN** selection diagnoses the availability cycle with the condition and dependency origins and admits neither arm

#### Scenario: Independent forward selection

- **WHEN** a condition uses a constant published by a later independent selected group
- **THEN** that group's condition is demanded first and the selected constant is available

### Requirement: Configuration completes before ordinary selection

Unconditional package-schema discovery, explicit binding normalization, defaults and validation SHALL complete before module selection. The published profile SHALL remain immutable. Conditional package parameters SHALL be rejected. A default depending on conditional availability SHALL produce a configuration dependency diagnostic with origins.

#### Scenario: Configuration controls imports

- **WHEN** a validated package parameter controls a module group
- **THEN** selection reads its completed profile value and never changes that profile to satisfy a dependency

### Requirement: Inactive imports do not participate in resolution

An import confined to an inactive group SHALL cause no resolver call, file load, import diagnostic, dependency edge or cycle edge. Condition-required imports SHALL remain actual dependencies. Selected imports SHALL retain ordinary missing-module, failure, cycle and provenance behavior.

#### Scenario: Missing alternative

- **WHEN** profile A selects an existing import and profile B selects a nonexistent alternative
- **THEN** A never resolves the alternative and B reports its ordinary missing-import diagnostic

### Requirement: Publication is explicit and selective

`pub import module { name, original as local }` SHALL publish only the named public members under the selected aliases, retaining original identity and origin provenance. Namespace-wide public imports and wildcard publication SHALL be rejected. Selected collisions SHALL be diagnosed; inactive names SHALL remain unavailable to consumers. Selected imports, aliases, types, ordinary and external declarations SHALL participate in the same concrete module surface.

#### Scenario: Re-export an alternate declaration

- **WHEN** mutually exclusive groups selectively publish a public external declaration as the same local name
- **THEN** consumers resolve only the selected original identity and inactive foreign declarations enter no inventory

### Requirement: Selection is isolated and invalidated by its dependencies

Generic analysis SHALL expose the active normalized profile, inactive ranges and one concrete selected public surface. Multiple profiles SHALL coexist with distinct semantic results; equivalent normalized profiles SHALL produce equal selection and diagnostics. Parsed syntax reuse MUST NOT imply selected semantic reuse. Both condition-required and selected program dependencies SHALL participate in invalidation, including helper body changes that leave its signature unchanged. Unloaded source outside both closures SHALL NOT invalidate the result.

#### Scenario: Reuse parsing across profiles

- **WHEN** two profiles share parsed syntax and choose different arms
- **THEN** each snapshot retains its own surface, inactive ranges, diagnostics and backend inventory

#### Scenario: Revise a helper body

- **WHEN** a demanded helper changes its bool result without changing its signature
- **THEN** dependent selection and semantics are recomputed while an edit to an unloaded alternative creates no invalidation
