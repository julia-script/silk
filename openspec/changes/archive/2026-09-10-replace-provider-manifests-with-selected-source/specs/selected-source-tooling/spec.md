## Purpose

Make compiler, editor and documentation availability agree with ordinary source selection for one explicit compilation profile.

## ADDED Requirements

### Requirement: Source declarations determine provider availability

All seven current native providers SHALL select their declarations and imports in ordinary Silk using immutable profile facts. Bundled metadata SHALL describe source bytes and logical inventory without portable/provider categories or target availability rules. A selected façade SHALL be allowed to import its selected implementation. Generic intrinsic machine restrictions SHALL remain enforced.

#### Scenario: Native and Wasm surfaces

- **WHEN** the existing providers are selected on supported native and LLVM-to-Wasm profiles
- **THEN** native profiles expose their existing declarations and Wasm profiles expose none of their native declarations or imports

#### Scenario: Inactive source is harmless

- **WHEN** an unavailable provider import is confined to an inactive branch
- **THEN** it contributes no selected declaration or native inventory and causes no provider-specific failure

### Requirement: Catalogs and documentation identify their selected profile

Source catalogs, auto-import candidates and generated documentation SHALL consume the canonical selected declaration surface, including selected publication, and identify its normalized profile. They MUST NOT union incompatible declarations. Equivalent source/profile inputs SHALL yield deterministic output; modified output SHALL fail regeneration checks. Factual source tampering SHALL continue to fail integrity validation.

#### Scenario: Selected public declaration

- **WHEN** two profiles select different declarations with the same module identity
- **THEN** each catalog and documentation surface agrees with that profile's compiler surface

#### Scenario: Profile-specific configuration

- **WHEN** two same-target profiles differ in a package parameter controlling availability
- **THEN** their candidates remain distinct while independently constructed equivalent profiles reuse compatible results

### Requirement: Editor profile transitions are atomic

Initialization and explicit configuration SHALL use the canonical named-profile, project-default, host-edge, full-override and triple-shorthand rules. A profile change SHALL invalidate incompatible worker/session results and refresh diagnostics, hover, navigation, completion, auto-import and inactive-range presentation. Results delayed from an earlier profile MUST NOT publish into the new profile.

#### Scenario: Delayed diagnostic and candidate result

- **WHEN** the active profile changes while an old analysis or query is running
- **THEN** old results are superseded and only matching new-profile results are presented

#### Scenario: Inactive source presentation

- **WHEN** a loaded source file has an inactive module branch
- **THEN** the editor presents compiler-provided inactive ranges and provides no active semantic candidate for declarations confined to that branch
