## Purpose

Describe the authority and verification provenance of admitted platform catalogs without making those records a source-selection mechanism.

## ADDED Requirements

### Requirement: Catalog records distinguish authority from verification

Each record SHALL identify its production method, authority/header version, logical target and deployment scope, admitted declarations and constant/layout/signature/symbol evidence, fixture/tool versions and update/drift-review provenance. Required missing or contradictory provenance SHALL be rejected. Planned evidence MUST NOT imply executed conformance. These descriptive records MUST NOT select providers or substitute for library ABI import/export manifests.

#### Scenario: Reviewed representative subset

- **WHEN** a representative catalog provides consistent versioned authority, scope, declaration evidence and review provenance
- **THEN** its record validates without resolving physical supplies or asserting an unexecuted fixture passed

#### Scenario: Invalid provenance

- **WHEN** a record omits a required authority/tool version, contradicts its target/deployment scope, or claims passing evidence without a result identity
- **THEN** validation rejects it with the offending record field
