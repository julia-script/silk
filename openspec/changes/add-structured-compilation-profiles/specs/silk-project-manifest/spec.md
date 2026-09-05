## ADDED Requirements

### Requirement: Project profiles carry typed package bindings

The manifest SHALL support a named default through build.profile, named logical profile inputs under profiles, project-tier build.bindings and profile-tier bindings. Each binding SHALL identify package, module and parameter and carry the tagged serializable value/provenance transport defined in the compilation-profile reference. Named profiles SHALL select an explicit canonical target. Complete request overrides and project-profile selection SHALL be mutually exclusive. Unknown profile names and invalid logical inputs SHALL produce structured project diagnostics before compilation. Existing physical output and native link paths SHALL remain outside logical profile identity.

#### Scenario: Select a named profile

- **WHEN** build.profile names a declared profile
- **THEN** compiler and tooling requests use that profile's logical facts and bindings

#### Scenario: Reject an unknown profile

- **WHEN** a request names a profile absent from the manifest
- **THEN** selection reports the unknown name and manifest origin without falling back to host defaults
