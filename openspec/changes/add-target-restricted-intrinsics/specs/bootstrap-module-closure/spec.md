## ADDED Requirements

### Requirement: Source closure does not imply executable intrinsic reachability

Module closure SHALL continue to load and semantically analyze canonical source needed for ordinary
name and type resolution, including declarations that mention restricted intrinsics. It SHALL
publish enough call identity for later executable closure to determine which intrinsic operations
survive from the selected entry. Source-module presence alone MUST NOT be interpreted as executable
use or target incompatibility.

#### Scenario: Load a portable and native provider together

- **WHEN** module closure loads declarations for both portable source and a native-only provider
- **THEN** both declarations remain navigable while only calls reachable from the chosen entry participate in availability validation

#### Scenario: Retain a reachable intrinsic identity

- **WHEN** an ordinary reachable wrapper calls one sealed restricted intrinsic
- **THEN** executable closure preserves that canonical intrinsic identity for target validation rather than treating the wrapper's module as the availability unit

