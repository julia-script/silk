## MODIFIED Requirements

### Requirement: Project analysis remains frontend-only

Project frontend analysis, root-view derivation and every analysis-intent preparation SHALL NOT execute instance discovery, target selection, layout planning, MIR lowering, runtime execution, or code generation. An analysis-intent bundle SHALL answer frontend queries after its resolver and parser are no longer available. Executable work on analysis-only input SHALL start a new explicit preparation request that leaves the prior bundle unchanged.

#### Scenario: Observe a completed project revision

- **WHEN** a multi-root project frontend analysis completes
- **THEN** its observations contain one frontend phase sequence for the union closure and no runtime realization phase

#### Scenario: Prepare for analysis only

- **WHEN** a single-root request is prepared with analysis intent
- **THEN** the bundle's phase report contains no instance-discovery, layout or MIR phase and later frontend queries need neither resolver nor parser

#### Scenario: Promote analysis to execution

- **WHEN** a sealed analysis frontend is promoted to executable intent
- **THEN** a new executable bundle is published and the analysis bundle's manifest is unchanged
