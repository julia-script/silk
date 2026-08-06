## ADDED Requirements

### Requirement: Frontend failures gate artifact production

The driver SHALL run recoverable frontend phases far enough to collect deterministic source
diagnostics and partial resolver facts, then gate every artifact-producing phase on that frontend
result. Any error diagnostic SHALL produce a closed source-rejected outcome carrying the merged
diagnostics, loaded source catalog, and executed-phase report. Any captured source-resolution
failure SHALL fail compilation as a typed operational failure carrying the canonically ordered
failures and available frontend report. Neither case SHALL perform MIR lowering, backend emission,
object emission, shim compilation, linking, or destination commit.

#### Scenario: Reject source errors before lowering

- **WHEN** closure loading and semantic analysis complete with one or more error diagnostics
- **THEN** the driver returns a source-rejected outcome and reports no MIR, backend, object, shim, or link phase

#### Scenario: Fail operationally after partial resolution

- **WHEN** closure loading captures a typed source-resolution failure
- **THEN** the driver fails with the ordered resolution failures and invokes no artifact-producing phase

#### Scenario: Preserve tooling-style recovery before the gate

- **WHEN** one import fails while another module remains analyzable
- **THEN** the driver's frontend result retains the successful module's facts and available diagnostics before compilation stops

#### Scenario: Compile only a clean frontend

- **WHEN** source resolution succeeds and recoverable frontend phases produce no error diagnostics
- **THEN** the driver proceeds through MIR lowering, backend emission, and the requested toolchain stages
