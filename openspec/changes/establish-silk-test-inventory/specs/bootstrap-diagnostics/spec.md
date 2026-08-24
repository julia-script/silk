## ADDED Requirements

### Requirement: Invalid test declarations have stable declaration-local diagnostics

Semantic analysis SHALL publish a stable test-eligibility diagnostic code and primary span at the
smallest declaration header element establishing each independent violation. It SHALL distinguish
public visibility, ordinary function kind, generic parameters, value parameters, non-unit success,
and nonempty residual requirements. Multiple independent marked declarations SHALL each report;
dependent body damage SHALL follow existing causal suppression and ordering rules. An invalid
marked declaration MUST NOT enter a partial inventory.

#### Scenario: Diagnose every invalid eligibility dimension

- **WHEN** separate marked declarations are public, ordinary, generic, parameterized, non-unit, and residually requiring
- **THEN** each receives the stable test-eligibility code at its offending header element and none enters the inventory

#### Scenario: Preserve deterministic diagnostic order

- **WHEN** several invalid marked declarations occur across modules
- **THEN** diagnostics retain the unified canonical cross-phase order independently of root input order

#### Scenario: Preserve a causal unavailable header

- **WHEN** a marked declaration's contract is unavailable because of an earlier parser or unknown-type diagnostic
- **THEN** analysis retains that cause, publishes no speculative test-eligibility diagnostic, and adds no inventory candidate

#### Scenario: Avoid a Reporter eligibility diagnostic

- **WHEN** a test has an empty residual requirement row and a runner later provides Reporter around reporting
- **THEN** the test is not diagnosed for lacking or naming Reporter because reporting is outside invocation eligibility
