## ADDED Requirements

### Requirement: Typed outcomes have one compiler-owned target shape

For every reachable flow contract, target planning SHALL publish a deterministic private outcome
shape containing a discriminant and payload storage sufficient for the success value or any failure
member. Canonical nominal identity SHALL determine failure tags. Evaluator and backends MUST consume
that shape without independently choosing tags, lanes, or padding.

#### Scenario: Plan mixed success and failure payloads

- **WHEN** a flow returns `Usize` and may fail with differently shaped nominal errors
- **THEN** the selected target plan fixes one tag and payload-lane mapping before MIR lowering
