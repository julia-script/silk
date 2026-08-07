## ADDED Requirements

### Requirement: Evaluation is the flow and failure oracle

Evaluation SHALL distinguish construction from execution, represent success and owned nominal
failure explicitly, run one layer, recover exact members, propagate unmatched members, and record
deterministic ordered flow/failure/cleanup events. Traps SHALL remain separate blocked outcomes.

#### Scenario: Compare lazy success and recovery

- **WHEN** one fixture first succeeds and then recovers its declared failure
- **THEN** traces show no body event before run and both executions produce the specified result with exact event order
