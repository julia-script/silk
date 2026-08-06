## ADDED Requirements

### Requirement: Operator and pipeline facts expose their canonical resolution

Every present operator expression SHALL publish its concrete operator, ordered operand facts,
resolved compiler-known actor operation when available, closed contract, result type, and exact
source provenance. Every pipeline SHALL publish its left input, qualified target lookup, explicit
later arguments, effective inserted argument mapping, canonical operation or declaration target,
contract, result type, and provenance. Type mismatches SHALL reuse `SEM0012` at the offending
operand span. Syntax-damaged, mistyped, missing, inaccessible, or conflicting dependencies SHALL
keep only the dependent fact explicitly unavailable with the originating diagnostic cause; analysis
MUST NOT synthesize an alternate operator, call target, type, or argument.

#### Scenario: Inspect a resolved infix operation

- **WHEN** a body returns `40 + 2`
- **THEN** its fact identifies `I32.add`, two ordered `I32` operands, an available `I32` result, and the complete infix span

#### Scenario: Inspect equality selected by operand type

- **WHEN** one function returns `1 == 1` and another returns `true == false`
- **THEN** their facts resolve to `I32.equals` and `Bool.equals` respectively with `Bool` results

#### Scenario: Keep a mistyped prefix unavailable

- **WHEN** a body returns `!1`
- **THEN** the operand carries `SEM0012` and the prefix fact has no available result type

#### Scenario: Inspect an inserted pipeline argument

- **WHEN** a body returns `2 |> I32.add(3)`
- **THEN** the pipeline fact maps `2` to parameter zero and `3` to parameter one of the resolved operation

#### Scenario: Preserve an unavailable imported pipeline target

- **WHEN** a pipeline names an inaccessible or conflicting imported operation
- **THEN** its target and result remain unavailable with the existing name-resolution cause and no secondary unknown-call diagnostic
