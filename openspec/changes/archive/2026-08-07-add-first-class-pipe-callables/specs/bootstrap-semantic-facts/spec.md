## MODIFIED Requirements

### Requirement: Operator and pipeline facts expose their canonical resolution

Every present operator expression SHALL publish its concrete operator, ordered operand facts,
resolved compiler-known actor operation when available, closed contract, result type, and exact
source provenance. Every pipeline SHALL publish its left input, callable-producing right
expression, resolved callable type and invocation mode, application contract, result type, and
provenance. Section facts SHALL separately identify the canonical function, omitted leading
parameter, supplied trailing arguments, captures, and resulting unary callable. Type mismatches
SHALL reuse `SEM0012` at the offending operand span. Damaged or unavailable dependencies SHALL keep
only the dependent fact unavailable with its originating diagnostic cause; analysis MUST NOT
synthesize an alternate operator, callable, type, or argument.

#### Scenario: Inspect a resolved infix operation

- **WHEN** a body returns `40 + 2`
- **THEN** its fact identifies `I32.add`, two ordered `I32` operands, an available `I32` result, and the complete infix span

#### Scenario: Inspect equality selected by operand type

- **WHEN** one function returns `1 == 1` and another returns `true == false`
- **THEN** their facts resolve to `I32.equals` and `Bool.equals` respectively with `Bool` results

#### Scenario: Keep a mistyped prefix unavailable

- **WHEN** a body returns `!1`
- **THEN** the operand carries `SEM0012` and the prefix fact has no available result type

#### Scenario: Inspect a section application pipeline

- **WHEN** a body returns `2 |> I32.add(3)`
- **THEN** facts distinguish construction of `I32.add(3)` from its unary application to `2`

#### Scenario: Preserve an unavailable callable

- **WHEN** a pipeline right expression resolves to an inaccessible function or a non-callable value
- **THEN** its callable and result facts remain unavailable with the originating cause and no fabricated target

## ADDED Requirements

### Requirement: Callable facts expose mode and capture obligations

Every available function reference and section SHALL publish its complete callable type, shared,
exclusive, or consuming invocation mode, canonical target, ordered capture facts, capture ownership
modes, retained dependencies, and exact source provenance. Calls through callable values SHALL
publish the required and provided invocation access independently of result typing.

#### Scenario: Explain a one-shot section

- **WHEN** a section captures one moved affine owner
- **THEN** its fact identifies the owner capture and `once fn` mode before any invocation occurs

### Requirement: Run facts retain the complete operand

A run fact SHALL identify the complete Effect-producing operand selected by the low-precedence
grammar, its residual failure and requirement rows, one-layer success type, and exact provenance.

#### Scenario: Inspect run around retry

- **WHEN** source spells `run attempt |> Effect.retry(2)` without grouping
- **THEN** the run fact's subject is the retried Effect and not the untransformed `attempt`
