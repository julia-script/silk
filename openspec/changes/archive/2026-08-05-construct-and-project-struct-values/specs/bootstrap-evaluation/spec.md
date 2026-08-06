## ADDED Requirements

### Requirement: Evaluation carries immutable nominal values

The MIR evaluator SHALL represent a nominal struct value by its canonical type and complete
declaration-ordered field values. Construction SHALL evaluate field operands exactly once in MIR
operand order and create no partial value. Whole-value moves, parameter binding, calls, and returns
SHALL preserve canonical nominal identity and value contents.

#### Scenario: Evaluate a factory result

- **WHEN** a factory constructs and returns `Pair { left: 1, right: 2 }`
- **THEN** evaluation produces one canonical `Pair` value containing declaration-ordered field values `1` and `2`

#### Scenario: Pass a nested aggregate through a call

- **WHEN** a complete nested struct is moved through an internal function and returned
- **THEN** evaluation preserves every nested nominal identity and field value without aliasing a partial source

### Requirement: Evaluation projects exact field values

An aggregate projection SHALL read the canonical field identified by MIR and return its stored value
with the declared result type. Chained projections SHALL evaluate left-to-right. A structurally
invalid projection SHALL be rejected by MIR verification rather than guessed by the evaluator.

#### Scenario: Evaluate a chained scalar projection

- **WHEN** `main` returns `token.span.start`
- **THEN** evaluation follows both canonical fields and returns the exact stored scalar

### Requirement: Aggregate traces are deterministic and bounded

Evaluation traces SHALL identify aggregate construction, whole-value movement across calls and
returns, field projection, and cleanup using canonical types, field identities, source provenance,
and compact deterministic value summaries. Trace ordering MUST NOT depend on object identity,
backend representation, physical address, or hash iteration.

#### Scenario: Repeat an aggregate trace

- **WHEN** the same construction-call-projection program is evaluated repeatedly
- **THEN** its event order, canonical identities, field summaries, values, and encoded trace are identical

### Requirement: Evaluation consumes aggregate calling shapes

Before executing a nominal call or return, evaluation SHALL verify that the logical aggregate value
matches the target plan's compiler-selected calling shape. It MUST NOT invent a flattening or
continue with a missing lane. The completed program result remains the fixed scalar bootstrap entry
result.

#### Scenario: Evaluate a flattened internal result

- **WHEN** an internal function returns a struct whose calling shape has multiple scalar lanes
- **THEN** evaluation transfers all lanes according to the selected field paths and the caller observes the original logical nominal value
