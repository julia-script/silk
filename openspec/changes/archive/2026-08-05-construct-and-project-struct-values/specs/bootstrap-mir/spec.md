## ADDED Requirements

### Requirement: MIR carries canonical nominal logical types

MIR locals, parameters, call results, and function results SHALL accept canonical nominal struct
types alongside built-in scalar types. Nominal identity SHALL remain the defining module and
declaration name; MIR MUST NOT replace it with structural field shapes, physical offsets, or backend
types. Every nominal MIR type SHALL have the same reachable catalog entry and calling shape in the
program's completed layout plan.

#### Scenario: Lower a nominal factory contract

- **WHEN** a reachable factory returns `Token`
- **THEN** its MIR result and every receiving local carry the canonical `Token` logical type and reference one selected plan entry

### Requirement: Aggregate construction and projection are explicit MIR operations

MIR SHALL represent construction as one destination nominal local plus declaration-ordered source
locals identified by canonical fields. MIR SHALL represent projection as one typed destination,
one nominal source local, and one canonical field identity. Whole-value moves, calls, returns, and
drops SHALL continue to use ordinary MIR operations over the widened logical type vocabulary.

#### Scenario: Lower a reordered literal canonically

- **WHEN** HIR constructs a struct whose source initializers were reordered
- **THEN** MIR construction operands follow canonical declaration order with their field identities and source provenance

#### Scenario: Lower a chained projection

- **WHEN** HIR reads `token.span.start`
- **THEN** MIR contains two ordered projection operations whose intermediate and final locals have the declared nominal and scalar types

#### Scenario: Lower whole-value cleanup

- **WHEN** ownership plans a live aggregate release
- **THEN** MIR emits one whole-value drop carrying generated provenance and no per-backend cleanup decision

### Requirement: MIR verifies aggregate consistency

MIR verification SHALL reject aggregate operations whose nominal type, field identity, operand type,
declaration order, layout entry, calling shape, or local type disagree. It SHALL also reject a
nominal call or return that does not match the compiler-selected lane shape. Violations SHALL remain
ordered deterministic data.

#### Scenario: Reject a mismatched construction field

- **WHEN** a construction operand names a field from another nominal type
- **THEN** verification reports the canonical field/type mismatch before evaluation or emission

#### Scenario: Reject a missing aggregate ABI shape

- **WHEN** a nominal parameter or result lacks its selected calling shape
- **THEN** verification reports the missing plan fact and no backend receives the module

### Requirement: Aggregate MIR encoding is deterministic

The textual MIR encoding SHALL include canonical nominal types, field identities, construction and
projection operands, whole-value moves and drops, calling shapes, and provenance in stable order.
Equivalent aggregate programs SHALL encode byte-identically across fresh processes.

#### Scenario: Repeat aggregate MIR lowering

- **WHEN** one nested construction-and-projection program is lowered repeatedly
- **THEN** its logical types, operations, lane shapes, field order, and encoding are byte-identical
