## ADDED Requirements

### Requirement: Instance discovery follows reflection-generated residual calls

Concrete specialization SHALL complete static reflection, template validation, and heterogeneous
iteration before publishing direct runtime call candidates. Each generated field operation SHALL
select evidence and contribute call edges using that iteration's concrete field type. Equal
reflection and template applications SHALL reuse one residual specialization; unequal template,
aggregate type, visibility authority, generic argument, evidence, or static value inputs MUST NOT be
conflated.

#### Scenario: Discover heterogeneous Display instances

- **WHEN** one template selects a `string` field and an `i32` field
- **THEN** the executable closure contains the independently selected `Display<string>` and `Display<i32>` runtime operations and no descriptor instance

#### Scenario: Keep templates distinct

- **WHEN** the same formatting function is reached with two unequal static template values over the same argument type
- **THEN** discovery retains two canonical residual specialization keys even if later optimization makes their emitted bytes equal

