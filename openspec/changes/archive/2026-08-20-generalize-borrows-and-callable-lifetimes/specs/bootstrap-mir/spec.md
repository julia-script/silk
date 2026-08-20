## MODIFIED Requirements

### Requirement: MIR represents slice loans in the structured control DAG

Slice formation SHALL retain compiler-owned temporary roots and complete field or fixed-array
element selectors. Runtime element selectors SHALL be bounds checked before address formation, and
temporary cleanup SHALL occur after the matching loan end.

#### Scenario: Lower a runtime indexed subplace

- **WHEN** HIR borrows `&mut matrix[index]`
- **THEN** MIR begins the loan from `matrix` with its checked element selector and never materializes a copied inner array

#### Scenario: Clean a temporary after its loan

- **WHEN** an addressable temporary contains values with cleanup obligations
- **THEN** MIR orders the matching loan end before the temporary owner's ordinary drop plan

### Requirement: MIR represents callable environments in the structured DAG

Callable environments SHALL keep capture evaluation order and original parameter ordinals as
separate facts, so backends can store captures in construction order and invoke targets in parameter order.

#### Scenario: Lower staged positional captures

- **WHEN** a three-parameter callable captures parameter two and then parameter one
- **THEN** MIR constructs captures in that order and applies them in original parameter order after parameter zero
