## ADDED Requirements

### Requirement: Ownership analyzes only generated runtime projections and values

Static type descriptors, field descriptors, static sequences, parsed template values, static loop
bindings, and inactive iterations SHALL create no runtime binding, move, borrow, loan, liveness,
cleanup, or destructor fact. After residualization, every generated field access SHALL obey the
ordinary ownership mode of its concrete operation. Template formatting SHALL use a shared borrow of
the argument pack and shared field projections, so formatting MUST NOT consume or mutate the pack or
its fields.

#### Scenario: Borrow an anonymous record temporary for formatting

- **WHEN** `&.{ name: "Julia", age: 32 }` is passed directly to template formatting
- **THEN** ownership creates one hidden temporary owner, keeps it live through all generated field displays, and cleans it after the complete formatting call

#### Scenario: Keep static plans outside cleanup

- **WHEN** template parsing creates and replaces immutable static sequences
- **THEN** no sequence allocation, replacement, or value appears in the runtime cleanup plan

