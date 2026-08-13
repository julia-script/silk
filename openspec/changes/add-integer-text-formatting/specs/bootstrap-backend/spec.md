## ADDED Requirements

### Requirement: A union member narrower than its payload slot survives the slot

A structural union's payload slot is as wide as the widest member that occupies it, so a member
whose own value is narrower is carried in a wider container. Every backend SHALL carry such a member
into its slot and back out again unchanged, and SHALL agree with the evaluator on the result. A
backend MUST NOT reject a program for this shape alone, and MUST NOT emit a sequence that loses or
reinterprets the member's bits.

Where a backend cannot release a member held in a wider slot, it SHALL refuse to emit rather than
release nothing, so a missing release is a reported failure and never a silent leak.

#### Scenario: Read back a narrow member from a wide slot

- **WHEN** a program constructs a union member narrower than the union's payload slot and then matches it out
- **THEN** every engine yields the member's original value

#### Scenario: Read back the wide member that set the slot's width

- **WHEN** a program constructs the union's widest member and then matches it out
- **THEN** every engine yields the full value, with no bits lost to the narrower member's width
