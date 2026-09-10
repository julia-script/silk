## ADDED Requirements

### Requirement: Stored Effects preserve complete lifetime-bearing outcomes

Storage in nominal fields and generic wrappers SHALL retain explicit Effect environment bounds, complete success and failure types, service requirements, exact representation and execution access. Ordinary owned containers SHALL retain the lifetime-bearing types of elements returned through Effects. No container name SHALL grant lifetime validity or detachment.

#### Scenario: Construct an existing Box of references

- **WHEN** existing ordinary-source Box.make returns a Box containing a valid external reference
- **THEN** the Box preserves that element lifetime and ordinary success/failure cleanup ownership without a new Box API

#### Scenario: Reject source-owner escape through allocation

- **WHEN** a Box returned by an Effect would outlive the owner referenced by its element
- **THEN** the compiler rejects the escape despite heap allocation
