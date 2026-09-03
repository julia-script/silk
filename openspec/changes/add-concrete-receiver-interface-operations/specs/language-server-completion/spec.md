## ADDED Requirements

### Requirement: Concrete receiver completion offers uniquely supplied interface operations

Completion after a runtime-concrete nominal receiver SHALL offer the receiver operations supplied by
the interface applications the resolver would admit at that position, under the same visibility,
proof, precedence, and ambiguity filter call resolution uses. An operation whose name an inherent
member already claims SHALL NOT be offered a second time, and an operation supplied by more than one
participating application SHALL NOT be offered as a callable member, because the call would be
ambiguous.

#### Scenario: Complete a conformance operation on a concrete value

- **WHEN** completion is requested after `document.` and `Document` conforms only to `Printable`
- **THEN** `print` is offered as a method alongside the inherent members

#### Scenario: An invisible interface is not offered

- **WHEN** the conforming interface is not visible to the requesting module
- **THEN** its operations are absent from the completion list
