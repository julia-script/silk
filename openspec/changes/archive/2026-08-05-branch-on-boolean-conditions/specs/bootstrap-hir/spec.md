## ADDED Requirements

### Requirement: Conditionals and booleans are typed HIR structure

The semantic type vocabulary SHALL grow to `I32` and `Bool`. HIR SHALL represent `true`/`false`
as boolean literal expressions typed `Bool`, and a conditional statement as a dedicated
statement carrying its typed condition expression, the taken arm's statement sequence, and the
otherwise arm's (possibly empty) statement sequence, each with exact provenance. Arm bindings
SHALL carry function-unique binding identities. An unavailable condition or a damaged arm SHALL
follow the existing explicit-unavailable rules. The encoder SHALL cover conditionals and boolean
literals, gated by committed golden files.

#### Scenario: Elaborate a conditional body

- **WHEN** `pub fn main() -> I32 { if I32.equals(1, 1) { return 1 } return 0 }` is elaborated
- **THEN** the body is one conditional statement whose condition is a typed `Bool` builtin call and whose taken arm returns a literal, followed by the trailing return

#### Scenario: Elaborate an else arm

- **WHEN** a conditional carries both arms with early returns
- **THEN** the HIR conditional carries both statement sequences in order

#### Scenario: Give arm bindings unique identities

- **WHEN** an arm declares `let inner = 1` after two body bindings
- **THEN** the arm binding's identity does not collide with any other binding in the function
