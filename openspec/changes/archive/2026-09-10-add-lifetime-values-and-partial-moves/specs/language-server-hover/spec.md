## ADDED Requirements

### Requirement: Hover shows inferred lifetime relationships

Hover SHALL render compiler-owned explicit lifetime expansion for available elided declarations and lifetime-bearing types with stable readable binder names. It SHALL distinguish a receiver-storage borrow, independent stored-data lifetimes, nested references, string-view validity, and callable or Effect environment bounds. Hover SHALL preserve authored documentation and existing token-specific selection, use the accepted semantic snapshot, and require no backend work. Missing lifetime facts SHALL remain unavailable without fabricated static or one-source guarantees.

#### Scenario: Show stored data separately from receiver storage

- **WHEN** hover selects a getter that explicitly returns a holder's stored data lifetime
- **THEN** the presentation distinguishes that data lifetime from the receiver borrow and remains identical at declaration and reference occurrences

#### Scenario: Show two inferred field binders

- **WHEN** hover selects a holder whose two borrowed fields omitted lifetime names
- **THEN** the expanded presentation shows two stable independent binders rather than one common region
