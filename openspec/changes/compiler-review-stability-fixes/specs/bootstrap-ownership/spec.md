## ADDED Requirements

### Requirement: Loan live-ranges account for uses nested in place and effect expressions

Loan-end analysis SHALL treat identifier and callable occurrences nested inside place-replace, effect-result, and requirement-binding expressions as uses at that occurrence: they SHALL extend the enclosing loan's live range and SHALL invalidate any earlier record that treated the callable's last invocation as its final use.

#### Scenario: View used inside a place replace keeps its loan live
- **WHEN** a shared view's last use sits inside a place-replace expression's value operand and the borrowed owner is mutated between the view's direct uses and that nested use
- **THEN** ownership analysis reports owner access during the loan — the view loan's live range extends through the place-replace use rather than ending at the last direct use
