## ADDED Requirements

### Requirement: Lifetime syntax and expansion have stable source formatting

Formatting SHALL canonically print <'a, T>, <'a: 'b, T: 'a>, &'a T, &'a mut T, &'a [T], string<'a>, for<'a> fn<'env>(...) -> ..., Effect<'env; A ! E ? R>, and effect<'env> fn declaration environments using existing width-aware layouts. It SHALL retain comments and explicit or omitted lifetime choices without silently elaborating normal format requests. Compiler-owned lifetime expansion SHALL produce valid source with stable readable nonconflicting binder names and preserve semantics on reparsing. Place refinement SHALL print match place value; explicit cleanup SHALL retain drop value statement syntax.

#### Scenario: Format lifetime syntax idempotently

- **WHEN** a valid declaration mixes irregularly spaced lifetime binders, bounds, references, a quantified callable, and an Effect environment bound
- **THEN** formatting twice produces identical source preserving comments and each semantic relationship

#### Scenario: Expand an elided holder signature

- **WHEN** a compiler lifetime-expansion request selects an elided holder constructor
- **THEN** the result explicitly names all inferred declaration binders and nominal lifetime arguments and reparses to the same canonical contract
