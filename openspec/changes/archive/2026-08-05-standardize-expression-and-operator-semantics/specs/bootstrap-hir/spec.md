## ADDED Requirements

### Requirement: Surface operators erase into canonical HIR operations

A resolved prefix, infix, or equality expression SHALL produce the same typed HIR builtin-call
operation and ordered argument expressions as its canonical qualified actor-call form. A resolved
pipeline SHALL produce the same ordinary builtin or declaration call with its left expression
inserted as argument zero. HIR MUST NOT retain a surface operator token, precedence node, pipeline
node, implicit namespace object, or distinct operator-call kind. The resulting operation SHALL
retain the complete surface expression span, and unavailable operator or pipeline facts SHALL
produce an unavailable HIR expression carrying their originating cause. Deterministic HIR encoding
SHALL therefore be independent of whether equivalent behavior was authored with operator,
pipeline, or complete qualified-call syntax except for source provenance.

#### Scenario: Erase infix addition

- **WHEN** a body returns `40 + 2`
- **THEN** HIR contains `BuiltinCall Add` with two typed literal arguments and the infix expression span

#### Scenario: Erase prefix negation

- **WHEN** a body returns `-value`
- **THEN** HIR contains the canonical trapping `Negate` builtin operation over the resolved `I32` value

#### Scenario: Erase a builtin pipeline

- **WHEN** a body returns `2 |> I32.add(3)`
- **THEN** HIR contains the same `BuiltinCall Add` arguments as `I32.add(2, 3)` and no pipeline-specific operation

#### Scenario: Erase an imported pipeline

- **WHEN** a body pipes a value into a resolved public namespace-qualified function
- **THEN** HIR contains one ordinary canonical declaration call with the inserted argument first

#### Scenario: Encode nested operator HIR deterministically

- **WHEN** equivalent grouped and precedence-driven operator programs are elaborated repeatedly
- **THEN** their resolved operation nesting and encodings remain deterministic with exact source provenance
