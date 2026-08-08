## MODIFIED Requirements

### Requirement: Surface operators erase into canonical HIR operations

A resolved prefix, infix, or equality expression SHALL produce the same typed HIR builtin-call
operation and ordered argument expressions as its canonical qualified actor-call form. A resolved
pipeline SHALL produce canonical unary callable application over its elaborated left and right
expressions. HIR MUST NOT retain a surface operator token, precedence node, pipeline node, implicit
namespace object, or distinct pipeline-call kind. Statically erasable sections MAY become direct
canonical calls, while stored or ownership-bearing sections SHALL retain canonical callable
construction and application. Every resulting expression SHALL retain complete source provenance,
and unavailable facts SHALL produce unavailable HIR with their originating cause.

#### Scenario: Erase infix addition

- **WHEN** a body returns `40 + 2`
- **THEN** HIR contains `BuiltinCall Add` with two typed literal arguments and the infix expression span

#### Scenario: Erase prefix negation

- **WHEN** a body returns `-value`
- **THEN** HIR contains the canonical trapping `Negate` builtin operation over the resolved `I32` value

#### Scenario: Erase a direct section pipeline

- **WHEN** a body returns `2 |> I32.add(3)` and the section need not escape
- **THEN** HIR may contain the same `BuiltinCall Add` arguments as `I32.add(2, 3)` while retaining callable facts and pipeline provenance

#### Scenario: Retain a stored callable application

- **WHEN** a body stores `I32.add(3)` and later pipes `2` into that binding
- **THEN** HIR retains canonical callable construction and application rather than inventing a surface pipeline operation

#### Scenario: Encode nested operator HIR deterministically

- **WHEN** equivalent grouped and precedence-driven operator and callable programs are elaborated repeatedly
- **THEN** their resolved operation nesting and encodings remain deterministic with exact source provenance

## ADDED Requirements

### Requirement: HIR represents callable values canonically

HIR SHALL represent named function values, automatic sections, callable types and modes, ordered
capture environments, direct or indirect application, and invocation access without backend layout
or surface-syntax lookup. Borrowed and owned captures SHALL retain their canonical ownership roots
and dependencies.

#### Scenario: Retain an owned section environment

- **WHEN** a section captures `move token` and crosses a function boundary
- **THEN** HIR carries one canonical take-once environment with the token's ownership transfer
