## MODIFIED Requirements

### Requirement: HIR represents canonical union conversion explicitly

HIR SHALL carry normalized union types as canonical ordinary member sets and represent each
accepted injection or widening as one typed conversion around its source expression. The conversion
SHALL carry the exact represented source type, target union, canonical total member mapping, access
mode, and provenance. It MUST NOT encode numeric runtime tags, backend storage, pattern narrowing,
or cyclic control edges.

#### Scenario: Elaborate a nominal injection

- **WHEN** an ordinary expression such as `i32` or `Token` enters a declared `i32 | Token` return context
- **THEN** HIR contains one conversion from its precise source type to the canonical two-member union

#### Scenario: Elaborate an executable injection

- **WHEN** an exact callable or opaque Effect value enters a union containing its public contract
- **THEN** HIR retains the represented source identity and maps it to that exact canonical member

#### Scenario: Elaborate union widening inside a loop

- **WHEN** a mutable `i32 | Token` binding is assigned into an `i32 | Token | Fault` destination inside a loop
- **THEN** the write source contains one canonical widening operation and the surrounding HIR region graph remains acyclic
