## MODIFIED Requirements

### Requirement: Parametric conformances monomorphize per instantiation

Instance discovery SHALL resolve capability dispatch and Drop cleanup for generic nominal types
through parametric and conditional conformances, substituting the instantiation's concrete kinded
arguments into the conformance head and proving every strict-subterm requirement. Each reachable
provider/interface instantiation SHALL produce exactly one concrete witness or hook instance with a
canonical normalized key, and the worklist SHALL remain finite without runtime dictionaries.

#### Scenario: One parametric Drop serves two element types
- **WHEN** a program makes `Vector<Token>` and `Vector<i32>` reachable under one `impl<T> Drop for Vector<T>`
- **THEN** discovery yields exactly two concrete Drop hook instances whose keys carry the normalized concrete arguments, and no third instance for the unsubstituted form

#### Scenario: Parametric witnesses dispatch like concrete ones
- **WHEN** a capability requirement is satisfied by a provider whose conformance is parametric
- **THEN** the run site dispatches to the substituted concrete operation identically to an equivalent hand-written concrete conformance

#### Scenario: Conditional witness follows its proof dependencies
- **WHEN** a reachable mapped provider requires and finds one concrete source-provider witness
- **THEN** discovery records the source witness and one mapped witness before lowering the static target
