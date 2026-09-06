## MODIFIED Requirements

### Requirement: Static evaluation produces one residual runtime program

`static if` SHALL require a statically evaluated `bool` condition. Both arms SHALL be parsed with
ordinary recovery, but only the selected arm SHALL undergo name resolution, type elaboration,
ownership-producing residualization, and call discovery for that specialization. The unselected arm
MUST NOT contribute types, Effects, requirements, ownership obligations, target availability,
runtime reachability, or backend operations. A `static if` without an `else` SHALL contribute no
operation when its condition is false.

The selected arm MAY contain ordinary runtime operations and runtime values. Such operations SHALL
be retained rather than executed by `StaticEvaluation`. Static selection SHALL be controlled only
by explicitly static constructs; an ordinary runtime `return`, branch, or loop MUST NOT decide
which later source is statically elaborated. At module scope, `static if` SHALL select declaration
groups under the module-static-selection contract. It SHALL remain a statement within executable
bodies and SHALL NOT introduce local declarations or become an expression.

#### Scenario: Retain runtime work from a selected static arm

- **WHEN** a target-selected arm logs through an ordinary runtime operation and returns one runtime value
- **THEN** specialization retains the log and return in the residual program without executing either during compilation

#### Scenario: Ignore an invalid name in an inactive arm

- **WHEN** the unselected arm of a syntactically valid `static if` refers to a declaration unavailable for the selected target
- **THEN** that reference receives no name or type diagnostic and contributes no semantic or runtime fact

#### Scenario: Preserve syntax diagnostics in every arm

- **WHEN** an unselected arm contains malformed syntax
- **THEN** parsing reports the ordinary syntax diagnostic because static selection does not suppress parsing or recovery

#### Scenario: Select module declarations

- **WHEN** source places `static if` at module scope
- **THEN** its selected declaration group contributes the profile-specific module surface

#### Scenario: Refuse a conditional declaration

- **WHEN** source attempts to introduce a local declaration inside an executable static arm
- **THEN** parsing rejects the local declaration; module declaration selection does not add local declarations

#### Scenario: Analyze returns after specialization

- **WHEN** an ordinary function returning `i32` has one selected arm that returns `32` and another unselected arm with a different return path
- **THEN** return analysis judges only the selected residual control flow for that specialization
