## ADDED Requirements

### Requirement: Type alias declarations parse losslessly and recover locally

The parser SHALL recognize `[pub] type Name = <type>` as a module-level declaration wherever a
struct declaration is accepted. The target SHALL accept every supported type form, including
structural unions, generic applications, arrays, and callable types. The concrete tree SHALL
preserve the optional `pub` token, the keyword, the name, the `=` token, the complete target, every
trivia item, and exact spans. A missing name, `=`, or target SHALL be recorded as explicit missing
syntax, and recovery SHALL resume at the next declaration start without consuming it. A
type-parameter list after the name SHALL parse as a retained explicit branch so semantic analysis
can reject it.

#### Scenario: Parse a public union alias

- **WHEN** source contains `pub type FetchError = HttpError | JsonError`
- **THEN** the concrete tree records one type alias declaration whose target is the union with both member spellings and the separator

#### Scenario: Recover a missing target

- **WHEN** source contains `type Broken =` followed by `struct Next {}`
- **THEN** the alias records an explicit missing target and the struct declaration parses intact

#### Scenario: Retain a parameter list for semantic rejection

- **WHEN** source contains `type Pair<T> = Point<T>`
- **THEN** the concrete tree retains the parameter list as a branch of the alias declaration without a parser diagnostic
