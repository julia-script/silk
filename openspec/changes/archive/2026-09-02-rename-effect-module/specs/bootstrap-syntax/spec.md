## MODIFIED Requirements

### Requirement: Minimal import declarations parse losslessly

The parser SHALL accept the accepted unconditional top-level forms: `import <path>`, `import <path>
as <namespace>`, `import <path> { <members> }`, and `import <path> as <namespace> { <members> }`.
`<path>` SHALL contain one or more contextual path-name segments separated by dots. A contextual
path-name segment MAY retain either an identifier token or a reserved-word token; this acceptance
MUST NOT make a reserved word legal as an alias, selected member, declaration name, local binding,
expression name, or type name. `<members>` SHALL contain one or more comma-separated identifiers,
each optionally followed by `as <local-name>`. The import declaration SHALL retain its keyword,
ordered path segments and dots, optional namespace alias, optional selected-member list with aliases
and separators, adjacent trivia, and exact source-owned span as one concrete branch. The concrete
tree MUST NOT decide what any path, alias, or member resolves to. Missing segments, aliases,
members, separators, and closing braces SHALL become explicit parser recovery data while following
top-level declarations remain parseable. An import whose final path segment is reserved and which
has neither an explicit namespace alias nor a selected-member list SHALL receive one stable parser
diagnostic because it cannot create a source-nameable implicit namespace binding.

#### Scenario: Parse a namespace import

- **WHEN** the source spells `import compiler.Syntax` before a complete function declaration
- **THEN** the import branch retains both path segments and their dot followed by the complete function branch

#### Scenario: Parse a changed namespace alias

- **WHEN** the source spells `import compiler.Syntax as Tree`
- **THEN** the import branch retains the `as` keyword and `Tree` alias after the complete path

#### Scenario: Parse selected members with an alias

- **WHEN** the source spells `import compiler.Syntax { Node, parse, encode as encodeSyntax }`
- **THEN** the import branch retains three ordered member entries, both commas, and the changed local alias without inventing a namespace binding

#### Scenario: Parse a hybrid import

- **WHEN** the source spells `import compiler.Syntax as Tree { Node, parse }`
- **THEN** one import branch retains the complete path, namespace alias, and both selected members in concrete order

#### Scenario: Parse a reserved path segment with an alias

- **WHEN** the source spells `import silk.effect as Effect`
- **THEN** the import path retains `effect` as its original reserved-word token and the import retains `Effect` as an ordinary explicit alias

#### Scenario: Parse a reserved interior path segment

- **WHEN** the source spells `import toolkit.effect.helpers as Helpers`
- **THEN** all three path segments are retained in order and the final ordinary segment remains eligible for namespace binding

#### Scenario: Reject an unusable implicit binding

- **WHEN** the source spells `import silk.effect` without an alias or selected-member list
- **THEN** the parser reports one stable diagnostic at the import and does not reinterpret `effect` as an ordinary identifier

#### Scenario: Keep reserved words unavailable as bindings

- **WHEN** `effect` is used as an import alias, selected member, parameter, or local binding
- **THEN** ordinary parser recovery rejects it in that position even though the same token is accepted within the import path

#### Scenario: Recover a missing path segment

- **WHEN** the source spells `import compiler. as Tree` before a function declaration
- **THEN** the import path contains an explicit missing identifier after the dot and recovery retains the alias and following function

#### Scenario: Recover a missing alias

- **WHEN** the source spells `import compiler.Syntax as` before a following declaration
- **THEN** the import branch contains a missing alias identifier with one parser diagnostic and the following declaration remains separate

#### Scenario: Recover a damaged selected list

- **WHEN** a selected-member list has a missing member, comma, alias, or closing brace
- **THEN** the damaged element remains explicit and recovery resumes at the next member boundary, closing brace, or following top-level declaration

#### Scenario: Parse multiple imports losslessly

- **WHEN** a source begins with two import declarations separated by trivia
- **THEN** both imports are separate concrete branches in source order and every token and trivia slice is retained exactly once
