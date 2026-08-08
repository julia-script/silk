## ADDED Requirements

### Requirement: Keywords use theme-friendly differentiated scopes

The TextMate grammar SHALL assign distinct scope families for control-flow keywords, declaration/storage keywords, and boolean literals, rather than placing every non-boolean keyword under a single undifferentiated keyword scope.

#### Scenario: Control versus declaration coloring

- **WHEN** a TextMate consumer tokenizes `pub fn main() { if true { return 1 } else { return 0 } }`
- **THEN** `if`, `else`, and `return` receive control-oriented keyword scopes while `pub` and `fn` receive declaration/storage-oriented scopes distinct from the control scopes, and `true` retains a boolean literal scope

### Requirement: Function names after fn receive entity scopes

The TextMate grammar SHALL scope the identifier immediately following `fn` as a function name so themes can color declarations differently from ordinary identifiers.

#### Scenario: Function declaration name

- **WHEN** a TextMate consumer tokenizes `fn greet()`
- **THEN** `greet` receives a function-name entity scope and `fn` retains its declaration/storage keyword scope

### Requirement: Type-like identifiers receive type scopes beyond builtins and patterns

The TextMate grammar SHALL assign type-oriented scopes to PascalCase type identifiers used in ordinary type positions (not only builtin names and nominal patterns before `{`), so user-defined types are highlighted consistently with builtins where themes map type scopes.

#### Scenario: User type in a signature

- **WHEN** a TextMate consumer tokenizes `fn id(x: Point) -> Point`
- **THEN** both `Point` occurrences receive a type scope

## MODIFIED Requirements

### Requirement: TextMate grammar covers the Silk lexical grammar

The package SHALL export a TextMate grammar for Silk (scope `source.silk`) that assigns scopes to keywords (with differentiated control vs declaration families), line comments, doc comments, decimal integer literals, function declaration names, type-like identifiers, operators, and punctuation as defined by the compiler's token kinds and the added scope requirements in this capability.

#### Scenario: Keyword scoping

- **WHEN** the grammar tokenizes `pub fn main() -> I32 { return 42 }`
- **THEN** `pub` and `fn` receive declaration/storage keyword scopes, `return` receives a control keyword scope, `main` receives a function-name scope, `I32` receives a type scope, and `42` receives a numeric scope

#### Scenario: Doc comment scoping

- **WHEN** the grammar tokenizes a line starting with `///`
- **THEN** the line receives a documentation comment scope distinct from a `//` line comment scope
