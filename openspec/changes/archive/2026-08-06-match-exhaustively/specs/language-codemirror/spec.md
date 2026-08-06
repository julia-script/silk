## ADDED Requirements

### Requirement: CodeMirror highlights the match surface from compiler tokens

The CodeMirror extension SHALL style `match`, `move`, `mut`, and guard `if` by their compiler keyword
kinds and SHALL style `&`, `=>`, `..`, braces, and other pattern punctuation from their compiler
token kinds. Nominal pattern names and field bindings SHALL retain identifier/type classification,
and `_` SHALL remain visibly distinct only through its compiler-supported pattern context when that
context is available without reimplementing matching semantics.

#### Scenario: Highlight a guarded borrowed match

- **WHEN** the editor contains a shared match with a nominal guarded arm and `_` fallback
- **THEN** every keyword, operator, punctuation, type, binding, and literal receives the compiler-consistent highlight range
