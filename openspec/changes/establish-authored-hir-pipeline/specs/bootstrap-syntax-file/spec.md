## ADDED Requirements

### Requirement: Syntax files lower locally into authored artifacts

One complete `SyntaxFile` and one logical module owner SHALL lower into one immutable authored module and one presentation artifact without a source resolver, imported semantic state, selected profile or evaluator. Every concrete grammar category SHALL have an explicit disposition: a semantic authored record, an ordered field of its parent, or an explicit recovery record; the disposition table SHALL be exhaustive over the concrete node vocabulary at compile time. Missing tokens and error regions SHALL become recovery records or local causes carrying the covering frontend diagnostic code. Lexical binders, shadowing, closure captures and evaluation order SHALL be recorded as owner-local references; module-level, imported, type and member lookups SHALL remain deferred. Presentation SHALL carry trivia-free spans, spellings, raw documentation and every lexical/parser diagnostic anchored to its nearest authored declaration.

#### Scenario: Release syntax after lowering

- **WHEN** a module is lowered and its `SyntaxFile` is discarded
- **THEN** the authored module retains every declaration, statement, expression, pattern and exact literal without any syntax, token or source object

#### Scenario: Shadow a parameter

- **WHEN** a body spells `let value = value + helper()` after a parameter `value`
- **THEN** the initializer's `value` refers to the parameter binder, later uses refer to the new binding, and `helper` has no lexical binding

#### Scenario: Keep a damaged neighbour local

- **WHEN** a loaded arm spells `const broken: = 1` and the next declaration is healthy
- **THEN** the constant's authored header carries a missing name with the parser's missing-token code, the healthy declaration's header and body encodings equal those of the repaired source, and formatting still rejects the damaged artifact without invoking semantics
