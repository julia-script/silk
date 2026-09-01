## ADDED Requirements

### Requirement: Static forms are lossless and phase-marked

The lexer and parser SHALL recognize `static` and `compileError` as keywords and preserve them
exactly in five initial forms: top-level `static fn` declarations, `static` parameter modifiers,
`let static` bindings, statement-position `static if`, and `compileError(message)` expressions or
statements. A static function MAY be prefixed by `pub` but MUST NOT combine `static` with `unsafe`,
`effect`, an implementation operation, or a service or interface operation. Static parameters and
static bindings MUST NOT also carry `mut`. Static functions SHALL otherwise use ordinary function
grammar; static parameters SHALL
retain an explicit type; static bindings SHALL retain an initializer; static conditionals SHALL
retain a condition, one block, and an optional `else` block; and `compileError` SHALL retain one
message expression without a trailing comma. The syntax tree SHALL record each form distinctly
without deciding whether an
expression is statically evaluable. `compileError` SHALL be dedicated syntax and MUST NOT parse as
an identifier call that source can import, shadow, capture, or pass as a value.

#### Scenario: Parse every initial static form

- **WHEN** source contains a static helper and a mixed function using a static parameter, static binding, static conditional, and `compileError`
- **THEN** the syntax tree retains every keyword, parameter, initializer, condition, arm, compile-error argument, trivia slice, and source span in concrete order

#### Scenario: Keep an ordinary literal ordinary in syntax

- **WHEN** a text or numeric literal appears as an argument to a static parameter
- **THEN** syntax retains the ordinary literal expression without inserting a synthetic `static` node

### Requirement: Static syntax recovery remains locally bounded

A missing static function name, parameter name or type, binding initializer, conditional condition
or arm, or compile-error argument or delimiter SHALL produce explicit missing syntax under the
existing bounded recovery rules. Recovery inside a static or compile-error form MUST preserve a
following statement, closing block, or declaration. `static if` SHALL be rejected in every
declaration-list position, while `static fn` SHALL remain the only static form that introduces a
declaration.

#### Scenario: Recover a damaged static conditional

- **WHEN** a static conditional omits its condition or closing brace before a following return
- **THEN** syntax records the missing element, preserves the following return, and terminates recovery without a cascade

#### Scenario: Reject a conditional top-level declaration

- **WHEN** a module places `static if` around a function declaration
- **THEN** parsing retains the damaged region and following declarations but produces no conditional declaration node
