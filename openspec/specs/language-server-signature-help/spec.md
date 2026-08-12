# language-server-signature-help Specification

## Purpose

Defines how the language server describes the call the cursor sits inside, so the parameter being
written is read from the editor rather than recovered by moving the cursor away and counting commas
by hand.

## Requirements

### Requirement: The server advertises signature help

The language server SHALL advertise signature-help support to compatible clients and SHALL declare
`(` and `,` among its trigger characters, so an editor requests help when a call is opened and again
as each argument is separated.

#### Scenario: Client initializes signature-help support

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise signature-help support declaring the `(` and `,` trigger characters

### Requirement: Signature help describes the called declaration

A request inside a call SHALL resolve the callee through the semantic occurrence index and return
exactly one signature. Silk resolves one name to one declaration in a flat namespace, so no overload
set is presented. The signature label SHALL be the callee's source-level callable form, and the
signature SHALL carry one parameter label for each declared parameter, in declaration order. Both
SHALL come from the same presentations hover and completion detail render, so one declaration reads
identically wherever it is shown.

#### Scenario: Get the signature of a three-parameter call

- **WHEN** the cursor sits inside a call to `pub fn clamp(value: i32, low: i32, high: i32) -> i32`
- **THEN** the returned signature is labelled `pub fn clamp(value: i32, low: i32, high: i32) -> i32` and carries the parameter labels `value: i32`, `low: i32`, and `high: i32`

### Requirement: The active parameter follows the cursor

The active parameter SHALL be the number of complete arguments before the cursor, counted from the
commas the selected call's own argument list owns. A comma belonging to a nested call SHALL NOT
advance the enclosing call's selection. When calls nest, the request SHALL describe the innermost
call whose argument list contains the cursor.

#### Scenario: Move the cursor across two commas

- **WHEN** the cursor moves from just after the opening parenthesis to just after the first comma and then just after the second comma of one call
- **THEN** the reported active parameter is 0, then 1, then 2

#### Scenario: Cursor inside a nested call

- **WHEN** the cursor sits inside a call written as an argument of another call
- **THEN** the returned signature describes the inner call

### Requirement: Signature help carries authored documentation

When the called declaration has attached `///` documentation, the signature SHALL carry that
documentation as Markdown. A declaration without documentation SHALL yield a signature with no
documentation rather than an empty section.

#### Scenario: Call a documented function

- **WHEN** the cursor sits inside a call to a function preceded by `///` prose
- **THEN** the returned signature carries that prose as Markdown

### Requirement: Signature help is limited to calls and survives damage

A position that is not inside a call's argument list SHALL produce no signature help, including a
position on the callee name itself and a position after the call's closing parenthesis. Because the
parser keeps a recovered call form, a request SHALL still be answered in a source that does not
compile, which is when the arguments are most likely still being written. A position whose callee
resolves to no source-backed function declaration SHALL produce no signature help.

#### Scenario: Request outside a call

- **WHEN** a signature-help request selects a position outside any call's argument list
- **THEN** the server returns no signature help

#### Scenario: Request inside a call that does not compile

- **WHEN** a signature-help request selects a position inside a call whose argument list is unterminated and whose module reports diagnostics
- **THEN** the server still returns the callee's signature and the active parameter for that position
