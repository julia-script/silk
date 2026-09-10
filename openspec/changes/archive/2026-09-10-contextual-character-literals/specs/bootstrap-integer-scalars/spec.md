## ADDED Requirements

### Requirement: Character literals select an immediate integer context

A character literal SHALL first decode exactly one valid Unicode scalar. Without an immediate
concrete integer expectation it SHALL default to `char`. With such an expectation it SHALL have
that integer type and the exact Unicode scalar number, provided that number is representable.
This selection SHALL apply to signed, unsigned and pointer-sized integers through existing
immediate contexts: annotated bindings, primitive constants, concrete ordinary and explicit generic
call parameters, pipeline parameters, returns, fields, contextual array elements, assignments and
homogeneous operands. It SHALL NOT convert encoded UTF-8 bytes or an already-typed value.

A known typed operand SHALL select a character literal operand's type in either operand order.
When all operands are literals, the existing first-literal fallback SHALL remain; no global,
retrospective or mixed-literal unification SHALL be introduced. Float, bool, string and nominal
enum expectations SHALL NOT make character literals values of those types.

#### Scenario: Preserve the default character type

- **WHEN** source declares `let value = 'A'` without an integer expectation
- **THEN** `value` has type `char` and passing it to a `u8` parameter without conversion fails

#### Scenario: Select the scalar number

- **WHEN** source declares `let ascii: u8 = 'A'` and `const accented: u8 = 'é'`
- **THEN** the values have type `u8` and exact values 65 and 233, respectively

#### Scenario: Select either operand order

- **WHEN** `byte` is `u8` and source compares `byte == 'A'` or `'A' == byte`
- **THEN** the character literal receives `u8` in both comparisons

#### Scenario: Reject unrepresentable scalars

- **WHEN** source supplies `'☃'` in a `u8` context or `'é'` in an `i8` context
- **THEN** compilation fails before lowering and reports the selected type and exact rejected magnitude

#### Scenario: Validate Unicode before representability

- **WHEN** a surrogate escape, malformed encoding or a zero/multiple-scalar literal appears in an integer context
- **THEN** the literal remains invalid rather than producing an integer

#### Scenario: Preserve evaluation and lowering identity

- **WHEN** a character literal receives an integer type in a constant, static computation or runtime expression
- **THEN** each path preserves that integer type and exact value through its result

### Requirement: Integer range diagnostics describe the selected range

Integer literal range diagnostics SHALL identify the selected integer type and preserve the exact
rejected magnitude and bounds without JavaScript-number rounding. Character literals selected as
integers SHALL use the same integer range diagnostic as numeric spellings; ordinary expression
range errors SHALL retain code `SEM0002`. Existing constant-declaration diagnostic boundaries SHALL
remain available and describe their declared type truthfully.

#### Scenario: Report the byte range

- **WHEN** an expression literal with value 9731 cannot fit `u8`
- **THEN** `SEM0002` reports the `u8` range 0 through 255 at the literal span

#### Scenario: Preserve wide bounds exactly

- **WHEN** an integer literal exceeds `u64`
- **THEN** its diagnostic retains the exact magnitude and the bound 18446744073709551615
