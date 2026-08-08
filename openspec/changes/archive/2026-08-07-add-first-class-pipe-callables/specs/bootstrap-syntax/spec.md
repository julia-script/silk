## MODIFIED Requirements

### Requirement: First call syntax remains explicit and recoverable

A call expression SHALL retain its complete callee expression, left parenthesis, ordered arguments,
separators, right parenthesis, trivia, and exact source-owned span in concrete order. Callees MAY be
named or qualified function references, sections, bindings, grouped expressions, or prior call
results. Missing call elements SHALL become explicit missing syntax with parser diagnostics.
Unexpected tokens inside a callee or argument position SHALL remain in an error region, and recovery
SHALL resume at the next comma, right parenthesis, enclosing brace, following declaration, or
end-of-file.

#### Scenario: Recover a missing right call parenthesis

- **WHEN** a returned call spells `answer(` immediately before the function's closing brace
- **THEN** the call contains a missing right parenthesis, the block retains its closing brace, and parsing completes

#### Scenario: Recover a missing call callee

- **WHEN** the returned expression consists only of `()`
- **THEN** the call retains a missing callee and both parentheses without inventing a name

#### Scenario: Preserve a supported call argument

- **WHEN** the returned call spells `identity(42)`
- **THEN** `42` is retained as the call's first decimal-integer argument rather than an error region

#### Scenario: Recover between call arguments

- **WHEN** unsupported punctuation appears between two otherwise valid arguments
- **THEN** the punctuation remains in an error region and the following comma-bounded argument remains parseable

#### Scenario: Keep integer expressions unchanged

- **WHEN** a function returns a decimal integer
- **THEN** its existing integer-literal concrete shape and recovery behavior remain unchanged

#### Scenario: Call a section result

- **WHEN** a body spells `I32.add(2)(3)`
- **THEN** the concrete tree retains two ordered postfix calls, with the first producing the callee of the second

### Requirement: Operator expressions parse losslessly by precedence

Every expression position SHALL accept grouped, prefix, infix, callable-application, and pipeline
expressions in the closed precedence and associativity order defined by
`bootstrap-operator-semantics`. A pipeline expression SHALL retain its left operand, pipe token, and
complete callable expression on the right rather than a qualified-target-only branch. Every token
and trivia slice SHALL remain owned exactly once, and concrete structure SHALL not claim that an
operator or callable resolves successfully.

#### Scenario: Parse a precedence ladder

- **WHEN** a body returns `1 + 2 * 3 == 7`
- **THEN** the concrete tree nests multiplication inside addition and addition inside equality with every token retained

#### Scenario: Parse grouped prefix syntax

- **WHEN** a body returns `-(value + 1)` and another returns `!(left == right)`
- **THEN** each prefix expression owns one grouped operand with exact parentheses and source spans

#### Scenario: Parse a function-reference pipeline

- **WHEN** a body returns `flag |> Bool.not`
- **THEN** the pipeline retains `Bool.not` as a callable expression without an invented argument list

#### Scenario: Parse a pipeline chain

- **WHEN** a body returns `2 |> I32.add(3) |> transform`
- **THEN** the concrete tree associates the two pipeline branches left-to-right in source order

### Requirement: Operator recovery stays inside the containing expression

A missing prefix operand, infix right operand, grouping parenthesis, callable operand, call
delimiter, or pipeline right expression SHALL become explicit parser recovery data at the nearest
expression boundary. Unexpected operator sequences SHALL remain in error regions. Recovery SHALL
resume at the next operand, comma, closing parenthesis, statement keyword, closing brace, top-level
declaration, or end-of-file, preserving following statements and declarations.

#### Scenario: Recover a missing infix operand

- **WHEN** a return expression spells `1 +` immediately before its block's closing brace
- **THEN** the infix expression contains an explicit missing operand and the block retains its closing brace

#### Scenario: Recover a missing grouped parenthesis

- **WHEN** a return expression spells `(1 + 2` before the block's closing brace
- **THEN** the group contains a missing right parenthesis and the block boundary remains separate

#### Scenario: Recover a missing pipeline callable

- **WHEN** a body spells `value |>` before a following declaration
- **THEN** the missing callable and diagnostic remain in the pipeline branch while the following declaration parses independently

#### Scenario: Reject ungrouped comparison chaining

- **WHEN** a body spells `1 < 2 < 3`
- **THEN** the second comparison is retained as recovered unexpected syntax with a parser diagnostic

## ADDED Requirements

### Requirement: Callable types parse explicitly

Type positions SHALL parse `fn(A) -> B`, `mut fn(A) -> B`, and `once fn(A) -> B` as distinct
callable types with ordered parameter types, result type, invocation-mode tokens, trivia, recovery,
and exact spans. Missing parameter, parenthesis, arrow, result, or mode-adjacent function keyword
SHALL recover within the containing type boundary.

#### Scenario: Parse all invocation modes

- **WHEN** one declaration accepts `fn(I32) -> I32`, `mut fn(I32) -> I32`, and `once fn(I32) -> I32`
- **THEN** the syntax tree retains three distinct callable-type branches with complete tokens and spans

### Requirement: Run consumes the complete following expression

The operand of `run` SHALL extend through the complete following expression until the enclosing
comma, closing delimiter, block delimiter, or statement boundary. It SHALL include following
pipeline branches regardless of line breaks. Grouping `run` SHALL terminate the operand explicitly
and allow the executed success value to participate in a surrounding expression.

#### Scenario: Run a transformed Effect without grouping

- **WHEN** source spells `return run attempt |> Effect.retry(2)`
- **THEN** the `RunExpression` contains the complete pipeline as its operand

#### Scenario: Pipe the executed result with grouping

- **WHEN** source spells `(run attempt) |> I32.add(1)`
- **THEN** the grouped run is the left operand of the outer pipeline

#### Scenario: Stop run at an argument comma

- **WHEN** source spells `consume(run attempt |> Effect.retry(2), other)`
- **THEN** the comma ends the run operand and `other` remains the second argument
