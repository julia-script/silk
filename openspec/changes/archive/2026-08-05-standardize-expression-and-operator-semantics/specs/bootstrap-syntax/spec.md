## ADDED Requirements

### Requirement: Operator expressions parse losslessly by precedence

Every expression position SHALL accept grouped, prefix, infix, and pipeline expressions in the
closed precedence and associativity order defined by `bootstrap-operator-semantics`. The concrete
tree SHALL retain a grouped expression's parentheses, a prefix expression's operator and operand,
an infix expression's left operand, operator, and right operand, and a pipeline expression's left
operand, pipe token, qualified target path, and optional later-argument list. Every token and trivia
slice SHALL remain owned exactly once, and concrete structure SHALL not claim that an operator or
pipeline target resolves successfully.

#### Scenario: Parse a precedence ladder

- **WHEN** a body returns `1 + 2 * 3 == 7`
- **THEN** the concrete tree nests multiplication inside addition and addition inside equality with every token retained

#### Scenario: Parse grouped prefix syntax

- **WHEN** a body returns `-(value + 1)` and another returns `!(left == right)`
- **THEN** each prefix expression owns one grouped operand with exact parentheses and source spans

#### Scenario: Parse a no-argument pipeline target

- **WHEN** a body returns `flag |> Bool.not`
- **THEN** the pipeline retains the qualified `Bool.not` target with no explicit argument list

#### Scenario: Parse a pipeline chain

- **WHEN** a body returns `2 |> I32.add(3) |> I32.multiply(4)`
- **THEN** the concrete tree associates the two pipeline branches left-to-right in source order

### Requirement: Operator recovery stays inside the containing expression

A missing prefix operand, infix right operand, grouping parenthesis, pipeline qualifier, pipeline
member, or pipeline argument delimiter SHALL become explicit parser recovery data at the nearest
expression boundary. Unexpected operator sequences SHALL remain in error regions. Recovery SHALL
resume at the next operand, comma, closing parenthesis, statement keyword, closing brace, top-level
declaration, or end-of-file, preserving following statements and declarations.

#### Scenario: Recover a missing infix operand

- **WHEN** a return expression spells `1 +` immediately before its block's closing brace
- **THEN** the infix expression contains an explicit missing operand and the block retains its closing brace

#### Scenario: Recover a missing grouped parenthesis

- **WHEN** a return expression spells `(1 + 2` before the block's closing brace
- **THEN** the group contains a missing right parenthesis and the block boundary remains separate

#### Scenario: Recover a damaged pipeline target

- **WHEN** a body spells `value |> .apply(1)` before a following declaration
- **THEN** the missing qualifier and diagnostic remain in the pipeline branch while the following declaration parses independently

#### Scenario: Reject ungrouped comparison chaining

- **WHEN** a body spells `1 < 2 < 3`
- **THEN** the second comparison is retained as recovered unexpected syntax with a parser diagnostic
