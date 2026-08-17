## ADDED Requirements

### Requirement: Contract-row algebra and constraints parse losslessly

The parser SHALL recognize contextual `Without<R, S>` expressions only where failure or requirement
rows are expected. Both operands SHALL use ordinary row-union precedence, and a value member in a
row context SHALL be preserved as a singleton member expression. Function declarations SHALL accept
one comma-separated `where` clause containing kind-directed `S in R` and fixed-mode
`&P provides S from R`, `&mut P provides S from R`, or `P provides S from R` constraints.

Call generic arguments SHALL accept a contiguous kind-correct prefix containing value, failure-row,
or requirement-row arguments. `Without`, `where`, `in`, `provides`, and `from` SHALL remain ordinary
identifiers outside their contextual grammar positions. Missing operands, separators, or constraint
terms SHALL produce bounded missing/unexpected nodes without consuming the next declaration.

#### Scenario: Parse nested row difference and constraints

- **WHEN** a declaration returns `Effect<A ! Without<E, First | Third> ? Without<R, S>> where First | Third in E, &mut P provides S from R`
- **THEN** the syntax tree retains both row-difference operands, union members, fixed exclusive provider mode, selected row, source row, commas, and contextual keywords losslessly

#### Scenario: Parse a row-generic call prefix

- **WHEN** `effect |> Effect.provideMut<&mut Logger@Audit>(&mut provider)` is parsed
- **THEN** the first generic argument remains a requirement-row expression and later omitted generic binders remain absent rather than synthesized syntax

#### Scenario: Recover a malformed constraint locally

- **WHEN** one constraint in a `where` list is missing `from R`
- **THEN** recovery records the missing terms inside that constraint and resumes at the next comma or declaration boundary

#### Scenario: Keep contextual words usable as identifiers

- **WHEN** `without`, `where`, `in`, `provides`, or `from` occurs outside the corresponding row or constraint grammar position
- **THEN** it parses under the ordinary identifier rules
