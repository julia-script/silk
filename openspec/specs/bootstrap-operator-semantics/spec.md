# bootstrap-operator-semantics Specification

## Purpose

Define one closed, deterministic bootstrap expression-operator model that preserves qualified
data-first behavior while giving source programs conventional precedence and pipeline syntax.
## Requirements
### Requirement: Operators resolve homogeneously across integers

Arithmetic, bitwise, and comparison operators SHALL resolve only for compatible operands of the same integer type. They SHALL select that type's signed or unsigned checked semantics without implicit conversion, overload lookup, truthiness, or operand reordering. Prefix negation SHALL support signed integers only; logical negation SHALL support `bool` only; bitwise complement SHALL support integers only. Each bitwise operator SHALL select exactly the operation its named counterpart selects — `&` selects `bitAnd`, `|` selects `bitOr`, `^` selects `bitXor`, and prefix `~` selects `bitNot` — without introducing an intrinsic of its own.

#### Scenario: Resolve unsigned multiplication

- **WHEN** both operands of `*` are `u32`
- **THEN** the operator selects checked unsigned multiplication returning `u32`

#### Scenario: Reject mixed widths

- **WHEN** operands have types `i32` and `i64`
- **THEN** operator analysis rejects them without conversion

#### Scenario: Resolve a bitwise operator to its named operation

- **WHEN** both operands of `&` are `u32`
- **THEN** the operator selects `u32.bitAnd` and `a & b` evaluates to the same value as `u32.bitAnd(a, b)`

#### Scenario: Reject a mixed-width bitwise operand pair

- **WHEN** operands of `&` have types `u32` and `i32`
- **THEN** operator analysis rejects them exactly as it rejects the same pair passed to `u32.bitAnd`

### Requirement: Short-circuit operators evaluate their right operand conditionally

`&&` and `||` SHALL accept `bool` operands only and SHALL give `bool`. Unlike every other infix
operator, they SHALL NOT resolve to an actor operation, because an actor call evaluates both
operands. `&&` SHALL NOT evaluate its right operand when its left operand is `false`, and `||`
SHALL NOT evaluate its right operand when its left operand is `true`; in both cases the result is
the left operand's value. The guarantee is observable rather than an optimization: a right operand
that would trap SHALL NOT trap on the path that skips it.

The right operand SHALL be a pure expression. An effect site (`run`) or a `move` anywhere inside
it SHALL be rejected with a dedicated diagnostic, so no effect is conditionally performed and no
value is conditionally consumed. The left operand carries no such restriction, because it always
evaluates. Programs needing a conditionally performed effect SHALL spell it as a statement-level
`if`, which already carries the ownership rules for a value produced on one path only.

#### Scenario: Skip the right operand that the left operand decides

- **WHEN** a body evaluates `index < values.length && values[index] > 0` and `index` is not less
  than `values.length`
- **THEN** the expression gives `false` without indexing `values`, and so without trapping

#### Scenario: Skip the right operand of a decided disjunction

- **WHEN** a body evaluates `index >= values.length || values[index] > 0` and `index` is not less
  than `values.length`
- **THEN** the expression gives `true` without indexing `values`

#### Scenario: Reject an effect site in the right operand

- **WHEN** a body spells `flag && run decide()`
- **THEN** operator analysis rejects the right operand as impure, while the same `run` spelled as
  the left operand is accepted

#### Scenario: Reject a move in the right operand

- **WHEN** a body spells `gate && unwrap(move flag)`
- **THEN** operator analysis rejects the right operand as impure

#### Scenario: Reject a non-`bool` operand

- **WHEN** either operand of `&&` or `||` has a type other than `bool`
- **THEN** operator analysis rejects it without truthiness or conversion

### Requirement: The bootstrap operator surface is closed and ordered

The language SHALL recognize prefix `-`, `!`, and `~`; multiplicative `*`, `/`, and `%`; additive
`+` and `-`; relational `<`, `<=`, `>`, and `>=`; equality `==` and `!=`; bitwise `&`, `^`, and
`|`; short-circuit `&&` and `||`; and pipeline `|>`. Grouping parentheses SHALL override
precedence.
Primary and grouped expressions SHALL bind most tightly,
followed by right-associative prefix operators, left-associative multiplicative operators,
left-associative additive operators, the three left-associative bitwise operators, non-associative
relational operators, non-associative equality operators, the two left-associative short-circuit
operators, and left-associative pipelines. The
bitwise operators SHALL occupy three distinct precedence levels that bind tighter than every
comparison and looser than every additive operator, ordered `&` tighter than `^` and `^` tighter
than `|`, so `a | b & c` groups as `a | (b & c)` and `a & b == c` groups as `(a & b) == c`.
The short-circuit operators SHALL occupy two distinct precedence levels that bind looser than
every equality operator and tighter than the pipeline, ordered `&&` tighter than `||`, so
`a && b || c` groups as `(a && b) || c`, `a || b && c` groups as `a || (b && c)`, and
`a == b && c == d` groups as `(a == b) && (c == d)`.
Chaining a non-associative comparison without explicit
grouping SHALL be a parser error rather than an implicit multi-way comparison.

#### Scenario: Order the two short-circuit levels against one another

- **WHEN** a body returns `a && b || c`
- **THEN** the expression groups as `(a && b) || c`

#### Scenario: Bind the short-circuit operators below equality

- **WHEN** a body returns `a == b && a != b`
- **THEN** both equality comparisons nest inside `&&`, and the operands of `&&` are `bool`

#### Scenario: Bind bitwise operators above comparison and above pipelines

- **WHEN** a body spells `a & b |> f`
- **THEN** the expression groups as `(a & b) |> f`, and `a & b == c` groups as `(a & b) == c`

#### Scenario: Order the three bitwise levels against one another

- **WHEN** a body returns `8 | 1 ^ 3 & 2`
- **THEN** the expression groups as `8 | (1 ^ (3 & 2))` and evaluates to `11`

#### Scenario: Apply bitwise left associativity

- **WHEN** a body returns `8 | 1 & 2`
- **THEN** the expression groups as `8 | (1 & 2)` and evaluates to `8`

#### Scenario: Apply arithmetic precedence

- **WHEN** a body returns `1 + 2 * 3`
- **THEN** the expression groups as `1 + (2 * 3)` and evaluates to `7`

#### Scenario: Apply left associativity

- **WHEN** a body returns `20 / 5 / 2`
- **THEN** the expression groups as `(20 / 5) / 2` and evaluates to `2`

#### Scenario: Override precedence with grouping

- **WHEN** a body returns `(1 + 2) * 3`
- **THEN** the grouped addition is the multiplication's left operand and the result is `9`

#### Scenario: Reject a comparison chain

- **WHEN** a body spells `1 < 2 < 3`
- **THEN** the second relational operator is retained as recovered syntax with a parser diagnostic rather than defining a chained comparison

### Requirement: Pipelines apply one unary callable

A pipeline SHALL evaluate its completed left expression exactly once, then evaluate the callable
expression on its right and invoke that callable with the left value as its sole argument. The right
side MAY be a named function, automatic leading-argument section, binding, grouped expression, or
other expression with compatible unary callable type. Pipelines SHALL associate left-to-right and
MUST NOT create method lookup, implicit imports, or runtime namespace objects.

#### Scenario: Pipe into an actor section

- **WHEN** a body returns `2 |> i32.add(3)`
- **THEN** `i32.add(3)` first denotes a unary callable and the pipeline invokes it with `2`, producing `5`

#### Scenario: Pipe into a callable binding

- **WHEN** `increment` holds `i32.add(1)` and a body returns `2 |> increment`
- **THEN** the pipeline invokes the stored callable and produces `3`

#### Scenario: Chain applications left-to-right

- **WHEN** a body returns `2 |> i32.add(3) |> i32.multiply(4)`
- **THEN** the first application produces `5` and the second produces `20`

### Requirement: Operator-authored programs reuse the backend-neutral pipeline

Elaboration SHALL erase surface operator sugar into ordinary canonical HIR builtin calls and SHALL
erase pipeline syntax into canonical unary callable application while retaining the complete source
expression span. Lowering SHALL reuse the existing MIR operation and callable-application
vocabulary; interpretation, LLVM emission, and WebAssembly emission SHALL consume that MIR without
a surface-operator-specific or surface-pipeline-specific path. Equivalent programs SHALL have the
same result or trap behavior, and repeated compilation SHALL produce deterministic facts and
encodings.

#### Scenario: Preserve arithmetic traps across execution paths

- **WHEN** an operator-authored program overflows or divides by zero
- **THEN** interpreter, native, and WebAssembly execution all trap at the operator expression's provenance

#### Scenario: Keep MIR backend-neutral

- **WHEN** `40 + 2` and `40 |> i32.add(2)` are lowered for native and WebAssembly targets
- **THEN** both targets consume the same canonical arithmetic and callable plans with target-aware layout

#### Scenario: Repeat operator compilation

- **WHEN** equivalent operator and callable-pipeline programs are compiled repeatedly in fresh processes
- **THEN** syntax, semantic facts, HIR, MIR, diagnostics, symbols, and emitted artifacts are deterministic

### Requirement: Operators resolve homogeneously for floats

Arithmetic, negation, equality, and ordering operators SHALL resolve for two operands of the same float width using conservative IEEE semantics. They MUST NOT mix widths, convert implicitly, or search source overloads.

#### Scenario: Resolve f64 division

- **WHEN** both `/` operands are `f64`
- **THEN** the operator selects canonical IEEE `f64` division

#### Scenario: Reject mixed float widths

- **WHEN** operands are `f32` and `f64`
- **THEN** analysis rejects them without conversion

### Requirement: String equality compares exact text sequences

The equality and inequality operators SHALL accept two `string` operands and compare their exact
valid UTF-8 sequences without allocation, normalization, case folding, locale behavior, or storage
identity. Physically distinct views of identical bytes SHALL compare equal; canonically equivalent
but scalar-distinct text SHALL compare unequal.

#### Scenario: Compare distinct backing storage

- **WHEN** a static literal and an owned-string view contain the same Unicode scalar sequence
- **THEN** string equality reports true independently of their backing storage

#### Scenario: Compare unnormalized text

- **WHEN** two valid strings differ only by precomposed versus combining scalar spellings
- **THEN** equality reports false and inequality reports true
