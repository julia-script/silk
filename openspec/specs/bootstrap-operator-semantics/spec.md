# bootstrap-operator-semantics Specification

## Purpose

Define one closed, deterministic bootstrap expression-operator model that preserves qualified
data-first behavior while giving source programs conventional precedence and pipeline syntax.
## Requirements
### Requirement: Operators resolve homogeneously across integers

Arithmetic and comparison operators SHALL resolve only for compatible operands of the same integer type. They SHALL select that type's signed or unsigned checked semantics without implicit conversion, overload lookup, truthiness, or operand reordering. Prefix negation SHALL support signed integers only; logical negation SHALL support `bool` only.

#### Scenario: Resolve unsigned multiplication

- **WHEN** both operands of `*` are `u32`
- **THEN** the operator selects checked unsigned multiplication returning `u32`

#### Scenario: Reject mixed widths

- **WHEN** operands have types `i32` and `i64`
- **THEN** operator analysis rejects them without conversion

### Requirement: The bootstrap operator surface is closed and ordered

The language SHALL recognize prefix `-` and `!`; multiplicative `*`, `/`, and `%`; additive `+`
and `-`; relational `<`, `<=`, `>`, and `>=`; equality `==` and `!=`; and pipeline `|>`. Grouping
parentheses SHALL override precedence. Primary and grouped expressions SHALL bind most tightly,
followed by right-associative prefix operators, left-associative multiplicative operators,
left-associative additive operators, non-associative relational operators, non-associative equality
operators, and left-associative pipelines. Chaining a non-associative comparison without explicit
grouping SHALL be a parser error rather than an implicit multi-way comparison.

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
