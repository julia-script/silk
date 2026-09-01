## ADDED Requirements

### Requirement: Applied qualified members remain lossless until declaration resolution

The expression grammar SHALL accept the declaration-neutral shape `Path<Arguments>.member`. The
syntax tree SHALL preserve the owner path, every ordered type argument, the selected member,
punctuation, trivia, and owner-qualified spans without declaring the owner to be an interface or a
nominal union and without reinterpreting owner arguments as operation-generic arguments. Semantic
resolution MAY then interpret a complete interface application as the qualifier of an operation in
a direct call or as a callable expression on the right of a pipeline. Existing nominal-union
constructors, unit values, and patterns SHALL retain the same lossless shape and meaning after
declaration resolution.

#### Scenario: Parse an applied interface operation call

- **WHEN** source evaluates `Encodable<u32>.encode(&age)`
- **THEN** syntax retains `Encodable<u32>` as the applied qualifier of operation `encode` and retains the shared-borrow argument as the call operand

#### Scenario: Preserve an applied nominal-union member

- **WHEN** source evaluates or patterns on an existing generic nominal-union member such as `Option<i32>.None`
- **THEN** syntax retains the same applied owner and member tokens and semantic resolution preserves the nominal-union meaning

#### Scenario: Keep owner and operation arguments distinct

- **WHEN** source contains `Interface<A>.operation<B>(value)`
- **THEN** syntax retains `A` on the applied owner and `B` on the operation call without merging or exchanging the argument lists

#### Scenario: Parse an applied interface operation section

- **WHEN** source evaluates `&age |> Encodable<u32>.encode`
- **THEN** syntax retains the applied qualifier as the pipeline's callable right expression and does not invent a method call or implicit argument

#### Scenario: Keep run greedy across an applied operation pipeline

- **WHEN** source evaluates `run &age |> Encodable<u32>.encode`
- **THEN** the `run` operand remains the complete pipeline under the existing run-expression boundary

#### Scenario: Recover a damaged applied qualifier locally

- **WHEN** an applied interface qualifier omits an argument delimiter or operation name before a valid following statement
- **THEN** syntax records explicit missing structure within that expression and preserves the following statement without a declaration-level cascade
