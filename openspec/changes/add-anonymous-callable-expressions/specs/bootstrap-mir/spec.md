## ADDED Requirements

### Requirement: MIR lowers anonymous callables as finite exact environments

MIR lowering SHALL create a statically selected executable body and one finite concrete environment
layout for each realized anonymous callable identity. Environment fields SHALL follow semantic
capture order and retain their Copy, borrowed, exclusive, or owned representation and cleanup plan.
Construction, invocation, ordinary result delivery, effect construction, dropped-uninvoked cleanup,
and consuming cleanup SHALL preserve source evaluation and ownership order on the evaluator, Wasm,
and native paths. Structural callable types MUST NOT gain a standalone layout, universal closure
ABI, runtime target table, or heap-allocation requirement from anonymous callables.

#### Scenario: Lower an environment-bearing callable

- **WHEN** a realized anonymous callable captures one Copy value and one moved affine owner
- **THEN** MIR names its static body, lays out the two fields in semantic capture order, and emits exactly-once cleanup for the owner

#### Scenario: Lower a capture-free handler

- **WHEN** a capture-free effectful anonymous handler is passed through a generic combinator
- **THEN** specialization retains its exact empty-environment identity and lowers invocation to its static body

#### Scenario: Keep structural contracts unlayoutable

- **WHEN** layout receives only an anonymous callable's structural `fn(A) -> B` contract without its hidden concrete identity
- **THEN** it remains unavailable rather than choosing an erased closure representation
