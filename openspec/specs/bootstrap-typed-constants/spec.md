# bootstrap-typed-constants Specification

## Purpose
Define explicit, typed, compile-time scalar names that improve real Silk programs while retaining
the same runtime representation and cost as spelling their literal values directly.
## Requirements
### Requirement: Constants have explicit primitive contracts

A top-level constant SHALL declare a name, one concrete primitive scalar type, and one literal
initializer. Accepted types SHALL include `bool`, every supported integer primitive including
`usize`, and every supported floating primitive. The initializer MUST be a literal of the declared
kind and fit the declared type for the selected target. Type inference, aggregate constants,
computed initializers, and effectful initialization SHALL remain unavailable.

#### Scenario: Declare representative scalar constants

- **WHEN** a module declares boolean, `u8`, `i32`, `usize`, `f32`, and `f64` constants with fitting literals
- **THEN** every declaration records its exact primitive type and canonical literal value

#### Scenario: Reject a mismatched or overflowing initializer

- **WHEN** a constant's literal has the wrong scalar kind or exceeds the declared primitive range
- **THEN** semantic analysis reports the declaration-local mismatch and exposes no usable value

### Requirement: Constant references preserve exact type and provenance

Every accepted constant reference SHALL resolve to one canonical declaration, carry its declared
primitive type, retain the use and definition spans, and behave as that typed value in contextual
literal, operator, call, return, match, and pipeline analysis. Constants SHALL be immutable and
MUST NOT be borrowed, assigned, moved as affine storage, or invoked as callables.

#### Scenario: Use an integer constant in typed expressions

- **WHEN** a `u8` opcode constant is compared, returned, passed to a `u8` parameter, and selected in a branch
- **THEN** every use has type `u8` without a repeated cast or literal annotation

#### Scenario: Refuse mutation and calls

- **WHEN** source assigns to, takes a mutable reference to, or calls a constant
- **THEN** analysis rejects the operation at the use site while keeping the declaration navigable

### Requirement: Constants have no runtime storage cost

HIR and MIR SHALL represent an accepted constant use as the declaration's typed immediate value.
Evaluation, native LLVM, and direct WebAssembly SHALL observe the same value as the equivalent
literal source without a global address, initialization routine, allocation, cleanup obligation,
or runtime load.

#### Scenario: Compare constants with direct literals

- **WHEN** equivalent programs use named constants or their direct scalar literals
- **THEN** all three engines return the same scalar observations and perform the same allocations and cleanup

#### Scenario: Preserve target-aware usize checking

- **WHEN** a `usize` constant fits one selected target but not another
- **THEN** each analysis applies that target's existing `usize` range and no target-independent runtime global is emitted

### Requirement: Constant artifacts and tools remain deterministic

Semantic facts, occurrences, hover, navigation, formatting, HIR, MIR, evaluation, symbols, and
backend artifacts SHALL present constants deterministically and preserve exact declaration/use
provenance across fresh processes.

#### Scenario: Navigate and reproduce a public constant

- **WHEN** a client navigates from an imported constant use and two fresh processes compile the same closure
- **THEN** navigation reaches the constant declaration and all published artifacts and backend bytes agree

### Requirement: Real programs validate the constrained surface

The lexer and stack VM pressure programs SHALL replace representative repeated byte classes,
opcodes, status values, or fixed bounds with typed constants while preserving their oracle results,
allocation evidence, cleanup, engine parity, and determinism.

#### Scenario: Run the constant-backed pressure corpus

- **WHEN** the updated pressure programs execute their existing success, malformed-input, and allocation-failure cases
- **THEN** their prior observable outcomes remain unchanged and the findings record whether literal repetition is resolved

