## ADDED Requirements

### Requirement: Effect exposes three transformable channels

An Effect contract SHALL treat success `A` and typed failure `E` as covariant output channels and
its access-qualified requirement row `R` as a contravariant input channel. Ordinary Silk library
code SHALL be able to transform either output, adapt an unknown requirement row through a typed
provider, or compose an effectful transformation while preserving every untouched channel and the
input Effect's run access.

#### Scenario: Transform every pure channel

- **WHEN** library code maps `Effect<A ! E ? R>` with `A -> B`, `E -> F`, and a typed requirement adapter from `R2` to `R`
- **THEN** it produces `Effect<B ! F ? R2>` without inspecting a runtime row value or changing execution timing

#### Scenario: Remove one requirement from an unknown remainder

- **WHEN** a provider satisfies one capability-role entry in `Effect<A ! E ? Capability | Rest>`
- **THEN** the resulting Effect has contract `Effect<A ! E ? Rest>` for any normalized `Rest`

### Requirement: Completed Effect outcomes can be reified compositionally

The compiler-owned Effect core SHALL offer an effectful operation that executes exactly one Effect
layer and reifies its completed typed outcome as ordinary `Result<A, E>` data instead of propagating
`E`. The operation SHALL preserve `R`, ownership, cleanup, run access, and lazy timing, and its
contract SHALL remain valid if execution can suspend before producing the Result in a future
runtime. Traps and future interruption MUST NOT be converted into typed `E` values.

#### Scenario: Map both completed branches in library code

- **WHEN** ordinary Silk code reifies `Effect<A ! E ? R>` and matches its Result with success and failure callbacks
- **THEN** either callback can produce the corresponding transformed channel while `R` remains required

#### Scenario: Preserve future suspension transparency

- **WHEN** a future execution suspends before its typed outcome completes
- **THEN** outcome reification waits compositionally and does not expose a pending state as `Result<A, E>`

### Requirement: Standard Effect combinators are library-defined

`map`, `mapError`, `mapBoth`, `flatMap`, `tap`, `catch`, `retry`, `provide`, and `provideWith` SHALL
resolve to canonical ordinary Silk declarations. The compiler MUST NOT select their semantics from
their names, actors, library origin, or a dedicated combinator HIR/MIR operation. Equivalent user
code using the compiler-owned Effect core SHALL receive the same typing, ownership, execution, and
cleanup behavior.

#### Scenario: Navigate and compile map as Silk

- **WHEN** a program calls or navigates to `Effect.map`
- **THEN** the target is canonical shipped Silk source compiled through ordinary declaration, callable, ownership, specialization, and lowering paths

#### Scenario: Define an equivalent user combinator

- **WHEN** user source defines the same generic success-channel transformation under another name
- **THEN** it compiles and executes without intrinsic registration or a compiler-recognized operation identity

### Requirement: Synchronous Effects retain a suspension-compatible abstraction

The public Effect contract SHALL NOT expose a concrete callback ABI, scheduler object, continuation
frame, runtime requirement record, or complete-or-suspended representation. Non-suspending programs
MUST NOT require a scheduler, fiber allocation, atomic synchronization, or mandatory suspension
branch, while the compiler-owned execution boundary SHALL remain capable of lowering future
suspendable Effects without changing library combinator contracts.

#### Scenario: Run a closed synchronous pipeline

- **WHEN** a closed Effect call graph cannot suspend, fork, interrupt, or observe a fiber
- **THEN** execution uses the synchronous runtime model and links no concurrency runtime solely because it uses library Effect combinators

#### Scenario: Preserve the future runner seam

- **WHEN** suspension is added later
- **THEN** existing source-defined combinators continue to compose through `run` and outcome reification without matching a scheduler-private pending state
