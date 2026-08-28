## MODIFIED Requirements

### Requirement: Completed Effect outcomes can be reified compositionally

Canonical ordinary Silk `Effect.result` SHALL execute exactly one Effect layer and reify its
completed typed outcome as direct ordinary nominal `Result<A, E>` data instead of propagating `E`.
It SHALL construct `Result<A, E>.Success` or `Result<A, E>.Failure` without a wrapper field,
detached member, or intermediate structural union. Its implementation SHALL first map the protected
success through an ordinary exact `once fn(A) -> Result<A, E>` constructor, then apply the general
`Effect.catchAll` operation with an ordinary exact `once fn(E) -> Result<A, E>` handler for the
complete typed failure value. It MUST NOT use a completed-outcome intrinsic, and the compiler MUST
NOT recognize `Result`, its module, or either variant by spelling. The composition SHALL preserve `R`,
ownership, cleanup, run access, and lazy timing, and its contract SHALL remain valid if execution can
suspend before producing the Result in a future runtime. Traps and future interruption MUST NOT be
converted into typed `E` values.

#### Scenario: Map both completed branches in library code

- **WHEN** ordinary Silk code reifies `Effect<A ! E ? R>` and matches its Result with success and failure callbacks
- **THEN** either callback can produce the corresponding transformed channel while `R` remains required

#### Scenario: Preserve future suspension transparency

- **WHEN** a future execution suspends before its typed outcome completes
- **THEN** outcome reification waits compositionally and does not expose a pending state as `Result<A, E>`

#### Scenario: Reify directly into the nominal Result

- **WHEN** an Effect completes once with success and once with a typed failure
- **THEN** ordinary source constructs the corresponding direct `Success` and `Failure` variants and every downstream phase observes one nominal Result layer

#### Scenario: Rename an equivalent source wrapper

- **WHEN** equivalent ordinary source maps success and catches failure into a user-defined result-like nominal union under another legal function name
- **THEN** it can construct and return a user-selected nominal union without compiler registration of that union or its variants

#### Scenario: Reify a compound failure union

- **WHEN** the protected Effect can fail with `HttpError | OutOfMemoryError`
- **THEN** `catchAll` passes that complete structural union to the Failure constructor without flattening the outer Result or losing either error alternative

#### Scenario: Preserve affine branch values

- **WHEN** either completed branch carries a move-only value and its constructor is an exact `once fn`
- **THEN** ordinary `map` and `catchAll` transfer that value exactly once and clean only the unselected callable environment
