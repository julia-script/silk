## ADDED Requirements

### Requirement: Copy is one sealed validated property

A type SHALL be Copy only through the compiler's single sealed Copy property. A user MAY declare
`impl Copy` without operations when every stored field is Copy and no cleanup obligation exists.
The compiler SHALL reject operation bodies, non-Copy fields, `Drop`, allocation ownership, cycles,
unavailable proofs, and conflicting evidence.

#### Scenario: Opt a plain struct into Copy

- **WHEN** a struct containing only Copy fields declares an empty `impl Copy`
- **THEN** reads may duplicate its value and arrays, unions, and generic bounds derive that same property

#### Scenario: Reject Copy over allocated storage

- **WHEN** a struct owns allocated memory or has a Drop hook and declares `impl Copy`
- **THEN** conformance validation rejects the declaration before ownership analysis uses it

### Requirement: Stored executable values obey ordinary aggregate ownership

Represented callable and Effect values SHALL derive Copy, moves, partial-move rejection, cleanup,
and storage behavior from their realized fields. The compiler SHALL retain access-specific capture
restrictions but SHALL NOT classify every executable-bearing nominal as move-only solely because it
contains executable representation.

#### Scenario: Store a Copy callable representation

- **WHEN** a callable representation contains only Copy captures and satisfies the sealed Copy rule
- **THEN** an aggregate containing it follows ordinary Copy behavior

#### Scenario: Reject moving one affine executable field

- **WHEN** an aggregate contains an affine captured callable and another field
- **THEN** moving the callable field reports the ordinary partial-move diagnostic and retains the complete owner for recovery

## MODIFIED Requirements

### Requirement: Generic ownership is checked once and specialized exactly

Ownership SHALL classify canonical type parameters through the compiler-owned sealed Copy property
and cleanup rules, check whole-value moves and cleanup once on generic HIR, and substitute that proof
for each concrete instance. A parameter SHALL be Copy only under an explicit `Copy` bound. A
specialization MUST NOT invent structural Copy evidence, duplicate cleanup, or re-check the source
body with concrete-only behavior.

#### Scenario: Propagate an explicit Copy bound

- **WHEN** a generic caller whose parameter is bounded by `Copy` supplies that parameter to another Copy-bounded declaration
- **THEN** the caller's symbolic Copy evidence satisfies the nested call without enumerating concrete types

#### Scenario: Specialize affine and Copy uses

- **WHEN** a checked generic whole-value transfer is instantiated once with `i32` and once with an affine struct
- **THEN** each instance receives the correct concrete copy or cleanup actions from one generic ownership proof

## REMOVED Requirements

### Requirement: Nominal struct bindings are move-only owners

**Reason:** Nominal ownership now follows the sealed Copy property rather than a blanket category.

**Migration:** Declare an empty valid `impl Copy` for cleanup-free nominals that must support ordinary duplicate reads; leave all others affine.
