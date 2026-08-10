## ADDED Requirements

### Requirement: MIR normalizes static Effect construction and dispatch

Before MIR consumers run, MIR SHALL fold a direct function whose complete body only constructs and
returns an Effect. When the resulting Effect environment is local, take-once, and consumed by its
statically selected runner with Copy/shared captures, MIR SHALL replace materialization and unpacking
with direct static runner arguments. The transformation MUST preserve evaluation order, typed failure
propagation, provider arguments, traps, semantic runtime observations, and cleanup. Applicability
MUST derive from generic
MIR shape and compiler facts rather than pipe syntax, declaration names, module identity, or source
location.

#### Scenario: Fold a direct constructor and static run

- **WHEN** a direct constructor contains only `MakeEffect` and return, and that value is consumed once by its selected synchronous runner
- **THEN** MIR contains the direct static runner operation with substituted captures and no constructor call or intermediate Effect environment

#### Scenario: Preserve provider and failure behavior

- **WHEN** the eligible static run carries provider references, failure mappings, or releases
- **THEN** the normalized operation retains those arguments and the same success or propagated-failure behavior

#### Scenario: Apply to copied user code

- **WHEN** a user-defined constructor has the same eligible MIR body shape as a library constructor
- **THEN** normalization reaches the same verdict without consulting its declaration or module name

#### Scenario: Refuse an unsafe candidate

- **WHEN** the constructor is complex or recursive, the Effect escapes or is reused, a capture is affine/exclusive, or synchronous execution is unknown
- **THEN** MIR retains the ordinary constructor and Effect value run without partial normalization

### Requirement: Static Effect normalization is deterministic and verifiable

The normalization SHALL run once on shared target-aware MIR before evaluation or either backend.
MIR SHALL record deterministic accepted and rejected verdicts with source provenance. The verifier
MUST reject dangling verdict identities, inconsistent direct-run capture facts, and an accepted
candidate whose synchronous premise is not proven. Repeating normalization MUST make no edits.

#### Scenario: Normalize once for all consumers

- **WHEN** one target-aware MIR program is evaluated or emitted by LLVM or direct Wasm
- **THEN** every consumer observes the same normalized operations and verdicts

#### Scenario: Repeat normalization

- **WHEN** the same program is normalized twice or compiled in fresh processes
- **THEN** the second pass makes no structural change and encoded MIR and verdicts are deterministic
