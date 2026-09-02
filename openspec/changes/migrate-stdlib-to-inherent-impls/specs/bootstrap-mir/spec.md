## MODIFIED Requirements

### Requirement: MIR normalizes static Effect construction and dispatch

Before MIR consumers run, MIR SHALL fold a direct function whose complete body only constructs and
returns an Effect, including a function that forwards its construction to one cleanup region whose
releases only drop parameters with effect-free cleanup before returning the constructed value: a
witness operation that drops its borrowed `self` is such a constructor. When the resulting Effect
environment is local, take-once, and consumed by its statically selected runner with Copy/shared
captures, MIR SHALL replace materialization and unpacking with direct static runner arguments. The
transformation MUST preserve evaluation order, typed failure propagation, provider arguments, traps,
semantic runtime observations, and cleanup. Applicability MUST derive from generic MIR shape and
compiler facts rather than pipe syntax, declaration names, module identity, or source location.

#### Scenario: Fold a direct constructor and static run

- **WHEN** a direct constructor contains only `MakeEffect` and return, and that value is consumed once by its selected synchronous runner
- **THEN** MIR contains the direct static runner operation with substituted captures and no constructor call or intermediate Effect environment

#### Scenario: Fold a constructor that drops a borrowed parameter

- **WHEN** a provided instance runs a service operation directly and the provider's witness operation constructs its Effect without capturing `self`, then drops the borrowed `self` in a cleanup region and returns the construction
- **THEN** the fold applies as if the drop were absent, the run becomes a direct static runner operation, and the enclosing function gains no suspension point for it

#### Scenario: Identify a run piped into an erased provide section

- **WHEN** a provider-specialized execution runs an ordinary Effect piped into an erased `Effect.provideMut` section, which is one recorded call of that section's declaration
- **THEN** normalization identifies the run's runner through that recorded call exactly as for a direct call, the execution is classified from its runs rather than reported unknown, and an eligible run reaches the direct static runner operation

#### Scenario: Preserve provider and failure behavior

- **WHEN** the eligible static run carries provider references, failure mappings, or releases
- **THEN** the normalized operation retains those arguments and the same success or propagated-failure behavior

#### Scenario: Apply to copied user code

- **WHEN** a user-defined constructor has the same eligible MIR body shape as a library constructor
- **THEN** normalization reaches the same verdict without consulting its declaration or module name

#### Scenario: Refuse an unsafe candidate

- **WHEN** the constructor is complex or recursive, the Effect escapes or is reused, a capture is affine/exclusive, a parameter drop has a cleanup effect, or synchronous execution is unknown
- **THEN** MIR retains the ordinary constructor and Effect value run without partial normalization
