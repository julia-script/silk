## ADDED Requirements

### Requirement: Native clocks use a minimal target-neutral intrinsic protocol

The sealed `Intrinsic` namespace SHALL expose five native-only unsafe primitives: system-clock read
and resolution, plus monotonic-clock read, resolution, and absolute-deadline wait. Reads SHALL use
scalar seconds and nanoseconds, resolutions SHALL use whole nanoseconds, and no primitive SHALL
expose platform clock identifiers. The scalar protocol SHALL be target-neutral. The primitive
surface MUST NOT construct or recognize
`Instant`, `SystemClock`, `MonotonicClock`, either OS provider, a standard-library module identity,
or scheduler policy.

Relative waiting, duration-to-deadline arithmetic, public canonical-value validation, provider
types, service conformance, and fatal policy for an unusable host result SHALL remain ordinary Silk
source over those primitives.

#### Scenario: Read clock components without constructing Instant

- **WHEN** an OS provider reads a selected clock through `Intrinsic`
- **THEN** the primitive writes target-neutral scalar components and ordinary source constructs the
  public `Instant`

#### Scenario: Keep provider reachability independent

- **WHEN** source reaches only a system-clock read
- **THEN** its intrinsic identity and canonical consumer do not also admit or retain a monotonic
  read, resolution query, or wait

#### Scenario: Build a relative wait in source

- **WHEN** `OsMonotonicClock.waitFor` receives a duration
- **THEN** ordinary source computes one absolute deadline and calls the absolute-wait primitive
  rather than requiring a distinct relative-wait intrinsic

#### Scenario: Copy an OS provider implementation

- **WHEN** equivalent provider source is copied under another legal module and declaration name
- **THEN** it retains equivalent semantics because only the explicit intrinsic calls have compiler
  meaning

### Requirement: Clock intrinsics preserve explicit validity and failure boundaries

Clock-read primitives SHALL report whether their scalar outputs were initialized successfully;
failed reads MUST NOT expose partial output. Resolution SHALL report success only for a positive
whole-nanosecond value representable as `u64`. Native absolute wait SHALL accept only canonical
non-negative monotonic deadlines and SHALL report success only after that clock reaches the
deadline. An evaluator host SHALL accept any canonical signed-`i64` deadline so a virtual provider
can use the complete shared `Instant` domain. The ordinary-source OS providers SHALL convert any
false result or impossible conversion into a fatal trap because the public services declare no
typed failure channel.

#### Scenario: Reject malformed scalar deadline arguments

- **WHEN** a direct unsafe intrinsic call supplies a negative fraction or at least one billion
  nanoseconds to the absolute-wait primitive
- **THEN** the native or evaluator boundary reports false without sleeping, while safe ordinary
  source cannot construct that malformed `Instant`

#### Scenario: Preserve atomic clock output

- **WHEN** the platform clock read fails
- **THEN** the primitive reports false and the source provider traps without constructing an
  `Instant` from partially initialized outputs

#### Scenario: Preserve a signed virtual monotonic timeline

- **WHEN** an evaluator host supplies a canonical negative monotonic mark and the source provider
  waits for that mark or a later canonical negative deadline
- **THEN** the evaluator forwards the exact deadline to that host rather than applying the native
  POSIX non-negative precondition

#### Scenario: Complete an absolute wait after its deadline

- **WHEN** the native primitive reports successful absolute waiting
- **THEN** a read of the selected monotonic clock is at or beyond the requested deadline
