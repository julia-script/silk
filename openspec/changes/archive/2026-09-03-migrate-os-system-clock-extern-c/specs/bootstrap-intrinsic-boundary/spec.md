## MODIFIED Requirements

### Requirement: Native clocks minimize compiler privilege

The sealed `Intrinsic` namespace SHALL expose exactly three native-only unsafe clock primitives:
monotonic-clock read, resolution, and absolute-deadline wait. It MUST NOT expose a system-clock
operation. `OsSystemClock` SHALL instead declare and call the platform's ordinary C
`clock_gettime` and `clock_getres` functions over a C-layout `timespec` record. Monotonic intrinsic
reads SHALL use scalar seconds and nanoseconds, resolution SHALL use whole nanoseconds, and no
intrinsic SHALL expose platform clock identifiers. The compiler MUST NOT construct or recognize
`Instant`, `SystemClock`, `OsSystemClock`, a standard-library module identity, or system-clock
policy.

Relative waiting, duration-to-deadline arithmetic, public canonical-value validation, provider
types, service conformance, and fatal policy for an unusable host result SHALL remain ordinary Silk
source over those primitives.

#### Scenario: Read monotonic components without constructing Instant

- **WHEN** `OsMonotonicClock` reads its selected clock through `Intrinsic`
- **THEN** the primitive writes target-neutral scalar components and ordinary source constructs the
  public `Instant`

#### Scenario: Keep the system clock outside Intrinsic

- **WHEN** source reaches only a system-clock read
- **THEN** the program retains the ordinary `clock_gettime` foreign import and no system-clock
  intrinsic identity or compiler-runtime symbol

#### Scenario: Build a relative wait in source

- **WHEN** `OsMonotonicClock.waitFor` receives a duration
- **THEN** ordinary source computes one absolute deadline and calls the absolute-wait primitive
  rather than requiring a distinct relative-wait intrinsic

#### Scenario: Copy an OS provider implementation

- **WHEN** equivalent provider source is copied under another legal module and declaration name
- **THEN** it retains equivalent semantics because only explicit foreign declarations and
  intrinsic calls have compiler meaning

### Requirement: Monotonic clock intrinsics preserve explicit validity and failure boundaries

Monotonic clock-read primitives SHALL report whether their scalar outputs were initialized
successfully; failed reads MUST NOT expose partial output. Resolution SHALL report success only for
a positive whole-nanosecond value representable as `u64`. Native absolute wait SHALL accept only
canonical non-negative monotonic deadlines and SHALL report success only after that clock reaches
the deadline. An evaluator host SHALL accept any canonical signed-`i64` deadline so a virtual
provider can use the complete shared `Instant` domain. The ordinary-source OS provider SHALL
convert any false result or impossible conversion into a fatal trap because the public service
declares no typed failure channel.

#### Scenario: Reject malformed scalar deadline arguments

- **WHEN** a direct unsafe intrinsic call supplies a negative fraction or at least one billion
  nanoseconds to the absolute-wait primitive
- **THEN** the native or evaluator boundary reports false without sleeping, while safe ordinary
  source cannot construct that malformed `Instant`

#### Scenario: Preserve atomic monotonic output

- **WHEN** the platform monotonic-clock read fails
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
