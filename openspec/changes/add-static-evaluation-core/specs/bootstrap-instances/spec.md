## MODIFIED Requirements

### Requirement: Instances are discovered from the entry by a recorded worklist

Instance discovery SHALL start from one of the root module's three valid user entries: a unique
zero-parameter public ordinary `main() -> ()`, a unique zero-parameter public ordinary
`main() -> i32`, or a unique zero-parameter public `effect fn main() -> () ! E` whose requirement
row is empty and whose failure members are concrete detached owned values. Discovery SHALL retain
the selected entry kind and normalized failure metadata.

The compiler SHALL select the concrete compilation target before constructing the executable
worklist. For each demanded concrete application, the realization coordinator SHALL first evaluate
its static arguments and static body operations for that target, record the resulting canonical
specialization key, and obtain one residual typed HIR body. It SHALL close a private candidate graph
from direct residual calls and cleanup-edge prepass facts without publishing executable
reachability. After that graph closes, it SHALL run ownership and cleanup exactly once over each
successful residual specialization before admitting the resulting local and cross-module runtime
call closure. The deterministic candidate worklist SHALL record a specialization before following
direct calls, so directly and mutually recursive programs terminate with each canonical
specialization discovered exactly once in deterministic order. Declarations and static applications
not reachable from the entry SHALL NOT become runtime instances merely because their modules are
loaded or imported.

#### Scenario: Discover a call chain once each

- **WHEN** ordinary `main` returns `identity(identity(42))`
- **THEN** discovery records exactly the `main` and `identity` runtime specializations in that order

#### Scenario: Discover an effectful entry chain

- **WHEN** effectful `main` runs one reachable effect function and can fail with one concrete detached owned type
- **THEN** discovery records `main`, the reachable residual function, the failure runtime type, and its cleanup hooks deterministically

#### Scenario: Discover a cross-module call chain

- **WHEN** root `main` calls a selectively imported public mixed function which residualizes a call into a third module
- **THEN** discovery records all three residual runtime specializations once under their canonical module-qualified keys in call-discovery order

#### Scenario: Terminate on recursion

- **WHEN** one residual `main` specialization returns `main()` with the same static application
- **THEN** discovery records that specialization exactly once and terminates

#### Scenario: Terminate on cross-module mutual recursion

- **WHEN** two imported public residual functions call one another with the same canonical static applications and one is reachable from `main`
- **THEN** discovery records each canonical specialization exactly once and terminates

#### Scenario: Distinguish static applications

- **WHEN** one reachable caller applies the same declaration with two unequal canonical static argument values
- **THEN** discovery records two distinct specializations and follows each residual body's calls independently

#### Scenario: Exclude an inactive static call

- **WHEN** a call appears only in the arm not selected by `static if`
- **THEN** that call produces no worklist entry or runtime instance

#### Scenario: Exclude unreachable declarations

- **WHEN** the closure contains a declaration no residual reachable body calls
- **THEN** it produces no runtime instance and none of its static functions execute

## ADDED Requirements

### Requirement: Instance keys include canonical static values

A mixed function's instance key SHALL consist of its canonical declaration identity, normalized
concrete type and contract-row arguments, selected evidence, and the canonical encoding of every
static value argument in parameter order. The selected target SHALL belong to the enclosing
realization identity rather than a runtime argument. Equal keys SHALL share one residual instance;
unequal static values MUST NOT be conflated even when their residual bodies happen to encode
identically.

#### Scenario: Deduplicate an equal static application

- **WHEN** two reachable calls apply one mixed function with equal types, evidence, and canonical static values
- **THEN** discovery records one shared runtime specialization without losing either call site's provenance

#### Scenario: Keep target realizations separate

- **WHEN** the same source application is realized for WebAssembly and for one native target
- **THEN** each target obtains its own deterministic residual closure without placing the target in a runtime instance key or ABI lane
