## Purpose

Define synchronous native callback invocation and indirect C address calls with explicit access, lifetime and termination obligations.

## ADDED Requirements

### Requirement: Callback invocation is an explicit complete-call promise

Foreign contracts SHALL identify callback parameters through `callbacks`. Each identified parameter SHALL be invoked only synchronously on the calling thread within the enclosing invocation, without escape for later invocation. An unlisted callback parameter or a requested escaping, different-thread or permitted-unwind contract SHALL be rejected before native emission.

#### Scenario: Admit the existing qsort consumer

- **WHEN** the source qsort declaration lists its comparator in `callbacks`
- **THEN** it passes the existing real exported comparator address under the stated complete-call promise

#### Scenario: Reject a retained callback declaration

- **WHEN** a foreign declaration accepts a callback without the admitted synchronous promise or requests retention
- **THEN** analysis rejects the declaration before use or linking

### Requirement: Nested callback access respects active source loans

Borrowed callback parameters SHALL retain their ordinary reference obligations for the complete invocation. Callback access alongside enclosing borrowed storage SHALL be argument-local or memory-none. Raw pointers SHALL remain non-owning unsafe addresses and SHALL NOT imply borrow provenance, initialization or pinning.

#### Scenario: Nested independent storage

- **WHEN** a callback makes a nested synchronous invocation over independently owned nonconflicting storage
- **THEN** analysis admits the invocation and native execution observes the nested result

#### Scenario: Nested conflicting loan

- **WHEN** a nested invocation requires access conflicting with an active exclusive source loan
- **THEN** analysis reports an ownership diagnostic at the conflicting access

#### Scenario: Hidden callback access during a loan

- **WHEN** a foreign call contract combines borrowed references with an externally accessing callback
- **THEN** analysis rejects the incompatible access contract

### Requirement: Native address invocation is unsafe and synchronous

Silk SHALL invoke admitted nonnull C function-pointer values with exact argument/result and behavioral contracts. Reference parameter lifetimes SHALL bind per call, with implicit or explicit lifetime binders; lifetime-only exported functions SHALL remain eligible for exact address conversion. Type/value generics and naked machine exports SHALL remain ineligible. Invocation SHALL require unsafe acknowledgement and SHALL NOT specialize the runtime address into an ordinary Silk callable identity. Unsupported nullable invocation and non-native execution SHALL diagnose before emission.

#### Scenario: Call a separately compiled C address

- **WHEN** C supplies a valid address with an admitted signature to Silk
- **THEN** Silk calls that value indirectly under the selected platform C ABI and returns the exact result

### Requirement: Forbidden unwind terminates at native boundaries

Indirect calls and exported callback entry SHALL enforce fatal termination when the supported native exception unwinder attempts to cross the boundary. An outer foreign catch SHALL NOT receive that exception. The boundary SHALL NOT introduce a typed Effect outcome or promise cleanup after fatal termination.

#### Scenario: Foreign throw through an indirect callee

- **WHEN** an indirectly called C++ function throws toward an enclosing C++ catch above Silk
- **THEN** execution terminates at the Silk boundary before that catch executes

#### Scenario: Ordinary wrapper exits

- **WHEN** a source wrapper around a synchronous callback completes normally or takes its admitted typed failure exit
- **THEN** its ordinary scoped cleanup runs according to the source lifetime contract

### Requirement: Independent native evidence covers the admitted subset

The callback boundary SHALL be validated by independent native fixtures on Darwin ARM64 and GNU x86-64 execution, and GNU ARM64 compilation, linking and inspection with execution when a runner exists. Required missing supplies SHALL fail the lane. Fixtures SHALL distinguish calling-thread/dynamic-extent behavior, nested calls, exact scalar/pointer signatures, optimized output and fatal unwind. Unsupported LTO SHALL be rejected.

#### Scenario: Required supply is missing

- **WHEN** a required callback conformance lane lacks its pinned compiler, platform or fixture
- **THEN** the lane fails instead of reporting success through a skip
