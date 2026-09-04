# bootstrap-random Specification

## Purpose

Define distinct provider-replaceable capabilities for secure random data, deterministic insecure
random streams, and one stable insecure seed, with portable policy implemented in ordinary Silk source.

## Requirements

### Requirement: Random denotes unpredictable CSPRNG data

The standard library SHALL define `silk/random.Random` as an exclusive provider-replaceable
service for fresh cryptographically secure random or pseudorandom data. A production provider of
`Random` MUST return data that remains computationally unpredictable to callers and MUST NOT use a
deterministic seed, the insecure generator, clocks, process identifiers, or addresses as a
substitute. An environment unable to uphold this contract SHALL omit the provider rather than
provide deterministic bytes under the `Random` identity.

#### Scenario: Obtain fresh secure data

- **WHEN** two nonempty requests are made to a conforming production `Random` provider
- **THEN** both requests return fresh CSPRNG output and neither result is derived from a
  caller-visible deterministic seed

#### Scenario: Omit an unavailable secure capability

- **WHEN** an execution environment cannot supply unpredictable CSPRNG data
- **THEN** it exposes no official `Random` provider instead of substituting `InsecureRandom`

### Requirement: Random owns one exact-fill provider primitive

The `Random` service SHALL own exactly one provider operation that fills a complete exclusive byte
slice and returns no recoverable failure. Every provider and the public wrapper MUST complete an
empty slice without consulting or advancing its source. A nonempty successful call MUST initialize
every requested byte; short successes are not observable at this public boundary. Provider failure
or malformed output SHALL terminate as a fatal defect rather than permit application fallback to
weaker data.

#### Scenario: Fill a complete slice

- **WHEN** a caller passes a nonempty exclusive byte slice to `Random.fillBytes`
- **THEN** the operation returns only after every element contains fresh provider data

#### Scenario: Fill an empty slice through either call surface

- **WHEN** an empty slice is passed through the public wrapper or direct `Random.fillBytes` service dispatch
- **THEN** the call succeeds without consulting or advancing the active provider

#### Scenario: Refuse weak recovery

- **WHEN** an official provider cannot complete a secure request
- **THEN** execution terminates without returning partial data or a recoverable value that invites an insecure fallback

### Requirement: Secure derived operations have stable byte consumption

The secure module SHALL derive `nextU64`, `nextBool`, and `below` in ordinary Silk source over the
exact-fill service. `nextU64` SHALL consume exactly eight bytes and interpret them least-significant
byte first. `nextBool` SHALL consume one `u64` and test bit 63. `below(0)` SHALL return `None`
without provider consumption; a positive bound SHALL use complete-word rejection sampling and MUST
NOT use an unconditional remainder.

#### Scenario: Decode one secure word

- **WHEN** a scripted test provider fills eight known bytes
- **THEN** `nextU64` returns their specified little-endian `u64` value and consumes exactly those eight bytes

#### Scenario: Reject biased secure words

- **WHEN** a scripted test provider first supplies a word in the rejection prefix and then an accepted word for a positive bound
- **THEN** `below` consumes both words and returns the accepted word reduced below the exclusive bound

#### Scenario: Select an empty secure range

- **WHEN** `below` receives zero
- **THEN** it returns `None` without invoking the active provider

### Requirement: InsecureRandom is the explicit deterministic stream capability

The existing seeded xoshiro256** service, provider, stable sequence, boolean mapping, unbiased
bounded selection, and little-endian byte filling SHALL move to
`silk/insecure_random.InsecureRandom`. The old `silk/random.Random` deterministic identity SHALL
cease to exist. `Xoshiro256StarStar` and `seeded` SHALL remain explicitly non-cryptographic and MUST
NOT be documented or exposed as suitable for secrets, credentials, keys, nonces, tokens, or
hash-flood protection.

#### Scenario: Preserve a seeded sequence under the insecure name

- **WHEN** two `InsecureRandom.seeded` providers receive the same `u64` seed and are advanced identically
- **THEN** they produce the same existing xoshiro256** sequence on native and LLVM-generated WebAssembly execution

#### Scenario: Remove the misleading old identity

- **WHEN** source imports `silk/random`
- **THEN** it receives only the secure `Random` API and cannot resolve the seeded xoshiro provider through that module

#### Scenario: Use insecure random portably

- **WHEN** an LLVM-generated WebAssembly program uses only `silk/insecure_random` with an ordinary provider
- **THEN** it requires no OS random intrinsic or host import

### Requirement: InsecureSeed is one provider-stable copyable 128-bit value

The standard library SHALL define a copyable `Seed` consisting of two observable `u64` words and a
shared `InsecureSeed` service that returns one provider-stable seed. The capability is intended to
be read once at application or language-runtime initialization for hash-flood hardening, but it
SHALL make no cryptographic-security promise. Repeated reads from one immutable provider SHALL
return the same pair rather than create a reusable random stream.

#### Scenario: Read one fixed seed repeatedly

- **WHEN** a fixed `InsecureSeed` provider is read more than once through shared provision
- **THEN** every read returns a copy of the same two `u64` words

#### Scenario: Construct a deterministic seed provider

- **WHEN** a deterministic environment constructs an `InsecureSeed` provider from two explicit words
- **THEN** it can provide the capability without importing or requiring secure `Random`

### Requirement: A secure provider can initialize InsecureSeed once

The insecure-seed module SHALL expose an ordinary source constructor that consumes exactly two
secure `u64` values from an active `Random` provider and stores them in one immutable
`InsecureSeed` provider. Reading the resulting provider SHALL perform no further random request.

#### Scenario: Initialize from secure random

- **WHEN** the constructor runs against a scripted `Random` provider containing sixteen known bytes
- **THEN** it stores the two corresponding little-endian words and later seed reads consume no additional bytes

### Requirement: Official OS random support is native, non-waiting, and pay-for-use

The official `OsRandom` provider SHALL support the current GNU/Linux and macOS native targets. It
MUST use an initialized OS CSPRNG without waiting for
external entropy; an unready or failing source SHALL cause immediate fatal failure rather than
blocking for entropy or returning weak data. A reachable OS-provider operation SHALL be rejected
for LLVM-generated WebAssembly. Merely importing, type-checking, or retaining an unreachable OS provider
SHALL add no native random symbol or reject a portable artifact. This version SHALL NOT provide
Windows, WASI, browser, or ambient default integration.

#### Scenario: Link one native random consumer

- **WHEN** a native executable reaches `OsRandom.fillBytes`
- **THEN** its artifact includes exactly the required OS-random runtime symbol and returns complete CSPRNG output without waiting for entropy readiness

#### Scenario: Fail rather than wait for entropy

- **WHEN** a supported host reports that its CSPRNG is not ready
- **THEN** `OsRandom` terminates without waiting or substituting predictable bytes

#### Scenario: Keep portable code free of OS random

- **WHEN** an executable reaches only `InsecureRandom` or a fixed `InsecureSeed` provider
- **THEN** it includes no OS-random runtime symbol

#### Scenario: Reject reachable LLVM-generated WebAssembly OS random

- **WHEN** an LLVM-generated WebAssembly entry reaches the official `OsRandom` provider
- **THEN** planning reports the stable intrinsic target-unavailable diagnostic before emission
