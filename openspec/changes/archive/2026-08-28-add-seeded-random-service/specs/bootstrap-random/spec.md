## Purpose

Define portable, provider-replaceable pseudorandom generation with a reproducible seeded provider
and unbiased derived operations implemented entirely in ordinary Silk source.

## ADDED Requirements

### Requirement: Random is an exclusive provider-replaceable service

The standard library SHALL define a `Random` service whose only provider operation is an infallible
`nextU64` step requiring exclusive access to the active provider. Public derived random operations
SHALL obtain words through that service rather than select a concrete generator, consult ambient
state, or receive compiler-known behavior.

#### Scenario: Replace the provider lexically

- **WHEN** a program supplies a user-defined provider that returns a scripted sequence of `u64`
  values
- **THEN** every public derived operation consumes that provider through the ordinary exclusive
  service requirement and observes the scripted sequence

#### Scenario: Advance one provider exclusively

- **WHEN** two successive `nextU64` operations run against one mutable provider
- **THEN** the second operation observes the state left by the first and no shared or ambient state
  participates

### Requirement: Xoshiro256StarStar has a stable seeded sequence

The standard library SHALL expose an algorithm-named `Xoshiro256StarStar` provider. Construction
from any `u64` seed SHALL expand that seed into four state words using successive SplitMix64 steps,
and each provider step SHALL use the xoshiro256** output and transition functions. The exact output
sequence for a seed SHALL be stable across evaluator, direct-WebAssembly, and native execution.

#### Scenario: Reproduce one seed

- **WHEN** two independently constructed `Xoshiro256StarStar` providers receive the same seed and
  are advanced the same number of times
- **THEN** they return the same word at every position

#### Scenario: Accept the zero seed

- **WHEN** a provider is constructed with seed zero
- **THEN** SplitMix64 expansion produces a valid nonzero generator state and the provider advances
  through its specified sequence

#### Scenario: Match every execution engine

- **WHEN** one committed known-answer program advances a provider from a fixed seed
- **THEN** logical evaluation, direct WebAssembly, and native execution return the same specified
  fingerprint of the generated words

### Requirement: Boolean sampling consumes one word deterministically

The public `nextBool` operation SHALL consume exactly one word from the active `Random` provider and
return whether that word's most-significant bit is set.

#### Scenario: Select both boolean values

- **WHEN** a scripted provider returns one word with a clear most-significant bit followed by one
  word with a set most-significant bit
- **THEN** two `nextBool` calls return `false` and `true` respectively and consume exactly those two
  words

### Requirement: Bounded sampling is unbiased and total at zero

The public `below(upperExclusive)` operation SHALL return `None` without consuming the provider when
`upperExclusive` is zero. For a positive bound it SHALL use rejection sampling over complete `u64`
words and return `Some` containing a value less than the bound, with every result equally likely
when the provider words are uniformly distributed. It MUST NOT implement bounded selection as an
unconditional remainder operation.

#### Scenario: Reject an empty domain without advancing

- **WHEN** `below` receives zero and a subsequent operation reads the active provider
- **THEN** `below` returns `None` and the subsequent operation observes the provider's first word

#### Scenario: Discard the biased prefix

- **WHEN** a scripted provider first returns a word in the rejection interval and then a word in
  the accepted interval for a positive bound
- **THEN** `below` consumes both words and returns `Some` with the accepted word reduced below the
  bound

#### Scenario: Preserve the exclusive upper bound

- **WHEN** `below` succeeds for any positive bound
- **THEN** its contained value is greater than or equal to zero and strictly less than
  `upperExclusive`

### Requirement: Byte filling has a stable word-to-byte mapping

The public `fillBytes` operation SHALL fill the complete mutable output slice from consecutive
provider words without allocation. Each word SHALL be written least-significant byte first. A
nonempty final fragment SHALL consume one complete provider word and write only the fragment's
required leading bytes; an empty output SHALL consume no word.

#### Scenario: Fill complete and partial words

- **WHEN** the output length is not a multiple of eight
- **THEN** `fillBytes` writes every requested byte in the specified little-endian word order and
  consumes the minimum whole number of provider words that cover the output

#### Scenario: Fill an empty slice

- **WHEN** `fillBytes` receives an empty mutable slice
- **THEN** it returns successfully without changing or advancing the active provider

### Requirement: Seeded random is explicitly non-cryptographic

The `Random` service, `Xoshiro256StarStar`, and every operation derived from them SHALL be documented
as deterministic pseudorandom generation unsuitable for secrets, credentials, cryptographic keys,
nonces, or security tokens. This capability SHALL NOT expose operating-system entropy or promise
unpredictability.

#### Scenario: Inspect the public contract

- **WHEN** a user reads generated reference documentation for the seeded random module
- **THEN** the documentation identifies reproducibility and non-cryptographic use and does not
  present any operation as secure system randomness

