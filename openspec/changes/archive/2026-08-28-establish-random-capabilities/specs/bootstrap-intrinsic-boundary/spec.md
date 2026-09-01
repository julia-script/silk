## ADDED Requirements

### Requirement: One unsafe native random fill primitive is admitted

The sealed `Intrinsic` namespace SHALL expose exactly one unsafe native-only random operation that
takes an exclusive byte slice, fills the complete slice, and returns `bool`. Success SHALL mean
every requested byte was initialized with host CSPRNG output. Failure SHALL commit no successful
result and SHALL expose no source-level entropy estimate, partial count, algorithm selector,
deterministic seed, native error code, or recoverable fallback. The compiler MUST NOT recognize a
random service, insecure generator, seed value, distribution, or standard-library declaration by
spelling.

#### Scenario: Fill bytes through the minimum boundary

- **WHEN** the ordinary native provider invokes the random intrinsic with a nonempty valid exclusive byte slice
- **THEN** the host reports success only after filling the complete slice

#### Scenario: Keep distributions in source

- **WHEN** standard-library code requests a random `u64`, boolean, or bounded integer
- **THEN** ordinary Silk source derives it from byte filling without another intrinsic operation

#### Scenario: Audit target availability

- **WHEN** tooling enumerates the intrinsic catalog
- **THEN** it finds one random-fill identity available to evaluation and current native targets but unavailable to direct WebAssembly
