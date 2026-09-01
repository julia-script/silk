## ADDED Requirements

### Requirement: Canonical source exports portable seeded random generation

The standard library SHALL ship the `Random` service, `Xoshiro256StarStar` provider, and derived
seeded-random operations as documented canonical `silk/random` source in the deterministic manifest.
The module SHALL compile through the ordinary source pipeline, depend only on portable
standard-library facilities, and contain no intrinsic call, host import, target-specific provider,
or compiler-recognized declaration.

#### Scenario: Import seeded random on every target

- **WHEN** a program imports `silk/random` and uses a seeded provider
- **THEN** module closure resolves the canonical source and the program remains executable by the
  evaluator, direct WebAssembly, and native LLVM without a random host capability

#### Scenario: Navigate the complete public surface

- **WHEN** tooling resolves the service, provider, constructor, or derived operation
- **THEN** go-to-definition opens its canonical shipped Silk declaration rather than a generated
  signature or compiler catalog entry

#### Scenario: Copy the implementation without privilege

- **WHEN** equivalent declarations are copied into a user module under different legal names
- **THEN** they receive the same service, mutation, arithmetic, ownership, and execution behavior
  without compiler registration

#### Scenario: Package the random module

- **WHEN** the compiler package and standard-library documentation are generated
- **THEN** content verification includes the canonical random source, its generated embedding, and
  its generated reference page with no independently editable duplicate
