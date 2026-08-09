## ADDED Requirements

### Requirement: FFT meaningfully exercises trigonometry

The radix-2 FFT example SHALL use an input and result fingerprint whose checked non-DC frequency
components depend on both sine and cosine. A unit impulse at index zero observed only through its DC
component MUST NOT satisfy the example contract.

#### Scenario: Execute the strengthened FFT

- **WHEN** the committed eight-sample signal is transformed through all three butterfly stages
- **THEN** evaluation, native execution, and direct WebAssembly return the same committed fingerprint

#### Scenario: Detect a broken transcendental operation

- **WHEN** sine or cosine returns an incorrect conformance value used by a butterfly
- **THEN** the FFT fingerprint differs from the committed result and the example fails
