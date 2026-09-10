## ADDED Requirements

### Requirement: Callback interfaces preserve complete behavioral identity

Published native interfaces SHALL preserve callback nonnullness, exact nested C signatures and normalized behavioral contracts, including each callback parameter's synchronous invocation promise. Equivalent property order and parameter renaming SHALL produce identical identity. Detectable behavior mismatches SHALL reject before backend cache reuse or linking. Generated C headers SHALL render valid C declarators without Silk-only properties. The replaced schema SHALL be rejected without compatibility decoding.

#### Scenario: Equivalent callback contract

- **WHEN** two declarations differ only in parameter naming or contract property order
- **THEN** their normalized callback interface identity agrees

#### Scenario: Mismatched callback behavior

- **WHEN** a supplied native interface differs in callback access or invocation behavior
- **THEN** compilation reports the mismatch with available declaration origins

#### Scenario: Render a callback-bearing C header

- **WHEN** a native library exports a callback-bearing function
- **THEN** its sibling header is valid C and its manifest retains the additional Silk behavioral identity
