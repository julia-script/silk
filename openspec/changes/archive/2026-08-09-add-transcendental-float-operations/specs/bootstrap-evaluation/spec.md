## ADDED Requirements

### Requirement: Evaluation applies the canonical transcendental contract

Evaluation SHALL compute sine and cosine from explicit input width and bits, apply the canonical
range reduction and approximation, round to the operation width, and publish the specified result
bits. It MUST NOT delegate observable semantics to ambient JavaScript `Math` behavior.

#### Scenario: Round f32 sine once

- **WHEN** an `f32.sin` result lies between adjacent binary32 values
- **THEN** evaluation applies the canonical operation sequence and publishes the specified binary32 bits

#### Scenario: Repeat f64 cosine

- **WHEN** an equivalent `f64.cos` program is evaluated repeatedly
- **THEN** its result bits and evaluation trace are identical
