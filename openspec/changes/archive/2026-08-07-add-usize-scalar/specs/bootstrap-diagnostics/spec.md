## ADDED Requirements

### Requirement: Target-dependent Usize diagnostics retain exact values

The compiler SHALL report an error before MIR lowering when a reachable contextual `Usize` literal
exceeds the selected target's unsigned range. The diagnostic SHALL retain the exact source magnitude,
selected target identity, and supported bit width and SHALL sort deterministically after
target-independent semantic diagnostics. It MUST NOT report a rounded or truncated value.

#### Scenario: Diagnose a native-sized literal on Wasm

- **WHEN** a literal of `4294967296` has contextual type `Usize` and the selected target is `wasm32-unknown-unknown`
- **THEN** one diagnostic names the exact magnitude, Wasm target, and 32-bit limit before MIR lowering
