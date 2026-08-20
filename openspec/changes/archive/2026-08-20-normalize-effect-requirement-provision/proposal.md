## Why

Requirement rows currently mix service identity, role spelling, access mode, and provider matching into unstable selectors. The confirmed model gives each dependency a canonical key, treats access compatibility separately, uses `at` only when same-service dependencies collide, and names the effectful provider helper `provideEffect`.

## What Changes

- Normalize requirement rows by canonical service-and-role keys independently from shared, exclusive, or acquired access.
- Implement `at` role selection, keyed lookup, union, subtraction, flattening, and deterministic collision diagnostics.
- Check provider access compatibility after selecting the key rather than embedding access in row identity.
- Align `provide`, `provideMut`, acquisition provision, and `provideEffect`; remove `provideWith` with no alias.
- Preserve nested Effect requirements under `flatten` and discharge only the exact selected dependency.

## Capabilities

### Modified Capabilities

- `bootstrap-type-generics`: normalize and subtract requirement keys deterministically.
- `bootstrap-flow-functions`: compose, flatten, and provide exact requirement rows.
- `bootstrap-service-declarations`: separate requirement identity from provider access compatibility.
- `bootstrap-silk-stdlib`: expose the confirmed provision APIs and role selectors.

## Impact

Depends on `normalize-effect-failure-types` and `unify-interface-service-conformance`. It changes semantic facts, generic row solving, provider selection, Effect source APIs, ownership loans, diagnostics, and tests. No ambient defaults or optional dependencies are introduced.
