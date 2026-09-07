## Why

JUL-133 moves the existing secure-fill native policy out of a compiler intrinsic and generated C. Selected source makes initialized storage, partial progress, platform error conventions and fatal service outcomes reviewable together.

## What Changes

- Implement ordinary selected getrandom/arc4random_buf declarations and complete-fill policy in OsRandom.
- Preserve initialized borrowed storage, empty no-call, secure complete success and fatal failure without a new typed error or rollback promise.
- **BREAKING**: delete osRandomFill and its lowering/runtime/reservation/prelude inventory; native members require selected system/GNU libc.
- Pin target headers and validate actual source/C calls with deterministic debug/optimized native fixtures and portable availability checks.

## Capabilities

### New Capabilities

- `source-owned-native-entropy`: selected ABI, initialized-buffer transfer states, exact-fill source policy and conformance.

### Modified Capabilities

- `bootstrap-random`: clarify initialized storage and selected source availability, replace generated adapter requirements and distinguish GNU nonblocking readiness from Darwin's void provider call.

## Impact

OsRandom source, Intrinsic/Type/OsRuntime inventories, existing random boundary tests, shared corpus, generated docs/catalogs and native supply CI. No new PRNG, distribution, raw-storage API, filesystem fallback or Wasm host import.
