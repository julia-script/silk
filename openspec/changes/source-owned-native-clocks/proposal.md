## Why

JUL-132 removes the remaining compiler-owned monotonic read, resolution and wait policy. The existing system-clock extern boundary needs the same selected ABI evidence, including Darwin's unsigned clock identifier.

## What Changes

- Move all native clock declarations and validation into ordinary selected Silk source.
- Preserve failure-channel-free services, stateless construction and one checked absolute deadline across interruptions.
- **BREAKING**: remove the three monotonic intrinsics, reserved runtime symbols and generated C clock fragments; native providers require the selected system/GNU libc.
- Verify both source providers with pinned headers and separately compiled C boundaries on all three admitted targets, in debug and optimized modes.

## Capabilities

### New Capabilities

- `source-owned-native-clocks`: selected scalar/record contracts, source wait/error policy and native conformance.

### Modified Capabilities

- `bootstrap-clock-services`: replace the generated native-clock shim requirement and clarify source-selected availability.

## Impact

Standard-library native/system/monotonic clock modules, Intrinsic and native lowering/runtime inventories, existing clock boundary tests and shared native corpus, scheduler consumers, generated documentation and platform conformance CI. No new scheduler, timer service, typed error channel or Wasm host API.
