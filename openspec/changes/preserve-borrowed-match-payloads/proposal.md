## Why

An exclusive match currently copies its selected payload before lending it to an operation. TLS
authentication consequently mutates a temporary provider and publishes the original provider state.
Nominal union carrier storage also prevents a borrowed aggregate from using its ordinary field offsets.

## What Changes

- Preserve shared and exclusive pattern bindings as references to the original selected payload,
  including projected scrutinees, scoped captures, and suspension.
- **BREAKING**: Store nominal union payloads using the active variant's canonical aggregate layout.
  Keep unified calling lanes at call boundaries, converting through the selected variant's layout.
- Remove superseded carrier-memory materialization and writeback paths, and update layout evidence
  and the language reference.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-target-layout`: Nominal unions store canonical variant bytes rather than calling lanes.
- `bootstrap-exhaustive-matching`: Make original-payload alias identity explicit for borrowed matches.

## Impact

Compiler layout, value storage, MIR match bindings, suspension storage, and LLVM emission change.
Existing ownership restrictions and the source TLS API remain intact. Structural layout and MIR
evidence cover the representation; the shared native corpus and owned TLS Wasm witness cover mutation
and publication through real execution.

Private native aggregate results also use caller-owned storage. The owned TLS integration exposed
flat result structs with over a thousand fields, making LLVM interprocedural constant propagation
exceed the native acceptance job's limit. The result transport follows typed-place classification
and preserves suspension status, diagnostic ownership, and foreign ABI boundaries.
