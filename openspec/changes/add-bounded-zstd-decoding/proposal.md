## Why

Silk cannot decode zstd HTTP content or zstd files in portable source. JUL-165 requires a resumable decoder whose resource limits remain effective across arbitrary input chunks and concatenated frames.

## What Changes

- Add an ordinary-source `silk/zstd` decoder for all non-dictionary RFC 8878 block modes, frame checksums, and concatenated/skippable frames.
- Expose borrowed input, caller-owned output, explicit final input, exact progress, typed terminal failures, and configurable cumulative budgets.
- Register the module and generate source/documentation surfaces, with prescriptive reference text and independently produced acceptance fixtures.

## Capabilities

### New Capabilities

- `bootstrap-zstd-decoding`: bounded streaming zstd frame decoding and its ownership, suspension, validation, and resource contract.

### Modified Capabilities

None. Existing ordinary-source standard-library packaging and allocation requirements apply unchanged.

## Impact

Standard-library source, manifest and generated embeddings/reference pages; compiler acceptance corpus and focused contract validation; runtime-and-standard-library reference. No compiler-known decoder, native codec dependency, encoder, dictionary support, or generic stream abstraction.
