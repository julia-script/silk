## Why

Compiler-shaped fixtures prove individual phases but do not show whether Silk composes naturally into programs people recognize. A durable suite of familiar algorithms will expose practical language walls and prevent unsupported features from being hidden behind synthetic stand-ins.

## What Changes

- Add readable Game of Life, Sieve of Eratosthenes, matrix multiplication, quicksort, CRC-32, and FFT programs under `examples/algorithms`.
- Give every example deterministic inputs, expected behavior, a capability inventory, and a checked execution status.
- Require Game of Life, Sieve, matrix multiplication, and CRC-32 to execute through evaluation and every supported declared backend.
- Permit quicksort and FFT to remain complete frontier programs only with precise deterministic blocker evidence.
- Fail on silent executable-to-frontier regressions and forbid fake primitive wrappers, precomputed answers, or algorithm-specific compiler exceptions.

## Capabilities

### New Capabilities

- `bootstrap-algorithm-examples`: Familiar executable and frontier algorithms as an honest end-to-end acceptance surface.

### Modified Capabilities

None.

## Impact

This adds example source, fixtures, expected outputs, analysis/execution harness integration, CI checks, and documentation. It may reveal follow-up language or stdlib changes, but those discoveries become separate proposals rather than silently expanding this one.
