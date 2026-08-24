## Why

The compiler boundary alone is intentionally policy-free; Silk needs an ordinary-source `Test`
actor that turns opaque inventory primitives into assertions, events, replaceable reporting, and a
usable default runner.

Source: [SLP-0004, revision 36](../../../proposals/0004-silk-native-testing/proposal.md),
SHA-256 `0a39823f15178c075870f85c54ee86c2a8be5cd873c3d4139696500c07331808`,
realization slice 2 of 4. Depends on `establish-silk-test-inventory`.

## What Changes

- Ship ordinary canonical `silk.test` source exposing `Function`, `StackPath`, `Outcome`, metadata,
  borrowed inventory access, and safe `Test.run` wrappers.
- Add silent `Test.assert(bool)` and non-generic `Test.equalBytes(&[u8], &[u8])` helpers that fail
  with `Test.AssertionError` and retain no expected or actual values.
- Add public structured passed/failed case events and the ordinary mutable `Test.Reporter` service;
  reporting occurs after the closed outcome and reporter failure remains infrastructure failure.
- Add the ordinary-source standard runner, ASCII case-insensitive byte-substring filtering, fresh
  per-case standard reporters, simple presentation, and statuses 0, 1, and 2 from canonical runner
  root `silk/test_runner` and its ordinary `pub fn main() -> i32` entry.
- Keep every public actor renameable and unknown to compiler phases by spelling.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-silk-stdlib`: add the canonical `Test` actor, reporter contract, assertion helpers,
  filtering policy, standard reporter, and default evaluator runner source.

## Impact

This changes shipped Silk sources, their manifest and generated source table, standard-library
documentation, and source-level tests. It depends on `establish-silk-test-inventory` and is a
prerequisite for `add-silk-test-command`.
