## Why

Silk-native testing is not usable until projects can declare bounded test roots and invoke a runner
through a deterministic command that passes raw filters without adding a test-only entry ABI.

Source: [SLP-0004, revision 36](../../../proposals/0004-silk-native-testing/proposal.md),
SHA-256 `0a39823f15178c075870f85c54ee86c2a8be5cd873c3d4139696500c07331808`,
realization slice 3 of 4. Depends on `establish-silk-test-inventory` and
`add-silk-test-standard-library`.

## What Changes

- Add an optional `[test]` manifest table with a nonempty `roots` array and optional `runner`, all
  resolved from the manifest directory and contained by the package source root.
- Default user test roots to `package.root`, default the runner to the canonical standard runner,
  and use the deterministic toolchain catalog for standard-library test targets.
- Add `silk test [filter ...]` for user projects and `silk test --standard-library [filter ...]`
  for the toolchain catalog, compose the separately designated runner root with the test-root
  union, and run it once through the evaluator for the ordinary host target only.
- Seed the existing low-level host-input adapter with argument zero followed by unchanged raw filter
  bytes; ordinary runner source constructs and lexically provides `OsHostInput` and `Allocator`.
- Forward evaluator output to command standard output exactly once, preserve the standard runner's
  statuses 0/1/2 and every ordinary custom-entry status, treat invalid compilations as command
  errors, and keep all non-entry-completion evaluator terminations on their existing paths.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `silk-project-manifest`: configure explicit source-root-contained test roots and an optional
  distinct custom runner root.
- `silk-cli-workflows`: expose deterministic evaluator-only `silk test` execution, raw filter
  forwarding, standard-library catalog selection, and stable status handling.

## Impact

This affects project decoding, source-entry materialization, CLI parsing and help, multi-root
analysis requests, evaluator host adapters, and command integration tests. It is a prerequisite for
`prove-silk-native-testing-sufficiency`.
