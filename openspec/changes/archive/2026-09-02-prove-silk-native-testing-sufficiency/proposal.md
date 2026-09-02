## Why

The handoff is incomplete until focused user and standard-library programs prove that the minimal
inventory, ordinary-source runner, reporter service, and byte assertion helper can test real
standard-library behavior without hidden actor privilege.

Source: [SLP-0004, revision 36](../../../proposals/0004-silk-native-testing/proposal.md),
SHA-256 `0a39823f15178c075870f85c54ee86c2a8be5cd873c3d4139696500c07331808`,
realization slice 4 of 4. Depends on `establish-silk-test-inventory`,
`add-silk-test-standard-library`, and `add-silk-test-command`.

## What Changes

- Add ordinary Silk tests for the existing seeded `Random` service, including its published
  `fillBytes` vector through `Test.equalBytes`.
- Exercise user-package and standard-library catalogs, default and explicit test roots, a distinct
  custom runner, replaceable Reporter state, complete logical failure paths, and focused filters.
- Prove deterministic order, single invocation, statuses 0/1/2, exact non-ASCII filter bytes, and
  fatal trap behavior through evaluator-only command acceptance cases.
- Audit syntax, semantic, HIR, MIR, evaluator, intrinsic, backend, and command artifacts to show no
  privileged phase recognizes `Test`, `Reporter`, assertions, filters, or runner actors by spelling.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-language-pressure-programs`: add connected user and standard-library testing witnesses
  and a findings gate for the minimal SLP-0004 surface.

## Impact

This affects pressure-program sources, the standard-library test catalog, command acceptance
fixtures, semantic and lowering inspections, and checked-in findings. It adds no richer equality,
value rendering, skips, target configuration, compiled test engine, or trap isolation.
