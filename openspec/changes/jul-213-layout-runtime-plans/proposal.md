# Why

The former `Layout.catalog` mixed declaration-wide type layout with types discovered only after
runtime reachability, obscuring which artifact can be computed before instantiation.

# What Changes

- Replace `Layout.catalog` with pre-reachability `Layout.computeTypes`.
- Replace `Layout.plan` with `Layout.computeRuntime`, which explicitly consumes the type catalog,
  instance graph, declaration facts, and opaque realizations.
- Keep concrete generic specialization and environment/calling/storage planning in the runtime
  operation.

# Capabilities

## New Capabilities

- `type-layout-catalogs`: target-relative declaration type layouts available before reachability.
- `runtime-layout-plans`: instance-dependent runtime storage, environment, and calling plans.

# Impact

Layout APIs, realization coordination, layout verification fixtures, documentation, and traces.
