# Why

Reachable instances are immutable compiler artifacts, but the discovery result currently retains a
live source-presentation registry and is produced only inside the monolithic realization coordinator.

# What Changes

- Add `Realization.instantiate` over explicit checked artifacts, declaration facts, profile
  completion, resolution, roots, and runtime composition.
- Remove the presentation registry from `Instances.Discovery`.
- Pass presentation explicitly only to diagnostic and lowering consumers that need spans.

# Capabilities

## New Capabilities

- `instance-artifacts`: portable instance graphs separated from live compilation-session state.

# Impact

Instance discovery, target diagnostics, layout/lowering coordination, inspector snapshots, and
focused instance fixtures are affected.
