## Why

JUL-150 identifies two leaked startup-reporting pipe endpoints when close-on-exec configuration
fails after successful acquisition. Repeated failures can exhaust descriptors.

## What Changes

- Separate notice-pipe acquisition from close-on-exec configuration.
- Release every acquired endpoint exactly once while preserving the original native failure.
- Add a deterministic fixture for configuration failure and notice-pipe acquisition failure.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-child-process`: Make partial startup resource cleanup and primary-error preservation explicit.

## Impact

The generated native child-process adapter and existing native boundary tests. The public process
error ABI and successful startup handoff remain unchanged. No spawning/provider migration.
