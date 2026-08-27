---
'@silk-lang/compiler-cli': major
---

Replace the single `silk compile` workflow with the project-oriented `build`, `check`, and `run`
commands backed by discoverable `silk.toml` manifests and deterministic artifact paths. Rename the
direct-file escape hatch to `build-exe`, add project, planning, workflow, and process actors as
public exports, and add `smol-toml` for strict manifest decoding.
