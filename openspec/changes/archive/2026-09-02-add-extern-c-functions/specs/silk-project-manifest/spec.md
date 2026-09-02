## MODIFIED Requirements

### Requirement: Optional build defaults

The manifest SHALL accept an optional `[build]` table with `backend`, `targets`, `output-dir`, and
`native-libraries`. Backend identifiers SHALL be `llvm` or `wasm`; targets SHALL be a non-empty ordered array of canonical target identifiers or the portable `host` selector; `output-dir` SHALL be a non-empty manifest-relative directory; and `native-libraries` SHALL be an array of library names that the native link step passes as structured `-l<name>` arguments, each a non-empty string without path separators, whitespace, NUL, or a leading `-`. An invalid `native-libraries` value SHALL fail project loading with a typed project error naming the manifest and reason, and the list SHALL be ignored for WebAssembly targets. When `[build]` or individual fields are omitted, `llvm` SHALL default to targets `["host"]`, `wasm` SHALL default to `["wasm32-unknown-unknown"]`, the output directory SHALL default to `build`, and the native library list SHALL default to empty.

#### Scenario: Apply sparse defaults

- **WHEN** the manifest contains no `[build]` table
- **THEN** project building selects backend `llvm`, target `host`, and output directory `build`

#### Scenario: Select multiple targets

- **WHEN** `[build]` declares `backend = "llvm"` and `targets = ["host", "wasm32-unknown-unknown"]`
- **THEN** target selectors retain their declared order and `host` resolves to the canonical current-host triple before planning

#### Scenario: Reject an incompatible batch

- **WHEN** `[build]` selects backend `wasm` with a native target
- **THEN** project planning fails before creating or replacing any artifact

#### Scenario: Deduplicate target selectors

- **WHEN** multiple selectors resolve to the same canonical target
- **THEN** that target is built once at the position of its first selector

#### Scenario: Load native libraries

- **WHEN** `silk.toml` declares `[build]` with `native-libraries = ["c", "m"]`
- **THEN** the project build configuration retains `["c", "m"]` in order and the native link command contains `-lc` and `-lm`

#### Scenario: Reject a flag disguised as a library

- **WHEN** `native-libraries = ["-Wl,--export-dynamic"]`
- **THEN** project loading fails with a typed error naming `build.native-libraries`
