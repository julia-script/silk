## ADDED Requirements

### Requirement: The pinned Clang finalizes LLVM WebAssembly

The external toolchain boundary SHALL finalize LLVM bitcode planned for `wasm32-unknown-unknown` into a standalone WebAssembly module using structured pinned-Clang arguments, no shell command string, no native runtime shim, no implicit host libraries, and an exported zero-argument `silk_main` entry returning `I32`. The resulting module SHALL be atomically committed to the requested `.wasm` destination. Process failure or incompatible output SHALL return typed data retaining the command, arguments, exit status, and process output.

#### Scenario: Finalize a standalone Wasm module

- **WHEN** compatible LLVM bitcode is finalized for `wasm32-unknown-unknown`
- **THEN** the destination is an instantiable WebAssembly module exporting `silk_main`

#### Scenario: Avoid the native shim

- **WHEN** LLVM WebAssembly finalization is planned
- **THEN** neither the native C `main` shim nor host system libraries are compiled or linked

#### Scenario: Surface Clang failure as data

- **WHEN** pinned Clang cannot finalize the Wasm module
- **THEN** the outcome contains the exact structured invocation and retained failure output without a partial destination

### Requirement: Direct WebAssembly finalization bypasses the external toolchain

When the selected backend artifact already contains validated final WebAssembly bytes, durable finalization SHALL atomically write those bytes and MUST NOT invoke Clang or any linker.

#### Scenario: Commit direct Wasm bytes

- **WHEN** backend `wasm` returns a valid final module artifact
- **THEN** those bytes are committed at the requested destination with zero external-tool invocations
