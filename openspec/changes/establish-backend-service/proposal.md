## Why

Code generation enters the pipeline as a nominal service, not a hardwired stage: the compiler
driver consumes a `Backend` capability that turns one monomorphized MIR program into one
relocatable object, without inspecting backend identity. This proposal establishes that contract
and the `LlvmBackend`'s first half — lowering MIR through the existing Silk LLVM builder
(`packages/llvm`) to deterministic bitcode. The external Clang invocation that completes the
object-artifact contract lands in `orchestrate-native-toolchain`.

## What Changes

- Define the nominal `Backend` service: consumes the whole monomorphized MIR program plus an
  explicit target and codegen request, produces one relocatable object artifact. One compilation
  request → one MIR program → one LLVM module → one program object; source modules are semantic
  namespaces, not codegen units.
- Implement `LlvmBackend` MIR lowering over the existing Silk LLVM builder: physical aggregate
  and union layouts chosen at emission time from the target-layout input; deterministic LLVM
  bitcode emitted directly. No `libLLVM`, no LLVM C API, no compiler-private native FFI.
- Textual LLVM IR remains an implementation-specific inspection artifact over the same builder
  model — not a phase interchange format.
- Debug builds emit native LLVM debug metadata: compile units, files, subprograms, lexical
  scopes, instruction locations; line/column positions derived from original bytes only at
  emission.
- Add the inspector lab: emitted LLVM IR text aligned to MIR operations via provenance.

## Capabilities

### New Capabilities

- `bootstrap-backend`: The nominal `Backend` service contract and the `LlvmBackend` MIR-to-bitcode
  lowering over the Silk LLVM builder.

### Modified Capabilities

- `bootstrap-syntax-inspector`: LLVM IR lab aligned to MIR provenance.

## Impact

First consumer of `packages/llvm` from the compiler; adds the service seam the driver and any
future WebAssembly backend share. The object-artifact half of the contract is completed by the
next proposal — until then the backend's output is deterministic bitcode plus the IR inspection
artifact.

## Plan References

- [Roadmap — Track 5, proposal 11](../../../roadmaps/compiler-realignment.md)
- [Issue 06](../../../wayfinder/bootstrap-language/issues/06-bootstrap-compiler-pipeline.md),
  codegen paragraphs: "Code generation is selected through a nominal `Backend` service. Its
  bootstrap operation consumes the whole monomorphized MIR program plus an explicit target and
  codegen request and produces one relocatable object artifact." And: "The bootstrap
  `LlvmBackend` lowers MIR into the existing Silk LLVM builder, emits deterministic LLVM bitcode
  directly … It does not load `libLLVM`, use the LLVM C API, or require a compiler-private native
  FFI. Textual LLVM IR remains an implementation-specific inspection artifact."
- Same ticket, debug-metadata paragraph: "Debug builds emit native LLVM debug compile units,
  files, subprograms, lexical scopes, and instruction locations."
- Same ticket, determinism gate: identical inputs produce byte-identical "LLVM bitcode."
