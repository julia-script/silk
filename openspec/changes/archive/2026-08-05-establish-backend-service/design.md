# Design — establish-backend-service

## Context

See proposal.md — Why. The lowered MIR program exists per snapshot; ticket 06 pins codegen as a
nominal `Backend` service whose bootstrap implementation lowers MIR through the existing Silk
LLVM builder (`@silk-lang/llvm`) and emits deterministic bitcode directly — no `libLLVM`, no C
API, no native FFI. The Clang object step is the next proposal's.

## Goals / Non-Goals

**Goals**

- `Backend.ts` in the compiler package: the service interface (one `emit` operation over program +
  layout + request) and the `LlvmBackend` implementation over the builder, producing
  `{bitcode, ir, symbols}` with `silk_main` for the entry.
- Deterministic bitcode gated by a committed digest; IR text as inspection artifact.
- Debug requests emit compile unit, file, subprograms, and instruction locations with
  line/column derived from source bytes at emission.
- Facade query (`Analysis.codegen`) and an LLVM IR lab.

**Non-Goals**

- No object file, no Clang, no linker — `orchestrate-native-toolchain` completes the object
  contract.
- No aggregate/union layouts yet: the slice's only type is `i32`; the layout input is threaded to
  emission where those decisions will live.
- No WebAssembly backend; the service seam is what a future one implements.

## Decisions

1. **The service is a TypeScript interface with one operation** (`emit(program, layout, request)`)
   and `LlvmBackend` as its bootstrap implementation. The builder is Effect-based and purely
   in-memory, so `emit` runs it synchronously (`Effect.runSync`) — the compiler package stays a
   pure synchronous library; the driver can lift the service into an Effect service when it
   exists.

2. **Symbols are deterministic and entry-stable.** The entry instance is always `silk_main`;
   every other instance gets `silk_<n>_<name>` with `n` its discovery ordinal and the name
   sanitized to `[A-Za-z0-9_]` — deterministic because discovery order is, and collision-free by
   construction. The artifact records the canonical-id-to-symbol table for the linker and shim.

3. **Lowering maps MIR one-to-one onto builder operations**: functions declared first (so calls
   are direct via `callDirect`), one LLVM block per MIR block, literals as interned constants,
   moves as value aliasing (locals map to SSA values; each lowered local is defined once),
   branches via `conditionalBranch` on an `icmp ne 0`, traps as `unreachable` after the trap
   intrinsic when available — the slice's lowered programs are single-block, so the general shape
   stays simple.

4. **Debug metadata follows the builder's native pattern**: `Metadata.file` + `compileUnit` +
   `llvm.dbg.cu`, one `subprogram` per function attached via `Function.setSubprogram`, and
   `setDebugLocation` per instruction from a line/column table computed from the source bytes at
   emission — MIR itself stays position-free, exactly as the ticket requires.

5. **Determinism is gated by a SHA-256 digest of the bitcode** committed as a golden. A full
   bitcode golden would be an opaque binary blob in review; the digest pins bytes just as hard
   and diffs legibly. The IR text golden stays human-readable alongside.

6. **The compiler package gains `@silk-lang/llvm` as a real dependency**; the release-candidate
   packs both tarballs and installs the compiler with an override pointing at the packed llvm,
   keeping the packed-consumer check honest.

## Risks / Trade-offs

- [Effect.runSync hides typed LlvmError failures] → Lowering only uses operations whose inputs
  the verifier already guarantees; a builder rejection is a compiler defect and may throw.
- [Digest goldens obscure what changed] → The IR golden shows the semantic change; the digest
  only enforces byte identity.

## Migration Plan

1. Land `Backend.ts` + tests + goldens (IR text, bitcode digest); package dep + surfaces.
2. Facade query, boundary-list update, LLVM IR lab.
3. Rollback is git-revert.

## Open Questions

None — object emission and entry naming beyond `silk_main` belong to the toolchain proposal.
