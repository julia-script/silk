## 1. Pin the upstream contract

- [x] 1.1 Add a provenance document recording Zig commit 6db520a4cd1ce2391c79d0d55b2b2d5297e133a3, the three source URLs, source hashes, and MIT notice obligations.
- [x] 1.2 Add fixture directories and metadata that distinguish exact-byte fixtures from canonical semantic fixtures.
- [x] 1.3 Add development-only commands for regenerating foundation fixtures and running the pinned LLVM compatibility tools without adding runtime dependencies.

## 2. Establish public foundation actors

- [x] 2.1 Implement the repository-wide SilkError actor with operation, message, and cause context and test its public Effect error channel.
- [x] 2.2 Implement immutable ByteString construction from copied bytes and UTF-8 strings, byte equality, hashing, and LLVM escaping tests.
- [x] 2.3 Implement opaque owner tokens and owner-bearing public handle infrastructure with cross-owner rejection tests.
- [x] 2.4 Implement the private Builder state shape, single-permit mutation gate, locked mutation helper, and read snapshot helper.
- [x] 2.5 Implement Builder.make as Effect.fn with source filename, target triple, data layout, strip mode, and module assembly state.
- [x] 2.6 Test atomic rollback for rejected mutations and safe retention of independent concurrent mutations.

## 3. Implement the private bitstream engine

- [x] 3.1 Implement fixed-width bit writing across 32-bit word boundaries with unsigned normalization and little-endian fixture tests.
- [x] 3.2 Implement bigint-capable VBR encoding and bit-count calculation with zero, boundary, and multiword fixtures.
- [x] 3.3 Implement Char6 encoding with typed rejection of invalid input.
- [x] 3.4 Implement blob encoding, 32-bit alignment, zero padding, and explicit little-endian Uint8Array materialization.
- [x] 3.5 Implement block entry, nested block state, abbreviation widths, end markers, and word-length backpatching.
- [x] 3.6 Implement unabbreviated records, abbreviation definitions, literal/fixed/VBR/array/Char6/blob operands, and module-dependent widths.
- [x] 3.7 Add focused bitstream golden tests covering every primitive and operand encoding from bitcode_writer.zig.

## 4. Encode and render a minimal module

- [x] 4.1 Translate the identification, module, and string-table record descriptors needed for an empty module into the private declarative schema.
- [x] 4.2 Implement IrText rendering for module headers and module assembly with exact byte escaping.
- [x] 4.3 Implement Bitcode encoding for the LLVM magic, identification block, minimal module block, and string-table block.
- [x] 4.4 Add deterministic empty-module and configured-header fixtures generated from the pinned Zig baseline.
- [x] 4.5 Add llvm-as, llvm-dis, verifier, and llvm-bcanalyzer compatibility checks for the minimal text and bitcode fixtures.
- [x] 4.6 Test repeated fresh-process output and package operation when Zig and LLVM executables are unavailable.

## 5. Publish and verify the foundation

- [x] 5.1 Add explicit Builder, ByteString, SilkError, IrText, and Bitcode namespace exports and package subpaths.
- [x] 5.2 Build package tests around one ManagedRuntime and Effect.fnUntraced runners without per-test Effect.runPromise calls.
- [x] 5.3 Document the builder lifecycle, byte inputs, bigint convention, runtime independence, pinned compatibility baseline, and provenance.
- [x] 5.4 Run pnpm typecheck, pnpm exec biome check ., and pnpm test in that order and resolve all change-related failures.
- [x] 5.5 Run pnpm check and pnpm release:candidate and record the successful foundation handoff.
