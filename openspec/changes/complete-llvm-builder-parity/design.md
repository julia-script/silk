## Context

The five preceding changes intentionally deliver vertical feature slices. A final audit is still required because the pinned source contains large calling-convention, attribute, intrinsic, metadata, instruction, and record inventories, plus JavaScript representation differences. Parity must be based on reviewable evidence rather than an informal claim that major APIs exist.

## Goals / Non-Goals

**Goals:**

- Produce a complete disposition for the pinned upstream surface.
- Demonstrate semantic interoperability and deterministic output across supported environments.
- Stabilize, document, benchmark, and package the public API for release.

**Non-Goals:**

- Track Zig master continuously or claim compatibility with unspecified LLVM versions.
- Implement upstream TODOs, panic-only paths, or operations outside the pinned builder's supported behavior.
- Require Zig or LLVM tools in the published runtime package.

## Decisions

### Make the parity manifest the release ledger

A checked-in JSON manifest records the upstream commit, source hashes, LLVM compatibility version, and inventories of exported operations, enum cases, record schemas, and known unsupported paths. Each inventory entry names its actor, tests, fixture class, and disposition. A validation script rejects duplicate or missing dispositions.

Alternative considered: maintain a prose checklist. It cannot be exhaustively validated and drifts too easily as tables change.

### Compare at the strongest stable level

Bitstream primitives and intentionally order-matching fixture cases compare exact bytes with Zig. Larger modules compare exact bytes where construction sequence and abbreviation strategy match; otherwise both outputs are disassembled and compared as canonical LLVM IR plus bitcode-analyzer structure. Every deviation states why byte equality is inappropriate.

### Pin a compatibility toolchain

The validation documentation names one LLVM major and minor baseline. CI runs assembly, disassembly, verification, and bitcode analysis with that toolchain. Checked-in expected artifacts keep normal unit tests hermetic, while the compatibility job is authoritative for the parity claim.

### Audit public failures separately from defects

The parity pass exercises malformed and cross-owner inputs for each public actor, ensuring recoverable cases return SilkError. Assertions representing implementation corruption remain defects and are documented as such; no public API exposes an ordinary throw from validation or serialization.

### Benchmark representative module shapes

Benchmarks cover many interned types and constants, one large straight-line function, many small functions, control-flow-heavy functions, metadata-heavy output, and large blobs. Results record traced and candidate untraced variants. A hot-path exception remains only if the benchmark shows a material reproducible benefit and the code comment identifies the workload.

### Freeze exports only after the audit

The root barrel exports explicit actor namespaces; each actor has an explicit package subpath. The README documents builder and body lifecycle, bytes and bigint choices, target and data-layout responsibility, compatibility baseline, error channel, and examples. Release-candidate tests import every path from the packed tarball.

### Treat upstream updates as reviewed changes

An update command downloads or reads explicitly selected source files, checks hashes, regenerates only inventories and development fixtures, and reports semantic differences. It never silently changes the committed baseline or application source. Adopting a candidate requires a new OpenSpec change and review of every changed disposition.

## Risks / Trade-offs

- [Exact Zig byte equality may overconstrain harmless ordering] → Require exact equality only where declared stable and use canonical semantic comparison elsewhere.
- [The manifest can become busywork] → Generate source inventories mechanically but require humans to review implementation and test dispositions.
- [Toolchain fixtures vary by LLVM version] → Pin one authoritative version and label broader-version runs informational until promoted.
- [Benchmarks can be noisy] → Use repeated samples, report medians, and avoid release failure thresholds until a stable baseline exists.
- [Six changes may land out of order] → Record dependencies in every design and do not apply or archive a change before all predecessors are complete.

## Migration Plan

Apply only after all five preceding LLVM changes. First generate the inventory and resolve gaps, then expand interoperability and differential tests, measure hot paths, finalize docs and exports, and run pnpm check plus pnpm release:candidate. If the audit uncovers a foundational API flaw, update the owning earlier design and spec instead of hiding it as a parity exception. Rollback reverts release-surface additions while retaining validated feature slices.
