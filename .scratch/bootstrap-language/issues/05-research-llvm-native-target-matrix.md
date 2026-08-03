# Determine the practical LLVM native target matrix

Type: research
Status: resolved

Research branch: `research/bootstrap-native-target-matrix`
Research artifact: `research/bootstrap-native-target-matrix.md`

## Question

Given a small single-threaded compiler with few platform services, what additional language-runtime,
ABI, linker, test, and distribution work is required to self-host through LLVM on targets beyond
`arm64-apple-darwin`, and what is the smallest target matrix whose marginal cost is low enough to
include in the bootstrap milestone?

## Answer

Require native stage-2 self-hosting on `arm64-apple-darwin`, `x86_64-unknown-linux-gnu`, and
`aarch64-unknown-linux-gnu`. This forms a low-cost partial CPU/OS cross while remaining entirely
64-bit and little-endian. Smoke-test COFF and WebAssembly object emission early, but defer Windows
and WebAssembly as required hosts.

LLVM removes CPU lowering and object-emission work, but Silk Effect must still own canonical target
and data-layout selection, a scalar-only private runtime ABI, platform adapters for process/files/
allocation, compiler-runtime dependencies, native linking, staged tests, and packaging policy. Use
a pinned native Clang driver for bootstrap linking rather than invoking LLD directly.

Still unresolved: external versus in-process LLVM, the runtime shim language, the Linux libc
baseline, pinned LLVM consumer compatibility, Arm64 Linux runner stability, optimization/debug
baselines, and whether an external harness may orchestrate code generation and linking.

Full primary-source report: `research/bootstrap-native-target-matrix.md` at commit
`bc27ee17b40be2afc3bc6029398f2e838be8f68d` on branch
`research/bootstrap-native-target-matrix`.
