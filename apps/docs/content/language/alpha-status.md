# Alpha status and supported targets

Silk is an unreleased alpha language. The current compiler is substantial enough to write and run
real programs, but neither source compatibility nor standard-library API compatibility is stable.
Expect breaking changes, incomplete tooling, and sharp edges while the language is still being
designed.

This page describes the implementation in the current repository. It is a boundary document, not
a promise that every listed facility is finished or suitable for production use.

## What works today

The implemented language includes:

- modules, imports, visibility, globals, and typed constants;
- integers, floating-point values, booleans, characters, and string and byte data;
- structs, scalar enums, nominal unions, fixed arrays, runtime slices, structural unions, and
  exhaustive matching;
- functions, first-class callables, pipelines, generics, interfaces, and specialization;
- mutable places, affine ownership, shared and exclusive borrowing, explicit moves, and
  deterministic `Drop` on structured exits;
- lazy typed Effects with independent success, failure, and service-requirement channels;
- replaceable runtime services, lexical providers, failure recovery, finalization, retry, and
  explicit stack-safe Effect suspension;
- cooperative single-threaded Fibers, structured cancellation, and an explicit local scheduler;
  and
- an embedded standard library covering owned collections, allocation, text and bytes, hashing,
  formatting, logging, filesystem and process capabilities, input and output, random values,
  metrics, Effects, and Fibers.

The compiler checks source with one semantic model, lowers it to target-aware intermediate forms,
and exercises it through a logical evaluator and executable backends. Backend implementation
details do not change the source language contract.

## Supported targets

The bootstrap compiler recognizes this closed target set:

| Target                      | Kind                          | Pointer width | Artifact paths                    |
| --------------------------- | ----------------------------- | ------------: | --------------------------------- |
| `aarch64-apple-darwin`      | native macOS on Apple silicon |        64-bit | LLVM-backed native executable     |
| `x86_64-unknown-linux-gnu`  | native Linux on x64           |        64-bit | LLVM-backed native executable     |
| `aarch64-unknown-linux-gnu` | native Linux on ARM64         |        64-bit | LLVM-backed native executable     |
| `wasm32-unknown-unknown`    | standalone WebAssembly        |        32-bit | LLVM-backed or direct WebAssembly |

The direct `wasm` backend accepts only `wasm32-unknown-unknown`. The `llvm` backend accepts every
target above. Native linking and LLVM-backed WebAssembly finalization require a suitable Clang
toolchain; cross-compiling may additionally require a compatible sysroot and linker environment.

The portable selector `host` resolves only on macOS ARM64 and Linux x64 or ARM64. Windows and Intel
macOS are not native bootstrap hosts in this alpha. Target support says that the compiler knows the
target's layout and backend path; it does not promise that every OS capability is available on
every target. Reachable target-specific intrinsics are checked before artifact emission.

## Execution paths

Silk currently uses three paths to keep semantics honest:

- the logical MIR evaluator, used for deterministic semantic execution and tests;
- native executables and WebAssembly emitted through the compiler's LLVM backend; and
- a direct WebAssembly backend that does not depend on LLVM for code generation.

These are compiler implementation and validation paths, not three language editions. User code
should depend on documented source semantics and explicit target capabilities rather than emitted
LLVM or WebAssembly details.

## Important alpha boundaries

The following are not current language capabilities:

- parallel execution, multithreading, preemptive scheduling, or detached tasks;
- a package registry or stable third-party dependency format;
- broad native FFI: scalar `extern "C"` imports exist, but pointers, C-layout records, foreign
  exports, library artifacts, and a stable ABI do not;
- self-hosting, macros, async/await syntax, or generic type aliases; and
- a compatibility guarantee for syntax, diagnostics, manifests, generated artifacts, or standard
  library APIs.

Networking is not a portable standard-library service in the current alpha. Some host-facing
facilities, such as OS filesystem and child-process providers, are necessarily target-specific and
expose typed unsupported or platform failures where their contracts cannot be met.

## How to read the documentation

Hand-written examples in this documentation are compiled by the repository test suite. The
standard-library reference and diagnostic index are generated from compiler-owned sources and are
checked for staleness. The prescriptive reference records programmer-visible rules, implementation
boundaries, and evidence.

When prose and behavior disagree during alpha development, treat it as a bug. Report the smallest
source example, selected target and backend, compiler revision, and diagnostic code or observed
result.

## See also

- [Getting started](./tutorial.md)
- [Language reference](../reference/)
- [Ownership, borrowing, and cleanup](./ownership.md)
- [Effects, failures, and services](./effects.md)
- [Fibers and local scheduling](./fibers.md)
- [Diagnostic index](./diagnostics.md)
