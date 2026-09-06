## Context and consumers

JUL-136 requires x86-64 rax for the operation/result, rdi/rsi/rdx/r10/r8/r9 for up to six
arguments, and rcx/r11 clobbers; ARM64 uses x8 plus x0–x5 and returns x0. Values are 64-bit words
or raw data pointers. Entry fragments must read the incoming rsp/sp, explicitly align a stack if
needed, and transfer to a source-chosen symbol. These facts determine this primitive's bounded
lanes; the compiler does not interpret an instruction as an OS operation or know its number.

## Decisions

`Intrinsic.assembly<Result>(template, constraints, clobbers, memory, sideEffects,
noReturn, inputs)` has six literal metadata arguments and one runtime tuple. The result uses an ordinary Silk type argument; the compiler infers the input tuple type from its literal expression, since anonymous tuple types are not source type syntax. The metadata is validated and consumed before runtime
lowering, with no string allocation or metadata parameters in the emitted calling convention.
This is a compiler-owned mixed operation, unlike ordinary source wrappers. Literal-only metadata
avoids admitting a second static descriptor language or allowing runtime strings to become code.

The result is unit or one i64/u64/isize/usize/raw-data-pointer lane. Inputs are a tuple of zero
through seven non-unit admitted lanes. The constraint grammar admits one optional `={register}`
or `=&{register}` output, followed by one `{register}` or `0` per input. `0` ties exactly one input
to the output with compatible LLVM lane kind. Fixed inputs cannot silently overlap an output;
users must spell the tie. Other register classes, memory operands, alternatives, indirect outputs,
multiple results and arbitrary modifiers are rejected. x86-64 fixed registers are rax, rdi, rsi,
rdx, rcx and r8–r11; ARM64 registers are x0–x17. Stack/frame/link/platform registers are excluded
from operands/clobbers. Early-clobber is preserved in the LLVM constraint, not inferred from text.

Clobbers are a comma-separated list of admitted registers plus `flags` (LLVM flags on x86-64,
cc on ARM64). Duplicates and operand overlap diagnose. `memory` is none/read/write/readwrite;
non-none memory produces LLVM's memory clobber and corresponding conservative call effects.
Side effects are explicit. No-return requires unit plus sideEffects=true and emits unreachable.
The template is single-dialect LLVM assembly text (AT&T x86-64, default ARM64), not an assembler
module: NUL, invalid text, module directives and unsupported template modifiers are rejected.
Ordinary numeric/assembler-local labels are allowed. Instruction correctness and truthful effects
remain unsafe obligations; Silk does not attempt instruction semantics or infer syscall behavior.

Raw pointers confer no bounds, ownership, lifetime or initialization proof. Language references,
owned storage handles, tuples as individual lanes, vectors/floats and callbacks are not operands.
Returning assembly must preserve stack and ABI-preserved state and cannot unwind. A caller needing
stack changes must use the constrained naked fragment; implicit alignstack is not admitted.

`with Intrinsic.machine(naked: true, noReturn: true)` is the sole admitted native function-property
combination. It requires an unsafe, monomorphic ordinary zero-parameter unit function, including
an explicitly unsafe C export. The body is exactly one terminal operand-free unit assembly call;
an enclosing return and unsafe acknowledgement are structural only. Reject all locals, ordinary
calls, cleanup, captures, loans, effect functions and additional statements, as well as sanitizer
or instrumentation modes. No stackAlignment override, stack probes, implicit stack realignment,
prologue or epilogue is generated. LLVM naked/noinline/noreturn attributes express the validated
contract; object disassembly independently proves it. A naked export is emitted under the C symbol
directly instead of through a wrapper that would disturb the initial stack.

## Prior art and deliberate differences

Studied Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa `lib/std/os/linux/{x86_64,aarch64}.zig`,
`lib/std/start.zig`, and `test/behavior/asm.zig`. Its syscall0–6 implementations explicitly bind
architecture registers and volatile/memory clobbers. Naked startup operates on the incoming stack.
Silk keeps the numbers and wrapper validation in ordinary source, admits only one word/pointer
result and explicit register constraints, and does not copy Zig's vectors or platform register
breadth. Zig sometimes spells matching fixed input/output registers; Silk requires an explicit tie.

Studied Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84 `compiler/rustc_codegen_llvm/src/asm.rs`,
`tests/assembly-llvm/asm/{x86,aarch64}-types.rs`, and
`tests/codegen-llvm/naked-fn/naked-functions.rs`. Rust separates read/no-memory, volatile, stack and
unwind options and tests real architecture outputs. Its naked implementation emits module assembly
rather than relying on LLVM's naked function facility. Silk deliberately reuses its existing LLVM
function/assembly actors with a stricter zero-operand, one-operation body; LLVM's prohibition on
referencing naked arguments is satisfied structurally. We prove the absence of generated machine
code rather than treating an attribute as proof. Neither implementation is the ABI oracle.

## Pinned authorities and fixtures

LLVM/Clang/opt/llvm-objdump 22.1.8 and the llvmorg-22.1.8 LangRef (SHA256
0f62c10776c9017e8805e561fdfdb780cfb814d3ff509a66ca14f3900c7101c4) govern inline assembly,
constraints, sideeffect, memory, unreachable and naked. The inherited supply record pins
AAPCS64 2025Q1 and x86-64 psABI e1ce098331da5dbd66e1ffc74162380bcc213236, along with the actual
GNU ARM64/x86-64 compiler/container identities. GNU headers come from linux-libc-dev 6.1.180-1.
Additional exact UAPI hashes are in `assembly-supplies.json`. Linux v6.1 architecture entry sources
(`arch/x86/entry/entry_64.S`, `arch/arm64/kernel/entry.S`) were inspected alongside the versioned
Linux userspace/ARM64 ELF documentation. The fixtures use baseline architecture instructions only,
without optional HWCAP features. Header constants belong to independent fixture C, not the compiler.

Designated fixtures: tied arithmetic, raw-pointer load/store, operation-number input with getpid
comparison to independent C, terminal trap control flow, and a naked stack-forwarding fragment
whose independent C probe observes the incoming ABI stack. Compile/inspect debug and optimized
objects on both real target lanes (ARM64 is not a substituted target label). No full startup,
allocator, hosted Darwin service or no-libc executable is required. LTO is rejected before codegen.

## Risks and validation

The largest risk is hidden code introduced around a naked operation or its export. Admission rejects
bodies that would need lowering work; MIR verification checks the exact terminal shape; actual
object disassembly checks entry labels and prologue/spill/probe absence. Call/memory contracts are
also checked structurally and with independent C-visible behavior. Literal/constraint and profile
failures use structured diagnostics at source spans. Source contracts and properties are serialized
through semantic/MIR/artifact identities; no legacy source assembly path exists to preserve.
