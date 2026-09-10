## Context

See proposal.md for motivation. The parser currently has fixed parameter lists; declaration facts
retain foreign symbols/contracts, CAbiSignature classifies fixed parameters, and MIR ForeignCall
carries that signature. NativeProgram declares each foreign symbol once and creates a fatal-unwind
guard. The LLVM Type.functionType operation already supports `variadic`; reuse it.

## Goals / Non-Goals

Preserve declaration identity separately from each call's promoted operands. Support the smallest
true native C ellipsis subset that admits source-owned open/openat policy. Existing fixed scalar and
pointer calls retain their rules. Variadic callback types/indirect calls, definitions/va_list, bool,
char, floating-point and pointer/aggregate tails are not admitted here.

## Decisions

### Syntax and admission

A foreign declaration ends its parameter list with `, ...`, after at least one fixed runtime
parameter. Ellipsis has no parameter name/type, must be last, and is not a Silk parameter binding.
Preserve its token/span in the lossless syntax tree; formatter and presentations retain it. Reject
ellipsis on ordinary/exported functions and function-pointer types. Keep foreign declaration phase,
unsafe, body, generics and contract restrictions unchanged.

Each call must supply all fixed operands, then zero or more admitted integers. Fixed operands keep
ordinary contextual typing; tail operands have no callee-provided type. Ordinary literal typing
still applies (an unannotated integer defaults to i32); callers use typed bindings/conversions for
other widths. Admit i8/u8/i16/u16/i32/u32/i64/u64/isize/usize only. C int is signed 32-bit on all three
admitted targets: both signed and unsigned 8/16-bit values promote to i32, preserving their values.
32/64-bit and pointer-width integer values retain width and signedness. Do not promote an unsigned
16-bit mode to u32: Darwin's mode_t is u16 and promotes to int; GNU's mode_t is u32 and remains u32.
No symbol-name special case, flag policy, or open/openat argument-count policy enters the compiler.

### Identity and call planning

A required variadic boolean plus the fixed parameter array describe each C declaration and enter
signature agreement, manifest encode/decode, inspection and canonical identity. Tail types never
alter that signature. Each reachable call separately records its complete promoted ABI argument
list and source-to-promoted integer conversion. Instance and MIR validation check fixed count,
admitted tails, canonical promotion and the unchanged foreign contract. Unsupported categories
produce source diagnostics before native lowering, including inactive-selection behavior inherited
from selected-source analysis.

ABI schema 4 combines the synchronous-callback contract from schema 3 with required variadic status; obsolete schemas are rejected. Fixed callback parameters retain their synchronous invocation contract, while unnamed callback operands remain unadmitted. Fixed indirect calls keep their own signature and fatal-unwind guard.

The ABI manifest keeps one symbol/signature per declaration. MIR inspection exposes call operands
and promotions. Changing tail arity/types creates a different call shape, not a conflicting native
redeclaration. No compatibility default hides the new required field in serialized ABI input.

### LLVM and foreign contracts

Declare the actual external symbol once with its fixed parameters and LLVM variadic flag. Use the
LLVM target's C lowering for placement: unnamed Darwin ARM64 integer arguments use 8-byte stack
slots; GNU AAPCS64 uses remaining general registers followed by stack; System V x86-64 uses remaining
integer registers then stack and the ABI's vector-register count convention (zero for integer tails).
Integer extension to the promoted type happens before the true variadic call. No function-pointer
cast or fake fixed declaration can stand in for that call.

Keep the existing fatal-unwind enforcement. A guard is keyed by symbol plus complete promoted call
shape, has fixed internal operands for that call, and invokes the real variadic declaration with
those operands. Fixed parameter attributes and conservative memory/retention semantics remain
attached to the real declaration/invoke. Tail integers grant no new pointer/borrow permissions.
The guard is existing unwind enforcement, not a generated C adapter or alternate ABI declaration.

### Authorities, prior art and fixtures

Pin LLVM/Clang/lld22.1.8, Darwin SDK15.5/deployment11.0.0 and GNU glibc2.36-9+deb12u14 /
GCC12.2.0-14+deb12u1 / Linux headers6.1.180-1 from JUL-126. `supplies.json` records exact authorities
and relevant header hashes before code changes. C N1570 sections6.3.1.1 and6.5.2.2 govern integer
promotions and the fixed boundary; Apple ARM64, AAPCS64 2025Q1, pinned x86-64 psABI and LLVM LangRef
22.1.8 govern placement. Real C compilers and ABI/object inspection are the correctness oracles.

Read Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa std/c.zig open/openat declarations,
test/c_abi/main.zig+cfuncs.c and behavior/var_args.zig, plus variadic_arg_validation.zig. The c_abi
varargs test is Win64-specific, not an oracle for our targets. Behavior tests cover integer va_arg
and zero/additional arguments but skip LLVM GNU ARM64; Silk's required ARM64 fixture must not skip.
Zig's explicit literal-width validation informs rejection boundaries; Silk retains its own existing
integer literal typing and performs specified C integer promotions automatically.

Read Rust c33d8f3b5a50b56466998e8c5ed8a077d2caed84 callconv/x86_64.rs+aarch64.rs,
hir_typeck/fn_ctxt/checks.rs, ui/error-codes/E0617.rs, ui/c-variadic/roundtrip.rs and
assembly-llvm/c-variadic/aarch64.rs read_i32/read_i64. Rust preserves fixed_count/c_variadic and
relies on LLVM caller placement; it rejects narrow values and asks for explicit casts. Silk adopts
separate fixed/call identity but deliberately performs C promotions. Rust aggregate-passing tests
are not variadic evidence. Neither implementation proves Silk's ABI or replaces the C fixtures.

A separately compiled C receiver uses va_arg(int/unsigned int/int64_t/uint64_t) with signed narrow,
unsigned narrow, high unsigned32, 64-bit and enough tail operands to overflow available registers.
Zero-tail and multiple shapes of one symbol distinguish declaration identity from call identity.
A minimal selected Silk fixture calls SDK-declared open/openat both without creation mode and with
an integer mode, then closes/removes the temporary files. C supplies test data/verification only;
it must not adapt or make the variadic call for Silk. No full JUL-131 provider migration is needed.

## Risks / Trade-offs

- Mistaking a tail operand for a fixed operand → carry the marker to LLVM and inspect real calls
  and Darwin stack placement; execute independent C receivers on all three targets.
- Reusing a fixed guard for another call shape → key guards by complete promoted shape and verify
  zero/multiple/different-width calls to the same native symbol.
- Contract drift → keep fixed borrow ordinals and conservative memory/retention/unwind checks;
  MIR validation rejects noncanonical per-call promotions.
- Broad C varargs surface → reject unsupported categories and declarations before lowering.

## Migration Plan

Add syntax/semantic admission and diagnostics, update all C signature producers/consumers and
serialized manifests together, then implement per-call MIR and native lowering. Update generated
catalogs/docs/goldens, required pinned CI and conformance records. Run all gates and publish above
JUL-126 through gh stack. No old fixed-signature variadic path or deferred cleanup remains.
