## Context

See `proposal.md` for motivation and the `bootstrap-language-pressure-programs` delta for observable requirements. The lexer is the first complete pressure program and reports three deferred seams: numeric codes in place of a closed named vocabulary, `Vector.get` requiring exclusive access for Copy reads, and visible linear/growth costs. The VM must test those seams independently rather than treating the lexer report as its design specification.

Silk currently has runtime byte slices, fixed arrays, mutable indexing, integer operators, loops and branches, typed effects, self-contained allocation, and the source-shipped `silk.vector`. `Vector` has append, length, capacity, and a Copy-only `get` that takes `&mut Vector<T>`; it has no pop operation. The test bridge cannot directly return a dynamic Silk vector to TypeScript.

## Goals / Non-Goals

**Goals:**

- Keep one complete VM program small enough to audit and familiar enough that its semantics are unsurprising.
- Exercise runtime dispatch, backward/forward jumps, bounded failure recovery, fixed mutable storage, owned dynamic observations, and allocation rollback.
- Compare exact semantic observations against an independent reference without adding a dynamic collection ABI.
- Decide whether repeated lexer findings deserve separate language or stdlib proposals.

**Non-Goals:**

- A production VM, assembler, JIT, general bytecode format, or compiler execution engine.
- Adding enums, constants, Vector mutation operations, iterators, Logger, filesystem input, or VM intrinsics in advance.
- Using the VM as a step toward self-hosting or porting the parser.
- Benchmarking bootstrap Silk against a tuned interpreter implementation.

## Decisions

### D1: Use a bounded fixed operand stack and allocated owned observations

The operand stack is a fixed `[i32; 16]` with an explicit stack pointer. This exercises ordinary mutable indexed storage and makes overflow a deterministic VM diagnostic. The owned result contains one `Vector<Step | VmDiagnostic>` in execution order so realistic execution and recovery still allocate, grow, migrate, and clean up. This is also the more faithful observation model: a diagnostic and a successful step are events in one timeline, not two collections whose relative order must be reconstructed.

The first implementation returned separate trace and diagnostic vectors. Native cleanup received an invalid union tag for the second generic vector after the composite result crossed the flattened ABI, although evaluator and Wasm execution were sound. Keeping one ordered event stream makes the pressure program executable without hiding that general compiler defect; the findings retain it as focused follow-up evidence.

Extending `Vector` with pop/set before the program exists was rejected because it would turn a pressure probe into a preselected stdlib design. An append-only fake operand stack was rejected because it would not implement familiar stack semantics honestly.

### D2: Define a compact local bytecode with numeric opcode values

Use a small instruction set: halt, push-immediate, add, subtract, multiply, duplicate, conditional jump, and unconditional jump. Operands are following bytes; jump targets are absolute byte offsets. Arithmetic uses ordinary trapping `i32` behavior, while malformed bytecode produces VM diagnostics. Local `u8` codes deliberately mirror the lexer's pre-enum constraint so the findings can compare navigation and maintenance costs.

### D3: Bound execution and make malformed recovery explicit

Execution stops on halt, end of input, truncated operands, invalid jump targets, fixed-stack overflow, or a 64-step limit. Unsupported opcodes and stack underflow append diagnostics and advance to the next byte so one program can exercise diagnostic-vector growth and recovery. The TypeScript reference owns the same rules and is the differential oracle.

This distinguishes malformed-program semantics from Silk traps: only valid arithmetic overflow remains a language-level trap. An unbounded loop was rejected because tests and compiled executables need deterministic termination.

### D4: Record exact observations as events are appended and use fingerprints for compiled engines

The Silk program calls named observation functions for result, each step field, and each diagnostic field at the point the event is appended. Tests read those evaluator trace calls for exact differential comparison. The program incrementally computes a bounded deterministic fingerprint consumed by native and Wasm gates. Reading the union event stream back through `Vector.get` was rejected after the general `Slot.copy` lowering produced invalid MIR for structural-union elements. Append-time observation preserves exact order and ownership pressure without adding a host ABI for dynamic vectors or pretending the union-copy defect is solved.

### D5: Sweep successful allocation ordinals and compare findings explicitly

One program interleaves enough successful and malformed execution events to grow the event vector through multiple allocations. Successful baselines establish the exact allocation ordinals; quota providers then fail each ordinal on evaluator, native, and Wasm, requiring typed failure and balanced acquire/release traces. The findings report includes a comparison column pointing back to lexer evidence.

## Risks / Trade-offs

- **[Risk] Numeric opcodes make the source needlessly cryptic.** → Centralize their meanings in the reference mapping and record navigation cost as evidence; do not solve it inside the VM.
- **[Risk] A fixed stack avoids allocating the data structure named by the example.** → State the bound visibly and allocate the semantically useful ordered event result; the goal is language pressure, not an unbounded production VM.
- **[Risk] Evaluator trace extraction couples tests to observation function names.** → Limit extraction to explicit public observation calls and independently verify compiled fingerprints.
- **[Risk] Failure sweeps multiply already-expensive cross-engine tests.** → Use two representative programs, derive ordinals from baselines, and keep the broader corpus evaluator-only.
- **[Risk] The program independently confirms a missing language/library feature.** → Complete with the honest local representation when possible, record both programs' evidence, and make any general solution a subsequent focused proposal.

## Migration Plan

This is additive. Land the visible VM, TypeScript oracle, acceptance/determinism gates, and findings together. Rollback removes those artifacts and any small general compiler repair discovered during implementation; no production compiler path or persisted format depends on the VM.
