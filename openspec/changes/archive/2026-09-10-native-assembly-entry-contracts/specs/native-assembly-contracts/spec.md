## ADDED Requirements

### Requirement: Typed sealed assembly boundary

Silk SHALL expose `Intrinsic.assembly<Result>(template, constraints, clobbers, memory,
sideEffects, noReturn, inputs)` as an unsafe residual primitive. The first six arguments SHALL be
literal compiler metadata; only the final tuple SHALL enter runtime lowering. Result SHALL be unit,
a 64-bit integer/usize/isize, or an admitted data pointer; tuple inputs SHALL contain zero through
seven such non-unit lanes. References, aggregates, floats, vectors and narrower/wider integers SHALL
be rejected. Assembly SHALL be unavailable in static execution and outside GNU/Linux x86-64/ARM64.

#### Scenario: Fixed and tied registers

- **WHEN** selected x86-64 source supplies `={rax},0,{rdi}` for one result and two compatible inputs
- **THEN** the first input is tied to the output and the second occupies rdi
- **AND** missing output, duplicate input registers, conflicting clobbers and malformed ties diagnose.

### Requirement: Explicit machine effects and storage obligations

The compiler SHALL validate one optional fixed-register output, optional early-clobber output,
fixed input registers or output tie `0`, and an explicit clobber list. Memory SHALL be `none`, `read`,
`write` or `readwrite`. Side effects and no-return SHALL be explicit literal booleans. No-return
SHALL require unit result and side effects, terminate MIR control flow, and emit LLVM unreachable.
Returning assembly SHALL preserve the stack, ABI-preserved registers and undeclared machine state;
the unsafe caller SHALL establish pointer validity, access, aliasing, lifetime and truthful effects.
Assembly SHALL NOT unwind, retain language borrows or grant pointer initialization implicitly.

#### Scenario: Observable store

- **WHEN** assembly declares a raw-pointer store and memory write effects
- **THEN** optimized native output retains the store and an independent C consumer observes it
- **AND** a false no-memory claim remains a violated unsafe source contract, not a compiler proof.

### Requirement: Constrained naked entry body

A `with Intrinsic.machine(naked: true, noReturn: true)` declaration SHALL be an unsafe monomorphic
ordinary zero-parameter unit function. Its body SHALL contain exactly one terminal assembly call
with no runtime inputs, unit result, side effects and no-return. Only the enclosing return/unsafe
acknowledgement is admitted. Locals, ordinary calls, branches, cleanup, captures, loans and values
requiring spills SHALL be rejected. Instrumentation, automatic stack realignment, stack probes,
prologue/epilogue generation and incompatible profile modes SHALL be rejected rather than inferred
safe from a naked attribute. A naked C export SHALL name the naked function itself, without a shim.

#### Scenario: Initial stack preserved

- **WHEN** a valid naked entry fragment reads the incoming stack and branches to an explicit symbol
- **THEN** actual target disassembly begins with the authored operation and contains no compiler
  prologue, epilogue, spills, probes or instrumentation in debug and optimized builds
- **AND** a body with an ordinary local or call is rejected before emission.

### Requirement: Bounded native conformance and identity

Contracts, targets and properties SHALL participate in semantic/MIR/artifact identity. The source
facility SHALL reuse LLVM assembly constants, callAssembly and function attributes. Each designated
GNU/Linux architecture SHALL emit and inspect actual debug/optimized objects with pinned tools;
missing supplies or skipped lanes SHALL fail conformance. LTO SHALL be explicitly rejected. Minimal
independent execution SHALL prove only distinguishing machine/ABI behavior, not JUL-136 startup.

#### Scenario: Required native matrix

- **WHEN** the conformance runner executes
- **THEN** both x86-64 and ARM64 objects are compiled and inspected with pinned LLVM and GNU supplies
- **AND** accepted register, pointer, effect, terminal control-flow and naked-entry cases are verified.
