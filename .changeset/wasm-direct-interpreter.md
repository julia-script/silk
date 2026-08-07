---
'@silk-effect/wasm': patch
---

Add `Interp`: a direct interpreter over committed module state with first-class debugging.
Execution uses an explicit call stack and per-block program counters, so a session can pause
between any two instructions — step, step over, step out, breakpoints on `Instr` values, and
inspection of frames, locals, value stacks, globals, and memory. Covers the full scalar
instruction set (numeric, control flow, calls including tail calls, memory, tables, segments,
references) with host imports for functions and globals; differentially verified against native
WebAssembly across every scalar operator. SIMD, atomics (except `atomic.fence`), GC, and
exception handling trap as unsupported.
