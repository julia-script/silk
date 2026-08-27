# Native Effect suspension lowering spike

This spike implements OpenSpec tasks 1.5–1.8 without introducing production suspension MIR or a
public `@silklang/llvm` API. Its schema, harness, direct lowering, and LLVM construction are all
local to this directory and are disposable after the native-strategy decision.

The frozen schema has two reached suspension points and one untaken branch. Each reached point
retains one affine owner and one scalar. The shared harness exercises success, unchanged typed
failure, allocation refusal at both ordinals, and private orderly teardown after either checkpoint.

Three independently built variants consume that protocol:

- `direct.c` is an explicit continuation record driven iteratively by the harness.
- `switched.ll` is hand-authored LLVM switched-resume IR. Each Silk boundary maps to a separate
  coroutine ramp, and the harness calls that ramp only after reaching the boundary. The ramp asks
  the selected allocator for `llvm.coro.size` and `llvm.coro.align` before `coro.begin`; refusal
  therefore occurs before publication, affine ownership transfer, or child execution.
- `retcon.ll` is the bounded task 1.9 LLVM returned-continuation construction. Its adapter obtains
  one complete 64-byte request from the same selected allocator after reaching the boundary, keeps
  the returned-continuation pointer in an 8-byte header, and supplies the remaining 56 bytes as the
  coroutine's inline frame. LLVM's fallback allocation/deallocation hooks abort the spike, so a
  transformed frame that outgrows the accepted request cannot silently escape the contract.

Run the executable conformance check with the repository-pinned LLVM 22.1.8 tools:

```sh
node packages/compiler/test/characterization/effect-suspension-native-lowering-spike/verify.mjs
```

Override `SILK_SPIKE_LLVM_BIN`, `SILK_SPIKE_CLANG`, `SILK_SPIKE_LLVM_AS`, or `SILK_SPIKE_OPT` when
the tools are installed elsewhere. The verifier:

1. validates the frozen fixture shape;
2. requires LLVM 22.1.8;
3. compiles and runs the direct reference;
4. assembles the switched and retcon IR, applies
   `coro-early,cgscc(coro-split),coro-cleanup`, and runs the LLVM verifier;
5. confirms CoroSplit produced switched resume/destroy entries and retcon continuation entries,
   while the retcon frame stays within its accepted inline request;
6. compiles and runs both lowered LLVM modules; and
7. requires byte-identical normalized traces across all three variants.

The traces prove one visible complete allocation per reached boundary, no allocation for the
untaken branch, allocator-loan closure before publication, a real child reborrow before child
start, no owner or child for a refused request, ordered inner-to-outer source cleanup followed by
exactly-once frame reclaim, and raw-frame-only harness teardown with no claimed source cleanup.

Run the retained task 1.8 evidence package with:

```sh
node packages/compiler/test/characterization/effect-suspension-native-lowering-spike/evidence.mjs
```

It freezes direct C as LLVM bitcode before timing, feeds all three candidates through identical
`clang -x ir` O0/O2 commands, and records semantic traces, allocation layouts, depth watermarks,
IR and linked-machine call graphs, DWARF symbolization, object/code-data measurements, raw benchmark
samples, exact commands, tool/host facts, and the post-split proof that retcon emitted no fallback
calls and kept every frame access rooted in its selected inline buffer. The compact tracked outputs are `evidence.json` and
`evidence-report.md`; reproducible heavyweight artifacts are generated under ignored `evidence/`.

Task 1.8 proves and measures the candidates. The same evidence pipeline includes the bounded task
1.9 returned-continuation experiment and generates `selection-report.md`. Both LLVM candidates
showed no material advantage and a material allocator-visible frame regression, so task 1.9 selects
direct iterative lowering.
