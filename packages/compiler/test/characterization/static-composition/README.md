# Static-composition tree characterization

This suite generates distinct nominal command leaves, per-leaf transforms, one normalized
application-action union, and named internal nodes in left-associated and balanced trees. It
analyzes and emits each `1`, `8`, `32`, `64`, and `128` leaf program for LLVM and direct WebAssembly
in two fresh Node processes. The test hard-gates canonical source, semantic/layout/MIR counts, and
LLVM-bitcode/Wasm hashes; phase time and phase-boundary sampled heap are noisy empirical
observations.

Baseline environment: macOS arm64, Node v26.7.0. The sample below was recorded on 2026-08-16. Phase
time is the sum of compiler phase reports and excludes target emission; heap is the maximum sample
taken before and after canonicalization, target analysis, and target emission. Later balanced cases
can therefore inherit allocations from earlier cases in the same process.

| shape | leaves | source bytes | semantic occurrences | layouts | MIR ops | phases ms | sampled heap | LLVM bitcode | Wasm bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| left | 1 | 443 | 38 | 3 | 12 | 34.3 | 40,211,960 | 1,596 | 1,101 |
| left | 8 | 2,443 | 241 | 25 | 34 | 13.6 | 43,306,632 | 3,652 | 3,217 |
| left | 32 | 12,036 | 937 | 97 | 106 | 152.3 | 58,404,128 | 10,800 | 12,932 |
| left | 64 | 36,052 | 1,865 | 193 | 202 | 1,440.5 | 87,525,384 | 20,256 | 31,397 |
| left | 128 | 116,348 | 3,721 | 385 | 394 | 19,541.2 | 131,952,496 | 39,816 | 87,656 |
| balanced | 1 | 443 | 38 | 3 | 12 | 16.7 | 94,155,920 | 1,652 | 1,153 |
| balanced | 8 | 2,306 | 237 | 25 | 34 | 9.0 | 97,045,216 | 3,820 | 3,337 |
| balanced | 32 | 9,090 | 911 | 97 | 106 | 52.6 | 108,614,312 | 11,352 | 11,278 |
| balanced | 64 | 18,295 | 1,808 | 193 | 202 | 170.9 | 132,226,264 | 21,320 | 22,287 |
| balanced | 128 | 37,252 | 3,601 | 385 | 394 | 621.1 | 183,770,648 | 41,904 | 45,584 |

```text
semantic occurrences (both shapes)
1      38
8      241      ####
32     937      ################
64     1865     ################################
128    3721     ################################################################
```

## Findings and thresholds

- Semantic occurrences, layout entries, and MIR operations remain linear. The checked thresholds
  are respectively `40n + 16`, `5n + 4`, and `10n + 8`; the small shape difference in semantic
  occurrences comes from the selected leaf's access path.
- LLVM bitcode size is shape-independent and approximately linear over this range.
- Balanced Wasm size is approximately linear. Left-associated Wasm size and compilation time grow
  substantially faster because each outer aggregate materializes a progressively larger nested
  value. This is an accepted empirical compiler cost, not semantic expansion, erasure, a runtime
  dictionary, or an indirect call. It is retained as pressure debt for backend aggregate lowering.
- Formatter indentation makes left-associated canonical source bytes approximately quadratic; the
  generator uses named node types so it does not also duplicate nested type expressions.
- Timing and heap are observations, not gates. A future stable benchmarking environment should add
  target-emission time/heap thresholds after addressing left-associated aggregate lowering.

Run from `packages/compiler` after building the package:

```sh
node test/characterization/static-composition/characterize.mjs
```
