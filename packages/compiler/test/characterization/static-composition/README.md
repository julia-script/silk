# Static-composition tree characterization

This suite generates distinct nominal leaves and named internal nodes in left-associated and
balanced trees. It analyzes and emits each `1`, `8`, `32`, `64`, and `128` leaf program for native
LLVM and direct WebAssembly in two fresh Node processes. The test hard-gates canonical source,
semantic/layout/MIR counts, and native/Wasm hashes; phase time and peak heap are recorded as noisy
empirical observations.

Baseline environment: macOS arm64, Node v26.7.0. The sample below was recorded on 2026-08-16. Phase
time is the sum of compiler phase reports and excludes target emission; heap is the process-wide
high-water observation after each case, so later balanced cases inherit earlier allocations.

| shape | leaves | source bytes | semantic occurrences | layouts | MIR ops | phases ms | peak heap | native bytes | Wasm bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| left | 1 | 105 | 8 | 2 | 5 | 19.5 | 27,049,136 | 416 | 62 |
| left | 8 | 1,092 | 99 | 16 | 26 | 14.6 | 40,068,128 | 436 | 288 |
| left | 32 | 7,075 | 411 | 64 | 98 | 90.1 | 48,420,648 | 504 | 3,505 |
| left | 64 | 26,291 | 827 | 128 | 194 | 945.9 | 74,788,040 | 584 | 13,361 |
| left | 128 | 96,819 | 1,659 | 256 | 386 | 12,923.4 | 125,691,640 | 756 | 51,571 |
| balanced | 1 | 105 | 8 | 2 | 5 | 0.8 | 129,073,896 | 420 | 62 |
| balanced | 8 | 975 | 99 | 16 | 26 | 2.8 | 130,952,504 | 440 | 244 |
| balanced | 32 | 4,281 | 411 | 64 | 98 | 21.3 | 128,300,000 | 508 | 1,303 |
| balanced | 64 | 8,841 | 827 | 128 | 194 | 76.0 | 141,855,480 | 588 | 3,191 |
| balanced | 128 | 18,345 | 1,659 | 256 | 386 | 316.7 | 143,586,320 | 760 | 7,415 |

```text
semantic occurrences (both shapes)
1      8
8      99       ####
32     411      ################
64     827      ################################
128    1659     ################################################################
```

## Findings and thresholds

- Semantic occurrences, layout entries, and MIR operations are shape-independent and linear. The
  checked thresholds are respectively `14n`, `2n`, and `4n + 2`.
- Native bitcode size is nearly shape-independent and sublinear over this range.
- Balanced Wasm size is approximately linear. Left-associated Wasm size and emission time grow
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
