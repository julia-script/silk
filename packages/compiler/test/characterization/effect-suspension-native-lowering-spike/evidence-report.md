# Native suspension lowering evidence

Status: **PASS for OpenSpec tasks 1.8 and 1.9**. Selected strategy: **direct iterative state machine**.

- LLVM: Homebrew clang version 22.1.8
- Target: `aarch64-apple-darwin`
- Coroutine pipeline: `coro-early,cgscc(coro-split),coro-cleanup`
- Semantic/allocation/cleanup parity: pass
- Constant-stack watermark at depths 1, 1,000, and 100,000 under O0/O2: pass
- Direct and indirect call-cycle audit: pass
- Residual coroutine structure-intrinsic audit: pass
- Retcon fallback-call and selected-inline-buffer-root audit: pass
- Static DWARF and synthetic Silk boundary symbolization: pass
- Second normalized semantic/depth replay: pass

## Measured values

| metric                         |   direct | switched |   retcon |
| ------------------------------ | -------: | -------: | -------: |
| O0 compile median (ms)         |   31.120 |   33.046 |   32.351 |
| O2 compile median (ms)         |   32.598 |   36.851 |   36.119 |
| O2 resume median (ms/boundary) | 0.001133 | 0.001150 | 0.001144 |
| frame bytes at boundary 1      |       24 |       40 |       64 |
| linked O2 code/data bytes      |     6396 |     6572 |     6662 |

The generated [selection report](selection-report.md) applies every threshold and records the rejection reason for both LLVM candidates. Raw samples, MADs, exact commands, stack results, call graphs, frame layouts, semantic traces, and debug symbolization are retained in [evidence.json](evidence.json).

Run:

```sh
node packages/compiler/test/characterization/effect-suspension-native-lowering-spike/evidence.mjs
```
