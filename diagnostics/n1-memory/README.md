# N0/N1 memory and use-after-free diagnostics

Diagnostic branch only. Do not merge. These scripts measured N0's memory while building N1, and
found the N1 `Shared.with` trap (a double release of consuming-pattern bindings that N0 lowers
as copies). Paths are relative to this directory. Scripts that write output use `out/` and `shim/`
next to themselves. Build N0 with the default recipe first (LLVM 22.1.8 first on `PATH`).

| File | Use |
| --- | --- |
| `run.sh <N0> <entry.silk> <label> [limitKB]` | Build one entry with `--emit llvm-ir`, sample RSS every `INTERVAL` s (default 15), kill above the limit, append `label exit peak secs` to `results.txt`. |
| `count2.sh <N0> <src> <label> [limitKB]` + `count2.py` | Run N0 under gdb and count `Semantic.storeMir` (MIR instances) and `buildFrom` passes. Every `EVERY` MIR it writes `mir= pass= rss= t=`. Compare two compilers at an equal MIR count, on the same input source: compiler-source edits change the N1 instance set. |
| `malloc-shim.c` | LD_PRELOAD. Tags each block with its caller. `SIGUSR1` writes live bytes per caller to `/tmp/n1-shim-dump.<pid>`. |
| `malloc-shim-large.c` | Same, and also logs every allocation of 512 KB or more to `/tmp/n1-shim-big.<pid>`. |
| `shimrun.sh <N0> <src> <label> <thresholdsKB...>` / `shimend.sh <src> <label>` | Drive the shim: dump at RSS thresholds, or at each new peak. |
| `resolve.py <dir> <label> <dump...>` | Resolve shim callers against `nm -n` of the binary plus the PIE base from `shim/<label>.maps`. Prints growth per allocating instance (element types decoded from the mangled name). |
| `uaf-shim.c` | LD_PRELOAD use-after-free probe. Freed blocks are never reused. With `UAF_POISON=1`, their payload becomes `0xA5`. Each freed header keeps a `FREED` magic and a 12-frame `backtrace()` of the free. If the program runs cleanly under it but traps without it, suspect use-after-free. |
| `uafscan2.py` | gdb script. At the first trap it scans frames 1 to 3 for pointers into freed blocks and prints each block's allocation site and free-time stack. |
| `gen.py <kind> <n>` + `syn.sh <kind> <n...>` + `measure.py` | Synthetic scaling programs (`calls`, `funcs`, `drops`, `runs`, `plain`, `generics`, `arith`) with peak RSS, time and IR size per size. `runs` and `drops` show quadratic exit cleanup. |
| `ircmp.sh <N0a> <N0b> <tag>` | Emit LLVM IR for every `examples/` program with both compilers and require byte-identical IR and stderr. |

N0 and N1 keep CFI, so gdb and `backtrace()` unwind them, but their symbols are content hashes. To
symbolize, emit the producing build's IR (`--emit llvm-ir`) and read the callees around a call
site. Effect-function instances in N0 carry readable `silk_<module>_<name>` local symbols.
