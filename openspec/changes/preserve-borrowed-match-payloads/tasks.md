## 1. Canonical nominal union storage

- [x] 1.1 Derive stored union payload size and alignment from canonical variant aggregate layouts
      and map calling lanes through those field offsets; verify differently sized variants retain
      their ordinary offsets and existing call/return and cleanup evidence uses the same target plan.
- [x] 1.2 Remove superseded carrier-memory materialization and writeback paths; update structural
      layout expectations and the language reference to describe direct active-payload addresses.

## 2. Borrowed pattern aliases

- [x] 2.1 Lower shared and exclusive matches from their original scrutinee place and represent
      pattern bindings as borrowed descriptors; verify projected fields, ordinary reads, reborrows,
      scoped captures, and suspension preserve the original payload without relaxing ownership errors.
- [x] 2.2 Add economical structural and shared-corpus regression evidence for mutation through a
      borrowed payload and later owner publication; preserve the owned TLS native and Wasm audit
      predicates and remove temporary investigation programs.
- [x] 2.3 Retain nested variant tests and canonical field paths through matching and lowering;
      distinguish inner-variant mismatches before bindings and guards, preserve source-order
      fallback, and recognize exhaustive nested alternatives without consuming an outer variant early.

- [x] 2.4 Carry private native aggregate results and failure metadata through caller-owned storage;
      keep suspension status separate, update all call/return/resume paths and foreign adapters,
      and cover result transport with structural and existing shared runtime evidence.
