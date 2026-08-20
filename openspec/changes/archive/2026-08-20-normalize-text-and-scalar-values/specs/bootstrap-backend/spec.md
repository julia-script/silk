## MODIFIED Requirements

### Requirement: Backends preserve string semantics and presentation

Native LLVM and direct WebAssembly emission SHALL realize the target plan for `string` exactly and
SHALL agree with evaluation on static text, validated runtime views, ordinary references to string
values, explicit UTF-8 bytes, byte length, `char` traversal, checked scalar conversion, exact
equality, calls, returns, and lexical ownership behavior. Debug builds and compiler inspection
artifacts SHALL retain the logical `string` and `char` identities and present valid string values as
quoted, escaped Unicode text; byte slices SHALL remain numeric binary views even when their bytes
are valid UTF-8.

#### Scenario: Compare engines on non-ASCII text

- **WHEN** a program passes a non-ASCII `string` through calls, traverses its scalars, and observes its bytes and exact equality
- **THEN** evaluation, native execution, and Wasm execution agree on all results without allocating for the view

#### Scenario: Reject invalid scalars identically

- **WHEN** checked scalar conversion receives surrogate and above-range integers
- **THEN** native and Wasm return the same `None` outcomes as evaluation

#### Scenario: Distinguish text in a debug build

- **WHEN** a debug build contains one `string` local and one `&[u8]` local with identical valid UTF-8 bytes
- **THEN** debug metadata identifies the first as UTF-8 text and the second as binary bytes

#### Scenario: Emit deterministically

- **WHEN** equivalent string-bearing programs are emitted repeatedly for one target and profile
- **THEN** native IR, bitcode, object output, Wasm text, Wasm bytes, static data, and debug metadata are deterministic
