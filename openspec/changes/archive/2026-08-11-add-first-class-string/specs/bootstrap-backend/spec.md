## ADDED Requirements

### Requirement: Backends preserve string semantics and presentation

Native LLVM and direct WebAssembly emission SHALL realize the target plan for `string` exactly and
SHALL agree with evaluation on static text, validated runtime views, explicit UTF-8 bytes, byte
length, exact equality, calls, returns, and lexical ownership behavior. Debug builds and compiler
inspection artifacts SHALL retain the logical `string` identity and present valid values as quoted,
escaped Unicode text; byte slices SHALL remain numeric binary views even when their bytes are valid
UTF-8.

#### Scenario: Compare engines on non-ASCII text

- **WHEN** a program passes a non-ASCII `string` through calls and observes its bytes and exact equality
- **THEN** evaluation, native execution, and Wasm execution agree on all results without allocating for the view

#### Scenario: Distinguish text in a debug build

- **WHEN** a debug build contains one `string` local and one `&[u8]` local with identical valid UTF-8 bytes
- **THEN** debug metadata identifies the first as UTF-8 text and the second as binary bytes

#### Scenario: Emit deterministically

- **WHEN** equivalent string-bearing programs are emitted repeatedly for one target and profile
- **THEN** native IR, bitcode, object output, Wasm text, Wasm bytes, static data, and debug metadata are deterministic
