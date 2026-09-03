## REMOVED Requirements

### Requirement: Foreign functions are native-only and pay-for-use

**Reason**: Evaluator host bindings and direct WebAssembly imports now provide explicit foreign-call
execution models beyond native linking.

**Migration**: Supply a symbol-keyed host-function table when evaluating a reachable foreign call,
or instantiate the direct WebAssembly module with its declared foreign imports.

## ADDED Requirements

### Requirement: Foreign functions use explicit reachability-based bindings on every supported surface

Foreign calls SHALL remain pay-for-use: an unreachable declaration SHALL contribute no binding,
diagnostic, or artifact import. A reachable call SHALL use exactly one surface-specific binding:
native and LLVM-native artifacts SHALL retain a direct external symbol for the linker, evaluator
execution SHALL require an explicit per-evaluation host function keyed by symbol, and the direct
WebAssembly backend SHALL emit a function import whose field is the symbol. LLVM emission for a
WebAssembly target SHALL remain unavailable until that backend has an explicit foreign binding
model. No evaluator or backend SHALL provide an implicit compiler-owned libc symbol set.

Before evaluator execution, every reachable symbol SHALL be bound and its declared host signature
SHALL exactly equal the reachable C-ABI signature. Missing and mismatched bindings SHALL block
evaluation before any operation runs and SHALL name the symbol and expected signature. A host call
failure SHALL produce a symbol-specific blocked outcome rather than escape as an untyped exception.

#### Scenario: Ignore an unreachable foreign declaration

- **WHEN** a program declares a foreign function that its executable closure never calls
- **THEN** evaluation and direct WebAssembly emission succeed without a binding or import for the symbol

#### Scenario: Evaluate through an exact host binding

- **WHEN** a reachable `abs(i32) -> i32` call has a per-evaluation host binding for `abs` with the exact classified C signature
- **THEN** evaluation invokes that binding and returns its result

#### Scenario: Block an unbound evaluator symbol

- **WHEN** evaluation reaches a closure requiring `abs` and its host table has no `abs` binding
- **THEN** evaluation starts no operations and returns a blocked reason naming `abs` and its expected signature

#### Scenario: Block a mismatched evaluator signature

- **WHEN** evaluation requires `abs(i32) -> i32` but the `abs` host binding declares `(i64) -> i64`
- **THEN** evaluation starts no operations and returns a blocked reason naming `abs`, the expected signature, and the supplied signature

#### Scenario: Emit a direct WebAssembly foreign import

- **WHEN** a direct-WebAssembly build reaches `abs(i32) -> i32`
- **THEN** its module imports one function from the versioned Silk foreign-host module under field `abs` with one `i32` parameter and one `i32` result

#### Scenario: Reject a reachable foreign call under LLVM wasm32

- **WHEN** the LLVM backend is asked to emit `wasm32-unknown-unknown` for a program whose closure calls a foreign function
- **THEN** planning reports the foreign-function-target-unavailable diagnostic naming the symbol and target and emits no bitcode

#### Scenario: Record reachable imports on artifacts

- **WHEN** a native or direct-WebAssembly build reaches `abs` and `silk_test_add`
- **THEN** its foreign-import inventory lists both symbols with their C signatures in deterministic order and nothing else
