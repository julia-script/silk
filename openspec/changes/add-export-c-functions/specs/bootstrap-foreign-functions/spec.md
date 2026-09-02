## ADDED Requirements

### Requirement: An exported function publishes one C-callable symbol behind a thunk

`[pub] export "C" fn <name>(<parameters>) -> <result> [as "<symbol>"] { <body> }` SHALL declare
an ordinary Silk function that native code may call through the native symbol (the `as` string or
the Silk name) under the ABI named after `export`. Only `"C"` SHALL be accepted. The exported symbol
SHALL name a compiler-generated thunk under the target's C calling convention whose parameters and
result follow the classified C signature; the Silk implementation SHALL keep its private
compiler-versioned symbol and internal ABI, and that symbol SHALL NOT be the exported one. Silk
callers SHALL call the function as an ordinary function without any thunk. `pub` SHALL keep its
Silk module-visibility meaning and SHALL NOT be implied by or imply native export.

#### Scenario: Export a scalar function

- **WHEN** a module declares `export "C" fn silk_test_double_v1(value: i32) -> i32 { return value * 2 }`
- **THEN** the native artifact defines an external C-calling-convention function `silk_test_double_v1` taking and returning `i32`, and a separately compiled C caller receives the doubled value

#### Scenario: Rename an export

- **WHEN** a module declares `export "C" fn double(value: i32) -> i32 as "silk_test_double_v1" { ... }`
- **THEN** Silk calls `double`, the native symbol is `silk_test_double_v1`, and no native symbol named `double` is defined

#### Scenario: Round-trip through C

- **WHEN** Silk calls a foreign C function that itself calls an exported Silk function and returns its result
- **THEN** the executable returns the value computed through both boundaries

#### Scenario: Keep the internal symbol private

- **WHEN** an exported function is emitted
- **THEN** the LLVM module contains the thunk under the export symbol with calling convention property `0` and the implementation under its ordinary compiler symbol, and the thunk's only body is a direct call to the implementation

### Requirement: Exported signatures and contracts follow the foreign admission rules

Each parameter and the result of an exported function SHALL be admitted by the same V1 foreign-ABI
relation as a foreign function, with the same diagnostics at the offending types. An exported
function SHALL NOT declare type parameters, a `where` clause, a failure row, a requirement row,
`effect`, `static`, or `unsafe`, and its body SHALL NOT be classified as suspending; each violation
SHALL be rejected with a diagnostic at the offending syntax or, for suspension, at the declaration
naming the suspending call.

#### Scenario: Reject a non-admitted result

- **WHEN** a module declares `export "C" fn bad() -> string { ... }`
- **THEN** analysis reports the foreign-type-not-admitted diagnostic at `string` and emits no thunk

#### Scenario: Reject an effectful export

- **WHEN** a module declares `export "C" effect fn bad() -> () { ... }`
- **THEN** analysis reports the foreign-declaration-restriction diagnostic at `effect`

#### Scenario: Reject a suspending body

- **WHEN** an exported function's body runs an Effect whose classification is not synchronous
- **THEN** planning reports the export-suspension diagnostic at the declaration and emits no thunk

### Requirement: Exported functions are discovery roots on native targets

Every `export "C"` declaration in the loaded module closure SHALL be an instance-discovery root when
the selected target's kind is native, in addition to the entry, so an export no Silk code calls is
still specialized, verified, and emitted. Exports SHALL NOT replace the entry: a native executable
SHALL still require the ordinary `main`. For a WebAssembly target, under either backend, an
`export "C"` declaration in the loaded closure SHALL be rejected with the
foreign-function-target-unavailable diagnostic naming the symbol and the target. The evaluator,
which runs the native discovery, SHALL report no diagnostic for an export and SHALL expose nothing
through it.

#### Scenario: Compile an uncalled export

- **WHEN** a module declares an exported function that `main` never calls
- **THEN** the native artifact still defines the export's thunk and implementation

#### Scenario: Reject an export for a Wasm target

- **WHEN** a build for `wasm32-unknown-unknown` under either backend loads a module containing an `export "C"` declaration
- **THEN** planning reports the foreign-function-target-unavailable diagnostic naming the symbol and constructs no module

#### Scenario: Ignore an export under the evaluator

- **WHEN** the evaluator runs a program whose closure contains an `export "C"` declaration
- **THEN** execution proceeds from `main` and reports no diagnostic for the export

### Requirement: Export symbols are unique across imports and exports

Within one executable closure, two exported declarations of one symbol SHALL be rejected, and an
exported symbol equal to a foreign import's symbol SHALL be rejected, each with one diagnostic
relating both declarations. An export symbol SHALL obey the foreign-symbol spelling and reservation
rules. The artifact SHALL record every export with its symbol and classified C signature in
deterministic order beside the foreign imports.

#### Scenario: Reject two exports of one symbol

- **WHEN** two loaded modules each declare an export `as "silk_test_v1"`
- **THEN** planning reports the conflicting-foreign-symbol diagnostic at one declaration relating the other

#### Scenario: Reject an export that shadows an import

- **WHEN** one module declares `unsafe extern "C" fn abs(value: i32) -> i32` and another declares `export "C" fn abs(value: i32) -> i32 { ... }`, both reachable
- **THEN** planning reports the conflicting-foreign-symbol diagnostic relating both declarations

#### Scenario: Record exports on the artifact

- **WHEN** a native build defines exports `silk_test_double_v1` and `silk_test_add_v1`
- **THEN** the artifact's export inventory lists both with signatures sorted by symbol
