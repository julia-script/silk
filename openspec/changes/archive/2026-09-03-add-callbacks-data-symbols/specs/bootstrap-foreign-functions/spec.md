## ADDED Requirements

### Requirement: C callbacks are exact noncapturing export addresses

Silk SHALL define `extern "C" fn(P...) -> R` as a C function-pointer type admitted only when every
parameter and result is admitted by the C ABI. A named, nongeneric, synchronous `export "C" fn`
item SHALL contextually convert to that type only when its classified signature is exact. Ordinary
functions, effect or suspending functions, generic functions, and capturing callables SHALL NOT
convert.

#### Scenario: Pass an exported comparator to qsort

- **WHEN** an exact synchronous exported comparator is passed to a foreign `qsort` declaration
- **THEN** native execution passes the comparator's C-callable address and C invokes the Silk thunk

#### Scenario: Reject a nonexported callback

- **WHEN** an ordinary private function is used where a C function pointer is required
- **THEN** semantic analysis reports that only an exact `export "C" fn` is addressable

#### Scenario: Reject a suspending or capturing callback

- **WHEN** an effect/suspending function or capturing anonymous callable is used as a C callback
- **THEN** semantic analysis reports the unsupported callback form at the conversion site

### Requirement: Foreign data symbols are immutable native bindings

An `unsafe extern "C" static` declaration SHALL bind one external native data symbol and reading it
SHALL load a value of its declared C-admitted type. An `export "C" static` declaration SHALL publish
one initialized native data symbol of its declared C-admitted type. The binding itself SHALL be
immutable; pointee mutability remains expressed by its pointer type.

#### Scenario: Read the process environment symbol

- **WHEN** native source reads an imported `environ` data symbol declared as `*mut *mut u8`
- **THEN** it receives the host process's environment-vector address

#### Scenario: Publish an ABI version symbol

- **WHEN** a native library exports `silk_abi_version: u32 = 1`
- **THEN** separately compiled C code links the data symbol and reads the value `1`

### Requirement: Callback and data-symbol reachability is native-only and pay-for-use

Only reachable callback conversions and data-symbol reads SHALL contribute native declarations or
availability requirements. The evaluator and direct WebAssembly SHALL reject a reachable callback
or data-symbol operation with the foreign-surface diagnostic and SHALL ignore unreferenced
declarations.

#### Scenario: Ignore unreferenced advanced foreign declarations

- **WHEN** a program declares but never uses a C callback type or foreign data symbol
- **THEN** evaluator and direct-WebAssembly execution remain available without bindings

#### Scenario: Reject a reachable data load outside native

- **WHEN** evaluator or direct WebAssembly reaches a foreign static read
- **THEN** compatibility analysis reports the symbol and unsupported execution surface before work
  begins
