## ADDED Requirements

### Requirement: Source syntax distinguishes C function pointers from Silk callables

The parser SHALL accept `extern "C" fn(P...) -> R` wherever a declared type is accepted, SHALL
retain the ABI marker and parameter/result syntax, and SHALL recover a missing or unsupported ABI
without interpreting the type as an ordinary Silk callable.

#### Scenario: Parse a C function-pointer parameter

- **WHEN** a declaration contains `compare: extern "C" fn(*const T, *const T) -> i32`
- **THEN** syntax facts preserve one C function-pointer type with two parameters and result `i32`

#### Scenario: Recover an unsupported callback ABI

- **WHEN** a function-pointer type uses an ABI other than `"C"`
- **THEN** parsing retains the declaration and reports the ABI at its own span

### Requirement: Source syntax defines foreign and exported static data

The parser SHALL accept `unsafe extern "C" static name: T` with an optional `as "symbol"` tail and
`export "C" static name: T = initializer`. A foreign static SHALL have no initializer, an exported
static SHALL have exactly one initializer, and neither form SHALL be parsed as a static-phase
function.

#### Scenario: Parse an imported data symbol

- **WHEN** source declares `unsafe extern "C" static environment: *mut *mut u8 as "environ"`
- **THEN** syntax preserves the local name, type, ABI, and native symbol independently

#### Scenario: Parse an exported data symbol

- **WHEN** source declares `export "C" static silk_abi_version: u32 = 1`
- **THEN** syntax preserves the declared type, initializer, ABI, and exported symbol
