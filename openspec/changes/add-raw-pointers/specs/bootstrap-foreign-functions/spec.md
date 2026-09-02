## MODIFIED Requirements

### Requirement: Foreign signatures admit only the C-compatible scalar subset

Each parameter and the result of a foreign function SHALL be classified by a foreign-ABI admission
relation that is distinct from Silk type compatibility. The admitted V1 subset SHALL be: `()` as the
result only; `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64` as exact-width integers;
`isize` and `usize` as pointer-width integers of the selected target; `f32` and `f64` as the C
`float` and `double` classes; and `*const T` and `*mut T` for any pointee `T` as the C pointer
class, without requiring the pointee itself to be admitted. Parameters SHALL be by value. Every
other type, including `bool`, `char`, `string`, references, slices, fixed arrays, structs, unions,
enums, callable types, and type parameters, SHALL be rejected with a diagnostic at the offending
type that names the type and the foreign ABI. Admission SHALL be judged on the type spelling alone, independent of the selected target, so a
foreign header is admitted or rejected once per module. The C classification of `isize`,
`usize`, and pointers SHALL take the selected target's pointer width when the executable is
realized for that target.

#### Scenario: Admit every scalar

- **WHEN** a module declares foreign functions whose parameters and results range over `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `isize`, `usize`, `f32`, `f64`, and a `()` result
- **THEN** analysis accepts every declaration and a native executable exchanges each value with a separately compiled C fixture without loss

#### Scenario: Admit pointers in both directions

- **WHEN** a module declares `unsafe extern "C" fn malloc(size: usize) -> *mut u8` and `unsafe extern "C" fn free(pointer: *mut u8) -> ()`
- **THEN** analysis accepts both and a native executable allocates, writes, reads, and frees through them

#### Scenario: Reject a Silk-only type

- **WHEN** a module declares `unsafe extern "C" fn bad(text: string) -> ()`
- **THEN** analysis reports the foreign-type-not-admitted diagnostic at `string` and publishes no callable

#### Scenario: Reject bool and char

- **WHEN** a module declares `unsafe extern "C" fn bad(flag: bool) -> char`
- **THEN** analysis reports one foreign-type-not-admitted diagnostic at `bool` and one at `char`

#### Scenario: Reject a borrowed parameter

- **WHEN** a module declares `unsafe extern "C" fn bad(bytes: &[u8]) -> ()`
- **THEN** analysis reports the foreign-type-not-admitted diagnostic at the slice type
