## MODIFIED Requirements

### Requirement: A foreign function declaration names a native symbol under an explicit ABI

`[pub] unsafe extern "C" fn <name>(<parameters>) -> <result> [as "<symbol>"] [with Intrinsic.foreign(...)]` SHALL declare one
module-level function whose implementation is supplied by native code linked into the artifact. The
declaration SHALL have no body. It SHALL carry three separate identities: the Silk name used by
source, the native symbol (the `as` string when present, otherwise the Silk name), and the ABI
named by the string after `extern`. Only the ABI `"C"` SHALL be accepted; any other ABI string
SHALL be rejected with a diagnostic at the string. The native symbol SHALL be the logical native
name: the compiler SHALL apply the target's own symbol decoration and SHALL NOT require or accept
source spelling of a target prefix. `pub` SHALL control Silk module visibility exactly as for an
ordinary function and SHALL NOT affect native linkage.

#### Scenario: Declare a libc function under its own name

- **WHEN** a module declares `unsafe extern "C" fn abs(value: i32) -> i32` and calls it inside an unsafe boundary
- **THEN** analysis resolves the call as an ordinary call to `abs`, the native symbol is `abs`, and a native executable returns the C library's result

#### Scenario: Rename a symbol

- **WHEN** a module declares `unsafe extern "C" fn cAbs(value: i32) -> i32 as "abs"`
- **THEN** source calls `cAbs`, the native symbol is `abs`, and no declaration named `abs` exists in the module

#### Scenario: Reject an unsupported ABI

- **WHEN** a module declares `unsafe extern "fastcall" fn f() -> ()`
- **THEN** analysis reports the unsupported-foreign-ABI diagnostic at the ABI string and publishes no callable

#### Scenario: A public foreign function is importable like any function

- **WHEN** module `a` declares `pub unsafe extern "C" fn abs(value: i32) -> i32` and module `b` imports it
- **THEN** `b` calls it under the same unsafe rule and the executable declares the symbol `abs` once

The optional sealed clause SHALL follow the foreign-call-contracts specification and retain each property origin.

### Requirement: Foreign signatures admit only the C-compatible scalar subset

Each parameter and the result of a foreign function SHALL be classified by a foreign-ABI admission
relation that is distinct from Silk type compatibility. The admitted V1 subset SHALL be: `()` as the
result only; `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64` as exact-width integers;
`isize` and `usize` as pointer-width integers of the selected target; `f32` and `f64` as the C
`float` and `double` classes; and `*const T` and `*mut T` for any pointee `T` as the C pointer
class, without requiring the pointee itself to be admitted. Parameters SHALL be by value. Every
other type, including `bool`, `char`, `string`, slices, fixed arrays, structs, unions,
enums, callable types, and type parameters, SHALL be rejected with a diagnostic at the offending
type that names the type and the foreign ABI. Admission SHALL use resolved types and the normalized foreign behavior contract, independent of machine target classification. A single-value reference parameter named by the explicit borrow assertion SHALL be admitted as one nonnull C pointer, with ordinary initialized-state and call-loan obligations. References without that assertion, slices and reference results SHALL be rejected. The C classification of `isize`,
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

### Requirement: Foreign symbols are valid and unique per executable

A native symbol SHALL be a non-empty ASCII identifier: a letter or underscore followed by letters,
digits, or underscores. Any other spelling, including an embedded NUL, SHALL be rejected at the
declaration. Within one executable closure, two reachable foreign declarations of the same symbol
SHALL be accepted when their classified C signatures and normalized behavioral contracts are equal and SHALL be rejected with one
diagnostic relating both declarations when they differ. A foreign symbol that names a compiler-owned
runtime symbol, the process entry `main`, or matches the compiler's generated symbol shape SHALL
be rejected at the declaration. A foreign symbol the native backend also declares for its own use
SHALL be accepted only when the classified signature agrees with the backend's declaration; a
disagreement SHALL be reported as a backend diagnostic naming the symbol.

#### Scenario: Reject an invalid symbol spelling

- **WHEN** a module declares `unsafe extern "C" fn f() -> () as "not a symbol"`
- **THEN** analysis reports the invalid-foreign-symbol diagnostic at the `as` string

#### Scenario: Accept an agreeing redeclaration

- **WHEN** two modules each declare `unsafe extern "C" fn abs(value: i32) -> i32` and both are reachable
- **THEN** the executable declares `abs` once and both calls resolve to it

#### Scenario: Reject a conflicting redeclaration

- **WHEN** one reachable module declares `abs(value: i32) -> i32` and another declares `abs(value: i64) -> i64`, both as `"abs"`
- **THEN** planning reports the conflicting-foreign-signature diagnostic at one declaration relating the other and constructs no artifact

#### Scenario: Reject a reserved symbol

- **WHEN** a module declares `unsafe extern "C" fn f() -> i32 as "silk_main"`
- **THEN** analysis reports the reserved-foreign-symbol diagnostic at the `as` string

### Requirement: Foreign calls are direct linked calls

A call to a foreign function SHALL lower to a direct linked native invocation through a compiler-generated forbidden-unwind guard under the target's C calling
convention with the classified signature. The artifact SHALL contain the symbol as an undefined
external reference resolved by the system linker from the program's link inputs. The compiler SHALL
NOT introduce runtime symbol lookup or dynamic binding. Its boundary guard SHALL preserve the admitted machine and behavioral contract and terminate a foreign exception at the boundary; a bare nounwind assertion SHALL NOT replace the guard. Linking SHALL fail as toolchain data when no link input defines the symbol.

#### Scenario: Call a separately compiled C function

- **WHEN** a native build links a C object defining `int32_t silk_test_add(int32_t, int32_t)` and Silk declares and calls it
- **THEN** the LLVM module declares `silk_test_add` with the C calling convention, emits a direct call, and the executable returns the C result

#### Scenario: Fail to link an undefined symbol

- **WHEN** a native build reaches a declared foreign function that no link input defines
- **THEN** the driver reports a typed link failure retaining the linker output and produces no executable
