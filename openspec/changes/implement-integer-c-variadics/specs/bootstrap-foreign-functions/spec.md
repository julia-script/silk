## ADDED Requirements

### Requirement: Foreign ellipsis preserves a fixed declaration boundary

An `unsafe extern "C" fn` declaration SHALL admit a final `, ...` after at least one fixed
parameter. Ellipsis SHALL introduce no named parameter and SHALL remain visible in syntax,
formatting, declaration presentation, C signature identity and ABI inspection. The same declaration
SHALL accept zero or more admitted tail arguments after all fixed arguments. Ellipsis on ordinary
or exported definitions and C function-pointer types SHALL diagnose; missing fixed arguments,
nonfinal ellipsis and a declaration with no fixed parameters SHALL diagnose.

#### Scenario: One declaration admits zero and additional operands

- **WHEN** selected source declares `unsafe extern "C" fn f(tag: i32, ...) -> i32` and calls `f(0)` and `f(1, 42)` inside unsafe boundaries
- **THEN** both calls share one genuinely variadic native declaration and preserve their distinct call operands

#### Scenario: Reject a variadic definition or missing fixed operand

- **WHEN** a source function defines a variadic body, or a call omits an ellipsis declaration's fixed parameter
- **THEN** analysis diagnoses the unsupported declaration or missing argument before native lowering

### Requirement: Integer tails use value-preserving C promotions

Variadic tails SHALL admit only i8/u8/i16/u16/i32/u32/i64/u64/isize/usize values. Tail expressions
SHALL retain ordinary expression typing without a declared contextual parameter type. On the three
admitted native targets, signed and unsigned 8/16-bit values SHALL promote to signed i32; 32/64-bit
and pointer-width integer values SHALL retain their target width and signedness. Fixed pointer and
scalar parameters SHALL retain their existing admission and contextual typing. Floating-point,
pointer, reference, aggregate, bool, char and callable tails SHALL diagnose explicitly.

#### Scenario: Promote Darwin mode without changing GNU mode

- **WHEN** selected Darwin source passes a u16 mode and selected GNU source passes a u32 mode to a variadic tail
- **THEN** the respective calls pass i32 and u32 values under the target's actual C variadic ABI

#### Scenario: Reject unsupported tail categories

- **WHEN** a variadic tail contains a floating-point, pointer, reference, aggregate, bool, char or callable value
- **THEN** analysis reports the unsupported tail at its source span and emits no native call

### Requirement: Variadic agreement and promoted call shapes remain distinct

C declaration agreement SHALL include variadic status and the fixed parameter boundary. ABI records
SHALL explicitly encode this status and reject missing or invalid serialized status. Each call SHALL
separately retain its promoted argument types and conversions without changing the callee's C
signature. Inactive selected-source variants SHALL contribute no symbols or signature conflicts.

#### Scenario: Reject a fixed versus variadic redeclaration

- **WHEN** two reachable declarations name one native symbol with equal fixed types but different variadic status
- **THEN** planning reports the conflicting signature and relates both declarations

#### Scenario: Keep multiple tail shapes under one signature

- **WHEN** reachable calls use different admitted tail counts and widths for the same variadic declaration
- **THEN** ABI inspection reports one symbol signature and call inspection retains each promoted shape

### Requirement: Native variadic lowering preserves the C ABI and foreign contract

Native lowering SHALL emit a true variadic external function type, retain the fixed boundary and
perform integer promotions before the call. Darwin ARM64 unnamed integer operands SHALL use the
platform's stack convention; GNU ARM64 and System V x86-64 SHALL use their prescribed register/stack
conventions. Conservative foreign memory, retention and fatal-unwind behavior SHALL remain in force.
The compiler SHALL NOT substitute a fixed-signature cast, generated C adapter or libc spelling rule.

#### Scenario: Observe stack and register boundaries with C

- **WHEN** independently compiled C va_arg receivers consume signed/unsigned promoted integers and enough operands to exceed the target's integer argument registers
- **THEN** debug and optimized native artifacts deliver the exact values under the target ABI and retain the foreign contract

### Requirement: Integer variadic conformance is required on pinned native supplies

Required CI SHALL compile, link, inspect and execute designated C receiver and direct open/openat
fixtures on Darwin ARM64 and GNU/Linux x86-64 and ARM64 using the recorded SDK/glibc/LLVM/linker
baselines. Fixtures SHALL distinguish zero/additional tails, promotions, call shapes and target
placement in debug and optimized modes. Missing tools/supplies or skipped designated cases SHALL
fail. Unverified LTO SHALL remain rejected. Source wrappers SHALL own flags/mode policy.

#### Scenario: Execute direct platform calls without adapters

- **WHEN** selected Silk calls native open/openat with no creation mode and with its target's integer mode
- **THEN** the fixture performs the intended file operation through the true variadic declarations without a C call adapter
