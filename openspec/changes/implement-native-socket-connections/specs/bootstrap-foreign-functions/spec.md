## MODIFIED Requirements

### Requirement: Foreign symbols are valid and unique per executable

A native symbol SHALL be a non-empty ASCII spelling whose first character is a letter or underscore
and whose remaining characters are letters, digits, underscores, or dollar signs. Any other
spelling, including an embedded NUL, SHALL be rejected at the declaration. The same target-neutral
spelling rule SHALL validate quoted import aliases, quoted export aliases, foreign data symbols,
and serialized ABI-catalog entries, and the admitted spelling SHALL remain byte-exact through LLVM,
object emission, and native linking. Within one executable closure, two reachable foreign
declarations of the same symbol SHALL be accepted when their classified C signatures and normalized
behavioral contracts are equal and SHALL be rejected with one diagnostic relating both declarations
when they differ. A foreign symbol that names a compiler-owned runtime symbol, the process entry
`main`, or matches the compiler's generated symbol shape SHALL be rejected at the declaration. A
foreign symbol the native backend also declares for its own use SHALL be accepted only when the
classified signature agrees with the backend's declaration; a disagreement SHALL be reported as a
backend diagnostic naming the symbol.

#### Scenario: Accept a dollar-bearing quoted symbol

- **WHEN** a module declares `unsafe extern "C" fn closeSocket(fd: i32) -> i32 as "close$NOCANCEL"`
- **THEN** analysis, ABI inspection, LLVM, object emission, and native linking preserve the exact
  symbol `close$NOCANCEL`

#### Scenario: Reject an invalid symbol spelling

- **WHEN** a module declares `unsafe extern "C" fn f() -> () as "not a symbol"`, a spelling with an
  embedded NUL, or a spelling beginning with `$`
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
