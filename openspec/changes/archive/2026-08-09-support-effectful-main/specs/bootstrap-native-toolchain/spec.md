## MODIFIED Requirements

### Requirement: The minimal runtime shim reaches a closed native entry

The toolchain SHALL generate the slice's minimal C runtime shim and compile it with the pinned
Clang. Its private, compiler-versioned scalar ABI SHALL call the explicit zero-parameter
`silk_main -> I32`. For an ordinary entry, the shim SHALL return that result unchanged as the
process exit status. For an effectful entry, `silk_main` SHALL return `0` or a normalized one-based
failure tag; the shim SHALL map success to status `0`, map a valid failure tag through its
compiler-provided canonical report table to one standard-error line and status `1`, and map an
incomplete standard-error write or invalid tag to operational status `2`. The shim is not
user-facing FFI, and its generated source is owned by the compiler.

#### Scenario: Compile and honor the ordinary shim ABI

- **WHEN** the shim is compiled and linked with an ordinary program whose `silk_main` returns `42`
- **THEN** the resulting executable exits with status `42`

#### Scenario: Report and normalize an effect failure

- **WHEN** an effectful program's `silk_main` returns the tag for `app.SomeError`
- **THEN** the shim writes `Error: app.SomeError\n` to standard error and exits with status `1`

#### Scenario: Reject an invalid effect failure tag

- **WHEN** an effectful program's `silk_main` returns a tag absent from its generated report table
- **THEN** the shim exits with operational status `2`
