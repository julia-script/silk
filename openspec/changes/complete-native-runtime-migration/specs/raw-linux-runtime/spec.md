## Purpose

A source runtime composes static non-PIE Linux programs without libc or CRT.

## ADDED Requirements

### Requirement: Kernel boundary

Selected source SHALL implement syscall arities zero through six and no-return with exact x86-64/ARM64 registers, clobbers and kernel negative-error decoding. Linux constants and UAPI declarations SHALL remain distinct from libc catalogs.

#### Scenario: Six arguments

- **WHEN** a mapping syscall is issued on either admitted architecture
- **THEN** all six arguments and its result obey the independently verified kernel ABI without errno access

### Requirement: Source startup and inputs

Source entry SHALL preserve the original kernel stack, decode argc/argv/envp and required auxiliary-vector values, invoke the explicit application root and terminate through exit_group. Required page facts SHALL come from AT_PAGESZ at runtime; missing or invalid facts SHALL terminate before application invocation.

#### Scenario: Runtime page size

- **WHEN** an admitted process enters through its source root
- **THEN** mapping policy uses the validated runtime page size rather than a compiler-host value

### Requirement: Mapping ownership

The source mapping allocator SHALL validate positive sizes, power-of-two alignment and rounding/address overflow, return recoverable allocation failures, expose only its initialized mapped range and release each successful mapping once on admitted structured exits. Fatal traps SHALL promise no cleanup.

#### Scenario: Over-aligned mapping

- **WHEN** an allocation requests alignment larger than the runtime page size
- **THEN** the returned usable range satisfies alignment and its owner retains the complete base mapping for release

### Requirement: Freestanding artifact

Raw composition SHALL emit static non-PIE ELF without interpreter, libc, CRT, implicit TLS or unexplained helper imports. Unsupported runtime capabilities SHALL diagnose before linking. Required x86-64 execution and ARM64 real link/ELF/disassembly/UAPI fixtures SHALL cover debug and optimized modes; LTO SHALL be rejected.

#### Scenario: Missing support

- **WHEN** a raw program reaches an unsupplied runtime capability
- **THEN** compilation reports the requirement rather than choosing hosted support
