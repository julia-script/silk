## Purpose

Define the qualified scalar buffer-pointer boundary and the ordinary source initialization proof needed to receive native output safely.

## ADDED Requirements

### Requirement: Pointer qualifiers describe the native address contract

Raw pointer types SHALL record invariant pointee, mutability, nullable or non-null representation, single or many extent, guaranteed minimum alignment and address space. `*const T` and `*mut T` SHALL be non-null single pointers; `[*]const T` and `[*]mut T` SHALL be non-null many pointers. Prefix `?` SHALL explicitly admit address-zero foreign null without a tag lane. Optional `align(N)` SHALL accept positive power-of-two byte alignments through 536870912; omitted alignment SHALL mean the pointee's natural semantic alignment. Optional `addrspace(0)` SHALL name the only admitted data address space. Invalid qualifiers SHALL produce structured diagnostics at their source spans.

#### Scenario: Describe the initial native signatures

- **WHEN** source declares descriptor buffers with a separate length, nullable scalar accessor results and pointers to pointer elements
- **THEN** their types retain independent extent, access, nullability and alignment at every pointer level

#### Scenario: Reject unsupported address spaces

- **WHEN** source spells an address space other than zero or a non-power-of-two alignment
- **THEN** analysis reports a structured diagnostic at the offending qualifier before lowering

### Requirement: Pointer conversions cannot silently strengthen proofs

Safe implicit conversion SHALL preserve pointee identity and extent. It SHALL only remove mutation capability, add nullability or weaken a proven alignment. It SHALL NOT convert nullable to non-null, const to mutable, single to many, many to single, or slice to bare pointer implicitly. Explicit unsafe qualifier conversion SHALL preserve pointee and address space and state its nullness, alignment, extent and access proof obligations. An ordinary source checked null operation SHALL return an Option carrying a non-null pointer only in the non-null case. Raw dereference SHALL remain unsafe even after null checking.

#### Scenario: Handle a nullable accessor explicitly

- **WHEN** an accessor returns a nullable scalar pointer
- **THEN** direct non-null access is rejected and explicit null handling exposes a non-null pointer for the present case

#### Scenario: Keep slices separate

- **WHEN** a foreign parameter expects a many pointer and a separate length but the caller passes a slice directly
- **THEN** analysis reports the ordinary structured type mismatch at the argument

### Requirement: Alignment guarantees reach native accesses

Many-pointer indexed address calculation SHALL use semantic element stride and require an unsafe proof of bounds and live storage. It SHALL return a single-object pointer. Aligned and explicitly unaligned Copy loads/stores SHALL use alignment no stronger than the source pointer and field-offset guarantees. Every pointer operation SHALL remain available through the retained LLVM-to-Wasm route for its admitted role.

#### Scenario: Read an unaligned scalar

- **WHEN** an unsafe caller supplies a live initialized scalar at an address with only byte alignment and uses explicit unaligned access
- **THEN** LLVM emits a load with a valid byte-alignment promise and execution observes the separately written scalar

### Requirement: Output storage separates address and initialization

Ordinary source SHALL expose owning Uninitialized and Initialized output states for Copy T using private representation. Forming or passing an output address SHALL NOT initialize its state or form a readable T reference. Safe initialization SHALL consume the uninitialized owner, write T and produce the initialized owner. An explicit unsafe assumption SHALL be required to assert that external C wrote a valid T. Extraction SHALL consume the initialized owner once. Address formation SHALL NOT prove pinning, ownership transfer, retained-address permission or liveness after owner destruction.

#### Scenario: Passing an address is not an initialization proof

- **WHEN** source forms an Uninitialized output address and calls a foreign writer
- **THEN** safe extraction still rejects that uninitialized owner until a valid state transition occurs

#### Scenario: Extract exactly once

- **WHEN** source initializes an output owner and extracts T
- **THEN** the value is available and ordinary ownership rejects a second extraction from the consumed owner

### Requirement: Independent native evidence gates conformance

Designated conformance lanes SHALL require pinned supplies and fail on missing supplies or skipped cases. Independent C and Silk objects SHALL compile and link in debug and optimized configurations. Darwin ARM64 and GNU/Linux x86-64 SHALL execute admitted cases; GNU/Linux ARM64 SHALL compile, link and undergo object inspection and SHALL execute when a runner is present. Fixtures SHALL distinguish primitive and external record/array layout, scalar/pointer/void calls in both directions, buffer writes observed by Silk, nested pointers, nullable results and supported alignment. LTO SHALL be tested before admission or explicitly rejected. ABI evidence SHALL NOT be presented as proof that arbitrary C initialized storage or respected pinning.

#### Scenario: A conformance lane lacks its platform supply

- **WHEN** a designated native conformance invocation cannot access its pinned compiler, linker, headers or required runner
- **THEN** it fails and names the missing supply instead of silently skipping or selecting host defaults
