# llvm-module-declarations Specification

## Purpose

Allow callers to describe LLVM module types, constants, attributes, and global declarations and serialize them into valid textual IR and LLVM bitcode.

## Requirements

### Requirement: Explicit target and data layout

The system SHALL accept an explicit target triple and LLVM data-layout string, SHALL parse supported layout components exactly, and SHALL expose layout queries for integer, floating-point, vector, pointer, aggregate, size, and alignment properties. Integer alignment SHALL follow the pinned LLVM semantics: effective lookup starts from the default `i8:8:8`, `i16:16:16`, `i32:32:32`, and `i64:32:64` rules; an explicit same-width rule overrides its default; repeated rules use the final entry; lookup selects an exact width, the smallest larger width, or the largest width; and the parsed source entries remain distinguishable from the effective rules. Aggregate alignment SHALL follow the pinned LLVM semantics: an absent rule behaves as `a:0:64`; `a:0` has one-byte ABI and preferred alignment; each encoded alignment MUST fit LLVM's unsigned 16-bit field; an explicit preferred alignment MUST be nonzero, power-of-two, and at least the ABI alignment; and the final repeated aggregate rule is authoritative.

#### Scenario: Parse a valid data layout

- **WHEN** a caller supplies a supported LLVM data-layout string
- **THEN** layout queries return the widths, address spaces, ABI alignments, and preferred alignments described by that string

#### Scenario: Resolve effective integer alignment

- **WHEN** a caller queries an integer width under an empty, endian-only, or sparse-override data layout
- **THEN** the effective query applies default and final explicit rules before selecting the exact, next-larger, or largest specification, while source-entry queries and rendering remain exact

#### Scenario: Compute arbitrary-width integer allocation layout

- **WHEN** a caller queries the size or alignment of an arbitrary-width integer, or an array, fixed vector, or structure containing one
- **THEN** its store size is rounded up to the effective ABI alignment; arrays use that allocation stride, fixed vectors pack element bits and apply exact or natural vector alignment, and structures use the resulting field layout

#### Scenario: Preserve an aggregate rule

- **WHEN** a caller supplies `a:<abi>[:<preferred>]`
- **THEN** the parsed layout exposes its effective ABI and preferred alignments and renders the original data-layout bytes exactly

#### Scenario: Resolve repeated aggregate rules

- **WHEN** a supported data-layout string contains more than one aggregate-alignment component
- **THEN** the final component determines the observable aggregate ABI and preferred alignments

#### Scenario: Apply aggregate ABI alignment to structures

- **WHEN** a caller queries a non-empty unpacked anonymous or named structure
- **THEN** its ABI alignment and tail-padded allocation size use the greater of the aggregate ABI minimum and the strongest field ABI alignment

#### Scenario: Preserve zero-sized and unaffected aggregate layouts

- **WHEN** a caller queries an empty unpacked structure under a nonzero aggregate ABI minimum
- **THEN** the structure uses the aggregate ABI alignment and retains a zero allocation size

#### Scenario: Preserve packed structure and array layouts

- **WHEN** a caller queries a packed structure or array
- **THEN** aggregate ABI minimums do not change that type's LLVM allocation size and alignment behavior

#### Scenario: Reject a malformed data layout

- **WHEN** a caller supplies a malformed or unsupported data-layout component, including a zero explicit preferred alignment, a non-power-of-two alignment, a preferred alignment below the ABI alignment, or an alignment outside LLVM's unsigned 16-bit field
- **THEN** parsing and builder creation fail with an `InvalidInput` `LlvmError` identifying the rejected component

### Requirement: Structural type interning

The system SHALL construct and structurally intern every type supported by the pinned Zig builder, including primitive, integer, pointer, function, vector, array, anonymous structure, named structure, opaque, and target-extension types.

#### Scenario: Intern an equivalent type

- **WHEN** the same structural type is requested twice from one builder
- **THEN** both operations return the same module-owned type identity

#### Scenario: Complete an opaque named structure

- **WHEN** a caller assigns a valid body to an opaque named structure
- **THEN** later queries and serialization expose the completed body under the original type identity

### Requirement: Exact constants

The system SHALL represent supported scalar, aggregate, string, null, zero, undef, poison, block-address, assembly, and constant-expression values without numeric or byte loss.

#### Scenario: Construct an arbitrary-width integer constant

- **WHEN** a caller supplies a signed or unsigned value valid for an arbitrary-width integer type
- **THEN** the constant is interned and serialized with the exact two's-complement value

#### Scenario: Construct a raw floating-point constant

- **WHEN** a caller supplies a supported raw floating-point bit pattern, including a NaN payload or extended format
- **THEN** the pattern is preserved exactly in bitcode and rendered in a valid LLVM textual form

#### Scenario: Reject a mismatched aggregate

- **WHEN** aggregate elements do not match the aggregate type's shape or child types
- **THEN** construction fails with `LlvmError` before module state is changed

### Requirement: Canonical attributes

The system SHALL support the pinned builder's parameter, return, and function attributes and SHALL canonicalize equivalent attribute sets independent of caller-provided ordering.

#### Scenario: Build equivalent attribute sets

- **WHEN** two attribute sets contain equivalent entries in different orders
- **THEN** they resolve to one canonical attribute-set identity and serialize identically

### Requirement: Global declarations

The system SHALL create, query, rename, replace, and configure globals, variables, aliases, and function declarations with their supported linkage, visibility, preemption, storage, thread-local, address-space, alignment, section, mutability, initializer, and calling-convention properties.

#### Scenario: Declare a global variable

- **WHEN** a caller creates a variable with a valid type, initializer, linkage, and alignment
- **THEN** the declaration can be queried by name and appears equivalently in text and bitcode

#### Scenario: Reject a duplicate incompatible global

- **WHEN** a caller attempts to create an incompatible declaration using an occupied global name
- **THEN** the operation fails with `LlvmError` and preserves the existing declaration

### Requirement: Function declarations

The system SHALL declare functions from function types and SHALL canonicalize repeated compatible declarations under one global identity.

#### Scenario: Repeat a compatible function declaration

- **WHEN** a caller repeats a function declaration with the same name, type, and compatible properties
- **THEN** the system returns the existing canonical function identity

### Requirement: Declaration serialization

The system SHALL emit all supported types, constants, attributes, and global declarations in both textual LLVM IR and LLVM bitcode.

#### Scenario: Round-trip a declaration module

- **WHEN** a module containing representative supported declarations is rendered and encoded
- **THEN** the text is accepted by `llvm-as`, the bitcode is accepted by `llvm-dis`, and both decode to equivalent LLVM IR
