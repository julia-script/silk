## Context

This change builds on the locked builder state and serialization seams established by establish-llvm-builder-foundation. See specs/llvm-module-declarations/spec.md for the required module vocabulary. Zig stores most entities as compact indices into parallel arrays and interns them through structural hash maps. TypeScript needs equivalent identity and order without depending on object identity or lossy number conversion.

## Goals / Non-Goals

**Goals:**

- Preserve structural identity, insertion order, exact constants, and global symbol semantics.
- Keep each public LLVM concept in its own actor module while sharing one private module state.
- Extend text and bitcode output in the same commits as each domain feature.

**Non-Goals:**

- Port Zig's complete std.Target database or infer the host target.
- Add function bodies, advanced intrinsic resolution, or metadata.
- Expose interning keys, backing arrays, or numeric bitcode indices.

## Decisions

### Split actors around LLVM concepts

Public modules are DataLayout, Type, Attribute, Constant, Global, Variable, Alias, and Function. Data and handles live with the named actor; sibling functions operate data-first. Builder owns cross-actor creation and global-name arbitration only where no narrower actor owns the operation. All public actors receive explicit subpath exports.

Alternative considered: reproduce the 16,000-line Builder.zig as one TypeScript module. That would violate repository actor boundaries and make dependencies, tests, and tree shaking worse.

### Canonicalize with deterministic structural keys

Each actor defines a private canonical-key encoder for its data. Keys use length-delimited byte segments and decimal or hexadecimal encodings for numeric values, never JSON serialization of unordered objects. Maps retain insertion order; table indices are assigned only after validation succeeds. Equivalent requests return the existing public handle.

Alternative considered: use data objects as Map keys. JavaScript object identity would fail to intern structurally equivalent requests.

### Keep type handles nominal and descriptions internal

A Type handle identifies a table entry. Tagged internal descriptions store simple, integer, pointer, function, vector, array, structure, named-structure, and target-extension payloads. Queries validate the expected tag and return SilkError for caller misuse rather than relying on Zig-style unreachable paths.

Named opaque structures retain one stable identity and a separately mutable body slot. Assigning the body is a guarded builder mutation and can happen only once with a structure body.

### Parse only explicit LLVM data layouts

Builder options accept a data-layout byte string and optional target triple. DataLayout.parse produces indexed primitive and pointer specifications and retains the original bytes for exact output. Defaults follow LLVM rules where the pinned builder does. Any target convenience table must be explicit static data and cannot inspect Node globals.

### Use bigint and raw floating-point bits

Arbitrary integer constants use signed bigint plus their declared LLVM integer type. Normalization computes the exact width-limited representation before interning. Floating constants use format-specific immutable bit records for half, bfloat, float, double, x86-fp80, fp128, and ppc-fp128; convenience number constructors convert only formats JavaScript can represent deterministically.

Alternative considered: store all floating constants as number. This loses NaN payloads and cannot represent extended formats.

### Maintain one ordered global symbol table

Globals own names and common symbol properties; variables, aliases, and functions reference a global entry and store actor-specific payloads. Rename, replacement, and conversion operations reserve names transactionally. Text and bitcode traversal follows stable global insertion order and the format's required category ordering.

### Extend both serializers feature by feature

Every new type, constant, attribute, or declaration adds its text renderer, bitcode record adapter, Zig fixture, and LLVM round-trip test together. Serializer switches are exhaustive over internal tags so a new model variant cannot compile without an encoding disposition.

## Risks / Trade-offs

- [Canonical string keys can allocate heavily] → Centralize key construction per actor and measure before introducing a custom hash representation.
- [Data-layout syntax changes across LLVM versions] → Accept the pinned supported grammar, preserve unknown-version decisions as typed errors, and validate against the compatibility LLVM version.
- [Large constants stress bigint conversions] → Test width boundaries and encode limbs in bounded chunks rather than converting through number.
- [Global replacement can leave stale actor handles] → Resolve global indirection at every public query and preserve the pinned builder's replacement semantics.

## Migration Plan

Apply after establish-llvm-builder-foundation. Add actor modules and exports incrementally, updating minimal module fixtures as declarations become available. Because no declaration API existed before this change, rollback removes only newly introduced subpaths and restores the foundation-only release surface.
