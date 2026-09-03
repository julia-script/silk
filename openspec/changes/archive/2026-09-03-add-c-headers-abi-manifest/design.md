## Context

See `proposal.md` for motivation. Native backend artifacts already retain target-qualified,
canonically ordered function imports/exports and data imports/exports. The driver has two cache
layers and multiple finalization exits, while the CLI alone owns the project package name.

## Goals / Non-Goals

**Goals:**

- Derive both companions from the same verified backend inventory on every library success path.
- Keep rendering pure and serialization deterministic across processes and hosts.
- Preserve typed Effect boundaries for durable filesystem writes and cleanup.

**Non-Goals:**

- ABI compatibility comparison or release policy.
- Public C names or definitions for Silk record pointees; V1 pointers remain opaque.
- Companions for executables or WebAssembly modules.

## Decisions

### Separate content actors from durable commit

`CHeader` owns C declarator rendering and `AbiManifest` owns the versioned JSON data model and
encoding. Both consume the backend's closed ABI-class inventory and return immutable bytes. The
native toolchain boundary owns sibling-path calculation and Effect-wrapped durable writes. This
keeps external I/O out of pure ABI policy and avoids placing filesystem work in the CLI.

Alternative: let the CLI write companions from `Driver.Compiled`. Rejected because direct driver
consumers and cache-hit paths would not share one artifact contract, and a CLI failure could leave
an apparently successful driver result without its promised interface.

### Pass the package artifact name into the driver

Library requests carry the validated package name separately from the platform filename. The
native toolchain derives `<package>.h` and `<package>.abi.json` in the destination directory, and
the driver returns a closed companion-path record only for library kinds. The package name also
feeds the include guard after uppercasing ASCII alphanumerics and replacing every other byte with
an underscore.

Alternative: strip `lib` and platform suffixes from the destination. Rejected because arbitrary
direct-driver destinations and platform suffixes make reverse inference ambiguous.

### Treat pointers as the public V1 opaque class

The artifact inventory deliberately records pointer mutability but not a stable public C pointee
name. Headers therefore render `*const` as `const void *` and `*mut` as `void *`; callback types are
rendered recursively by a declarator function so nested `(*name)(...)` placement is syntactically
valid. This matches the admitted ABI without inventing record-name collision rules.

Alternative: emit full `extern "C" struct` definitions. Deferred because that would require a new
public C type-naming contract and a richer manifest schema not required to consume the current
opaque-pointer ABI.

### Use one versioned manifest entry union

The manifest has top-level `silkForeignAbi`, `target`, `exports`, and `imports`. Each array contains
a discriminated function/data entry with explicit ABI and direction; function entries carry
parameter/result class strings and data entries carry a type class. Rendering sorts the union by
symbol then kind and serializes with two-space indentation plus one trailing newline.

Alternative: separate function and data maps. Rejected because direction-first arrays are easier
to compare as one public symbol namespace and match the existing collision rule.

### Regenerate companions outside binary caches

Backend caches retain the ABI inventory already, so every successful library exit renders and
commits companions after obtaining the primary artifact, including final-artifact cache hits.
Companion bytes are cheap and package-name dependent; caching them separately would add invalidation
surface without avoiding compiler or linker work.

## Risks / Trade-offs

- [A companion write fails after the primary binary is committed] → The toolchain boundary removes
  all three issue-scoped destinations before returning its typed storage failure, so a failed build
  does not advertise a partial library interface.
- [C declarator precedence is easy to render incorrectly] → Pin pure scalar, pointer, empty-arity,
  callback-parameter, and callback-result goldens, then compile the existing native consumer
  through the generated header.
- [Cross-target goldens accidentally invoke unavailable tools] → Render from explicit Darwin and
  Linux inventory fixtures only; retain one host-native include/link acceptance already paid by the
  library suite.
