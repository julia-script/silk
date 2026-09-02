## Why

`Vector` is the only collection the standard library ships, and Silk has no hashed lookup of any
kind. A program that needs a keyed lookup scans linearly, and there is no sort, so it cannot binary
search instead. The TypeScript compiler this project is bootstrapping away from uses 348 `new Map`
and `new Set` sites across 35 of its 64 modules; a self-hosted compiler needs hashed collections for
its symbol table, its declaration index, and its type interner. That makes this a self-hosting
prerequisite rather than a convenience.

No spec text mentions hash collections today, so the surface needs a new capability rather than an
amendment to an existing one.

## What Changes

- Charter `bootstrap-hashed-collections`: the `HashKey` conformance contract, the `HashSeed` value
  that fixes iteration order, and the `HashMap<K, V>` and `HashSet<T>` collections built over them.
- Require a `HashKey` witness for a `HashMap`'s key type and for a `HashSet`'s element type, so a
  type with no equivalence and no hash cannot be used as one.
- Bind the two halves of `HashKey` to each other: two values a witness calls equivalent SHALL hash
  equal under one seed. A witness that breaks this breaks lookup, so the spec states it as the
  contract rather than leaving it to documentation.
- Fix iteration order per seed, so two runs of one program over one seed observe one order, and
  make the order a function of the seed rather than of allocation addresses or insertion timing.
- Implement all of it as ordinary Silk over `Allocator` and typed storage, and add no hash
  operation to the compiler, the MIR, the evaluator, or any backend.
- Register the new module in the stdlib manifest so it resolves without vendoring.

## Capabilities

### Added Capabilities

- `bootstrap-hashed-collections`: the `HashKey` contract, `HashSeed`, `HashMap<K, V>`, and
  `HashSet<T>`, with the equivalence-implies-equal-hash invariant, seeded deterministic iteration
  order, ownership of move-only keys and values, and the no-compiler-hash constraint.

### Modified Capabilities

- `bootstrap-silk-stdlib`: ship the hashed collection module as canonical Silk source resolvable
  without vendoring, under its own manifest namespace.

## Impact

The collections are library code. They compose the owned-allocation substrate `Vector` already
uses — `Allocator`, `RawBuffer`, `Layout`, `Slot` — and the conformance machinery `Integer` and
`Order` already use. Nothing in semantic analysis, HIR, MIR, evaluation, or a backend learns what a
hash is: every hash a program computes is a function some witness declared in ordinary Silk.

Out of scope: an ordered map or ordered set, a concurrent collection, an iteration protocol (no
iterable abstraction is required or introduced), and the deterministic `Vector` sort, which #36
covers separately over the same conformance machinery.

## Status

Blocked on one enabling gap, confirmed by test before any collection code was written and recorded
in `design.md`: `Bound.operation(args)` reaches a witness that names a sealed intrinsic but not one
that names a function of the provider's own actor, which is the only combination a user-defined key
type's `hash` can use. The charter above is unaffected by how that gap is closed.
