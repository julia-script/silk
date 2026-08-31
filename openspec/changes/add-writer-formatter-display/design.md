## Context

See `proposal.md` for motivation. Today an inline conformance operation is indexed as a canonical
synthetic function with `conformanceImplementation` metadata, but source-witness validation and
later witness lookup recover that function through the nominal provider's module. A scalar has no
nominal provider module, so its already-indexed inline function is skipped and the mapping falls
through to the sealed-intrinsic branch. The resulting `SEM0083` is generic because compatibility was
never evaluated.

The current format actor renders through `String` construction and `Allocator`; Writer already
provides the correct typed streaming boundary. Standard-library source must remain ordinary Silk,
and the compiler may not recognize `Format`, `Formatter`, `Display`, Writer, or an integer actor by
spelling.

## Goals / Non-Goals

**Goals:**

- Give every source conformance exactly one deterministic owning module, including scalar heads.
- Treat an inline scalar witness as the canonical source function it already is throughout the
  compiler pipeline.
- Separate byte transport (`Writer`), formatting policy/session (`Formatter`), and type-directed
  presentation (`Display`).
- Make integer presentation allocation-free while retaining precise Writer failure behavior and
  complete parse behavior.
- Give width, alignment, fill, sign, zero-padding, precision, alternate-form, and color permission
  stable meanings suitable for later presentation interfaces.

**Non-Goals:**

- Format-string syntax, interpolation syntax, macros, reflection, or runtime witness tables.
- Binary, octal, hexadecimal, debug, locale-sensitive, or floating-point presentation contracts.
- Terminal detection or automatic color selection inside Formatter.
- Atomic output across a Writer failure.
- Source mappings from scalar conformances to ordinary actor functions; scalar source witnesses are
  inline, while sealed intrinsic mappings remain available for compiler-defined operations.

## Decisions

### Scalar conformances are owned by the source contract

For a source contract, conformance completion computes one legal owner:

```text
nominal provider  -> outer nominal provider module
scalar provider   -> source interface/service module
other structure   -> ineligible
```

Nominal locality therefore remains unchanged. Interface ownership gives a scalar conformance one
canonical home without pretending that the compiler scalar `i32` is nominally owned by the library
module spelled `silk/i32`. It also matches the library shape needed by `silk.format`: the module that
defines `Display` owns the complete scalar witness table.

Alternatives considered:

- Allow scalar impls in any module. Rejected because imports would activate otherwise orphaned
  behavior and whole-program overlap would replace coherent ownership.
- Treat `silk/i32` and peers as compiler-known provider modules. Rejected because it grants semantic
  privilege to standard-library spellings and scatters one interface's scalar witness table.
- Permit only nominal wrappers. Rejected because generic formatting of primitive values is the
  capability being added.

### Inline witness identity comes from conformance metadata, not provider shape

Declaration completion resolves an inline mapping by finding the canonical declaration in the
conformance's module whose `conformanceImplementation` metadata matches the conformance ordinal and
operation name. Compatibility then runs on that declaration for both nominal and scalar providers.
Mapped nominal functions keep their provider-actor rule, and `Intrinsic.*` keeps its sealed branch.
A mapped ordinary target on a scalar receives a dedicated diagnostic directing the author to an
inline operation.

Conformance proof uses the same operation-form distinction. For `Inline`, it returns the canonical
declaration from the conformance module; for `Mapped`, it retains the nominal provider lookup; for
`Intrinsic`, it returns no source declaration and preserves intrinsic selection. This single target
resolution feeds executable-origin discovery, instance reachability, bound-call lowering, and
witness-effect runner construction, so downstream phases do not add scalar-specific dispatch.

Storing or re-deriving a standard-library module for each scalar was rejected. Adding a second
lowering path was also rejected because it would let admissibility and executable selection diverge.

### Formatter is mutable policy state over an ambient Writer

The public shape is equivalent to:

```silk
pub enum Alignment { Default, Left, Center, Right }
pub enum Sign { NegativeOnly, Always, Space }

pub struct FormatOptions {
  pub width: Option<usize>
  pub alignment: Alignment
  pub fill: char
  pub sign: Sign
  pub alternate: bool
  pub zeroPad: bool
  pub precision: Option<usize>
  pub color: bool
}

pub struct Formatter {
  options: FormatOptions
}

pub interface Display {
  effect fn display(
    self: &Self,
    formatter: &mut Formatter
  ) -> () ! WriterError ? &mut Writer
}
```

Formatter owns options and session-local bookkeeping only. Its write and padding actor functions
forward through the ambient mutable Writer requirement. It neither stores a Writer borrow nor
selects a provider. Public default and options-based entry functions construct one Formatter and
invoke the statically selected `Display` witness.

Passing raw options directly to every Display was rejected because it leaves each implementation to
duplicate emission and padding policy. Making Formatter a service was rejected because formatting
policy is explicit call data, while Writer is the replaceable runtime dependency.

### Defaults and option interactions are explicit

Canonical defaults are no width, `Alignment.Default`, space fill, `Sign.NegativeOnly`, no alternate
form, no zero padding, no precision, and color disabled. `Default` lets a presentation choose its
semantic alignment; integer Display uses right alignment when width is present.

Width counts visible Unicode scalar values, not UTF-8 bytes or terminal cells. Center alignment puts
half of odd excess fill on the left and the remainder on the right. ANSI SGR bytes emitted under
color permission have zero visible width. Formatter performs no terminal probing: callers set
`color` according to destination and policy.

For integer Display, precision is the minimum digit count and never suppresses the single zero digit.
Sign precedes digit zero-padding. An explicit precision takes precedence over the `zeroPad` width
flag; remaining width uses ordinary fill and alignment. Decimal alternate form has no effect.
`color = true` permits but does not require styling, and the standard integer Display remains
unstyled.

Terminal-column width was rejected because it requires a versioned width/locale policy unrelated to
Writer. Byte width was rejected because multibyte fill and text would align incorrectly for users.

### Integer rendering uses a bounded core and streamed padding

Signed and unsigned engines write decimal digits backwards into fixed local byte storage large
enough for the widest catalog integer, then expose the populated suffix as a borrowed slice. The
sign, precision zeroes, and bounded core are emitted in order through complete Writer operations;
unbounded requested fill is emitted in reusable bounded chunks. No digit loop performs one Writer
call per digit, and no path constructs owned text.

The signed engine retains a non-positive magnitude so the minimum signed value never requires an
unrepresentable positive counterpart. Narrow integers widen losslessly to the existing widest signed
or unsigned representation before rendering. Parsing remains its existing allocation-free inverse
and is separated from Formatter and Writer.

Building an owned String and writing it once was rejected because it preserves the allocator
dependency. Recursive most-significant-first per-digit writes were rejected because they multiply
Writer operations and can deepen effectful control unnecessarily.

### Writer failure preserves the accepted prefix

Formatter forwards the first `WriterError` and stops emitting. It cannot retract bytes already
accepted by a provider, including fill or styling bytes, and does not attempt compensating writes
that could replace the original failure. Documentation and tests state this explicitly.

## Risks / Trade-offs

- **[Risk] Width measured in Unicode scalars differs from terminal columns for combining and wide
  characters.** → Name the unit explicitly and leave terminal-aware presentation to a separate
  versioned policy.
- **[Risk] User Display implementations can calculate content width incorrectly.** → Centralize
  padding arithmetic and emission in Formatter helpers and test the shipped implementations at
  multibyte-fill boundaries.
- **[Risk] Interface-owned scalar locality is enforced differently from nominal locality.** → Use
  one owner-selection function and exercise local, foreign, scalar, nominal, and structural heads in
  the same conformance suite.
- **[Risk] Admitting compatibility without executable discovery would recreate an unlowerable
  witness gap.** → Resolve the same canonical inline identity in declaration completion,
  ConformanceProof, reachability, and lowering tests before accepting the conformance.
- **[Risk] Arbitrarily large width causes many Writer operations.** → Emit fill in bounded chunks;
  never allocate proportional to width and never write one scalar per operation when a chunk fits.
- **[Risk] A Writer failure after ANSI styling can leave an external terminal styled.** → Default
  color to false, document non-atomic output, and keep standard integer Display unstyled.

## Migration Plan

1. Extend conformance ownership and canonical inline witness resolution, with focused declaration,
   proof, diagnostic, reachability, and effectful bound-call tests.
2. Replace `silk.format` rendering with Formatter, options, Display, padding helpers, and bounded
   signed/unsigned emission; keep parsing intact.
3. Add every integer Display conformance in `silk.format`, then migrate integer actor APIs and all
   repository callers directly to Writer-backed entry functions.
4. Delete the String-producing rendering functions and append-based rendering engine in the same
   change; regenerate the stdlib embedding, manifest-derived artifacts, and documentation.
5. Verify semantics cheaply through shared Analysis snapshots, add the formatting program to the
   differential native corpus where appropriate, and run the repository's required check and
   release-candidate gates.

Rollback is a source revert of the complete change. There is no persisted data or compatibility
format to migrate, and the old and new public rendering surfaces do not coexist.
