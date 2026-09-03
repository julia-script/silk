## Context

See `proposal.md` for motivation. Raw-pointer admission deliberately ignores the pointee, while nominal layout planning already owns target-aware aligned aggregate placement. The implementation must add an explicit field-interpretation contract without narrowing opaque-pointer interoperability or creating a second layout authority.

## Goals / Non-Goals

**Goals:**

- Make the C-layout promise a declaration-owned semantic fact preserved across imports.
- Validate a recursively closed, concrete C object subset before layout and lowering.
- Reuse the compiler's nominal layout catalog and ordinary struct value semantics.
- Prove the layout and write-observability contract with one economical C oracle and one native acceptance corpus entry.

**Non-Goals:**

- Passing records by value, C unions, bitfields, flexible arrays, packed records, user-selected alignment, or platform-dependent C spellings such as `long`.
- Generic C-layout records or inferred layout promises.
- Runtime symbol lookup, generated record adapters, or a compiler-known standard-library actor.

## Decisions

### The declaration is `extern "C" struct`

The parser will retain `ExternKeyword` and the ABI text directly on the existing `StructDeclaration` node. This reuses the language's explicit ABI vocabulary and avoids introducing a general attribute system solely for layout. `repr(C)` and a new `cstruct` keyword were rejected because each adds syntax machinery without improving the contract.

### Struct facts carry a total layout-contract discriminant

`StructFact` carries a total `layout` discriminant: `Silk`, valid `Foreign` with ABI `C` and source span, or diagnostic-backed `InvalidForeign` retaining the requested ABI and marker span. Every ordinary, tuple, and synthesized struct constructor explicitly writes `Silk`. Only `Foreign` grants the layout promise; `InvalidForeign` preserves source provenance and tooling presentation without silently degrading an erroneous marker into an ordinary declaration. The module semantic surface includes the semantic discriminant while excluding physical source positions. A total field avoids treating absence as a compatibility default in this green-field repository.

### A dedicated C-layout actor owns recursive field eligibility

A focused `CLayout` module will validate resolved struct facts against the closed field vocabulary: fixed-width and pointer-sized integers, `f32`/`f64`, raw pointers, non-zero fixed arrays, and other valid C-layout structs. It will reject ordinary nested structs and every other semantic type. Declaration completion invokes this relation after types and dependencies resolve. This keeps `CAbi` focused on call-position classification and preserves its intentional pointer-pointee opacity.

### Existing packing remains the only aggregate layout authority

The existing nominal catalog and aligned `Packing.pack` rule already compute the required declaration-order C layout for the admitted field vocabulary. C-layout validation constrains inputs; it does not fork layout computation. Layout verification will assert that a foreign-layout fact only reaches supported representations. Native lowering continues to use the same aggregate storage and pointer lane.

### Invalid markers preserve the nominal while withholding the promise

Unsupported ABIs reuse the stable unsupported-ABI diagnostic family. Generic declarations and unsupported fields receive focused stable diagnostics at the parameter list or field type. The nominal struct remains available to tooling and ordinary Silk semantics, but its layout state is diagnostic-backed `InvalidForeign`, so an error cannot silently grant foreign field interoperability or erase the user's explicit marker.

### Verification combines structural and boundary evidence

Parser, formatter, declaration, module-surface, and layout cases extend existing files. One host C oracle reports `sizeof`, `_Alignof`, `offsetof`, and sentinel writes for a mixed/nested/array record; it is compared with one shared compiler analysis. One DriverNativeAcceptance corpus program covers `clock_gettime` and post-call field reads. This avoids a redundant per-feature native parity harness.

## Risks / Trade-offs

- **Host C compilers differ outside the admitted subset** → keep the field vocabulary to fixed representations and supported target profiles; reject platform-dependent or packed constructs.
- **Recursive validation could become declaration-order dependent** → validate through canonical facts with memoized visiting/completed states, reject active inline edges, and reuse existing inline-cycle diagnostics.
- **An invalid marker falling back to ordinary layout could confuse tooling** → retain an explicit invalid-marker state with stable diagnostics while never exposing the valid C-layout promise when validation fails.
- **Native writes could be hidden by cached aggregate values** → use the existing address-root materialization and post-foreign-call reload path, with a C sentinel test that reads multiple fields afterwards.

## Migration Plan

This is additive syntax with no stable compatibility obligation. Land the declaration/fact contract, update all fact constructors and surface encoders atomically, then add validation, layout verification, native evidence, and docs. Rollback is a normal stack-layer revert because no persisted user data or migration format is introduced.
