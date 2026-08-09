## Context

See `proposal.md` for motivation. Entry discovery currently accepts only an ordinary
`main() -> I32`; lowering preserves discovery order; both backends assign `silk_main` to function
ordinal zero; and the native shim blindly returns that scalar. Effect functions instead lower to
runners returning an explicit multi-lane `EffectOutcome`, so exposing one directly as `silk_main`
would violate the existing machine ABI and leak its typed failure row.

The bootstrap language has canonical nominal failure types, normalized failure rows, compiler-
planned cleanup, explicit conformance declarations, and native/Wasm effect outcome lowering. It
does not yet have strings, general formatting, or the complete `StandardStreams` capability.

## Goals / Non-Goals

**Goals:**

- Keep the machine boundary closed and scalar for both entry forms.
- Make entry identity semantic MIR data rather than a function-order convention.
- Reuse the existing typed-outcome and cleanup machinery for the generated adapter.
- Require explicit opt-in before an application failure can become a terminal report.
- Produce deterministic native reports without adding runtime reflection or reading payload bytes.
- Keep direct WebAssembly standalone and import-free.

**Non-Goals:**

- User-defined formatting or structural dumps of failure payload fields.
- Supplying arbitrary capability requirements to application entries.
- Catching traps, promising trap unwinding, or converting defects into typed failures.
- Stabilizing the private machine ABI or preserving internal pre-release APIs.

## Decisions

### Use two deliberately different source entry contracts

Ordinary `pub fn main() -> I32` remains the explicit status form. The new form is
`pub effect fn main() -> Unit ! E`; `Unit` success means status zero, matching the Rust
`Result<(), E>` model rather than overloading a successful `I32` with two meanings.

Alternatives considered:

- Accept `effect fn main() -> I32 ! E`. This makes successful application values and process
  statuses indistinguishable and weakens the Rust-like default.
- Replace ordinary main. That would remove a useful low-level status entry for no semantic gain.

### Make `Report` an explicit compiler-sealed marker in the bootstrap

`Report` is a nominal intrinsic capability. A bootstrap conformance is exactly an empty marker
`impl Report for ErrorType {}`. It authorizes the compiler to publish only the type's canonical
identity in a terminal report. Conformance remains explicit, while the implementation avoids
pretending that the current language has a formatting API it does not have.

The native report is exact static UTF-8: `Error: <module>.<type>\n`. It is generated from canonical
compiler data, never payload memory. A later richer reporting change can add a real reporting
operation once owned text and standard-stream capabilities exist; this change does not reserve a
payload formatting ABI.

Alternatives considered:

- Automatically report every nominal type. This adds implicit reflection and no author opt-in.
- Dump fields structurally. This exposes layout details, padding, ownership internals, and an
  accidental formatting contract.
- Require a formatter operation now. The bootstrap cannot express its owned text and stream
  boundary honestly yet.

### Retain an explicit entry descriptor and generate a MIR adapter

`Instances.Entry` records whether the selected user entry is ordinary or effectful. For an
effectful entry it also records the normalized failure types and canonical report identities.
`Mir.Module` receives an explicit machine-entry key. Ordinary entries point at the selected user
instance. Effectful entries point at a generated zero-parameter `I32` adapter function.

The adapter uses one dedicated backend-neutral MIR operation to run the user effect runner and
close its outcome. The operation carries the outcome local, one typed payload local and complete
cleanup plan per failure member, and the normalized tag mapping. Success stores zero. A failure
copies only the selected payload lanes into its typed local, runs its cleanup exactly once, stores
the one-based tag, and joins the scalar return path. Traps never reach that join.

This keeps failure ownership and verification in MIR instead of independently reconstructing it in
each backend. The verifier checks the target signature, tags, local types, report ordering, and
cleanup/type correspondence. The MIR encoder includes the descriptor and generated operation.

Alternatives considered:

- Generate independent wrappers inside LLVM and Wasm. That duplicates semantic cleanup and makes
  interpreter parity difficult.
- Keep ordinal zero as the entry. Adding generated runners already makes ordering an implementation
  detail; preserving it would retain a fragile hidden invariant.

### Export only the explicit machine entry as `silk_main`

Symbol selection compares each concrete function key with `Mir.Module.entry`; it never consults an
array ordinal. The generated adapter receives `silk_main` for effectful entries. The user effect
runner keeps its injective private instance symbol. Backend artifacts retain ordered report
identities beside their existing symbol metadata.

### Keep reporting in the native shim and termination in compiled code

The compiled adapter owns semantic work: run, outcome selection, payload cleanup, and production of
`0` or the normalized one-based failure tag. The generated native shim owns platform work: select
the static report bytes for that tag, write the complete line to file descriptor two, and return
status `1`. It loops over partial writes and returns status `2` on a non-progressing/error write or
an invalid tag. Ordinary-entry shims retain pass-through behavior.

Report strings are emitted as decimal byte arrays, avoiding C string escaping and source-injection
concerns. The shim remains compiled per artifact with the pinned target Clang.

Direct WebAssembly uses the same adapter result but has no shim: hosts observe zero or the failure
tag and can pair it with artifact report metadata. No WASI or ad hoc stderr import is introduced.

### Reject open effect entries before lowering

An effectful `main` with any requirement row is unavailable. This enforces the existing rule that
requirements cannot cross an executable boundary without inventing implicit global providers.
Default host-provider construction belongs to the later minimum-runtime work.

## Risks / Trade-offs

- **[Marker reports omit payload detail]** → The exact bootstrap contract says canonical identity
  only and requires explicit `Report` opt-in; richer formatting remains a separate coherent change.
- **[Failure cleanup can expose backend parity bugs]** → Encode cleanup plans in MIR, verify their
  types/tags, and test evaluator, native, and Wasm paths with hook-bearing failures.
- **[Generated C contains program-derived identities]** → Encode identities as numeric byte arrays,
  not interpolated string literals.
- **[Wasm cannot print without a host]** → Preserve the failure tag and ordered report metadata
  without introducing imports; native remains the executable reporting implementation.
- **[Entry metadata changes internal APIs and snapshots]** → Migrate all hand-built MIR fixtures and
  golden text directly; no compatibility layer is retained in this pre-release project.

## Migration Plan

1. Add `Report` to intrinsic type/conformance analysis and reject invalid marker bodies.
2. Extend entry discovery and reachability with entry kind, report identities, and failure cleanup
   dependencies.
3. Add explicit MIR entry metadata and the verified closing operation; migrate evaluator behavior.
4. Update both backends to export by explicit entry identity and lower the generated operation.
5. Generate native shim source from backend termination metadata and pass it through finalization.
6. Replace entry-order fixtures, add success/failure/cleanup/reporting parity tests, and run the full
   release-candidate validation because emitted artifacts and private symbols change.

Rollback is a normal source revert: the project is unreleased, no persisted data migrates, and no
compatibility bridge is required.
