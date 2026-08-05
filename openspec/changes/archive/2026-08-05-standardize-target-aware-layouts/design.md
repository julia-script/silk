## Context

See [proposal.md](proposal.md) for motivation and the delta specs for behavior.

Today `Mir.TargetLayout` combines an arbitrary triple with pointer facts and one `I32` entry, but it
is not part of `Mir.Module`. `NativeToolchain.hostLayout` guesses a native profile, while
`Analysis.defaultLayout` independently hardcodes Apple arm64. `Analysis.codegen`,
`Backend.Backend.emit`, the LLVM backend, and the direct WebAssembly backend all accept this value
beside MIR. LLVM reads only the triple and otherwise hardcodes `i32`; WebAssembly ignores the value
and hardcodes `i32`. The driver chooses the layout after MIR lowering. The interpreter and analysis
labs therefore cannot inspect the physical decisions that emission will make.

The repository already has two concrete backends. The LLVM builder accepts a target triple in its
existing construction options, and the direct WebAssembly backend emits deterministic WebAssembly
from the same MIR. This change must preserve both paths while making incompatible target/backend
combinations explicit.

The project remains in quiet alpha: the current compiler-internal API is intentionally replaced,
not supported through compatibility overloads.

## Goals / Non-Goals

**Goals:**

- Make one compiler value the only authority for target facts and concrete runtime layouts.
- Put target selection before target-sensitive analysis and layout planning after concrete instance
  discovery but before MIR lowering.
- Make both existing backends demonstrate that the plan is consumed rather than recomputed.
- Leave a direct extension point for nominal aggregate and structural-union entries without defining
  those layouts in this change.
- Keep unsupported target, target/backend mismatch, and malformed-plan states typed and queryable.

**Non-Goals:**

- Struct, union, array, pointer operation, calling-convention, or public FFI layout.
- A stable external ABI or a promise that private layout encodings remain compatible.
- Native WebAssembly linking, a WebAssembly host adapter, or another backend.
- General target-triple parsing, arbitrary LLVM targets, CPU-feature selection, or a user-editable
  data-layout string.
- Simulating raw memory in the MIR interpreter.

## Decisions

### 1. `Target` owns closed canonical profiles; `Layout` owns reachable type plans

Add two public compiler actor modules, following the repository's one-module-per-actor rule.

- `Target` owns the immutable target value, the four exact canonical profiles, host selection,
  request resolution, native-versus-WebAssembly classification, and deterministic target encoding.
  A target records only backend-neutral facts needed by Silk: canonical triple, pointer size and
  alignment, endianness, and target kind.
- `Layout` owns an immutable plan containing exactly one `Target.Target` and an ordered collection
  of concrete type entries. An entry identifies a canonical Silk semantic type and records size,
  alignment, and a Silk representation description. The initial representation variants are the
  signed `I32` scalar and zero-or-one `Bool` scalar; later tickets extend this closed data model for
  nominal aggregates and unions.

`Layout.Plan` contains the target. `Mir.Module` stores one `layout: Layout.Plan`; it does not also
store a target field. This single nesting prevents the two values from disagreeing.

The three native profiles use eight-byte pointers; `wasm32-unknown-unknown` uses four-byte pointers.
All initial profiles are little-endian. `I32` and `Bool` are four-byte, four-byte-aligned values on
all four profiles. The plan does not contain an LLVM data-layout string, LLVM types, WebAssembly
value types, or backend handles.

Alternatives considered:

- Keep target and layout in `Mir`: rejected because selection and planning are compiler concepts
  needed before MIR and by facade queries.
- Put only an opaque layout ID in MIR: rejected because it would restore an out-of-band lookup and
  make artifacts harder to inspect or reproduce.
- Permit arbitrary target records from callers: rejected because internal consistency and stable
  identity would again depend on the caller.

### 2. Target selection and layout planning are two distinct moments

The compilation request carries an optional canonical target ID, not an arbitrary layout. Target
resolution runs at snapshot/driver construction: an explicit ID resolves through `Target`; absence
uses the current host only when it is one of the three required native hosts. WebAssembly is always
explicit because it is not a host default.

The resolved target travels through the target-neutral frontend phases. After `Instances.Discovery`
finishes, `Layout.plan` walks discovered instances and their elaborated function contracts and
operations to collect reachable concrete runtime types. It canonicalizes and sorts those types,
computes entries from the selected target, and publishes the immutable plan. `Lower.lower` then
requires the completed plan and embeds it in its `Mir.Module` result.

Target resolution and layout planning are named `Effect.fn` boundaries because unsupported inputs
are expected compiler failures. A snapshot records an available plan or a precise unavailable
state; it never substitutes a default after failure. The pure `Layout.verify` operation returns
ordered violation data for hand-built or damaged plans, matching MIR verification.

Alternatives considered:

- Plan before discovery: rejected because it would lay out unused declarations and cannot handle
  future monomorphized nominal types without speculation.
- Plan during backend emission: rejected because it hides target facts from analysis and duplicates
  language semantics across backends.
- Plan during MIR lowering: rejected because it entangles two phases and prevents the facade from
  exposing the completed plan independently.

### 3. MIR verification proves coverage and canonicality

`Mir.Module` replaces the separate `TargetLayout` type with `layout: Layout.Plan`. MIR samples and
all lowering call sites must provide it. The MIR encoder writes a target header followed by layout
entries in canonical type order before functions.

Verification composes structural MIR violations with layout violations and coverage checks. Every
function parameter, local, result, and operation type must have an entry. The initial scalar entry
must equal the canonical facts for the plan's target; callers cannot create a valid alternative
`Bool` layout. Violations stay deterministic data and stop evaluation or emission at the facade
boundary.

The plan records concrete representation facts, while MIR operations continue to name logical Silk
types and backend-neutral control flow. This is the distinction that makes MIR target-aware without
making it backend-shaped.

Alternatives considered:

- Trust only plans created through constructors: rejected because tests, labs, and recovery paths
  intentionally build data directly and need explainable validation.
- Let each consumer verify only what it uses: rejected because malformed MIR could produce
  consumer-dependent behavior.

### 4. Backend emission becomes effectful and target-compatible

The nominal backend contract becomes one effectful operation over `(program, codegenRequest)`.
Removing the layout parameter is a deliberate breaking change. Each backend declares supported
canonical targets; the shared `Backend.emit` actor operation verifies MIR and target compatibility
before delegating. Target mismatch and invalid MIR are typed backend failures, not thrown values.

- The LLVM backend accepts the three native profiles in this slice. It creates the LLVM builder
  with `program.layout.target.triple`, maps `I32` and `Bool` by reading their plan entries, and keeps
  using the builder's existing target-triple support. It must not carry a fallback scalar map.
- The direct WebAssembly backend accepts only `wasm32-unknown-unknown`. It stops ignoring the layout,
  validates and maps the same scalar entries to WebAssembly `i32`, and rejects native plans before
  builder creation.

The interpreter receives the already verified MIR module. It may continue representing logical
values as JavaScript data because no current operation observes bytes; it neither accepts nor
creates a layout argument.

Alternatives considered:

- Let a backend silently reinterpret compatible-looking scalars: rejected because aggregates would
  make compatibility accidental and untestable.
- Add backend-specific layout sections to the plan: rejected because the compiler would cease to
  be backend-agnostic.
- Preserve synchronous emission with `Effect.runSync` inside implementations: rejected because
  target incompatibility and builder failures are expected public failures and the repository's
  Effect boundary rules require a typed error channel.

### 5. Native tooling consumes the selected native target; it does not select one

`NativeToolchain.hostLayout` is removed. Host detection moves to `Target.host`; object and link
planning receive the target already carried by MIR/backend artifacts. Planned Clang commands name
that canonical target explicitly. The native executable driver accepts only the three native target
profiles; `wasm32-unknown-unknown` remains available through the direct WebAssembly facade path and
never reaches the native linker.

Artifacts retain the canonical target required to check that program, runtime shim, object, and
link inputs agree. The driver reports target resolution near request setup and reports layout as a
named phase between instances and MIR. Existing timing/memory report conventions remain unchanged.

Alternatives considered:

- Infer the target again from the host or artifact contents in the toolchain: rejected because it
  reintroduces a second authority and prevents cross-target planning.
- Allow the native driver to accept WebAssembly and fail in Clang: rejected because the compiler
  can report the mismatch earlier and more clearly.

### 6. The analysis snapshot is the sharing boundary

`Analysis.Snapshot` records target selection and layout planning alongside instances and MIR.
`Analysis.target`, `Analysis.layout`, `Analysis.loweredMir`, `Analysis.evaluate`, and codegen all
project from that same snapshot. Single-source conveniences accept an optional target ID and
otherwise resolve the supported native host.

The existing WebAssembly convenience no longer swaps backends after building a native-target
snapshot: its callers construct or request a `wasm32-unknown-unknown` snapshot first. LLVM and MIR
labs display the snapshot's target and ordered plan beside emitted representation. This makes a
visible mismatch impossible without a failing test.

Alternatives considered:

- Let `codegenWasm` replace the target at emission: rejected because target-sensitive layout would
  change after analysis and contradict the snapshot model.
- Add layout props only to the React labs: rejected because tooling must consume the supported
  facade rather than recreate compiler facts.

## Risks / Trade-offs

- [The four-profile table can drift from real platform ABIs] → Keep the initial facts deliberately
  scalar and gate each profile with exact target/layout goldens; aggregate ABI work extends the plan
  only with dedicated evidence.
- [Effectful backend emission touches many call sites] → Break the API once, remove every sync
  compatibility path, and convert tests to `@effect/vitest` effect tests where emission can fail.
- [The direct WebAssembly backend was previously target-blind] → Require explicit WebAssembly
  snapshots and add negative tests proving native MIR is rejected before builder construction.
- [Encoding target/layout changes every MIR golden and downstream bitcode digest] → Regenerate only
  after semantic tests pass, then retain fresh-process repetition gates for all four profiles.
- [Cross-target object emission may depend on an installed Clang/sysroot] → Separate pure command
  planning tests for all native profiles from runnable host tests; this change does not promise a
  cross-linking sysroot.
- [A scalar-only plan may tempt later ad hoc aggregate fields] → Keep `Layout` as the sole owner and
  make the struct and union tickets extend its entry vocabulary and verifier rather than MIR or a
  backend directly.

## Migration Plan

1. Introduce `Target` and `Layout` with profile, planning, verification, encoding, and deterministic
   tests, without adapting consumers yet.
2. Add the resolved target and completed plan to analysis snapshots, place planning after instances,
   and make MIR modules require the plan; update MIR samples, verifier, encoder, lowering, and
   goldens together.
3. Replace the backend contract with effectful target-aware emission and migrate LLVM,
   WebAssembly, interpreter, facade, and tests without compatibility overloads.
4. Move native host resolution and target propagation out of `NativeToolchain`; update driver phase
   reports, command planning, artifact compatibility, and native differential tests.
5. Update MIR, LLVM, and WebAssembly inspection labs to consume facade target/layout queries and
   refresh deterministic artifacts.

Rollback is a normal source revert while the change is unreleased. There is no persisted layout
format or external API migration to preserve.
