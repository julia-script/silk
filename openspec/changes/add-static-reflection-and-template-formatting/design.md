## Context

See [proposal.md](proposal.md) for motivation and the delta specs for the source contract. This
change builds on three completed foundations that are present in `origin/main` but remain separate
OpenSpec changes until archival:

- `add-static-evaluation-core` supplies explicit static functions, parameters, bindings,
  conditionals, canonical static values, residualization, limits, and traces.
- `add-tuples-and-contextual-record-literals` erases named and anonymous tuples and records into the
  existing nominal struct representation while retaining labels, positions, and occurrence identity.
- `add-writer-formatter-display` supplies ordinary source `Display`, `Formatter`, and mutable
  `Writer` contracts with no intermediate String requirement.

The current static value algebra can already carry recursively pure nominal and array aggregates,
but source has no first-class type metadata, no way to iterate heterogeneous field types, and no
identity-free growable static collection. Runtime HIR, ownership, MIR, evaluation, WebAssembly, and
LLVM intentionally know nothing about static evaluation. The design must preserve that boundary and
the repository's minimal-compiler-privilege rule.

## Goals / Non-Goals

**Goals:**

- Make aggregate metadata sufficient for ordinary static source to generate differently typed
  residual operations for each visible tuple position or record field.
- Let static source inspect admitted aggregate parser values through ordinary typed member
  projection without adding static union matching.
- Keep descriptors and variable-length parser state immutable, deterministic, identity-free, and
  outside runtime ownership.
- Express the complete template grammar and formatting policy in navigable Silk source over a small
  sealed primitive seam.
- Reuse existing nominality, visibility, shared-borrow, interface-witness, Writer, and residual-call
  rules rather than introducing reflection-specific runtime variants.
- Preserve exact static and template provenance through diagnostics and inspection.

**Non-Goals:**

- General token, syntax-tree, declaration, implementation, or module generation.
- Structural typing, width subtyping, row reflection, private-field bypass, universal automatic
  `Display`, or runtime reflection.
- Variadic calls, automatic borrowing, expression-form `if` or `static if`, phase inference, or
  interpretation of ordinary functions as static functions.
- Runtime panic, aggregate top-level constants, LSP purity presentation, compiler configuration,
  tracked file embedding, static Effects, services, unsafe access, or ambient host observations.
- Format specifiers, interpolation expressions, dynamic widths, allocator-backed String results, or
  a runtime formatting parser in the first template surface.

## Decisions

### 1. Static for is semantic repetition, not a runtime loop

The new statement form is:

```silk
static for field in Reflect.fields<Args>() {
  // This body is elaborated independently for each concrete field descriptor.
}
```

The iterable is evaluated first. The residualizer then creates a fresh lexical scope and
re-elaborates the authored body once per canonical element. A heterogeneous reflection iterable may
bind `Field<Args, string>` in one iteration and `Field<Args, i32>` in another. Ordinary runtime
values remain available in the body, so each iteration may retain differently typed runtime calls.
No `StaticFor` node enters runtime HIR.

Re-elaboration, rather than cloning already typed HIR, is essential: the binding's concrete type,
interface evidence, overload selection, field projection, and diagnostics can differ per element.
Each generated fact retains the loop span and current element provenance. Existing evaluator step,
retained-value, call-depth, and residual-growth budgets count iteration work and output.

Alternatives rejected:

- Lower one runtime loop over descriptors. It requires a runtime descriptor representation and
  cannot statically select heterogeneous `Display` evidence.
- Type the loop once over an erased `AnyField`. That introduces dynamic typing, casts, or a runtime
  witness table.
- Make `static for` an expression first. Silk has no ordinary `if` or `for` expression model to
  mirror, and template generation needs ordered statements rather than a value result.

### 2. Field descriptors encode owner and value types explicitly

The intrinsic boundary defines the following sealed phase-only nominal types, and the public
reflection actor exposes them through ordinary source wrappers:

```silk
Type<Owner>
Fields<Owner>
Field<Owner, Value>
StaticSequence<Element>
```

Their generic arities are part of the source contract: one argument for `Type`, `Fields`, and
`StaticSequence`, and two for `Field`. `Type<Owner>` records the aggregate kind and canonical owner. `Fields<Owner>` is a finite
heterogeneous iterable. Each element has a concrete `Field<Owner, Value>` instantiation, declaration
ordinal, either a positional ordinal or public source label, visibility authorization, and source
origin. The compiler substitutes the element's `Value` argument before elaborating that loop
iteration; Silk gains no general existential or dependent runtime type.

Every descriptor nominal lacks runtime layout, alias identity, and ownership. Any occurrence that
would retain one in a residual signature, binding, or call is a phase violation before runtime HIR
is published.

Named tuple positions never acquire `_0`-style labels. Anonymous record labels remain available for
their occurrence-generated owner. Equal-shaped owners remain nominally distinct because the owner
argument and descriptor encoding contain canonical declaration identity.

Alternatives rejected:

- Return only strings and integer offsets. That loses the field type needed for static witness
  selection and invites runtime offset access.
- Reflect backend layout. Layout is target and representation policy, not source-semantic metadata.
- Treat type objects as runtime values. The accepted uses require no runtime reflection registry.

### 3. Reflection authorization follows existing aggregate visibility

Anonymous tuples and records expose all members because their literal occurrence creates the
complete local aggregate authority. Named tuples expose every position but no generated field name.
Named structs and union variants expose only fields ordinarily visible at the declaration using
reflection. The `silk.format` implementation is ordinary standard-library source, so a named type's
private fields do not become visible merely because a public generic operation specializes on it.

Reflection metadata is filtered before a descriptor becomes a `StaticValue`; diagnostics can list
only authorized labels or positions. Generic debug rendering of private state therefore continues to
require an explicit owner-authored `Display` or future `Debug` implementation.

Alternatives rejected:

- Reveal private metadata but reject projection. Names and types are already encapsulated
  information and would leak through completion and diagnostics.
- Use the formatting call site's visibility. That makes one library declaration's specialization
  semantics depend on a remote caller's lexical authority.

### 4. Static aggregates support ordinary compile-time member projection

An aggregate admitted as a `StaticValue` may be inspected with the same ordinary member syntax used
for its source type. Static projection validates the nominal field identity, substitutes the
concrete field type, and returns the already-admitted nested static value. It creates no runtime
projection, borrow, ownership, or cleanup fact.

The template parser uses one homogeneous `Part` struct containing an enum mode plus literal slice,
label, ordinal, and byte-range fields. Static conditionals inspect the enum and fields. The first
change does not add static union pattern matching; introducing a union-shaped `Part` would therefore
be a separate language expansion.

### 5. Static sequences are immutable compiler values behind ordinary source

Template parsing needs a variable-length result, but admitting runtime `Vector` would import
allocation, capacity, cleanup, and resource identity into static evaluation. The intrinsic boundary
therefore gains one static-only canonical sequence value category plus irreducible empty, append,
concatenate, length, and indexed-read primitives. An ordinary `silk.static_sequence` actor wraps
those operations with the public `StaticSequence<T>` API.

Every operation returns a new canonical value. Inside a `static fn`, ordinary `let mut` may replace a
complete sequence binding during parsing:

```silk
let mut parts = StaticSequence.empty<Part>()
while cursor < byteLength(template) {
  parts = StaticSequence.append(parts, parsePart(template, cursor))
}
return parts
```

The evaluator may use persistent nodes or copy-on-write internally, but allocation, capacity,
sharing, and copy count are unobservable. Sequence contents must themselves be admitted static
values. The type and every operation are phase-restricted and never receive runtime layout or
ownership.

Alternatives rejected:

- Permit runtime owned collections in static functions. That gives compiler execution runtime
  destructors and allocator behavior.
- Add mutable sequence references or builders. They require an alias and lifetime model distinct
  from complete binding replacement.
- Parse templates in a monolithic intrinsic. Placeholder grammar and policy belong in source.

### 6. One mixed projection bridge connects static metadata to ordinary runtime values

The public reflection wrapper exposes an operation conceptually equivalent to:

```silk
fn borrowField<Owner, Value>(
  owner: &Owner,
  static field: Field<Owner, Value>,
) -> &Value
```

Its sealed primitive validates the descriptor's canonical owner and authorization while
residualizing. The static descriptor occupies no runtime parameter lane. Successful residualization
emits the same typed shared field projection authored source would produce by spelling a known field;
the ordinary returned-borrow and owner-lifetime machinery then governs the reference. Owned and
exclusive projection are excluded initially, because formatting needs only observation and neither
should bypass field movement or mutation rules.

This operation is deliberately a mixed intrinsic contract rather than an ordinary whole-operation
phase classification: `owner` is a runtime shared-reference lane, `field` is a required static lane
consumed by specialization, and the residual result is the ordinary runtime `&Value` projection.
The catalog and verifier represent and validate those parameter phases explicitly; a residual
intrinsic call or descriptor lane is invalid.

Alternatives rejected:

- Copy the field out. Anonymous aggregates are affine as whole values and reflected fields may be
  non-Copy.
- Pass a runtime offset or name. That creates runtime reflection and can bypass nominal and
  visibility checks.

### 7. Template formatting is a mixed Writer effect with one borrowed pack

The canonical source operation is:

```silk
pub effect fn format<Args>(
  static template: string,
  args: &Args,
) -> () ! WriterError ? &mut Writer
```

A direct anonymous pack therefore uses explicit ordinary borrowing:

```silk
run Format.format(
  "Hello, {name}",
  &.{ name: "Julia" },
)
```

Borrowing a temporary uses the existing hidden-owner lifetime. A named local uses `&args` and can be
formatted repeatedly. Keeping the shared reference explicit avoids consuming affine argument packs
and avoids adding automatic borrowing solely for this API. Variadics remain unnecessary unless
future experience shows that tuple and record packs cannot express real calls ergonomically.

The operation is a mixed effect function: template parsing, reflection, matching, and iteration run
during specialization; Writer and `Display` operations remain runtime Effects. Static Effects are
not introduced—the evaluator generates an ordinary residual Effect body and never constructs or
runs compiler-phase Effect values.

Alternatives rejected:

- Accept `Args` by value. Formatting would unexpectedly consume reusable affine records.
- Return `string`. That requires allocator policy and duplicates the existing streaming Formatter
  direction.
- Add a variadic ABI. Existing aggregate packs already preserve each argument's concrete type.

### 8. The first template grammar has separate positional and named modes

Ordinary source parses four forms:

```text
literal UTF-8       emitted as one Writer segment
{{ or }}            emitted as one literal brace
{}                   next tuple position
{name}               visible record field named name
```

One template cannot mix positional and named placeholders. Positional formatting requires every
tuple position exactly once, making missing and excess arguments statically visible. Named
formatting permits repeated fields and unused record fields because labels make selection explicit.
Named structs may participate through visible public fields; anonymous records expose their complete
literal field set.

The parser is a `static fn` implemented with `silk.static_text` and `silk.static_sequence`. It
returns an immutable homogeneous sequence of one flat `Part` struct whose enum mode, literal slice,
label, ordinal, and source byte-range fields are inspected through ordinary static aggregate
projection. The mixed formatter statically iterates
parts; for a placeholder it statically iterates `Fields<Args>`, retains only the matching descriptor,
projects the runtime field, and selects `Display<Value>` in that concrete iteration. Literal parts
emit ordinary Writer text calls. The standard library also supplies `Display<string>` through the
same Writer path so the motivating string-field examples require no formatting-specific primitive.

Alternatives rejected:

- Allow `{}` and `{name}` to mix. The argument-consumption rule becomes harder to read and validate
  without adding expressive power to either agreed argument-pack style.
- Add format specifiers immediately. They require a separate options grammar and additional
  `Formatter` policy unrelated to proving reflection and unrolling.
- Store a runtime parse plan. The template is a static contract and must disappear.

### 9. Source validation uses existing static and interface diagnostics

Malformed templates and aggregate-mode mismatches end in ordinary source `compileError`, retaining
the existing diagnostic identity. Static text values carry general transformed provenance made of
the authored template expression plus a UTF-8 byte start and end. Slicing composes offsets into that
range, including through static bindings and helper calls, so `compileError` can anchor the precise
originating bytes without recognizing `silk.format` or adding format-specific diagnostics.
Missing `Display` evidence remains the ordinary interface-selection diagnostic. Phase violations,
static limits, borrow failures, and Writer runtime failures similarly retain their owning diagnostic
or error contracts. This avoids a compiler-known template diagnostic family selected by source API
spelling.

Semantic facts connect the authored `static for`, canonical element, template part, selected field,
and generated residual operation. Diagnostics never expose inaccessible field names, host stacks,
evaluator addresses, or backend details.

Alternatives rejected:

- Add a compiler `TemplateError` diagnostic operation. Ordinary source can request `compileError`
  with complete static provenance.
- Convert missing `Display` evidence into template text. That discards the structured conformance
  details and source locations already owned by interface selection.

## Risks / Trade-offs

- **[Risk] Per-element re-elaboration increases compiler work.** → Deduplicate canonical descriptor
  and template-plan values, count every iteration against existing budgets, and fail through the
  residual-growth limit before ownership or backend work.
- **[Risk] A heterogeneous loop becomes an implicit dependent-type feature.** → Restrict differing
  binding types to finite evaluator-produced elements and elaborate each scope independently; do not
  expose existential values, dynamic type tests, or runtime dependent types.
- **[Risk] Reflection weakens encapsulation.** → Filter named aggregate metadata by declaration-site
  visibility before descriptor construction and never include inaccessible spellings in facts or
  diagnostics.
- **[Risk] StaticSequence looks like a second collection language.** → Expose only immutable
  value-semantic operations, complete binding replacement, deterministic budgets, and no allocator,
  capacity, reference, or runtime representation.
- **[Risk] Template specializations cause code growth.** → Include template and argument type in the
  existing canonical specialization key, reuse equal plans, coalesce adjacent literal Writer
  segments in source, and enforce residual-growth limits.
- **[Risk] Standard-library formatting accidentally gains compiler privilege.** → Keep the grammar,
  validation, matching, and Writer composition in inspectable source and audit the catalog for only
  metadata, sequence, and projection primitives.
- **[Risk] Mixed projection becomes a general staging escape hatch.** → Admit the parameter-phase
  contract only for sealed intrinsics, require every static lane to be consumed during
  specialization, and reject any descriptor or mixed call that reaches runtime HIR.

## Migration Plan

1. Add syntax, semantic facts, and diagnostics for static iteration while keeping the form
   unavailable to residual lowering.
2. Extend canonical static values with type, field, heterogeneous-field-sequence, and homogeneous
   static-sequence data plus stable encodings and budgets.
3. Add the minimum sealed reflection, sequence, and projection primitives and ordinary source
   wrappers, then prove visibility and nominality before enabling runtime projection.
4. Re-elaborate `static for` bodies during residualization and admit their ordinary generated calls
   to instance discovery, ownership, and HIR.
5. Implement the ordinary source template parser and Writer-backed `Format.format`, then add
   positional tuple, named record, failure, and cross-engine acceptance fixtures.
6. Update reference documentation and generated standard-library/compiler artifacts, remove every
   prototype or dual path, and run the complete repository and release-candidate gates.

The repository is green-field. Rollback is a source revert of the complete change; no compatibility
parser, variadic fallback, runtime template path, reflection registry, or alternate formatting API
is retained.
