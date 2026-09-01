## Why

Silk can now specialize runtime bodies from explicit static inputs, and it already represents tuples
and contextual records as ordinary nominal aggregates, but static source cannot inspect those
aggregate shapes or generate heterogeneous runtime work from them. As a result, APIs such as
compile-time-validated template formatting still require macros, hand-written overloads, or
type-specific boilerplate.

## What Changes

- Add finite, static-only type and field descriptors for tuples, anonymous records, and visible
  fields of named aggregates without introducing runtime reflection or compiler-known library
  declarations.
- Allow ordinary member projection from admitted aggregate values during static evaluation so
  source-defined parsers can inspect homogeneous compile-time records without static pattern
  matching or runtime ownership.
- Add heterogeneous `static for` as a statement form that re-elaborates its body once per finite
  static element and leaves only the generated ordinary runtime operations in residual HIR.
- Add an immutable, identity-free static sequence abstraction so ordinary static source can parse a
  variable-length template without a source-visible allocator, reference, or second ownership
  model.
- Allow a static field descriptor to project its corresponding runtime field from a shared aggregate
  reference, with the concrete field type known independently in each unrolled iteration.
- Add Writer-backed template formatting whose template is a static parameter and whose runtime
  arguments are supplied as a borrowed tuple or record. Positional `{}` placeholders consume tuple
  positions, named `{name}` placeholders select record fields, and `{{` / `}}` emit literal braces.
- Add the ordinary-source `Display<string>` conformance required by literal and named-template
  examples, reusing the existing Writer-backed presentation path.
- Reject malformed templates, missing or incompatible placeholders, unsupported aggregate shapes,
  inaccessible fields, and missing `Display` evidence during specialization, before any runtime
  formatter body reaches HIR or a backend.
- Preserve explicit phase boundaries: the change does not add variadic calls, phase-polymorphic
  functions, runtime reflection, conditional declarations, static Effects, ambient host access,
  runtime panic, aggregate top-level constants, or expression-form conditionals.

## Capabilities

### New Capabilities

- `static-reflection`: Defines static type and field descriptors, immutable static sequences,
  heterogeneous static iteration, visibility, runtime projection, residualization, and the absence
  of runtime reflection state.
- `template-formatting`: Defines compile-time template parsing and validation over borrowed tuple or
  record argument packs, plus residual Writer and `Display` operations.

### Modified Capabilities

- `bootstrap-syntax`: Adds the lossless and recoverable `static for` statement form.
- `bootstrap-intrinsic-boundary`: Adds only the static-only metadata and projection primitives that
  ordinary source wrappers cannot derive.
- `bootstrap-struct-types`: Makes aggregate kind, ordered positions, visible field labels, and field
  types available to authorized static reflection without making aggregate compatibility
  structural.
- `bootstrap-hir`: Requires reflection and static iteration to erase into ordinary field projections,
  calls, and Writer operations before runtime HIR is published.
- `bootstrap-instances`: Discovers `Display` witnesses and runtime calls generated independently by
  each unrolled static iteration.
- `bootstrap-ownership`: Applies ordinary temporary-owner and shared-borrow rules to formatting
  argument packs while excluding descriptors and static sequences from runtime ownership.
- `bootstrap-semantic-facts`: Retains deterministic provenance for reflected fields, static
  iterations, template segments, and their generated residual operations.
- `bootstrap-diagnostics`: Adds stable specialization diagnostics and source offsets for malformed
  templates and invalid reflection operations.
- `bootstrap-silk-stdlib`: Adds ordinary source actors for static reflection, static sequences, and
  Writer-backed template formatting over the sealed intrinsic boundary.

## Impact

- The lexer, parser, syntax tree, formatter, declaration templates, static evaluator,
  residualization coordinator, semantic inspectors, diagnostic catalog, and language reference gain
  static iteration and reflection facts.
- The sealed intrinsic catalog gains a minimal static metadata/projection seam; public reflection,
  parsing policy, placeholder grammar, validation, and formatting composition remain ordinary Silk
  source.
- `silk.format` composes the existing `Display`, `Formatter`, and mutable `Writer` contracts and
  introduces no intermediate runtime string allocation or runtime format parser.
- Existing tuple, contextual-record, ownership, interface-witness, evaluator, WebAssembly, and native
  representations remain the only runtime representations; no reflection table, descriptor object,
  template string, or static sequence enters an artifact.
- Runtime panic, broader top-level constant values, LSP purity/static-eligibility presentation,
  configuration inputs, and tracked file embedding remain separate follow-up changes.
