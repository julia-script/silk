## Context

See [proposal.md](proposal.md) for motivation and
[specs/tuple-and-contextual-record-literals/spec.md](specs/tuple-and-contextual-record-literals/spec.md)
for the source contract. Silk already has nominal source structs, field-complete construction,
canonical struct HIR, nominal instance reachability, and whole-value ownership. Today a struct
literal begins with an explicit type target, and expression analysis resolves that target before
checking fields. Tuple declarations, positional literals, and targetless `.{ ... }` literals do not
exist.

The important implementation constraint is to add bidirectional context only at aggregate literal
boundaries. Silk must not become structurally typed and must not search visible types by shape.
Anonymous aggregates also need deterministic identities suitable for semantic facts, generic
instance keys, ownership, layout, and tooling without inventing source-visible declarations.

## Goals / Non-Goals

**Goals:**

- Reuse one canonical nominal aggregate pipeline below source analysis.
- Make expected-type propagation explicit and narrowly bounded.
- Give each uncontextualized aggregate literal one deterministic occurrence identity.
- Preserve source evaluation order separately from canonical member order.
- Keep anonymous aggregate facts useful to generic consumers and future static reflection.

**Non-Goals:**

- Structural tuple or record types, row polymorphism, width subtyping, or shape-based overload search.
- Labeled tuples, source-visible synthetic fields, or a `(T0, T1)` type grammar.
- Default field values, partial construction, tuple destructuring, spread, or rest members.
- Automatic `Copy` derivation for anonymous aggregates.
- Reflection, static field iteration, variadic functions, formatting, or template parsing.

## Decisions

### 1. Every accepted aggregate has one nominal declaration fact

Named `tuple` declarations enter the same nominal aggregate catalog as structs with an aggregate
kind of `Positional` and ordered ordinal members. Source structs retain kind `Named`. An
uncontextualized tuple or record literal creates a generated declaration fact with kind
`AnonymousPositional` or `AnonymousNamed`. Generated records use their literal field order as
declaration order; generated tuples use ordinal order.

A small `AggregateIdentity` actor owns identity construction and presentation. Source declarations
continue using canonical module-plus-declaration identity. Generated declarations use canonical
module identity plus a stable syntax occurrence key and aggregate kind. The key is reproducible for
equal source in fresh processes but is allowed to change when the literal moves during an edit.
Neither member shape nor inferred types participate in identity.

Generated declarations live in semantic aggregate facts, not lexical name tables. They therefore
participate in type identity, HIR, instances, ownership, and layout without becoming lookup,
import, or export candidates.

Alternatives rejected:

- Intern all equal shapes. That is structural typing under another name and makes unrelated
  convenience literals interchangeable.
- Create a global `Tuple2<A, B>` family. It conflates separate anonymous occurrences and makes
  positional arity a privileged standard-library type.
- Insert fabricated names into `DeclarationIndex`. Tooling and diagnostics would expose names that
  source cannot write.

### 2. Tuple positions are semantic ordinals, not fake source field names

The canonical member identity becomes a closed labeled-or-ordinal form. Existing structs use a
declared field identity; tuple-backed structs use an ordinal identity. Positional projection `.0`
resolves directly to the ordinal member. HIR and every runtime consumer still see an ordered nominal
aggregate construction and projection, but source resolution never maps `_0` or another reserved
string to a tuple member.

This is why `Point { _0: 0, _1: 0 }` is not admitted even though the runtime representation is the
same as a two-field struct. Desugaring is an implementation model, not an extra public syntax.

Alternatives rejected:

- Expose `_0` fields. This leaks compiler naming policy, creates collision questions, and turns
  positional values into awkward labeled records.
- Keep a separate tuple HIR and ownership kind. That duplicates every aggregate rule solely to
  preserve source punctuation after it no longer matters.

### 3. Aggregate literals are analyzed with an optional expected aggregate

Expression analysis gains one narrow aggregate-literal entry that accepts an optional already-known
expected type. It is used by explicit binding annotations, declared returns, known call parameters,
and other contracts that already determine one type without examining the literal's shape.

For `(values...)`, an expected positional nominal selects that declaration; for `.{ fields... }`, an
expected named source struct selects that declaration. Analysis then delegates arity, visibility,
generic inference, completeness, compatibility, source-order evaluation, and canonical reordering
to the existing struct-construction machinery. A mismatched aggregate kind is a contextual type
error rather than a request to search for another type.

When no expected aggregate exists, the syntax occurrence is finalized once as an anonymous
declaration from its inferred members. The result is memoized per semantic analysis. Separate
branch literals therefore remain distinct unless an explicit outer expected type is pushed into
both arms.

Alternatives rejected:

- Analyze bottom-up and structurally coerce later. That would let convenience syntax introduce
  shape compatibility throughout assignment and branch joining.
- Search visible structs for matching fields. Equal-shaped declarations become ambiguous and
  private construction boundaries become easier to probe accidentally.
- Give every immutable binding's initializer an inferred named target retroactively. Type identity
  would depend on later use sites and generic consumers.

### 4. Parentheses keep their existing meanings at arity zero and one

`()` remains unit and `(value)` remains grouping. A tuple literal has two or more comma-separated
elements, or one element plus a trailing comma. Named tuple construction remains call-shaped:
`Point(0, 0)`. The callee resolver distinguishes a positional aggregate declaration from a function
before ordinary call analysis and produces an aggregate-construction fact rather than a call edge.

The `.{` prefix is a primary expression start, so it cannot be confused with a statement block.
Colon remains invalid inside positional literals; labeled values use record syntax exclusively.

### 5. Contextual construction does not require the type name to be in caller scope

The expected canonical type comes from the resolved contract, not from a fresh name lookup at the
literal site. A caller that can call `foo` can therefore write `foo(.{ ... })` without separately
importing `Person`. Construction authority is still checked against `Person` at the call site:
private fields, opaque declarations, missing fields, and generic conflicts fail exactly as they do
for `Person { ... }`.

This is contextual construction, not implicit conversion. A record first created anonymously
cannot later turn into `Person` because its shape matches.

### 6. Sugar disappears before ownership and runtime planning

After semantic resolution, elaboration publishes the existing nominal struct construction and
projection HIR with a source or generated `AggregateIdentity`. Initializer HIR retains source
evaluation order and canonical member mapping. No tuple or structural-record runtime node reaches
ownership, instance discovery, layout, MIR, evaluation, LLVM, or WebAssembly.

Instance keys include the complete nominal identity. Two uses of one anonymous binding share a
generic specialization; two literal occurrences do not. Runtime reachability remains pay-for-use,
so generated declarations in unreachable bodies stay semantic-only.

Ownership consumes the same aggregate identity and ordered members as an ordinary struct. Generated
types receive no implicit `Copy` evidence. Named tuples can participate in ordinary nominal
interfaces through their declared name; anonymous aggregates remain affine unless later general
ownership rules provide evidence without source naming.

### 7. Tooling presents provenance, not generated names

Semantic encodings and inspectors identify an anonymous aggregate as an anonymous tuple or record at
its source span, followed by its ordered member types. Navigation returns to the literal occurrence.
Completion may offer expected struct fields inside a contextual record literal, but generated types
do not appear in type-name completion, imports, exports, or declaration search.

Formatting preserves the source form and existing trailing-comma policy. Future static reflection
may consume canonical member metadata, but this change exposes no source operation for doing so.

## Risks / Trade-offs

- **[Risk] Expected-type propagation expands into general bidirectional inference.** → Restrict the
  new path to aggregate literals and contexts whose contract is independently known; add negative
  tests for branch joining and later-use inference.
- **[Risk] Generated identities become unstable or cache-dependent.** → Centralize identity in
  `AggregateIdentity`, derive it only from canonical module and syntax occurrence, and gate semantic
  and instance encodings with deterministic goldens.
- **[Risk] Anonymous types multiply runtime specializations.** → Preserve occurrence nominality but
  instantiate only reachable values; recommend a named type when deliberate sharing is desired.
- **[Risk] Tuple support forks aggregate code paths.** → Erase to the canonical aggregate member and
  HIR shapes before ownership and runtime planning, and test that backend inventories contain no
  tuple-specific category.
- **[Risk] Contextual literals reveal private fields through diagnostics.** → Reuse the existing
  inaccessible-construction diagnostic policy that does not enumerate hidden fields.
- **[Trade-off] Anonymous all-Copy records remain affine.** → This follows ordinary nominal
  ownership and avoids creating a second implicit Copy rule; callers can borrow them or declare a
  named type when reusable value semantics are required.

## Migration Plan

1. Reserve and format the new syntax, updating any green-field fixture that used `tuple` as an
   identifier rather than retaining a compatibility lexer path.
2. Introduce aggregate identities and declaration facts, then route existing struct construction
   through the generalized member model.
3. Add contextual and anonymous analysis, followed by HIR erasure and downstream nominal coverage.
4. Update inspectors and the prescriptive reference, then regenerate deterministic artifacts.

There is no persisted data or public compatibility contract to migrate. Rollback removes the syntax
and generated facts together; no dual representation or fallback path is retained.
