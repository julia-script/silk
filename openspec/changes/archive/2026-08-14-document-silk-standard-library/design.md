## Context

See [proposal.md](proposal.md) for motivation and
[the capability spec](specs/silk-standard-library-documentation/spec.md) for required behavior.

The canonical stdlib contains 41 modules and about 9,500 lines. The current documentation model
reports 1,042 public root declarations, of which 1,012 already have a comment, but those comments
are overwhelmingly one-line summaries. No module has a `//!` document; no public function
parameter or type parameter is documented; 19 of 39 public fields and 12 of 21 service or
interface operations are documented; 39 model-visible implementations and their 40 operations have
no documentation. The source contains one recognized `Examples` section and no structured
selection, detail, gotcha, or semantic-link usage.

The documentation model already represents modules, root declarations, type parameters,
parameters, fields, service or interface operations, implementations, and implementation
operations. The current Markdown generator does not traverse that hierarchy: it emits public root
items, conditionally emits field or parameter tables, omits the other child kinds, counts private
items in its module totals, and inserts source headings without rebasing them. It writes one
generated `stdlib.md`, which is already more than 3,000 lines before the richer prose is added.

The existing doctest can compile complete fenced modules from documentation JSON, but stdlib
doctests are not part of the normal repository check. Markdown parsing is intentionally total and
cannot fail ordinary compilation, so authoring-policy enforcement must remain a separate
documentation verification step.

## Goals / Non-Goals

**Goals:**

- Give the entire shipped public stdlib a consistent, didactic source documentation layer.
- Preserve Silk's Markdown-native, declaration-owned documentation model rather than importing a
  TypeScript tag dialect.
- Make every documentable public child visible in the generated reference.
- Keep examples useful, compilable, source-located, and sparse enough to remain maintainable.
- Make coverage, structure, doctests, and generated output enforceable in normal verification.
- Keep the generated main documentation navigable as the authored content grows.

**Non-Goals:**

- Changing compiler semantics, runtime behavior, public types, or declaration visibility.
- Stabilizing the experimental documentation JSON schema.
- Adding return-value, failure-row, or requirement-row prose fields that duplicate semantic facts.
- Requiring comments on private implementation helpers or mechanically documenting every obvious
  operand and generic placeholder.
- Turning doctest into an execution engine; behavioral claims remain covered by stdlib tests.
- Preserving the current generated `stdlib.md` file layout solely for compatibility during alpha.

## Decisions

### Keep canonical documentation in Silk source

The `//!` and `///` blocks in `packages/compiler/stdlib/silk/` remain the only authored API
documentation. The generated reference consumes the same renderer-neutral project model used by
JSON and editor tooling. The detailed stdlib README, implementation, tests, call sites, and archived
OpenSpec artifacts are evidence for the source comments, not parallel documentation to copy
verbatim or maintain independently.

This avoids drift between hover, generated docs, and source while preserving ordinary compilation's
independence from Markdown. A hand-authored reference was rejected because it would immediately
create a second source of truth.

### Adapt the TSDoc teaching pattern to CommonMark headings

Each documentation block is an independent Markdown document with this optional section order:

1. one required summary paragraph;
2. `# When to use`;
3. `# Details`;
4. `# Gotchas`;
5. `# Examples`, with `## <scenario title>` before each example;
6. `# See also`, normally a short list of semantic ``[`Symbol`]`` links.

The actual heading depth in generated pages is a formatter concern: the source uses a stable
document-local hierarchy, and the page renderer rebases it below the declaration that owns it.
This is preferable to hard-coding `####` headings for one current consumer or using bold text that
cannot express an example section to the existing parser.

Sections are optional and content-driven. The checker enforces order and shape, not the presence of
every optional section. `@param`, `@returns`, `@fails`, `@requires`, `@examples`, categories,
versions, and similar tags are not introduced.

### Use tiered coverage rather than comment-count maximization

Coverage has three tiers:

- **Required teaching surface:** every module, public root declaration, public field, and service or
  interface operation has a useful summary.
- **Conditional local semantics:** parameters and type parameters receive directly attached comments
  when they explain information not evident from name and type. The owning declaration's main block
  does not substitute a parameter list.
- **Orienting implementation docs:** model-visible conformances and implementation operations are
  rendered when present and documented where their selection or behavior matters. Private helpers
  remain undocumented unless a maintainer-facing invariant warrants a comment.

The policy checker requires the first tier and validates any comments in the other tiers. It does
not reward prose such as “the left operand” or “the value type,” because that volume would obscure
the contracts users actually need.

### Audit questionable public state before describing it

The first authoring task records every currently public root declaration and classifies it as a
user-facing concept, a representation detail required by today's type surface, or a likely
visibility defect. Types such as collection storage states are not silently promoted into
recommended usage. Because API changes are out of scope, every currently public declaration still
receives an accurate neutral description for reference completeness; suspected exposure is also
recorded as a separate local tracker issue for a later change.

This keeps the documentation change complete without smuggling breaking API work into a prose
task.

### Concentrate examples on semantic anchors

Examples are selected per API family rather than per declaration. A module or representative
operation carries the shared mental model; sibling declarations link to it and explain only their
distinct behavior. Primitive numeric families, for example, need representative examples for
checked, wrapping, saturating, conversion, parsing, and formatting semantics—not hundreds of
renamed calls.

Every executable example is a complete module in one `silk` fence below an `Examples` heading.
Examples use public import paths, explicit setup, deterministic inputs, bounded work, and one
observable contract. `silk,ignore` is reserved for genuinely illustrative source that cannot be a
complete module without hiding the lesson. The doctest proves that executable examples compile;
existing or added behavioral tests prove runtime claims.

### Render structured documentation instead of splicing raw Markdown

The module-page renderer traverses the documentation project's structured items recursively and
renders every included item kind in source order. It renders documentation blocks from the parsed
model so it can:

- rebase headings without altering section membership;
- turn resolved symbol links into links to generated declaration anchors;
- keep unresolved links readable as inline code;
- preserve fenced example language and source order; and
- render signatures and child descriptions consistently.

The renderer applies the model's public visibility rather than independently guessing which items
are public. Module counts come from the same filtered item collection that is rendered. Extending
the existing raw-Markdown concatenation was rejected because it cannot safely rebase headings or
emit resolved semantic links.

### Generate an index and deterministic module pages

Replace the single generated `packages/language/docs/stdlib.md` with:

- `packages/language/docs/stdlib/README.md` as the stable index and `/docs/language/stdlib` landing
  page; and
- one generated Markdown file per manifest module beneath that directory.

File names are a deterministic normalization of the canonical module name, and generation rejects
collisions. The index preserves manifest order and carries the module name, import namespace,
public declaration count, summary, and link. Each module page carries module prose and its complete
public hierarchy. The existing docs application already reads package Markdown in place and treats
`README.md` as an index, so no copied content or site-specific frontmatter is needed.

Keeping one giant page was rejected because the planned content and hierarchy would harm load time,
navigation, reviewability, and search result precision.

### Add a stdlib-specific policy checker above the total parser

The checker consumes the compiler analysis and shared documentation project; it does not add
diagnostics to ordinary compiler analysis. It reports source-located violations for:

- missing required coverage;
- missing or multi-paragraph summaries;
- unknown, duplicate, empty, or misordered teaching sections;
- parameter documentation placed in an owning declaration instead of on the parameter;
- examples without a distinct scenario title or outside `Examples`;
- malformed or unsupported fence attributes; and
- stdlib semantic links that do not resolve to an included documentation target.

The shared parser remains total and editor documentation remains best-effort. The stricter checker
is a repository quality gate for maintained stdlib sources, analogous to a documentation linter,
not a language rule.

### Verify four independent documentation gates

Normal verification covers:

1. **Policy:** coverage, structure, attachment, headings, example form, and links.
2. **Compile:** every non-ignored stdlib example through the existing doctest, with skipped and
   failed counts visible.
3. **Behavior:** existing or targeted stdlib tests for claims examples or prose make.
4. **Render:** deterministic generated index/module pages are current and the docs application can
   build them.

The compiler package's documentation check orchestrates the policy, doctest, and freshness checks
so the root `pnpm check` reaches them through the existing package test graph. Full repository
verification remains the required handoff check.

### Author in evidence-backed module-family passes

The source pass is ordered to establish shared concepts before their consumers:

1. module index, policy, renderer, and verification infrastructure;
2. foundational values and unsafe substrate (`bool`, `char`, `option`, `result`, `box`, `bytes`,
   `string`, `layout`, `raw-buffer`, `slot`, `order`, and `format`);
3. collections (`vector`, `hash`, `hash_map`, and `hash_set`);
4. effects and portable services (`core`, `effects`, `logging`, `metrics`, `filesystem`,
   `child_process`, `host_input`, and `standard_input`);
5. native providers (`os_child_process`, `os_filesystem`, `os_host_input`, and
   `os_standard_input`); and
6. numeric contracts and primitive families (`numeric`, signed and unsigned integers, pointer-sized
   integers, and floating-point modules).

Each pass regenerates and verifies only after its source comments and examples are complete. The
final pass runs all gates together and audits cross-module links and repeated-family consistency.

## Risks / Trade-offs

- **Large review surface** → Land the work in coherent module-family checkpoints, keep generated
  output deterministic, and review authored comments separately from generated pages.
- **Verbose parameter comments reduce signal** → Require local parameter documentation only for
  non-obvious semantic information and make the checker validate quality rather than raw count.
- **Public representation details become harder to remove after documentation** → Classify them
  first, describe them neutrally, and record suspected visibility defects as separate follow-up
  issues without changing API in this change.
- **Examples compile but still claim the wrong runtime behavior** → Treat doctest as the compile
  gate and require behavioral test evidence for runtime, ownership, lifecycle, failure, and ordering
  claims.
- **Module-page generation changes documentation paths** → Update all repository links in the same
  change; the project is pre-stable and deliberately does not preserve the old generated file
  layout.
- **Strict documentation checks conflict with total parsing** → Keep policy checking outside
  compiler analysis and invoke it only from maintained documentation verification.
- **Cross-page symbol links can become stale** → Derive anchors from canonical declaration
  identities and validate every resolved target during generation.

## Migration Plan

1. Introduce the policy checker and structured module renderer with tests against focused fixtures.
2. Generate the new index/module tree, update repository links, and remove the obsolete monolithic
   generated page in the same commit boundary.
3. Complete the public-surface audit and record follow-up visibility issues without changing APIs.
4. Author and verify documentation in the ordered module-family passes.
5. Wire stdlib doctests and policy checking into normal verification after existing examples and
   comments satisfy the gates.
6. Regenerate all documentation, build the documentation application, and run the repository's
   required checks before handoff.

Because authored comments remain canonical, rollback consists of reverting this change's source,
generator, policy, and generated-page commits together; no runtime data or compatibility migration
is involved.
