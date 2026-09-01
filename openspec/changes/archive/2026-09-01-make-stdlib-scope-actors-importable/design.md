## Context

See `proposal.md` for motivation. Silk selected imports bind public declarations, not catalog
namespace metadata. Actor-backed modules can therefore use `import module { Actor }`, but modules
whose operation qualifier is only a namespace alias cannot. `silk.effect` already demonstrates the
ordinary-source solution: a documented zero-data struct names the module scope while the builtin
`Effect<...>` type remains separate.

The user-facing examples must preserve their existing qualifiers. Primitive modules are the one
intentional exception because their lowercase module name already is the desired qualifier.

## Goals / Non-Goals

**Goals:**

- Make the selected scope import valid for every affected nonprimitive operation module.
- Keep each example's operation qualifier and canonical declaration targets unchanged.
- Derive generated import guidance deterministically from the manifest's preferred qualifier and
  primitive classification.
- Keep every new scope actor in ordinary canonical Silk source.

**Non-Goals:**

- Change import grammar, selected-member semantics, operation lookup, runtime layouts, or backends.
- Turn primitive modules into nominal actors.
- Preserve aliases that merely duplicate the public scope actor's qualifier.

## Decisions

### Declare zero-data scope actors in ordinary source

Each affected module will declare a public empty struct whose name is the example's operation
qualifier and whose documentation states that it names the module scope and is not constructed.
This follows the existing `silk.effect.Effect` pattern and lets ordinary selected-import and
qualified-operation rules do all resolution work.

Changing the compiler to treat catalog namespaces as selected members was rejected because catalog
metadata must not create semantic scope and would give standard-library modules compiler privilege.
Keeping namespace aliases was rejected for qualifiers whose only purpose is to reproduce the public
scope name.

### Keep intentional aggregation aliases

An example may retain a namespace alias when one qualifier intentionally groups multiple
independent declarations and errors, and no single actor represents that complete surface. Such an
alias is semantic rather than redundant. Actor member aliases may likewise remain when preserving
an intentionally different local concept name.

### Render imports from an explicit primitive classification

The reference renderer will use a closed primitive-namespace set. Entries in that set render as
plain module imports; all other manifest entries render as selected scope-actor imports. This is
preferred over guessing from capitalization or inspecting declaration kinds because intrinsic
facades such as `RawBuffer` deliberately use source scope actors even though their represented type
is compiler-defined.

## Risks / Trade-offs

- [A scope actor collides with an intrinsic type spelling] → Characterize the existing `Effect`
  pattern and compile each new declaration and selected import before migrating all examples.
- [A selected actor exposes a narrower operation set than the prior namespace] → Run every doctest
  and retain a semantic aggregation alias where no single actor can preserve the complete surface.
- [Generated guidance drifts from compilable source] → Test primitive and nonprimitive rendering and
  run the generated examples against the same standard-library revision.
- [Public declaration counts and source offsets change] → Regenerate reference pages, embedded
  source, integrity data, and committed MIR/hash goldens in one change.

## Migration Plan

1. Add and characterize scope actors for the affected modules.
2. Migrate documentation comments, guides, labs, fixtures, and shipped examples while preserving
   qualifiers and keeping primitives unaliased.
3. Update reference rendering and regenerate all derived artifacts.
4. Run doctests, focused semantic/golden tests, repository checks, and package-content validation.

Rollback removes the new ordinary declarations and restores the namespace imports; there is no
runtime or data migration.
