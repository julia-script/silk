# Future Silk language intelligence

Status: tracked non-normative planning note. These are follow-up capabilities, not requirements or
implementation tasks of this OpenSpec change.

## Why this note exists

The auto-import design exposed several upcoming features that need shared compiler facts and source
transformation machinery. Recording them now prevents the first implementation from becoming a
dead end without prematurely committing the auto-import change to build them.

The comparative indexing research is in [research.md](research.md).

## Preserved architecture

```text
Accepted workspace revision
│
├── WorkspaceInventory                    cheap, broad, header-level
│   ├── per-module exports
│   ├── per-module imports
│   ├── exact export lookup
│   └── future reverse-import view
│
├── ProjectAnalysis                       semantic, current open-root union closure
│   ├── declarations and resolution
│   ├── per-module semantic occurrences
│   ├── ownership
│   └── future per-function demand/usage facts
│
├── future WorkspaceReferences            semantic, lazy/background outside closure
│   ├── per-module resolved-use summaries
│   └── canonical identity lookup
│
└── SourceAction                          protocol-neutral transformation seam
    ├── ImportPlan
    ├── future ContractPlan
    ├── future FunctionConversion
    └── future safe lint fixes
```

The inventory, references, and function-demand facts remain separate actors. They have different
costs and invalidation domains; combining them into one global symbol database would force cheap
features to pay for semantic facts they do not need.

Every product used by an editor request must belong to one atomically accepted workspace revision.
Open buffers override disk. Expensive edits should use `codeAction/resolve` and verify their source
preconditions before returning a change plan.

Disk persistence remains a measured startup optimization, not a prerequisite for incrementality.

## Workspace references and usage CodeLens

The LSP already implements references by matching canonical `SemanticOccurrence` identities across
the accepted `ProjectAnalysis`. That revision contains open roots and their outgoing import closure,
so it does not contain a closed reverse-dependent module unless some open root reaches it.

Future exhaustive references need `WorkspaceReferences`:

- retain immutable per-module resolved-use summaries;
- key top-level uses by canonical declaration identity;
- use `WorkspaceInventory` imports to derive reverse dependents and avoid blindly analyzing the
  whole source root;
- refresh candidate modules lazily or in bounded background work;
- keep locals, parameters, and pattern bindings module/function-local;
- make incomplete/stale state explicit rather than silently returning partial rename edits.

The summary must retain use context, not just a location:

```text
Declaration | Import | DirectCall | FirstClassValue | RunSubject | TypeUse | Read | Write | Move | Drop
```

That context is unnecessary for a simple reference list but required by function conversion,
write-only/unused lints, and more meaningful CodeLens counts.

Local-variable and parameter CodeLens can use current per-module semantic occurrences. Top-level
function/declaration CodeLens needs the workspace layer. A lens should count references rather than
the declaration itself; whether import clauses count as usages remains a product decision.

Rename has a stronger completeness requirement than read-only references. It should use the same
workspace summaries only after the server can prove every affected module is current and editable.

## Function contract completion

Silk already retains declared function kind, return type, failure row, requirement row, expression
types, `fail` facts, and the failure/requirement rows demanded by each `run`. The missing semantic
product is a declaration-independent per-function summary:

```text
FunctionDemand
  success/result type
  propagated failure row
  propagated requirement row
  effect/laziness facts
  source provenance for each demanded member
  availability/underdetermination reason
```

It must be computed from all reachable return/failure paths rather than the last terminal and must
remain queryable when the declared contract is incomplete—the exact state in which the action is
needed. It may rely on available callee contracts; recursive or generic/open-row cases should be
reported as underdetermined rather than guessed.

Two actions should remain distinct:

- **Complete contract** conservatively adds an absent result, failures, requirements, and imports
  without narrowing an existing valid contract.
- **Minimize/synchronize contract** is an explicit refactor that may remove unused row members,
  narrow a result, and remove imports after proving the new contract.

Public declarations may intentionally expose a wider abstraction than their current body requires.
They should not receive automatic narrowing warnings by default. Contract spelling should reuse
the auto-import change's scope-aware presentation and `ImportPlan` machinery, and all header/import
edits should be one atomic `SourceAction.ChangePlan`.

## Ordinary/effect function conversion

`fn` and `effect fn` are not syntax-equivalent. Ordinary statements execute eagerly; invoking an
effect function delays its entire body. An ordinary function may intentionally perform eager setup
and return an `effect {}` recipe. Converting it would change setup/trap/ownership timing.

Therefore conversion is a semantic `refactor.rewrite`, not a general lint fix or keyword edit. A
safe conversion planner needs:

- `FunctionDemand` and explicit laziness/effect-footprint facts;
- workspace reference summaries with call/use context;
- call-site insertion or removal of `run`;
- callable-type and first-class-value validation;
- ownership, capture-access, trap-timing, and public-contract checks;
- one atomic multi-file change plan, or a refusal explaining the first untransformable use.

The default policy should offer only transformations proven semantics-preserving. If the product
later wants an intentionally behavior-changing conversion, it should be a separately named action
with preview—not presented as an ordinary safe refactor.

## Lint warnings and unused variables

Compiler diagnostics currently have only severity `error`. Lints need non-blocking observations,
probably a `lint` phase/category with at least `warning`, while `Diagnostic.hasErrors` continues to
gate compilation only on errors.

Unused-variable analysis must not be implemented as “identity occurrence count equals one.” It
must distinguish read, write, move, and drop, and consult ownership/effect facts:

- deleting `let value = run operation()` removes execution;
- deleting an affine binding can change cleanup;
- a write-only mutable binding differs from a never-referenced binding;
- removing a parameter changes every call and may be a workspace refactor;
- public declarations can be external interfaces and should not be considered unused solely from
  workspace references.

Warnings and fixes should be separate products. A lint may be valid even when no safe change plan
exists. Offer deletion/rewrite only when the compiler proves evaluation, ownership, and cleanup are
preserved; otherwise publish the warning without a fix.

Potential policy lints such as unnecessary failures/requirements or “could be an effect function”
must account for public abstraction and lazy timing. They should not be inferred from textual shape.

## Suggested follow-up changes

1. `add-workspace-reference-index` — full source-root references, richer use contexts, and complete
   reference freshness semantics.
2. `add-usage-code-lenses` — local and top-level counts defined over the reference product.
3. `add-function-contract-actions` — `FunctionDemand`, conservative completion, and explicit
   minimization/synchronization.
4. `add-semantic-lint-warnings` — warning severity, lint configuration, unused binding/parameter
   analysis, and safe fixes.
5. `add-effect-function-refactors` — proven-safe whole-workspace ordinary/effect conversion.

These should be separate OpenSpec changes. Each can reuse the inventory, source-action, import-plan,
and atomic revision seams without being bundled into auto-import.

## Decisions still needed in those changes

- Whether usage CodeLens counts import clauses or only executable/type reference sites.
- Whether lenses show zero, hide zero, or distinguish local versus workspace counts.
- Whether unused parameters have an intentional-unused spelling convention.
- Whether public contract minimization is disabled, opt-in, or merely informational.
- How workspace reference indexing reports partial progress before every candidate module is fresh.
- Which first-class callable uses make function conversion unavailable versus eligible for a larger
  type-preserving rewrite.
