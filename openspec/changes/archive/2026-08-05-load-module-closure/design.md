# Design — load-module-closure

## Context

See proposal.md — Why. The compiler compiles exactly one `SyntaxFile`; ticket 06's frontend step 1
is loading the reachable module closure of one compilation request. Issue 04 pins the semantics
this change must respect: import paths are logical module identities (never OS paths), importing
the current module is redundant and rejected, and **module-level import cycles are permitted** —
only irreducible _semantic_ cycles are errors, and none of those can exist yet in this slice.
Issue 08 owns the final import spelling; this change ships a deliberately provisional one.

## Goals / Non-Goals

**Goals**

- `import <identifier>` as a provisional top-level declaration (lexer keyword + parser branch).
- A compilation request (root identity + supplied sources) loaded into a deterministic closure of
  `SyntaxFile` artifacts with canonical (identity-sorted) module order.
- Explicit import facts: resolved, unknown (diagnosed), self (diagnosed), unavailable (parser
  recovery, cause-suppressed).
- Cycle facts via strongly connected components — recorded and marked, never errors.
- A direct-link module-closure inspector lab.

**Non-Goals**

- No import _bindings_ — no namespace aliases, no selective imports, no name-resolution effects
  (issue 04's binding rules arrive with declaration collection; downstream phases keep operating
  per module).
- No filesystem: sources arrive as bytes in the request, exactly like `SourceFile` today.
- No command-line spelling or source-root arrangement (issues 07/09).

## Decisions

1. **Request shape: `{ rootModule, sources: ReadonlyMap<identity, bytes> }`.** The logical module
   identity doubles as the `SourceFile` id, so spans and diagnostics already carry canonical
   module identity with no new plumbing. A missing _root_ is a caller-contract defect (rejected),
   not a source diagnostic — there is no source position to attach it to. A missing _imported_
   module is an ordinary diagnostic at the import name's span.

2. **The `module` phase joins the closed phase union** with codes `MOD0001` (unknown module) and
   `MOD0002` (self-import), ranked between parser and semantic — closure resolution happens after
   parsing the importing module and before any semantic phase consumes the closure.

3. **Cycles are facts, not diagnostics.** Issue 04 explicitly permits import cycles ("an import
   cycle does not excuse an irreducible semantic dependency cycle" — the cycle itself is legal).
   The closure computes strongly connected components over resolved imports; each SCC with more
   than one member becomes a cycle fact listing members in canonical order, and the cycle list is
   ordered by first member. Self-imports are excluded from cycle facts — they are already
   diagnosed as redundant.

4. **Import facts follow the existing sentinel idiom.** `Resolved`/`Unknown`/`Self`/`Unavailable`
   tagged union; `Unknown` and `Self` carry the originating diagnostic's identity as `cause`;
   `Unavailable` (missing name after recovery) produces no module diagnostic — the parser
   diagnostic stands alone, matching the cause-suppression rule from `unify-compiler-diagnostics`.

5. **Loading is iterative worklist over a deterministic frontier.** Start at the root; parse; queue
   resolved import targets not yet loaded, processing the queue in canonical (sorted) order so
   traversal order never leaks into results even transiently. Final module list is sorted by
   identity regardless.

6. **The lab is its own direct-link page** (`/docs/labs/module-closure`), reusing the syntax
   inspector's CSS module and diagnostic panel component. Presets supply multi-module sources with
   per-module editable text areas; the closure recomputes locally on edit, like the existing lab.

## Risks / Trade-offs

- [Provisional spelling leaks into fixtures] → Contained: one keyword, one node kind; issue 08's
  respelling is a lexer/parser-local change with the closure API untouched.
- [`import` becomes a reserved word user code can't use as an identifier] → Accepted for
  bootstrap; noted as provisional in the spec.
- [Map-based request tempts insertion-order dependence] → The determinism scenario supplies the
  same modules in different orders and requires identical results; the worklist and final order
  are both canonically sorted.

## Migration Plan

1. Lexer keyword + parser branch + tests (grammar-local, nothing downstream breaks — semantic
   analysis selects only `FunctionDeclaration` children).
2. `ModuleClosure.ts` (request, load, facts, cycles) + phase/codes in `Diagnostic.ts` + tests.
3. Exports, release-candidate surface, inspector lab.
4. Rollback is git-revert; no persisted data.

## Open Questions

None — final import spelling (issue 08), import bindings and visibility (declaration collection),
and semantic cycle rejection (later phases) are explicitly out of scope.
