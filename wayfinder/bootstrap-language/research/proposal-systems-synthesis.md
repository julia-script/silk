# A proposal system for Silk

Status: recommended direction for discussion; not an accepted process.

Last reviewed: 2026-08-14.

## The boundary Silk needs

The repository already has two useful decision altitudes:

1. the completed bootstrap-language Wayfinder established broad, connected language decisions; and
2. OpenSpec changes turn a chosen slice into normative specification deltas and implementation
   tasks.

What is missing is a repeatable, reviewable bridge between them. That bridge should answer:

> Should Silk move in this direction, and does the resulting language work as a coherent whole?

It should not answer which files to edit, enumerate implementation tasks, or become the normative
language reference. After acceptance, one or more OpenSpec changes own those details.

```text
Silk Language Proposal          OpenSpec change(s)          Canonical specs and docs
direction, coherence, why   ->  exact deltas and tasks  ->  implemented language truth
```

The external systems point to the same division from different angles:

- Swift visions establish broad goals and models without pre-accepting their concrete child
  proposals ([Swift vision process](https://forums.swift.org/t/the-role-of-vision-documents-in-swift-evolution/62101)).
- TC39 separately matures the problem space, a preferred solution, normative text, and
  implementation evidence ([TC39 process](https://tc39.es/process-document/)).
- Rust separates a programmer-facing explanation from a semantic explanation and explicitly asks
  about drawbacks, alternatives, and whole-language future effects
  ([Rust RFC template](https://github.com/rust-lang/rfcs/blob/master/0000-template.md)).
- Python makes authorship, status, decision authority, resolution, and supersession durable parts of
  the artifact ([PEP 1](https://peps.python.org/pep-0001/)).
- Dart separates the canonical feature specification from its implementation project
  ([Dart language-feature lifecycle](https://github.com/dart-lang/language/blob/main/doc/life_of_a_language_feature.md)).

## The viable formats

| Format | Shape | Strength | Failure mode for Silk |
| --- | --- | --- | --- |
| One-shot RFC | One complete document moves from draft to accepted or declined | Cheapest process; strong historical decision record | Authors can commit to a local solution before the problem and language-wide interactions have been challenged |
| TC39-like stage track | A proposal advances through problem, design, specification, and implementation-evidence stages | Clear evidence threshold at every maturity level | Later stages duplicate OpenSpec; six standards-committee stages create ceremony without adding authority |
| Vision plus concrete proposals | A broad vision governs several narrower proposal documents | Best treatment of a large language area and a connected program of work | Two conceptual artifact types are probably unnecessary until Silk has repeated evidence that one proposal cannot hold one coherent thesis |
| Staged design dossier with OpenSpec handoff | One evolving conceptual document moves from unfinished draft to reviewable candidate, then decomposes into OpenSpec after a decision | Preserves big-picture reasoning, iterative adversarial review, and a clean implementation boundary | Requires strong review prompts so the interaction map does not become checklist boilerplate |

The fourth format is the best fit now. It can later gain a separate Swift-like **Language Vision**
artifact if proposals repeatedly need a shared parent direction. Starting with both would be process
ahead of demonstrated need.

## Recommended artifact: Silk Language Proposal

A Silk Language Proposal (SLP) has one central design thesis. It may add, remove, or reshape several
features when those changes are inseparable parts of one language model. It must be broad in
analysis, but bounded in what its acceptance actually decides.

The Draft is a living design workbench, not a form completed from top to bottom. It begins with the
real program or activity the author wants Silk to support, then evolves as conversation discovers
the actual missing language capability. The document should become more concrete alongside the
conversation rather than waiting for a long interview to finish.

### Metadata

```markdown
SLP: <stable number>
Title:
Status: Draft | Candidate | Accepted direction | Deferred | Declined | Withdrawn | Superseded
Revision:
Author:
Created:
Updated:
Discussion:
Review record:
Depends on:
Supersedes:
Superseded by:
Resolution:
OpenSpec handoff:
```

Assigning a number gives a proposal a stable identity; it does not endorse it. The author is the
human owner and decision-maker. AI agents may help draft and review the document, but they are not
authors, approvers, or a source of consensus. `Review record` links fixed candidate revisions to
their adversarial findings and resulting edits. A deferred proposal records a concrete
`Revisit when:` trigger rather than becoming a permanent waiting room.

### Body

```markdown
# Summary

## Problem and evidence
Real programs, language friction, or compiler/library evidence. State the need before the mechanism.

## Driving examples: current and desired
Paired, realistic examples showing what the author is trying to do, what Silk permits today, what
blocks the program, and exactly what the desired language would permit or reject.

## Goals and non-goals
The properties being optimized and the attractive work deliberately excluded.

## Current language model
What Silk means today, the current workaround, and the conceptual weight being displaced.

## Proposed language model
New terminology, programmer mental model, governing invariants, and representative examples.

## Worked language experience
Teach the resulting experience through examples, including outputs, diagnostics, common mistakes,
boundary cases, and interactions with existing Silk concepts.

## Semantic sketch
Semantics and corner cases precise enough to expose contradictions without duplicating a final spec.

## Compiler–standard library boundary
The irreducible compiler primitive, the public model built from it in Silk, and the argument that
the primitive is both necessary and sufficient.

## Whole-language interaction map
For each relevant surface: affected, not affected with a reason, or unknown with a decision gate.

## Scope cohesion
Why the proposal is one decision; which separable proposals were considered and why they were split
or kept together.

## Complexity and subtraction budget
Concepts, syntax, privilege, runtime cost, and teaching burden added; concepts or special cases removed.

## Surface displacement
What becomes valid, invalid, unnecessary, removed, renamed, or migrated before 1.0.

## Drawbacks and risks
The strongest case against the direction, including soundness, cost, tooling, and teaching risks.

## Alternatives and prior art
The status quo, a smaller primitive or library answer, the strongest competing model, and lessons.

## Falsifiers and acceptance blockers
Evidence or contradictions that would revise, defer, decline, withdraw, or supersede the proposal.

## Open realization questions
Questions deliberately delegated to prototypes or later OpenSpec changes.

## Future directions
Natural extensions and whole-language consequences that are explicitly not accepted here.

## OpenSpec realization map
Expected capability-level slices and dependencies, without task lists or file plans.

## Revision and decision record
Substantive revisions, review constraints, final resolution, and rationale.
```

### Examples are design evidence

Every central claim needs a concrete example. Prefer realistic slices of the compiler, a library,
or another intended Silk program over isolated syntax fragments. Each driving case should show:

1. **Intent** — the real activity the program is trying to perform.
2. **Current Silk** — valid source, the current workaround, or the exact point where the program is
   inexpressible. Do not invent current behavior merely to make a before/after pair.
3. **Desired Silk** — the complete source experience being proposed. Mark unsettled syntax as
   illustrative rather than allowing notation to masquerade as a decision.
4. **Observable result** — value, effect, failure, ownership transition, emitted artifact, runtime
   cost, or diagnostic that makes the semantics visible.
5. **Boundary case** — a nearby program that remains invalid or behaves differently, showing where
   the feature stops.

Add cross-feature examples wherever the proposal touches ownership, Effects, services, generics,
modules, or target behavior. Examples constrain the prose: when an example and the stated model
disagree, revise one before the proposal becomes Candidate. A candidate with only explanatory prose
is incomplete.

### Whole-language interaction map

The interaction map is the main local addition. A proposal examines these surfaces rather than
assuming that silence means no effect:

| Surface | Questions |
| --- | --- |
| Syntax and names | Grammar, parsing, formatting, name resolution, modules, visibility |
| Types and abstraction | Inference, generics, structural/nominal identity, interfaces, specialization |
| Execution contracts | Eagerness/laziness, Effects, typed failure, requirements, services |
| Ownership and resources | Moves, borrows, lifetimes, allocation, cleanup, stored values |
| Runtime and targets | Representation, cost model, native/Wasm parity, ABI/FFI consequences |
| Compiler | The smallest target-neutral primitive, its intrinsic semantics, and which compiler phases must change |
| Standard library | The public types, policy, validation, generic selection, providers, and safe wrappers written in ordinary Silk |
| Tooling and diagnostics | Recovery, errors, formatting, language service, inspection, debugging |
| Learning and use | Mental model, progressive disclosure, common errors, complete example programs |

An `unknown` is honest in Draft. Before acceptance it must either be resolved, be named
as an OpenSpec realization question that cannot reverse the direction, or be accepted explicitly as
a falsifier/condition. `Not affected` always carries one sentence of reasoning.

### Compiler–standard library boundary

The proposal must not merely list compiler and standard-library work. It must prove the boundary:

1. **Compiler necessity** — identify the capability that ordinary Silk source cannot express today
   and explain why some compiler primitive is unavoidable.
2. **Smallest primitive** — define the narrowest target-neutral operation that makes the feature
   expressible. The operation belongs to the sealed `Intrinsic` namespace; semantic analysis,
   HIR, MIR, evaluation, and backends must not recognize a standard-library declaration by spelling.
3. **Standard-library sufficiency** — construct the user-facing feature from that primitive in
   ordinary Silk. Validation, policy, generic selection, provider types, reusable combinators, and
   safe wrappers remain in the standard library.
4. **Privilege audit** — explain why a smaller primitive is insufficient and why every additional
   proposed compiler behavior can instead remain in Silk source.

If the proposal cannot draw this boundary yet, that is an acceptance blocker. OpenSpec may later
refine lowering and representation, but it must not move public policy into the compiler simply
because doing so is easier to implement.

### Scope cohesion and splitting

A broad impact does not by itself make a proposal too broad: one semantic thesis may necessarily
touch the compiler, standard library, ownership, Effects, and tooling. The useful test is whether the
document asks for one decision.

During review, test whether:

- different parts solve independent driving cases;
- one part could be accepted while another is declined without making the model incoherent;
- the parts introduce distinct programmer concepts rather than one concept's necessary mechanics;
- different evidence or unresolved questions could change their outcomes independently; or
- the proposal is using one title to conceal a roadmap of loosely related features.

When one of these is true, the reviewer should propose a concrete split: child theses, their driving
examples, and their dependency order. A revision then either creates linked proposals and records
the split, or adds a **Scope cohesion** argument explaining why separate decisions would be
misleading. Merely having both compiler and standard-library work is not a reason to split: those are
two realization layers of the same feature when the public model depends on the primitive.

## Lifecycle and gates

```text
Draft -> Candidate --review findings--> revised Candidate --...--> outcome
                                                              |
                         +------------------------------------+-------------------+
                         | Accepted direction | Deferred | Declined | Withdrawn |
                         +------------------------------------+-------------------+
                                                              |
                                      Accepted direction -> OpenSpec change(s)
                                                         \-> Superseded by a later SLP
```

- **Draft**: unfinished author-controlled thinking. The problem, desirability, and solution may all
  remain open; no one else needs to admit or approve the work.
- **Candidate**: the author considers one fixed revision complete enough for adversarial review. It
  presents a chosen direction, credible alternatives, the whole-language interaction map, the
  compiler/standard-library boundary, and falsifiers. Review findings produce a new candidate
  revision, not an agent approval. The proposal stays Candidate through as many review-and-revision
  rounds as needed to reach an outcome.
- **Accepted direction**: the conceptual model and tradeoff are approved as constraints for one or
  more OpenSpec changes by the human author/decision-maker. This is intentionally reversible before
  1.0 and is not an implementation, stability, or compatibility claim.
- **Deferred**, **Declined**, and **Withdrawn** preserve reviewed reasoning without pretending that
  an agent or committee made the decision. A deferred proposal can return to Candidate when its
  recorded revisit trigger occurs.
- **Superseded** links an earlier accepted direction to the later SLP that replaces it.

Draft revisions may change in place with a revision log. Candidate review targets a fixed revision;
substantive edits increment the revision before another review. Editorial clarifications may amend
an accepted proposal, while a change to its central model, scope, tradeoff, or cross-cutting effects
requires renewed Candidate review or a superseding proposal. Implementation evidence may
legitimately return an accepted direction to Candidate or supersede it.

Implementation progress is orthogonal metadata derived from linked OpenSpec changes. `Implemented`
and `Final` are deliberately not SLP statuses.

## Review model

When asked, one review skill should spawn adversarial agents across three independent lenses and
then synthesize disagreements:

1. **Scope and language coherence** — tests whether the dossier contains one decision, proposes a
   concrete split when it does not, checks invariants and the interaction map, and searches for a
   simpler subtractive design.
2. **Examples and programmer model** — tries to understand the feature from current/desired cases,
   searches for missing counterexamples and interactions, and tests diagnostics, progressive
   disclosure, and whether the proposal earns its cognitive weight.
3. **Compiler privilege and realizability** — independently tries to shrink the primitive, move
   policy into ordinary Silk, and break the claimed compiler/standard-library boundary; it also
   tests target neutrality, cost transparency, toolability, and feasibility unknowns.

These reviews produce objections, constraints, and counterproposals. They never approve a proposal,
vote, or manufacture consensus. The human author decides whether each finding changes the next
revision and eventually records the outcome and rationale.

## OpenSpec handoff contract

Acceptance transfers only:

- the selected language model and governing invariants;
- closed decisions and explicitly rejected alternatives;
- affected existing concepts and specs;
- accepted falsifiers or evidence gates; and
- the capability-level realization map.

Each resulting OpenSpec change links back to the SLP and says which slice it realizes. OpenSpec may
refine mechanics, but it may not silently reverse an accepted conceptual decision. If implementation
finds a fundamental flaw, the proposal is reviewed again or superseded before the implementation
change redefines the language.

## Minimal supporting skills

The process needs four initial skills. Keep the proposal schema, Candidate criteria, and review
lenses in one shared reference so the four skills do not drift into slightly different processes.

### Develop an SLP (`develop-silk-proposal`)

This is the primary skill and should be model-invoked when the author brings a high-level language
idea, desired program, or unfinished proposal. It operates as a collaborative workbench:

1. Start from what the author wants to build or express. Produce a first realistic desired example
   early, even when its syntax is provisional.
2. Work backward through the current language: identify the first blocked expression, semantic
   operation, or library abstraction, then distinguish the desired user feature from the actual
   missing language capability.
3. Offer candidate models, examples, tradeoffs, and a recommendation before asking a focused
   question. Keep questions conversational and consequential rather than walking the template as a
   questionnaire.
4. Make the critique reciprocal. Challenge assumptions and attractive syntax; invite the author to
   reject the AI's model, examples, or taste; use that disagreement to refine the thesis.
5. Maintain the Draft as understanding changes. Record discarded directions and why they lost, but
   let unresolved sections remain visibly incomplete rather than filling them with plausible prose.
6. Revisit the compiler/standard-library boundary whenever the public model changes: find the
   smallest missing primitive, then reconstruct the rest in ordinary Silk.
7. Surface scope pressure as soon as multiple independent theses appear. Sketch a possible split
   without performing it automatically; let the author split or add a cohesion argument.
8. Move to Candidate only when the author chooses and every central behavior has current, desired,
   and boundary examples.

Completion means the proposal's thesis, examples, language model, scope, and privilege boundary are
coherent enough for adversarial review. It does not mean every OpenSpec realization detail is known.

### Review an SLP (`review-silk-proposal`)

Spawn adversarial agents for the three lenses above. Review a fixed Candidate revision, distinguish
proposal blockers from OpenSpec realization questions, and return findings with concrete examples
or counterproposals. For a scope finding, provide the proposed child theses and dependency map. The
review produces neither a score nor an approval.

### Resolve and hand off an SLP (`resolve-silk-proposal`)

Help the author work through review findings conversationally. For every proposed split, either
create the linked proposal shape the author selects or capture the author's scope-cohesion rationale.
Record the author's outcome and rationale. On acceptance, convert the realization map into linked
OpenSpec proposal scopes without pre-writing their detailed specs or tasks.

### Converge an SLP (`converge-silk-proposal`)

This is a deliberately user-invoked autonomous mode. Use it when the author wants the agent to run
the review/revision loop to convergence rather than calling the review and resolution skills after
every round. Because it can run several agent rounds and substantially revise a dossier, it should
never start merely because a proposal happens to exist.

For each round:

1. Freeze the current Candidate revision and record its identity.
2. Spawn fresh adversarial agents for scope/coherence, examples/programmer model, and compiler
   privilege/realizability. Give each the proposal and repository context, not the other reviewers'
   conclusions.
3. Synthesize findings by underlying design claim. Separate proposal-level blockers from edits,
   OpenSpec realization questions, and disagreements between reviewers.
4. Revise the dossier to resolve every material finding that can be settled from evidence and the
   accepted Silk constraints. Add or repair examples before relying on prose to close an objection.
5. For a scope objection, either split the working dossier into linked Draft children with distinct
   theses and driving examples, or add a scope-cohesion argument that directly defeats the proposed
   split. Continue review over every resulting child proposal.
6. Increment the revision, append a concise account of findings and responses, and begin another
   independent review round.

Declare **agent convergence** only after two consecutive fresh review rounds find no unresolved
proposal-level blocker. A finding is material when resolving it could change the thesis, scope,
programmer model, semantics, examples, or compiler/standard-library boundary. Typos, clearer prose,
and questions safely delegated to OpenSpec do not reset convergence.

The loop may stop without convergence only when:

- the same foundational objection survives three consecutive revision rounds;
- resolving a fork requires the author's values or taste rather than repository evidence; or
- a named prototype or external fact is required before either direction can be defended.

In that case, return one compact decision fork containing the competing models, their strongest
examples, the exact unresolved tradeoff, and a recommendation. Do not return a pile of raw review
comments or claim consensus by averaging reviewers.

Convergence leaves the artifact in `Candidate`. It means the adversarial loop could no longer find a
material internal objection under the current evidence; it does not accept, decline, or otherwise
decide the proposal for the author.

A separate index/maintenance skill is unnecessary at first; a small script can validate metadata,
links, terminal-state reasons, and the single canonical index if manual maintenance becomes noisy.

## Decisions incorporated

- There is no admission or approval gate between unfinished work and review. The author moves a
  proposal from Draft to Candidate when it is ready for adversarial review.
- Candidate is an iterative state. Agent review rounds challenge fixed revisions; only the human
  author determines when the argument has reached an accepted, deferred, declined, or withdrawn
  outcome.
- Every candidate must separate compiler changes from standard-library changes and defend the
  smallest compiler primitive that makes the public feature implementable in ordinary Silk.
- Proposal development is a reciprocal design conversation that starts from a desired real program,
  discovers the missing language capability, and keeps an evolving Draft rather than conducting a
  front-loaded questionnaire.
- Examples are required design evidence: current behavior, desired behavior, observable semantics,
  and a boundary case must make every central claim concrete.
- Reviewers must detect multiple independent theses. Revision either splits them into linked
  proposals or records why they form one indivisible decision.
- An explicit convergence skill may run fresh review/revision rounds autonomously. It requires two
  consecutive clean rounds, stops on a repeated irreducible fork or missing evidence, and leaves the
  result in Candidate for the author's decision.
