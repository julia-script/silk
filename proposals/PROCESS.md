# Silk Language Proposal process

This is the authoritative process for Silk Language Proposals. An SLP asks whether Silk should move
in one conceptual direction and whether the resulting language works as a coherent whole. It is not
a task plan or the normative language reference.

## Storage and identity

Store SLP `<number>` at `proposals/<NNNN>-<slug>/proposal.md`. Assign the next unused four-digit
number when creating a Draft; numbering gives stable identity, not endorsement.

Store supporting artifacts beside it:

```text
proposals/NNNN-slug/
├── proposal.md
├── examples/                 # optional standalone pressure programs
├── reviews/
│   └── rNNN.md               # one fixed-revision proposal review round
└── audits/
    ├── openspec-<change>-oNNN.md
    └── implementation-<change>-iNNN.md
```

Use [TEMPLATE.md](TEMPLATE.md) for `proposal.md` and [REVIEW-TEMPLATE.md](REVIEW-TEMPLATE.md) for
proposal review records. Use [OPENSPEC-AUDIT-TEMPLATE.md](OPENSPEC-AUDIT-TEMPLATE.md) and
[IMPLEMENTATION-AUDIT-TEMPLATE.md](IMPLEMENTATION-AUDIT-TEMPLATE.md) for traceability audits. Keep
the table in [README.md](README.md) synchronized when a proposal is created, renamed, or changes
status.

## Authority and lifecycle

The human author owns the proposal and its outcome. AI agents may draft, challenge, revise, and
converge a Candidate; their agreement is evidence rather than approval.

```text
Draft -> Candidate --review/revision--> Candidate -> outcome
                                                  |
                 Accepted direction | Deferred | Declined | Withdrawn
                                                  |
                       Accepted direction -> OpenSpec change(s)
                                          \-> Superseded by a later SLP
```

- **Draft** — unfinished working dossier. Unknowns and provisional examples are expected.
- **Candidate** — the author has chosen a fixed revision for adversarial review.
- **Accepted direction** — the author adopts the conceptual model as an OpenSpec constraint. This
  remains reversible before 1.0 and promises neither implementation nor compatibility.
- **Deferred** — record a concrete `Revisit when` trigger; return to Candidate when it occurs.
- **Declined** and **Withdrawn** — preserve the reasoning and resolution.
- **Superseded** — link both the old and replacement SLP.

Drafts may change in place while incrementing `Revision` after substantive changes. Every formal
review targets the proposal digest and revision recorded in its review file. Clarify accepted text
in place only when meaning is unchanged; otherwise return it to Candidate or supersede it.

## Development stance

Treat a Draft as a collaborative workbench, not a questionnaire. Begin with what the author wants to
build or express. Work backward from a realistic desired program through the current blocker to the
actual missing capability. Offer candidate models, tradeoffs, and examples before asking a focused
question. Make critique reciprocal: the agent challenges the author's idea and exposes its own
recommendation for the author to challenge.

Maintain the dossier as the model changes. Leave unknown sections explicit instead of filling them
with plausible prose. Promote to Candidate only when the author chooses and the Candidate bar below
is met.

## Examples are design evidence

Every central claim needs a realistic example. Each driving case contains:

1. **Intent** — the real activity the program performs.
2. **Current Silk** — valid current source, an actual workaround, or the exact point where the
   program is inexpressible.
3. **Desired Silk** — the proposed complete source experience. Mark unsettled syntax illustrative.
4. **Observable result** — the value, effect, failure, ownership transition, emitted artifact,
   runtime cost, or diagnostic that exposes the semantics.
5. **Boundary case** — a nearby program that remains invalid or behaves differently.

Add cross-feature cases for every material interaction with ownership, Effects, services, generics,
modules, or targets. Resolve contradictions between examples and prose before Candidate review. A
prose-only proposal is never a Candidate.

## Compiler and standard-library boundary

Every SLP proves the minimal-privilege boundary:

1. **Compiler necessity** — identify the capability ordinary Silk source cannot express.
2. **Smallest primitive** — define the narrowest target-neutral operation that makes the feature
   expressible. All source-callable compiler operations live in sealed `Intrinsic`; the compiler
   never recognizes a standard-library declaration by spelling.
3. **Standard-library sufficiency** — build the public feature in ordinary Silk. Keep validation,
   policy, generic selection, provider types, combinators, and safe wrappers in the standard library.
4. **Privilege audit** — explain why a smaller primitive fails and why every additional proposed
   compiler behavior can remain in Silk source.

An unresolved boundary is a proposal blocker. Having both compiler and standard-library changes is
not a reason to split when they are realization layers of one public feature.

## Whole-language interaction map

Mark every surface `Affected`, `Not affected — <reason>`, or `Unknown — <decision gate>`:

| Surface | Required analysis |
| --- | --- |
| Syntax and names | Grammar, parsing, formatting, name resolution, modules, visibility |
| Types and abstraction | Inference, generics, identity, interfaces, specialization |
| Execution contracts | Eagerness, Effects, typed failure, requirements, services |
| Ownership and resources | Moves, borrows, lifetimes, allocation, cleanup, stored values |
| Runtime and targets | Representation, cost, native/Wasm parity, ABI/FFI consequences |
| Compiler | Primitive semantics and affected compiler phases |
| Standard library | Public types, policy, providers, combinators, and safe wrappers |
| Tooling and diagnostics | Recovery, errors, formatting, language service, debugging |
| Learning and use | Mental model, progressive disclosure, common mistakes, real programs |

Before acceptance, resolve each `Unknown`, delegate it to OpenSpec only when it cannot reverse the
direction, or name it as an explicit falsifier or accepted condition.

## Scope cohesion

One SLP asks for one decision, although that decision may cross every interaction surface. Propose a
split when parts solve independent driving cases, introduce distinct programmer concepts, can reach
different outcomes coherently, depend on different evidence, or form a roadmap rather than one
semantic thesis.

A split finding includes child theses, their driving examples, and dependency order. Revision must
either create linked Draft children and preserve the source history, or answer the proposed split in
`Scope cohesion`. Compiler and standard-library layers alone do not imply separate proposals.

## Candidate bar

A proposal is ready for Candidate only when:

- one central thesis and its scope-cohesion argument are explicit;
- every central behavior has current, desired, observable, and boundary examples;
- the semantic sketch exposes relevant corner cases without pretending to be the final spec;
- every interaction-map surface is accounted for;
- compiler necessity, the smallest primitive, standard-library sufficiency, and privilege audit are
  defensible;
- alternatives include the status quo, a smaller primitive or library solution, and the strongest
  competing model;
- risks, falsifiers, acceptance blockers, and OpenSpec realization questions are separated; and
- the author explicitly chooses Candidate.

## Adversarial review

Review one fixed Candidate revision through three independent lenses:

1. **Scope and language coherence** — attempt to split the thesis, find implicit special cases,
   challenge invariants, and search for a simpler subtractive model.
2. **Examples and programmer model** — try to understand the feature from cases alone; find missing
   counterexamples, diagnostics, cross-feature interactions, and unearned cognitive weight.
3. **Compiler privilege and realizability** — shrink the primitive, move policy into Silk, attack
   target neutrality and cost transparency, and identify feasibility facts that could reverse the
   direction.

Reviewers return concrete objections, examples, and counterproposals. They do not edit the dossier,
score it, vote, or approve it. The coordinating agent writes one synthesized review record and
separates material proposal blockers from editorial edits and safe OpenSpec realization questions.

## Agent convergence

An autonomous convergence run repeats fixed-revision review and revision. Use fresh independent
reviewers each round. Revise every material finding that repository evidence and accepted Silk
constraints can settle, update examples before closing objections with prose, increment the revision,
and record the response.

Convergence requires two consecutive fresh rounds with no unresolved material proposal blocker. A
finding is material when resolving it could change the thesis, scope, programmer model, semantics,
examples, or compiler/standard-library boundary.

Stop without convergence only when the same foundational objection survives three consecutive
rounds, a fork requires the author's taste or values, or named prototype/research evidence is
missing. Return one compact decision fork: competing models, strongest examples, exact tradeoff, and
a recommendation. Convergence leaves the SLP in Candidate.

## OpenSpec handoff

Acceptance transfers the selected model and invariants, closed decisions, rejected alternatives,
affected current specs, falsifiers/evidence gates, and capability-level realization map. It does not
transfer task lists or file plans.

Create linked OpenSpec changes only after the author accepts the direction and requests handoff.
Each change identifies the SLP slice it realizes. OpenSpec may refine mechanics but cannot silently
reverse the accepted thesis; return the SLP to Candidate or supersede it when implementation exposes
a conceptual flaw.

## Traceability gates

Treat the accepted SLP, OpenSpec change, and implementation as three distinct authorities:

```text
SLP direction and invariants
  -> OpenSpec normative requirements and scenarios
    -> design and tasks
      -> implementation, tests, generated artifacts, and documentation
```

Trace in both directions. Every accepted SLP decision that a program can observe needs an OpenSpec
requirement and scenario. Every normative scenario needs implementation and verification work.
Every task and implementation behavior must trace back to a requirement, design necessity, or named
repository obligation. Artifact existence, checked task boxes, and passing tests are supporting
evidence, not proof of coverage or fidelity.

Audit an OpenSpec change before implementation. Freeze the SLP revision and artifact digests, then
check direction coverage, normative completeness, internal consistency, task and evidence coverage,
and the compiler/standard-library boundary. A planning audit is ready only when no conceptual
decision is invented, omitted, or contradicted and every observable contract has verification work.

Audit the implementation after its tasks are complete and before archive. Review the actual code,
tests, documentation, generated artifacts, and behavior against both the fixed SLP and OpenSpec
contract. Classify every mismatch by its correct source of truth:

- **Realization refinement** — an implementation detail that changes no normative behavior; record
  it in design or tasks when durable explanation is useful.
- **OpenSpec gap or divergence** — the SLP direction remains intact but a normative requirement,
  scenario, or task is missing or wrong; revise OpenSpec and re-audit.
- **Justified SLP divergence** — implementation evidence exposes a necessary or materially better
  conceptual model; revise the SLP back to Candidate, update its examples and interaction map, and
  review the new direction before treating the implementation as conformant.
- **Unjustified implementation divergence** — the implementation departed from the accepted
  contracts for convenience or without sufficient evidence; request concrete implementation changes.
- **Author decision fork** — competing models depend on taste, values, or missing research; present
  the fork and leave every authority unchanged.

Justification must come from real cases, repository constraints, prototypes, or observable behavior,
not from the implementation's mere existence. Never amend an accepted SLP silently to describe code
after the fact. A justified amendment creates a new Candidate revision; only the author chooses its
outcome.
