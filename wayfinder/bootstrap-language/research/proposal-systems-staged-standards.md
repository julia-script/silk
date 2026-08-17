# Staged language-standard proposal systems

Status: research for a Silk conceptual-proposal process; not an accepted process design.

Last reviewed: 2026-08-14.

## Question

Silk needs a proposal artifact that can judge a language change as part of the whole language before
an accepted direction is decomposed into OpenSpec specifications and implementation tasks. This note
compares the official evolution processes of ECMAScript (TC39), Swift, Dart, and C++ (WG21), with
special attention to conceptual breadth, lifecycle, review, revision, and the boundary between design
and implementation.

The recommendation is to borrow the **layering** of these systems, not their institutional weight:

1. a broad, reviewable Silk proposal establishes the problem, system-wide design direction, and
   simplification trade;
2. acceptance endorses that direction without declaring detailed semantics frozen or implementation
   complete; and
3. one or more OpenSpec changes subsequently define exact behavioral deltas and delivery tasks.

## Comparison at a glance

| System | Conceptual artifact | Detailed artifact | Main gates | Implementation separation | Revision and retirement |
| --- | --- | --- | --- | --- | --- |
| TC39 | Stage 0/1 explainer and problem-space work | Stage 2+ normative spec text | Committee consensus at every stage; independent spec reviewers before 2.7; tests and implementations for 3/4 | Stage 0/1 is design; prototypes begin during refinement; production implementation evidence is late-stage | Champion may request regression or withdrawal by consensus; major changes can trigger stage re-evaluation; TC39 deliberately has no permanent rejected state |
| Swift | Vision for a broad topic; roadmap for decomposition | Evolution proposal for one concrete design | Pitch readiness, open review, steering/workgroup decision; substantial revisions are reviewed again | The evolution process reviews design, while implementation uses normal code review; experimental features are outside evolution review | Explicit returned-for-revision, withdrawn, rejected, accepted, and implemented states; reviewed revisions are permalinked; later amendments are recorded |
| Dart | User problem/request plus competing feature proposals | A canonical feature specification | Language-team consensus, then implementation-team sign-offs, experiment, testing, and release readiness | After acceptance, the specification and a separate feature project track semantics and tasks | Working specs evolve in place; accepted specs keep changelogs; work may stop at any point; obsolete accepted specs can be marked abandoned |
| WG21 | Initial design paper | Revised paper with standard wording | Evolution/library-evolution design review, core/library wording review, full committee motion, then ISO ballots | Papers are specification inputs, not implementation plans; implementations may provide evidence but are not tracked as paper tasks | Stable `Pxxxx` identity with `R0`, `R1`, ... revisions and change histories; a later paper can replace an earlier direction; papers can simply cease progressing |

## TC39: explicit maturity stages

### Artifact and breadth

Each TC39 proposal normally owns a repository. Its `README.md` is an explainer of the proposal's
purpose and high-level shape, while later normative text lives separately (normally in `spec.emu`).
The official explainer guide suggests status, motivation, multiple realistic use cases, a
developer-facing description, comparisons with languages or libraries, implementation evidence,
and a Q&A; it deliberately calls those sections optional rather than requiring a uniform monolith
([TC39 explainer guide](https://github.com/tc39/how-we-work/blob/main/explainer.md),
[proposal repository template](https://github.com/tc39/template-for-proposals)).

The early-stage requirements are notably broader than an implementation plan. Stage 0 asks authors
to define a problem space, examine current facilities, study other languages and ecosystems, sketch
possible solutions, and identify challenges. Stage 1 adds the need, general solution shape, key
semantics or abstractions, cross-cutting concerns, and implementation complexity. Only at Stage 2,
after a preferred solution has been selected, are all high-level APIs/syntax and initial normative
specification text required
([TC39 process, stages 0–2](https://tc39.es/process-document/#sec-stages)).

This sequence is the most useful part of TC39 for Silk: **approve sustained exploration of a problem
before requiring a complete solution, and approve a solution direction before requiring normative
text**.

### Lifecycle and gates

TC39 currently uses Stage 0, 1, 2, 2.7, 3, and 4. Each advancement requires committee consensus.
Stage 2.7 says the design and specification are complete enough that further change should come from
tests, implementations, or usage. Stage 3 recommends production implementation and protects
implementer investment by limiting late objections. Stage 4 requires Test262 coverage, two compatible
implementations with in-field experience, and an integration pull request
([TC39 process](https://tc39.es/process-document/)).

Review also becomes more independent as the proposal hardens. TC39 appoints reviewers who are not
the spec-text authors; both those reviewers and the relevant editor group must sign off before Stage
2.7 ([TC39 reviewer rules](https://tc39.es/process-document/#sec-reviewers)). Failure to reach
consensus must leave a rationale in meeting notes and the proposal repository. The process prefers
actionable constraints over a bare veto and, importantly, does not define a permanent “rejected”
state: a champion may rethink a proposal and return later
([TC39 non-consensus rules](https://tc39.es/process-document/#sec-in-cases-where-the-committee-does-not-come-to-consensus)).

### Amendment, withdrawal, and supersession

A champion can request regression to an earlier stage or withdrawal, but both require committee
consensus and a rationale. Another delegate can adopt an unchampioned proposal. A major design change
can cause the committee to reassess the current stage
([TC39 withdrawal and champion rules](https://tc39.es/process-document/#sec-withdrawing-proposals-reverting-to-earlier-stages-and-adopting-proposals)).
The withdrawn Records and Tuples repository is useful evidence that reaching Stage 2 does not make a
direction inevitable; its archived explainer preserves the design and points to its withdrawal rather
than erasing the work
([TC39 Records and Tuples](https://github.com/tc39/proposal-record-tuple)).

### What Silk should borrow and avoid

Borrow:

- stages with distinct evidence thresholds, especially problem-space approval before solution
  approval;
- a readable explainer separate from normative specification text;
- explicit cross-cutting concerns and language/ecosystem comparisons at the first serious gate;
- named champions and independent reviewers;
- regression/withdrawal with rationale, preserving the historical artifact.

Avoid:

- copying six stages when Silk has one small design authority and no independent VM ecosystem;
- treating “accepted direction” as TC39 Stage 3 stability—pre-1.0 Silk must still be able to remove or
  replace a feature after implementation experience;
- requiring multiple implementations or standards-grade wording before accepting a conceptual
  direction;
- the absence of any terminal declined state. Silk needs a visible way to stop zombie proposals while
  still allowing a later proposal to cite and revisit them.

## Swift Evolution: visions, roadmaps, and concrete proposals

### Artifact and breadth

Swift has the clearest answer to the “bigger picture versus narrow change” problem. Its process names
three distinct documents:

- a **vision** establishes a high-level design for a broad topic, shared goals, and a possible program
  of work;
- a **roadmap** explains how a complex change will be decomposed into separately reviewable
  proposals; and
- an **evolution proposal** specifies one concrete design in detail.

A workgroup must approve a vision, but that endorsement does not pre-approve any concrete proposal.
Roadmaps are planning aids and are not themselves reviewed
([Swift process: proposals, roadmaps, and visions](https://github.com/swiftlang/swift-evolution/blob/main/process.md#proposals-roadmaps-and-visions)).

The concrete proposal template asks for a short summary, motivation and current workarounds, proposed
solution, implementation-level detailed design, source and ABI compatibility, adoption implications,
future directions, and alternatives. Header metadata can link the proposal to its encompassing
vision or roadmap, implementation, previous proposal, reviewed revisions, and review history
([Swift language proposal template](https://github.com/swiftlang/swift-evolution/blob/main/proposal-templates/0000-swift-template.md)).

Swift demonstrates that subtraction can be a normal proposal, not a special emergency mechanism.
SE-0003 removed `var` parameters because their limited convenience caused confusion, with staged
warning-then-error migration
([SE-0003](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0003-remove-var-parameters.md)).
That is a useful precedent for Silk, although Silk should require less compatibility ceremony before
1.0.

### Lifecycle and gates

For full evolution review, an author develops a proposal in a public pitch. When the author and
workgroup consider it ready, a review manager opens a dedicated review of a fixed version for at
least ten days. The workgroup may accept, reject, or return it for revision; a substantially new
revision must receive another open review. Review is argument-based, not a vote
([Swift evolution review](https://github.com/swiftlang/swift-evolution/blob/main/process.md#evolution-review)).

Swift records awaiting review, active review, returned for revision, withdrawn, rejected, accepted,
accepted with revisions, previewing, and implemented states. It preserves official prior revisions
as permalinks and standardizes links to pitches, reviews, acceptance, rejection, and amendments
([Swift proposal states and metadata](https://github.com/swiftlang/swift-evolution/blob/main/process.md#proposal-documents)).
SE-0026, “Abstract classes and methods,” is a preserved rejected proposal with links to its pitch,
review, deferral, and final rejection; the rejected artifact still documents the need, design, and
alternatives
([SE-0026](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0026-abstract-classes-and-methods.md)).

### Separation from implementation

Swift states the boundary directly: evolution review covers feature design, while implementation and
user documentation go through normal project code review. Experimental designs may be implemented,
changed, or removed outside evolution review, but must be gated against accidental use and must not
remain experimental forever
([Swift process scope](https://github.com/swiftlang/swift-evolution/blob/main/process.md#scope)).

This is close to Silk's desired division, except that OpenSpec can provide a stronger formal bridge
from an accepted conceptual proposal to exact specification deltas and implementation tasks.

### What Silk should borrow and avoid

Borrow:

- the explicit distinction among broad direction, decomposition map, and focused specification;
- acceptance of a broad direction without implicit acceptance of every child feature;
- fixed reviewed revisions plus an editable working draft;
- review-manager or reviewer ownership separate from authorship;
- explicit returned-for-revision, withdrawn, rejected, and superseded history;
- treating subtraction as ordinary language evolution.

Avoid:

- making every Silk proposal as implementation-ready as a Swift concrete proposal; that would simply
  duplicate OpenSpec;
- importing Swift's ABI and source-compatibility defaults before Silk has a stable release promise;
- using an unreviewed roadmap as the main conceptual artifact. Silk wants the bigger-picture document
  itself to be the reviewable decision.

## Dart: problem requests, evolving feature specifications, and feature projects

### Artifact and breadth

Dart begins with a user issue or feature request that documents the problem. Multiple competing
language features may answer one request. A serious candidate gets a `feature` issue and a Markdown
`working/<feature>/feature-specification.md`, which may be revised in place. Significant changes
normally receive at least one solicited feedback round
([Dart lifecycle: design and feedback](https://github.com/dart-lang/language/blob/main/doc/life_of_a_language_feature.md#design-feedback-and-iteration)).

Before shipping, Dart expects the feature specification to cover grammar, static typing, inference,
runtime semantics, interactions with the rest of the language, and tooling. It also weighs ongoing
engineering cost and the cognitive load imposed on users, not just implementability
([Dart lifecycle overview](https://github.com/dart-lang/language/blob/main/doc/life_of_a_language_feature.md#lifecycle-of-a-language-feature)).

The accepted Records specification is a representative artifact: it sets the feature in a larger
records/patterns family, specifies syntax and semantics, and keeps a detailed changelog. Its history
includes removing APIs, reflective operations, ambiguous syntax, and single-element records during
design—not merely accumulating additions
([Dart Records specification and changelog](https://github.com/dart-lang/language/blob/main/accepted/3.0/records/feature-specification.md#changelog)).

### Lifecycle, gates, and implementation separation

Language-team consensus accepts a specific proposal and assigns a shepherd. From that point, Dart
tracks two distinct artifacts:

1. the canonical feature specification, which remains editable when implementation uncovers missing
   cases or infeasible behavior; and
2. a feature project, whose meta issue links all implementation issues and cross-team tasks.

Parser/front-end, developer-experience, VM, web, and Wasm teams then give implementation-readiness
sign-off. Most features are built behind a flag, tested under both old and new language versions, and
only enabled after migration and release checks
([Dart acceptance and feature-project split](https://github.com/dart-lang/language/blob/main/doc/life_of_a_language_feature.md#acceptance),
[Dart implementation sign-offs](https://github.com/dart-lang/language/blob/main/doc/life_of_a_language_feature.md#kick-off-meetings)).

The process may stop a feature at any point. After shipping, the feature specification moves from
`working` into a versioned `accepted` directory. The accepted-spec index distinguishes ongoing,
specified, done, and abandoned specifications; “abandoned” explicitly covers an unreleased design
replaced by another specification
([Dart accepted-feature status model](https://github.com/dart-lang/language/tree/main/accepted#feature-specification-status)).

### What Silk should borrow and avoid

Borrow:

- separate problem statements from candidate solutions, allowing competing proposals to share one
  diagnosed need;
- a canonical design artifact distinct from the implementation project;
- editable accepted direction when implementation reveals a real semantic hole;
- explicit assessment of grammar, types, inference, runtime, tools, backends, user cognition, and
  maintenance burden;
- a visible abandoned/superseded state rather than silently deleting unsuccessful work.

Avoid:

- calling the conceptual artifact a “feature specification”; for Silk that name should remain on the
  post-acceptance OpenSpec side of the boundary;
- adopting Dart's large cross-team sign-off checklist before the project has those teams;
- allowing an accepted conceptual direction to drift indefinitely in place. Silk should require a
  new review when an amendment changes the core tradeoff, scope, or interaction model.

## WG21: revisioned papers through design and wording groups

### Artifact and breadth

WG21 papers have a stable document identity and explicit revisions: `PxxxxR0`, `PxxxxR1`, and so on.
Headers identify date, authors/reply-to, and target audience; non-trivial papers should have an
abstract, and revisions should include a change history
([WG21 SD-7](https://isocpp.org/std/standing-documents/sd-7-mailing-procedures-and-how-to-write-papers)).
The public proposal guide recommends beginning with the problem and alternatives, circulating a
draft, iterating toward support, and only then submitting a numbered paper. Its library template
expects a high-level introduction and progressively more complete technical specification, with
formal standard wording allowed to arrive in a revision
([WG21 proposal guide](https://isocpp.org/std/submit-a-proposal)).

### Lifecycle and gates

WG21 has a process pipeline rather than a single proposal status ladder. Evolution Working Group or
Library Evolution Working Group first decides whether to explore a problem, chooses and refines a
design, then asks for wording. Core Working Group or Library Working Group reviews the wording, and
the full committee votes to apply it to the working draft. ISO ballot and publication follow at the
standard level
([life of a WG21 proposal](https://isocpp.org/std/the-life-of-an-iso-proposal)).

P2786R13, “Trivial Relocatability for C++26,” is a representative successful paper: the same `P2786`
identity accumulated thirteen reviewable revisions, recorded design and wording-group decisions, and
was marked adopted in the official 2025 paper index
([P2786R13](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2025/p2786r13.html),
[2025 WG21 paper index](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2025/)).
Rejected directions are often preserved as paper history rather than a registry-level terminal
status; later papers can revisit them with new evidence. This makes the archive valuable, but the
state of an old proposal can require reading indices, minutes, and successor papers.

### Separation from implementation

The design/wording split is strong, but both sides still live in papers. The paper proposes changes
to the standard; it does not serve as an implementation task tracker. Implementations, benchmarks,
and field practice can be evidence, while the acceptance mechanism remains design review, wording
review, committee motions, and ballots.

### What Silk should borrow and avoid

Borrow:

- stable proposal identity with named revisions and a concise change history;
- explicit audience or affected-system metadata;
- a “design decision before wording” gate that maps naturally to “proposal before OpenSpec”;
- review records that show which concerns caused each substantive revision.

Avoid:

- a general paper format so loose that every author invents the structure;
- proposal status discoverable only through meeting minutes or external indices;
- requiring synchronous presentation by a champion to make progress;
- allowing a broad change to become one enormous specification paper merely because one identifier
  is convenient.

## Recommended Silk shape

### One reviewable conceptual artifact

Call the artifact a **Language Proposal** (or **Silk Proposal**), not a specification. It should be
broad enough to judge coherence but bounded enough to have one central design thesis. When a topic is
larger than one thesis, use an endorsed **direction** document above several proposals, following the
Swift vision model; do not force one proposal to become a language manifesto.

A proposal should answer “should Silk move in this direction, and does it improve the language as a
whole?” It should not contain implementation task breakdowns, file lists, or acceptance-test plans.
Those belong to the OpenSpec changes created after acceptance.

### Proposed required sections

1. **Metadata** — stable ID, title, authors/champion, reviewers, status, revision, created/updated
   dates, affected surfaces, and predecessor/supersession links.
2. **Summary** — the direction and its central tradeoff in a short paragraph.
3. **Problem and evidence** — real programs or compiler/library experience that demonstrate the
   problem; distinguish user need from the proposed mechanism.
4. **Goals and non-goals** — include what the proposal deliberately refuses to solve.
5. **Current language model** — the existing concepts, workaround, and complexity being changed.
6. **Proposed language model** — concepts, terminology, invariants, and representative source
   examples; exact grammar or exhaustive edge cases are optional until OpenSpec.
7. **Whole-language interaction map** — explicitly examine syntax/parsing, types and inference,
   Effects and failure, ownership/lifetimes/allocation, services/interfaces/intrinsics, modules and
   visibility, standard library, evaluation/runtime, native and Wasm lowering, diagnostics/tooling,
   and teachability. Mark a surface “not affected” with a reason rather than omitting it.
8. **Complexity and subtraction budget** — what concepts, syntax, special cases, compiler privilege,
   or library surface this adds; what it removes or makes unnecessary. A subtractive proposal uses
   the same section and is not required to pretend to be additive.
9. **Alternatives and prior art** — include the status quo, at least one smaller primitive or library
   solution, and the strongest competing language design.
10. **Risks and falsifiers** — conditions or prototype results that would cause revision,
    withdrawal, or rejection.
11. **Realization map** — the expected OpenSpec slices and their dependencies, named only at the
    capability level. Acceptance does not accept those child specs in advance.
12. **Revision and decision record** — substantive changes, review constraints, resolution, and
    final rationale.

### A lightweight lifecycle

Use four main states, with a fixed reviewed revision at every gate:

1. **Draft** — author-controlled exploration; no project endorsement.
2. **Exploring** — the problem is worth attention, a champion exists, and reviewers agree the
   proposal covers the relevant solution space and cross-cutting surfaces. This endorses the problem,
   not the selected solution.
3. **Accepted direction** — reviewers accept the conceptual model and tradeoff as the basis for one
   or more OpenSpec changes. It is still pre-1.0 direction, not a compatibility promise or declaration
   that implementation must ship unchanged.
4. **Retired** with a reason: **withdrawn**, **declined**, or **superseded**. A revised idea may return
   as a new proposal that links the retired one; history is never erased.

“Implemented” should be orthogonal metadata, not a proposal maturity stage. The proposal decides a
direction; OpenSpec and repository state report realization. This prevents an implemented experiment
from masquerading as an accepted language design and prevents a good conceptual proposal from being
judged by task completion.

### Review gates

At **Exploring**, require:

- an identified champion;
- a demonstrated problem and current workaround;
- credible competing solution shapes;
- a first whole-language interaction map; and
- explicit complexity/subtraction accounting.

At **Accepted direction**, require:

- a fixed revision reviewed by at least one non-author;
- resolved or explicitly accepted cross-cutting constraints;
- coherent examples that exercise the proposal with existing Silk concepts;
- a reasoned choice over alternatives;
- named falsifiers and a rollback/removal story; and
- a plausible decomposition into OpenSpec changes without embedding their task lists.

The decision should be written as an argument, not a vote tally. A decline should say whether the
problem was unconvincing, the direction conflicted with a language invariant, the complexity budget
was too high, or evidence was insufficient. “Declined for now” is preferable to leaving an inactive
proposal indefinitely open.

### Amendment policy for a pre-1.0 language

- Draft and Exploring proposals may change in place, with a concise revision log.
- Formal review targets an immutable commit/revision.
- Editorial corrections and OpenSpec-discovered clarifications may amend an Accepted direction in
  place and must be logged.
- A change to the central model, scope, tradeoff, or cross-cutting effects requires renewed review or
  a superseding proposal.
- Implementation evidence may legitimately send an Accepted direction back to Exploring or retire it.
- No accepted proposal creates a pre-1.0 compatibility entitlement. If removal yields a more coherent
  eventual language, subtraction remains available through the same proposal process.

## Bottom line

The best base for Silk is **Swift's vision/proposal distinction plus TC39's problem-before-solution
gates, Dart's specification/project separation, and WG21's revision identity**. The key local
adaptation is to make “Accepted direction” intentionally weaker than standards-track finality:
strong enough to justify detailed OpenSpec work, but explicitly reversible when prototypes,
implementation, or whole-language analysis reveal that Silk would be simpler without it.
