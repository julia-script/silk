# Governance models for conceptual language proposals

Research date: 2026-08-14. Sources are the official Kubernetes enhancements repository, the RFC
Editor and IETF, and the official Python PEP repository.

## Finding

Silk should not copy any one of these systems. The useful combination is:

- **Python PEPs for the document and decision core:** one durable design argument, explicit type
  and status, a champion, one canonical discussion, recorded objections, rejected alternatives,
  and a linked resolution;
- **Kubernetes KEPs for system-wide review:** named owning and affected areas, distinct reviewers
  and decision makers, explicit non-goals, cross-cutting impact analysis, and evidence gates; and
- **IETF RFCs for maturity and history:** distinguish an evolving proposal from an accepted
  publication, make risk sections explicit even when the answer is “not applicable,” and replace
  accepted decisions through a new document that updates or supersedes the old one.

The proposal should remain a **conceptual decision record**, not become an implementation plan.
Acceptance should mean “this is the language direction we intend,” after which one or more OpenSpec
changes define implementation deltas and tasks. Evidence such as prototypes can inform acceptance,
but tasks, milestones, test inventories, and release checklists belong downstream.

## Comparison

| System | Best mechanism to borrow | Main mismatch for Silk | Can remain conceptual? |
| --- | --- | --- | --- |
| Kubernetes KEP | Cross-area ownership, risks, graduation evidence, operational review | The template combines product requirements, design, implementation tracking, testing, and release readiness | Only in its early `provisional` state; the standard template quickly becomes implementation-heavy |
| IETF RFC process | Separate publication categories and maturity; mandatory risk review; immutable publication with explicit updates/obsolescence | A Proposed Standard is already stable and deployable, not an early idea; the full consensus machinery is far too large | Internet-Drafts, Informational, and Experimental documents can, but Standards Track documents must become precise specifications |
| Python PEP | Lightweight durable design rationale; editorial/substantive review split; explicit resolution and rejected ideas | Standards Track PEPs normally converge on a specification and reference implementation, while Informational PEPs cannot propose a feature | Yes during Draft, and permanently for Informational/Process material; a feature PEP eventually becomes implementation-linked |

## Kubernetes Enhancement Proposals

### What the system is optimized for

The KEP process exists because an issue or pull request is too small an abstraction for changes
that cross project boundaries or releases. Its stated goals include clear motivation, explicit
stability milestones and graduation criteria, durable project history, and representation of all
stakeholders. A KEP deliberately combines a feature/effort tracker, product requirements document,
and design document. That combination is valuable for Kubernetes delivery, but is also the reason
it is a poor direct template for Silk's pre-implementation layer.
([KEP-0000: process](https://github.com/kubernetes/enhancements/blob/master/keps/sig-architecture/0000-kep-process/README.md))

### Reusable mechanisms

- **One owner, all affected areas named.** A cross-cutting KEP has one owning SIG, participating
  SIGs, reviewers, and approvers drawn from impacted SIGs. The owner drives the document, but does
  not get to define another area's impact unilaterally. The process also separates authors,
  reviewers, approvers, and an editor who keeps the proposal moving.
  ([KEP metadata and stewardship](https://github.com/kubernetes/enhancements/blob/master/keps/sig-architecture/0000-kep-process/README.md#stewardship))
- **A state that is safe for incomplete thinking.** `provisional` means the proposal is actively
  being defined; `implementable` requires approver agreement; terminal/history states include
  `rejected`, `withdrawn`, and `replaced`, while `deferred` preserves work that is not active.
  Replacement is explicit and bidirectional through `replaces` and `superseded-by` metadata.
  ([KEP workflow](https://github.com/kubernetes/enhancements/blob/master/keps/sig-architecture/0000-kep-process/README.md#kep-workflow))
- **Prompt for the negative space.** The template asks for goals, non-goals, risks and mitigations,
  drawbacks, and alternatives. These prevent a proposal from presenting only its happy path.
  ([KEP template](https://github.com/kubernetes/enhancements/blob/master/keps/NNNN-kep-template/README.md))
- **Evidence-gated maturity.** Graduation criteria make later confidence an explicit burden rather
  than an assumption. Production-readiness questions cover enablement, rollback, defaults,
  monitoring, SLOs/SLIs, dependencies, scalability, resource exhaustion, and troubleshooting.
  ([KEP production-readiness questionnaire](https://github.com/kubernetes/enhancements/blob/master/keps/NNNN-kep-template/README.md#production-readiness-review-questionnaire))

For Silk, “affected areas” should be semantic rather than organizational: syntax, name resolution,
types, contracts/effects and services, ownership/lifetimes, evaluation, modules, FFI/platforms,
tooling, standard library, diagnostics, and teaching. A proposal author should mark each one
**affected**, **not affected**, or **unknown**, with a short explanation. Unknowns are legitimate in
Draft but must be resolved or explicitly accepted as risks before acceptance.

### What not to copy

- Do not merge a release signoff checklist, unit/integration/e2e test inventory, infrastructure
  needs, and implementation history into the conceptual proposal. Those sections are excellent for
  delivery and would turn the Silk document into the OpenSpec artifact it is meant to precede.
- Do not make `provisional` imply that the owning group has already accepted that the work “must be
  done,” as Kubernetes does. Silk needs a cheaper **Exploring** state where the problem and even the
  desirability of solving it remain contestable.
- Do not require every operational question for every language idea. Use a small applicability
  matrix, then require deeper sections only for marked areas. A mandatory unexplained checklist
  encourages boilerplate; an explicit “not applicable because…” records actual consideration.
- Avoid role theater in a small project. Author/champion, reviewers, decision maker, and editor are
  useful hats, but one person may hold multiple hats if that is declared. The irreducible separation
  is between editorial admission and the substantive acceptance decision.

## IETF Internet-Draft and RFC lifecycle

### What the system is optimized for

An Internet-Draft is an evolving working document for review and revision and has no formal status.
Publication as an IETF-stream RFC requires rough consensus; Standards Track action also goes
through community review and IESG evaluation of the applicable maturity criteria, technical
quality, and clarity.
([RFC 2026, Internet-Drafts and standards actions](https://www.rfc-editor.org/rfc/rfc2026.html#section-2.2),
[RFC 8789, rough consensus](https://www.rfc-editor.org/rfc/rfc8789.html))

The current Standards Track has two maturity levels: Proposed Standard and Internet Standard. A
Proposed Standard is already stable, has resolved known design choices, has significant community
review, has no known technical omissions, and is good enough to deploy; implementation experience
is desirable but normally not required. Internet Standard adds significant implementation and
successful operational experience. Thus “Proposed Standard” is a misleading label for Silk's raw
proposal stage—it is closer to **Accepted specification**.
([RFC 6410, two maturity levels](https://www.rfc-editor.org/rfc/rfc6410.html#section-2),
[RFC 7127, Proposed Standard criteria](https://www.rfc-editor.org/rfc/rfc7127.html#section-3))

### Reusable mechanisms

- **Type and maturity are separate dimensions.** Standards Track is distinct from Experimental,
  Informational, Historic, and Best Current Practice. Informational records may disseminate useful
  material without claiming consensus or recommendation; Experimental records research and
  development; BCP captures ratified process or practice.
  ([RFC 2026 categories](https://www.rfc-editor.org/rfc/rfc2026.html#section-4.2),
  [BCP category](https://www.rfc-editor.org/rfc/rfc2026.html#section-5))
- **Risk review is part of the document, not an afterthought.** RFCs require a Security
  Considerations section. The IETF operations guidance similarly asks authors to consider
  deployment, coexistence, defaults, upgrade paths, failure recovery, monitoring, scalability,
  dependencies, and manageability from the start. If there are no new manageability concerns, it
  recommends saying so and briefly explaining why.
  ([RFC 3552, security considerations](https://www.rfc-editor.org/rfc/rfc3552.html#section-1),
  [RFC 5706, operations and management](https://www.rfc-editor.org/rfc/rfc5706.html))
- **Published decisions are stable references.** A revision of an established standard runs
  through the process as a new specification; the new document can update or obsolete the old one,
  and the relationship is explicit. Published RFC text is not silently rewritten.
  ([RFC 2026, revising a standard](https://www.rfc-editor.org/rfc/rfc2026.html#section-6.3),
  [RFC 7322, Updates and Obsoletes](https://www.rfc-editor.org/rfc/rfc7322.html#section-4.1.4))
- **Cross-discipline review can be escalated by impact.** The IESG can commission independent
  technical review for specifications with unusually broad potential impact; the Proposed
  Standard criteria can also demand implementation or operational evidence when a change affects
  core protocols or has significant operational consequences.
  ([RFC 2026, IESG review](https://www.rfc-editor.org/rfc/rfc2026.html#section-6.1.2),
  [RFC 7127, impact-sensitive evidence](https://www.rfc-editor.org/rfc/rfc7127.html#section-3.1))

For Silk, the valuable distinction is not a miniature standards bureaucracy but three proposal
types: **Feature direction** (a change to the language or standard library), **Process** (a change to
how Silk is specified or governed), and **Informational** (a design landscape, terminology, or
guidance that makes no feature decision). Type should not encode maturity; all three can move
through their own statuses.

### What not to copy

- Do not call early work an RFC or “Proposed Standard.” Both imply more consensus and precision
  than a pre-1.0 conceptual exploration has earned.
- Do not create multiple publication streams, formal appeals, area directors, or an IETF-scale
  consensus ritual. A named decision maker who publishes a reasoned resolution is sufficient.
- Do not make accepted proposal text literally immutable if small clarifications are needed. Keep
  editorial corrections possible, but require a new proposal for semantic changes and record the
  supersession relation. The goal is stable meaning, not archival inconvenience.
- Do not confuse document maturity with feature maturity. A conceptual direction may be accepted
  while its implementations are still experimental; implementation maturity should live in the
  downstream OpenSpec changes and release policy.

## Python PEP distinctions and workflow

### What the system is optimized for

A PEP is a durable design document for a major feature, information, or a process change. It is
intended to collect community input and preserve the design decisions, with the author responsible
for consensus-building and documenting dissent. PEPs are divided into Standards Track,
Informational, and Process types.
([PEP 1: purpose and types](https://peps.python.org/pep-0001/#what-is-a-pep))

This is the lightest close fit for Silk. PEP 1 separates **editorial admission**—sound, complete,
properly motivated, correctly structured—from **substantive acceptance** by a Steering Council or
delegate. Publication of a Draft is explicitly not acceptance. Each proposal has a champion, may
have a sponsor, links one canonical public discussion, and records its decision maker.
([PEP submission](https://peps.python.org/pep-0001/#submitting-a-pep),
[PEP review and resolution](https://peps.python.org/pep-0001/#pep-review-resolution))

### Reusable mechanisms

- **The proposal is an argument, not a ticket.** Required content includes motivation,
  specification, rationale, alternatives, important objections, compatibility impact, security
  implications, teaching, rejected ideas, and open issues. Rejected ideas include reasons so later
  participants do not unknowingly replay the same branch.
  ([PEP 1: successful PEP contents](https://peps.python.org/pep-0001/#what-belongs-in-a-successful-pep))
- **The decision is durable and attributable.** Draft, Accepted, Provisional, Deferred, Rejected,
  Withdrawn, Final, and Superseded states distinguish inactivity, author withdrawal, rejection by
  the decision authority, and replacement. Accepted/rejected/withdrawn PEPs link a public
  resolution. `Replaces` and `Superseded-By` preserve lineage.
  ([PEP statuses and resolution](https://peps.python.org/pep-0001/#pep-review-resolution),
  [PEP header metadata](https://peps.python.org/pep-0001/#pep-header-preamble))
- **Drafts evolve; resolved decisions become history.** Substantive draft changes return to the
  canonical discussion, and a major rewrite normally starts a new discussion thread. Once resolved,
  a PEP is a historical design record; normative behavior moves into the language or library
  specification rather than treating the proposal as the eternal source of truth.
  ([changing PEPs](https://peps.python.org/pep-0001/#changing-existing-peps),
  [PEP maintenance](https://peps.python.org/pep-0001/#pep-maintenance))
- **Ownership can transfer without erasing authorship.** A proposal can gain a new champion when
  the original author no longer has time or interest. Disagreement with the author is not grounds
  to seize the document; a competing proposal remains possible.
  ([transferring PEP ownership](https://peps.python.org/pep-0001/#transferring-pep-ownership))

### What not to copy

- Python recommends one narrowly focused key idea and normally couples a Standards Track PEP to a
  reference implementation. Silk's stated problem is that narrow proposals can optimize one
  feature without explaining how it composes with the rest of the language. Retain **one coherent
  decision**, but require the semantic impact matrix and permit a proposal to reshape several
  features when they form one model.
- Do not use “Silk-like” or taste alone as the acceptance test. PEP 1 ultimately includes
  “pythonic” among its criteria. Silk should instead publish explicit project principles and ask
  reviewers to show how the proposal improves coherence, expressive power, minimality,
  implementability, diagnosability, teachability, and performance cost.
- Do not copy a backward-compatibility presumption into a project that explicitly welcomes
  breaking changes. Replace that section with **Displacement and migration**: what existing
  concepts, syntax, implementation work, tests, or user knowledge become obsolete, and whether the
  subtraction leaves a simpler whole.
- Do not reserve “Provisional” for post-implementation inclusion. Silk needs to distinguish an
  unfinished conceptual draft from a direction accepted with named validation questions. Call the
  latter **Accepted with conditions** or **Trial direction**, and require observable exit criteria.

## Recommended mechanisms for Silk

### Document identity and ownership

Every proposal should have a stable number and title, type, status, author/champion, decision maker,
reviewers, created/updated dates, canonical discussion, affected semantic areas, dependencies, and
`replaces`/`superseded-by` links. Assigning a number means “admitted for consideration,” not
“endorsed.” Keep the author as the document owner during Draft; let ownership transfer explicitly
if the proposal is orphaned.

### Minimal lifecycle

1. **Exploring** — the problem, desired properties, and design space are being established; open
   questions and unknown impacts are expected.
2. **Candidate** — editorial review says the argument is coherent and complete enough for a
   decision; affected areas and reviewers are named; unresolved questions are either answered or
   explicitly accepted as risks.
3. **Accepted** or **Accepted with conditions** — a published resolution explains why the direction
   is a net improvement and identifies conditions or validation evidence, if any. Acceptance
   authorizes creation of one or more OpenSpec changes; it does not itself create tasks.
4. **Rejected**, **Withdrawn**, or **Deferred** — preserve the argument and resolution without
   implying implementation work.
5. **Superseded** — a new proposal changes or replaces an accepted semantic decision and links both
   directions. Small editorial corrections do not require supersession; meaning changes do.

“Implemented” and “Final” should not be conceptual-proposal statuses. They collapse the boundary
with OpenSpec and release maturity. The proposal index can instead derive implementation links and
state from related OpenSpec changes.

### Acceptance review

The review should ask whether the proposal:

- states the user/language-design problem and desired properties before presenting syntax;
- describes a coherent semantic model, not only examples or surface notation;
- accounts for additive **and subtractive** effects on existing language concepts;
- maps interactions across every semantic area, including unknowns and tensions;
- identifies invariants that downstream specifications must preserve;
- compares credible alternatives and records why rejected branches lose;
- has a risk analysis covering security, soundness, resource behavior, performance, diagnostics,
  tooling, interoperability, and teaching where applicable;
- is implementable in principle, with a prototype or research evidence required only when the
  reviewers identify a concrete feasibility uncertainty;
- defines success and disconfirmation evidence without decomposing that evidence into tasks; and
- makes the whole language simpler, more orthogonal, or more capable by enough to justify the new
  conceptual weight and displaced work.

### Boundary with OpenSpec

An accepted proposal should provide OpenSpec with **constraints and rationale**, not a backlog:

- the accepted semantic direction and invariants;
- the affected current specs and concepts;
- decisions that are closed versus validation questions that remain open;
- explicitly rejected alternatives that implementation changes must not accidentally reintroduce;
- evidence gates that may require separate experiments; and
- links from each resulting OpenSpec change back to the proposal.

If implementation exposes a fundamental conceptual flaw, update the proposal's resolution and
create a competing or superseding proposal. Do not silently let implementation tasks redefine the
language direction.

## Anti-patterns to guard against

1. **The implementation plan wearing a rationale section.** Long task lists make the conceptual
   model hard to review and cause acceptance to hinge on delivery estimates.
2. **The syntax-first proposal.** Concrete notation creates false progress while interactions with
   types, effects, ownership, evaluation, modules, and diagnostics remain unspecified.
3. **Checklist compliance without analysis.** Empty “N/A” answers and copied risk boilerplate hide
   uncertainty. Require a reason for non-applicability and allow “unknown” while exploring.
4. **One giant roadmap proposal.** Cross-cutting does not mean unbounded. A proposal needs one
   coherent decision and explicit non-goals, even when its consequences touch many features.
5. **One tiny local optimization.** A proposal that cannot explain its effect on the semantic
   matrix is not ready merely because its isolated syntax or implementation looks elegant.
6. **Consensus by document merge.** Editorial admission, community support, and the decision
   authority's acceptance are different events and should have different evidence.
7. **Mutable history.** Rewriting an accepted proposal to match a later direction destroys the
   reasoning trail. Clarify in place; change meaning through a linked superseding proposal.
8. **Maturity inflation.** “Accepted” says the conceptual direction won, not that the feature is
   stable, implemented, performant, or ready for users.

