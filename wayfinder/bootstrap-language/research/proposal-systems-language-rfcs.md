# Language proposal systems: Rust RFCs, Python PEPs, and Swift Evolution

Research date: 2026-08-14. Sources are first-party process documents, templates, and repositories.

## Executive synthesis

Silk should not copy any one system wholesale.

- **Swift's two-level model is the closest fit for the problem Silk is trying to solve.** A Swift
  *vision* gives a broad area a shared problem statement, terminology, goals, and basic direction;
  approval strongly endorses the problem and goals, less strongly endorses the approach, and does
  not pre-approve concrete proposals. Accepted visions then become foundations that narrower
  proposals link back to. This is almost exactly the desired boundary between a conceptual Silk
  proposal and later OpenSpec changes. [Swift vision process](https://forums.swift.org/t/the-role-of-vision-documents-in-swift-evolution/62101)
- **Rust has the best feature-proposal writing frame.** Its template forces both a guide-level
  account of how a programmer experiences and learns the feature and a reference-level account of
  semantics, interactions, corner cases, and implementability. It separately asks for drawbacks,
  alternatives, prior art, unresolved questions, and holistic future effects. [Rust RFC template](https://github.com/rust-lang/rfcs/blob/master/0000-template.md)
- **Python has the clearest lightweight recordkeeping.** A PEP names its champion, sponsor or
  delegate where applicable, canonical discussion, status, dependencies, supersession, and decision
  resolution. It preserves rejected ideas and open issues, and treats resolved PEPs as historical
  design records rather than the canonical language reference. [PEP 1](https://peps.python.org/pep-0001/),
  [PEP 12 template](https://peps.python.org/pep-0012/)

The recommended Silk system is therefore a **Swift-style conceptual layer, authored with a reduced
Rust-style template, governed with Python-style metadata and resolution records**. An accepted
proposal should remain an accepted design-direction artifact; its implementation status should live
in linked OpenSpec changes rather than changing the proposal into a task plan.

## Comparison at a glance

| Dimension | Rust RFCs | Python PEPs | Swift Evolution | Fit for small pre-1.0 Silk |
| --- | --- | --- | --- | --- |
| Unit of change | A substantial change; semantic/syntactic additions and removals qualify | One focused key proposal; overly broad documents may be rejected | A focused proposal, optionally grounded in a broader approved vision or roadmap | Use a coherent language direction, potentially decomposed into several future OpenSpec changes |
| Core document shape | Summary; motivation; guide-level explanation; reference-level explanation; drawbacks; rationale and alternatives; prior art; unresolved questions; future possibilities | Abstract; motivation; specification; rationale; compatibility; security; how to teach; reference implementation; rejected ideas; open issues | Summary; motivation; proposed solution; detailed design; compatibility/adoption; future directions; alternatives | Prefer Rust's two explanatory levels, add explicit goals/non-goals and whole-language fit, omit stable-language ceremony |
| Whole-language coherence | Future possibilities explicitly asks about holistic effects, roadmap fit, and natural evolution | Motivation must fit Python philosophy and the community, but the process prefers narrow documents | Vision documents exist specifically to establish terminology, conceptual models, goals, and a program of work across later proposals | Make this a required section and allow a proposal to be broader than one implementation change |
| Open questions | Separates questions resolved before RFC merge, during implementation before stabilization, and future independent work | `Open Issues` records remaining decisions | Review can return a proposal for revision; future directions are scoped away from the accepted proposal | Divide acceptance blockers from questions deliberately delegated to OpenSpec/prototyping |
| Decision owner | Relevant sub-team owns final decision after final-comment period | Steering Council or a named PEP-Delegate | Evolution workgroup decides; a review manager runs review but is not a vote counter | Name one decision owner; do not invent councils or quorum for a tiny project |
| Rejection/postponement | Closed with rationale; `postponed` means “not evaluating or implementing now” and may be reopened | Rejected and Withdrawn remain records; Deferred can return to Draft; resolution URL is recorded | Rejected, Withdrawn, or Returned for revision are explicit states | Preserve every outcome and require a concrete `Revisit when` trigger for deferral |
| Concept-to-code handoff | Accepted RFC becomes active; a separate issue tracks implementation and priority | Accepted requires the reference implementation to be completed before Final; canonical reference docs live elsewhere | Proposal links implementation PRs and moves Accepted to Implemented; language proposals require a usable prototype before review | Link accepted proposal to one or more OpenSpec changes; do not require implementation before conceptual acceptance |

## Rust RFCs

### Document and coherence model

Rust's template is unusually good at checking whether an idea works as a language feature rather
than merely as compiler work:

- `Motivation` must describe a real user problem and concrete use cases.
- `Guide-level explanation` teaches the feature as if it already existed, including examples,
  programmer mental models, diagnostics or migration where relevant, and effects on readability and
  maintenance.
- `Reference-level explanation` makes interactions with other features clear, gives enough detail
  to make implementation plausible, and dissects corner cases.
- `Drawbacks` asks the author to argue against the proposal.
- `Rationale and alternatives` asks why this point in the design space is best, what was rejected,
  what happens if nothing changes, and whether a language feature belongs in a library or macro.
- `Prior art` asks for both positive and negative lessons rather than precedent as authority.
- `Unresolved questions` distinguishes pre-acceptance decisions, implementation/stabilization
  decisions, and independently deferrable work.
- `Future possibilities` explicitly asks for natural evolution, effects on the language and project
  as a whole, and roadmap fit. [Rust RFC template](https://github.com/rust-lang/rfcs/blob/master/0000-template.md)

That last section was added specifically to counter narrow proposal thinking. The accepted
meta-RFC says the prompt should help authors, teams, and readers understand the proposal's
long-term effects and how it fits the product vision, while warning that possible future work is not
itself accepted. [Rust RFC 2561](https://rust-lang.github.io/rfcs/2561-future-possibilities.html)

### Lifecycle, ownership, and handoff

A proposal is revised publicly in a pull request and assigned to the relevant sub-team. A team
member eventually proposes a final-comment period with a disposition of merge, close, or postpone;
all sub-team members sign off before the ten-day period begins. The process seeks informed
consensus, but does not require unanimity among everyone in the discussion. The relevant sub-team
makes the final decision and records rationale when it is not already clear. [Rust RFC process](https://github.com/rust-lang/rfcs/blob/master/README.md#what-the-process-is)

Acceptance makes an RFC `active`: major stakeholders agree in principle, but acceptance neither
assigns priority nor guarantees final inclusion. Every accepted RFC gets a separate implementation
issue, and substantial later design changes require another RFC. [Rust RFC lifecycle and implementation](https://github.com/rust-lang/rfcs/blob/master/README.md#the-rfc-life-cycle)

Postponement is meaningfully distinct from rejection: it records that the project does not want to
evaluate or implement the idea *now*, but might reopen it when the time is right. Rust historically
used this for work that could wait until after 1.0. [Rust RFC postponement](https://github.com/rust-lang/rfcs/blob/master/README.md#rfc-postponement)

### What Silk should borrow or avoid

Borrow the guide/reference split, adversarial `Drawbacks`, staged unresolved questions, and
holistic future-effects prompt. Avoid Rust's team/FCP machinery: it solves coordination and
legitimacy problems of a large distributed project, not Silk's current bottleneck. Also avoid
treating “active” as a combined design and delivery status; OpenSpec already supplies the delivery
layer.

## Python PEPs

### Document and coherence model

PEP 1 defines a PEP as both a concise technical specification and a rationale, and makes the author
responsible for building consensus and documenting dissent. Its template records author, sponsor,
delegate, canonical discussion, status, dependencies, replacement/supersession, and a resolution
link. [PEP purpose and headers](https://peps.python.org/pep-0001/#what-is-a-pep)

The standard sections are `Abstract`, `Motivation`, `Specification`, `Rationale`, `Backwards
Compatibility`, `Security Implications`, `How to Teach This`, `Reference Implementation`, `Rejected
Ideas`, and `Open Issues`. The specification must be precise enough for interoperable
implementations; rationale must record alternatives and significant objections; rejected ideas
preserve reasoning and prevent repeated dead-end discussion. [PEP contents](https://peps.python.org/pep-0001/#what-belongs-in-a-successful-pep),
[PEP template](https://peps.python.org/pep-0012/#suggested-sections)

Python is less suitable as the primary model for Silk's “bigger picture” goal. PEP 1 explicitly
recommends one focused key proposal and permits editors to reject proposals that are too broad. It
does, however, require the champion to test whether an idea applies to the wider community rather
than only to the author. [PEP idea stage](https://peps.python.org/pep-0001/#start-with-an-idea-for-python)

### Lifecycle, ownership, and handoff

PEP editors administer format and status but do not accept content. The elected Steering Council is
the final authority and may delegate a specific PEP to a named expert who can accept or reject it.
If no suitable decision maker exists, the PEP is Deferred. [PEP review and resolution](https://peps.python.org/pep-0001/#pep-review-resolution)

The lifecycle distinguishes Draft, Accepted, Provisional, Deferred, Rejected, Withdrawn, Final, and
Superseded. Accepted work becomes Final only after its reference implementation is merged. Rejected
and Withdrawn PEPs remain as records with decision links; Deferred PEPs can return to Draft. A
resolved PEP becomes a historical design document, while canonical behavior moves to the language
or library reference. [PEP statuses and maintenance](https://peps.python.org/pep-0001/#pep-review-resolution)

### What Silk should borrow or avoid

Borrow explicit author/champion, decision owner, status, dependency, supersession, canonical
discussion, and resolution fields. Borrow the distinction between the design-history artifact and
canonical reference/specification. Avoid Python's compatibility emphasis as a gate: Silk's
pre-1.0 policy encourages breaking changes. Replace it with a neutral section that inventories what
is added, removed, invalidated, simplified, and migrated.

## Swift Evolution

### The important distinction: visions and proposals

Swift is the most relevant precedent because it has two design altitudes.

An evolution *vision* pairs a long-form account of an area's current state with a high-level future
direction. It establishes common terminology, conceptual models, and expectations for later design
work. Official approval has deliberately graduated force: it strongly endorses the current-state
analysis and goals, somewhat endorses the overall approach, and only weakly endorses concrete
future proposals. Those proposals still undergo ordinary review and may change or be rejected.
Approved visions act as foundations linked from later pitches and proposals, but they are design
artifacts rather than canonical language documentation. [Role of Swift vision documents](https://forums.swift.org/t/the-role-of-vision-documents-in-swift-evolution/62101)

The main process now defines a vision as a high-level design for a broad topic that creates shared
understanding, goals, and a possible program of work; approving it does not approve any concrete
proposal. [Swift Evolution process](https://github.com/swiftlang/swift-evolution/blob/main/process.md#proposals-roadmaps-and-visions)
An accepted concurrency vision demonstrates the shape in practice: explicit goals, prioritized use
cases, a progressive-disclosure model, out-of-scope issues, performance constraints, risks, and
multiple connected future language changes. [Approachable Concurrency vision](https://github.com/swiftlang/swift-evolution/blob/main/visions/approachable-concurrency.md)

Formal Swift proposals are narrower. The template includes summary, motivation, proposed solution,
detailed design, compatibility and adoption effects, future directions, and alternatives. It links
an encompassing vision or roadmap where one exists. The detailed design must be sufficient for
someone other than the author to implement it; the future-directions section must not blur what the
current review is accepting. [Swift proposal template](https://github.com/swiftlang/swift-evolution/blob/main/proposal-templates/0000-swift-template.md)

### Lifecycle, ownership, and handoff

Ideas begin as informal pitches, then become proposal documents. A workgroup appoints a review
manager, conducts a dedicated public review, and decides to accept, reject, or return for revision;
substantially revised proposals receive another review. Review is explicitly not a vote. [Swift proposal process](https://github.com/swiftlang/swift-evolution/blob/main/process.md#making-a-proposal)

Swift records Awaiting review, Scheduled review, Active review, Returned for revision, Withdrawn,
Rejected, Accepted, Accepted with revisions, Previewing, and Implemented states. Proposal headers
link pitch/review/acceptance/rejection history and implementation pull requests. For language and
standard-library changes, a viable prototype is required before formal review. [Swift states and metadata](https://github.com/swiftlang/swift-evolution/blob/main/process.md#proposal-documents),
[language proposal requirements](https://github.com/swiftlang/swift-evolution/blob/main/process.md#language-and-standard-library-evolution)

### What Silk should borrow or avoid

Borrow the semantic separation between **endorsing a direction** and **accepting every mechanism
mentioned in it**. Borrow links from later work back to the parent conceptual artifact. Avoid the
mandatory prototype-before-review rule at Silk's conceptual layer: it would pull proposals back
toward implementation, recreating the limitation that motivated this investigation. Prototypes may
be requested to resolve a specific uncertainty, and OpenSpec can require them later.

## Recommendation for Silk

### 1. Establish one conceptual artifact above OpenSpec

Call it a **Language Proposal** unless a second type is genuinely needed. Unlike an OpenSpec change,
it answers “what should this part of Silk become, and why does that improve the language as a
whole?” It may contain several candidate mechanisms and identify several later implementation
slices. Acceptance endorses:

1. the problem statement and goals strongly;
2. the chosen language direction strongly enough to constrain future work;
3. illustrative mechanics only where the proposal explicitly marks them as decided; and
4. no implementation task or speculative future extension merely by mentioning it.

This adapts Swift's graduated vision endorsement without introducing separate “vision” and
“proposal” bureaucracies on day one. If the collection later shows two natural altitudes, broad
Language Visions can be split out without invalidating early documents.

### 2. Use a compact but demanding template

Recommended metadata:

```markdown
Status: Draft | In review | Accepted | Deferred | Rejected | Withdrawn | Superseded
Author:
Decision owner:
Created:
Discussion:
Depends on:
Supersedes:
Resolution:
OpenSpec handoff:
```

Recommended body:

```markdown
# Summary
One-paragraph direction and scope.

## Problem and evidence
Concrete user/compiler/library problems and representative use cases.

## Goals and non-goals
The outcomes being optimized and the attractive work deliberately excluded.

## Language model and whole-language fit
Terminology, programmer mental model, governing principles, and interactions with
types, effects/services, ownership, modules, generics, runtime, diagnostics,
tooling, standard library, and compilation targets as applicable.

## Guide-level explanation
Teach the resulting experience as if it existed, using complete examples.

## Semantic sketch
Syntax and semantics precise enough to expose contradictions, interactions, and
corner cases, without pretending to be the final implementation spec.

## Surface-area changes
What becomes valid, invalid, unnecessary, removed, or migrated. Pre-1.0 breakage
is evaluated by whether it simplifies and coheres the language, not compatibility.

## Drawbacks and risks
The strongest case against this direction, including conceptual and teaching cost.

## Alternatives and status quo
Competing language models, library-level solutions, and the cost of doing nothing.

## Prior art
What other systems teach, including failures and mismatches with Silk.

## Acceptance blockers
Questions that must be resolved before the proposal can be accepted.

## Open implementation questions
Questions intentionally delegated to prototypes or future OpenSpec changes.

## Future directions
Natural extensions and holistic consequences that are explicitly not accepted now.

## OpenSpec decomposition
Named implementation slices, dependencies, and acceptance boundaries; no task list.
```

`Language model and whole-language fit` is the critical addition. A generic
`Future directions` section alone can become an idea dump; the former forces the proposal to account
for today's complete language, while the latter captures tomorrow without expanding acceptance.

### 3. Keep lifecycle small and semantically sharp

```text
Draft -> In review -> Accepted
                  |-> Deferred
                  |-> Rejected
Draft/In review ----> Withdrawn
Accepted/Rejected ---> Superseded (by another numbered proposal)
```

- **Accepted** means the language direction is approved and may be translated into OpenSpec. It is
  not an implementation-progress state.
- **Deferred** means “plausible, but not deciding now.” It must record `Revisit when:` with a
  concrete trigger such as evidence, a prerequisite decision, a prototype result, or a project
  milestone. Without a trigger, reject it instead of creating a permanent waiting room.
- **Rejected** preserves the proposal and a decision note explaining the decisive tradeoff. A new
  proposal may revisit it only with new evidence or a materially different design.
- **Superseded** links both directions; do not silently rewrite a resolved design record.

Do not add `Implemented` to this lifecycle. The linked OpenSpec change or changes own Draft/Apply/
Archive and implementation completion. The proposal's `OpenSpec handoff` field can list those links
without changing the proposal's accepted status.

### 4. Separate facilitation from decision authority

For Silk's current scale, one named **decision owner** should accept, defer, or reject after review,
with a short checked-in resolution note. The author owns revisions and the truthful presentation of
alternatives; a review skill can facilitate consistency and challenge the document, but an agent
must not manufacture authority through a score or pseudo-consensus. If more maintainers join, the
single field can name a language group without changing the artifacts.

### 5. Make the OpenSpec handoff explicit

After acceptance:

1. Freeze the proposal except for links, clarifications, or an explicit superseding proposal.
2. Create one or more OpenSpec changes from `OpenSpec decomposition`.
3. Each OpenSpec change links back to the proposal and states which accepted slice it realizes.
4. OpenSpec may refine implementation details, but a contradiction of an accepted language
   decision requires amending or superseding the Language Proposal first.
5. When implementation lands, update canonical Silk specifications and documentation; keep the
   proposal as the explanation of *why*, not the source of truth for *what is*.

This produces a clean chain:

```text
Language Proposal (direction and coherence)
    -> OpenSpec change(s) (normative delta and implementation tasks)
        -> canonical specs/docs (implemented language truth)
```

## Bottom line

The strongest fit is not ECMAScript staging reproduced at a smaller scale. It is **Swift's vision
boundary plus Rust's explanatory discipline plus Python's decision record**, with process stripped
down to one owner and one handoff. That gives Silk room to make additive and subtractive pre-1.0
changes while preventing each narrow implementation change from locally optimizing the language
into an incoherent whole.
