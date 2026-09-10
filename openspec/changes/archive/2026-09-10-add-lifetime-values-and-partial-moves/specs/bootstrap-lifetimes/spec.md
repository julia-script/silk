## Purpose

Defines static lifetime-bearing type contracts, deterministic elaboration, and bounded checking so borrowed values retain validity through ordinary abstractions without runtime lifetime identity.

## ADDED Requirements

### Requirement: Semantic lifetimes express validity independently of loans

Lifetime-bearing references, slices, strings, nominal applications, ordinary union members, callable contracts, and Effect environments SHALL retain canonical semantic region arguments. A lifetime SHALL express validity, while concrete loans SHALL separately establish source place, access, reborrow ancestry, and live use or cleanup requirements. Two sources sharing a lifetime SHALL NOT acquire shared runtime identity, and moving an owning wrapper SHALL NOT move its external referents or erase nested lifetimes.

#### Scenario: Keep independent pair lifetimes

- **WHEN** Pair<'left, 'right> stores two shared views and a getter explicitly returns only 'left
- **THEN** the result retains left-source validity without retaining the wrapper's own storage or unnecessarily equating the right data lifetime

#### Scenario: Preserve nested references

- **WHEN** a shared slice stores &'inner Item elements under outer lifetime 'outer
- **THEN** copying an element produces &'inner Item while borrowing its slot produces &'outer &'inner Item with the corresponding well-formedness obligation

### Requirement: Outlives and variance are finite declared obligations

The checker SHALL interpret 'long: 'short as sufficient longer validity and T: 'use as validity of T's borrowed contents, not immortality of a T owner. Well-formed &'a T SHALL imply T: 'a. Shared references SHALL be covariant in region and payload; exclusive references SHALL be covariant in access duration and invariant in destination type. Function inputs SHALL reverse and results preserve variance. Nominal variance SHALL derive from declared fields and validated intrinsic representation rules over a finite lattice in actual recursive components; opaque unsafe storage SHALL default invariant. Additional obligations not implied by parameter types SHALL require declared bounds.

#### Scenario: Reject a shorter mutable destination payload

- **WHEN** a caller attempts to view &mut Holder<'long> as a slot for Holder<'short> where 'short cannot satisfy 'long
- **THEN** compatibility rejects the invariant destination even if the outer exclusive access is brief

#### Scenario: Interpret static ownership correctly

- **WHEN** an owned detached buffer satisfies a static contents bound and is dropped after its last use
- **THEN** the bound does not require leaking its owner, while a reference to an ordinary local cannot be coerced to 'static

#### Scenario: Derive recursive variance finitely

- **WHEN** mutually recursive declarations refer to one another through already resolved field types
- **THEN** their variance summary converges in the actual component without expanding recursive field paths or inspecting arbitrary method bodies

### Requirement: Elision is deterministic declaration elaboration

Each independent omitted top-level input reference lifetime SHALL introduce a fresh binder. One top-level borrowed input SHALL supply omitted output lifetimes, and a borrowed self receiver SHALL take precedence as the method default. Omitted nominal input lifetime arguments SHALL be fresh independent binders, distinct from the outer borrow. Omitted nominal output lifetimes SHALL use the output default; without a unique default or explicit relationship the declaration SHALL diagnose ambiguity. Each omitted field reference or nominal lifetime argument SHALL introduce a distinct declaration binder. Local nominal omissions SHALL infer use-driven regions. Return bodies and constructor histories MUST NOT choose public lifetime relationships.

#### Scenario: Elide a borrowed constructor

- **WHEN** fn make<A>(items: &[A]) -> SliceStream<A> refers to a nominal with one data lifetime
- **THEN** elaboration introduces one input region and uses it as the output nominal lifetime

#### Scenario: Preserve nominal input independence

- **WHEN** a function accepts &Pair whose nominal fields have two omitted data lifetimes
- **THEN** elaboration gives the wrapper borrow and each independent stored field separate binders; an omitted output borrows the wrapper by default

#### Scenario: Reject ambiguous output elision

- **WHEN** an ordinary function has two borrowed inputs, no receiver default, and an omitted output lifetime
- **THEN** declaration completion requests an explicit relationship without inspecting either return branch

#### Scenario: Expand implicit fields independently

- **WHEN** a struct declares two independently omitted shared borrowed fields
- **THEN** its canonical declaration publishes two lifetime parameters with no equality inferred from constructor bodies

### Requirement: Local solving consumes resolved facts without search

Lifetime checking SHALL consume resolved constructors, selected operations and interface evidence, explicit provider bindings, declared summaries, and upstream static context. It SHALL generate obligations over a finite local control-flow domain and propagate deterministically. It MUST NOT initiate implementation or provider discovery, implicit conversion search, speculative associated-type normalization, or compile-time execution to repair a failed lifetime obligation. Unsupported forms or exhausted implementation resources SHALL diagnose deterministically with the originating declaration and exhausted dimension, never accept unchecked or select a different candidate.

#### Scenario: Fail against the selected operation

- **WHEN** an already selected interface operation has an unsatisfied lifetime requirement
- **THEN** the checker reports that requirement and attribution shows no candidate discovery or speculative static evaluation initiated by lifetime solving

#### Scenario: Check without backend work

- **WHEN** an editor or ordinary check request needs a lifetime verdict
- **THEN** the verdict uses semantic queries without LLVM emission, linking, or release optimization and agrees across development and optimized paths

### Requirement: Canonical lifetime identity supports semantic reuse and runtime erasure

Public binders SHALL use stable declaration-relative identity and binder structure, independent of names, byte offsets, allocation order, or caller roots. Repeated type comparisons SHALL reuse facts only with the same canonical type pair, binder environment, and relevant assumptions. Local inference identities SHALL remain body-scoped. Semantic and implementation dependencies SHALL be distinguishable. Generic checks SHALL be reused in an unchanged semantic context; residual ownership verification SHALL consume checked facts with distinct inputs and work attribution and MUST NOT repeat frontend analysis or rescue an invalid source contract. Lifetimes SHALL erase from runtime layout, instance identity, symbols, and runtime generic arguments.

#### Scenario: Preserve alpha identity

- **WHEN** a lifetime binder is renamed without changing any relationship
- **THEN** canonical semantic fingerprints stay equal while source presentation updates

#### Scenario: Keep incompatible assumptions distinct

- **WHEN** the same type pair is compared once under 'a: 'b and once without that assumption
- **THEN** only the compatible-context comparison can reuse the first result

#### Scenario: Attribute residual work separately

- **WHEN** representation-specific residual control flow requires further ownership validation
- **THEN** inspection names its distinct inputs and accounting and does not rerun generic body checking under a lowering label

### Requirement: Ownership and union analysis avoid hidden state enumeration

A body SHALL share one canonical sparse move-path forest containing referenced places and necessary ancestors, with finite per-path initialization and reachability joins. Large fixed arrays SHALL track accessed constant indices and untouched summaries without a node for every untouched element. Canonical union payloads and cleanup recipes SHALL be shared; ordinary propagation MUST NOT enumerate joint active variants or field-hole subsets, create public partial types, or specialize runtime instances by partial state. Conditional cleanup flags SHALL be permitted without representing lifetime identity.

#### Scenario: Track sparse fixed-array moves

- **WHEN** a body moves a few known indices from a very large fixed array
- **THEN** structural evidence contains only relevant index paths and necessary ancestors plus untouched summary facts

#### Scenario: Avoid Cartesian optional states

- **WHEN** independent optional borrowed locals and independent conditionally moved fields are analyzed
- **THEN** facts scale with actual types, uses, paths, and joins without enumerating combinations or creating a cleanup-function family per hole subset

### Requirement: Lifetime admission includes inspectable work and growth evidence

Correctness evidence SHALL expose canonical identity, reuse and negative invalidation, obligation instantiation, no-search attribution, runtime erasure, sparse path structure, and shared cleanup recipes. Opt-in growth workloads SHALL vary loan count and chain length, wrapper depth, union width/depth, callback and binder width, module fan-out, actual recursive component size, moved fields, projection depth, sparse array length, and branch/loop joins independently and in selected combinations. Cold and warm checks, body edits, alpha-renames, exported-bound edits, new generic calls, and failed diagnostics SHALL report attributed query, constraint, comparison, loan, move-path, cleanup, propagation, candidate, instance, memory, and phase-time work. Unexplained Cartesian expansion or repeated generic frontend checks SHALL fail admission; other superlinear growth SHALL require an explicit accepted-domain or algorithm decision.

#### Scenario: Measure invalid programs too

- **WHEN** an opt-in family increases wrapper depth for accepted and escaping-reference cases
- **THEN** its report includes successful propagation and diagnostic construction with per-phase work rather than correctness-suite timing thresholds
