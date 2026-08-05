# Bootstrap Syntax Inspector Specification

## Purpose

Give compiler developers a small direct-link browser surface for seeing how the first Silk fixture
and nearby malformed inputs become tokens, concrete syntax, and diagnostics.
## Requirements
### Requirement: Direct-link-only syntax inspector
The docs site SHALL expose a Syntax Inspector at `/docs/labs/syntax-inspector` while omitting the
page from the normal docs navigation and package sidebars. The page SHALL distinguish its lossless
concrete syntax tree from its semantic fact view and SHALL state that no semantic AST, HIR, or code
generation exists yet.

#### Scenario: Open the hidden inspector directly
- **WHEN** a developer navigates directly to `/docs/labs/syntax-inspector`
- **THEN** the docs site renders the syntax and semantic inspection views without advertising the page in normal navigation

### Requirement: Inspect the accepted fixture
The inspector SHALL start with `pub fn main() -> I32 { return 42 }` and SHALL display the concrete
tree hierarchy, every token kind, owner-qualified half-open byte span, exact source slice,
declaration facts, type facts, integer-value facts, return compatibility, and separate lexical,
parser, and semantic diagnostic collections produced for the current input.

#### Scenario: Inspect the initial program
- **WHEN** the inspector first loads
- **THEN** the accepted fixture has a complete function tree, exact token coverage, a public `main` declaration, resolved `I32` facts, exact value `42`, compatible return, and no diagnostics

### Requirement: Explore nearby malformed text
The inspector SHALL let a developer edit the source text and recompute lexing, parsing, and semantic
analysis locally without a network request. The output SHALL remain renderable for empty input,
missing syntax, unexpected ASCII punctuation, unknown return types, out-of-range integer literals,
and valid Unicode text whose UTF-8 bytes are unsupported by the bootstrap vocabulary.

#### Scenario: Remove the closing brace
- **WHEN** a developer deletes the fixture's closing brace
- **THEN** the tree shows an empty-span missing element, the parser diagnostic identifies the end-of-file position, and available semantic facts remain visible

#### Scenario: Enter unsupported Unicode text
- **WHEN** a developer enters a non-ASCII character
- **THEN** the inspector displays its UTF-8 bytes as retained invalid token data and remains interactive

#### Scenario: Enter an unknown return type
- **WHEN** a developer replaces `I32` with `Mystery`
- **THEN** the semantic view shows an unresolved return type, unavailable return compatibility, and its semantic diagnostic

#### Scenario: Enter an out-of-range integer
- **WHEN** a developer replaces `42` with `2147483648`
- **THEN** the semantic view shows an unavailable integer value and its semantic diagnostic

### Requirement: Inspector state is disposable
The inspector SHALL keep source and derived results only in browser memory. It MUST NOT write files,
persist source text, invoke a compiler service, or imply that the page is a supported language
playground.

#### Scenario: Reload the inspector
- **WHEN** a developer reloads the page after editing the source
- **THEN** the accepted fixture is restored and no previous input is recovered from storage

### Requirement: Inspect parameter and argument syntax
The Syntax Inspector SHALL provide valid and malformed presets for typed parameter declarations,
bare-identifier expressions, and value-carrying calls. It SHALL show the concrete parameter and
argument branches, every separator and token span, local recovery nodes, exact declaration parameter
counts, local parameter-resolution states, and positional call-contract facts.

#### Scenario: Inspect the identity syntax slice
- **WHEN** a developer selects the parameter-and-argument preset
- **THEN** the concrete view shows `value: I32`, the returned `value`, and the `42` in `identity(42)` while semantic panels show both the local parameter relationship and positional call contract

#### Scenario: Inspect malformed list recovery
- **WHEN** a developer selects a preset with a missing parameter type, comma, or call parenthesis
- **THEN** the relevant missing token and parser diagnostic remain visible while following syntax still renders

### Requirement: Inspect multiple concrete function branches
The Syntax Inspector SHALL provide a two-function preset and display each parsed function as a
separate top-level concrete branch in source order. The semantic panel SHALL show one ordered
function fact for each branch without collapsing or reordering later declarations.

#### Scenario: Inspect two parsed functions
- **WHEN** a developer selects the two-function preset
- **THEN** the concrete tree shows two function-declaration branches while the semantic panel shows their two corresponding ordered function facts

#### Scenario: Inspect recovery at a function boundary
- **WHEN** the first function in a two-function source is missing its closing brace
- **THEN** the tree keeps the missing brace in the first branch and the complete second branch visible

### Requirement: Inspect the declaration collection
The Syntax Inspector SHALL show one semantic function card per parsed declaration in concrete source
order. Each card SHALL display declaration identity, name state, return type, integer value, return
compatibility, and provenance, and the inspector SHALL provide a duplicate-name preset that displays
the ambiguous lookup state and its semantic diagnostic.

#### Scenario: Inspect two collected declarations
- **WHEN** a developer selects the two-function preset
- **THEN** the semantic view shows ordered `answer` and `main` cards whose ordinals and spans match their concrete branches

#### Scenario: Inspect a duplicate declaration name
- **WHEN** a developer selects the duplicate-name preset
- **THEN** both declarations remain visible, name lookup is shown as ambiguous, and `SEM0003` identifies the later name

### Requirement: Inspect parameter declarations and references
The Syntax Inspector SHALL render each function's ordered parameter facts and every bare-identifier
reference relationship. It SHALL show owning function and parameter identities, declaration and
reference spans, declared and expression types, lookup outcome, return compatibility, and
phase-separated diagnostics. Presets SHALL cover resolved, unknown, duplicate, cross-function, and
syntax-unavailable references.

#### Scenario: Inspect a resolved parameter reference
- **WHEN** a developer selects the identity-function preset
- **THEN** the semantic view links the returned `value` to parameter zero and shows `I32` expression type and compatible return

#### Scenario: Inspect an unknown local name
- **WHEN** a developer selects the unknown-parameter-reference preset
- **THEN** the relationship is missing and `SEM0006` identifies the exact returned identifier

#### Scenario: Inspect duplicate local parameters
- **WHEN** a developer selects the duplicate-parameter preset
- **THEN** both declarations remain visible, the reference lists both matches without choosing one, and `SEM0005` identifies the later declaration

### Requirement: Inspect the first call expression
The Syntax Inspector SHALL provide valid-call, missing-call-syntax, and unsupported-argument presets.
It SHALL show the call's concrete subtree, exact token slices and spans, reference and type facts,
return compatibility, and separate parser and semantic diagnostic collections.

#### Scenario: Inspect valid call syntax
- **WHEN** a developer selects the valid-call preset
- **THEN** the concrete view shows `answer()` as a call expression and the semantic view preserves its exact call-site facts

#### Scenario: Inspect damaged call syntax
- **WHEN** a developer selects a missing-parenthesis or unsupported-argument preset
- **THEN** explicit missing or error syntax stays visible beside the unavailable call facts and parser-owned diagnostics

### Requirement: Inspect call argument contracts
The Syntax Inspector SHALL show every ordered argument fact, its expression and type, its positional
target parameter when available, and the complete call-contract state. Presets SHALL cover a
compatible call, too few arguments, too many arguments, an unavailable mapped type, and an
unresolved call while retaining the existing syntax, relationship, return compatibility, and
phase-separated diagnostic views.

#### Scenario: Inspect a compatible call contract
- **WHEN** a developer selects the `identity(42)` preset
- **THEN** the inspector shows argument zero mapped to `identity` parameter zero with available `I32` types and a compatible contract

#### Scenario: Inspect wrong arity
- **WHEN** a developer selects a too-few or too-many preset
- **THEN** expected and actual counts, any positionally available mappings, and `SEM0007` are visible without hiding the resolved call target

#### Scenario: Inspect an unavailable contract
- **WHEN** a developer selects a preset with an unresolved argument type or call target
- **THEN** the inspector explains which prerequisite is unavailable and does not display an invented binding or mismatch

### Requirement: Inspect nested call concrete syntax
The Syntax Inspector SHALL provide valid and malformed nested-call presets and SHALL display every
inner and outer call branch, argument boundary, recovery node, token slice, and byte span. Its
semantic view SHALL distinguish analyzed nested facts from any unavailable dependency rather than
implying successful resolution or evaluation.

#### Scenario: Inspect a valid nested call
- **WHEN** a developer selects the `identity(identity(42))` preset
- **THEN** the concrete tree shows both call branches and the semantic view exposes the nested call's recursive facts

#### Scenario: Inspect malformed nested recovery
- **WHEN** a developer selects a damaged-inner-call preset
- **THEN** the inner missing token, outer call boundary, parser diagnostic, and all available downstream facts remain visible

### Requirement: Inspect recursive semantic expression facts
The Syntax Inspector SHALL provide valid, unresolved, incompatible, and syntax-damaged nested-call
presets. Its semantic view SHALL expose every nested call, argument, target-resolution state,
positional mapping, contract, result type, and exact source provenance as a hierarchy matching the
concrete expression nesting. Known inner facts SHALL remain visible when a dependent outer fact is
unavailable, and the view MUST NOT collapse nested calls into flat arguments or imply an AST, HIR,
or MIR.

#### Scenario: Inspect nested identity semantics
- **WHEN** a developer selects the `identity(identity(42))` preset
- **THEN** the semantic view shows the inner literal-to-parameter contract, the inner call result as the outer argument, and the outer contract with links to both call spans

#### Scenario: Inspect an unavailable inner relationship
- **WHEN** a developer selects a preset whose inner target is missing or ambiguous
- **THEN** the inner candidates or missing state remain visible and the dependent outer contract ends in a labeled unavailable state without an invented edge

### Requirement: Inspect the first resolved call relationship
The Syntax Inspector SHALL visualize each present call as a directed caller-to-target relationship
when uniquely resolved and as missing, ambiguous, or syntax-unavailable otherwise. The relationship
view SHALL keep caller, call-site, and target declaration spans available and SHALL remain beside the
concrete tree, function facts, and phase-separated diagnostics.

#### Scenario: Inspect a resolved call edge
- **WHEN** a developer selects the two-function resolved-call preset
- **THEN** the semantic view shows `main → answer`, the target declaration identity, an `I32` call type, and compatible caller return

#### Scenario: Inspect an unknown call target
- **WHEN** a developer selects the unknown-call preset
- **THEN** the relationship is shown as missing, compatibility is unavailable, and `SEM0004` identifies the call-site name

#### Scenario: Inspect an ambiguous call target
- **WHEN** a developer selects the ambiguous-call preset
- **THEN** the relationship shows every matching declaration without choosing one and the existing duplicate-name diagnostics remain visible

### Requirement: Inspect the first complete value-flow path
The Syntax Inspector SHALL derive a visual data-flow projection from existing semantic facts,
connecting each call argument to its mapped target parameter, each resolved parameter reference to
that declaration, each nested call result to its owning outer argument, and each returned expression
to the enclosing function and caller result. After explicit evaluation, the projection SHALL
distinguish static semantic relationships from reachable trace order and exact values. Every group,
node, and edge SHALL retain an accessible text description and exact syntax identity or span. The
view MUST NOT invent evaluation order, runtime values, or relationships absent from semantic facts
or the current evaluation outcome.

#### Scenario: Follow a literal through identity
- **WHEN** the canonical program calls `identity(42)` and all references and contracts are compatible
- **THEN** the view connects literal argument `42` to `identity.value`, the returned `value` reference, the `identity` call result, and `main`'s return in one navigable path

#### Scenario: Follow a nested result into an outer call
- **WHEN** `main` completes `identity(identity(42))`
- **THEN** the view groups both call sites and connects the inner result `42` through the outer argument and parameter to the completed entry result

#### Scenario: Preserve sibling evaluation order
- **WHEN** two nested arguments complete from left to right
- **THEN** their semantic branches remain grouped by argument ordinal and the evaluation overlay identifies the first branch before the second and both before the enclosing bindings

#### Scenario: Navigate from a flow item to syntax
- **WHEN** a developer activates a nested group, node, or edge with source provenance
- **THEN** the inspector identifies and emphasizes the corresponding source span and concrete, semantic, or trace detail without changing the analyzed input

#### Scenario: Read the flow without graphics
- **WHEN** the view is consumed through its accessible text structure
- **THEN** the same nested groups, ordered nodes, relationships, states, values, and source ranges are available without relying on position or color

### Requirement: Incomplete data flow remains explicit
The data-flow view SHALL represent missing, ambiguous, incompatible, unavailable, blocked, and cyclic
relationships as terminal or branched states rather than drawing a successful enclosing path. It
SHALL preserve all known nested provenance and link each stopped edge to the semantic fact,
evaluation reason, trace prefix, or phase-owned diagnostic that explains it. A completed earlier
argument branch SHALL remain visible when a later sibling blocks, but bindings or returns that did
not occur MUST NOT be drawn as evaluated flow.

#### Scenario: Stop at wrong arity
- **WHEN** a resolved call at any nesting depth has an arity-mismatch contract
- **THEN** the view shows any available positional pairs, marks unmatched arguments or parameters, and stops before claiming a valid result from that call or its enclosing call

#### Scenario: Branch at an ambiguous reference
- **WHEN** a parameter or function reference has multiple matches
- **THEN** the view exposes all candidates without selecting a successful edge

#### Scenario: Stop at unavailable syntax or type
- **WHEN** parser recovery or an unresolved type makes a required nested fact unavailable
- **THEN** the affected branch ends at a labeled unavailable state with the available syntax and diagnostic context retained

#### Scenario: Stop at a nested evaluation failure
- **WHEN** an inner argument blocks after an earlier argument completed
- **THEN** the earlier completed branch and partial trace remain visible while the inner reason terminates the enclosing evaluated path before its bindings

#### Scenario: Show a recursive cycle as a closed terminal path
- **WHEN** nested evaluation reports a recursive call cycle
- **THEN** the view lists the ordered declaration cycle and closing call-site span without drawing an infinite or successful path

### Requirement: Data-flow presets remain disposable
The inspector SHALL provide complete flat, complete nested, nested sibling, wrong-arity,
unknown-reference, ambiguous-reference, syntax-damaged, inner-blocked, and nested-cycle data-flow
presets. Flow mode, evaluation overlay, selection, and source emphasis SHALL remain in browser memory
and SHALL reset to the canonical preset on reload.

#### Scenario: Compare complete and incomplete paths
- **WHEN** a developer switches among flat, nested, and blocked data-flow presets
- **THEN** the projection and accessible description recompute locally from each preset's current semantic facts and optional evaluation outcome

#### Scenario: Compare static and evaluated flow
- **WHEN** a developer analyzes and then explicitly evaluates a nested preset
- **THEN** the same semantic relationships remain visible while reachable order, exact values, and any blocked endpoint are added from that outcome

#### Scenario: Reload after selecting a flow node
- **WHEN** the inspector is reloaded after source edits, evaluation, or flow navigation
- **THEN** the canonical source and unselected canonical flow are restored without persisted state

### Requirement: Evaluate the current bootstrap program
The Syntax Inspector SHALL provide an explicit browser-local evaluation action for the current
analyzed source. It SHALL display either the completed exact `I32` result or the closed blocked
reason and SHALL render the ordered evaluation trace with links to existing function, call,
argument, parameter, reference, and source provenance. Evaluation MUST NOT make a network request,
write files, persist results, or imply native compilation.

#### Scenario: Evaluate the canonical identity program
- **WHEN** a developer activates evaluation for `main` returning `identity(42)`
- **THEN** the inspector displays result `42` and an ordered trace matching the visible semantic data-flow path

#### Scenario: Inspect a blocked evaluation
- **WHEN** a developer evaluates a preset with a missing entry, wrong call arity, unavailable fact, or recursive cycle
- **THEN** the inspector shows the exact blocked reason, partial trace, and relevant source relationships without becoming unresponsive

#### Scenario: Edit after evaluation
- **WHEN** source text changes after an outcome is displayed
- **THEN** the stale outcome is cleared and the edited source must be analyzed before a new explicit evaluation

#### Scenario: Reload after evaluation
- **WHEN** the page reloads after a completed or blocked evaluation
- **THEN** the canonical source returns with no persisted result or trace

### Requirement: Inspect recursive evaluation outcomes
The Syntax Inspector SHALL provide completed, inner-blocked, and nested-cycle evaluation presets
and SHALL render each nested trace event beside the semantic expression and source provenance that
produced it. Successful inner results SHALL be visibly connected to their enclosing positional
bindings, while a blocked inner path SHALL end before any enclosing binding or return that did not
occur. The trace SHALL remain available as an ordered accessible text structure and MUST NOT rely
on indentation, position, or color alone to communicate nesting.

#### Scenario: Inspect a completed nested evaluation
- **WHEN** a developer evaluates the `identity(identity(42))` preset
- **THEN** the inspector displays result `42` and distinguishes the inner call, inner return, outer binding, outer return, and their two call-site spans in trace order

#### Scenario: Inspect an inner blocked outcome
- **WHEN** a nested argument blocks because its target, contract, value, or cycle is unavailable
- **THEN** the inspector shows the exact inner reason and partial trace without displaying an enclosing binding or completed result

#### Scenario: Read nested trace order without graphics
- **WHEN** the nested trace is consumed through its accessible text representation
- **THEN** call depth, event order, values, identities, states, and source ranges communicate the same outcome as the visual trace

### Requirement: Inspect the unified diagnostic stream
The inspector SHALL present the compilation's diagnostics from the unified model: each diagnostic
showing its stable code, severity, message, primary span, and originating phase, in the
deterministic driver order. Selecting a diagnostic that carries a causal diagnostic identity
SHALL reveal its originating diagnostic.

#### Scenario: Diagnostics show phase origin in driver order
- **WHEN** the inspected source produces lexical, parser, and semantic diagnostics together
- **THEN** the inspector lists all of them in the deterministic driver order, each labeled with its originating phase, code, and severity

#### Scenario: Follow a causal chain
- **WHEN** a listed diagnostic carries a cause
- **THEN** selecting it reveals the originating diagnostic and its primary span

### Requirement: Inspect the syntax artifact

The syntax lab SHALL read its data from one `SyntaxFile` artifact: it SHALL present the complete
token stream including trivia and the surface tree, and SHALL highlight missing elements and
error regions distinctly from ordinary tokens and nodes.

#### Scenario: Present the token stream with trivia

- **WHEN** the inspected source contains whitespace, comments, and supported tokens
- **THEN** the lab lists every token of the artifact in source order with its kind and span, including trivia tokens

#### Scenario: Highlight recovered structure from the artifact

- **WHEN** the inspected source produces missing tokens and error regions
- **THEN** the lab's tree highlights each missing element and error region, sourced from the same artifact as the token stream

### Requirement: Inspect the module-closure import graph

The docs site SHALL expose a direct-link module-closure lab that loads a compilation request and
presents the loaded closure: every module in canonical identity order with its import facts,
cycle facts marked on their participating modules, and the closure's module-phase diagnostics.
The lab SHALL keep its state in browser memory only.

#### Scenario: Inspect a diamond closure

- **WHEN** a developer selects a preset whose root imports two modules sharing one dependency
- **THEN** the lab lists all four modules in canonical order, each with its resolved import facts, and reports no cycles

#### Scenario: Mark an import cycle

- **WHEN** a developer selects a preset containing mutually importing modules
- **THEN** the lab marks every module participating in the cycle and names the cycle's members in canonical order

#### Scenario: Surface closure diagnostics

- **WHEN** a preset contains an unknown import target or a self-import
- **THEN** the lab lists the module-phase diagnostics with their codes and exact spans

### Requirement: Inspect the declaration index

The docs site SHALL expose a direct-link declaration-index lab presenting the collected headers
of a loaded closure: every declaration with its module, canonical identity state, and resolved
signature, in canonical index order, with duplicate and unavailable states explicit and the
header-level diagnostics listed in driver order. The lab SHALL keep its state in browser memory
only.

#### Scenario: Inspect headers across modules

- **WHEN** a developer selects a preset whose modules declare functions with resolved signatures
- **THEN** the lab lists every header in canonical order with its module, canonical identity, parameters, and return type

#### Scenario: Inspect duplicate and unavailable states

- **WHEN** a preset contains a duplicate declaration name and a declaration with a missing name
- **THEN** the duplicate header is marked as a caused duplicate of the original and the unnamed header is marked unidentified, while both remain listed

#### Scenario: Surface header diagnostics

- **WHEN** a preset contains an unknown parameter or return type
- **THEN** the lab lists the `SEM0001` diagnostic with its exact span in the unified panel

### Requirement: Inspect elaborated HIR with typed provenance

The syntax lab SHALL present the elaborated HIR of the current source: each function with its
canonical identity state and normalized contract, and its body as typed core operations in
evaluation order, with unavailable states explicit. Hovering or focusing an HIR expression SHALL
reveal its resolved type and exact source span. The lab's semantic views SHALL read elaboration
facts.

#### Scenario: View a function's HIR

- **WHEN** the inspected source elaborates a function returning a resolved call
- **THEN** the HIR view lists the function's contract and a typed call operation referencing the target's canonical identity

#### Scenario: Reveal type and span on hover

- **WHEN** a developer hovers an HIR expression entry
- **THEN** the entry reveals its resolved type and exact half-open source span

#### Scenario: Keep unavailable HIR explicit

- **WHEN** the inspected source contains an unknown call target
- **THEN** the HIR view marks the expression unavailable rather than fabricating a typed operation

