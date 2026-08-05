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
counts, local parameter-resolution states, and the explicitly deferred positional-checking boundary.

#### Scenario: Inspect the identity syntax slice
- **WHEN** a developer selects the parameter-and-argument preset
- **THEN** the concrete view shows `value: I32`, the returned `value`, and the `42` in `identity(42)` while semantic panels show the local parameter relationship and clearly mark positional argument checking as deferred

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
