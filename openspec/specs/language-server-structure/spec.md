# language-server-structure Specification

## Purpose

Defines how the language server describes the shape of a Silk source: the meaning of each token, the
regions an editor may collapse, and the call relationships between functions. All three read
artifacts the compiler already produces — the lexer's token kinds, the lossless concrete syntax
tree, and the semantic occurrence index — so an editor reads the same facts the compiler decided
rather than re-deriving them from a regular expression.

## Requirements

### Requirement: The server advertises structural capabilities

The language server SHALL advertise semantic-tokens support carrying its legend, folding-range
support, and call-hierarchy support to compatible clients. The semantic-tokens capability SHALL
declare full-document requests. Delta and range requests are not advertised, so a client always
requests the whole document.

#### Scenario: Client initializes structural support

- **WHEN** a compatible client initializes a Silk language-server session
- **THEN** the returned capabilities advertise a semantic-tokens provider carrying the server's legend and declaring full-document support, a folding-range provider, and a call-hierarchy provider

### Requirement: The semantic token legend is built from the compiler's token kinds

The semantic token legend SHALL name only standard protocol token types, and every type it names
SHALL be reachable from a compiler token kind or a semantic occurrence. The keyword, comment,
string, number, and operator types SHALL be decided by the lexer's own token kinds — the same kinds
the TextMate grammar in `@silk-lang/language` colors — so a keyword added to the compiler reaches
both the grammar and the legend from one definition.

#### Scenario: Compare the legend with the grammar's keywords

- **WHEN** a document spelling every keyword the TextMate grammar colors is tokenized
- **THEN** each keyword carries a semantic token of the legend's keyword type

### Requirement: Semantic tokens type an identifier by what it resolves to

A full-document semantic tokens request SHALL return one token per classified lexer token, in source
order, using the protocol's delta encoding. An identifier's token type SHALL be decided by the
semantic occurrence covering it rather than by its spelling, so a type name and a variable name
receive different token types where the TextMate grammar's regular expressions must color them
alike. An identifier with no covering occurrence SHALL contribute no token rather than a guessed
one. A token spanning more than one line SHALL be omitted, because the protocol's encoding cannot
express one.

#### Scenario: Color a type name and a variable name

- **WHEN** a semantic tokens request covers a document declaring a struct type and binding a local variable
- **THEN** the type name carries the `type` token type and the variable name carries the `variable` token type

### Requirement: Folding ranges cover braced regions and comment runs

A folding-range request SHALL return one range for each block, each declaration body, and each run
of adjacent comment lines. A braced region SHALL fold from the line holding its opening brace to the
line holding its closing brace, so the line an editor leaves visible keeps the brace that opened the
region. A region whose braces sit on one line SHALL yield no range. Because Silk has no delimited
block comment, a run of adjacent `//`, `///`, or `//!` lines SHALL fold as one range carrying the
`comment` kind. A range whose folding is controlled by a comment marker such as `// region` is out
of scope.

#### Scenario: Fold a function body

- **WHEN** a folding-range request covers a function whose body opens on its declaration line and closes three lines later
- **THEN** one range folds from the declaration line to the closing brace's line

#### Scenario: Fold a run of comment lines

- **WHEN** a folding-range request covers three adjacent comment lines followed by a declaration
- **THEN** one range of the `comment` kind covers the three comment lines

### Requirement: Call hierarchy is prepared from the selected declaration

A prepare-call-hierarchy request SHALL resolve the position through the semantic occurrence index
and SHALL return the source-backed function declaration it names, so a call site, a use as a value,
and the declaration's own name all prepare the same declaration. A position naming no source-backed
function declaration SHALL prepare no item. The prepared item SHALL carry the declaration's name,
its source-level callable form as detail, the range of the whole declaration, and the name token as
its selection range.

#### Scenario: Prepare from a declaration name

- **WHEN** a prepare-call-hierarchy request selects the name of `pub fn helper(value: i32) -> i32`
- **THEN** one item named `helper` is returned carrying `pub fn helper(value: i32) -> i32` as its detail

### Requirement: Incoming calls name every calling function

An incoming-calls request SHALL return every function whose body encloses a use of the selected
declaration, across every module of the analyzed project. Two calls written in one function SHALL
collapse into one entry carrying both call ranges. A use written outside any function body, such as
an import clause or the declaration's own name, SHALL contribute no caller.

#### Scenario: Two callers in two modules

- **WHEN** an incoming-calls request selects a function called once from each of two other modules
- **THEN** both calling functions are returned, each carrying the range of its own call

### Requirement: Outgoing calls name every called function

An outgoing-calls request SHALL return every function declaration the selected function's body
names, in the order the calls are written. A callee reached through a qualified name SHALL be found
exactly as a bare one is, and repeated calls to one callee SHALL collapse into a single entry
carrying every call's range.

#### Scenario: Call two functions, one of them twice

- **WHEN** an outgoing-calls request selects a function calling `helper` twice and `double` once
- **THEN** `helper` is returned carrying two ranges and `double` carrying one

### Requirement: Structural responses use the negotiated position encoding

Every semantic token position, folding range line, and call hierarchy range SHALL be expressed in
the position encoding negotiated at initialization, so a source containing characters outside the
Basic Multilingual Plane places every structural answer exactly where the editor expects it.

#### Scenario: Answer a structural request in a document holding astral characters

- **WHEN** a document holds a character outside the Basic Multilingual Plane before a declaration
- **THEN** the semantic tokens, folding ranges, and call hierarchy ranges of that declaration carry columns counted in the negotiated encoding

### Requirement: Pattern identifiers retain semantic token and statement structure

Semantic tokens SHALL classify shared pattern declaration and reference tokens from their compiler
semantic occurrences. Structural queries SHALL retain statements nested in both bodies of if-let,
while keeping taken-body binding identity absent from the mismatch body.

#### Scenario: Tokenize and structure if-let

- **WHEN** a document contains an if-let with statements in both bodies
- **THEN** the declaration and taken-body uses receive local-binding semantics and both nested statement bodies remain queryable
