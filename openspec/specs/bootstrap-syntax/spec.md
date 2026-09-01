# Bootstrap Syntax Specification

## Purpose

Turn the bootstrap lexer result into the smallest source-faithful grammatical structure that can
recover from ordinary mistakes without introducing semantic or lowering representations.
## Requirements
### Requirement: Canonical integer, unit, and bottom syntax is lossless

The parser SHALL preserve every lowercase integer primitive spelling, `()` in type and value positions, `never` in type positions, omitted unit results, bare `return`, and exact signed or unsigned literal tokens under existing bounded recovery rules.

#### Scenario: Parse canonical forms

- **WHEN** source contains lowercase integer declarations plus unit and bottom forms
- **THEN** syntax retains every token and exact span without deciding target width

### Requirement: First function grammar
The parser SHALL recognize a source file containing zero or more top-level import or function
declarations followed by end-of-file. Every function SHALL have the form `[pub] fn <name>(<parameters>)
-> <return-type> { <statements> }`, where `pub` is optional and `<statements>` is zero or more binding
statements followed by exactly one return statement. A binding statement SHALL have the form `let
<name> = <expression>`. A parameter SHALL have the form `<name>: <type>` and parameters SHALL be
comma-separated. An expression SHALL be a decimal integer with an optional directly applied `-`
sign, a bare identifier, a `move <name>` operand, or a call. A call SHALL have the form
`<callee>(<arguments>)`, where `<callee>` is one identifier or a qualified path
`<namespace>.<member>`; arguments SHALL be comma-separated expressions. Empty parameter and
argument lists SHALL remain valid. Lexer trivia SHALL be permitted between grammar elements,
statements, and declarations. Names, qualifiers, members, and types SHALL remain uninterpreted
identifier tokens, and declaration, statement, parameter, and argument order SHALL match concrete
source order.

#### Scenario: Parse an empty module
- **WHEN** the source contains only end-of-file
- **THEN** the result contains a source-file root with end-of-file, no recovered declaration, and no parser diagnostic

#### Scenario: Parse the accepted integer fixture
- **WHEN** the source bytes spell `pub fn main() -> i32 { return 42 }`
- **THEN** the result contains one complete function declaration with an empty parameter list, an integer literal return expression, and end-of-file

#### Scenario: Parse a private function

- **WHEN** the source bytes spell `fn helper() -> i32 { return 42 }`
- **THEN** the result contains one complete function declaration with no public-modifier token and exact source provenance

#### Scenario: Parse trivia between grammar elements
- **WHEN** whitespace and line comments appear between every pair of grammar elements in the accepted fixture
- **THEN** the parser recognizes the same grammatical structure while retaining the exact trivia tokens

#### Scenario: Parse two functions in source order
- **WHEN** `answer` returning `42` is followed by `main` returning `0`
- **THEN** the source-file tree contains exactly two complete function declarations in that order before end-of-file

#### Scenario: Parse a typed parameter reference
- **WHEN** the source spells `pub fn identity(value: i32) -> i32 { return value }`
- **THEN** the function contains one typed parameter and a bare-identifier return expression with exact concrete provenance

#### Scenario: Parse the first value-carrying call
- **WHEN** `identity` accepts one `i32` parameter and `main` returns `identity(42)`
- **THEN** `main` contains one complete call expression with one decimal-integer argument

#### Scenario: Parse trivia inside a call
- **WHEN** whitespace and line comments appear around a call's callee, parentheses, arguments, and commas
- **THEN** the parser recognizes the same call structure while retaining every trivia token exactly

#### Scenario: Parse a binding sequence

- **WHEN** the source spells `pub fn main() -> i32 { let value = 42 return value }`
- **THEN** the block contains one binding statement retaining its keyword, name, equals, and initializer expression, followed by one return statement, in source order

#### Scenario: Parse a move operand

- **WHEN** a binding initializer or call argument spells `move value`
- **THEN** the expression is a move operand retaining the keyword and the moved name with exact spans

#### Scenario: Parse a signed literal

- **WHEN** a return expression or call argument spells `-42`
- **THEN** the integer literal expression retains the minus token and the decimal token as one concrete branch with exact spans

#### Scenario: Parse a qualified callee

- **WHEN** a body spells `i32.add(1, 2)`
- **THEN** the call expression retains the actor identifier, the dot, the operation identifier, and the argument list in concrete order

### Requirement: First call syntax remains explicit and recoverable
A call expression SHALL retain its complete callee expression, left parenthesis, ordered arguments,
separators, right parenthesis, trivia, and exact source-owned span in concrete order. Callees MAY be
named or qualified function references, sections, bindings, grouped expressions, or prior call
results. Missing call elements SHALL become explicit missing syntax with parser diagnostics.
Unexpected tokens inside a callee or argument position SHALL remain in an error region, and recovery
SHALL resume at the next comma, right parenthesis, enclosing brace, following declaration, or
end-of-file.

#### Scenario: Recover a missing right call parenthesis
- **WHEN** a returned call spells `answer(` immediately before the function's closing brace
- **THEN** the call contains a missing right parenthesis, the block retains its closing brace, and parsing completes

#### Scenario: Recover a missing call callee
- **WHEN** the returned expression consists only of `()`
- **THEN** the call retains a missing callee and both parentheses without inventing a name

#### Scenario: Preserve a supported call argument
- **WHEN** the returned call spells `identity(42)`
- **THEN** `42` is retained as the call's first decimal-integer argument rather than an error region

#### Scenario: Recover between call arguments
- **WHEN** unsupported punctuation appears between two otherwise valid arguments
- **THEN** the punctuation remains in an error region and the following comma-bounded argument remains parseable

#### Scenario: Keep integer expressions unchanged
- **WHEN** a function returns a decimal integer
- **THEN** its existing integer-literal concrete shape and recovery behavior remain unchanged

#### Scenario: Call a section result

- **WHEN** a body spells `i32.add(2)(3)`
- **THEN** the concrete tree retains two ordered postfix calls, with the first producing the callee of the second

### Requirement: Parse nested call arguments losslessly
The bootstrap parser SHALL accept a call expression wherever a call argument expression is allowed.
It SHALL preserve each nested call and argument list as its own concrete branch with every token,
separator, trivia slice, and owner-qualified half-open byte span retained exactly once. This grammar
extension MUST NOT imply that nested calls are already semantically resolved or evaluated.

#### Scenario: Parse one nested identity call
- **WHEN** a function returns `identity(identity(42))`
- **THEN** the outer argument contains a complete inner call-expression branch whose literal `42` and both parenthesis pairs retain exact source order and spans

#### Scenario: Parse two nested arguments
- **WHEN** a function returns `choose(identity(1), identity(2))`
- **THEN** both outer arguments contain independent nested call branches separated by the outer comma

#### Scenario: Recover a damaged inner call
- **WHEN** damaged inner syntax reaches an outer sibling boundary or an inner call lacks a closing parenthesis before the outer closing parenthesis
- **THEN** recovery records the inner error or missing token and keeps the outer argument boundary, following arguments, and enclosing call visible

#### Scenario: Preserve a following declaration after nested damage
- **WHEN** malformed nested call syntax is followed by another `pub fn` declaration
- **THEN** recovery remains bounded to the damaged function and the following declaration remains a separate complete concrete branch

### Requirement: Function-boundary recovery remains local
Recovery inside one function SHALL stop at a following function's `pub` token when that token can
begin the next declaration. Unexpected concrete input between declarations SHALL remain lossless,
and parsing SHALL either consume a concrete token or insert missing syntax without looping. An
empty module SHALL terminate directly at end-of-file without entering declaration recovery. After
the first diagnostic in one recovery episode, dependent missing or damaged syntax SHALL remain
explicit in the tree without producing further diagnostics until the parser consumes a concrete
token expected by the grammar. At that synchronization token, ordinary diagnostic reporting SHALL
resume for later independent mistakes.

#### Scenario: Preserve a second function after a missing brace
- **WHEN** the first function omits its closing brace immediately before a valid second function
- **THEN** the first function receives a missing right brace and the second function remains a separate complete declaration

#### Scenario: Recover unexpected input between functions
- **WHEN** unsupported punctuation appears after one complete function and before the next `pub`
- **THEN** the punctuation is retained in an error region with a parser diagnostic and the following function remains parseable

#### Scenario: Preserve empty module structure
- **WHEN** the source is empty
- **THEN** the parser returns only the source-file root and end-of-file without missing syntax or parser diagnostics

#### Scenario: Stop an end-of-file declaration cascade
- **WHEN** the source contains only `pub`
- **THEN** the parser reports the missing `fn`, retains the remaining recovered function structure, and suppresses its dependent missing-token and missing-return diagnostics through end-of-file

#### Scenario: Report again after a grammar anchor
- **WHEN** one missing token starts recovery, a later expected concrete token is consumed, and another independent token is missing after that anchor
- **THEN** the parser reports both independent mistakes without reporting dependent insertions between them

### Requirement: Lossless concrete syntax tree

The concrete syntax tree SHALL retain every lexer token exactly once as a leaf in source order,
including whitespace, comments, invalid tokens, and end-of-file. Concrete nodes SHALL expose their
kind, ordered children, and source-owned half-open span. The tree SHALL distinguish the source file,
function declaration, parameter list, parameter declaration, return type, block, return statement,
integer literal expression, identifier expression, call expression, argument list, missing element,
and unexpected-token error region without claiming semantic meaning. The tree SHALL be owned by
the `SyntaxFile` artifact of its source module, and every node and token SHALL be addressable
there by a stable identity.

#### Scenario: Reconstruct parsed source

- **WHEN** all concrete token leaves except end-of-file are sliced from their owning source in tree order
- **THEN** their concatenated bytes reproduce the original source exactly

#### Scenario: Preserve invalid lexer data

- **WHEN** the lexer emits an invalid token inside a parameter or argument list
- **THEN** the concrete tree retains that exact token inside an error region and preserves its lexical diagnostic

#### Scenario: Address tree elements through the artifact

- **WHEN** a source module is parsed into its `SyntaxFile`
- **THEN** every tree node and token leaf resolves to a stable identity qualified by the source identity

### Requirement: Typed parameter lists remain explicit and recoverable
Each parameter SHALL retain its name, colon, type identifier, adjacent trivia, and source-owned span.
Each comma SHALL remain a concrete separator rather than belonging to either neighboring parameter.
Missing names, colons, types, commas, and closing parentheses SHALL remain explicit parser-owned
recovery data, and a following return arrow or declaration SHALL bound recovery.

#### Scenario: Parse two typed parameters
- **WHEN** a function declares `(left: i32, right: i32)`
- **THEN** the parameter list contains two ordered parameter declarations and one concrete comma separator

#### Scenario: Recover a missing parameter type
- **WHEN** a parameter name and colon are followed immediately by the list's right parenthesis
- **THEN** the parameter retains a missing type identifier at that boundary and the function continues parsing at the return arrow

#### Scenario: Recover a missing comma
- **WHEN** two complete typed parameters are adjacent without a comma
- **THEN** the list contains an explicit missing comma and preserves both parameters in source order

### Requirement: Missing syntax remains explicit
When a required grammar element is absent at the current source position, the parser SHALL insert a
missing element with the expected token kind and an empty source-owned span at that byte boundary.
The parser SHALL emit a stable diagnostic for the source-level mistake without consuming an
unrelated concrete token. Multiple missing elements introduced solely to represent one absent
construct MAY share one construct-level diagnostic rather than producing one diagnostic per leaf.

#### Scenario: Recover a missing function name
- **WHEN** the source spells `pub fn () -> i32 { return 42 }`
- **THEN** the function contains a missing identifier before `(`, the remaining structure parses, and one parser diagnostic identifies the missing name position

#### Scenario: Recover a missing closing brace
- **WHEN** the accepted fixture ends after the decimal integer
- **THEN** the block contains a missing right brace at end-of-file and the parser returns the partial tree with one parser diagnostic

#### Scenario: Aggregate a wholly missing return statement
- **WHEN** a required-return block reaches its closing brace without a return keyword or expression
- **THEN** the CST retains the recovered return structure and one parser diagnostic identifies the missing return statement

### Requirement: Unexpected syntax remains explicit
When concrete tokens cannot satisfy the next grammar element, the parser SHALL consume a maximal
run of unexpected non-trivia tokens into one error region until it reaches the expected token, the
next structurally valid token, a closing brace, or end-of-file. Recovery SHALL always consume a
concrete token or insert a missing element, and SHALL retain every skipped token in the tree.

#### Scenario: Recover before a function name
- **WHEN** unsupported punctuation appears between `fn` and the function name
- **THEN** the punctuation is retained in one error region, the following identifier becomes the function name, and parsing continues through end-of-file

#### Scenario: Terminate on wholly unrelated input
- **WHEN** no input token begins the first function grammar
- **THEN** the parser returns a source-file tree containing the unexpected tokens, explicit missing structure, end-of-file, and a finite ordered diagnostic collection

### Requirement: Parser diagnostics are deterministic data

Parsing SHALL produce the `SyntaxFile` artifact, which retains the token stream and lexical
diagnostics and exposes parser diagnostics as a separate readonly collection. Every parser
diagnostic SHALL be a unified `Diagnostic` value whose originating phase is the parser, containing
a stable code, severity, concise message, structured reason data, and source-owned primary span.
Within the artifact, parser diagnostics SHALL be ordered by primary span and stable code, and
parsing SHALL return its artifact and diagnostics rather than throwing or failing an Effect for
source mistakes.

#### Scenario: Repeat malformed parsing

- **WHEN** equivalent malformed source files are lexed and parsed repeatedly in fresh processes
- **THEN** their tree kinds, ordered elements, spans, source slices, lexical diagnostics, and parser diagnostics are identical

#### Scenario: Parser diagnostics carry their phase

- **WHEN** a source produces any parser diagnostic
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the parser as its originating phase

### Requirement: Minimal import declarations parse losslessly

The parser SHALL accept the accepted unconditional top-level forms: `import <path>`, `import <path>
as <namespace>`, `import <path> { <members> }`, and `import <path> as <namespace> { <members> }`.
`<path>` SHALL contain one or more identifier segments separated by dots. `<members>` SHALL contain
one or more comma-separated identifiers, each optionally followed by `as <local-name>`. The import
declaration SHALL retain its keyword, ordered path segments and dots, optional namespace alias,
optional selected-member list with aliases and separators, adjacent trivia, and exact source-owned
span as one concrete branch. The concrete tree MUST NOT decide what any path, alias, or member
resolves to. Missing segments, aliases, members, separators, and closing braces SHALL become
explicit parser recovery data while following top-level declarations remain parseable.

#### Scenario: Parse a namespace import

- **WHEN** the source spells `import compiler.Syntax` before a complete function declaration
- **THEN** the import branch retains both path segments and their dot followed by the complete function branch

#### Scenario: Parse a changed namespace alias

- **WHEN** the source spells `import compiler.Syntax as Tree`
- **THEN** the import branch retains the `as` keyword and `Tree` alias after the complete path

#### Scenario: Parse selected members with an alias

- **WHEN** the source spells `import compiler.Syntax { Node, parse, encode as encodeSyntax }`
- **THEN** the import branch retains three ordered member entries, both commas, and the changed local alias without inventing a namespace binding

#### Scenario: Parse a hybrid import

- **WHEN** the source spells `import compiler.Syntax as Tree { Node, parse }`
- **THEN** one import branch retains the complete path, namespace alias, and both selected members in concrete order

#### Scenario: Recover a missing path segment

- **WHEN** the source spells `import compiler. as Tree` before a function declaration
- **THEN** the import path contains an explicit missing identifier after the dot and recovery retains the alias and following function

#### Scenario: Recover a missing alias

- **WHEN** the source spells `import compiler.Syntax as` before a following declaration
- **THEN** the import branch contains a missing alias identifier with one parser diagnostic and the following declaration remains separate

#### Scenario: Recover a damaged selected list

- **WHEN** a selected-member list has a missing member, comma, alias, or closing brace
- **THEN** the damaged element remains explicit and recovery resumes at the next member boundary, closing brace, or following top-level declaration

#### Scenario: Parse multiple imports losslessly

- **WHEN** a source begins with two import declarations separated by trivia
- **THEN** both imports are separate concrete branches in source order and every token and trivia slice is retained exactly once

### Requirement: Binding statements remain explicit and recoverable

Each binding statement SHALL retain its `let` keyword, bound name, equals token, initializer
expression, adjacent trivia, and source-owned span as its own concrete branch. Missing names,
equals tokens, and initializer expressions SHALL become explicit missing elements with parser
diagnostics, and recovery SHALL resume at the next `let`, `return`, closing brace, following
declaration, or end-of-file. A `move` operand missing its name SHALL retain the keyword with an
explicit missing identifier. Statements after the return statement and blocks without a return
statement SHALL remain recoverable: extra statements stay in the tree as concrete branches, and a
missing return statement becomes an explicit recovered return structure covered by one
construct-level parser diagnostic.

#### Scenario: Recover a missing initializer

- **WHEN** a block spells `let value = return 42`
- **THEN** the binding retains a missing initializer expression at the `return` boundary with one parser diagnostic, and the return statement parses completely

#### Scenario: Recover a missing binding name

- **WHEN** a block spells `let = 42 return 0`
- **THEN** the binding contains a missing identifier before the equals token and both statements remain separate concrete branches

#### Scenario: Recover a missing return statement

- **WHEN** a block contains only binding statements before its closing brace
- **THEN** the block retains every binding followed by a recovered return structure with one parser diagnostic for the missing statement

#### Scenario: Recover a bare move

- **WHEN** an initializer spells `move` immediately before the statement boundary
- **THEN** the move operand retains its keyword and a missing identifier with one parser diagnostic

### Requirement: Qualified callees and signed literals remain recoverable

A qualified callee SHALL retain its actor identifier, dot, and operation identifier with exact
spans; a dot with a missing operation identifier SHALL become an explicit missing token with a
parser diagnostic while the argument list keeps parsing. A minus token not directly applicable to
a decimal literal in expression position SHALL remain recoverable: the minus stays in the tree
and the expression recovers at the existing statement and argument boundaries.

#### Scenario: Recover a missing operation name

- **WHEN** a body spells `i32.(1, 2)`
- **THEN** the callee retains the actor identifier and dot with an explicit missing identifier and one parser diagnostic, and both arguments remain parseable

#### Scenario: Recover a dangling minus

- **WHEN** a return expression spells `-` immediately before the closing brace
- **THEN** the minus token stays in the tree with an explicit missing decimal literal and one parser diagnostic

### Requirement: Conditional statements parse losslessly

The parser SHALL accept `if <expression> { <statements> } else { <statements> }` wherever a
statement may appear, with the `else` arm optional and no parentheses around the condition, per
the accepted surface. The taken arm SHALL be a brace-delimited statement sequence (bindings,
conditionals, and return statements). The `else` arm SHALL be either such a brace-delimited
statement sequence or a chained conditional statement introduced by `if` immediately after the
`else` keyword; the chained form SHALL be recorded as a conditional statement nested directly
inside the conditional it continues, so a chain of conditions SHALL introduce no additional node
kind and SHALL nest one level per arm. `if` SHALL introduce a chained arm only immediately after
`else`. The function body SHALL still end in exactly one trailing return statement after any
conditionals. `true` and `false` SHALL parse as boolean literal expressions wherever an expression
is allowed. Missing conditions, braces, and damaged arms SHALL remain explicit recovery data
bounded by the existing statement and declaration anchors, and every token SHALL be retained
losslessly, so the formatter SHALL print a chained arm as `} else if <condition> {` on one line.

#### Scenario: Parse a conditional with both arms

- **WHEN** a body spells `if flag { return 1 } else { return 2 } return 0`
- **THEN** the block contains one conditional statement retaining the keyword, condition expression, both brace-delimited arms, and the else keyword, followed by the trailing return statement

#### Scenario: Parse a conditional without an else arm

- **WHEN** a body spells `if flag { return 1 } return 0`
- **THEN** the conditional retains one arm and no else branch, and the trailing return remains a sibling statement

#### Scenario: Parse a chain of three conditions

- **WHEN** a body spells `if first { return 1 } else if second { return 2 } else if third { return 3 } else { return 4 } return 0`
- **THEN** the block contains one conditional statement whose else keyword is followed by a nested conditional statement, nested once more for the third condition, with the two chained conditionals retaining one brace-delimited arm each and the innermost retaining both arms, and the trailing return remains a sibling statement

#### Scenario: Print a chained arm on one line

- **WHEN** the formatter prints a conditional whose else arm is a chained conditional
- **THEN** the arm is printed as `} else if <condition> {` on one line, and printing the result again produces identical bytes

#### Scenario: Recover an else arm that is neither a block nor a chained if

- **WHEN** a body spells `if first { return 1 } else while second { }`
- **THEN** the else keyword retains an explicit missing arm with one parser diagnostic, and every token stays in the tree

#### Scenario: Parse boolean literals

- **WHEN** a return expression spells `true` and a binding initializer spells `false`
- **THEN** both are boolean literal expressions retaining their keyword tokens

#### Scenario: Recover a missing condition

- **WHEN** a body spells `if { return 1 } return 0`
- **THEN** the conditional retains an explicit missing condition with one parser diagnostic and both blocks keep parsing

#### Scenario: Recover a damaged arm before the next statement

- **WHEN** an arm omits its closing brace before the trailing return statement
- **THEN** recovery inserts the missing brace and the trailing return remains a separate statement

### Requirement: Operator expressions parse losslessly by precedence

Every expression position SHALL accept grouped, prefix, infix, callable-application, and pipeline
expressions in the closed precedence and associativity order defined by
`bootstrap-operator-semantics`. A pipeline expression SHALL retain its left operand, pipe token, and
complete callable expression on the right rather than a qualified-target-only branch. Every token
and trivia slice SHALL remain owned exactly once, and concrete structure SHALL not claim that an
operator or callable resolves successfully.

#### Scenario: Parse a precedence ladder

- **WHEN** a body returns `1 + 2 * 3 == 7`
- **THEN** the concrete tree nests multiplication inside addition and addition inside equality with every token retained

#### Scenario: Parse grouped prefix syntax

- **WHEN** a body returns `-(value + 1)` and another returns `!(left == right)`
- **THEN** each prefix expression owns one grouped operand with exact parentheses and source spans

#### Scenario: Parse a function-reference pipeline

- **WHEN** a body returns `flag |> bool.not`
- **THEN** the pipeline retains `bool.not` as a callable expression without an invented argument list

#### Scenario: Parse a pipeline chain

- **WHEN** a body returns `2 |> i32.add(3) |> transform`
- **THEN** the concrete tree associates the two pipeline branches left-to-right in source order

### Requirement: Operator recovery stays inside the containing expression

A missing prefix operand, infix right operand, grouping parenthesis, callable operand, call
delimiter, or pipeline right expression SHALL become explicit parser recovery data at the nearest
expression boundary. Unexpected operator sequences SHALL remain in error regions. Recovery SHALL
resume at the next operand, comma, closing parenthesis, statement keyword, closing brace, top-level
declaration, or end-of-file, preserving following statements and declarations.

#### Scenario: Recover a missing infix operand

- **WHEN** a return expression spells `1 +` immediately before its block's closing brace
- **THEN** the infix expression contains an explicit missing operand and the block retains its closing brace

#### Scenario: Recover a missing grouped parenthesis

- **WHEN** a return expression spells `(1 + 2` before the block's closing brace
- **THEN** the group contains a missing right parenthesis and the block boundary remains separate

#### Scenario: Recover a missing pipeline callable

- **WHEN** a body spells `value |>` before a following declaration
- **THEN** the missing callable and diagnostic remain in the pipeline branch while the following declaration parses independently

#### Scenario: Reject ungrouped comparison chaining

- **WHEN** a body spells `1 < 2 < 3`
- **THEN** the second comparison is retained as recovered unexpected syntax with a parser diagnostic

### Requirement: Callable types parse explicitly

Type positions SHALL parse `fn(A) -> B`, `mut fn(A) -> B`, and `once fn(A) -> B` as distinct
callable types with ordered parameter types, result type, invocation-mode tokens, trivia, recovery,
and exact spans. Missing parameter, parenthesis, arrow, result, or mode-adjacent function keyword
SHALL recover within the containing type boundary.

#### Scenario: Parse all invocation modes

- **WHEN** one declaration accepts `fn(i32) -> i32`, `mut fn(i32) -> i32`, and `once fn(i32) -> i32`
- **THEN** the syntax tree retains three distinct callable-type branches with complete tokens and spans

### Requirement: Run consumes the complete following expression

The operand of `run` SHALL extend through the complete following expression until the enclosing
comma, closing delimiter, block delimiter, or statement boundary. It SHALL include following
pipeline branches regardless of line breaks. Grouping `run` SHALL terminate the operand explicitly
and allow the executed success value to participate in a surrounding expression.

#### Scenario: Run a transformed Effect without grouping

- **WHEN** source spells `return run attempt |> Effect.retry(2)`
- **THEN** the `RunExpression` contains the complete pipeline as its operand

#### Scenario: Pipe the executed result with grouping

- **WHEN** source spells `(run attempt) |> i32.add(1)`
- **THEN** the grouped run is the left operand of the outer pipeline

#### Scenario: Stop run at an argument comma

- **WHEN** source spells `consume(run attempt |> Effect.retry(2), other)`
- **THEN** the comma ends the run operand and `other` remains the second argument

### Requirement: Struct declarations parse losslessly

Every top-level declaration position SHALL accept `pub` optionally followed by `struct`, a name,
and a braced field list. Each field SHALL retain an optional `pub`, a name, a colon, and an explicit
local, selected, or namespace-qualified type path. Fields SHALL be recognized sequentially without
requiring punctuation between complete field declarations. The concrete tree SHALL own every token
and trivia slice exactly once and SHALL NOT claim that names, types, or visibility are semantically
valid.

#### Scenario: Parse a public struct

- **WHEN** source declares `pub struct Token { pub kind: i32 lexeme: Text }`
- **THEN** the tree retains the public struct, both ordered fields, the public first field, and every delimiter and trivia slice

#### Scenario: Parse an empty struct

- **WHEN** source declares `struct Marker {}`
- **THEN** the tree retains one default-private struct with an empty field list and exact braces

#### Scenario: Parse a qualified field type

- **WHEN** a field declares type `Tree.Node`
- **THEN** its type syntax retains the namespace, dot, and member independently of later name resolution

### Requirement: Struct recovery remains inside its declaration

A missing struct name, brace, field name, colon, or field type SHALL become explicit recovery data
at the nearest struct or field boundary. Recovery SHALL resume at the next `pub`, field-shaped name,
closing brace, top-level declaration keyword, or end-of-file. Unexpected tokens SHALL remain in
error regions, and a damaged struct MUST NOT consume a following top-level declaration.

#### Scenario: Recover a missing field type

- **WHEN** one field ends after its colon before a following field
- **THEN** the first field contains an explicit missing type and the following field parses independently

#### Scenario: Recover a missing closing brace

- **WHEN** a struct omits its closing brace before a following function declaration
- **THEN** the struct receives a missing brace and the function remains a separate top-level declaration

#### Scenario: Recover a missing field name

- **WHEN** a struct contains `pub : i32` before its closing brace
- **THEN** the field retains an explicit missing name and the struct retains its closing brace

### Requirement: Struct literals parse losslessly

Every expression position SHALL accept a one- or two-segment type path followed by a braced,
comma-separated labeled field-initializer list. Each initializer SHALL retain its field name, colon,
expression, separators, trivia, and exact source span. An empty list SHALL remain valid. Concrete
syntax MUST NOT decide whether the target is a struct, construction is authorized, fields are
complete, or initializer types match.

#### Scenario: Parse a labeled literal

- **WHEN** source contains `Token { kind: kind, lexeme: move lexeme }`
- **THEN** the tree retains the target path, braces, both labeled initializers, comma, nested move expression, and every trivia slice exactly once

#### Scenario: Parse an empty literal

- **WHEN** an expression contains `End {}`
- **THEN** the tree contains one complete zero-field struct-literal branch

### Requirement: Field projections are postfix expressions

Every expression SHALL accept one or more `.field` postfix projections after a primary expression.
Projection SHALL bind more tightly than prefix, infix, equality, and pipeline operations and SHALL
associate left-to-right. The concrete tree SHALL retain each dot and field token independently of
whether the subject is a value or the field resolves.

#### Scenario: Parse a chained projection

- **WHEN** source returns `token.span.start + 1`
- **THEN** the tree nests `token.span` then `.start` before the addition with every token retained

#### Scenario: Distinguish projection from a qualified call

- **WHEN** source contains `Token.make(1)` and `token.kind`
- **THEN** the first remains a qualified call target and the second remains a field-projection expression

### Requirement: Struct-value recovery stays inside the expression

A missing literal target, brace, field name, colon, initializer, comma, projection member, or
projection subject SHALL become explicit recovery data at the nearest expression boundary.
Recovery SHALL preserve later field initializers, arguments, statements, closing braces, and
top-level declarations without interpreting damaged syntax as a complete value.

#### Scenario: Recover a missing field initializer

- **WHEN** a literal contains `Pair { left:, right: 2 }`
- **THEN** `left` retains a missing expression and `right` remains a separate complete initializer

#### Scenario: Recover a missing projection member

- **WHEN** a body contains `value.` before its return boundary
- **THEN** the projection retains a missing field identifier without consuming the following statement or declaration

### Requirement: Fixed-array type syntax is lossless and recoverable

The parser SHALL recognize `[T; N]` wherever an explicit type is accepted, retaining the left
bracket, nested element-type syntax, semicolon, non-negative decimal length, right bracket, trivia,
missing elements, and exact spans. Recovery SHALL remain inside the type constructor. The former
`Array<T, N>` spelling SHALL NOT remain as a compatible fixed-array source form.

#### Scenario: Parse a nested array type

- **WHEN** a field declares `cells: [[i32; 4]; 3]`
- **THEN** the concrete tree retains both nested constructors and every punctuation token byte-for-byte

#### Scenario: Recover a missing array length

- **WHEN** a declaration contains `[i32; ]`
- **THEN** the tree records missing length syntax without consuming the following declaration

#### Scenario: Reject the former array spelling

- **WHEN** an explicit type position contains `Array<i32, 4>`
- **THEN** the parser does not produce a fixed-array type or silently translate the former spelling

### Requirement: JSX-like template starts are reserved at primary-expression boundaries

When a primary expression is required, `<` immediately followed by a tag identifier or `>` SHALL be
reserved for a future built-in template expression. Until template expressions are implemented,
the parser SHALL reject that reserved syntax with a parser-owned diagnostic, SHALL NOT reinterpret
it as prefix type syntax or an infix relational expression, and SHALL recover without consuming a
following statement or declaration. A relational token following an already-started expression
SHALL retain its existing operator meaning.

#### Scenario: Reserve an element start

- **WHEN** a return expression starts with `<Button />` before a following statement
- **THEN** the parser reports reserved template syntax and the following statement parses independently

#### Scenario: Reserve a fragment start

- **WHEN** a return expression starts with `<>` before a following statement
- **THEN** the parser reports reserved template syntax and the following statement parses independently

#### Scenario: Preserve relational less-than

- **WHEN** an expression spells `left < right`
- **THEN** `<` remains the non-associative relational operator between the two expressions

### Requirement: Array literals parse losslessly

The parser SHALL recognize empty and comma-separated array literals in every expression position,
including nested literals and an optional trailing comma. It SHALL retain element source order,
punctuation, trivia, recovery nodes, and exact spans.

#### Scenario: Parse a nested literal

- **WHEN** source contains `[[1, 2], [3, 4]]`
- **THEN** the concrete tree contains two nested element lists in exact source order

### Requirement: Indexing is a repeated postfix expression

Indexing SHALL parse as a repeated postfix operation that binds more tightly than prefix, infix,
equality, and pipeline expressions and composes left-to-right with field projection and calls.
Recovery for a missing subject, bracket, index expression, or closing bracket SHALL stay within the
containing expression.

#### Scenario: Parse a mixed place chain

- **WHEN** source contains `matrix[row][column].value`
- **THEN** the tree nests two index projections and one field projection from left to right

#### Scenario: Recover a missing closing bracket

- **WHEN** source contains `values[index` before a return boundary
- **THEN** the index node records the missing bracket without consuming the following statement

### Requirement: Mutable bindings and assignments parse losslessly

The parser SHALL recognize `let mut name = expression` and statement-form `place = expression`,
where a place is a binding followed by zero or more field or index projections. An identifier-led
construct SHALL be classified as an assignment only when the complete place is followed by `=`.
The concrete tree SHALL retain the `mut` token, assignment token, complete place syntax,
expression, trivia, and exact spans. Assignment SHALL remain distinct from equality and SHALL NOT
be an expression.

#### Scenario: Parse an indexed field update

- **WHEN** source contains `pairs[index].left = value`
- **THEN** the tree retains the binding root, index projection, field projection, assignment token, and right-hand expression in source order

#### Scenario: Distinguish assignment from equality

- **WHEN** a loop body contains both `current = next` and `current == next`
- **THEN** the first is an assignment statement and the second remains an equality expression

#### Scenario: Recover a final identifier as a missing-keyword return

- **WHEN** a required-return block ends with `foo` immediately before its closing brace
- **THEN** `foo` is the recovered return expression, one parser diagnostic identifies the missing `return` keyword, and no assignment syntax is synthesized

### Requirement: Owned parameter mutability parses losslessly

An ordinary or Effect function parameter MAY contain one `mut` token immediately before its name. The
concrete parameter node SHALL retain that token, its trivia, name, colon, and type in source order.
Recovery for a missing name, colon, or type, or for a duplicate or misplaced `mut`, MUST remain
inside the damaged parameter and preserve every following comma-separated parameter.

#### Scenario: Parse a mutable owned parameter

- **WHEN** source declares `fn update(mut value: Token) -> Token`
- **THEN** the parameter node retains `mut value: Token` without changing the parameter type syntax

#### Scenario: Recover duplicate parameter mutability

- **WHEN** source contains `mut mut value: Token, next: i32`
- **THEN** the second `mut` is retained as one local unexpected token and `next` remains a complete following parameter

### Requirement: Structured loop syntax is lossless and recoverable

The parser SHALL recognize `while condition { statements }`, bare `break`, and bare `continue` as
statements. Loop bodies SHALL retain ordinary bindings, assignments, conditionals, nested loops,
returns, and control transfers in exact source order. Missing conditions, braces, assignment sides,
or damaged transfers SHALL recover within the nearest statement or loop boundary without consuming a
following statement or declaration.

#### Scenario: Parse nested loops and transfers

- **WHEN** a loop body contains a nested loop with `continue` and then an outer `break`
- **THEN** the concrete tree preserves both loop regions and both transfer statements at their exact spans

#### Scenario: Recover a missing loop brace

- **WHEN** a damaged `while` body reaches the next top-level function declaration
- **THEN** the loop records missing closing syntax without consuming that declaration

### Requirement: Structural union type syntax is lossless and recoverable

The parser SHALL recognize `|` as a type-level union separator in every supported type position.
Union parsing SHALL preserve every member, separator, parenthesis, qualification, trivia item, and
exact span in source order while allowing semantic normalization to erase order, duplicates, and
nesting later. Type-level `|` SHALL remain distinct from expression operators.

#### Scenario: Parse a nested qualified union

- **WHEN** a return type contains `syntax.Token | (syntax.End | syntax.Token)`
- **THEN** the concrete tree retains all three member spellings, both separators, parentheses, and exact spans

#### Scenario: Recover a missing union member

- **WHEN** a parameter type contains `Token |` before its closing delimiter
- **THEN** the union records an explicit missing member without consuming the following parameter or function body

### Requirement: Match expressions are lossless in every expression position

Every expression position SHALL accept `match` followed by an optional `move`, `&`, or `&mut` mode,
one scrutinee expression, and a braced source-ordered arm list. Each arm SHALL contain a nominal or
universal pattern, an optional `if` guard expression, `=>`, and one result expression. Newlines and
trivia MAY separate arms without a comma. The concrete tree SHALL retain every token, pattern,
guard, arm boundary, trivia item, and exact span without deciding coverage or types.

#### Scenario: Parse a consuming match initializer

- **WHEN** a binding initializes from `match move event { Token { kind, .. } => kind End {} => 0 }`
- **THEN** the concrete tree retains one match expression with its mode, scrutinee, two ordered arms, patterns, results, and punctuation

#### Scenario: Parse a guarded shared match

- **WHEN** a return expression matches `&event` with a guarded nominal arm followed by `_`
- **THEN** the tree retains the ampersand, guard expression, both fat arrows, and universal identifier in source order

### Requirement: Nominal patterns are lossless and recursively recoverable

A nominal pattern SHALL retain its one- or two-segment type path, braces, ordered field patterns,
commas, nested nominal patterns, shorthand bindings, explicit local names, and optional `..` marker.
Missing type names, fields, colons, nested patterns, commas, or braces SHALL remain explicit recovery
data bounded by the next field, guard, fat arrow, arm, closing brace, statement, or declaration.

#### Scenario: Parse a nested renamed binding

- **WHEN** a pattern spells `Token { span: Span { start: offset, .. }, .. }`
- **THEN** the tree retains both nominal patterns, both omission markers, and the renamed leaf binding exactly

#### Scenario: Recover a damaged arm locally

- **WHEN** one pattern or guard is damaged before its fat arrow
- **THEN** its recovery nodes and diagnostics remain inside that arm while the following arm and enclosing expression remain parseable

### Requirement: Type parameter and application syntax is contextual and recoverable

The parser SHALL represent angle-bracket type parameter lists after struct and function declaration
names, generic applications in type positions, and explicit specialization after a recognized
callee. Generic brackets MUST NOT consume comparison operators, and reserved JSX-like template
starts SHALL remain reserved only at primary-expression boundaries. Missing names, commas, closing
brackets, and type arguments SHALL remain explicit local syntax nodes and diagnostics.

#### Scenario: Parse a generic declaration and call
- **WHEN** source contains `pub fn identity<T>(value: T) -> T` and `identity<i32>(1)`
- **THEN** syntax records the declaration parameter and call specialization losslessly

#### Scenario: Preserve a comparison
- **WHEN** source contains `left < right`
- **THEN** the expression remains a comparison rather than a damaged generic application

#### Scenario: Keep a reserved template start distinct
- **WHEN** `<Panel />` appears where a primary expression begins
- **THEN** the parser preserves the reserved template start rather than treating `Panel` as a type argument

### Requirement: Lexical slice syntax is lossless and recoverable

The parser SHALL recognize shared `&[T]` and exclusive `&mut [T]` type branches and prefix `&` and
`&mut` borrow-expression branches. It SHALL retain every ampersand, keyword, bracket, nested element
type, trivia token, recovery element, and exact source-owned span without deciding whether the
operand is borrowable or the type is permitted at that source position.

#### Scenario: Parse a shared slice parameter and borrow argument

- **WHEN** source spells `fn fold(values: &[i32]) -> i32 { return use(&values) }`
- **THEN** the tree retains one shared slice type and one shared borrow expression with their punctuation and provenance in source order

#### Scenario: Parse an exclusive slice parameter and borrow argument

- **WHEN** source spells `fn edit(values: &mut [i32]) -> i32 { return use(&mut values) }`
- **THEN** the tree retains both `mut` keywords under distinct exclusive slice-type and borrow-expression branches

#### Scenario: Recover a damaged slice type

- **WHEN** a parameter starts a slice type but omits its element or closing bracket before the parameter boundary
- **THEN** the parser inserts explicit missing syntax, preserves following parameters and the function body, and emits deterministic parser diagnostics

### Requirement: Effect and failure syntax is lossless and locally recoverable

The syntax layer SHALL preserve `effect {}`, `effect fn`, `!` failure rows, prefix `run`, Copy or
moved `fail`, and an explicit catch type argument on a callable section with every token and source
span. Recovery from a missing function keyword, row member, run operand, or failure operand SHALL
stay within the damaged declaration or statement.

#### Scenario: Parse the complete surface

- **WHEN** source declares an Effect with two failure members, constructs it, runs it, and originates one failure
- **THEN** the syntax tree retains distinct Effect construction, failure row, run expression, and fail statement nodes

#### Scenario: Recover a missing row member

- **WHEN** `!` is followed by a body delimiter
- **THEN** one missing type is retained and the following body remains a separate block

#### Scenario: Parse pipelined exact recovery

- **WHEN** an Effect recipe is piped into `Effect.catch<E>(handler)`
- **THEN** the callable section retains the explicit `E` and the handler argument losslessly

### Requirement: Effect and owned-allocation syntax is lossless and recoverable

The syntax layer SHALL preserve `effect {}`, `effect fn`, `Effect` actor calls, `run`, Copy or moved
`fail`, allocator requirements, explicit consuming `drop`, restricted Drop declarations, and
qualified unsafe allocation/buffer operations. The lexer SHALL reserve `effect` as the sole
effect-function keyword.

#### Scenario: Recover a damaged effect allocation body

- **WHEN** source contains an effect function with a damaged unsafe allocation call followed by a valid `Effect.catch` pipeline
- **THEN** the syntax tree retains bounded damaged allocation nodes and the later Effect pipeline without inventing legacy effect or allocation-scope nodes

### Requirement: Unsafe allocation and Drop forms are explicit and bounded

The lexer and parser SHALL preserve explicit `unsafe { ... }` boundaries containing ordinary
qualified calls to the raw allocation or typed-storage seam, restricted `impl Drop for Name`
declarations, ordinary `impl Allocator for Name` conformance declarations, and consuming
`drop value` statements for early cleanup. These forms SHALL reuse
ordinary qualified calls, type arguments, Effect failure and requirement rows, roles, blocks, and
expression precedence. The grammar MUST NOT introduce a named lifetime scope, allocator-specific
block, or special allocation-call syntax. Missing unsafe delimiters, Drop declaration parts, call
delimiters, type arguments, or drop operands SHALL recover locally and deterministically.

#### Scenario: Preserve an allocating Effect

- **WHEN** source contains `Allocator.allocate(layout)` inside `unsafe { ... }` under an allocator requirement and later `drop allocation`
- **THEN** syntax and canonical formatting retain the unsafe boundary, qualified call, requirement, failure path, drop statement, and every source span

#### Scenario: Preserve a restricted Drop declaration

- **WHEN** a nominal actor declares `impl Drop for Guard { ... }`
- **THEN** the syntax tree distinguishes the restricted conformance from an ordinary interface implementation and retains its target, hook body, and tokens losslessly

#### Scenario: Preserve an allocator conformance

- **WHEN** source declares `impl Allocator for TestAllocator` and maps its operation to the provider actor
- **THEN** the syntax tree retains the capability, nominal provider, operation mapping, and source spans without erasing the provider type

#### Scenario: Recover after a damaged unsafe call

- **WHEN** an unsafe typed-slot operation omits an argument or closing delimiter before a valid following statement
- **THEN** parsing records explicit missing syntax inside the operation and preserves the following statement without inventing scope syntax

### Requirement: Impl declarations accept a contextual type-parameter list

`impl` declarations SHALL accept an optional type-parameter list between the keyword and the
capability path, using the same contextual angle-bracket discipline as function and struct type
parameters. The list SHALL parse losslessly, format canonically, and recover locally: a malformed
parameter list confines its errors to the impl declaration and the following top-level declaration
still parses.

#### Scenario: Parse a parametric conformance losslessly

- **WHEN** source declares `impl<T> Drop for Vector<T> { ... }`
- **THEN** the syntax tree retains the parameter list, capability, and target with full-fidelity reproduction of the original text

#### Scenario: Recover from a malformed parameter list

- **WHEN** an impl type-parameter list is unclosed or contains an unexpected token
- **THEN** the parser reports deterministic diagnostics inside the impl declaration and the next top-level declaration parses normally

### Requirement: Whole-member binding patterns parse losslessly

Match patterns SHALL accept `Member name` alongside field destructuring, parsing losslessly with
canonical formatting and local recovery inside the containing arm.

#### Scenario: Parse and format a binding pattern

- **WHEN** source matches with arms `Empty nothing => 0` and `Full full => 1`
- **THEN** the syntax tree retains both binding patterns with full-fidelity reproduction and the formatter prints them canonically

### Requirement: Floating literal syntax is lossless

The parser SHALL preserve decimal point, exponent marker/sign, leading sign, digits, trivia, recovery elements, and exact spans without rounding during syntax construction.

#### Scenario: Parse exponent notation

- **WHEN** source contains `-1.25e-3`
- **THEN** syntax retains every component as one recoverable expression

### Requirement: Text and byte literals parse losslessly

The parser SHALL accept single-line and multiline text and byte literal tokens as static-literal
primary expressions. It SHALL preserve the recognized modifier, one-quote or three-quote
delimiters, content, escapes, trivia, recovery elements, and exact spans without decoding storage
during syntax construction. Every complete literal form SHALL remain valid anywhere a primary
expression is accepted, including as the left operand of a pipeline.

#### Scenario: Recover a malformed escape

- **WHEN** a complete literal contains a malformed escape
- **THEN** the literal remains one lossless static-literal expression, damage remains local, and the following statement remains parseable

#### Scenario: Parse every literal width and category

- **WHEN** one body contains `"text"`, `b"bytes"`, `"""text"""`, and `b"""bytes"""`
- **THEN** the syntax tree contains four static-literal expressions whose tokens reproduce every modifier, delimiter, and content byte exactly

#### Scenario: Parse piped literal operands

- **WHEN** single-line and multiline text and byte literals each appear before `|>`
- **THEN** every pipeline retains the complete literal expression as its left operand and the complete callable expression as its right operand

### Requirement: Top-level constants are lossless and recoverable

The parser SHALL recognize `[pub] const <name>: <type> = <literal>` as a top-level constant
declaration, retaining every modifier, separator, literal token, trivia slice, and source span.
Recovery SHALL stop at the next top-level declaration or end-of-file and SHALL keep missing names,
colons, types, equals signs, and literals explicit.

#### Scenario: Parse a public typed integer constant

- **WHEN** source spells `pub const opcode_add: u8 = 1`
- **THEN** the source tree contains one complete constant declaration with exact token provenance

#### Scenario: Recover before a following function

- **WHEN** a damaged constant declaration is followed by a valid function declaration
- **THEN** the damage remains inside the constant and the function remains a separate complete branch

### Requirement: Service declarations are lossless and recoverable

The syntax SHALL preserve `service` declarations, visibility, nominal names, generic parameters,
operation names, complete function contracts, documentation, delimiters, and source spans. Service
operations SHALL reuse ordinary function-contract syntax rather than introduce method bodies or
stored fields. Missing names, contracts, or delimiters MUST recover inside the declaration without
consuming a following top-level member.

#### Scenario: Parse a service contract

- **WHEN** source declares a public service with effectful operations and explicit failure and requirement rows
- **THEN** the syntax tree retains every token and distinguishes the service from a struct, interface implementation, and source module

#### Scenario: Recover a damaged service operation

- **WHEN** one service operation omits a parameter delimiter before a valid following operation
- **THEN** the parser records local missing syntax and preserves the following operation and declaration

### Requirement: Contract-row algebra and constraints parse losslessly

The parser SHALL recognize contextual `Without<R, S>` expressions only where failure or requirement
rows are expected. Both operands SHALL use ordinary row-union precedence, and a value member in a
row context SHALL be preserved as a singleton member expression. Function declarations SHALL accept
one comma-separated `where` clause containing kind-directed `S in R` and fixed-mode
`&P provides S from R`, `&mut P provides S from R`, or `P provides S from R` constraints.

Call generic arguments SHALL accept a contiguous kind-correct prefix containing value, failure-row,
or requirement-row arguments. `Without`, `where`, `in`, `provides`, and `from` SHALL remain ordinary
identifiers outside their contextual grammar positions. Missing operands, separators, or constraint
terms SHALL produce bounded missing/unexpected nodes without consuming the next declaration.

#### Scenario: Parse nested row difference and constraints

- **WHEN** a declaration returns `Effect<A ! Without<E, First | Third> ? Without<R, S>> where First | Third in E, &mut P provides S from R`
- **THEN** the syntax tree retains both row-difference operands, union members, fixed exclusive provider mode, selected row, source row, commas, and contextual keywords losslessly

#### Scenario: Parse a row-generic call prefix

- **WHEN** `effect |> Effect.provideMut<Logger at Audit>(&mut provider)` is parsed
- **THEN** the first generic argument remains a requirement-row expression and later omitted generic binders remain absent rather than synthesized syntax

#### Scenario: Recover a malformed constraint locally

- **WHEN** one constraint in a `where` list is missing `from R`
- **THEN** recovery records the missing terms inside that constraint and resumes at the next comma or declaration boundary

#### Scenario: Keep contextual words usable as identifiers

- **WHEN** `without`, `where`, `in`, `provides`, or `from` occurs outside the corresponding row or constraint grammar position
- **THEN** it parses under the ordinary identifier rules

### Requirement: Shared pattern positions are lossless and recursively recoverable

The parser SHALL build one lossless recursive pattern grammar for match arms, unconditional local
bindings, and statement-form conditional bindings. The grammar SHALL retain exact type selectors,
whole-value bindings, field shorthand, field renaming, nested nominal destructuring, rest markers,
wildcards, access-bearing initializer expressions, separators, trivia, and recovery tokens without
giving patterns executable call semantics.

#### Scenario: Parse one recursive local pattern

- **WHEN** source writes `let Pair { point: Point { x, .. }, extra } = move pair`
- **THEN** the syntax tree retains one nested pattern tree and the complete initializer expression

#### Scenario: Parse statement-form if-let

- **WHEN** source writes `if let i32 number = &value { use(number) } else { fallback() }`
- **THEN** the syntax tree retains the pattern, initializer, taken body, and optional mismatch body

### Requirement: Scalar enum declarations are lossless and recoverable

The parser SHALL retain a scalar enum declaration's optional visibility, `enum` keyword, optional
parenthesized representation type, name, braces, ordered members, commas, trivia, and exact spans.
Each member SHALL retain its name and optional equals plus signed decimal integer literal without
assigning or validating a discriminant. Missing or unexpected representation and member elements
SHALL use explicit missing or error syntax, and recovery SHALL resume at the next comma, closing
brace, following declaration, or end-of-file.

#### Scenario: Parse default and represented enums

- **WHEN** source contains one default enum and one `enum(u8)` with explicit discriminants
- **THEN** both declarations retain all tokens and members in source order without semantic width decisions

#### Scenario: Recover a damaged member

- **WHEN** one member contains an unexpected token before a later comma-bounded member
- **THEN** the damage remains in an error region and the later member plus following declaration remain parseable

### Requirement: Qualified enum member paths remain source-faithful expressions and patterns

The syntax tree SHALL retain `EnumName.Member` with both identifiers and the dot in expression and
match-pattern positions. Syntax SHALL NOT decide whether the qualifier is an enum, whether the member
exists, or whether it belongs to the scrutinee type.

#### Scenario: Parse one enum member match arm

- **WHEN** a match arm pattern spells `AssertionResult.Pass`
- **THEN** syntax retains the complete qualified path and exact span without treating it as an integer pattern

### Requirement: Nominal union syntax is lossless and recoverable

The parser SHALL retain optional declaration visibility, the `union` keyword, name, optional type
parameters, ordered unit and named-field variants, field visibility and types, separators, braces,
comments, and unavailable recovery elements in one lossless union CST. Expression and pattern
syntax SHALL parse a nominal union path with an optional contiguous explicit generic prefix followed
by a dot and variant name. A constructor MAY then have a named-field body; a pattern SHALL use a
complete applied parent and MAY have the selected variant's named-field pattern body.

#### Scenario: Parse a generic mixed union

- **WHEN** source declares `union Option<T> { None, Some { pub value: T } }`
- **THEN** the CST retains the parent type parameter and distinct unit variant, field variant, and field nodes with exact spans

#### Scenario: Parse an applied variant constructor

- **WHEN** an expression spells `Result<i32, Problem>.Success { value: 42 }`
- **THEN** the CST treats `Result<i32, Problem>` as the applied parent qualifier and `Success` as its variant rather than attaching the arguments to a detached member

#### Scenario: Parse a constructor with an omitted parent suffix

- **WHEN** an expression spells `Option.Some { value: 42 }`
- **THEN** the CST retains `Option` as the unapplied parent declaration path and leaves generic completion to semantic field inference

#### Scenario: Recover within one damaged variant

- **WHEN** a named-field variant has a missing field type or closing brace beside valid sibling variants
- **THEN** recovery remains within that declaration and preserves the valid siblings as available syntax

### Requirement: Expression nesting is bounded and losslessly recoverable

The parser SHALL accept at most 256 nested expression edges. An expression parsed directly in a
statement, declaration, or other non-expression position SHALL have depth zero. Beginning an
expression while another containing expression remains active SHALL increment the containing
depth by one, including grouped contents, prefix operands, operator operands, pipeline targets,
call arguments, array or aggregate elements, field initializers, match components, and every other
expression child. Sequential sibling expressions SHALL each use their common parent's depth plus
one and SHALL NOT accumulate depth from preceding siblings.

Concrete syntax whose maximum depth is at most 256 SHALL retain its ordinary concrete shape. When
parsing would begin a child at depth 257, the parser SHALL instead retain the over-budget region as
one explicit error branch, consume at least the first significant token of that child, preserve all
source tokens exactly once, and resume at the owning expression boundary. Recovery SHALL remain
host-stack independent even when the rejected region is substantially deeper than the limit.

#### Scenario: Preserve syntax immediately below the limit

- **WHEN** a valid expression has maximum depth 255
- **THEN** the parser produces its ordinary concrete syntax without an excessive-nesting diagnostic

#### Scenario: Preserve syntax at the limit

- **WHEN** a valid expression has maximum depth exactly 256
- **THEN** the parser produces its ordinary concrete syntax without an excessive-nesting diagnostic

#### Scenario: Recover the first edge beyond the limit

- **WHEN** parsing a child expression would increase the active expression depth from 256 to 257
- **THEN** that child is represented by one explicit error branch and parsing advances beyond the offending syntax

#### Scenario: Bound every recursive expression form

- **WHEN** grouping, array nesting, call or container nesting, or direct prefix nesting extends substantially beyond depth 256
- **THEN** each form returns a lossless recovered syntax tree without exhausting the host stack

#### Scenario: Measure siblings independently

- **WHEN** one container contains multiple sibling expressions whose individual depths do not exceed 256
- **THEN** earlier siblings do not reduce the nesting available to later siblings

#### Scenario: Resume after an over-budget expression

- **WHEN** an over-budget expression is followed by another statement in its block and another top-level declaration
- **THEN** both following constructs remain independently parseable and no token from either is consumed into the recovered expression

### Requirement: Static iteration syntax is lossless and recoverable

The parser SHALL recognize `static for <binding> in <expression> { <statements> }` exactly as a
statement form. The syntax tree SHALL retain the `static`, `for`, and `in` keywords, binding,
iterable expression, body, trivia, delimiters, and source spans without deciding whether the
iterable is static, finite, homogeneous, heterogeneous, or reflectable. The form MUST NOT parse in a
declaration list or create a declaration node.

#### Scenario: Parse one static field iteration

- **WHEN** source contains `static for field in Reflect.fields<Args>() { display(field) }`
- **THEN** syntax retains one static-for statement with its complete iterable call and body in concrete order

### Requirement: Static iteration recovery remains locally bounded

A missing iteration binding, `in` keyword, iterable expression, opening brace, body, or closing
brace SHALL remain explicit missing syntax under the existing bounded recovery rules. Recovery
MUST preserve the following statement, enclosing block boundary, and following declaration.

#### Scenario: Recover a missing iterable

- **WHEN** `static for field in` is followed by a block and then a return statement
- **THEN** the static-for node records a missing iterable, retains its block, and preserves the following return

### Requirement: Static forms are lossless and phase-marked

The lexer and parser SHALL recognize `static` and `compileError` as keywords and preserve them
exactly in five initial forms: top-level `static fn` declarations, `static` parameter modifiers,
`let static` bindings, statement-position `static if`, and `compileError(message)` expressions or
statements. A static function MAY be prefixed by `pub` but MUST NOT combine `static` with `unsafe`,
`effect`, an implementation operation, or a service or interface operation. Static parameters and
static bindings MUST NOT also carry `mut`. Static functions SHALL otherwise use ordinary function
grammar; static parameters SHALL
retain an explicit type; static bindings SHALL retain an initializer; static conditionals SHALL
retain a condition, one block, and an optional `else` block; and `compileError` SHALL retain one
message expression without a trailing comma. The syntax tree SHALL record each form distinctly
without deciding whether an
expression is statically evaluable. `compileError` SHALL be dedicated syntax and MUST NOT parse as
an identifier call that source can import, shadow, capture, or pass as a value.

#### Scenario: Parse every initial static form

- **WHEN** source contains a static helper and a mixed function using a static parameter, static binding, static conditional, and `compileError`
- **THEN** the syntax tree retains every keyword, parameter, initializer, condition, arm, compile-error argument, trivia slice, and source span in concrete order

#### Scenario: Keep an ordinary literal ordinary in syntax

- **WHEN** a text or numeric literal appears as an argument to a static parameter
- **THEN** syntax retains the ordinary literal expression without inserting a synthetic `static` node

### Requirement: Static syntax recovery remains locally bounded

A missing static function name, parameter name or type, binding initializer, conditional condition
or arm, or compile-error argument or delimiter SHALL produce explicit missing syntax under the
existing bounded recovery rules. Recovery inside a static or compile-error form MUST preserve a
following statement, closing block, or declaration. `static if` SHALL be rejected in every
declaration-list position, while `static fn` SHALL remain the only static form that introduces a
declaration.

#### Scenario: Recover a damaged static conditional

- **WHEN** a static conditional omits its condition or closing brace before a following return
- **THEN** syntax records the missing element, preserves the following return, and terminates recovery without a cascade

#### Scenario: Reject a conditional top-level declaration

- **WHEN** a module places `static if` around a function declaration
- **THEN** parsing retains the damaged region and following declarations but produces no conditional declaration node

### Requirement: Applied qualified members remain lossless until declaration resolution

The expression grammar SHALL accept the declaration-neutral shape `Path<Arguments>.member`. The
syntax tree SHALL preserve the owner path, every ordered type argument, the selected member,
punctuation, trivia, and owner-qualified spans without declaring the owner to be an interface or a
nominal union and without reinterpreting owner arguments as operation-generic arguments. Semantic
resolution MAY then interpret a complete interface application as the qualifier of an operation in
a direct call or as a callable expression on the right of a pipeline. Existing nominal-union
constructors, unit values, and patterns SHALL retain the same lossless shape and meaning after
declaration resolution.

#### Scenario: Parse an applied interface operation call

- **WHEN** source evaluates `Encodable<u32>.encode(&age)`
- **THEN** syntax retains `Encodable<u32>` as the applied qualifier of operation `encode` and retains the shared-borrow argument as the call operand

#### Scenario: Preserve an applied nominal-union member

- **WHEN** source evaluates or patterns on an existing generic nominal-union member such as `Option<i32>.None`
- **THEN** syntax retains the same applied owner and member tokens and semantic resolution preserves the nominal-union meaning

#### Scenario: Keep owner and operation arguments distinct

- **WHEN** source contains `Interface<A>.operation<B>(value)`
- **THEN** syntax retains `A` on the applied owner and `B` on the operation call without merging or exchanging the argument lists

#### Scenario: Parse an applied interface operation section

- **WHEN** source evaluates `&age |> Encodable<u32>.encode`
- **THEN** syntax retains the applied qualifier as the pipeline's callable right expression and does not invent a method call or implicit argument

#### Scenario: Keep run greedy across an applied operation pipeline

- **WHEN** source evaluates `run &age |> Encodable<u32>.encode`
- **THEN** the `run` operand remains the complete pipeline under the existing run-expression boundary

#### Scenario: Recover a damaged applied qualifier locally

- **WHEN** an applied interface qualifier omits an argument delimiter or operation name before a valid following statement
- **THEN** syntax records explicit missing structure within that expression and preserves the following statement without a declaration-level cascade

### Requirement: Tuple and contextual record syntax is lossless and recoverable

The lexer and parser SHALL reserve `tuple` and parse `tuple Name(T0, T1, ...)` as a declaration with
ordered explicit element types. Expression grammar SHALL distinguish parenthesized expressions,
unit, positional tuple literals, named tuple construction calls, positional projections such as
`.0`, and record literals beginning with `.{`. Record literal members SHALL contain an identifier,
colon, and expression. Tuple and record lists SHALL preserve commas, optional trailing commas,
trivia, and exact source spans.

Missing tuple names, element types, elements, record labels, colons, values, commas, or closing
delimiters SHALL use the existing explicit-missing syntax and bounded expression or declaration
recovery. A colon inside a positional tuple literal SHALL NOT create labeled-tuple syntax. The
leading dot on a record literal SHALL keep it distinct from a block in every expression position.

#### Scenario: Parse the aggregate forms distinctly

- **WHEN** source contains a tuple declaration, named construction, positional literal, positional projection, record literal, grouped expression, and unit
- **THEN** syntax retains seven distinct source forms with every token, delimiter, trivia slice, and span in source order

#### Scenario: Recover a damaged record literal

- **WHEN** one record member omits its colon or value before a following member and statement
- **THEN** recovery records the missing syntax, preserves the following member and statement, and terminates without a declaration-level cascade

#### Scenario: Reject labeled tuple syntax

- **WHEN** a positional tuple literal contains `name: value`
- **THEN** syntax reports the unexpected colon without reinterpreting the literal as a record or block

### Requirement: Referent projection is a postfix place expression

The parser SHALL recognize `reference.*` as a postfix referent projection within the repeated
projection chain. Referent projection SHALL bind more tightly than prefix, infix, and pipeline
operators, compose with field, index, call, borrow, and assignment syntax, and remain distinct from
infix multiplication.

#### Scenario: Parse a scalar receiver projection

- **WHEN** source contains `self.*`
- **THEN** the parser produces a referent-projection expression whose subject is `self`
- **AND** the dot and star tokens belong to that projection

#### Scenario: Compose a referent with other projections

- **WHEN** source contains `items[index].*.field`
- **THEN** parsing retains the index, referent, and field projections in source order
- **AND** the chain can be used as a borrow or assignment subject

#### Scenario: Preserve multiplication syntax

- **WHEN** source contains `left * right`
- **THEN** parsing produces an infix multiplication expression rather than a referent projection

#### Scenario: Recover an incomplete referent projection

- **WHEN** source contains a postfix dot whose following star or subject is missing
- **THEN** parsing reports the ordinary local syntax failure
- **AND** subsequent declarations remain recoverable
