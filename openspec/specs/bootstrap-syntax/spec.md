# Bootstrap Syntax Specification

## Purpose

Turn the bootstrap lexer result into the smallest source-faithful grammatical structure that can
recover from ordinary mistakes without introducing semantic or lowering representations.
## Requirements
### Requirement: First function grammar
The parser SHALL recognize a source file containing one or more top-level import or function
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

#### Scenario: Parse the accepted integer fixture
- **WHEN** the source bytes spell `pub fn main() -> I32 { return 42 }`
- **THEN** the result contains one complete function declaration with an empty parameter list, an integer literal return expression, and end-of-file

#### Scenario: Parse a private function

- **WHEN** the source bytes spell `fn helper() -> I32 { return 42 }`
- **THEN** the result contains one complete function declaration with no public-modifier token and exact source provenance

#### Scenario: Parse trivia between grammar elements
- **WHEN** whitespace and line comments appear between every pair of grammar elements in the accepted fixture
- **THEN** the parser recognizes the same grammatical structure while retaining the exact trivia tokens

#### Scenario: Parse two functions in source order
- **WHEN** `answer` returning `42` is followed by `main` returning `0`
- **THEN** the source-file tree contains exactly two complete function declarations in that order before end-of-file

#### Scenario: Parse a typed parameter reference
- **WHEN** the source spells `pub fn identity(value: I32) -> I32 { return value }`
- **THEN** the function contains one typed parameter and a bare-identifier return expression with exact concrete provenance

#### Scenario: Parse the first value-carrying call
- **WHEN** `identity` accepts one `I32` parameter and `main` returns `identity(42)`
- **THEN** `main` contains one complete call expression with one decimal-integer argument

#### Scenario: Parse trivia inside a call
- **WHEN** whitespace and line comments appear around a call's callee, parentheses, arguments, and commas
- **THEN** the parser recognizes the same call structure while retaining every trivia token exactly

#### Scenario: Parse a binding sequence

- **WHEN** the source spells `pub fn main() -> I32 { let value = 42 return value }`
- **THEN** the block contains one binding statement retaining its keyword, name, equals, and initializer expression, followed by one return statement, in source order

#### Scenario: Parse a move operand

- **WHEN** a binding initializer or call argument spells `move value`
- **THEN** the expression is a move operand retaining the keyword and the moved name with exact spans

#### Scenario: Parse a signed literal

- **WHEN** a return expression or call argument spells `-42`
- **THEN** the integer literal expression retains the minus token and the decimal token as one concrete branch with exact spans

#### Scenario: Parse a qualified callee

- **WHEN** a body spells `I32.add(1, 2)`
- **THEN** the call expression retains the actor identifier, the dot, the operation identifier, and the argument list in concrete order

### Requirement: First call syntax remains explicit and recoverable
A call expression SHALL retain its callee identifier, left parenthesis, ordered arguments,
separators, right parenthesis, trivia, and exact source-owned span in concrete order. Missing call
elements SHALL become explicit missing tokens with parser diagnostics. Unexpected tokens inside an
argument position SHALL remain in an error region without being interpreted as an argument, and
recovery SHALL resume at the next comma, right parenthesis, enclosing brace, following declaration,
or end-of-file.

#### Scenario: Recover a missing right call parenthesis
- **WHEN** a returned call spells `answer(` immediately before the function's closing brace
- **THEN** the call contains a missing right parenthesis, the block retains its closing brace, and parsing completes

#### Scenario: Recover a missing call callee
- **WHEN** the returned expression consists only of `()`
- **THEN** the call contains a missing identifier and retains both parentheses without inventing a callee name

#### Scenario: Preserve a supported call argument
- **WHEN** the returned call spells `identity(42)`
- **THEN** `42` is retained as the call's first decimal-integer argument rather than an error region

#### Scenario: Recover between call arguments
- **WHEN** unsupported punctuation appears between two otherwise valid arguments
- **THEN** the punctuation remains in an error region and the following comma-bounded argument remains parseable

#### Scenario: Keep integer expressions unchanged
- **WHEN** a function returns a decimal integer
- **THEN** its existing integer-literal concrete shape and recovery behavior remain unchanged

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
and parsing SHALL either consume a concrete token or insert missing syntax without looping.

#### Scenario: Preserve a second function after a missing brace
- **WHEN** the first function omits its closing brace immediately before a valid second function
- **THEN** the first function receives a missing right brace and the second function remains a separate complete declaration

#### Scenario: Recover unexpected input between functions
- **WHEN** unsupported punctuation appears after one complete function and before the next `pub`
- **THEN** the punctuation is retained in an error region with a parser diagnostic and the following function remains parseable

#### Scenario: Preserve empty-input recovery
- **WHEN** the source is empty
- **THEN** the parser still returns one recovered missing function structure and end-of-file rather than treating an empty file as a valid program

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
- **WHEN** a function declares `(left: I32, right: I32)`
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
The parser SHALL emit a stable diagnostic for the missing element without consuming an unrelated
concrete token.

#### Scenario: Recover a missing function name
- **WHEN** the source spells `pub fn () -> I32 { return 42 }`
- **THEN** the function contains a missing identifier before `(`, the remaining structure parses, and one parser diagnostic identifies the missing name position

#### Scenario: Recover a missing closing brace
- **WHEN** the accepted fixture ends after the decimal integer
- **THEN** the block contains a missing right brace at end-of-file and the parser returns the partial tree with one parser diagnostic

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
missing return statement becomes an explicit recovered return structure with parser diagnostics.

#### Scenario: Recover a missing initializer

- **WHEN** a block spells `let value = return 42`
- **THEN** the binding retains a missing initializer expression at the `return` boundary with one parser diagnostic, and the return statement parses completely

#### Scenario: Recover a missing binding name

- **WHEN** a block spells `let = 42 return 0`
- **THEN** the binding contains a missing identifier before the equals token and both statements remain separate concrete branches

#### Scenario: Recover a missing return statement

- **WHEN** a block contains only binding statements before its closing brace
- **THEN** the block retains every binding followed by a recovered return structure whose missing elements carry parser diagnostics

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

- **WHEN** a body spells `I32.(1, 2)`
- **THEN** the callee retains the actor identifier and dot with an explicit missing identifier and one parser diagnostic, and both arguments remain parseable

#### Scenario: Recover a dangling minus

- **WHEN** a return expression spells `-` immediately before the closing brace
- **THEN** the minus token stays in the tree with an explicit missing decimal literal and one parser diagnostic

### Requirement: Conditional statements parse losslessly

The parser SHALL accept `if <expression> { <statements> } else { <statements> }` wherever a
statement may appear, with the `else` arm optional and no parentheses around the condition, per
the accepted surface. Each arm SHALL be a brace-delimited statement sequence (bindings,
conditionals, and return statements), and the function body SHALL still end in exactly one
trailing return statement after any conditionals. `true` and `false` SHALL parse as boolean
literal expressions wherever an expression is allowed. Missing conditions, braces, and damaged
arms SHALL remain explicit recovery data bounded by the existing statement and declaration
anchors, and every token SHALL be retained losslessly.

#### Scenario: Parse a conditional with both arms

- **WHEN** a body spells `if flag { return 1 } else { return 2 } return 0`
- **THEN** the block contains one conditional statement retaining the keyword, condition expression, both brace-delimited arms, and the else keyword, followed by the trailing return statement

#### Scenario: Parse a conditional without an else arm

- **WHEN** a body spells `if flag { return 1 } return 0`
- **THEN** the conditional retains one arm and no else branch, and the trailing return remains a sibling statement

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

Every expression position SHALL accept grouped, prefix, infix, and pipeline expressions in the
closed precedence and associativity order defined by `bootstrap-operator-semantics`. The concrete
tree SHALL retain a grouped expression's parentheses, a prefix expression's operator and operand,
an infix expression's left operand, operator, and right operand, and a pipeline expression's left
operand, pipe token, qualified target path, and optional later-argument list. Every token and trivia
slice SHALL remain owned exactly once, and concrete structure SHALL not claim that an operator or
pipeline target resolves successfully.

#### Scenario: Parse a precedence ladder

- **WHEN** a body returns `1 + 2 * 3 == 7`
- **THEN** the concrete tree nests multiplication inside addition and addition inside equality with every token retained

#### Scenario: Parse grouped prefix syntax

- **WHEN** a body returns `-(value + 1)` and another returns `!(left == right)`
- **THEN** each prefix expression owns one grouped operand with exact parentheses and source spans

#### Scenario: Parse a no-argument pipeline target

- **WHEN** a body returns `flag |> Bool.not`
- **THEN** the pipeline retains the qualified `Bool.not` target with no explicit argument list

#### Scenario: Parse a pipeline chain

- **WHEN** a body returns `2 |> I32.add(3) |> I32.multiply(4)`
- **THEN** the concrete tree associates the two pipeline branches left-to-right in source order

### Requirement: Operator recovery stays inside the containing expression

A missing prefix operand, infix right operand, grouping parenthesis, pipeline qualifier, pipeline
member, or pipeline argument delimiter SHALL become explicit parser recovery data at the nearest
expression boundary. Unexpected operator sequences SHALL remain in error regions. Recovery SHALL
resume at the next operand, comma, closing parenthesis, statement keyword, closing brace, top-level
declaration, or end-of-file, preserving following statements and declarations.

#### Scenario: Recover a missing infix operand

- **WHEN** a return expression spells `1 +` immediately before its block's closing brace
- **THEN** the infix expression contains an explicit missing operand and the block retains its closing brace

#### Scenario: Recover a missing grouped parenthesis

- **WHEN** a return expression spells `(1 + 2` before the block's closing brace
- **THEN** the group contains a missing right parenthesis and the block boundary remains separate

#### Scenario: Recover a damaged pipeline target

- **WHEN** a body spells `value |> .apply(1)` before a following declaration
- **THEN** the missing qualifier and diagnostic remain in the pipeline branch while the following declaration parses independently

#### Scenario: Reject ungrouped comparison chaining

- **WHEN** a body spells `1 < 2 < 3`
- **THEN** the second comparison is retained as recovered unexpected syntax with a parser diagnostic

### Requirement: Struct declarations parse losslessly

Every top-level declaration position SHALL accept `pub` optionally followed by `struct`, a name,
and a braced field list. Each field SHALL retain an optional `pub`, a name, a colon, and an explicit
local, selected, or namespace-qualified type path. Fields SHALL be recognized sequentially without
requiring punctuation between complete field declarations. The concrete tree SHALL own every token
and trivia slice exactly once and SHALL NOT claim that names, types, or visibility are semantically
valid.

#### Scenario: Parse a public struct

- **WHEN** source declares `pub struct Token { pub kind: I32 lexeme: Text }`
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

- **WHEN** a struct contains `pub : I32` before its closing brace
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

- **WHEN** a field declares `cells: [[I32; 4]; 3]`
- **THEN** the concrete tree retains both nested constructors and every punctuation token byte-for-byte

#### Scenario: Recover a missing array length

- **WHEN** a declaration contains `[I32; ]`
- **THEN** the tree records missing length syntax without consuming the following declaration

#### Scenario: Reject the former array spelling

- **WHEN** an explicit type position contains `Array<I32, 4>`
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
where a place is a binding followed by zero or more field or index projections. The concrete tree
SHALL retain the `mut` token, assignment token, complete place syntax, expression, trivia, and exact
spans. Assignment SHALL remain distinct from equality and SHALL NOT be an expression.

#### Scenario: Parse an indexed field update

- **WHEN** source contains `pairs[index].left = value`
- **THEN** the tree retains the binding root, index projection, field projection, assignment token, and right-hand expression in source order

#### Scenario: Distinguish assignment from equality

- **WHEN** a loop body contains both `current = next` and `current == next`
- **THEN** the first is an assignment statement and the second remains an equality expression

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

### Requirement: Never type syntax is explicit

The parser SHALL recognize `Never` as a type reference wherever a declared type is accepted and
SHALL retain its exact token and span. `Never` SHALL have no literal or constructor syntax.

#### Scenario: Parse Never in a return type

- **WHEN** a function declares `Never` as its return type
- **THEN** the concrete tree retains one complete declared-type node for the built-in type

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
- **WHEN** source contains `pub fn identity<T>(value: T) -> T` and `identity<I32>(1)`
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

- **WHEN** source spells `fn fold(values: &[I32]) -> I32 { return use(&values) }`
- **THEN** the tree retains one shared slice type and one shared borrow expression with their punctuation and provenance in source order

#### Scenario: Parse an exclusive slice parameter and borrow argument

- **WHEN** source spells `fn edit(values: &mut [I32]) -> I32 { return use(&mut values) }`
- **THEN** the tree retains both `mut` keywords under distinct exclusive slice-type and borrow-expression branches

#### Scenario: Recover a damaged slice type

- **WHEN** a parameter starts a slice type but omits its element or closing bracket before the parameter boundary
- **THEN** the parser inserts explicit missing syntax, preserves following parameters and the function body, and emits deterministic parser diagnostics
