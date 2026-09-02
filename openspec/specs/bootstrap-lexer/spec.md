# bootstrap-lexer Specification

## Purpose

Turn exact Silk source bytes into a deterministic, lossless token stream for the first parser
slice while retaining trivia and recoverable lexical errors.

## Requirements

### Requirement: Kernel token vocabulary

The lexer SHALL recognize ASCII whitespace, `//` line comments, `///` documentation comments as a
distinct token kind, the keywords `pub`, `fn`, `return`, `let`, `move`, and the provisional
`import`, ASCII identifiers, decimal integer literals, `(`, `)`, `{`, `}`, `:`, `,`, `=`, `.`,
`-`, `->`, and end-of-file. An identifier SHALL begin with an ASCII letter or underscore and
continue with ASCII letters, digits, or underscores. An integer literal written without a base
prefix SHALL contain one or more ASCII digits and SHALL be read in base ten; the base prefixes are
specified by "Integer literals select a base from their prefix", and the `_` digit separator is
specified by "Number literals accept a digit separator between digits". A `-` immediately followed
by `>` SHALL remain one arrow token; any other `-` SHALL be one minus token.

#### Scenario: Lex the first parser fixture

- **WHEN** the source bytes spell `pub fn main() -> i32 { return 42 }`
- **THEN** the token stream contains the expected keywords, identifiers, punctuation, integer literal, trivia, and end-of-file in source order

#### Scenario: Lex a typed parameter and call argument

- **WHEN** the source bytes spell `pub fn identity(value: i32) -> i32 { return value }` followed by a call `identity(42)`
- **THEN** the colon and all list punctuation are distinct supported tokens with exact source spans

#### Scenario: Preserve a keyword prefix

- **WHEN** the source bytes spell `public function returnValue`
- **THEN** all three words are identifier tokens rather than keyword tokens followed by suffixes

#### Scenario: Lex a documentation comment

- **WHEN** the source bytes spell `/// doc` on its own line before a function
- **THEN** the stream contains one documentation-comment token distinct from the line-comment kind, covering the slashes through the byte before the line ending

#### Scenario: Lex the import keyword

- **WHEN** the source bytes spell `import math` followed by `importer`
- **THEN** the stream contains one import-keyword token, an identifier `math`, and an identifier `importer` rather than a keyword prefix

#### Scenario: Lex a binding statement

- **WHEN** the source bytes spell `let answer = 42` followed by `letter movement`
- **THEN** the stream contains one let-keyword token, an identifier, one equals token, and an integer literal, while `letter` and `movement` remain identifier tokens rather than keyword prefixes

#### Scenario: Distinguish equals from the arrow

- **WHEN** the source bytes spell `= ->` separated by a space
- **THEN** the stream contains one equals token and one arrow token, each with its exact span

#### Scenario: Lex a signed literal and a qualified callee

- **WHEN** the source bytes spell `-42 i32.add`
- **THEN** the stream contains one minus token, one integer literal, one identifier, one dot token, and one identifier, each with its exact span

#### Scenario: Distinguish minus from the arrow

- **WHEN** the source bytes spell `- -> -5`
- **THEN** the stream contains one minus token, one arrow token, and a minus token followed by an integer literal

### Requirement: Lossless token coverage

Every non-end-of-file token SHALL own a non-empty span, token spans SHALL be contiguous and
non-overlapping in source order, and their concatenated source slices SHALL reconstruct every input
byte exactly once. The end-of-file token SHALL own the empty span at the source length.

#### Scenario: Reconstruct trivia-heavy source

- **WHEN** a source contains spaces, tabs, line endings, comments, supported tokens, and invalid bytes
- **THEN** concatenating every non-end-of-file token slice reproduces the original bytes exactly

### Requirement: Trivia remains explicit

The lexer SHALL emit contiguous supported whitespace as whitespace tokens and SHALL emit each `//`
line comment from its opening slashes through the byte before its line ending or through
end-of-file. A comment beginning with exactly `///` SHALL be a documentation-comment token with
the same coverage rule and SHALL NOT carry semantic attachment. Line endings following comments
SHALL remain separate whitespace tokens.

#### Scenario: Lex a line comment

- **WHEN** a line comment is followed by a line ending and another token
- **THEN** the stream contains a comment token, a whitespace token containing the exact line ending, and the following token

#### Scenario: Lex a final line comment

- **WHEN** a line comment reaches end-of-file without a line ending
- **THEN** the comment token covers every remaining byte and is followed by the end-of-file token

#### Scenario: Distinguish documentation from plain comments

- **WHEN** a source contains both `// note` and `/// doc` comments
- **THEN** each is emitted with its own token kind and both remain trivia with exact source spans

### Requirement: Deterministic longest token recognition

The lexer SHALL choose the longest supported token beginning at the current byte, classify a
complete identifier as a keyword only when its full bytes equal a reserved keyword, and preserve
each token's exact source span. Tokenization MUST NOT depend on locale, Unicode normalization,
object identity, or process state.

#### Scenario: Recognize the arrow token

- **WHEN** the next bytes are `->`
- **THEN** the lexer emits one arrow token spanning both bytes

#### Scenario: Repeat lexing

- **WHEN** equivalent source files are lexed repeatedly in fresh processes
- **THEN** their token kinds, spans, source slices, and lexical diagnostics are identical

### Requirement: Invalid bytes remain recoverable data

Bytes that cannot begin any supported token SHALL be emitted as invalid tokens and SHALL produce a
stable lexical diagnostic covering the same span. The lexer SHALL consume at least one byte,
continue at the next supported token boundary, and return the complete token stream and diagnostic
collection rather than throwing or failing an Effect.

#### Scenario: Recover after an invalid byte

- **WHEN** an unsupported byte appears between two identifiers
- **THEN** the lexer emits the first identifier, an invalid token and diagnostic, the second identifier, and end-of-file

#### Scenario: Preserve unsupported non-ASCII bytes

- **WHEN** the source contains a multi-byte UTF-8 sequence outside the kernel vocabulary
- **THEN** every byte remains covered by invalid token data and the lexer continues after the unsupported sequence

### Requirement: Diagnostics use source-owned byte spans

Every lexical diagnostic SHALL be a unified `Diagnostic` value whose originating phase is the
lexer, containing a stable code, severity, concise message, and primary span owned by the lexed
source file. Within the lexical result, diagnostics SHALL be ordered by primary span and stable
code.

#### Scenario: Order multiple lexical errors

- **WHEN** a source contains invalid byte regions at distinct offsets
- **THEN** the returned diagnostics appear in ascending source order with spans that slice to the exact invalid bytes

#### Scenario: Lexical diagnostics carry their phase

- **WHEN** a source produces any lexical diagnostic
- **THEN** the diagnostic is a unified `Diagnostic` value identifying the lexer as its originating phase

### Requirement: Conditional keywords join the vocabulary

The lexer SHALL recognize `if`, `else`, `true`, and `false` as keyword tokens under the same
complete-identifier rule as every other keyword: a longer identifier beginning with a keyword
spelling SHALL remain one identifier token.

#### Scenario: Lex a conditional statement

- **WHEN** the source bytes spell `if flag { return true } else { return false }`
- **THEN** the stream contains if-keyword, identifier, braces, return-keyword, true-keyword, else-keyword, and false-keyword tokens with exact spans

#### Scenario: Preserve conditional keyword prefixes

- **WHEN** the source bytes spell `iffy elsewhere truer falsehood`
- **THEN** all four words are identifier tokens

### Requirement: Expression operators use deterministic longest tokens

The lexer SHALL recognize `+`, `-`, `*`, `/`, `%`, `!`, `<`, `<=`, `>`, `>=`, `==`, `!=`, `&`,
`|`, `^`, `~`, and `|>` as distinct operator tokens with exact source spans. Longest recognition
SHALL prefer `->` over `-`, `<=` over `<`, `>=` over `>`, `==` over `=`, `!=` over `!`, and `|>`
over a bare `|`. `//` SHALL continue to begin a line comment while a single `/` SHALL be the
division token. Operator recognition SHALL preserve the existing lossless coverage and invalid-byte
recovery guarantees.

#### Scenario: Lex every operator spelling

- **WHEN** source contains the complete operator vocabulary separated by trivia
- **THEN** each spelling produces one supported token with its exact span and source slice

#### Scenario: Lex the bitwise operator bytes

- **WHEN** source contains `& | ^ ~ |>`
- **THEN** the four bitwise spellings are punctuation tokens, `|>` still wins over a bare `|`, and no unsupported-byte diagnostic is reported

#### Scenario: Prefer comments over division pairs

- **WHEN** source contains `/ // comment` followed by a line ending
- **THEN** the first slash is a division token and the double slash begins one line-comment token

#### Scenario: Distinguish assignment and equality

- **WHEN** source contains `= == ! != < <= > >= |>`
- **THEN** every single- and double-byte spelling is tokenized independently by longest match

### Requirement: Match tokens use deterministic longest recognition

The lexer SHALL recognize `match` as a complete-identifier keyword and SHALL recognize `&`, `=>`,
and `..` as supported punctuation with exact source spans. Longest recognition SHALL prefer `=>`
over `=`, `..` over `.`, and every existing multi-byte operator over its prefix. The exact spelling
`_` SHALL remain an identifier token and gain universal-pattern meaning only in pattern position.

#### Scenario: Lex a borrowed guarded arm

- **WHEN** source contains `match &value { Token { kind, .. } if guard => kind _ => 0 }`
- **THEN** every keyword, ampersand, brace, dot-dot, guard, fat arrow, identifier, and literal is covered once by the expected token kind

#### Scenario: Preserve keyword and punctuation prefixes

- **WHEN** source contains `matcher = > . .. =>`
- **THEN** `matcher` remains one identifier while the remaining supported and invalid spellings retain deterministic independent coverage

### Requirement: Module documentation comments are distinct trivia

The lexer SHALL classify a line comment beginning with exactly `//!` as a module-documentation
comment token distinct from ordinary `//` comments and declaration `///` documentation comments.
The token SHALL cover the marker through the byte before the line ending or through end-of-file,
and SHALL carry no semantic attachment.

#### Scenario: Distinguish all line comment forms

- **WHEN** source contains `// note`, `/// declaration`, and `//! module` on separate lines
- **THEN** the lexer emits distinct line-comment, declaration-documentation, and module-documentation token kinds with exact spans

### Requirement: const is a complete-identifier keyword

The lexer SHALL classify the exact lowercase spelling `const` as a distinct keyword token under the
same longest complete-identifier rule as every other keyword.

#### Scenario: Lex const without consuming prefixes

- **WHEN** source contains `const constant constable`
- **THEN** only `const` is a const-keyword token and the longer spellings remain identifiers

### Requirement: String literal introductions are closed and extensible

The lexer SHALL recognize unmodified, `b`-modified, and `r`-modified string literal introductions
with either one quote or three quotes as one deterministic literal token. The modifier SHALL be
adjacent to the opening delimiter, delimiter recognition SHALL prefer three quotes over one quote,
and the token kind SHALL retain whether the literal denotes text or bytes without requiring a
distinct kind for every delimiter width. The body-decoding policy SHALL be selected by the modifier
independently of the delimiter width: the unmodified and `b`-modified forms SHALL be escaped, and
the `r`-modified form SHALL be raw. A raw literal SHALL denote text and SHALL carry the text token
kind. An identifier-like spelling immediately adjacent to a quote delimiter that is not in the
closed modifier vocabulary SHALL be reserved as an invalid literal introduction and SHALL produce a
lexical diagnostic rather than tokenize as an identifier followed by a literal. A modifier spelling
SHALL remain an ordinary identifier wherever it is not adjacent to a quote delimiter.

#### Scenario: Recognize every committed literal introduction

- **WHEN** source contains `"text"`, `b"bytes"`, `r"text"`, `"""text"""`, `b"""bytes"""`, and `r"""text"""`
- **THEN** the lexer emits six literal tokens retaining their text-or-byte category, delimiter width, exact source span, and source slice

#### Scenario: Keep a modifier spelling usable as an identifier

- **WHEN** source contains `r` and `b` where no quote delimiter follows either spelling
- **THEN** each spelling remains one ordinary identifier token and produces no lexical diagnostic

#### Scenario: Prefer the multiline delimiter

- **WHEN** the next source bytes begin with `"""`
- **THEN** longest recognition begins one triple-delimited literal rather than a sequence of empty or adjacent single-line literals

#### Scenario: Reserve an unknown modifier

- **WHEN** source contains `future"value"` and `future"""value"""`
- **THEN** each adjacent `future` spelling is retained with its literal as one invalid introduction and produces a stable lexical diagnostic naming the unknown modifier

### Requirement: A character literal holds exactly one Unicode scalar

The lexer SHALL recognize `'` as the delimiter of a character literal, which is a fourth literal
introduction outside the closed quote-delimiter vocabulary and takes no modifier. The literal SHALL
carry one distinct token kind. Its body SHALL admit the escape vocabulary of an escaped quote
literal and SHALL additionally admit `\'` for its own delimiter; one escape SHALL keep one meaning
in every literal form that admits it. The body SHALL denote exactly one Unicode scalar value, and
that rule SHALL count scalars rather than bytes, so a multi-byte scalar is one character. A body
denoting no scalar and a body denoting more than one SHALL each produce exactly one lexical
diagnostic. An unterminated character literal SHALL stop immediately before a physical CR or LF and
SHALL produce exactly one lexical diagnostic, exactly as a single-line quote literal does. An
identifier-like spelling adjacent to `'` SHALL NOT be reserved as an unknown modifier, because the
character form declares no modifier to misspell.

#### Scenario: Recognize a character literal and its escapes

- **WHEN** source contains `'a'`, `' '`, `'\t'`, `'\u{2603}'`, `'\''`, and `'é'`
- **THEN** the lexer emits six character-literal tokens with exact source spans and produces no lexical diagnostic

#### Scenario: Reject a body that is not one scalar

- **WHEN** source contains `''` and `'ab'`
- **THEN** each is retained as one invalid literal token over its exact span and produces exactly one lexical diagnostic naming the scalar count

#### Scenario: Recover an unterminated character literal

- **WHEN** an opening `'` is followed by content and a line ending without a closing `'`
- **THEN** the token stops immediately before the line ending, one lexical diagnostic is produced, and the following line lexes normally

#### Scenario: Keep the delimiter outside the modifier vocabulary

- **WHEN** source contains `b'a'`
- **THEN** `b` remains one ordinary identifier token, `'a'` is one character-literal token, and no unknown-modifier diagnostic is produced

### Requirement: String literal boundaries recover deterministically

An escaped single-line literal SHALL close at the first unescaped quote and SHALL otherwise stop
immediately before a physical CR or LF. An escaped multiline literal SHALL close at the first
unescaped run of three quotes and MAY contain physical line endings; if no closing delimiter
exists, it SHALL consume through end-of-file. A raw literal SHALL treat a backslash as ordinary
content and SHALL therefore close at the first run of its delimiter, under the same single-line and
multiline rules. Each unterminated literal SHALL produce exactly one lexical diagnostic anchored to
its introduction. Recovery MUST NOT infer a closing boundary from indentation, keywords,
declarations, braces, comments, or other code-like content inside a multiline literal.

#### Scenario: Recover an unterminated single-line literal

- **WHEN** an opening single quote is followed by content and a line ending without a closing quote
- **THEN** the literal token stops before the line ending, one lexical diagnostic identifies the unterminated literal, and lexing resumes with the line ending and following source

#### Scenario: Consume an unterminated multiline literal

- **WHEN** a triple-delimited literal has no unescaped closing delimiter before end-of-file
- **THEN** one literal token covers its introduction through end-of-file and exactly one lexical diagnostic is produced without interpreting apparent declarations inside its body

#### Scenario: Ignore escaped quotes when finding the boundary

- **WHEN** a multiline literal body contains `\"\"\"` followed later by an unescaped `"""`
- **THEN** the escaped quotes remain literal content and the later unescaped triple quote closes the token

#### Scenario: Close a raw literal at its first delimiter run

- **WHEN** source contains `r"path\"` followed by further source on the same line
- **THEN** the raw literal closes at the quote after the backslash rather than continuing past it, and the following source lexes independently

### Requirement: Integer literals select a base from their prefix

An integer literal SHALL accept the base prefixes `0x` and `0X` for base sixteen, `0b` and `0B` for
base two, and `0o` and `0O` for base eight, each followed by one or more digits of its selected
base. A prefixed literal SHALL be one integer literal token of the same kind as an unprefixed
literal, SHALL retain its exact magnitude, and SHALL NOT accept a fraction part or an exponent
part. A leading `0` that no base letter follows SHALL keep the decimal reading, so `0` alone and
`0.5` retain their existing token kinds. A base prefix that no digit of its base follows SHALL be
one invalid token producing exactly one lexical diagnostic. Every conversion of an integer literal
to a value SHALL read the base from the token's own source slice rather than assume base ten.

#### Scenario: Lex every base prefix

- **WHEN** the source bytes spell `0xff 0b1010 0o777`
- **THEN** the stream contains exactly three integer literal tokens whose slices are `0xff`, `0b1010`, and `0o777`

#### Scenario: Preserve the unprefixed readings

- **WHEN** the source bytes spell `0 0.5`
- **THEN** the stream contains one integer literal token `0` and one floating-point literal token `0.5`

#### Scenario: Reject a prefix without digits

- **WHEN** the source bytes spell `0x` with no following hexadecimal digit
- **THEN** the lexer emits one invalid token covering the prefix and exactly one lexical diagnostic

#### Scenario: Keep a prefixed literal free of float parts

- **WHEN** the source bytes spell `0xff.5`
- **THEN** the integer literal token ends after `0xff` and the remaining bytes are tokenized independently

#### Scenario: Convert a prefixed literal in its own base

- **WHEN** a compiled program returns `0xff` where another returns `255`
- **THEN** both programs produce the same value, and a prefixed literal outside its selected type's range produces the existing out-of-range diagnostic

### Requirement: Number literals accept a digit separator between digits

An integer or floating-point literal SHALL accept the byte `_` as a digit separator, and the
separator SHALL be well placed only with a digit of the same digit run immediately on each side.
A separator therefore SHALL NOT open or close a literal, SHALL NOT follow another separator, SHALL
NOT sit immediately after a base prefix, and SHALL NOT sit immediately before or after the decimal
point or the exponent letter. The lexer SHALL consume the separators of a literal itself, keeping
one token rather than a literal followed by an identifier, and SHALL NOT require any group size.
A literal whose separators are not all well placed SHALL be one invalid token covering the literal
and SHALL produce exactly one lexical diagnostic over that same span, without introducing a new
token kind. Every conversion of a literal to a value SHALL read it without its separators, so a
separated literal SHALL retain the exact value of its separator-free spelling. The `_` byte SHALL
keep its identifier meaning everywhere else, so a spelling beginning with `_` remains one
identifier.

#### Scenario: Separate the digits of an integer literal

- **WHEN** the source bytes spell `1_000 1_048_576 0b1010_0000 0xff_ff`
- **THEN** the stream contains exactly four integer literal tokens whose slices retain their separators, with no lexical diagnostic

#### Scenario: Separate the digits of a float literal

- **WHEN** the source bytes spell `1_000.5 1.000_5 1e1_0`
- **THEN** the stream contains exactly three floating-point literal tokens, with no lexical diagnostic

#### Scenario: Preserve the value of a separated literal

- **WHEN** a compiled program compares `1_000` with `1000` and `1.000_5` with `1.0005`
- **THEN** each pair is equal on every execution engine

#### Scenario: Reject a misplaced separator

- **WHEN** the source bytes spell any of `1_`, `1__0`, `0x_ff`, `1_.5`, `1._5`, `1_e5`, or `1e_5`
- **THEN** the lexer emits one invalid token covering the literal and exactly one lexical diagnostic

#### Scenario: Keep a leading underscore an identifier

- **WHEN** the source bytes spell `_1` followed by `x_1`
- **THEN** both spellings remain single identifier tokens

### Requirement: Enum is a complete-identifier keyword

The lexer SHALL emit `enum` as the dedicated enum keyword only when it is a complete identifier and
SHALL retain exact source provenance under the existing trivia and recovery model.

#### Scenario: Distinguish enum from an identifier prefix

- **WHEN** source contains `enum` and `enumerate`
- **THEN** the first token is the enum keyword and the second remains one identifier token

### Requirement: Union is a complete-identifier keyword

The lexer SHALL emit `union` as the dedicated nominal-union keyword only when it is a complete
identifier and SHALL retain exact source provenance under the existing trivia and recovery model.

#### Scenario: Distinguish union from an identifier prefix

- **WHEN** source contains `union` and `unionize`
- **THEN** the first token is the union keyword and the second remains one identifier token

### Requirement: Duration-looking numeric text commits to recoverable literal recognition

When numeric source text is immediately followed by ASCII letters, the lexer SHALL commit the
complete contiguous duration-looking extent to duration-literal recognition. A valid extent SHALL
produce one duration-literal token. An extent with an unknown unit, non-whole or non-decimal
component, invalid digit separator, reordered or repeated unit, or out-of-range subordinate field
SHALL remain one invalid-duration token and SHALL produce one focused lexical diagnostic for the
first determinable violation. The lexer SHALL resume at the first byte that cannot continue that
extent, preserving lossless source coverage. Existing numeric spellings with no trailing duration
unit, including floating exponents such as `1e5`, SHALL retain their existing tokenization.

#### Scenario: Recognize a complete compound token

- **WHEN** source bytes spell `waitFor(1h30m30s)`
- **THEN** `1h30m30s` is one duration-literal token with its exact source span

#### Scenario: Commit an unknown unit

- **WHEN** source bytes spell `3sec`, `1H`, or `1h30x`
- **THEN** each complete duration-looking spelling is one invalid-duration token with one diagnostic naming its first unknown unit

#### Scenario: Commit an invalid numeric component

- **WHEN** source bytes spell `1.5s` or `0x10s`
- **THEN** each complete spelling is one invalid-duration token with one diagnostic stating that duration components must be whole decimal amounts

#### Scenario: Commit an invalid canonical compound

- **WHEN** source bytes spell `1h60m`, `30s1m`, or `1h2h`
- **THEN** each complete spelling is one invalid-duration token with one diagnostic identifying the first bound, ordering, or repetition violation

#### Scenario: Stop at an expression boundary

- **WHEN** source bytes spell `1h+30m`, `1h + 30m`, or `1h.member`
- **THEN** each duration token ends before the operator, trivia, or projection punctuation and following bytes tokenize independently

#### Scenario: Preserve an ordinary exponent literal

- **WHEN** source bytes spell `1e5` without a trailing duration unit
- **THEN** the source remains one floating-point literal rather than becoming an invalid duration

#### Scenario: Recover after an invalid duration

- **WHEN** an invalid duration-looking token is followed by punctuation and another valid expression
- **THEN** the invalid token and its diagnostic preserve their exact span and lexing continues with the following punctuation and expression

### Requirement: Type is a complete-identifier keyword

The lexer SHALL emit `type` as the dedicated type-alias keyword only when it is a complete
identifier and SHALL retain exact source provenance under the existing trivia and recovery model.

#### Scenario: Distinguish type from an identifier prefix

- **WHEN** source contains `type` and `typeName`
- **THEN** the first token is the type keyword and the second remains one identifier token
