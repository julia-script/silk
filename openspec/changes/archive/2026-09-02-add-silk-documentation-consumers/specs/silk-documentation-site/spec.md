## Purpose

Define the documentation site rendered from the formatter-neutral documentation JSON: what it may
read, what it writes, and what it owes a schema that is free to change under it.

## ADDED Requirements

### Requirement: The renderer reads the documentation JSON and nothing else

The site renderer SHALL take a documentation JSON value as its only input and SHALL declare no
runtime dependency on the compiler, on the documentation model package, or on any other workspace
package. It MUST NOT import a compiler internal type, and MUST NOT reach past the JSON into source
files, a compiler snapshot, or a build directory.

The renderer's view of the JSON SHALL be its own declared shape, validated from an `unknown` value,
so that reading a file the emitter did not write is a reported failure rather than a crash partway
through a page.

#### Scenario: Render from a parsed file

- **WHEN** the renderer is given a value parsed from documentation JSON written by generation
- **THEN** it produces the site without consulting any source file, compiler package, or build artifact

#### Scenario: Reject a value that is not documentation JSON

- **WHEN** the renderer is given a parsed value that does not carry the documentation schema marker and a module list
- **THEN** it reports the mismatch and writes nothing

### Requirement: The site is one page per module plus an index

The renderer SHALL write one HTML page for every module in the JSON and one index page listing those
modules. A module page SHALL carry the module's documentation, and for each declaration its name,
its kind, its signature, its visibility, its documentation, and its documented children.

Rendering SHALL be deterministic: the same JSON SHALL produce byte-identical pages across fresh
processes, in the JSON's own module and declaration order.

Every value taken from the JSON SHALL be escaped for the context it is written into, so a
documentation comment containing markup renders as text rather than as structure.

#### Scenario: Render every module of a library

- **WHEN** the renderer is given documentation JSON for the standard library
- **THEN** it writes one page per module in the JSON and an index page that links to each of them

#### Scenario: Escape documentation text

- **WHEN** a declaration's documentation contains HTML-significant characters
- **THEN** the rendered page shows those characters as text and does not emit them as markup

### Requirement: An unrecognized node degrades instead of failing

Because the documentation JSON schema is experimental and may change without migration, the renderer
SHALL render a block or inline node whose tag it does not recognize as that node's recoverable text,
or as nothing when it carries none, and SHALL continue rendering the rest of the page.

#### Scenario: Render a document containing an unknown node

- **WHEN** a documentation block carries a tag the renderer does not know
- **THEN** the page still renders every node the renderer does know and does not fail the run

### Requirement: The site ships one static search index built from the JSON

The renderer SHALL write one static index file built from the declaration names and the documentation
text in the JSON. Each entry SHALL carry the declaration's name, its module, its kind, its signature,
the page location that shows it, and a short text summary drawn from its documentation.

The rendered site SHALL load that one file and SHALL find a declaration by its name without a server,
a build step, or a network service. The index SHALL be deterministic in the same sense the pages are.

#### Scenario: Find a declaration by name

- **WHEN** the site is loaded and a declaration's name is entered into its search field
- **THEN** the index yields that declaration with the module and the page location that shows it

#### Scenario: Build the index from documentation text

- **WHEN** a declaration carries documentation prose
- **THEN** its index entry carries a summary drawn from that prose, and a search over the index can match it
