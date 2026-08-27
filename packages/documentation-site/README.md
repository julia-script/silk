# `@silk-lang/documentation-site`

Renders a static HTML documentation site and a static search index from the documentation JSON that
`silk doc` emits.

```console
$ silk doc --output build/documentation.json
$ silk-docs-site --input build/documentation.json --output build/site --title "My library"
Documentation site: build/site (38 modules, 41 files)
```

The result is plain files — an index page, one page per module, a stylesheet, and the search index.
Open `index.html` from disk or serve the directory; both work, and neither needs a build step.

## Why this is a separate package

`silk-documentation-json` requires the emitted value to be readable "without importing compiler
internals or reparsing source comments", and forbids the compiler from emitting HTML at all. That
requirement is only meaningful if some consumer really does render from the JSON alone.

So this package **declares no workspace dependency**. There is no compiler package installed under
it to import, which is a stronger guarantee than a convention about imports: a future import cannot
appear without a visible edit to `package.json`, and a test fails if one does. Its view of the JSON
is a set of locally declared interfaces validated from an `unknown` value.

The compiler does appear as a development dependency, in one place: the tests generate real
standard-library documentation JSON, round-trip it through `JSON.parse`, and render that. A fixture
would age; what the emitter actually writes does not.

## Reading an experimental schema

The JSON schema is chartered as experimental and free to change without migration. A renderer that
threw on an unfamiliar node would turn every schema experiment into a site that does not build, so:

- **The top level is strict.** A file with no schema marker or no module list is reported, because
  that is not an unfamiliar node — it is the wrong file.
- **Everything below it degrades.** An unrecognized block or inline node renders as whatever text it
  can recover, its own value or its children, and the page still builds. A field that is absent
  falls back rather than losing the declaration it belongs to.

## The search index

One file, `search-index.js`, built from declaration names and documentation text. Each entry carries
a declaration's name, module, kind, signature, page location, and a one-line summary.

It is JavaScript rather than JSON because a hosted site is deliberately out of scope: `fetch`
refuses a `file://` URL, and a search that only works behind a server is not a search this site can
promise. The payload is still one JSON literal, so anything wanting the raw data can read it.

Parameters and type parameters are not indexed. They are read on the declaration that owns them, and
in the standard library alone there are 1775 of them against 1351 of everything else — indexing them
would more than double what every page downloads to find worse results.

The matcher the pages run is `Search.query`, serialized into each page with
`Function.prototype.toString`. A hand-written copy in a template string would be a second matcher
that no test covers, which is how a search box silently stops agreeing with what was verified.

## Determinism

Rendering is a pure function from the JSON to a list of files. Nothing touches a filesystem, a
clock, or a random source, so the same input produces byte-identical output across fresh processes —
the same determinism the JSON itself is required to have, carried through to the last step.

## Modules

- `Model` — the renderer's own view of the documentation JSON, and its decoder.
- `Prose` — renders documentation block and inline trees as HTML.
- `Search` — builds the index, ranks a query, and emits the index file and the page script.
- `Site` — renders the index page, the module pages, the stylesheet, and the index.
- `Html` — escaping, slugs, and the rendered-file value.
