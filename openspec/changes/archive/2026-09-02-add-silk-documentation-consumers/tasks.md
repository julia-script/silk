## 1. Doctest workflow

- [x] 1.1 Add a package that collects fenced Silk examples from a documentation JSON value, walking
      module documentation and every declaration and nested child in source order.
- [x] 1.2 Parse fence attributes as comma-delimited words in the language token, honor `ignore` as
      the opt-out, and fail an example carrying an attribute the workflow does not define.
- [x] 1.3 Compile each collected example as one complete module, exactly as written.
- [x] 1.4 Convert the JSON's byte offset into a one-based line against caller-supplied source bytes,
      and report the position as unavailable rather than guessed when those bytes are absent.
- [x] 1.5 Report each failure with its source identity, line, declaring module, declaring
      declaration, and diagnostics, and name the comma-delimited opt-out form in that report.
- [x] 1.6 Summarize collected, passed, skipped, and failed counts, and exit non-zero on any failure.
- [x] 1.7 Add a command-line entry point that resolves standard-library sources through the shipped
      manifest and project sources through a source root.

## 2. Site renderer and search index

- [x] 2.1 Add a package with no workspace runtime dependency that declares its own view of the
      documentation JSON and validates it from an `unknown` value.
- [x] 2.2 Render one page per module and one index page, deterministically and in the JSON's order,
      escaping every value taken from the JSON.
- [x] 2.3 Render an unrecognized block or inline node as its recoverable text and keep going.
- [x] 2.4 Build one static search index from declaration names and documentation text, and load it
      from the rendered pages so a declaration is found by name with no server.
- [x] 2.5 Add a command-line entry point that reads a JSON file and writes the site to a directory.

## 3. Standard library

- [x] 3.1 Mark the `silk/filesystem` composition example with the comma-delimited opt-out, since it
      is an illustration of a shape rather than a compilable module.
- [x] 3.2 Give `silk/option.unwrapOr` a compilable example, so the standard-library gate compiles
      something rather than skipping everything it is given.
- [x] 3.3 Regenerate the compiler-shipped source table and the standard-library documentation page
      from those sources.

## 4. Acceptance

- [x] 4.1 Assert a wrong example fails the doctest workflow and that the report carries the file and
      the one-based line of the fence.
- [x] 4.2 Assert an example fenced with the comma-delimited opt-out is reported as skipped and never
      compiled, and that an unknown attribute fails instead.
- [x] 4.3 Assert the workflow compiles every fenced Silk example in the standard library without a
      failure, driven by the shipped manifest rather than a fixed module list.
- [x] 4.4 Assert the renderer package declares no workspace runtime dependency and that no renderer
      source file imports one.
- [x] 4.5 Assert the renderer writes a page for every module of real standard-library documentation
      JSON round-tripped through `JSON.parse`, driven by the shipped manifest.
- [x] 4.6 Assert rendering is deterministic and that documentation text containing markup is escaped.
- [x] 4.7 Assert an unrecognized node renders as text rather than failing the run.
- [x] 4.8 Assert the search index carries an entry for a declaration and that the site's own search
      finds that declaration by name.
