## ADDED Requirements

### Requirement: Native library discovery is rooted at explicit C exports

Instance discovery SHALL select its root policy from the requested artifact kind. Executable and
WebAssembly discovery SHALL retain the valid root-module `main` policy. Native shared- and
static-library discovery SHALL instead seed reachability from every valid `export "C"` declaration
in canonical module and declaration order, require at least one such root, and SHALL NOT require,
select, or synthesize a `main` entry.

#### Scenario: Discover a library without main

- **WHEN** a native library closure contains two valid C exports and no declaration named `main`
- **THEN** discovery resolves a library entry and includes both exports plus their transitively reachable instances in canonical order

#### Scenario: Ignore unrelated executable main

- **WHEN** a native library closure also contains a valid `main`
- **THEN** `main` enters the library plan only if reachable from an explicit C export and never becomes the library's machine entry

#### Scenario: Reject an empty library surface

- **WHEN** native library discovery finds no valid C export
- **THEN** discovery returns an unavailable entry with the missing-library-export reason and no runtime worklist
