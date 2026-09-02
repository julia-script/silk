## Context

Project loading currently materializes one package root and optional build defaults. Project
commands share manifest discovery and stable exit classes. Evaluation already accepts a scripted
HostInput adapter whose arguments are ordered byte arrays, while ordinary Silk constructs
OsHostInput and Allocator providers. The prerequisite changes add a standard runner root and
test-aware compilation request.

## Goals / Non-Goals

**Goals:**

- Make user and standard-library test roots explicit, bounded, and reproducible.
- Invoke either the standard or a custom zero-parameter runner through one evaluator path.
- Forward filter bytes through the existing host-input seam without compiler interpretation.

**Non-Goals:**

- Build native or Wasm test artifacts, select multiple targets, or isolate traps.
- Add CLI-owned filter syntax or scan a directory for marked declarations.

## Decisions

### Extend Project with one materialized TestConfiguration

Manifest decoding accepts `[test]` only as a table. `roots` is required and nonempty when the table
exists; each value is validated as a manifest-relative exact `.silk` path. `runner` is optional and
validated the same way. SourceEntry materialization resolves every path against the manifest
directory, then rejects any path outside the already selected package source root before analysis
and derives module identity relative to that source root. Repeated identical roots are retained in
Project facts in declaration order; compilation's existing canonical module de-duplication prevents
duplicate loading or inventory entries. With no table, the ordinary package
entry is reused as the sole test root. With no runner, the toolchain supplies the canonical standard
runner entry from shipped `silk/test_runner`; a user source of the same relative spelling cannot
shadow it.

`silk test --standard-library` bypasses project discovery and user manifest roots, rejects
`--manifest-path`, and reads an explicit generated catalog versioned with shipped sources. The
catalog is regenerated with the standard-library source table, contains canonical shipped-source
identities only, and selects the same canonical shipped runner. Its integrity is gated jointly with
the generated source table.

Alternatives rejected: optional roots inside a present table creates ambiguous partial defaults;
directory globbing makes membership filesystem-dependent; placing runner inside roots contaminates
inventory by default.

### Add a dedicated evaluator TestWorkflow actor

The CLI command parser selects exactly one of user-project mode or `--standard-library`, collects
the admitted manifest selector and positional filter arguments, then delegates to one TestWorkflow
boundary actor. It loads TestConfiguration or the toolchain catalog, constructs the
test-compilation request, completes recoverable analysis, rejects unavailable inventory or runner
entry, and evaluates the runner once. It does not call a backend or create an artifact.

The workflow passes the ordinary host target used by evaluator project analysis. Manifest build
backend, target, and profile values are ignored. No `--engine`, `--backend`, `--target`, `--profile`,
`--release`, or `--watch` option is accepted for this initial command. `--` ends option parsing, so
every subsequent argument, including bytes spelling one of those options, is a positional filter.

Alternatives rejected: routing through build/run would create a native-only first contract; adding
an engine flag would promise unreviewed parity and trap behavior.

### Use the byte-capable HostInput script as the only runner argument seam

The workflow constructs the existing evaluator HostInput script with the command's admitted
program-name bytes at index zero and filter byte arrays afterward. Those index-zero bytes remain
the existing platform-derived HostInput program name: they are forwarded unchanged but receive no
new cross-platform spelling guarantee. The platform argument boundary must retain the bytes made
available for each positional filter; once admitted to TestWorkflow they are never decoded or
normalized. Tests inject arbitrary byte arrays directly at this seam, including invalid UTF-8, so
the standard runner's byte contract is independently verifiable. Ordinary runner source constructs
OsHostInput and Allocator and provides them lexically; there is no ambient test-only service.

Before implementing the parser-to-HostInput bridge, an evidence gate enumerates supported
OS/runtime boundaries and the byte domain each can admit, invokes the real executable with
representative non-NUL byte sequences, and compares the post-parser bytes. If any admitted byte is
normalized, implementation stops and returns SLP-0004 to Candidate as required by the accepted
direction; invalid bytes a platform cannot admit are tested only at the byte-capable HostInput seam.

Alternatives rejected: a parameter-bearing main changes entry semantics; compiler-owned filters
prevent custom policy; String conversion weakens the accepted raw-byte matcher.

### Connect evaluator output to command standard output

TestWorkflow constructs a scoped evaluator StandardStreams provider that forwards every runner
standard-output write to the command's standard output in original byte order exactly once. It does
not buffer and replay an already emitted transcript. A host write failure is returned through the
ordinary stream provider, allowing StandardReporter to translate it to ReportError and the standard
runner to return 2. Provider resources close on every TestWorkflow exit without changing the
primary source, operational, entry, or evaluator termination.

### Preserve pre-execution and entry status ownership

Project/manifest/catalog/storage failures, including absent or unreadable configured root files,
remain operational status 2. An absent transitive import and compiler diagnostics, including test
eligibility or runner-entry shape, remain source-rejection status 1 and prevent execution; a mixed
source and operational failure returns 2 under existing command precedence. After successful
compilation, the standard runner's 0/1/2 result is returned unchanged. A custom runner retains the
canonical entry contract: unit or successful effect completion returns 0, an unhandled typed entry
failure returns 1, and ordinary `main() -> i32` returns its exact value. Traps and every other
non-entry-completion evaluator classification retain their existing reporting and process behavior
rather than entering the standard-runner status union.

## Risks / Trade-offs

- **Platform CLI parsing cannot expose a raw byte sequence on one host** → keep the byte contract at
  the existing HostInput seam, characterize each supported host boundary, and return to SLP-0004 if
  a required positional filter would be normalized rather than silently weakening it.
- **Custom runner is accidentally inventoried** → retain separate root roles through project facts
  and add a runner-only marked declaration fixture.
- **Standard-library catalog drifts from embedded sources** → generate both from one ordered manifest
  and gate their digests together.
- **Runner status collides with compiler status 1** → retain phase ownership in structured workflow
  results and assert that invalid compilations never execute.
- **Test configuration leaks into ordinary commands** → keep TestConfiguration lazy and verify
  build, check, and run continue to use only the package root and their existing argument rules.
