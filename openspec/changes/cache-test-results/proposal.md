# Proposal

## Why

`silk test` recompiles and executes every selected test on every invocation even when a previously
passing test and every declared execution input are unchanged. The local authored fingerprint
cannot safely authorize reuse because it excludes helper bodies, compile-time dependencies,
providers, compilation configuration, the runner/compiler, and native inputs.

## What Changes

- Add a separate versioned per-test execution identity derived from the test's authored identity,
  complete transitive runtime and compile-time dependency closure, selected providers, published
  compilation profile, runner/compiler identity, and resolved external native inputs.
- Preserve the existing authored fingerprint as local metadata; never use semantic-query reuse,
  whole-file identity, or whole-suite/artifact identity as a substitute for per-test completeness.
- Persist strict pass-only result records through the existing `Storage` capability in a dedicated
  project result namespace, treating missing, stale, invalid, corrupt, and expected storage
  failures as execution rather than proof of success.
- Add a versioned runner exchange isolated from user stdout/stderr so the CLI can provide admitted
  cache hits and receive authoritative executed-pass outcomes without parsing user output.
- Make result caching default for `silk test`, report cached versus executed counts, and add
  `--no-cache` to bypass result-cache reads and writes while leaving compilation caches enabled.
- Replace the first runner's requirement that equal fingerprints always execute: eligible valid
  pass records may skip only their exact selected test; failures, incomplete identities, and
  invalid exchanges always execute or fail operationally.
- Keep sequential in-process execution, existing filters, diagnostic/exit behavior, and ordinary
  source-owned runner policy. Do not add watch mode, process-per-test isolation, or a general
  compiler-cache rewrite.

## Capabilities

### New Capabilities

- `silk-test-result-caching`: Dependency-complete per-test execution identities, persistent
  pass-only records, bounded isolated runner exchange, cache failure policy, and reuse reporting.

### Modified Capabilities

- `silk-test-runner`: Replace unconditional re-execution with exact admitted per-test skip policy
  while retaining ordinary Silk selection, sequential execution, reporting, and failure semantics.
- `silk-cli-workflows`: Make result reuse default for `silk test`, add the result-only
  `--no-cache` bypass, and preserve command output and exit classifications.

## Impact

Compiler test-discovery/instance metadata gains an execution-dependency manifest and Driver exposes
the completed per-test identities for a compiled test executable. The CLI gains a Storage-backed
result-record actor, scoped process-exchange support, workflow integration, and the `--no-cache`
flag. The bundled source runner gains bounded plan/result exchange and cached/executed reporting.
Focused compiler, source-runner, Storage, Program, Workflow, and CLI fixtures change; the language
test declaration contract, authored fingerprint meaning, ordinary build artifacts, and compilation
cache controls do not.
