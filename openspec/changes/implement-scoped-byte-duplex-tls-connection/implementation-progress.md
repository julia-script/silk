# JUL-188 completion progress

The user authorized taking over JUL-188 and merging the scoped callback prerequisite into its branch. PR #427 was merged through commit `234cba3870ca39a88cfbc0bc6fa6422d3cce73af` on PR #424. The verified JUL-187 implementation from main is an ancestor. The original worktree and both stashes remain intact.

## Corrections after the merge

- Named generic callbacks retain their explicit invocation lifetime while inferring their value and service-row arguments. Borrowed-input validity is available during nested Effect comparison, and nominal validation distinguishes bound invocation lifetimes from free captures.
- Aggregate outlives assumptions imply validity of their stored generic parameters. Callable schema binders scope their lifetime metadata as well as parameters/results.
- Fully selected named callbacks prove concrete provider/row constraints before their values lose generic schema metadata. Missing provider implementations remain errors; unresolved or assumed evidence remains subject to the existing escape restriction.
- Mapped provider operations respect the associated provider owner when several providers define the same method name.
- TLS provider selection covers the complete operation row, excludes ambient byte duplex at every access level, and uses explicit reborrows. Named fixture/doc callbacks state one invocation lifetime. Fixture startup handles trust/allocation failures.
- TLS Wasm acceptance moves out of the general Driver file into its own process/CI lane. TLS connection compilation exhausted the default 4 GiB heap after semantic checking; the targeted lane uses the existing TLS-client 6 GiB allowance. CLI/LSP CI jobs run all their tests one file at a time after observed 60 s timeouts under concurrent files.

## Verification so far

Focused type tests: 105 passed. Related callback/provider/inference tests: 65 passed. The scoped-bracket capture/escape regression, including a borrowed generic aggregate, passes analysis and MIR verification. The TLS namespace witness accepts its callback and rejects an ambient-byte-duplex callback. Standard-library documentation generation checked 123 modules with no policy violations. Compiler source builds pass.

TLS native execution, TLS Wasm execution, final local checks, and exact-head full CI are still pending. The first repaired native compilation reached the backend but exhausted the default heap; this is not a passing runtime witness. The draft remains in progress, and no review or final handoff is claimed.
