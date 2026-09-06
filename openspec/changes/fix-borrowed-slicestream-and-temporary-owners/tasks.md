## 1. Conformance and receiver contracts

- [x] 1.1 Complete conformance lifetime header replay and operation binder inheritance; verify explicit/elided contract and call equivalence plus bounds/arity rejection in existing analysis tests.
- [x] 1.2 Correct implicit receiver loan provenance; verify repeated holder updates and real wrapper/backing-owner conflicts with shared analysis snapshots.

## 2. Storage and projection

- [x] 2.1 Preserve slice descriptors through direct field indexing; verify structural dynamic bounds and consumed native equivalence to a local view.
- [x] 2.2 Materialize binding-initializer borrowed array owners at their original evaluation points; verify ordered exact-once evaluation, ordinary inference, block/branch/iteration locality and local escape rejection.
- [x] 2.3 Retain hidden owners through suspension and ordinary cleanup; verify initialized affine elements drop exactly once after dependent loans on normal completion, early exits and interruption.

## 3. Integration and handoff

- [x] 3.1 Add a consumed shared native corpus witness using all four fixes and reconcile the prescriptive reference; verify the original full reproducer and strict OpenSpec validation.
- [ ] 3.2 Run pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, pnpm check and pnpm release:candidate; record exact results.
- [ ] 3.3 Obtain independent code and dedicated test-economics approval of the exact committed base-to-head diff, including focused base/head timings; resolve findings and rerun affected checks.
- [ ] 3.4 Publish the issue-only draft PR, read back its base/head/draft state, and hand JUL-151 to In Review with exact committed implementation baseline and verification evidence.
