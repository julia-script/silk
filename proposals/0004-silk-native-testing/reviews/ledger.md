# Finding ledger: SLP-0004

| Id | Claim | Severity | State | Raised | Last touched | Evidence / resolution |
| --- | --- | --- | --- | --- | --- | --- |
| S1 | C2 | High | FIXED | r001 | r001 | Requirement rows are nominal. Revision 32 gives the sealed operation a source-supplied reporter-service witness instead of recognizing `silk.test.Reporter`. |
| S2 | C1 | High | FIXED | r001 | r001 | MODULE-004 excludes unreachable files. Revision 32 defines manifest test roots, the package-root fallback, and the standard-library test catalog. |
| S3 | C3 | High | FIXED | r001 | r001 | Lexical provision cannot force provider-object freshness. Revision 32 makes freshness standard-runner policy and permits custom sharing. |
| P3 | C5 | High | FIXED | r001 | r001 | Existing `silk.host_input` exposes raw ordered arguments. Revision 32 removes the test-entry adapter and uses HostInput. |
| E2 | C4 | High | FIXED | r001 | r001 | Current parser has no enum declaration. Revision 32 uses structs plus structural unions. |
| E6 | C6 | Medium | FIXED | r001 | r001 | Revision 32 uses `let mut reporter`, borrowed provision, `match move`, and an owned path transfer. |
| P4 | C2 | Medium | CLOSED | r001 | revision 35 | Accepted semantics require an owned logical StackPath. OpenSpec must establish its allocation, cleanup, and evaluator capture representation before implementation; inability to do so triggers the proposal's explicit revisit gate rather than silently changing the direction. |
| E3 | C1 | Medium | FIXED | r001 | r001 | Revision 32 limits marked declarations to Effect functions, the only form exercised by the assertion contract. |
| E4 | C1 | Medium | CLOSED | r001 | revision 35 | Delegated to OpenSpec: each invalid eligibility shape needs a normative diagnostic scenario with observable code/span expectations, but those diagnostics cannot reverse the accepted eligibility model. |
| S6 | C6 | Medium | CLOSED | r001 | revision 33 | Revision 33 maps runner input, output, allocation, and reporting infrastructure failures to status 2 while selected test failures remain status 1. |
| S5 | C4 | Low | REJECTED | r001 | r001 | Manual expansion is possible, but the pressure-driven ordinary-source helper is an explicit in-scope author choice and adds no compiler privilege. |
| S7 | C4 | High | FIXED | r002 | r002 | Revision 33 makes assertions silent and moves closed Outcome-to-Event reporting into ordinary runner policy while retaining replaceable Reporter presentation. |
| S8 | C2 | High | FIXED | r002 | r002 | Revision 33 removes the witness model; eligible tests are closed and no compiler phase selects a library service identity. |
| S9 | C1 | High | FIXED | r002 | r002 | Revision 33 defines a distinct runner executable root composed with, but excluded from, the inventory-root closure. |
| E9 | C1 | High | FIXED | r002 | r002 | Revision 33 requires roots inside the existing package source root and derives identities from that root; the example is contained. |
| S11 | C5 | High | FIXED | r002 | r002 | Revision 33 uses a closed ordinary entry whose source edge constructs/providers OsHostInput, Allocator, and output services and maps infrastructure failure to status 2. |
| E8 | C6 | Medium | FIXED | r002 | r002 | Revision 33 exposes borrowed slice access and uses a current while/index loop. |
| E10 | C6 | Medium | CLOSED | r002 | revision 34 | Revisions 33–34 define byte matching completely: ASCII letters fold case, all other bytes remain exact, invalid UTF-8 needs no decoding, and acquisition failure maps to infrastructure status 2. |
| E11 | C4 | Medium | FIXED | r002 | r002 | Revision 33 removes assertion-side events; a recovered assertion error and the returned Outcome can no longer disagree with a prior report. |
| S13 | C3 | Medium | FIXED | r002 | r002 | Revision 33 removes the fresh Effect-scope promise and retains only fresh standard-reporter state. |
| P6 | C2 | Medium | FIXED | r002 | r002 | Revision 33 assigns multi-root construction to existing project-closure tooling rather than the intrinsic privilege. |
| P7 | C2 | Medium | FIXED | r002 | r002 | Revision 33 records uniform compiler-generated per-test adapters, ordinal dispatch, test-only rooting, and code-size scope. |
| P10 | C3 | Medium | CLOSED | r003 | revision 35 | Delegated to OpenSpec: the path retains complete logical frames and standard presentation may filter runner/wrapper frames without changing StackPath semantics. |
| E13 | C4 | High | CLOSED | r003 | audit a001 | Revised by the author in revision 34 and passed targeted audit a001: public case/Event fields are externally inspectable without changing ownership. |
| E14 | C6 | High | CLOSED | r003 | audit a001 | Revised by the author in revision 34 and passed targeted audit a001: ASCII folding fixes the focused examples while non-ASCII bytes remain exact. |
| E15 | C4 | Medium | CLOSED | r003 | revision 35 | Revised in revision 35 with a complete single-file runner/test root, bound inventory handle, deliberate failing test, Reporter provision, owned path drop, and final count assertion. |
| O1 | C4 | High | CLOSED | OpenSpec audit o001 | revision 36 | The canonical standard-library contract requires shipped error names ending in `Error`, while `Test.Failure` was both nonconforming and imprecise. The author selected `Test.AssertionError`; this revises the assertion API name without changing its typed-failure behavior or the broader `Outcome.Failed` contract. |
