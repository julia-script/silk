# Source C entry admission

This fixture selects `silk/native_start` through the existing composition
catalog and `Intrinsic.application` import. Its C export owns `main`; there is no
compiler invocation root or generated executable adapter. The source wraps the
application in an owned Execution and drives it from a non-suspending C export.
Application result conversion uses a source interface and phantom provider, keeping
lazy Effect identities in function operands rather than erased stored fields.

Run `node packages/compiler/conformance/hosted-start/run.mjs` after building the
compiler, with the standard SILK_SUPPLY tool/sysroot/container variables. Required
CI executes both optimization modes on all three native targets. Cases cover integer
and unit returns, an ordinary function crossing Effect.suspend, successful Effect
entry, captured Effect state, typed failure and a HostInput service lookup before and after
suspension. The captured value comes from a
separately compiled C function and is checked when the Effect runs. Independent C code declares `int main(int, char **)`, while the
pinned platform CRT invokes the source export. Object inspection rejects silk_main
and generated host/report imports; malloc/free remain the allocator boundary.
Reports and artifacts live in `.scratch/hosted-start`.

The source captures argc/argv and the selected libc environment into owned NativeHostInput
storage, moves OsHostInput into the Execution body, and supplies the HostInput service
through ordinary source result specialization. Lookup after suspension observes that
same owned invocation state.

NativeDiagnostics owns observation inside the application Execution, with 64 context
nodes, 4096 output bytes and 32 frames. The selected terminal handler drops its failure
payload before obtaining report policy 1. Bootstrap failure observation uses no pool;
report refusal or output failure preserves that policy. A generic `NonParking` bound
checks the complete application call, including the selected result interface. Nested
transfers are admitted; external parking requires an execution owner in application source.

`SILK_STARTUP_FAULTS=true` selects the separate bootstrap fault fixture and writes evidence
to `.scratch/hosted-start-faults`. Its C test constructor calls the actual source C entry,
then exits before the CRT's normal main call. It refuses every allocation ordinal observed
in a successful bootstrap, repeats that sweep with failed report writes, poisons freed
allocations, rejects leaks and duplicate frees, and exercises invalid inputs, application
failure payload cleanup and reentrant entry. This constructor belongs only to the test harness.
The selected SDK/glibc header pins are retained in `startup-supplies.json`.

The default executable composition selects this source runtime. A tap-failure case
fails after suspension and verifies the original failure origin survives sequencing;
its success callback would trap if invoked. Shared Wasm entry admission lives in the
execution-storage runner. Full migration closure remains tracked by JUL-130.
