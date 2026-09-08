# Source process protocol and admission evidence

The production provider is now `silk/native_process`, selected for Darwin ARM64
with system libc and GNU Linux ARM64/x86-64. `OsChildProcess` delegates to it.
The execute/capture intrinsics, generated C implementation, and shared capture
buffers have been removed. Final admission still requires the failure-injection
and overlapping-invocation evidence listed below.

## Selected protocols

Darwin uses `posix_spawn` with `POSIX_SPAWN_CLOEXEC_DEFAULT`, explicit standard
stream duplication/closure actions, and optional `addchdir_np`. Actions and
attributes are separately owned opaque pointer handles; failed initialization
creates no owner, and every successfully initialized handle is destroyed.
Spawn returns its error directly; errno is not consulted for these operations.

GNU uses `fork`, a CLOEXEC startup-notice pipe, `dup2`, `fcntl`, `closefrom`,
optional `chdir`, and `execve`. All acquired descriptors are relocated to at
least four before fork. The child reserves descriptor three for the startup
notice, maps standard streams, and closes every unrelated descriptor above
three. Successful exec closes the notice writer. Failure sends the four-byte
little-endian errno value and calls `_exit(127)`. The parent accepts clean EOF
without notice bytes as exec success and treats partial notice as failure.
A legitimate application exit 127 remains outcome data. Post-fork source code
uses fixed stack storage and foreign calls; it does not allocate, capture,
format reports, or enter an Effect operation. Normal `NativeForeignGuard`
invocation adds no allocation, lock or mutable reporting state.

The selected glibc `closefrom` provides the async-signal-safe descriptor closure
operation. Its documented safety annotation is visible in the glibc manual and
upstream addition:

- https://www.sourceware.org/glibc/manual/2.38/html_node/Opening-and-Closing-Files.html
- https://sourceware.org/pipermail/glibc-cvs/2021q3/073653.html

This GNU protocol was chosen after an independent posix_spawn probe under local
x86-64 emulation returned a PID for a missing executable. That result is not
interpreted from exit status. A separate fork/exec notice probe distinguished
ENOENT from exit 127 on both GNU architectures. Direct close_range returned
ENOSYS under the emulator; the selected libc closefrom completed successfully.
There is one GNU production protocol, with no compatibility fallback.

## Pointer and resource ownership

Request program, argv, environment and cwd bytes are copied into per-call owners
and NUL terminated before launch. The pointer vectors borrow those stable bytes;
explicit drops after launch keep every owner live throughout native use. Native
calls never retain these pointers after launch returns. Stdin uses `/dev/null`
for EOF. Every capture owns its own output Bytes and scratch buffer.

Pipe endpoints become owners immediately after successful acquisition, including
before relocation. Owners clear their descriptor before closing, so an error
cannot double-close a reused descriptor. Parent setup failure after fork owns a
child whose Drop sends SIGKILL and waits through EINTR for terminal status.
Successful wait disarms that owner; ECHILD disarms it to avoid killing a reused
PID. The host must not separately reap a child owned by this provider.

Capture polls both live streams and reads each ready stream once per iteration.
Positive prefixes are appended, EINTR retries through polling, EOF closes its
endpoint, and capture failure drops both endpoints and resolves the child.
Cleanup failures do not replace the original typed error. Fatal traps do not
promise cleanup. A stopped/continued wait status does not discharge ownership.

## Evidence and remaining work

`conformance/native-process/layout.c` independently asserts selected system
header types, layouts, flags and errno values. `process-supplies.json` pins its
header inputs. The runner retains object inspections, disassembly, tool versions,
physical link plans and actual execution results for debug and optimized lanes.

The expanded source fixture passed all six real lanes: debug and optimized on
Darwin ARM64, GNU ARM64 and GNU x86-64. Two simultaneous native threads each
exercise separate stdout/stderr, missing executable versus exit 127, signal
termination, selected cwd/environment, stdin EOF, non-UTF-8 argument bytes, and
simultaneous 128 KiB captures on both streams.

All six fault lanes pass. The successful parent lifecycle contains 42 Darwin
foreign-call ordinals and 39 GNU ordinals, each independently failed. Every source
allocation ordinal is also failed (32 Darwin, 32 GNU). Each run verifies no live
descriptor or handle remains, acquired children are reaped once, and secondary
cleanup errors preserve the original failure. Baselines inject EINTR in poll,
read and wait; low-numbered descriptors require relocation; stopped/continued
statuses require another wait. GNU cases deliver fragmented, truncated and
malformed startup notices. See process-conformance.json for observed counts.

The foreign-call sweep substitutes a parent PID for GNU fork; it does not prove
child-side dup/CLOEXEC/exec/notice-write faults. The separate GNU child-fault receiver
now executes real children and covers those branches in ten cases per lane. It
checks shared injection/write observations, descriptor flags, and absence of owned
children after each call. At the exec boundary it verifies cwd, standard-stream
access modes, notice CLOEXEC and closure of descriptors above three. One-byte
writes exercise the child sender, including EINTR, zero progress and partial failure.
The four debug/optimized target lanes passed; detailed evidence is recorded under
.scratch/native-process with the -child-faults suffix. Additional receiver headers
are pinned in process-child-supplies.json. CI includes both GNU targets.

The zero-progress case establishes an unavoidable limit of this EOF protocol:
without any error-notice byte, failed startup looks like successful exec and exit 127. Partial notices are rejected. This limitation is documented on OsChildProcess;
it does not weaken descriptor cleanup or the owned-child reap obligation.
Independent C function-type assertions for every foreign declaration now pass
against all three pinned supplies with Clang 22.1.8 and warnings as errors. Real launch tests exercise child startup success and ENOENT. These are
implementation and verification tasks, not user-input blockers.
