# Hosted process-input source contract

The hosted entry receives `int argc`, `char **argv` and the selected platform's
environment vector. An unsafe source capture operation copies these into an affine
`NativeHostInput` value before application invocation. Capture requires readable,
NUL-terminated strings, an aligned argv vector through argc, and an environment
vector through its null terminator; those inputs must remain unchanged for the
capture. Negative argc and premature null argument elements are typed input
failures. All completed and partially completed owned allocations are released on
capture failure. No raw input pointer survives successful capture.

Argument count and indexed argument lookup observe the captured order, including
argv[0]. Environment lookup matches raw bytes before the first equals sign and
returns the bytes after it. Missing and empty values remain distinct. Names with
NUL or equals cannot match. Duplicate names select the first matching entry.
Malformed entries without equals are ignored. Each lookup returns an optional
full byte length and writes only the prefix fitting the initialized caller slice;
the unwritten tail remains untouched. The snapshot is read-only, and independent
snapshots share no mutable accounting or buffers.

Working-directory lookup owns a fresh initialized buffer per call. It starts at
256 bytes, doubles on ERANGE through 1 MiB, and releases each superseded buffer.
It captures errno before performing any subsequent operation. Other host failures
and an exhausted bound produce HostInputError; allocation failure remains
OutOfMemoryError. Success copies the bytes before NUL into independent Bytes.
The output-copy variant uses the same full-length/committed-prefix contract.
Changing the process cwd is outside this read-only operation and remains a caller
coordination responsibility.

Pinned Darwin MacOSX15.5.sdk and both GNU sysroots declare
`char *getcwd(char *, size_t)` in unistd.h. The source declaration uses a nullable
mutable byte pointer and usize, with direct errno access from the already admitted
platform accessor. Independent C fixtures must verify widths, ERANGE and the exact
function pointer signature on each target before final admission. This contract
does not authorize ambient argument globals or compiler recognition of its source
module. The OsHostInput facade owns this value and uses its immutable lengths to
allocate exact-size owned lookup results. Missing values need no output allocation.
Its constructor takes ownership without reading foreign state.

Library boundaries without entry arguments may explicitly capture an environment-only
snapshot. It has zero arguments and copies the selected libc environment: Darwin
uses the `char ***_NSGetEnviron(void)` accessor declared in crt_externs.h; GNU reads
the ordinary external `char **environ` object. A null environment vector denotes no
variables. This unsafe capture requires exclusion of concurrent foreign environment
mutation. It never supplies ambient arguments or retains the foreign vector.

The four old intrinsic operations, OsCall lowering and generated C adapters are deleted.
Default hosted startup must capture actual entry arguments and
provide the resulting owned facade before application invocation.

NativeHostInput.captureProcess receives C entry argc/argv and copies the selected
libc environment through the same environmentSnapshot boundary. Argument decoding
is shared with explicit-vector capture. The resulting affine value moves into the
source entry's Execution body, where OsHostInput provides HostInput to the selected
application Effect. Source entry uses the ordinary C ABI `int main(int, char **)`;
Silk's read-only byte-pointer qualifiers grant no foreign ownership or extended
lifetime. Capturing both vectors completes before application invocation. The current
entry fixture admits this handoff; default selection and reporting remain pending.
