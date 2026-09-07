# Reviewed prior art

Exact URLs and SHA-256 identities are in supplies.json. Zig revision
`e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa` and Rust revision
`c33d8f3b5a50b56466998e8c5ed8a077d2caed84` are design evidence, not ABI authorities or executed tests.

Zig fs.zig redirects operations toward Io.Dir/File; those actors and Io.Threaded were followed.
Threaded.closeFd treats EINTR as a completed close rather than retrying. Silk also never retries,
but its explicit fallible close reports the native failure after otherwise successful work; Drop
cannot replace a primary failure. Zig fs/test.zig covers file/directory symlinks and no-follow
inspection. Silk adopts explicit no-follow boundary cases without adopting Zig's broader filesystem,
raw kernel selection or seek/locking surface.

Rust sys/fs/unix.rs ReadDir clears errno before readdir, owns copied byte names and explicitly warns
that returned dirent storage may be smaller or larger than the nominal struct. Silk uses bounded
byte projection and owned pending names; it does not form or load a whole-record reference. Rust's
broad host path semantics and symlink support are not the confined Silk provider contract.

Rust fs/tests.rs smoke read/write/remove, NotFound and EOF cases inform actual-effect regression
coverage. Its sys/fs/unix/tests.rs permissions-format tests are not an analogue for source ownership
or injected close failures. Neither upstream suite directly proves Silk cancellation, typed-primary
versus cleanup error ordering, pending short-buffer retries or this bounded counter naming policy;
those require Silk-specific deterministic fixtures. No upstream tests were claimed as run here.
