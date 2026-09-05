# Primitive target evidence

The fixture covers bool storage, fixed 8/16/32/64-bit integers, binary32/binary64,
data pointers and C long. It does not establish aggregate layout, parameter classification,
foreign calls, startup or runtime behavior. `primitives.c` is independently authored C,
compiled without any headers or Silk-generated declarations. All targets compiled successfully
with Clang 22.1.8 using the arguments in `provenance.json`; `llvm-readobj` 22.1.8 confirmed their
object architectures/formats and the exported witness symbol. `.ll` files record the independently
computed size/alignment witness and LLVM data layout. Raw LLVM layouts are evidence, not source APIs.

`authorities.json` pins the normative sources: AAPCS64 2025Q1 (fundamental types and stack
constraints); the x86-64 psABI revision shown there (data representation); WebAssembly Basic C ABI
version 1 at its recorded commit; and Apple's ARM64 platform differences, whose unversioned JSON
is content-pinned by SHA-256. Retrieved 2026-09-05. C-header inventory is empty by design.
The description exposes the ordinary data address space only; LLVM's special address spaces are
not admitted Silk machine-pointer capabilities. The stack alignment is 16 bytes on these targets.

Prior art is pinned to Zig e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa and Rust
c33d8f3b5a50b56466998e8c5ed8a077d2caed84, as discussed in the OpenSpec design. Zig's
`dep_shared_builtin` checks imported builtin identity, not differing same-target configurations.
Rust's `const-fn-cycle` is a successful unused-helper regression, and `const-size_of-cycle`
checks a type-level cycle. None directly specifies Silk package-configuration bootstrap; the
Silk configuration suite must supply those acceptance cases.
