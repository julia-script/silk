## 1. Public codec

- [x] 1.1 Add `silk.base64` with the stateless actor, structured reason/error types, overflow-safe encoded sizing, and strict exact decoded sizing; verify focused analysis accepts the public signatures and boundary-size assertions on both pointer widths.
- [x] 1.2 Implement atomic caller-slice encoding and decoding with standard padding, deterministic validation precedence, zero allocation, and preserved suffixes; verify the Base64 acceptance program passes independent RFC/binary vectors and malformed/capacity cases.

## 2. Exported surface and evidence

- [x] 2.1 Register `silk.base64` and its public aliases in the generated standard-library manifest/catalog; verify an ordinary import resolves and generated metadata is current.
- [x] 2.2 Add one consolidated Base64-specific acceptance fixture/test covering 32/64-bit analysis and code generation, LLVM-to-Wasm behavior, ownership overlap rejection, and pointer-width sizing without per-vector recompilation.
- [x] 2.3 Add the fixture's public-import source to the shared native acceptance corpus as the native behavioral truth.

## 3. Reference

- [x] 3.1 Add the public Base64 reference with API signatures, canonical examples, error precedence, atomic caller-storage rules, portability, and excluded variants; verify its executable example matches the analyzed acceptance surface.
