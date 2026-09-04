# Portable FileSystem

This example provides the source-defined `FileSystem` service with application-owned memory. The
program constructs a provider-absolute `Path`, passes one complete byte view to
`writeFileWithParents`, reads independently owned `Bytes`, checks existence, and handles portable
`FileError | OutOfMemoryError` failures at the application boundary.

The provider is deliberately ordinary Silk source. A native application can instead provide the
confined `OsFileSystem`; a browser or Wasm application can provide a virtual filesystem without
changing the consumer operations. Importing `silk.filesystem` never selects a platform or an
ambient current directory.

This source is a standalone acceptance fixture rather than a manifest-based CLI project. Run its
structured compiler checks from the repository root with:

```sh
pnpm --filter @silklang/compiler exec vitest run test/FileSystemAcceptance.test.ts
```

The process exits with `42`, the checksum of the four bytes written and read back.
