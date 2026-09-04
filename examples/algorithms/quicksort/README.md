# In-place quicksort

Lomuto partitioning sorts eight signed integers in place and recursively processes the two
partitions through independent runtime activation frames. The expected result is
`[-8, -3, 0, 1, 2, 5, 7, 9]`, whose independently pinned native rolling fingerprint is `50`.
LLVM-native and LLVM-to-Wasm lowering share the language's emitted recursion semantics.
