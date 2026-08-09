# In-place quicksort

Lomuto partitioning sorts eight signed integers in place and recursively processes the two
partitions through independent runtime activation frames. The expected result is
`[-8, -3, 0, 1, 2, 5, 7, 9]`, whose rolling fingerprint is `50` across evaluation, native, and
direct WebAssembly. Evaluator step and call-depth limits are tooling safeguards; they do not alter
the language's emitted recursion semantics.
