# CRC-32

Computes the reflected IEEE CRC-32 of the committed static byte literal
`b"\x99\x13\x1d\x00"`, one byte and one bit at a time through an allocation-free shared byte
view. Its full unsigned checksum is `0x00000007`, which also fits the native process result channel
without folding or comparing against a precomputed answer.
