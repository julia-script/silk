# CRC-32

Computes the reflected IEEE CRC-32 of the committed bytes `[153, 19, 29, 0]`, one byte and one bit
at a time. Their full unsigned checksum is `0x00000007`, which also fits the native process result
channel without folding or comparing against a precomputed answer.
