# Fast Fourier transform

This complete radix-2 Cooley–Tukey transform bit-reverses eight complex samples, then performs its
three butterfly stages in place. Its unit impulse is shifted to index one, so every non-DC output
depends on sine and cosine twiddle factors instead of collapsing to a constant DC-only check.

The entry point scales each real and imaginary component from bins one through seven by 1000,
converts those values to stable integers, and folds them with distinct weights. The committed
weighted sum is `53420`, reduced modulo 251 to the process-safe fingerprint `208`. A stubbed
operation, a quadrant sign error, or a broken butterfly changes the result while the scaling
remains comfortably away from integer conversion boundaries.
