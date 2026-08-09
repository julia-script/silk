# Fast Fourier transform frontier

This complete radix-2 Cooley–Tukey transform bit-reverses eight complex samples, then performs its
three butterfly stages in place. It is currently frontier evidence for the missing trigonometric
scalar operations needed to construct each stage's twiddle factor.
