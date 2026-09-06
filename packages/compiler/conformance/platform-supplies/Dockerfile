FROM debian:bookworm-slim@sha256:88200866dfff7ea7f5cbcb6ec7c8a701889efe6fe859fe64d6990e4b07ea4171
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc=4:12.2.0-3 gcc-12=12.2.0-14+deb12u1 \
    libc6=2.36-9+deb12u14 libc6-dev=2.36-9+deb12u14 \
    binutils=2.40-2 linux-libc-dev=6.1.180-1 \
    && rm -rf /var/lib/apt/lists/*
