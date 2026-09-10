# Raw Linux admission

## Machine and memory boundary

The only machine-specific layer is typed source assembly in `raw_linux.silk` and
the naked `_start` fragments in `raw_start.silk`. x86-64 syscall results use rax;
arguments use rdi, rsi, rdx, r10, r8 and r9, with rcx/r11/flags clobbered. ARM64
uses x8 for the number, x0 for the result and x0–x5 for arguments. Each operation
has side effects and read/write memory. No errno or libc symbol is involved.

The source pointer conversion is unsafe typed assembly, not a compiler-known
library declaration. It establishes only a bit-pattern conversion. Kernel-stack
readability, pointed-to string validity, allocation lifetime and access bounds
remain caller obligations. The existing Pointer operations perform access and
requalification; no new memory-ownership escape is introduced.

The raw entry uses the existing selected runtime root and Intrinsic.application
binding. It does not add spelling recognition to semantic analysis or lowering.
The source C initializer is an export governed by the existing foreign declaration
contract. Its stack pointer precondition is documented; it is not a public Silk
constructor for arbitrary memory. Only the machine entry carries machine metadata.

Process inputs remain borrowed for the process lifetime. The decoder validates the
argv terminator and requires one positive power-of-two AT_PAGESZ before application
invocation. The unsafe caller supplies complete terminated readable storage. This
contract deliberately does not claim bounded validation of arbitrary memory.

Mapping acquisition checks power-of-two alignment and page size, then addition
and rounding overflow before mmap. The owner exists immediately after successful
mmap, before subsequent address checks. It retains the entire reservation for
structured drop. Usable pointers borrow that reservation; explicit release consumes
it. Error values are recoverable Result values. Fatal traps have no cleanup promise.

## Prior art

Reviewed Zig commit e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa:

- `lib/std/os/linux/x86_64.zig`, syscall0–6: identical Linux register allocation and
  rcx/r11/memory clobber obligations. Silk declares condition flags conservatively.
- `lib/std/os/linux/aarch64.zig`, syscall0–6: x8/x0–x5 with memory effects. Silk's
  typed assembly uses an output/input tie for x0 when an argument occupies it.
- `lib/std/start.zig`, posixCallMainAndExit: argc followed by argv, null, environment,
  null and auxiliary pairs. Zig's broader startup performs TLS/relocation/runtime
  initialization. Silk's admitted subset is static non-PIE and synchronous and
  requires its runtime page fact before calling the application.
- `lib/std/heap/PageAllocator.zig`, map: over-allocation for alignment, with prefix
  and suffix unmapping. Silk retains one complete mapping instead, so its affine
  owner has one release obligation and no splitting/reassembly bookkeeping.

The pinned Rust checkout c33d8f3b5a50b56466998e8c5ed8a077d2caed84 provides hosted
runtime and naked-function comparisons from the initial investigation. Its hosted
std initialization does not supply this raw Linux contract; no claim of raw-runtime
parity is made from it.

## Independent evidence and restrictions

`raw-catalog.json` validates against PlatformCatalog.decode. It records exact UAPI
headers and separates constant and layout claims. `uapi.c` compiles independently
in both pinned Linux installations. `raw-conformance.json` records local debug and
optimized execution for x86-64 and ARM64; the full report retains link inputs and
object evidence. Required CI lanes are wired but have not been observed here.

Static PIE, LTO, raw spawning, execution storage and terminal reports are not part
of this composition. No-libc artifacts with retained generated language-runtime
symbols fail before object compilation. The physical helper planner independently
checks legalized object requirements and final ELF closure. Hosted/Wasm migration
remains separate outstanding work, not an alternate raw execution path.
