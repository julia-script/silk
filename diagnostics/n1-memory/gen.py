import sys
kind, n = sys.argv[1], int(sys.argv[2])
L = []
if kind == "calls":
    L += ["fn add(a: i32, b: i32) -> i32 {", "  return a + b", "}", "pub fn main() -> i32 {", "  let mut x = 0"]
    L += ["  x = add(x, 1)"] * n
    L += ["  return x", "}"]
elif kind == "arith":
    L += ["pub fn main() -> i32 {", "  let mut x = 0"]
    L += ["  x = x + 1"] * n
    L += ["  return x", "}"]
elif kind == "funcs":
    for i in range(n):
        L += ["fn f%d(a: i32) -> i32 {" % i, "  return a + 1", "}"]
    L += ["pub fn main() -> i32 {", "  let mut x = 0"]
    L += ["  x = f%d(x)" % i for i in range(n)]
    L += ["  return x", "}"]
elif kind == "drops":
    L += ["import silk.allocator {Allocator, OutOfMemoryError}", "import silk.vector {Vector}", "import silk.host_input {HostInput}", "import silk.effect {Effect}",
          "effect fn body(flag: i32) -> i32 ! OutOfMemoryError ? &mut Allocator {"]
    for i in range(n):
        L += ["  let mut v%d = Vector.make<u8>()" % i, "  run Vector.append<u8>(&mut v%d, 1)" % i, "  if flag == %d {" % i, "    return %d" % i, "  }"]
    L += ["  return 0", "}",
          "pub effect fn main() -> i32 ! OutOfMemoryError ? &mut HostInput {",
          "  let mut allocator = Allocator.systemAllocatorProvider()",
          "  return run body(7) |> Effect.provideMut<Allocator>(&mut allocator)", "}"]
elif kind == "runs" or kind == "plain":
    L += ["import silk.allocator {Allocator, OutOfMemoryError}", "import silk.vector {Vector}", "import silk.host_input {HostInput}", "import silk.effect {Effect}",
          "effect fn body() -> i32 ! OutOfMemoryError ? &mut Allocator {"]
    for i in range(n):
        L += ["  let mut v%d = Vector.make<u8>()" % i]
        L += ["  run Vector.append<u8>(&mut v%d, 1)" % i] if kind == "runs" else []
    L += ["  return 0", "}",
          "pub effect fn main() -> i32 ! OutOfMemoryError ? &mut HostInput {",
          "  let mut allocator = Allocator.systemAllocatorProvider()",
          "  return run body() |> Effect.provideMut<Allocator>(&mut allocator)", "}"]
elif kind == "generics":
    for i in range(n):
        L += ["struct S%d {" % i, "  value: i32", "}"]
    L += ["fn pick<T>(value: T) -> T {", "  return value", "}", "pub fn main() -> i32 {", "  let mut x = 0"]
    for i in range(n):
        L += ["  let s%d = pick<S%d>(S%d {value: %d})" % (i, i, i, i), "  x = x + s%d.value" % i]
    L += ["  return x", "}"]
print("\n".join(L))
